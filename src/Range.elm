module Range exposing
    ( BoundFlag(..)
    , Range
    , SubtypeConfig
    , containsElement
    , containsRange
    , create
    , decoder
    , empty
    , encode
    , equal
    , fromString
    , isEmpty
    , lowerBoundInclusive
    , lowerBoundInfinite
    , lowerElement
    , merge
    , toString
    , upperBoundInclusive
    , upperBoundInfinite
    , upperElement
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)



-- MODELS


type Range subtype
    = Bounded ( Bound subtype, Bound subtype )
    | Empty


type Bound subtype
    = Exclusive subtype
    | Inclusive subtype
    | Infinite


type alias SubtypeConfig subtype =
    { toString : subtype -> String
    , fromString : String -> Result String subtype
    , compare : subtype -> subtype -> Order
    , canonical : Maybe (Range subtype -> ( ( Maybe subtype, Maybe subtype ), ( BoundFlag, BoundFlag ) ))
    }


type BoundFlag
    = Inc
    | Exc


type BoundType
    = LowerBound
    | UpperBound



-- CREATE


empty : Range subtype
empty =
    Empty


create :
    SubtypeConfig subtype
    -> Maybe subtype
    -> Maybe subtype
    -> ( BoundFlag, BoundFlag )
    -> Result String (Range subtype)
create subtypeConfig maybeLower maybeUpper flags =
    -- Flags are mandatory as it will be the subtype's canonical function that
    -- specifies the convention to use.
    construct ( ( maybeLower, maybeUpper ), flags ) |> validate subtypeConfig


fromString :
    SubtypeConfig subtype
    -> String
    -> Result (List Parser.DeadEnd) (Range subtype)
fromString subtypeConfig =
    Parser.run (parser subtypeConfig)


toString : SubtypeConfig subtype -> Range subtype -> String
toString subtypeConfig range =
    case range of
        Empty ->
            "empty"

        Bounded bounds ->
            case bounds of
                ( Inclusive lower, Inclusive upper ) ->
                    "[" ++ subtypeConfig.toString lower ++ "," ++ subtypeConfig.toString upper ++ "]"

                ( Exclusive lower, Inclusive upper ) ->
                    "(" ++ subtypeConfig.toString lower ++ "," ++ subtypeConfig.toString upper ++ "]"

                ( Inclusive lower, Exclusive upper ) ->
                    "[" ++ subtypeConfig.toString lower ++ "," ++ subtypeConfig.toString upper ++ ")"

                ( Exclusive lower, Exclusive upper ) ->
                    "(" ++ subtypeConfig.toString lower ++ "," ++ subtypeConfig.toString upper ++ ")"

                ( Infinite, Inclusive upper ) ->
                    "(," ++ subtypeConfig.toString upper ++ "]"

                ( Infinite, Exclusive upper ) ->
                    "(," ++ subtypeConfig.toString upper ++ ")"

                ( Inclusive lower, Infinite ) ->
                    "[" ++ subtypeConfig.toString lower ++ ",)"

                ( Exclusive lower, Infinite ) ->
                    "(" ++ subtypeConfig.toString lower ++ ",)"

                ( Infinite, Infinite ) ->
                    "(,)"



-- OPERATIONS


equal : SubtypeConfig subtype -> Range subtype -> Range subtype -> Bool
equal { compare } r1 r2 =
    case ( r1, r2 ) of
        ( Empty, Empty ) ->
            True

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            let
                lowerBoundOrder =
                    compareBounds compare ( lower1, LowerBound ) ( lower2, LowerBound )

                upperBoundOrder =
                    compareBounds compare ( upper1, UpperBound ) ( upper2, UpperBound )
            in
            lowerBoundOrder == EQ && upperBoundOrder == EQ

        _ ->
            False


lessThan : SubtypeConfig subtype -> Range subtype -> Range subtype -> Order
lessThan { compare } r1 r2 =
    case ( isEmpty r1, isEmpty r2 ) of
        ( True, True ) ->
            EQ

        ( True, False ) ->
            LT

        ( False, True ) ->
            GT

        ( False, False ) ->
            EQ


containsRange : SubtypeConfig subtype -> Range subtype -> Range subtype -> Bool
containsRange ({ compare } as config) outerRange innerRange =
    let
        comp el =
            Maybe.map (containsElement config outerRange) (el innerRange)
                |> Maybe.withDefault False
    in
    comp lowerElement && comp upperElement


containsElement : SubtypeConfig subtype -> Range subtype -> subtype -> Bool
containsElement { compare } range element =
    let
        lt_ =
            lt compare

        lte_ =
            lte compare

        gt_ =
            gt compare

        gte_ =
            gte compare
    in
    case range of
        Empty ->
            False

        Bounded bounds ->
            case bounds of
                ( Inclusive lower, Inclusive upper ) ->
                    lte_ lower element && lte_ element upper

                ( Inclusive lower, Exclusive upper ) ->
                    lte_ lower element && lt_ element upper

                ( Exclusive lower, Inclusive upper ) ->
                    lt_ lower element && lte_ element upper

                ( Exclusive lower, Exclusive upper ) ->
                    lt_ lower element && lt_ element upper

                ( Infinite, Inclusive upper ) ->
                    lte_ element upper

                ( Infinite, Exclusive upper ) ->
                    lt_ element upper

                ( Inclusive lower, Infinite ) ->
                    lte_ lower element

                ( Exclusive lower, Infinite ) ->
                    lt_ lower element

                ( Infinite, Infinite ) ->
                    True



-- FUNCTIONS


lowerElement : Range subtype -> Maybe subtype
lowerElement range =
    case range of
        Bounded ( lower, _ ) ->
            boundElement lower

        Empty ->
            Nothing


upperElement : Range subtype -> Maybe subtype
upperElement range =
    case range of
        Bounded ( _, upper ) ->
            boundElement upper

        Empty ->
            Nothing


isEmpty : Range subtype -> Bool
isEmpty range =
    case range of
        Bounded _ ->
            False

        Empty ->
            True


lowerBoundInclusive : Range subtype -> Bool
lowerBoundInclusive range =
    case range of
        Bounded ( Inclusive _, _ ) ->
            True

        _ ->
            False


upperBoundInclusive : Range subtype -> Bool
upperBoundInclusive range =
    case range of
        Bounded ( _, Inclusive _ ) ->
            True

        _ ->
            False


lowerBoundInfinite : Range subtype -> Bool
lowerBoundInfinite range =
    case range of
        Bounded ( Infinite, _ ) ->
            True

        _ ->
            False


upperBoundInfinite : Range subtype -> Bool
upperBoundInfinite range =
    case range of
        Bounded ( _, Infinite ) ->
            True

        _ ->
            False


merge : SubtypeConfig subtype -> Range subtype -> Range subtype -> Range subtype
merge { compare } range1 range2 =
    case ( range1, range2 ) of
        ( Empty, Empty ) ->
            Empty

        ( Bounded _, Empty ) ->
            range1

        ( Empty, Bounded _ ) ->
            range2

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            let
                lower =
                    if compareBounds compare ( lower1, LowerBound ) ( lower2, LowerBound ) == LT then
                        lower1

                    else
                        lower2

                upper =
                    if compareBounds compare ( upper1, UpperBound ) ( upper2, UpperBound ) == GT then
                        upper1

                    else
                        upper2
            in
            Bounded ( lower, upper )



-- JSON


{-| Range JSON decoder
-}
decoder :
    SubtypeConfig subtype
    -> Decode.Decoder (Range subtype)
decoder subtypeConfig =
    Decode.string
        |> Decode.andThen
            (\rangeStr ->
                case fromString subtypeConfig rangeStr of
                    Ok range ->
                        Decode.succeed range

                    Err _ ->
                        Decode.fail "Not a valid range"
            )


{-| Encode Range to JSON
-}
encode : SubtypeConfig subtype -> Range subtype -> Encode.Value
encode subtypeConfig =
    toString subtypeConfig >> Encode.string



-- HELPERS


validate : SubtypeConfig subtype -> Range subtype -> Result String (Range subtype)
validate { canonical, compare } range_ =
    let
        boundErr =
            Err "Lower bound must be less than or equal to upper bound"

        checkBounds range =
            case Maybe.map2 compare (lowerElement range) (upperElement range) of
                Just order ->
                    case order of
                        GT ->
                            boundErr

                        EQ ->
                            -- Edge case: if bounds are equal, and both exclusive, range is empty
                            if not (lowerBoundInclusive range || upperBoundInclusive range) then
                                Ok Empty

                            else
                                range |> canonicalize |> normalize

                        _ ->
                            range |> canonicalize |> normalize

                Nothing ->
                    range |> canonicalize |> normalize

        normalize range =
            case Maybe.map2 compare (lowerElement range) (upperElement range) of
                Just order ->
                    case order of
                        LT ->
                            Ok range

                        EQ ->
                            Ok Empty

                        GT ->
                            boundErr

                Nothing ->
                    Ok range

        canonicalize range =
            case canonical of
                Nothing ->
                    range

                Just fn ->
                    construct (fn range)
    in
    range_
        |> checkBounds


construct : ( ( Maybe subtype, Maybe subtype ), ( BoundFlag, BoundFlag ) ) -> Range subtype
construct ( ( maybeLower, maybeUpper ), ( lowerFlag, upperFlag ) ) =
    let
        flagToBound flag =
            case flag of
                Inc ->
                    Inclusive

                Exc ->
                    Exclusive

        lower =
            maybeLower
                |> Maybe.map (flagToBound lowerFlag)
                |> Maybe.withDefault Infinite

        upper =
            maybeUpper
                |> Maybe.map (flagToBound upperFlag)
                |> Maybe.withDefault Infinite
    in
    Bounded ( lower, upper )


parser : SubtypeConfig subtype -> Parser (Range subtype)
parser subtypeConfig =
    let
        parseSubtype ( bound, str ) =
            if String.isEmpty str then
                Parser.succeed Infinite

            else
                case subtypeConfig.fromString str of
                    Ok date ->
                        Parser.succeed (bound date)

                    Err error ->
                        Parser.problem error

        lowerBoundParser =
            Parser.succeed (\bound str -> ( bound, str ))
                |= Parser.oneOf
                    [ Parser.succeed Inclusive
                        |. Parser.symbol "["
                    , Parser.succeed Exclusive
                        |. Parser.symbol "("
                    ]
                |= Parser.getChompedString (Parser.chompUntil ",")
                |> Parser.andThen parseSubtype

        upperBoundParser =
            Parser.succeed (\str bound -> ( bound, str ))
                |= Parser.getChompedString
                    (Parser.oneOf
                        [ Parser.chompUntil ")"
                        , Parser.chompUntil "]"
                        ]
                    )
                |= Parser.oneOf
                    [ Parser.succeed Inclusive
                        |. Parser.symbol "]"
                    , Parser.succeed Exclusive
                        |. Parser.symbol ")"
                    ]
                |> Parser.andThen parseSubtype
    in
    Parser.succeed (\lower upper -> Bounded ( lower, upper ))
        |= lowerBoundParser
        |. Parser.symbol ","
        |= upperBoundParser
        |> Parser.andThen
            (\range ->
                case validate subtypeConfig range of
                    Ok range_ ->
                        Parser.succeed range_

                    Err err ->
                        Parser.problem err
            )


boundElement : Bound subtype -> Maybe subtype
boundElement bound =
    case bound of
        Inclusive val ->
            Just val

        Exclusive val ->
            Just val

        Infinite ->
            Nothing


lt : (subtype -> subtype -> Order) -> subtype -> subtype -> Bool
lt compare a b =
    case compare a b of
        LT ->
            True

        _ ->
            False


lte : (subtype -> subtype -> Order) -> subtype -> subtype -> Bool
lte compare a b =
    case compare a b of
        GT ->
            False

        _ ->
            True


gt : (subtype -> subtype -> Order) -> subtype -> subtype -> Bool
gt compare a b =
    case compare a b of
        GT ->
            True

        _ ->
            False


gte : (subtype -> subtype -> Order) -> subtype -> subtype -> Bool
gte compare a b =
    case compare a b of
        LT ->
            False

        _ ->
            False


{-| Compare two bounds.

The bounds can be any combination of upper and lower; so it's useful for a
variety of operators.

The simple case is when both bounds are finite and inclusive, the result is a
simple comparison of their values.

If a bound is exclusive, then we need to know whether it's a lower bound, in
which case we treat the boundary point as "just greater than" the held value;
or an upper bound, in which case we treat the boundary point as "just less
than" the held value.

If a bound is infinite, it represents minus infinity (less than every other
point) if it's a lower bound; or plus infinity (greater than every other point)
if it's an upper bound.

There is only one case where two boundaries compare equal but are not
identical: when both bounds are inclusive and hold the same finite value, but
one is an upper bound and the other a lower bound.

-}
compareBounds :
    (subtype -> subtype -> Order)
    -> ( Bound subtype, BoundType )
    -> ( Bound subtype, BoundType )
    -> Order
compareBounds compare ( bound1, bound1Type ) ( bound2, bound2Type ) =
    case ( bound1, bound2 ) of
        ( Infinite, Infinite ) ->
            case ( bound1Type, bound2Type ) of
                ( LowerBound, LowerBound ) ->
                    EQ

                ( UpperBound, UpperBound ) ->
                    EQ

                ( LowerBound, UpperBound ) ->
                    LT

                ( UpperBound, LowerBound ) ->
                    GT

        ( Infinite, _ ) ->
            if bound1Type == LowerBound then
                LT

            else
                GT

        ( _, Infinite ) ->
            if bound2Type == UpperBound then
                LT

            else
                GT

        ( Exclusive bound1Val, Exclusive bound2Val ) ->
            let
                order =
                    compare bound1Val bound2Val
            in
            if order == EQ then
                case ( bound1Type, bound2Type ) of
                    ( LowerBound, LowerBound ) ->
                        EQ

                    ( UpperBound, UpperBound ) ->
                        EQ

                    ( LowerBound, UpperBound ) ->
                        GT

                    ( UpperBound, LowerBound ) ->
                        LT

            else
                order

        ( Exclusive bound1Val, Inclusive bound2Val ) ->
            let
                order =
                    compare bound1Val bound2Val
            in
            if order == EQ then
                if bound1Type == LowerBound then
                    GT

                else
                    LT

            else
                order

        ( Inclusive bound1Val, Exclusive bound2Val ) ->
            let
                order =
                    compare bound1Val bound2Val
            in
            if order == EQ then
                if bound2Type == LowerBound then
                    LT

                else
                    GT

            else
                order

        ( Inclusive bound1Val, Inclusive bound2Val ) ->
            compare bound1Val bound2Val


min : (subtype -> subtype -> Order) -> subtype -> subtype -> subtype
min compare x y =
    if lt compare x y then
        x

    else
        y


max : (subtype -> subtype -> Order) -> subtype -> subtype -> subtype
max compare x y =
    if gt compare x y then
        x

    else
        y


rangeCompare : SubtypeConfig subtype -> Range subtype -> Range subtype -> Order
rangeCompare { compare } r1 r2 =
    case ( r1, r2 ) of
        ( Empty, Empty ) ->
            EQ

        ( Empty, _ ) ->
            LT

        ( _, Empty ) ->
            GT

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            let
                lowerBoundOrder =
                    compareBounds compare ( lower1, LowerBound ) ( lower2, LowerBound )

                upperBoundOrder =
                    compareBounds compare ( upper1, UpperBound ) ( upper2, UpperBound )
            in
            if lowerBoundOrder == EQ then
                upperBoundOrder

            else
                lowerBoundOrder
