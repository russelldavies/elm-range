module Range exposing
    ( BoundFlag(..)
    , Config
    , Range
    , containsElement
    , containsRangeInternal
    , create
    , decoder
    , empty
    , encode
    , equal
    , fromString
    , isEmpty
    , lessThan
    , lowerBoundInclusive
    , lowerBoundInfinite
    , lowerElement
    , merge
    , serialize
    , toString
    , upperBoundInclusive
    , upperBoundInfinite
    , upperElement
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)



-- MODELS


type alias Range subtype =
    { config : Config subtype
    , range : RangeInternal subtype
    }


type RangeInternal subtype
    = Bounded ( Bound subtype, Bound subtype )
    | Empty


type Bound subtype
    = Exclusive subtype
    | Inclusive subtype
    | Infinite


type alias Config subtype =
    { toString : subtype -> String
    , fromString : String -> Result String subtype
    , compare : subtype -> subtype -> Order
    , canonical : Maybe (RangeInternal subtype -> ( ( Maybe subtype, Maybe subtype ), ( BoundFlag, BoundFlag ) ))
    }


type BoundFlag
    = Inc
    | Exc


type BoundType
    = LowerBound
    | UpperBound



-- CREATE


empty : Config subtype -> Range subtype
empty config =
    Range config Empty


create :
    Config subtype
    -> Maybe subtype
    -> Maybe subtype
    -> ( BoundFlag, BoundFlag )
    -> Result String (Range subtype)
create config maybeLower maybeUpper flags =
    -- Flags are mandatory as it will be the subtype's canonical function that
    -- specifies the convention to use.
    {-
       construct maybeLower maybeUpper flags
           |> validate config
           |> Result.map (Range config)
    -}
    Ok <| Range config Empty


fromString :
    Config subtype
    -> String
    -> Result (List Parser.DeadEnd) (RangeInternal subtype)
fromString config =
    Parser.run (parser config)


toString : Config subtype -> RangeInternal subtype -> String
toString config range =
    case range of
        Empty ->
            "empty"

        Bounded bounds ->
            case bounds of
                ( Inclusive lower, Inclusive upper ) ->
                    "[" ++ config.toString lower ++ "," ++ config.toString upper ++ "]"

                ( Exclusive lower, Inclusive upper ) ->
                    "(" ++ config.toString lower ++ "," ++ config.toString upper ++ "]"

                ( Inclusive lower, Exclusive upper ) ->
                    "[" ++ config.toString lower ++ "," ++ config.toString upper ++ ")"

                ( Exclusive lower, Exclusive upper ) ->
                    "(" ++ config.toString lower ++ "," ++ config.toString upper ++ ")"

                ( Infinite, Inclusive upper ) ->
                    "(," ++ config.toString upper ++ "]"

                ( Infinite, Exclusive upper ) ->
                    "(," ++ config.toString upper ++ ")"

                ( Inclusive lower, Infinite ) ->
                    "[" ++ config.toString lower ++ ",)"

                ( Exclusive lower, Infinite ) ->
                    "(" ++ config.toString lower ++ ",)"

                ( Infinite, Infinite ) ->
                    "(,)"



-- OPERATIONS


equal : Config subtype -> RangeInternal subtype -> RangeInternal subtype -> Bool
equal config r1 r2 =
    r1 == r2


lessThan : Config subtype -> RangeInternal subtype -> RangeInternal subtype -> Bool
lessThan config r1 r2 =
    rangeCompare config r1 r2 == LT


containsRangeInternal : Config subtype -> RangeInternal subtype -> RangeInternal subtype -> Bool
containsRangeInternal ({ compare } as config) outerRangeInternal innerRangeInternal =
    let
        comp el =
            Maybe.map (containsElement config outerRangeInternal) (el innerRangeInternal)
                |> Maybe.withDefault False
    in
    comp lowerElement && comp upperElement


containsElement : Config subtype -> RangeInternal subtype -> subtype -> Bool
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


lowerElement : RangeInternal subtype -> Maybe subtype
lowerElement range =
    case range of
        Bounded ( lower, _ ) ->
            boundElement lower

        Empty ->
            Nothing


upperElement : RangeInternal subtype -> Maybe subtype
upperElement range =
    case range of
        Bounded ( _, upper ) ->
            boundElement upper

        Empty ->
            Nothing


isEmpty : RangeInternal subtype -> Bool
isEmpty range =
    case range of
        Bounded _ ->
            False

        Empty ->
            True


lowerBoundInclusive : RangeInternal subtype -> Bool
lowerBoundInclusive range =
    case range of
        Bounded ( Inclusive _, _ ) ->
            True

        _ ->
            False


upperBoundInclusive : RangeInternal subtype -> Bool
upperBoundInclusive range =
    case range of
        Bounded ( _, Inclusive _ ) ->
            True

        _ ->
            False


lowerBoundInfinite : RangeInternal subtype -> Bool
lowerBoundInfinite range =
    case range of
        Bounded ( Infinite, _ ) ->
            True

        _ ->
            False


upperBoundInfinite : RangeInternal subtype -> Bool
upperBoundInfinite range =
    case range of
        Bounded ( _, Infinite ) ->
            True

        _ ->
            False


merge : Config subtype -> RangeInternal subtype -> RangeInternal subtype -> RangeInternal subtype
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


{-| RangeInternal JSON decoder
-}
decoder :
    Config subtype
    -> Decode.Decoder (RangeInternal subtype)
decoder config =
    Decode.string
        |> Decode.andThen
            (\rangeStr ->
                case fromString config rangeStr of
                    Ok range ->
                        Decode.succeed range

                    Err _ ->
                        Decode.fail "Not a valid range"
            )


{-| Encode RangeInternal to JSON
-}
encode : Config subtype -> RangeInternal subtype -> Encode.Value
encode config =
    toString config >> Encode.string



-- HELPERS


{-| Construct a range value from bounds and range flags

This does not force canonicalization of the range value. In most cases,
external callers should only be canonicalization functions. Note that we
perform some datatype-independent canonicalization checks anyway.

-}
serialize :
    (subtype -> subtype -> Order)
    -> Maybe ( ( BoundFlag, subtype -> subtype ), ( BoundFlag, subtype -> subtype ) )
    -> Maybe subtype
    -> Maybe subtype
    -> ( BoundFlag, BoundFlag )
    -> Result String (RangeInternal subtype)
serialize compare maybeCanonical maybeLower maybeUpper ( lowerFlag, upperFlag ) =
    let
        canonical =
            Maybe.withDefault ( ( lowerFlag, identity ), ( upperFlag, identity ) ) maybeCanonical

        lowerBound =
            maybeLower
                |> Maybe.map (canonicalize (Tuple.first canonical) lowerFlag)
                |> Maybe.withDefault Infinite

        upperBound =
            maybeUpper
                |> Maybe.map (canonicalize (Tuple.second canonical) upperFlag)
                |> Maybe.withDefault Infinite

        boundedRange =
            Bounded ( lowerBound, upperBound )
    in
    case Maybe.map2 compare maybeLower maybeUpper of
        Just GT ->
            Err "Range lower bound must be less than or equal to range upper bound"

        Just EQ ->
            if not (lowerFlag == Inc && upperFlag == Inc) then
                Ok Empty

            else
                Ok boundedRange

        Just LT ->
            Ok boundedRange

        Nothing ->
            Ok boundedRange


canonicalize ( canonicalFlag, step ) specifiedFlag val =
    let
        steppedVal =
            if specifiedFlag == canonicalFlag then
                val

            else
                step val
    in
    flagToBound canonicalFlag <| steppedVal


flagToBound : BoundFlag -> (subtype -> Bound subtype)
flagToBound flag =
    case flag of
        Inc ->
            Inclusive

        Exc ->
            Exclusive


boundToValFlag bound =
    case bound of
        Exclusive val ->
            ( Just val, Exc )

        Inclusive val ->
            ( Just val, Inc )

        Infinite ->
            ( Nothing, Exc )


deserialize : RangeInternal subtype -> ( Maybe subtype, Maybe subtype, ( BoundFlag, BoundFlag ) )
deserialize range =
    case range of
        Empty ->
            ( Nothing, Nothing, ( Exc, Exc ) )

        Bounded ( lowerBound, upperBound ) ->
            let
                ( lowerVal, lowerFlag ) =
                    boundToValFlag lowerBound

                ( upperVal, upperFlag ) =
                    boundToValFlag upperBound
            in
            ( lowerVal, upperVal, ( lowerFlag, upperFlag ) )


{-| Construct a range from bounds

This does not force canonicalization of the range value. In most cases,
external callers should only be canonicalization functions. Note that we
perform some datatype-independent canonicalization checks anyway.

-}
construct : Maybe subtype -> Maybe subtype -> ( BoundFlag, BoundFlag ) -> RangeInternal subtype
construct maybeLower maybeUpper ( lowerFlag, upperFlag ) =
    let
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



{-
   validate : Config subtype -> RangeInternal subtype -> Result String (RangeInternal subtype)
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
-}


parser : Config subtype -> Parser (RangeInternal subtype)
parser config =
    let
        parseSubtype ( bound, str ) =
            if String.isEmpty str then
                Parser.succeed Infinite

            else
                case config.fromString str of
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



{-
   |> Parser.andThen
       (\range ->
           case validate config range of
               Ok range_ ->
                   Parser.succeed range_

               Err err ->
                   Parser.problem err
       )
-}


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


rangeCompare : Config subtype -> RangeInternal subtype -> RangeInternal subtype -> Order
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
