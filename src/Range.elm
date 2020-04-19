module Range exposing
    ( BoundFlag(..)
    , Config
    , Range
    , ce
    , configs
    , cr
    , create
    , decoder
    , empty
    , encode
    , eq
    , fromString
    , gt
    , gte
    , isEmpty
    , lowerBoundInclusive
    , lowerBoundInfinite
    , lowerElement
    , lt
    , lte
    , merge
    , neq
    , nxl
    , nxr
    , ov
    , sl
    , sr
    , toString
    , upperBoundInclusive
    , upperBoundInfinite
    , upperElement
    )

import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
import Time



-- MODELS


{-| Be aware that a Range stores functions internally.
If you want to use `(==)` for comparing two Ranges use the [eq](#eq)
operator.
-}
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
    , canonical : Maybe (Canonical subtype)
    }


type alias Canonical subtype =
    ( ( BoundFlag, subtype -> subtype ), ( BoundFlag, subtype -> subtype ) )


type BoundFlag
    = Inc
    | Exc


type BoundType
    = LowerBound
    | UpperBound



-- CONFIGS


configs =
    { int =
        { toString = String.fromInt
        , fromString = String.toInt >> Result.fromMaybe "Invalid integer"
        , compare = Basics.compare
        , canonical = Just ( ( Inc, (+) 1 ), ( Exc, (+) 1 ) )
        }
    , float =
        { toString = String.fromFloat
        , fromString = String.toFloat >> Result.fromMaybe "Invalid float"
        , compare = Basics.compare
        , canonical = Nothing
        }
    , string =
        { toString = identity
        , fromString = identity >> Ok
        , compare = Basics.compare
        , canonical = Nothing
        }
    , timestamp =
        { fromString = Iso8601.toTime >> Result.mapError (always "Time format invalid")
        , compare = \lower upper -> Basics.compare (Time.posixToMillis lower) (Time.posixToMillis upper)
        , toString = Iso8601.fromTime
        , canonical = Nothing
        }
    }



-- CREATE


empty : Config subtype -> Range subtype
empty config =
    Range config Empty


create :
    Config subtype
    -> Maybe subtype
    -> Maybe subtype
    -> Maybe ( BoundFlag, BoundFlag )
    -> Result String (Range subtype)
create config lower upper flags =
    let
        defaultFlags =
            ( Inc, Exc )
    in
    make config ( lower, upper, Maybe.withDefault defaultFlags flags )
        |> Result.map (Range config)


fromString :
    Config subtype
    -> String
    -> Result String (Range subtype)
fromString config str =
    case Parser.run (parser config) str of
        Ok rangeParts ->
            make config rangeParts |> Result.map (Range config)

        Err errs ->
            errs
                |> List.map
                    (\err ->
                        case err.problem of
                            Parser.Problem s ->
                                s ++ " at col " ++ String.fromInt err.col

                            Parser.ExpectingSymbol s ->
                                "Expecting " ++ s ++ " at col " ++ String.fromInt err.col

                            _ ->
                                "Invalid string"
                    )
                |> String.join "; "
                |> Err


toString : Range subtype -> String
toString range =
    let
        config =
            range.config
    in
    case range.range of
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



-- OPERATORS
-- No custom operators :(


{-| Equal

    -- > Ok True
    Result.map2 Range.eq
        (Range.create Range.configs.int (Just 1) (Just 5) Nothing)
        (Range.fromString Range.configs.int "[1,4]")

Use this over `(==)` which may cause a runtime error.

-}
eq : Range subtype -> Range subtype -> Bool
eq r1 r2 =
    rangeCompare r1.config r1.range r2.range == EQ


{-| Not Equal

    -- Ok True
    Result.map2 Range.neq
        (Range.create Range.configs.float (Just 1.1) (Just 2.2) Nothing)
        (Range.create Range.configs.float (Just 1.1) (Just 2.3) Nothing)

-}
neq : Range subtype -> Range subtype -> Bool
neq r1 r2 =
    rangeCompare r1.config r1.range r2.range /= EQ


{-| Less Than

    -- Ok True
    Result.map2 Range.lt
        (Range.create Range.configs.int (Just 1) (Just 10) Nothing)
        (Range.create Range.configs.int (Just 2) (Just 3) Nothing)

-}
lt : Range subtype -> Range subtype -> Bool
lt r1 r2 =
    rangeCompare r1.config r1.range r2.range == LT


{-| Greater Than

    -- Ok True
    Result.map2 Range.gt
        (Range.create Range.configs.int (Just 1) (Just 10) Nothing)
        (Range.create Range.configs.int (Just 1) (Just 5) Nothing)

-}
gt : Range subtype -> Range subtype -> Bool
gt r1 r2 =
    rangeCompare r1.config r1.range r2.range == LT


{-| Less Than or Equal

    -- Ok True
    Result.map2 Range.lte
        (Range.create Range.configs.float (Just 1.1) (Just 2.2) Nothing)
        (Range.create Range.configs.float (Just 1.1) (Just 2.2) Nothing)

-}
lte : Range subtype -> Range subtype -> Bool
lte r1 r2 =
    let
        cmp =
            rangeCompare r1.config r1.range r2.range
    in
    cmp == LT || cmp == EQ


{-| Greater Than or Equal

    -- Ok True
    Result.map2 Range.gte
        (Range.create Range.configs.float (Just 1.1) (Just 2.2) Nothing)
        (Range.create Range.configs.float (Just 1.1) (Just 2.0) Nothing)

-}
gte : Range subtype -> Range subtype -> Bool
gte r1 r2 =
    let
        cmp =
            rangeCompare r1.config r1.range r2.range
    in
    cmp == GT || cmp == EQ


{-| Contains Range

    -- Ok True
    Result.map2 Range.cr
        (Range.create Range.configs.int (Just 2) (Just 4) Nothing)
        (Range.create Range.configs.int (Just 2) (Just 3) Nothing)

-}
cr : Range subtype -> Range subtype -> Bool
cr r1 r2 =
    contains r1.config r1.range r2.range


{-| Contains Element

    -- Ok True
    Result.map2 Range.ce
        (Range.fromString Range.configs.timestamp "[2011-01-01,2011-03-01)"
        (Iso8601.toTime "2011-01-10")

-}
ce : Range subtype -> subtype -> Bool
ce range element =
    let
        compare =
            range.config.compare

        eq_ =
            compare element >> (==) EQ

        lt_ =
            compare element >> (==) GT

        gt_ =
            compare element >> (==) LT

        lte_ =
            compare element >> (/=) LT

        gte_ =
            compare element >> (/=) GT
    in
    case range.range of
        Empty ->
            False

        Bounded bounds ->
            case bounds of
                ( Inclusive lower, Inclusive upper ) ->
                    lte_ lower && gt_ upper

                ( Inclusive lower, Exclusive upper ) ->
                    lte_ lower && gte_ upper

                ( Exclusive lower, Inclusive upper ) ->
                    lt_ lower && gt_ upper

                ( Exclusive lower, Exclusive upper ) ->
                    lt_ lower && gte_ upper

                ( Infinite, Inclusive upper ) ->
                    gt_ upper

                ( Infinite, Exclusive upper ) ->
                    gte_ upper

                ( Inclusive lower, Infinite ) ->
                    lte_ lower

                ( Exclusive lower, Infinite ) ->
                    lt_ lower

                ( Infinite, Infinite ) ->
                    True


{-| Overlap (have points in common)

    -- Ok True
    Result.map2 Range.ov
        (Range.fromString Range.configs.int (Just 3) (Just 7) Nothing)
        (Range.fromString Range.configs.int (Just 4) (Just 12) Nothing)

-}
ov : Range subtype -> Range subtype -> Bool
ov r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
        -- An empty range does not overlap any other range
        ( Empty, Empty ) ->
            False

        ( Empty, _ ) ->
            False

        ( _, Empty ) ->
            False

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            let
                ll1 =
                    compareBounds compare ( lower1, LowerBound ) ( lower2, LowerBound )

                lu1 =
                    compareBounds compare ( lower1, LowerBound ) ( upper2, UpperBound )

                ll2 =
                    compareBounds compare ( lower2, LowerBound ) ( lower1, LowerBound )

                lu2 =
                    compareBounds compare ( lower2, LowerBound ) ( upper1, UpperBound )
            in
            if (ll1 == GT || ll1 == EQ) && (lu1 == LT || lu1 == EQ) then
                True

            else if (ll2 == GT || ll2 == EQ) && (lu2 == LT || lu2 == EQ) then
                True

            else
                False


{-| Strictly Left of

    -- Ok True
    Result.map2 Range.sl
        (Range.fromString Range.configs.int (Just 1) (Just 10) Nothing)
        (Range.fromString Range.configs.int (Just 100) (Just 110) Nothing)

-}
sl : Range subtype -> Range subtype -> Bool
sl r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
        -- An empty range does not overlap any other range
        ( Empty, Empty ) ->
            False

        ( Empty, _ ) ->
            False

        ( _, Empty ) ->
            False

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            compareBounds compare ( upper1, UpperBound ) ( lower2, LowerBound ) == LT


{-| Strictly Right of

    -- Ok True
    Result.map2 Range.sr
        (Range.fromString Range.configs.int (Just 50) (Just 60) Nothing)
        (Range.fromString Range.configs.int (Just 20) (Just 30) Nothing)

-}
sr : Range subtype -> Range subtype -> Bool
sr r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
        -- An empty range does not overlap any other range
        ( Empty, Empty ) ->
            False

        ( Empty, _ ) ->
            False

        ( _, Empty ) ->
            False

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            compareBounds compare ( upper1, UpperBound ) ( lower2, LowerBound ) == GT


{-| Does not extend to the right of

    -- Ok True
    Result.map2 Range.nxr
        (Range.fromString Range.configs.int (Just 1) (Just 20) Nothing)
        (Range.fromString Range.configs.int (Just 18) (Just 20) Nothing)

-}
nxr : Range subtype -> Range subtype -> Bool
nxr r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
        -- An empty range does not overlap any other range
        ( Empty, Empty ) ->
            False

        ( Empty, _ ) ->
            False

        ( _, Empty ) ->
            False

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            compareBounds compare ( upper1, UpperBound ) ( upper2, UpperBound ) == GT


{-| Does not extend to the left of

    -- Ok True
    Result.map2 Range.nxl
        (Range.fromString Range.configs.int (Just 7) (Just 20) Nothing)
        (Range.fromString Range.configs.int (Just 5) (Just 10) Nothing)

-}
nxl : Range subtype -> Range subtype -> Bool
nxl r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
        -- An empty range does not overlap any other range
        ( Empty, Empty ) ->
            False

        ( Empty, _ ) ->
            False

        ( _, Empty ) ->
            False

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            compareBounds compare ( lower1, LowerBound ) ( lower2, LowerBound ) == LT



-- FUNCTIONS


lowerElement : Range subtype -> Maybe subtype
lowerElement range =
    case range.range of
        Bounded ( lower, _ ) ->
            boundElement lower

        Empty ->
            Nothing


upperElement : Range subtype -> Maybe subtype
upperElement range =
    case range.range of
        Bounded ( _, upper ) ->
            boundElement upper

        Empty ->
            Nothing


isEmpty : Range subtype -> Bool
isEmpty range =
    case range.range of
        Bounded _ ->
            False

        Empty ->
            True


lowerBoundInclusive : Range subtype -> Bool
lowerBoundInclusive range =
    case range.range of
        Bounded ( Inclusive _, _ ) ->
            True

        _ ->
            False


upperBoundInclusive : Range subtype -> Bool
upperBoundInclusive range =
    case range.range of
        Bounded ( _, Inclusive _ ) ->
            True

        _ ->
            False


lowerBoundInfinite : Range subtype -> Bool
lowerBoundInfinite range =
    case range.range of
        Bounded ( Infinite, _ ) ->
            True

        _ ->
            False


upperBoundInfinite : Range subtype -> Bool
upperBoundInfinite range =
    case range.range of
        Bounded ( _, Infinite ) ->
            True

        _ ->
            False


merge : Range subtype -> Range subtype -> Range subtype
merge range1 range2 =
    let
        config =
            range1.config
    in
    case ( range1.range, range2.range ) of
        ( Empty, Empty ) ->
            empty config

        ( Bounded _, Empty ) ->
            range1

        ( Empty, Bounded _ ) ->
            range2

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            let
                lower =
                    if compareBounds config.compare ( lower1, LowerBound ) ( lower2, LowerBound ) == LT then
                        lower1

                    else
                        lower2

                upper =
                    if compareBounds config.compare ( upper1, UpperBound ) ( upper2, UpperBound ) == GT then
                        upper1

                    else
                        upper2
            in
            Range config <| Bounded ( lower, upper )



-- JSON


{-| RangeInternal JSON decoder
-}
decoder :
    Config subtype
    -> Decode.Decoder (Range subtype)
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
encode : Range subtype -> Encode.Value
encode =
    toString >> Encode.string



-- HELPERS


{-| Construct a range value from bounds and range flags

This does not force canonicalization of the range value. Note that we
perform some datatype-independent canonicalization checks anyway.

-}
serialize :
    (subtype -> subtype -> Order)
    -> ( Maybe subtype, Maybe subtype, ( BoundFlag, BoundFlag ) )
    -> Result String (RangeInternal subtype)
serialize compare ( maybeLower, maybeUpper, ( lowerFlag, upperFlag ) ) =
    let
        boundedRange =
            Bounded
                ( maybeLower
                    |> Maybe.map (flagToBound lowerFlag)
                    |> Maybe.withDefault Infinite
                , maybeUpper
                    |> Maybe.map (flagToBound upperFlag)
                    |> Maybe.withDefault Infinite
                )
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


{-| Serialize and canonicalize (if applicable) the range
-}
make :
    Config subtype
    -> ( Maybe subtype, Maybe subtype, ( BoundFlag, BoundFlag ) )
    -> Result String (RangeInternal subtype)
make { compare, canonical } rangeParts =
    rangeParts
        |> serialize compare
        |> Result.andThen
            (canonical
                |> Maybe.map (canonicalize compare)
                |> Maybe.withDefault Ok
            )


canonicalize :
    (subtype -> subtype -> Order)
    -> Canonical subtype
    -> RangeInternal subtype
    -> Result String (RangeInternal subtype)
canonicalize compare canonical range =
    let
        empty_ =
            case range of
                Bounded _ ->
                    False

                Empty ->
                    True
    in
    if empty_ then
        Ok range

    else
        let
            ( lowerVal, upperVal, ( lowerFlag, upperFlag ) ) =
                deserialize range

            lower =
                Maybe.map (stepVal (Tuple.first canonical) lowerFlag) lowerVal

            upper =
                Maybe.map (stepVal (Tuple.second canonical) upperFlag) upperVal
        in
        serialize compare
            ( lower
            , upper
            , ( (Tuple.first >> Tuple.first) canonical
              , (Tuple.second >> Tuple.first) canonical
              )
            )


stepVal : ( BoundFlag, subtype -> subtype ) -> BoundFlag -> subtype -> subtype
stepVal ( canonicalFlag, step ) specifiedFlag val =
    if specifiedFlag == canonicalFlag then
        val

    else
        step val


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


parser : Config subtype -> Parser ( Maybe subtype, Maybe subtype, ( BoundFlag, BoundFlag ) )
parser config =
    let
        cast str =
            if String.isEmpty str then
                Parser.succeed Nothing

            else
                case config.fromString str of
                    Ok val ->
                        Parser.succeed (Just val)

                    Err error ->
                        Parser.problem error

        lowerBoundParser =
            Parser.succeed Tuple.pair
                |= Parser.oneOf
                    [ Parser.succeed Inc
                        |. Parser.symbol "["
                    , Parser.succeed Exc
                        |. Parser.symbol "("
                    ]
                |= (Parser.getChompedString (Parser.chompUntil ",") |> Parser.andThen cast)

        upperBoundParser =
            Parser.succeed Tuple.pair
                |= (Parser.getChompedString
                        (Parser.oneOf
                            [ Parser.chompUntil ")"
                            , Parser.chompUntil "]"
                            ]
                        )
                        |> Parser.andThen cast
                   )
                |= Parser.oneOf
                    [ Parser.succeed Inc
                        |. Parser.symbol "]"
                    , Parser.succeed Exc
                        |. Parser.symbol ")"
                    ]
    in
    Parser.succeed (\( lowerFlag, lower ) ( upper, upperFlag ) -> ( lower, upper, ( lowerFlag, upperFlag ) ))
        |= lowerBoundParser
        |. Parser.symbol ","
        |= upperBoundParser


boundElement : Bound subtype -> Maybe subtype
boundElement bound =
    case bound of
        Inclusive val ->
            Just val

        Exclusive val ->
            Just val

        Infinite ->
            Nothing


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


contains : Config subtype -> RangeInternal subtype -> RangeInternal subtype -> Bool
contains { compare } r1 r2 =
    case ( r1, r2 ) of
        ( Empty, Empty ) ->
            True

        ( Empty, _ ) ->
            False

        ( _, Empty ) ->
            True

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            let
                lowerBoundOrder =
                    compareBounds compare ( lower1, LowerBound ) ( lower2, LowerBound )

                upperBoundOrder =
                    compareBounds compare ( upper1, UpperBound ) ( upper2, UpperBound )
            in
            if lowerBoundOrder == GT then
                False

            else if upperBoundOrder == LT then
                False

            else
                True
