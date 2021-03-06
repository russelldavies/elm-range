module Range exposing
    ( Range, TypeConfig, Canonical
    , empty, create, createWith, BoundFlag(..), fromString, types
    , toString, decoder, encode
    , eq, neq, lt, gt, le, ge, cr, ce, ov, sl, sr, nxr, nxl, adj, union
    , intersect, diff
    , lowerElement, upperElement, isEmpty, lowerBoundInclusive
    , upperBoundInclusive, lowerBoundInfinite, upperBoundInfinite, merge
    )

{-| Model and operate on a range of values in Elm.


# Definition

@docs Range, TypeConfig, Canonical


# Creation

@docs empty, create, createWith, BoundFlag, fromString, types


# Serialization

@docs toString, decoder, encode


# Operators

@docs eq, neq, lt, gt, le, ge, cr, ce, ov, sl, sr, nxr, nxl, adj, union
@docs intersect, diff


# Functions

@docs lowerElement, upperElement, isEmpty, lowerBoundInclusive
@docs upperBoundInclusive, lowerBoundInfinite, upperBoundInfinite, merge

-}

import Comparison
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
import Time



-- MODELS


{-| A range of a certain type that stores information about its bounds.

Be aware that a Range stores functions internally. If you want to use `(==)`
for comparing two Ranges use the [eq](#eq) operator.

-}
type alias Range subtype =
    { config : TypeConfig subtype
    , range : RangeInternal subtype
    }


{-| When you need to use a custom type you must create an instance of this.

This acts as a value level typeclass.

-}
type alias TypeConfig subtype =
    { toString : subtype -> String
    , fromString : String -> Result String subtype
    , compare : subtype -> subtype -> Order
    , canonical : Maybe (Canonical subtype)
    }


{-| A discrete range type should have a canonicalization function that is aware of
the desired step size for the element type. The canonicalization function is
charged with converting equivalent values of the range type to have
identical representations, in particular consistently inclusive or exclusive
bounds. If a canonicalization function is not specified, then ranges with
different formatting will always be treated as unequal, even though they
might represent the same set of values in reality.

Here is the definition for an integer range:

    ( ( Inc, (+) 1 ), ( Exc, (+) 1 ) )

It specifies that the lower bound should be inclusive and the upper exclusive.
If the supplied bound flags are different during creating then step the value by

-}
type alias Canonical subtype =
    ( ( BoundFlag, subtype -> subtype ), ( BoundFlag, subtype -> subtype ) )


type RangeInternal subtype
    = Bounded ( Bound subtype, Bound subtype )
    | Empty


type Bound subtype
    = Exclusive subtype
    | Inclusive subtype
    | Infinite


{-| Used internally for deserialization
-}
type BoundType
    = LowerBound
    | UpperBound



-- TYPE CLASSES


{-| Built-in [TypeConfigs](#TypeConfig) for common types.
-}
types :
    { int : TypeConfig Int
    , float : TypeConfig Float
    , string : TypeConfig String
    , timestamp : TypeConfig Time.Posix
    }
types =
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


{-| Create an empty range

    Range.empty Range.types.int

-}
empty : TypeConfig subtype -> Range subtype
empty config =
    Range config Empty


{-| Create a range in standard form (lower bound inclusive, upper bound
exclusive).

    Range.create Range.types.float (Just 1.2) Nothing

-}
create :
    TypeConfig subtype
    -> Maybe subtype
    -> Maybe subtype
    -> Result String (Range subtype)
create config lower upper =
    createWith config lower upper Nothing


{-| Create a range with whatever user specified flags.

    Range.create Range.types.float (Just 1.5) (Just 12.2) (Just ( Range.Inc, Range.Inc ))

-}
createWith :
    TypeConfig subtype
    -> Maybe subtype
    -> Maybe subtype
    -> Maybe ( BoundFlag, BoundFlag )
    -> Result String (Range subtype)
createWith config lower upper flags =
    let
        defaultFlags =
            ( Inc, Exc )
    in
    make config ( lower, upper, Maybe.withDefault defaultFlags flags )
        |> Result.map (Range config)


{-| The kind of bounds, inclusive or exclusive, to be supplied to
[createWith](#createWith).
-}
type BoundFlag
    = Inc
    | Exc


{-| Create a range from a string.

    Range.fromString Range.types.int "(1,5)"

-}
fromString :
    TypeConfig subtype
    -> String
    -> Result String (Range subtype)
fromString config str =
    case Parser.run (parser config) str of
        Ok maybeRangeParts ->
            case maybeRangeParts of
                Just rangeParts ->
                    make config rangeParts |> Result.map (Range config)

                Nothing ->
                    Ok (empty config)

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


{-| Convert a range to its string representation.

    -- Ok [1,2)
    Range.create Range.types.int (Just 1) (Just 2)
        |> Result.map Range.toString

-}
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
        (Range.create Range.types.int (Just 1) (Just 5))
        (Range.fromString Range.types.int "[1,4]")

Use this over `(==)` which may cause a runtime error.

-}
eq : Range subtype -> Range subtype -> Bool
eq r1 r2 =
    rangeCompare r1.config r1.range r2.range == EQ


{-| Not Equal

    -- Ok True
    Result.map2 Range.neq
        (Range.create Range.types.float (Just 1.1) (Just 2.2))
        (Range.create Range.types.float (Just 1.1) (Just 2.3))

-}
neq : Range subtype -> Range subtype -> Bool
neq r1 r2 =
    rangeCompare r1.config r1.range r2.range /= EQ


{-| Less Than

    -- Ok True
    Result.map2 Range.lt
        (Range.create Range.types.int (Just 1) (Just 10))
        (Range.create Range.types.int (Just 2) (Just 3))

-}
lt : Range subtype -> Range subtype -> Bool
lt r1 r2 =
    rangeCompare r1.config r1.range r2.range == LT


{-| Greater Than

    -- Ok True
    Result.map2 Range.gt
        (Range.create Range.types.int (Just 1) (Just 10))
        (Range.create Range.types.int (Just 1) (Just 5))

-}
gt : Range subtype -> Range subtype -> Bool
gt r1 r2 =
    rangeCompare r1.config r1.range r2.range == GT


{-| Less Than or Equal

    -- Ok True
    Result.map2 Range.lte
        (Range.create Range.types.float (Just 1.1) (Just 2.2))
        (Range.create Range.types.float (Just 1.1) (Just 2.2))

-}
le : Range subtype -> Range subtype -> Bool
le r1 r2 =
    let
        cmp =
            rangeCompare r1.config r1.range r2.range
    in
    cmp == LT || cmp == EQ


{-| Greater Than or Equal

    -- Ok True
    Result.map2 Range.gte
        (Range.create Range.types.float (Just 1.1) (Just 2.2))
        (Range.create Range.types.float (Just 1.1) (Just 2.0))

-}
ge : Range subtype -> Range subtype -> Bool
ge r1 r2 =
    let
        cmp =
            rangeCompare r1.config r1.range r2.range
    in
    cmp == GT || cmp == EQ


{-| Contains Range

    -- Ok True
    Result.map2 Range.cr
        (Range.create Range.types.int (Just 2) (Just 4))
        (Range.create Range.types.int (Just 2) (Just 3))

-}
cr : Range subtype -> Range subtype -> Bool
cr r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
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
            -- lower1 <= lower2 and upper1 >= upper2
            lowerBoundOrder /= GT && upperBoundOrder /= LT


{-| Contains Element

    -- Ok True
    Result.map2 Range.ce
        (Range.fromString Range.types.timestamp "[2011-01-01,2011-03-01)"
        (Iso8601.toTime "2011-01-10")

-}
ce : Range subtype -> subtype -> Bool
ce range elem =
    let
        c =
            Comparison.instance range.config.compare
    in
    case range.range of
        Empty ->
            False

        Bounded bounds ->
            case bounds of
                ( Inclusive lower, Inclusive upper ) ->
                    c.le lower elem && c.le elem upper

                ( Inclusive lower, Exclusive upper ) ->
                    c.le lower elem && c.lt elem upper

                ( Exclusive lower, Inclusive upper ) ->
                    c.lt lower elem && c.le elem upper

                ( Exclusive lower, Exclusive upper ) ->
                    c.lt lower elem && c.lt elem upper

                ( Infinite, Inclusive upper ) ->
                    c.le elem upper

                ( Infinite, Exclusive upper ) ->
                    c.lt elem upper

                ( Inclusive lower, Infinite ) ->
                    c.le lower elem

                ( Exclusive lower, Infinite ) ->
                    c.lt lower elem

                ( Infinite, Infinite ) ->
                    True


{-| Overlap (have points in common)

    -- Ok True
    Result.map2 Range.ov
        (Range.create Range.types.int (Just 3) (Just 7))
        (Range.create Range.types.int (Just 4) (Just 12))

-}
ov : Range subtype -> Range subtype -> Bool
ov r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
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

        _ ->
            False


{-| Strictly Left of

    -- Ok True
    Result.map2 Range.sl
        (Range.create Range.types.int (Just 1) (Just 10))
        (Range.create Range.types.int (Just 100) (Just 110))

-}
sl : Range subtype -> Range subtype -> Bool
sl r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            compareBounds compare ( upper1, UpperBound ) ( lower2, LowerBound ) == LT

        _ ->
            False


{-| Strictly Right of

    -- Ok True
    Result.map2 Range.sr
        (Range.create Range.types.int (Just 50) (Just 60))
        (Range.create Range.types.int (Just 20) (Just 30))

-}
sr : Range subtype -> Range subtype -> Bool
sr r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            compareBounds compare ( lower1, LowerBound ) ( upper2, UpperBound ) == GT

        _ ->
            False


{-| Does not extend to the right of

    -- Ok True
    Result.map2 Range.nxr
        (Range.create Range.types.int (Just 1) (Just 20))
        (Range.create Range.types.int (Just 18) (Just 20))

-}
nxr : Range subtype -> Range subtype -> Bool
nxr r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            -- upper1 <= upper2
            compareBounds compare ( upper1, UpperBound ) ( upper2, UpperBound ) /= GT

        _ ->
            False


{-| Does not extend to the left of

    -- Ok True
    Result.map2 Range.nxl
        (Range.create Range.types.int (Just 7) (Just 20))
        (Range.create Range.types.int (Just 5) (Just 10))

-}
nxl : Range subtype -> Range subtype -> Bool
nxl r1 r2 =
    let
        compare =
            r1.config.compare
    in
    case ( r1.range, r2.range ) of
        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            -- lower1 >= lower2
            compareBounds compare ( lower1, LowerBound ) ( lower2, LowerBound ) /= LT

        _ ->
            False


{-| Is Adjacent to

    -- Ok True
    Result.map2 Range.adj
        (Range.create Range.types.float (Just 1.1) (Just 2.2))
        (Range.create Range.types.float (Just 2.2) (Just 3.3))

-}
adj : Range subtype -> Range subtype -> Bool
adj r1 r2 =
    case ( r1.range, r2.range ) of
        -- An empty range is not adjacent to any other range
        ( Empty, Empty ) ->
            False

        ( Empty, _ ) ->
            False

        ( _, Empty ) ->
            False

        {- Given two ranges A..B and C..D, the ranges are adjacent if and only
           if B is adjacent to C, or D is adjacent to A.

           A..B
             -|-
              C..D
                -|-
                 A..B
        -}
        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            let
                config =
                    r1.config
            in
            boundsAdjacent config upper1 lower2 || boundsAdjacent config upper2 lower1


{-| Union

    -- Ok "[5,20)"
    Result.map2 Range.adj
        (Range.create Range.types.float (Just 5) (Just 15))
        (Range.create Range.types.float (Just 10) (Just 20))
        |> Result.map Range.toString

-}
union : Range subtype -> Range subtype -> Result String (Range subtype)
union r1 r2 =
    let
        config =
            r1.config
    in
    case ( r1.range, r2.range ) of
        -- If either is empty then the other is the correct answer
        ( Empty, Empty ) ->
            Ok r1

        ( Empty, _ ) ->
            Ok r2

        ( _, Empty ) ->
            Ok r1

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            if not (ov r1 r2) && not (adj r1 r2) then
                Err "Result of range union would not be contiguous"

            else
                let
                    ( lowerVal, lowerFlag ) =
                        boundToValFlag
                            (case compareBounds config.compare ( lower1, LowerBound ) ( lower2, LowerBound ) of
                                LT ->
                                    lower1

                                _ ->
                                    lower2
                            )

                    ( upperVal, upperFlag ) =
                        boundToValFlag
                            (case compareBounds config.compare ( upper1, UpperBound ) ( upper2, UpperBound ) of
                                GT ->
                                    upper1

                                _ ->
                                    upper2
                            )
                in
                make config ( lowerVal, upperVal, ( lowerFlag, upperFlag ) )
                    |> Result.map (Range config)


{-| Intersection

    -- Ok [10,15)
    Result.map2 Range.intersect
        (Range.create Range.types.float (Just 5) (Just 15))
        (Range.create Range.types.float (Just 10) (Just 20))
        |> Result.map Range.toString

-}
intersect : Range subtype -> Range subtype -> Range subtype
intersect r1 r2 =
    let
        config =
            r1.config
    in
    case ( r1.range, r2.range ) of
        ( Empty, Empty ) ->
            empty config

        ( Empty, _ ) ->
            empty config

        ( _, Empty ) ->
            empty config

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            if not (ov r1 r2) then
                empty config

            else
                let
                    ( lowerVal, lowerFlag ) =
                        boundToValFlag
                            (case compareBounds config.compare ( lower1, LowerBound ) ( lower2, LowerBound ) of
                                LT ->
                                    lower2

                                _ ->
                                    lower1
                            )

                    ( upperVal, upperFlag ) =
                        boundToValFlag
                            (case compareBounds config.compare ( upper1, UpperBound ) ( upper2, UpperBound ) of
                                GT ->
                                    upper2

                                _ ->
                                    upper1
                            )
                in
                make config ( lowerVal, upperVal, ( lowerFlag, upperFlag ) )
                    |> Result.map (Range config)
                    |> Result.withDefault (empty config)


{-| Difference

    -- Ok [5,10)
    Result.map2 Range.intersect
        (Range.create Range.types.float (Just 5) (Just 15))
        (Range.create Range.types.float (Just 10) (Just 20))
        |> Result.toString

-}
diff : Range subtype -> Range subtype -> Result String (Range subtype)
diff r1 r2 =
    let
        config =
            r1.config
    in
    case ( r1.range, r2.range ) of
        -- If either is empty, r1 is the correct answer
        ( Empty, Empty ) ->
            Ok r1

        ( Empty, _ ) ->
            Ok r1

        ( _, Empty ) ->
            Ok r1

        ( Bounded ( lower1, upper1 ), Bounded ( lower2, upper2 ) ) ->
            let
                l1l2 =
                    compareBounds config.compare ( lower1, LowerBound ) ( lower2, LowerBound )

                l1u2 =
                    compareBounds config.compare ( lower1, LowerBound ) ( upper2, UpperBound )

                u1l2 =
                    compareBounds config.compare ( upper1, UpperBound ) ( lower2, LowerBound )

                u1u2 =
                    compareBounds config.compare ( upper1, UpperBound ) ( upper2, UpperBound )
            in
            if l1l2 == LT && u1u2 == GT then
                Err "Result of range difference would not be contiguous"

            else if l1u2 == GT || u1l2 == LT then
                Ok r1

            else if l1l2 /= LT && u1u2 /= GT then
                Ok (empty config)

            else if l1l2 /= GT && u1l2 /= LT && u1u2 /= GT then
                let
                    ( lowerVal, lowerFlag ) =
                        boundToValFlag lower1

                    ( upperVal, upperFlag ) =
                        boundToValFlag lower2
                in
                make config ( lowerVal, upperVal, ( lowerFlag, flipBoundFlag upperFlag ) )
                    |> Result.map (Range config)

            else if l1l2 /= LT && u1u2 /= LT && l1u2 /= GT then
                let
                    ( lowerVal, lowerFlag ) =
                        boundToValFlag upper2

                    ( upperVal, upperFlag ) =
                        boundToValFlag upper1
                in
                make config ( lowerVal, upperVal, ( flipBoundFlag lowerFlag, upperFlag ) )
                    |> Result.map (Range config)

            else
                Err "unexpected case"



-- FUNCTIONS


{-| The element of a range's lower bound.

    -- Ok (Just 1.1)
    Range.create Range.types.float (Just 1.1) (Just 2.2)
        |> Result.map Range.lowerElement

If the range is empty or the bound infinity then the result is `Nothing`.

-}
lowerElement : Range subtype -> Maybe subtype
lowerElement range =
    case range.range of
        Bounded ( lower, _ ) ->
            boundElement lower

        Empty ->
            Nothing


{-| The element of a range's upper bound.

    -- Ok (Just 2.2)
    Range.create Range.types.float (Just 1.1) (Just 2.2)
        |> Result.map Range.upperElement

If the range is empty or the bound infinity then the result is `Nothing`.

-}
upperElement : Range subtype -> Maybe subtype
upperElement range =
    case range.range of
        Bounded ( _, upper ) ->
            boundElement upper

        Empty ->
            Nothing


{-| Is the range empty?

    -- Ok False
    Range.create Range.types.float (Just 1.1) (Just 2.2)
        |> Result.map Range.isEmpty

-}
isEmpty : Range subtype -> Bool
isEmpty range =
    case range.range of
        Bounded _ ->
            False

        Empty ->
            True


{-| Is the lower bound inclusive.

    -- Ok True
    Range.create Range.types.float (Just 1.1) (Just 2.2)
        |> Result.map Range.lowerBoundInclusive

If a range is empty or the lower bound is infinite then it is not inclusive.

-}
lowerBoundInclusive : Range subtype -> Bool
lowerBoundInclusive range =
    case range.range of
        Bounded ( Inclusive _, _ ) ->
            True

        _ ->
            False


{-| Is the upper bound inclusive.

    -- Ok False
    Range.create Range.types.float (Just 1.1) (Just 2.2)
        |> Result.map Range.upperBoundInclusive

If a range is empty or the upper bound is infinite then it is not inclusive.

-}
upperBoundInclusive : Range subtype -> Bool
upperBoundInclusive range =
    case range.range of
        Bounded ( _, Inclusive _ ) ->
            True

        _ ->
            False


{-| Is the lower bound infinite.

    -- Ok True
    Range.create Range.types.float Nothing (Just 2.2)
        |> Result.map Range.lowerBoundInfinite

If a range is empty then it is not infinite.

-}
lowerBoundInfinite : Range subtype -> Bool
lowerBoundInfinite range =
    case range.range of
        Bounded ( Infinite, _ ) ->
            True

        _ ->
            False


{-| Is the upper bound infinite.

    -- Ok False
    Range.create Range.types.float Nothing (Just 2.2)
        |> Result.map Range.upperBoundInfinite

If a range is empty then it is not infinite.

-}
upperBoundInfinite : Range subtype -> Bool
upperBoundInfinite range =
    case range.range of
        Bounded ( _, Infinite ) ->
            True

        _ ->
            False


{-| The smallest range which includes both of the given ranges.

Like set union, except also allow and account for non-adjacent input ranges.

    -- Ok "[1,4)"
    Result.map2 Range.merge
        (Range.create Range.types.int (Just 1) (Just 2))
        (Range.create Range.types.int (Just 3) (Just 4))
        |> Result.map Range.toString

-}
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


{-| JSON decoder
-}
decoder :
    TypeConfig subtype
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


{-| Encode a `Range` to JSON
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
    TypeConfig subtype
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


boundToValFlag : Bound subtype -> ( Maybe subtype, BoundFlag )
boundToValFlag bound =
    case bound of
        Exclusive val ->
            ( Just val, Exc )

        Inclusive val ->
            ( Just val, Inc )

        Infinite ->
            ( Nothing, Exc )


parser : TypeConfig subtype -> Parser (Maybe ( Maybe subtype, Maybe subtype, ( BoundFlag, BoundFlag ) ))
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
    Parser.oneOf
        [ Parser.keyword "empty" |> Parser.map (always Nothing)
        , Parser.succeed (\( lowerFlag, lower ) ( upper, upperFlag ) -> Just ( lower, upper, ( lowerFlag, upperFlag ) ))
            |= lowerBoundParser
            |. Parser.symbol ","
            |= upperBoundParser
        ]


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


{-| Similar to `compareBounds` but simpler as we just compare the values
held in the bounds, ignoring inclusive/exclusive flags. The lower/upper flags
only mater for infinities, where they tell us if the infinity is plus or minus.
-}
compareBoundValues :
    (subtype -> subtype -> Order)
    -> ( Bound subtype, BoundType )
    -> ( Bound subtype, BoundType )
    -> Order
compareBoundValues compare ( bound1, bound1Type ) ( bound2, bound2Type ) =
    case ( bound1, bound2 ) of
        -- Both infinity, so they are equal unless one is lower and other not
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
            compare bound1Val bound2Val

        ( Exclusive bound1Val, Inclusive bound2Val ) ->
            compare bound1Val bound2Val

        ( Inclusive bound1Val, Exclusive bound2Val ) ->
            compare bound1Val bound2Val

        ( Inclusive bound1Val, Inclusive bound2Val ) ->
            compare bound1Val bound2Val


rangeCompare : TypeConfig subtype -> RangeInternal subtype -> RangeInternal subtype -> Order
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


{-| Check if two bounds A and B are "adjacent", where A is an upper bound and B
is a lower bound. For the bounds to be adjacent, each subtype value must
satisfy strictly one of the bounds: there are no values which satisfy both
bounds (i.e. less than A and greater than B); and there are no values which
satisfy neither bound (i.e. greater than A and less than B).

For discrete ranges, we rely on the canonicalization function to see if A..B
normalizes to empty. (If there is no canonicalization function, it's impossible
for such a range to normalize to empty, so we needn't bother to try.)

If A == B, the ranges are adjacent only if the bounds have different flags
(incluisve, exclusive); i.e., exactly one of the ranges includes the common
boundary point.

And if A > B then the ranges are not adjacent in this order.

-}
boundsAdjacent :
    TypeConfig subtype
    -> Bound subtype
    -> Bound subtype
    -> Bool
boundsAdjacent config boundA boundB =
    case compareBoundValues config.compare ( boundA, UpperBound ) ( boundB, LowerBound ) of
        -- Bounds don't overlap; see if there are points in between
        LT ->
            if config.canonical == Nothing then
                -- In a continuous subtype, there are assumed to be points between
                False

            else
                -- The bounds are of a discrete range type; so make a range
                -- A..B and see if it's empty
                let
                    ( boundAVal, boundAFlag ) =
                        boundToValFlag boundA

                    ( boundBVal, boundBFlag ) =
                        boundToValFlag boundB
                in
                make config
                    ( boundAVal
                    , boundBVal
                    , ( flipBoundFlag boundAFlag, flipBoundFlag boundBFlag )
                    )
                    |> Result.map
                        (\r ->
                            case r of
                                Bounded _ ->
                                    False

                                Empty ->
                                    True
                        )
                    |> Result.withDefault False

        EQ ->
            isBoundInclusive boundA /= isBoundInclusive boundB

        -- Bounds overlap
        GT ->
            False


isBoundInclusive bound =
    case bound of
        Inclusive _ ->
            True

        _ ->
            False


flipBoundFlag : BoundFlag -> BoundFlag
flipBoundFlag flag =
    case flag of
        Inc ->
            Exc

        Exc ->
            Inc
