module Int exposing (canonical, checkBounds, equal, isEmpty)

import Expect exposing (Expectation)
import Range exposing (Range)
import Range.Int
import Range.Int.Fuzz as Fuzz
import Test exposing (..)



-- Type specific


canonical : Test
canonical =
    describe "Verifies Int canonical function"
        [ fuzz2 Fuzz.validMaybeIntPair Fuzz.boundFlagPair "Random bound values and flags" <|
            \( maybeLowerElement, maybeUpperElement ) (( lowerBoundFlag, upperBoundFlag ) as boundFlags) ->
                case Range.Int.create maybeLowerElement maybeUpperElement (Just boundFlags) of
                    Ok range ->
                        let
                            bothInclusive =
                                lowerBoundFlag == Range.Inc && upperBoundFlag == Range.Inc

                            bothExclusive =
                                lowerBoundFlag == Range.Exc && upperBoundFlag == Range.Exc
                        in
                        case ( maybeLowerElement, maybeUpperElement ) of
                            ( Just lowerElement, Just upperElement ) ->
                                if
                                    (upperElement == lowerElement && not bothInclusive)
                                        || (upperElement - lowerElement == 1 && bothExclusive)
                                then
                                    Expect.true "Expected empty range" (Range.isEmpty range)

                                else
                                    Expect.all
                                        [ expectedLowerBoundInclusive
                                        , expectedUpperBoundExclusive
                                        , upperElementExpectation upperElement upperBoundFlag
                                        , lowerElementExpectation lowerElement lowerBoundFlag
                                        ]
                                        range

                            ( Nothing, Just upperElement ) ->
                                Expect.all
                                    [ expectedUpperBoundExclusive
                                    , expectedLowerBoundExclusive
                                    , upperElementExpectation upperElement upperBoundFlag
                                    ]
                                    range

                            ( Just lowerElement, Nothing ) ->
                                Expect.all
                                    [ expectedUpperBoundExclusive
                                    , expectedLowerBoundInclusive
                                    , lowerElementExpectation lowerElement lowerBoundFlag
                                    ]
                                    range

                            ( Nothing, Nothing ) ->
                                Expect.all
                                    [ expectedLowerBoundExclusive
                                    , expectedUpperBoundExclusive
                                    ]
                                    range

                    Err err ->
                        Expect.fail err
        ]



-- Type generic


checkBounds : Test
checkBounds =
    fuzz Fuzz.intPair "Check range bounds: lower must be less than or equal to upper" <|
        \( lower, upper ) ->
            case Range.Int.create (Just lower) (Just upper) Nothing of
                Ok range ->
                    case compare lower upper of
                        LT ->
                            Expect.false "Expected range not to be empty" (Range.isEmpty range)

                        EQ ->
                            Expect.true "Expected range to be empty" (Range.isEmpty range)

                        GT ->
                            Expect.fail "Valid bounds"

                Err err ->
                    case compare lower upper of
                        LT ->
                            Expect.fail "Valid bounds, should be a range"

                        EQ ->
                            Expect.fail "Valid bounds, should be empty"

                        GT ->
                            Expect.equal err "Lower bound must be less than or equal to upper bound"


isEmpty : Test
isEmpty =
    let
        createRange bounds =
            Range.Int.create (Just 1) (Just 1) bounds
                |> Result.map Range.isEmpty
    in
    describe "isEmpty"
        [ test "inc-inc" <|
            \_ ->
                Just ( Range.Inc, Range.Inc )
                    |> createRange
                    |> Expect.equal (Ok False)
        , test "inc-exc" <|
            \_ ->
                Just ( Range.Inc, Range.Exc )
                    |> createRange
                    |> Expect.equal (Ok True)
        , test "exc-exc" <|
            \_ ->
                Just ( Range.Exc, Range.Exc )
                    |> createRange
                    |> Expect.equal (Ok True)
        , test "exc-inc" <|
            \_ ->
                Just ( Range.Exc, Range.Inc )
                    |> createRange
                    |> Expect.equal (Ok True)
        ]


equal : Test
equal =
    let
        r1 =
            Range.Int.create (Just 1) (Just 2) Nothing
                |> Result.withDefault Range.empty

        r2 =
            Range.Int.create (Just 11) (Just 12) Nothing
                |> Result.withDefault Range.empty
    in
    describe "equal"
        [ test "both empty" <|
            \_ ->
                Range.Int.equal Range.empty Range.empty
                    |> Expect.true "Both empty should be true"
        , describe "one empty"
            [ test "first empty" <|
                \_ ->
                    Range.Int.equal Range.empty r1
                        |> Expect.false "Empty and non-empty should be false"
            , test "second empty" <|
                \_ ->
                    Range.Int.equal r1 Range.empty
                        |> Expect.false "Empty and non-empty should be false"
            ]
        , describe "both filled"
            [ test "same values" <|
                \_ ->
                    Range.Int.equal r1 r1
                        |> Expect.true "both filled should be true with same values"
            , test "different values" <|
                \_ ->
                    Range.Int.equal r1 r2
                        |> Expect.false "both filled with different values should be false"
            ]
        ]



{-
   lessThan : Test
   lessThan =
       let
           r1 =
               create (Just 1) (Just 2)

           r2 =
               Range.Int.create (Just 2) (Just 3) Nothing
                   |> Result.withDefault Range.empty
       in
       describe "lessThan"
           [ test "both empty" <|
               \_ ->
                   Range.Int.lessThan Range.empty Range.empty
                       |> Expect.false "Both empty should be false"
           , describe "one empty"
               [ fuzz nonEmptyRange "first empty" <|
                   \range ->
                       Range.Int.lessThan Range.empty range
                           |> Expect.true "Empty should always be less"
               , test "second empty" <|
                   \_ ->
                       Range.Int.lessThan r1 Range.empty
                           |> Expect.false "Bounded and empty should be false"
               ]
           , describe "both filled"
               [ test "same values" <|
                   \_ ->
                       Range.Int.lessThan r1 r1
                           |> Expect.false "the same so not true"
               , test "lower bound less, upper bound greater" <|
                   \_ ->
                       Range.Int.lessThan r1 r2
                           |> Expect.true "should be true"
               , test "lower bounds equal, upper bound greater" <|
                   \_ ->
                       Range.Int.lessThan
                           (create (Just 1) (Just 2))
                           (create (Just 1) (Just 5))
                           |> Expect.true "should be true"
               ]
           ]
-}
-- HELPERS


expectedLowerBoundInclusive : Range subtype -> Expectation
expectedLowerBoundInclusive =
    Expect.true "Expected lower bound inclusive" << Range.lowerBoundInclusive


expectedLowerBoundExclusive : Range subtype -> Expectation
expectedLowerBoundExclusive =
    Expect.false "Expected lower bound exclusive" << Range.lowerBoundInclusive


expectedUpperBoundInclusive : Range subtype -> Expectation
expectedUpperBoundInclusive =
    Expect.true "Expected upper bound inclusive" << Range.upperBoundInclusive


expectedUpperBoundExclusive : Range subtype -> Expectation
expectedUpperBoundExclusive =
    Expect.false "Expected upper bound exclusive" << Range.upperBoundInclusive


upperElementExpectation : Int -> Range.BoundFlag -> Range Int -> Expectation
upperElementExpectation element boundFlag =
    Range.upperElement
        >> Maybe.map (Expect.equal (canonicalize boundFlag Range.Exc element))
        >> Maybe.withDefault (Expect.fail "Upper bound missing")


lowerElementExpectation : Int -> Range.BoundFlag -> Range Int -> Expectation
lowerElementExpectation element boundFlag =
    Range.lowerElement
        >> Maybe.map (Expect.equal (canonicalize boundFlag Range.Inc element))
        >> Maybe.withDefault (Expect.fail "Lower bound missing")


canonicalize : Range.BoundFlag -> Range.BoundFlag -> Int -> Int
canonicalize flag expectedFlag el =
    if flag == expectedFlag then
        el

    else
        el + 1
