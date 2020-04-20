module Int exposing
    ( canonical
    , ce
    , checkBounds
    , cr
    , eq
    , fromString
    , gt
    , isEmpty
    , lt
    , neq
    , toString
    )

import Expect exposing (Expectation)
import Fuzz
import Range exposing (Range, types)
import Range.Int.Fuzz as IntFuzz
import Test exposing (..)



-- Type specific


canonical : Test
canonical =
    describe "Verifies Int canonical function"
        [ fuzz2 IntFuzz.validMaybeIntPair IntFuzz.boundFlagPair "Random bound values and flags" <|
            \(( maybeLowerElement, maybeUpperElement ) as elements) boundFlags ->
                Range.create types.int maybeLowerElement maybeUpperElement (Just boundFlags)
                    |> Result.map (validRange elements boundFlags)
                    |> resultFailErr
        ]



-- CREATION


checkBounds : Test
checkBounds =
    fuzz IntFuzz.intPair "Check range bounds: lower must be less than or equal to upper" <|
        \( lower, upper ) ->
            case Range.create types.int (Just lower) (Just upper) Nothing of
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
                            Expect.equal err "Range lower bound must be less than or equal to range upper bound"


fromString : Test
fromString =
    let
        elementToString =
            Maybe.map String.fromInt >> Maybe.withDefault ""
    in
    describe "Creation from string parsing"
        [ fuzz2 IntFuzz.validMaybeIntPair IntFuzz.boundFlagCharPair "Random bound values and flags" <|
            \(( maybeLowerElement, maybeUpperElement ) as elements) ( lowerBoundFlagChar, upperBoundFlagChar ) ->
                (String.fromChar lowerBoundFlagChar
                    ++ elementToString maybeLowerElement
                    ++ ","
                    ++ elementToString maybeUpperElement
                    ++ String.fromChar upperBoundFlagChar
                )
                    |> Range.fromString types.int
                    |> Result.map
                        (validRange elements
                            ( if lowerBoundFlagChar == '[' then
                                Range.Inc

                              else
                                Range.Exc
                            , if upperBoundFlagChar == ']' then
                                Range.Inc

                              else
                                Range.Exc
                            )
                        )
                    |> Result.withDefault (Expect.fail "Invalid")
        , fuzz Fuzz.string "Random string that should fail" <|
            (Range.fromString types.int
                >> Result.map (always (Expect.fail "Created range with invalid string"))
                >> Result.withDefault Expect.pass
            )
        , test "Empty range" <|
            \_ ->
                Range.fromString types.int "empty"
                    |> Result.map (Range.isEmpty >> Expect.true "`empty` is valid")
                    |> resultFailErr
        ]


toString : Test
toString =
    describe "Convert valid range to string"
        [ test "Empty range" <|
            \_ -> Range.empty types.int |> Range.toString |> Expect.equal "empty"
        , fuzz IntFuzz.rangeString "Restore the original string" <|
            \rangeStr ->
                -- Can use fromString as it's been tested already
                rangeStr
                    |> Range.fromString types.int
                    |> Result.map (Range.toString >> Expect.equal rangeStr)
                    |> resultFailErr
        , test "Infinite range (both sides)" <|
            \_ ->
                Range.create types.int Nothing Nothing Nothing
                    |> Result.map (Range.toString >> Expect.equal "(,)")
                    |> resultFailErr
        , test "Infinite range (lower)" <|
            \_ ->
                Range.create types.int Nothing (Just 10) Nothing
                    |> Result.map (Range.toString >> Expect.equal "(,10)")
                    |> resultFailErr
        , test "Infinite range (upper)" <|
            \_ ->
                Range.create types.int (Just 10) Nothing Nothing
                    |> Result.map (Range.toString >> Expect.equal "[10,)")
                    |> resultFailErr
        ]



-- OPERATORS


eq : Test
eq =
    let
        emptyRange =
            Range.empty types.int
    in
    describe "equal"
        [ test "both empty" <|
            \_ ->
                Range.eq emptyRange emptyRange
                    |> Expect.true "Both empty should be true"
        , describe "one empty"
            [ fuzzRange "first empty"
                (Range.eq emptyRange
                    >> Expect.false "Empty and non-empty should be false"
                )
            , fuzzRange "second empty"
                (flip Range.eq emptyRange
                    >> Expect.false "Empty and non-empty should be false"
                )
            ]
        , describe "both bounded"
            [ fuzzRange "same range" <|
                \range ->
                    Range.eq range range
                        |> Expect.true "Same range should be equal"
            , fuzz2 IntFuzz.range IntFuzz.range "different values" <|
                \range1 range2 ->
                    Range.eq range1 range2
                        |> Expect.false "both bounded with different values should be false"
            ]
        ]


neq : Test
neq =
    let
        emptyRange =
            Range.empty types.int
    in
    describe "not equal"
        [ test "both empty" <|
            \_ ->
                Range.neq emptyRange emptyRange
                    |> Expect.false "Both empty should be false"
        , describe "one empty"
            [ fuzzRange "first empty"
                (Range.neq emptyRange
                    >> Expect.true "Empty and non-empty should be true"
                )
            , fuzzRange "second empty"
                (flip Range.neq emptyRange
                    >> Expect.true "Empty and non-empty should be true"
                )
            ]
        , describe "both bounded"
            [ fuzzRange "same range" <|
                \range ->
                    Range.neq range range
                        |> Expect.false "Same range should be not equal"
            , fuzz2 IntFuzz.range IntFuzz.range "different values" <|
                \range1 range2 ->
                    Range.neq range1 range2
                        |> Expect.true "both bounded with different values should be not equal"
            ]
        ]


lt : Test
lt =
    let
        emptyRange =
            Range.empty types.int

        create l u =
            Range.create types.int l u Nothing |> Result.withDefault emptyRange
    in
    describe "less than"
        [ test "both empty" <|
            \_ ->
                Range.lt emptyRange emptyRange
                    |> Expect.false "If both empty then should be false"
        , describe "one empty"
            [ fuzzRange "first empty"
                (Range.lt emptyRange
                    >> Expect.true "Empty is always less than a bounded range"
                )
            , fuzzRange "second empty"
                (flip Range.lt emptyRange
                    >> Expect.false "Empty is never less than a bounded range"
                )
            ]
        , describe "both bounded"
            [ fuzzRange "same range" <|
                \range ->
                    Range.lt range range
                        |> Expect.false "A range is not less than itself"
            , describe "both bounds finite"
                [ test "Lower less and upper equal" <|
                    \_ ->
                        Range.lt
                            (create (Just 1) (Just 5))
                            (create (Just 2) (Just 5))
                            |> Expect.true "[1,5) < [2,5)"
                , test "Lower less and upper less" <|
                    \_ ->
                        Range.lt
                            (create (Just 1) (Just 4))
                            (create (Just 2) (Just 5))
                            |> Expect.true "[1,4) < [2,5)"
                , test "Lower less and upper more" <|
                    \_ ->
                        Range.lt
                            (create (Just 1) (Just 5))
                            (create (Just 2) (Just 4))
                            |> Expect.true "[1,5) < [2,4)"
                , test "Lower equal and upper less" <|
                    \_ ->
                        Range.lt
                            (create (Just 1) (Just 4))
                            (create (Just 1) (Just 5))
                            |> Expect.true "[1,4) < [1,5)"
                , test "Lower equal and upper more" <|
                    \_ ->
                        Range.lt
                            (create (Just 1) (Just 5))
                            (create (Just 1) (Just 4))
                            |> Expect.false "[1,5) not < [1,4)"
                , test "Lower more and upper equal" <|
                    \_ ->
                        Range.lt
                            (create (Just 2) (Just 5))
                            (create (Just 1) (Just 5))
                            |> Expect.false "[2,5) not < [1,5)"
                , test "Lower more and upper less" <|
                    \_ ->
                        Range.lt
                            (create (Just 2) (Just 4))
                            (create (Just 1) (Just 5))
                            |> Expect.false "[2,4) not < [1,5)"
                , test "Lower more and upper more" <|
                    \_ ->
                        Range.lt
                            (create (Just 2) (Just 5))
                            (create (Just 1) (Just 4))
                            |> Expect.false "[2,5) not < [1,4)"
                ]
            , describe "infinite and finite bounds"
                [ fuzz2 Fuzz.int IntFuzz.rangeFinite "Infinite lower" <|
                    \i range ->
                        Range.lt
                            (create Nothing (Just i))
                            range
                            |> Expect.true "Should be less than any finite range"
                , fuzz IntFuzz.rangeFinite "Infinite upper" <|
                    \range ->
                        Range.lt
                            (create (Range.lowerElement range) Nothing)
                            range
                            |> Expect.false "Should be greater than any finite range"
                ]
            ]
        ]


gt : Test
gt =
    let
        emptyRange =
            Range.empty types.int

        create l u =
            Range.create types.int l u Nothing |> Result.withDefault emptyRange
    in
    describe "greater than"
        [ test "both empty" <|
            \_ ->
                Range.gt emptyRange emptyRange
                    |> Expect.false "If both empty then should be false"
        , describe "one empty"
            [ fuzzRange "first empty"
                (Range.gt emptyRange
                    >> Expect.false "Empty is never greater than a bounded range"
                )
            , fuzzRange "second empty"
                (flip Range.gt emptyRange
                    >> Expect.true "Empty is always greater than a bounded range"
                )
            ]
        , describe "both bounded"
            [ fuzzRange "same range" <|
                \range ->
                    Range.gt range range
                        |> Expect.false "A range is not less than itself"
            , describe "both bounds finite"
                [ test "Lower less and upper equal" <|
                    \_ ->
                        Range.gt
                            (create (Just 1) (Just 5))
                            (create (Just 2) (Just 5))
                            |> Expect.false "[1,5) > [2,5)"
                , test "Lower less and upper less" <|
                    \_ ->
                        Range.gt
                            (create (Just 1) (Just 4))
                            (create (Just 2) (Just 5))
                            |> Expect.false "[1,4) > [2,5)"
                , test "Lower less and upper more" <|
                    \_ ->
                        Range.gt
                            (create (Just 1) (Just 5))
                            (create (Just 2) (Just 4))
                            |> Expect.false "[1,5) > [2,4)"
                , test "Lower equal and upper less" <|
                    \_ ->
                        Range.gt
                            (create (Just 1) (Just 4))
                            (create (Just 1) (Just 5))
                            |> Expect.false "[1,4) > [1,5)"
                , test "Lower equal and upper more" <|
                    \_ ->
                        Range.gt
                            (create (Just 1) (Just 5))
                            (create (Just 1) (Just 4))
                            |> Expect.true "[1,5) > [1,4)"
                , test "Lower more and upper equal" <|
                    \_ ->
                        Range.gt
                            (create (Just 2) (Just 5))
                            (create (Just 1) (Just 5))
                            |> Expect.true "[2,5) > [1,5)"
                , test "Lower more and upper less" <|
                    \_ ->
                        Range.gt
                            (create (Just 2) (Just 4))
                            (create (Just 1) (Just 5))
                            |> Expect.true "[2,4) < [1,5)"
                , test "Lower more and upper more" <|
                    \_ ->
                        Range.gt
                            (create (Just 2) (Just 5))
                            (create (Just 1) (Just 4))
                            |> Expect.true "[2,5) > [1,4)"
                ]
            , describe "infinite and finite bounds"
                [ fuzz2 Fuzz.int IntFuzz.rangeFinite "Infinite lower" <|
                    \i range ->
                        Range.gt
                            (create Nothing (Just i))
                            range
                            |> Expect.false "Should be less than any finite range"
                , fuzz IntFuzz.rangeFinite "Infinite upper" <|
                    \range ->
                        Range.gt
                            (create (Range.lowerElement range) Nothing)
                            range
                            |> Expect.true "Should be greater than any finite range"
                ]
            ]
        ]


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b


fuzzRange desc fn =
    fuzz IntFuzz.range desc fn



-- Functions


isEmpty : Test
isEmpty =
    let
        createRange bounds =
            Range.create types.int (Just 1) (Just 1) (Just bounds)
                |> Result.map Range.isEmpty
    in
    describe "isEmpty"
        [ test "inc-inc" <|
            \_ ->
                ( Range.Inc, Range.Inc )
                    |> createRange
                    |> Expect.equal (Ok False)
        , test "inc-exc" <|
            \_ ->
                ( Range.Inc, Range.Exc )
                    |> createRange
                    |> Expect.equal (Ok True)
        , test "exc-exc" <|
            \_ ->
                ( Range.Exc, Range.Exc )
                    |> createRange
                    |> Expect.equal (Ok True)
        , test "exc-inc" <|
            \_ ->
                ( Range.Exc, Range.Inc )
                    |> createRange
                    |> Expect.equal (Ok True)
        ]


cr : Test
cr =
    let
        emptyRange =
            Range.empty types.int
    in
    describe "Contains Range"
        [ test "both empty" <|
            \_ ->
                Range.cr emptyRange emptyRange
                    |> Expect.true "An empty range contains another"
        , describe "one empty"
            [ fuzzRange "first empty"
                (Range.cr emptyRange
                    >> Expect.false "An empty range doesn't contain an bound one"
                )
            , fuzzRange "second empty"
                (flip Range.cr emptyRange
                    >> Expect.true "A bound range contains an empty range"
                )
            ]
        , describe "both bounded"
            [ fuzzRange "same range" <|
                \range ->
                    Range.cr range range
                        |> Expect.true "A range contains itself"
            , fuzz2 IntFuzz.rangeFinite IntFuzz.rangeFinite "two bounded ranges" <|
                \r1 r2 ->
                    Maybe.map2 (&&)
                        (Maybe.map2 (<=) (Range.lowerElement r1) (Range.lowerElement r2))
                        (Maybe.map2 (>=) (Range.upperElement r1) (Range.upperElement r2))
                        |> Maybe.withDefault False
                        |> Expect.equal (Range.cr r1 r2)
            ]
        ]


ce : Test
ce =
    let
        emptyRange =
            Range.empty types.int
    in
    describe "Contains Element"
        [ test "both empty" <|
            \_ ->
                Range.cr emptyRange emptyRange
                    |> Expect.true "An empty range contains another"
        , describe "one empty"
            [ fuzzRange "first empty"
                (Range.cr emptyRange
                    >> Expect.false "An empty range doesn't contain an bound one"
                )
            , fuzzRange "second empty"
                (flip Range.cr emptyRange
                    >> Expect.true "A bound range contains an empty range"
                )
            ]
        , describe "both bounded"
            [ fuzzRange "same range" <|
                \range ->
                    Range.cr range range
                        |> Expect.true "A range contains itself"
            , fuzz2 IntFuzz.rangeFinite IntFuzz.rangeFinite "two bounded ranges" <|
                \range1 range2 ->
                    let
                        result =
                            Range.cr range1 range2

                        lower1 =
                            Range.lowerElement range1

                        upper1 =
                            Range.upperElement range1

                        lower2 =
                            Range.lowerElement range2

                        upper2 =
                            Range.upperElement range2
                    in
                    Maybe.map2 (&&)
                        (Maybe.map2 (<=) lower1 lower2)
                        (Maybe.map2 (>=) upper1 upper2)
                        |> Maybe.withDefault False
                        |> Expect.equal (Range.cr range1 range2)
            ]
        ]



-- HELPERS


validRange :
    ( Maybe Int, Maybe Int )
    -> ( Range.BoundFlag, Range.BoundFlag )
    -> Range Int
    -> Expectation
validRange ( maybeLowerElement, maybeUpperElement ) ( lowerBoundFlag, upperBoundFlag ) range =
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
                    , lowerElementExpectation lowerElement lowerBoundFlag
                    , upperElementExpectation upperElement upperBoundFlag
                    ]
                    range

        ( Nothing, Just upperElement ) ->
            Expect.all
                [ expectedLowerBoundExclusive
                , expectedUpperBoundExclusive
                , upperElementExpectation upperElement upperBoundFlag
                ]
                range

        ( Just lowerElement, Nothing ) ->
            Expect.all
                [ expectedLowerBoundInclusive
                , expectedUpperBoundExclusive
                , lowerElementExpectation lowerElement lowerBoundFlag
                ]
                range

        ( Nothing, Nothing ) ->
            Expect.all
                [ expectedLowerBoundExclusive
                , expectedUpperBoundExclusive
                ]
                range


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


resultFailErr : Result String Expectation -> Expectation
resultFailErr result =
    case result of
        Ok a ->
            a

        Err err ->
            Expect.fail err
