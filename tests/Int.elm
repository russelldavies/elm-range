module Int exposing (checkBounds, equal, isEmpty)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Range
import Range.Int
import Test exposing (..)


checkBounds : Test
checkBounds =
    let
        isOk r =
            case r of
                Ok _ ->
                    True

                Err _ ->
                    False

        createRange l u expect ( testName, bounds ) =
            test testName <|
                \_ ->
                    Range.Int.create (Just l) (Just u) (Just bounds)
                        |> isOk
                        |> expect "Expect ok"

        rangeList l u expect =
            [ ( "inc-inc", ( Range.Inc, Range.Inc ) )
            , ( "inc-exc", ( Range.Inc, Range.Exc ) )
            , ( "exc-inc", ( Range.Exc, Range.Inc ) )
            , ( "exc-exc", ( Range.Exc, Range.Exc ) )
            ]
                |> List.map (createRange l u expect)
    in
    describe "checkBounds"
        [ describe "lower less than upper"
            (rangeList 1 2 Expect.true)
        , describe "equal"
            (rangeList 1 1 Expect.true)
        , describe "lower greater than upper"
            (rangeList 2 1 Expect.false)
        ]


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
            Range.Int.create (Just 1) (Just 2) Nothing |> Result.withDefault Range.empty

        r2 =
            Range.Int.create (Just 11) (Just 12) Nothing |> Result.withDefault Range.empty
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
