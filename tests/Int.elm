module Int exposing (suite)

import Expect
import Generic exposing (resultFailErr, validRange)
import Random
import Range exposing (Range, types)
import Range.Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "int tests"
        [ canonical
        , toString
        ]



-- Type specific


canonical : Test
canonical =
    describe "Verifies Int canonical function"
        [ fuzz2 validMaybeIntPair Range.Fuzz.boundFlagPair "Random bound values and flags" <|
            \(( maybeLowerElement, maybeUpperElement ) as elements) boundFlags ->
                Range.createWith types.int maybeLowerElement maybeUpperElement (Just boundFlags)
                    |> Result.map (validRange elements boundFlags)
                    |> resultFailErr
        ]


toString : Test
toString =
    describe "Convert valid range to string"
        [ test "Empty range" <|
            \_ -> Range.empty types.int |> Range.toString |> Expect.equal "empty"
        , fuzz (Range.Fuzz.numberStringDefault Random.int String.fromInt) "Restore the original string" <|
            \rangeStr ->
                -- Can use fromString as it's been tested already
                rangeStr
                    |> Range.fromString types.int
                    |> Result.map (Range.toString >> Expect.equal rangeStr)
                    |> resultFailErr
        , test "Infinite range (both sides)" <|
            \_ ->
                Range.create types.int Nothing Nothing
                    |> Result.map (Range.toString >> Expect.equal "(,)")
                    |> resultFailErr
        , test "Infinite range (lower)" <|
            \_ ->
                Range.create types.int Nothing (Just 10)
                    |> Result.map (Range.toString >> Expect.equal "(,10)")
                    |> resultFailErr
        , test "Infinite range (upper)" <|
            \_ ->
                Range.create types.int (Just 10) Nothing
                    |> Result.map (Range.toString >> Expect.equal "[10,)")
                    |> resultFailErr
        ]



-- Helpers


validMaybeIntPair =
    Range.Fuzz.validMaybeNumPair Random.int
