module Range.Int.Fuzz exposing (..)

import Fuzz exposing (Fuzzer, int, list, string)
import Random
import Range exposing (Range, types)
import Range.Fuzz
import Shrink


{-| Is not empty but can contain an infinite bound
-}
range : Fuzzer (Range Int)
range =
    Fuzz.map
        (\( lowerBound, upperBound ) ->
            Range.create types.int lowerBound upperBound Nothing
                |> Result.withDefault (Range.empty types.int)
        )
        (Range.Fuzz.validMaybeNumPair Random.int)


{-| Is not empty but bounds are always finite
-}
rangeFinite : Fuzzer (Range Int)
rangeFinite =
    Fuzz.map
        (\( lowerBound, upperBound ) ->
            Range.create types.int (Just lowerBound) (Just upperBound) Nothing
                |> Result.withDefault (Range.empty types.int)
        )
        (Range.Fuzz.validNumPair Random.int)
