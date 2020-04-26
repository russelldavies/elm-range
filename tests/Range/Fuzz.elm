module Range.Fuzz exposing (..)

import Fuzz exposing (Fuzzer, int, list, string)
import Random exposing (Generator)
import Range exposing (Range, types)
import Shrink



-- TYPE SPECIFIC FUZZERS


{-| Is not empty but can contain an infinite bound
-}
intRange : Fuzzer (Range Int)
intRange =
    Fuzz.map
        (\( lowerBound, upperBound ) ->
            Range.create types.int lowerBound upperBound
                |> Result.withDefault (Range.empty types.int)
        )
        (validMaybeNumPair Random.int)


{-| Is not empty but bounds are always finite
-}
intRangeFinite : Fuzzer (Range Int)
intRangeFinite =
    Fuzz.map
        (\( lowerBound, upperBound ) ->
            Range.create types.int (Just lowerBound) (Just upperBound)
                |> Result.withDefault (Range.empty types.int)
        )
        (validNumPair Random.int)


{-| Is not empty but can contain an infinite bound
-}
floatRange : Fuzzer (Range Float)
floatRange =
    Fuzz.map2
        (\( lowerBound, upperBound ) flags ->
            Range.createWith types.float lowerBound upperBound (Just flags)
                |> Result.withDefault (Range.empty types.float)
        )
        (validMaybeNumPair Random.float)
        boundFlagPair


{-| Is not empty but bounds are always finite
-}
floatRangeFinite : Fuzzer (Range Float)
floatRangeFinite =
    Fuzz.map
        (\( lowerBound, upperBound ) ->
            Range.create types.float (Just lowerBound) (Just upperBound)
                |> Result.withDefault (Range.empty types.float)
        )
        (validNumPair Random.float)



-- GENERIC FUZZERS


boundFlag : Fuzzer Range.BoundFlag
boundFlag =
    Fuzz.oneOf
        [ Fuzz.constant Range.Inc
        , Fuzz.constant Range.Exc
        ]


boundFlagPair : Fuzzer ( Range.BoundFlag, Range.BoundFlag )
boundFlagPair =
    Fuzz.tuple ( boundFlag, boundFlag )


boundFlagCharPair : Fuzzer ( Char, Char )
boundFlagCharPair =
    Fuzz.tuple
        ( Fuzz.oneOf
            [ Fuzz.constant '['
            , Fuzz.constant '('
            ]
        , Fuzz.oneOf
            [ Fuzz.constant ']'
            , Fuzz.constant ')'
            ]
        )


{-| Generate a valid range in string form like (-1023,982] with random numbers and flags.
-}
numberString : (number -> number -> Generator number) -> (number -> String) -> Fuzzer String
numberString generator toStr =
    Fuzz.map2
        (\( lowerBound, upperBound ) ( lowerFlag, upperFlag ) ->
            String.fromChar lowerFlag ++ toStr lowerBound ++ "," ++ toStr upperBound ++ String.fromChar upperFlag
        )
        (validNumPair generator)
        boundFlagCharPair


{-| Generate a valid range in string form like (-1023,982] with random numbers but fixed flags
-}
numberStringDefault : (number -> number -> Generator number) -> (number -> String) -> Fuzzer String
numberStringDefault generator toStr =
    Fuzz.map
        (\( lowerBound, upperBound ) ->
            "[" ++ toStr lowerBound ++ "," ++ toStr upperBound ++ ")"
        )
        (validNumPair generator)


{-| Lower may be greater than upper
-}
maybeNumPair : Fuzzer number -> Fuzzer ( Maybe number, Maybe number )
maybeNumPair fuzzer =
    let
        maybeNum =
            Fuzz.oneOf
                [ Fuzz.constant Nothing
                , Fuzz.map Just fuzzer
                ]
    in
    Fuzz.tuple ( maybeNum, maybeNum )


{-| Lower must be less than or equal to upper
-}
validNumPair : (number -> number -> Generator number) -> Fuzzer ( number, number )
validNumPair numberGenerator =
    let
        min =
            -2147483648

        max =
            2147483647

        generator =
            numberGenerator min (max - 1)
                |> Random.andThen
                    (\num ->
                        Random.pair
                            (Random.constant num)
                            (numberGenerator (num + 1) max)
                    )
    in
    Fuzz.custom generator (Shrink.tuple ( Shrink.noShrink, Shrink.noShrink ))


{-| Lower must be less than or equal to upper
-}
validMaybeNumPair : (number -> number -> Generator number) -> Fuzzer ( Maybe number, Maybe number )
validMaybeNumPair numGenerator =
    let
        min =
            -2147483648

        max =
            2147483647

        nothingOrNum : Generator (Maybe number)
        nothingOrNum =
            Random.float 0 1
                |> Random.andThen
                    (\p ->
                        if p < 0.5 then
                            Random.constant Nothing

                        else
                            numGenerator min max
                                |> Random.map Just
                    )

        generator : Generator ( Maybe number, Maybe number )
        generator =
            Random.pair nothingOrNum nothingOrNum
                |> Random.andThen
                    (\pair ->
                        case pair of
                            ( Just a, Just b ) ->
                                if a > b then
                                    Random.constant ( Just b, Just a )

                                else
                                    Random.constant pair

                            _ ->
                                Random.constant pair
                    )
    in
    Fuzz.custom generator
        (Shrink.tuple
            ( Shrink.maybe Shrink.noShrink
            , Shrink.maybe Shrink.noShrink
            )
        )
