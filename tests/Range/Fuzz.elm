module Range.Fuzz exposing (..)

import Fuzz exposing (Fuzzer, int, list, string)
import Random exposing (Generator)
import Range
import Shrink


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

        randJust =
            Random.map Just

        generator =
            Random.float 0 1
                |> Random.andThen
                    (\p ->
                        if p < 0.5 then
                            Random.constant Nothing

                        else
                            randJust <| numGenerator min (max - 1)
                    )
                |> Random.andThen
                    (Maybe.map
                        (\num ->
                            Random.pair
                                (randJust <| Random.constant num)
                                (randJust <| numGenerator (num + 1) max)
                        )
                        >> Maybe.withDefault
                            (Random.pair
                                (Random.constant Nothing)
                                (randJust <| numGenerator min max)
                            )
                    )
    in
    Fuzz.custom generator
        (Shrink.tuple
            ( Shrink.maybe Shrink.noShrink
            , Shrink.maybe Shrink.noShrink
            )
        )
