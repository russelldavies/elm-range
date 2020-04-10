module Range.Int.Fuzz exposing (..)

import Fuzz exposing (Fuzzer, int, list, string)
import Random
import Range exposing (Range)
import Range.Int
import Shrink


range : Fuzzer (Result String (Range Int))
range =
    Fuzz.map
        (\( lowerBound, upperBound ) ->
            Range.Int.create lowerBound upperBound Nothing
        )
        validMaybeIntPair


{-| Lower may be greater than upper
-}
intPair : Fuzzer ( Int, Int )
intPair =
    Fuzz.tuple ( Fuzz.int, Fuzz.int )


{-| Lower may be greater than upper
-}
maybeIntPair : Fuzzer ( Maybe Int, Maybe Int )
maybeIntPair =
    let
        maybeInt =
            Fuzz.oneOf
                [ Fuzz.constant Nothing
                , Fuzz.map Just Fuzz.int
                ]
    in
    Fuzz.tuple ( maybeInt, maybeInt )


{-| Lower must be less than or equal to upper
-}
validIntPair : Fuzzer ( Int, Int )
validIntPair =
    let
        generator =
            Random.int Random.minInt Random.maxInt
                |> Random.andThen
                    (\i ->
                        Random.pair
                            (Random.constant i)
                            (Random.int i Random.maxInt)
                    )
    in
    Fuzz.custom generator (Shrink.tuple ( Shrink.int, Shrink.int ))


{-| Lower must be less than or equal to upper
-}
validMaybeIntPair : Fuzzer ( Maybe Int, Maybe Int )
validMaybeIntPair =
    let
        randJust =
            Random.map Just

        generator =
            Random.float 0 1
                |> Random.andThen
                    (\p ->
                        if p < 0.5 then
                            Random.constant Nothing

                        else
                            randJust <| Random.int Random.minInt Random.maxInt
                    )
                |> Random.andThen
                    (Maybe.map
                        (\i ->
                            Random.pair
                                (randJust <| Random.constant i)
                                (randJust <| Random.int i Random.maxInt)
                        )
                        >> Maybe.withDefault
                            (Random.pair
                                (Random.constant Nothing)
                                (randJust <| Random.int Random.minInt Random.maxInt)
                            )
                    )
    in
    Fuzz.custom generator
        (Shrink.tuple
            ( Shrink.maybe Shrink.int
            , Shrink.maybe Shrink.int
            )
        )


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


rangeString : Fuzzer String
rangeString =
    Fuzz.map
        (\( lowerBound, upperBound ) ->
            "[" ++ String.fromInt lowerBound ++ "," ++ String.fromInt upperBound ++ ")"
        )
        validIntPair
