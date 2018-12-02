module Range.Int exposing
    ( containsElement
    , create
    , decoder
    , encode
    , fromString
    , merge
    , toString
    )

import Json.Decode as Decoder exposing (Decoder)
import Json.Encode as Encode
import Parser
import Range exposing (Range)


config : Range.SubtypeConfig Int
config =
    { toString = String.fromInt
    , fromString = String.toInt >> Result.fromMaybe "Invalid integer"
    , compare = Basics.compare
    , canonical = Just canonical
    }


fromString : String -> Result (List Parser.DeadEnd) (Range Int)
fromString =
    Range.fromString config


toString : Range Int -> String
toString =
    Range.toString config


create : Maybe Int -> Maybe Int -> Maybe ( Range.Flag, Range.Flag ) -> Result String (Range Int)
create maybeLower maybeUpper maybeFlags =
    Range.create config maybeLower maybeUpper (Maybe.withDefault ( Range.Inc, Range.Exc ) maybeFlags)


containsElement : Int -> Range Int -> Bool
containsElement =
    Range.containsElement config


merge : Range Int -> Range Int -> Range Int
merge =
    Range.merge config


decoder : Decoder (Range Int)
decoder =
    Range.decoder config


encode : Range Int -> Encode.Value
encode =
    Range.encode config


canonical : Range Int -> ( ( Maybe Int, Maybe Int ), ( Range.Flag, Range.Flag ) )
canonical range =
    let
        lowerElement =
            if Range.lowerBoundInclusive range then
                Range.lowerElement range

            else
                (Range.lowerElement >> Maybe.map ((+) 1)) range

        upperElement =
            if Range.upperBoundInclusive range then
                (Range.upperElement >> Maybe.map ((+) 1)) range

            else
                Range.upperElement range
    in
    ( ( lowerElement, upperElement ), ( Range.Inc, Range.Exc ) )
