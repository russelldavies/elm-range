module Range.Float exposing
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


config =
    { toString = String.fromFloat
    , fromString = String.toFloat >> Result.fromMaybe "Invalid number"
    , compare = Basics.compare
    , canonical = Just canonical
    }


fromString : String -> Result (List Parser.DeadEnd) (Range Float)
fromString =
    Range.fromString config


toString : Range Float -> String
toString =
    Range.toString config


create : Maybe Float -> Maybe Float -> Maybe ( Range.Flag, Range.Flag ) -> Result String (Range Float)
create maybeLower maybeUpper maybeFlags =
    Range.create config maybeLower maybeUpper (Maybe.withDefault ( Range.Inc, Range.Exc ) maybeFlags)


containsElement : Float -> Range Float -> Bool
containsElement =
    Range.containsElement config


merge : Range Float -> Range Float -> Range Float
merge =
    Range.merge config


decoder : Decoder (Range Float)
decoder =
    Range.decoder config


encode : Range Float -> Encode.Value
encode =
    Range.encode config


canonical : Range Float -> ( ( Maybe Float, Maybe Float ), ( Range.Flag, Range.Flag ) )
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
