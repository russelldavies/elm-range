module Range.Int exposing
    ( config
    , containsElement
    , create
    , decoder
    , encode
    , equal
    , fromString
    , lessThan
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


create : Maybe Int -> Maybe Int -> Maybe ( Range.BoundFlag, Range.BoundFlag ) -> Result String (Range Int)
create maybeLower maybeUpper maybeBoundFlags =
    Range.create config maybeLower maybeUpper (Maybe.withDefault ( Range.Inc, Range.Exc ) maybeBoundFlags)


fromString : String -> Result (List Parser.DeadEnd) (Range Int)
fromString =
    Range.fromString config


toString : Range Int -> String
toString =
    Range.toString config


containsElement : Range Int -> Int -> Bool
containsElement =
    Range.containsElement config


equal : Range Int -> Range Int -> Bool
equal =
    Range.equal config


lessThan : Range Int -> Range Int -> Bool
lessThan =
    Range.lessThan config


merge : Range Int -> Range Int -> Range Int
merge =
    Range.merge config


decoder : Decoder (Range Int)
decoder =
    Range.decoder config


encode : Range Int -> Encode.Value
encode =
    Range.encode config


canonical : Range Int -> ( ( Maybe Int, Maybe Int ), ( Range.BoundFlag, Range.BoundFlag ) )
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
