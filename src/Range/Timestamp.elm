module Range.Timestamp exposing
    ( containsElement
    , create
    , decoder
    , encode
    , fromString
    , merge
    , toString
    )

import Iso8601
import Json.Decode as Decoder exposing (Decoder)
import Json.Encode as Encode
import Parser
import Range exposing (Range)
import Time


config =
    { fromString = Iso8601.toTime >> Result.mapError (always "Time format invalid")
    , compare = \lower upper -> Basics.compare (Time.posixToMillis lower) (Time.posixToMillis upper)
    , toString = Iso8601.fromTime
    , canonical = Nothing
    }


fromString : String -> Result (List Parser.DeadEnd) (Range Time.Posix)
fromString =
    Range.fromString config


toString : Range Time.Posix -> String
toString =
    Range.toString config


create : String -> String -> Maybe ( Range.Flag, Range.Flag ) -> Result String (Range Time.Posix)
create maybeLower maybeUpper maybeFlags =
    let
        toPosix =
            Iso8601.toTime >> Result.toMaybe
    in
    Range.create config (toPosix maybeLower) (toPosix maybeUpper) (Maybe.withDefault ( Range.Inc, Range.Exc ) maybeFlags)


containsElement : Time.Posix -> Range Time.Posix -> Bool
containsElement =
    Range.containsElement config


merge : Range Time.Posix -> Range Time.Posix -> Range Time.Posix
merge =
    Range.merge config


decoder : Decoder (Range Time.Posix)
decoder =
    Range.decoder config


encode : Range Time.Posix -> Encode.Value
encode =
    Range.encode config
