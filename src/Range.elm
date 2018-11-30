module Range exposing
    ( Flag(..)
    , Range
    , SubtypeConfig
    , containsElement
    , create
    , decoder
    , empty
    , encode
    , fromString
    , isEmpty
    , lowerBoundInclusive
    , lowerBoundInfinite
    , lowerElement
    , toString
    , upperBoundInclusive
    , upperBoundInfinite
    , upperElement
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)



-- MODELS


type Range subtype
    = Bounded ( Bound subtype, Bound subtype )
    | Empty


type Bound subtype
    = Exclusive subtype
    | Inclusive subtype
    | Infinite


type alias SubtypeConfig subtype =
    { toString : subtype -> String
    , fromString : String -> Result String subtype
    , compare : subtype -> subtype -> Order
    , canonical : Maybe (Range subtype -> ( ( Maybe subtype, Maybe subtype ), ( Flag, Flag ) ))
    }


type Flag
    = Inc
    | Exc



-- CREATE


empty : Range subtype
empty =
    Empty


create :
    SubtypeConfig subtype
    -> Maybe subtype
    -> Maybe subtype
    -> ( Flag, Flag )
    -> Result String (Range subtype)
create subtypeConfig maybeLower maybeUpper flags =
    -- Flags are mandatory as it will be the subtype's canonical function that
    -- specifies the convention to use.
    construct ( ( maybeLower, maybeUpper ), flags )
        |> validate subtypeConfig


fromString :
    SubtypeConfig subtype
    -> String
    -> Result (List Parser.DeadEnd) (Range subtype)
fromString subtypeConfig =
    Parser.run (parser subtypeConfig)


toString : SubtypeConfig subtype -> Range subtype -> String
toString subtypeConfig range =
    case range of
        Empty ->
            "empty"

        Bounded bounds ->
            case bounds of
                ( Inclusive lower, Inclusive upper ) ->
                    "[" ++ subtypeConfig.toString lower ++ "," ++ subtypeConfig.toString upper ++ "]"

                ( Exclusive lower, Inclusive upper ) ->
                    "(" ++ subtypeConfig.toString lower ++ "," ++ subtypeConfig.toString upper ++ "]"

                ( Inclusive lower, Exclusive upper ) ->
                    "[" ++ subtypeConfig.toString lower ++ "," ++ subtypeConfig.toString upper ++ ")"

                ( Exclusive lower, Exclusive upper ) ->
                    "(" ++ subtypeConfig.toString lower ++ "," ++ subtypeConfig.toString upper ++ ")"

                ( Infinite, Inclusive upper ) ->
                    "(," ++ subtypeConfig.toString upper ++ "]"

                ( Infinite, Exclusive upper ) ->
                    "(," ++ subtypeConfig.toString upper ++ ")"

                ( Inclusive lower, Infinite ) ->
                    "[" ++ subtypeConfig.toString lower ++ ",)"

                ( Exclusive lower, Infinite ) ->
                    "(" ++ subtypeConfig.toString lower ++ ",)"

                ( Infinite, Infinite ) ->
                    "(,)"



-- OPERATIONS


containsElement : SubtypeConfig subtype -> subtype -> Range subtype -> Bool
containsElement config element range =
    let
        lt_ =
            lt config.compare

        lte_ =
            lte config.compare

        gt_ =
            gt config.compare

        gte_ =
            gte config.compare
    in
    case range of
        Empty ->
            False

        Bounded bounds ->
            case bounds of
                ( Inclusive lower, Inclusive upper ) ->
                    lte_ lower element && lte_ element upper

                ( Inclusive lower, Exclusive upper ) ->
                    lte_ lower element && lt_ element upper

                ( Exclusive lower, Inclusive upper ) ->
                    lt_ lower element && lte_ element upper

                ( Exclusive lower, Exclusive upper ) ->
                    lt_ lower element && lt_ element upper

                ( Infinite, Inclusive upper ) ->
                    lte_ element upper

                ( Infinite, Exclusive upper ) ->
                    lt_ element upper

                ( Inclusive lower, Infinite ) ->
                    lte_ lower element

                ( Exclusive lower, Infinite ) ->
                    lt_ lower element

                ( Infinite, Infinite ) ->
                    True



-- FUNCTIONS


lowerElement : Range subtype -> Maybe subtype
lowerElement range =
    case range of
        Bounded ( lower, _ ) ->
            boundElement lower

        Empty ->
            Nothing


upperElement : Range subtype -> Maybe subtype
upperElement range =
    case range of
        Bounded ( _, upper ) ->
            boundElement upper

        Empty ->
            Nothing


isEmpty : Range subtype -> Bool
isEmpty range =
    case range of
        Bounded _ ->
            False

        Empty ->
            True


lowerBoundInclusive : Range subtype -> Bool
lowerBoundInclusive range =
    case range of
        Bounded ( Inclusive _, _ ) ->
            True

        _ ->
            False


upperBoundInclusive : Range subtype -> Bool
upperBoundInclusive range =
    case range of
        Bounded ( _, Inclusive _ ) ->
            True

        _ ->
            False


lowerBoundInfinite : Range subtype -> Bool
lowerBoundInfinite range =
    case range of
        Bounded ( Infinite, _ ) ->
            True

        _ ->
            False


upperBoundInfinite : Range subtype -> Bool
upperBoundInfinite range =
    case range of
        Bounded ( _, Infinite ) ->
            True

        _ ->
            False



-- JSON


{-| Range JSON decoder
-}
decoder :
    SubtypeConfig subtype
    -> Decode.Decoder (Range subtype)
decoder subtypeConfig =
    Decode.string
        |> Decode.andThen
            (\rangeStr ->
                case fromString subtypeConfig rangeStr of
                    Ok range ->
                        Decode.succeed range

                    Err _ ->
                        Decode.fail "Not a valid range"
            )


{-| Encode Range to JSON
-}
encode : SubtypeConfig subtype -> Range subtype -> Encode.Value
encode subtypeConfig =
    toString subtypeConfig >> Encode.string



-- HELPERS


validate : SubtypeConfig subtype -> Range subtype -> Result String (Range subtype)
validate { canonical, compare } range_ =
    let
        checkBounds range =
            case Maybe.map2 compare (lowerElement range) (upperElement range) of
                Just order ->
                    case order of
                        LT ->
                            Ok range

                        EQ ->
                            Ok Empty

                        GT ->
                            Err "Lower bound must be less than or equal to upper bound"

                Nothing ->
                    Ok range

        canonicalize range =
            case canonical of
                Nothing ->
                    range

                Just fn ->
                    construct (fn range)
    in
    range_
        |> canonicalize
        |> checkBounds


construct : ( ( Maybe subtype, Maybe subtype ), ( Flag, Flag ) ) -> Range subtype
construct ( ( maybeLower, maybeUpper ), ( lowerFlag, upperFlag ) ) =
    let
        flagToBound flag =
            case flag of
                Inc ->
                    Inclusive

                Exc ->
                    Exclusive

        lower =
            maybeLower
                |> Maybe.map (flagToBound lowerFlag)
                |> Maybe.withDefault Infinite

        upper =
            maybeUpper
                |> Maybe.map (flagToBound upperFlag)
                |> Maybe.withDefault Infinite
    in
    Bounded ( lower, upper )


parser : SubtypeConfig subtype -> Parser (Range subtype)
parser subtypeConfig =
    let
        parseSubtype ( bound, str ) =
            if String.isEmpty str then
                Parser.succeed Infinite

            else
                case subtypeConfig.fromString str of
                    Ok date ->
                        Parser.succeed (bound date)

                    Err error ->
                        Parser.problem error

        lowerBoundParser =
            Parser.succeed (\bound str -> ( bound, str ))
                |= Parser.oneOf
                    [ Parser.succeed Inclusive
                        |. Parser.symbol "["
                    , Parser.succeed Exclusive
                        |. Parser.symbol "("
                    ]
                |= Parser.getChompedString (Parser.chompUntil ",")
                |> Parser.andThen parseSubtype

        upperBoundParser =
            Parser.succeed (\str bound -> ( bound, str ))
                |= Parser.getChompedString
                    (Parser.oneOf
                        [ Parser.chompUntil ")"
                        , Parser.chompUntil "]"
                        ]
                    )
                |= Parser.oneOf
                    [ Parser.succeed Inclusive
                        |. Parser.symbol "]"
                    , Parser.succeed Exclusive
                        |. Parser.symbol ")"
                    ]
                |> Parser.andThen parseSubtype
    in
    Parser.succeed (\lower upper -> Bounded ( lower, upper ))
        |= lowerBoundParser
        |. Parser.symbol ","
        |= upperBoundParser
        |> Parser.andThen
            (\range ->
                case validate subtypeConfig range of
                    Ok range_ ->
                        Parser.succeed range_

                    Err err ->
                        Parser.problem err
            )


boundElement : Bound subtype -> Maybe subtype
boundElement bound =
    case bound of
        Inclusive val ->
            Just val

        Exclusive val ->
            Just val

        Infinite ->
            Nothing


lt : (subtype -> subtype -> Order) -> subtype -> subtype -> Bool
lt compare a b =
    case compare a b of
        LT ->
            True

        _ ->
            False


lte : (subtype -> subtype -> Order) -> subtype -> subtype -> Bool
lte compare a b =
    case compare a b of
        GT ->
            False

        _ ->
            True


gt : (subtype -> subtype -> Order) -> subtype -> subtype -> Bool
gt compare a b =
    case compare a b of
        GT ->
            True

        _ ->
            False


gte : (subtype -> subtype -> Order) -> subtype -> subtype -> Bool
gte compare a b =
    case compare a b of
        LT ->
            False

        _ ->
            False
