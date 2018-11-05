module Range exposing
    ( Range
    , decoder
    , empty
    , encode
    , fromString
    , toString
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)


type Range subtype
    = Bounded ( Bound subtype, Bound subtype )
    | Empty


type Bound subtype
    = Exclusive subtype
    | Inclusive subtype
    | Unbounded


type alias Comparator subtype =
    subtype -> subtype -> Order


empty : Range subtype
empty =
    Empty


fromString :
    Parser subtype
    -> Comparator subtype
    -> String
    -> Result (List Parser.DeadEnd) (Range subtype)
fromString subtypeParser compare =
    Parser.run (parser subtypeParser compare)


toString : (subtype -> String) -> Range subtype -> String
toString valToString range =
    case range of
        Empty ->
            "empty"

        Bounded bounds ->
            case bounds of
                ( Inclusive lower, Inclusive upper ) ->
                    "[" ++ valToString lower ++ "," ++ valToString upper ++ "]"

                ( Exclusive lower, Inclusive upper ) ->
                    "(" ++ valToString lower ++ "," ++ valToString upper ++ "]"

                ( Inclusive lower, Exclusive upper ) ->
                    "[" ++ valToString lower ++ "," ++ valToString upper ++ ")"

                ( Exclusive lower, Exclusive upper ) ->
                    "(" ++ valToString lower ++ "," ++ valToString upper ++ ")"

                ( Unbounded, Inclusive upper ) ->
                    "(," ++ valToString upper ++ "]"

                ( Unbounded, Exclusive upper ) ->
                    "(," ++ valToString upper ++ ")"

                ( Inclusive lower, Unbounded ) ->
                    "[" ++ valToString lower ++ ",)"

                ( Exclusive lower, Unbounded ) ->
                    "(" ++ valToString lower ++ ",)"

                ( Unbounded, Unbounded ) ->
                    "(,)"


parser :
    Parser subtype
    -> Comparator subtype
    -> Parser (Range subtype)
parser subtypeParser compare =
    let
        lowerBoundParser =
            Parser.oneOf
                [ Parser.backtrackable
                    (Parser.succeed Inclusive
                        |. Parser.symbol "["
                        |= subtypeParser
                    )
                , Parser.backtrackable
                    (Parser.succeed Exclusive
                        |. Parser.symbol "("
                        |= subtypeParser
                    )
                , Parser.succeed Unbounded
                    |. Parser.oneOf
                        [ Parser.symbol "("
                        , Parser.symbol "["
                        ]
                ]

        upperBoundParser =
            Parser.oneOf
                [ Parser.succeed (\val bound -> bound val)
                    |= subtypeParser
                    |= Parser.oneOf
                        [ Parser.succeed Inclusive
                            |. Parser.symbol "]"
                        , Parser.succeed Exclusive
                            |. Parser.symbol ")"
                        ]
                , Parser.succeed Unbounded
                    |. Parser.oneOf
                        [ Parser.symbol "]"
                        , Parser.symbol ")"
                        ]
                ]

        checkBounds :
            subtype
            -> subtype
            -> ( Bound subtype, Bound subtype )
            -> Parser (Range subtype)
        checkBounds lower upper bounds =
            case compare lower upper of
                LT ->
                    Parser.succeed (Bounded bounds)

                EQ ->
                    Parser.succeed Empty

                GT ->
                    Parser.problem "Range lower bound must be less than or equal to range upper bound"

        validate : ( Bound subtype, Bound subtype ) -> Parser (Range subtype)
        validate bounds =
            case bounds of
                ( Exclusive lower, Exclusive upper ) ->
                    checkBounds lower upper bounds

                ( Inclusive lower, Exclusive upper ) ->
                    checkBounds lower upper bounds

                ( Exclusive lower, Inclusive upper ) ->
                    checkBounds lower upper bounds

                ( Inclusive lower, Inclusive upper ) ->
                    checkBounds lower upper bounds

                _ ->
                    Parser.succeed (Bounded bounds)
    in
    Parser.succeed Tuple.pair
        |= lowerBoundParser
        |. Parser.symbol ","
        |= upperBoundParser
        |> Parser.andThen validate



-- JSON


{-| Range JSON decoder
-}
decoder :
    Parser subtype
    -> Comparator subtype
    -> Decode.Decoder (Range subtype)
decoder subtypeParser compare =
    Decode.string
        |> Decode.andThen
            (\rangeStr ->
                case fromString subtypeParser compare rangeStr of
                    Ok range ->
                        Decode.succeed range

                    Err _ ->
                        Decode.fail "Not a valid range"
            )


{-| Encode Range to JSON
-}
encode : (subtype -> String) -> Range subtype -> Encode.Value
encode valToString =
    toString valToString >> Encode.string
