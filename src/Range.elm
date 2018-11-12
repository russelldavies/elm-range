module Range exposing
    ( Range
    , containsElement
    , decoder
    , empty
    , encode
    , fromString
    , lowerBound
    , lowerBoundInclusive
    , lowerBoundInfinite
    , toString
    , upperBound
    , upperBoundInclusive
    , upperBoundInfinite
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
    | Infinite


type alias SubtypeConfig subtype =
    { toString : subtype -> String
    , parser : Parser subtype
    , compare : subtype -> subtype -> Order
    , canonical : Maybe (Range subtype -> Range subtype)
    }


createRange : Maybe subtype -> Maybe subtype -> Range subtype
createRange maybeLower maybeUpper =
    let
        lower =
            maybeLower |> Maybe.map Inclusive |> Maybe.withDefault Infinite

        upper =
            maybeUpper |> Maybe.map Inclusive |> Maybe.withDefault Infinite
    in
    Bounded ( lower, upper )


empty : Range subtype
empty =
    Empty


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


parser :
    SubtypeConfig subtype
    -> Parser (Range subtype)
parser subtypeConfig =
    let
        lowerBoundParser =
            Parser.oneOf
                [ Parser.backtrackable
                    (Parser.succeed Inclusive
                        |. Parser.symbol "["
                        |= subtypeConfig.parser
                    )
                , Parser.backtrackable
                    (Parser.succeed Exclusive
                        |. Parser.symbol "("
                        |= subtypeConfig.parser
                    )
                , Parser.succeed Infinite
                    |. Parser.oneOf
                        [ Parser.symbol "("
                        , Parser.symbol "["
                        ]
                ]

        upperBoundParser =
            Parser.oneOf
                [ Parser.succeed (\val bound -> bound val)
                    |= subtypeConfig.parser
                    |= Parser.oneOf
                        [ Parser.succeed Inclusive
                            |. Parser.symbol "]"
                        , Parser.succeed Exclusive
                            |. Parser.symbol ")"
                        ]
                , Parser.succeed Infinite
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
            case subtypeConfig.compare lower upper of
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



-- FUNCTIONS


lowerBound : Range subtype -> Maybe subtype
lowerBound range =
    case range of
        Bounded ( lower, _ ) ->
            boundVal lower

        Empty ->
            Nothing


upperBound : Range subtype -> Maybe subtype
upperBound range =
    case range of
        Bounded ( _, upper ) ->
            boundVal upper

        Empty ->
            Nothing


boundVal : Bound subtype -> Maybe subtype
boundVal bound =
    case bound of
        Inclusive val ->
            Just val

        Exclusive val ->
            Just val

        Infinite ->
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
