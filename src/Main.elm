module Main exposing (main)

import Array
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.Array


type Player
    = Player1
    | Player2


type alias GameState =
    { p1Deck : Deck
    , p1Cards : Dict Int Card
    , p1Score : Int
    , p1Characters : Dict Int Card
    , p2Deck : Deck
    , p2Cards : Dict Int Card
    , p2Score : Int
    , p2Characters : Dict Int Card
    , currentPlayer : Player
    }


playCard : ( Int, Card ) -> GameState -> GameState
playCard ( id, card ) state =
    case card of
        OneShot (GenerateProgress (Progress n)) ->
            { state | p1Score = state.p1Score + n }

        Character _ ->
            { state | p1Characters = Dict.insert id card state.p1Characters }


type Game
    = Choosing
    | Playing GameState
    | Complete


type alias Model =
    Game


type Msg
    = SelectDeck Deck
    | StartPlaying GameState
    | PlayCard Int Card
    | EndTurn


type Deck
    = Flashy
    | SlowAndSteady


type Progress
    = Progress Int


type Power
    = GenerateProgress Progress


type Card
    = Character Progress
    | OneShot Power


slowAndSteadyDeck : List Card
slowAndSteadyDeck =
    [ Character (Progress 1)
    , Character (Progress 2)
    , Character (Progress 3)
    , Character (Progress 4)
    , Character (Progress 5)
    , Character (Progress 6)
    ]


flashyDeck : List Card
flashyDeck =
    [ OneShot (GenerateProgress <| Progress 10)
    , OneShot (GenerateProgress <| Progress 20)
    , OneShot (GenerateProgress <| Progress 30)
    , OneShot (GenerateProgress <| Progress 20)
    , OneShot (GenerateProgress <| Progress 10)
    , OneShot (GenerateProgress <| Progress 10)
    ]


shuffle : List Card -> Random.Generator (List ( Int, Card ))
shuffle cards =
    cards
        |> Array.fromList
        |> Random.Array.shuffle
        |> Random.map Array.toList
        |> Random.map (List.indexedMap Tuple.pair)


startGame : Deck -> List ( Int, Card ) -> List ( Int, Card ) -> GameState
startGame selectedDeck p1Cards p2Cards =
    { p1Deck = selectedDeck
    , p1Cards = Dict.fromList p1Cards
    , p1Characters = Dict.empty
    , p2Deck = otherDeck selectedDeck
    , p2Cards = Dict.fromList p2Cards
    , p1Score = 0
    , p2Score = 0
    , p2Characters = Dict.empty
    , currentPlayer = Player1
    }


otherDeck : Deck -> Deck
otherDeck deck =
    case deck of
        SlowAndSteady ->
            Flashy

        Flashy ->
            SlowAndSteady


shuffleAndStart : Deck -> Random.Generator GameState
shuffleAndStart deck =
    case deck of
        Flashy ->
            Random.map2 (startGame deck)
                (shuffle flashyDeck)
                (shuffle slowAndSteadyDeck)

        SlowAndSteady ->
            Random.map2 (startGame deck)
                (shuffle slowAndSteadyDeck)
                (shuffle flashyDeck)


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( Choosing, Cmd.none )


deckChoices : List Deck
deckChoices =
    [ Flashy, SlowAndSteady ]


deckName : Deck -> String
deckName deck =
    case deck of
        SlowAndSteady ->
            "Slow and Steady"

        Flashy ->
            "Flashy"


removeFromHand : Int -> GameState -> GameState
removeFromHand cardId state =
    { state | p1Cards = Dict.remove cardId state.p1Cards }


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectDeck deck ->
            ( model, Random.generate StartPlaying (shuffleAndStart deck) )

        StartPlaying state ->
            ( Playing state, Cmd.none )

        EndTurn ->
            case model of
                Playing state ->
                    { state | currentPlayer = nextPlayer state.currentPlayer }
                        |> Playing
                        |> (\s -> ( s, Cmd.none ))

                _ ->
                    ( model, Cmd.none )

        PlayCard cardId card ->
            case model of
                Playing state ->
                    state
                        |> playCard ( cardId, card )
                        |> removeFromHand cardId
                        |> Playing
                        |> (\s -> ( s, Cmd.none ))

                _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Muses"
    , body = [ h1 [] [ text "Muses" ], viewGame model ]
    }


viewGame : Game -> Html Msg
viewGame game =
    case game of
        Choosing ->
            choices

        Playing state ->
            viewPlaying state

        Complete ->
            text "Someone won!"


choices : Html Msg
choices =
    ul [] <| List.map choice deckChoices


choice : Deck -> Html Msg
choice deck =
    li [ onClick <| SelectDeck deck ] [ text <| deckName deck ]


viewPlaying : GameState -> Html Msg
viewPlaying state =
    case state.currentPlayer of
        Player1 ->
            player1View state

        Player2 ->
            player2View state


player1View : GameState -> Html Msg
player1View { p1Deck, p1Cards, p1Score, p1Characters, p2Score, p2Characters } =
    div []
        [ h3 [] [ text <| "Player2" ]
        , div [] [ text <| "Score: " ++ String.fromInt p2Score ]
        , h4 [] [ text "Characters" ]
        , ul [] <| List.map viewCharacter <| Dict.toList p2Characters
        , h3 [] [ text <| "Me - " ++ deckName p1Deck ]
        , div [] [ text <| "Score: " ++ String.fromInt p1Score ]
        , h4 [] [ text "Characters" ]
        , ul [] <| List.map viewCharacter <| Dict.toList p1Characters
        , h4 [] [ text "Hand" ]
        , ul [] <| List.map viewCard <| Dict.toList p1Cards
        , button [ onClick EndTurn ] [ text "End Turn" ]
        ]


player2View : GameState -> Html Msg
player2View { p2Deck, p2Cards, p1Score, p1Characters, p2Score, p2Characters } =
    div []
        [ h3 [] [ text <| "Player 1" ]
        , div [] [ text <| "Score: " ++ String.fromInt p1Score ]
        , h4 [] [ text "Characters" ]
        , ul [] <| List.map viewCharacter <| Dict.toList p1Characters
        , h3 [] [ text <| "Me - " ++ deckName p2Deck ]
        , div [] [ text <| "Score: " ++ String.fromInt p2Score ]
        , h4 [] [ text "Characters" ]
        , ul [] <| List.map viewCharacter <| Dict.toList p2Characters
        , h4 [] [ text "Hand" ]
        , ul [] <| List.map viewCard <| Dict.toList p2Cards
        , button [ onClick EndTurn ] [ text "End Turn" ]
        ]


viewCard : ( Int, Card ) -> Html Msg
viewCard ( id, card ) =
    case card of
        Character (Progress n) ->
            li [ onClick (PlayCard id card) ] [ text <| "Character - " ++ String.fromInt n ]

        OneShot (GenerateProgress (Progress n)) ->
            li [ onClick (PlayCard id card) ] [ text <| "OneShot - " ++ String.fromInt n ]


viewCharacter : ( Int, Card ) -> Html Msg
viewCharacter ( id, card ) =
    case card of
        Character (Progress n) ->
            li [] [ text <| "Character - " ++ String.fromInt n ]

        OneShot _ ->
            li [] [ text "Not Allowed" ]
