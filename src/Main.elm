module Main exposing (main)

import Array
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.Array


type alias GameState =
    { selectedDeck : Deck
    , myCards : Dict Int Card
    , myScore : Int
    , opponentCards : Dict Int Card
    , opponentScore : Int
    }


playCard : Card -> GameState -> GameState
playCard card state =
    case card of
        OneShot (GenerateProgress (Progress n)) ->
            { state | myScore = state.myScore + n }

        Character _ ->
            state


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
startGame selectedDeck myCards opponentCards =
    { selectedDeck = selectedDeck
    , myCards = Dict.fromList myCards
    , opponentCards = Dict.fromList opponentCards
    , myScore = 0
    , opponentScore = 0
    }


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
    { state | myCards = Dict.remove cardId state.myCards }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectDeck deck ->
            ( model, Random.generate StartPlaying (shuffleAndStart deck) )

        StartPlaying state ->
            ( Playing state, Cmd.none )

        PlayCard cardId card ->
            case model of
                Playing state ->
                    state
                        |> playCard card
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
viewPlaying { selectedDeck, myCards, myScore, opponentScore } =
    div []
        [ h3 [] [ text <| "Oponnent" ]
        , div [] [ text <| "Score: " ++ String.fromInt opponentScore ]
        , h3 [] [ text <| "Me - " ++ deckName selectedDeck ]
        , div [] [ text <| "Score: " ++ String.fromInt myScore ]
        , ul [] <| List.map viewCard <| Dict.toList myCards
        ]


viewCard : ( Int, Card ) -> Html Msg
viewCard ( id, card ) =
    case card of
        Character (Progress n) ->
            li [] [ text <| "Character - " ++ String.fromInt n ]

        OneShot (GenerateProgress (Progress n)) ->
            li [ onClick (PlayCard id card) ] [ text <| "OneShot - " ++ String.fromInt n ]
