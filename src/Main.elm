module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.Array


type alias GameState =
    { selectedDeck : Deck
    , myCards : List Card
    , myScore : Int
    , opponentCards : List Card
    , opponentScore : Int
    }


type Game
    = Choosing
    | Playing GameState
    | Complete


type alias Model =
    Game


type Msg
    = SelectDeck Deck
    | StartPlaying GameState


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


shuffle : List Card -> Random.Generator (List Card)
shuffle cards =
    cards
        |> Array.fromList
        |> Random.Array.shuffle
        |> Random.map Array.toList


startGame : Deck -> List Card -> List Card -> GameState
startGame selectedDeck myCards opponentCards =
    { selectedDeck = selectedDeck
    , myCards = myCards
    , opponentCards = opponentCards
    , myScore = 100
    , opponentScore = 100
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectDeck deck ->
            ( model, Random.generate StartPlaying (shuffleAndStart deck) )

        StartPlaying state ->
            ( Playing state, Cmd.none )


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


viewPlaying : GameState -> Html a
viewPlaying { selectedDeck, myCards, myScore, opponentScore } =
    div []
        [ h3 [] [ text <| "Oponnent" ]
        , div [] [ text <| "Score: " ++ String.fromInt opponentScore ]
        , h3 [] [ text <| "Me - " ++ deckName selectedDeck ]
        , div [] [ text <| "Score: " ++ String.fromInt myScore ]
        , ul [] <| List.map viewCard myCards
        ]


viewCard : Card -> Html a
viewCard card =
    case card of
        Character (Progress n) ->
            li [] [ text <| "Character - " ++ String.fromInt n ]

        OneShot (GenerateProgress (Progress n)) ->
            li [] [ text <| "OneShot - " ++ String.fromInt n ]
