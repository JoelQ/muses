module Main exposing (main)

import Array
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.Array


type alias Player =
    { deck : Deck
    , cards : Dict Int Card
    , score : Int
    , characters : Dict Int Card
    }


type alias GameState =
    { currentPlayer : Player
    , otherPlayer : Player
    }


increaseScore : Int -> Player -> Player
increaseScore n player =
    { player | score = player.score + n }


playCharacter : Int -> Card -> Player -> Player
playCharacter id card player =
    { player | characters = Dict.insert id card player.characters }


playCard : ( Int, Card ) -> GameState -> GameState
playCard ( id, card ) state =
    case card of
        OneShot (GenerateProgress (Progress n)) ->
            { state | currentPlayer = increaseScore n state.currentPlayer }

        Character _ ->
            { state | currentPlayer = playCharacter id card state.currentPlayer }


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


initialPlayer : Deck -> List ( Int, Card ) -> Player
initialPlayer deck cards =
    Player deck (Dict.fromList cards) 0 Dict.empty


startGame : Deck -> List ( Int, Card ) -> List ( Int, Card ) -> GameState
startGame selectedDeck p1Cards p2Cards =
    { currentPlayer = initialPlayer selectedDeck p1Cards
    , otherPlayer = initialPlayer (otherDeck selectedDeck) p2Cards
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


removeFromPlayerHand : Int -> Player -> Player
removeFromPlayerHand cardId player =
    { player | cards = Dict.remove cardId player.cards }


removeFromHand : Int -> GameState -> GameState
removeFromHand cardId state =
    { state | currentPlayer = removeFromPlayerHand cardId state.currentPlayer }


swapPlayers : GameState -> GameState
swapPlayers { currentPlayer, otherPlayer } =
    { currentPlayer = otherPlayer, otherPlayer = currentPlayer }


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
                    state
                        |> swapPlayers
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
viewPlaying { currentPlayer, otherPlayer } =
    div []
        [ h3 [] [ text <| "Player2" ]
        , div [] [ text <| "Score: " ++ String.fromInt otherPlayer.score ]
        , h4 [] [ text "Characters" ]
        , ul [] <| List.map viewCharacter <| Dict.toList otherPlayer.characters
        , h3 [] [ text <| "Me - " ++ deckName currentPlayer.deck ]
        , div [] [ text <| "Score: " ++ String.fromInt currentPlayer.score ]
        , h4 [] [ text "Characters" ]
        , ul [] <| List.map viewCharacter <| Dict.toList currentPlayer.characters
        , h4 [] [ text "Hand" ]
        , ul [] <| List.map viewCard <| Dict.toList currentPlayer.cards
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
