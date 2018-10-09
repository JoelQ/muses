module Main exposing (main)

import Array
import Browser
import Dict exposing (Dict)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , column
        , el
        , fill
        , height
        , padding
        , paragraph
        , px
        , row
        , spacing
        , width
        )
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (..)
import Html.Attributes exposing (max, value)
import Html.Events exposing (onClick)
import Random
import Random.Array



-- PLAYER


type alias Player =
    { name : String
    , deck : Deck
    , cards : Dict Int Card
    , score : Int
    , characters : Dict Int Card
    }


scoreFromCharacters : Player -> Int
scoreFromCharacters { characters } =
    Dict.values characters
        |> List.map magnitude
        |> List.map rawPoints
        |> List.sum


addPointsFromCharacters : Player -> Player
addPointsFromCharacters player =
    increaseScore (scoreFromCharacters player) player


increaseScore : Int -> Player -> Player
increaseScore n player =
    { player | score = player.score + n }


playCharacter : Int -> Card -> Player -> Player
playCharacter id card player =
    { player | characters = Dict.insert id card player.characters }


initialPlayer : String -> Deck -> List ( Int, Card ) -> Player
initialPlayer name deck cards =
    Player name deck (Dict.fromList cards) 0 Dict.empty


removeFromPlayerHand : Int -> Player -> Player
removeFromPlayerHand cardId player =
    { player | cards = Dict.remove cardId player.cards }



-- GAME STATE


type alias GameState =
    { currentPlayer : Player
    , otherPlayer : Player
    }


playCard : ( Int, Card ) -> GameState -> GameState
playCard ( id, card ) state =
    case card of
        OneShot (GeneratePoints (MuseumPoints n)) ->
            modifyCurrentPlayer (increaseScore n) state

        Character _ ->
            modifyCurrentPlayer (playCharacter id card) state


startGame : Deck -> List ( Int, Card ) -> List ( Int, Card ) -> GameState
startGame selectedDeck p1Cards p2Cards =
    { currentPlayer = initialPlayer "Player 1" selectedDeck p1Cards
    , otherPlayer = initialPlayer "Player 2" (otherDeck selectedDeck) p2Cards
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


removeFromHand : Int -> GameState -> GameState
removeFromHand cardId state =
    modifyCurrentPlayer (removeFromPlayerHand cardId) state


playCurrentCharacters : GameState -> GameState
playCurrentCharacters =
    modifyCurrentPlayer addPointsFromCharacters


swapPlayers : GameState -> GameState
swapPlayers { currentPlayer, otherPlayer } =
    { currentPlayer = otherPlayer, otherPlayer = currentPlayer }


modifyCurrentPlayer : (Player -> Player) -> GameState -> GameState
modifyCurrentPlayer playerFunction state =
    { state | currentPlayer = playerFunction state.currentPlayer }



-- GAME


type Game
    = Choosing
    | Playing GameState
    | Complete


checkWin : GameState -> Game
checkWin ({ currentPlayer, otherPlayer } as state) =
    if currentPlayer.score >= 100 || otherPlayer.score >= 100 then
        Complete

    else
        Playing state


andThen : (GameState -> Game) -> Game -> Game
andThen function game =
    case game of
        Playing state ->
            function state

        _ ->
            game


map : (GameState -> GameState) -> Game -> Game
map function =
    andThen (Playing << function)


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


type MuseumPoints
    = MuseumPoints Int


rawPoints : MuseumPoints -> Int
rawPoints (MuseumPoints n) =
    n


type Power
    = GeneratePoints MuseumPoints


type Card
    = Character MuseumPoints
    | OneShot Power


slowAndSteadyDeck : List Card
slowAndSteadyDeck =
    [ Character (MuseumPoints 1)
    , Character (MuseumPoints 2)
    , Character (MuseumPoints 3)
    , Character (MuseumPoints 4)
    , Character (MuseumPoints 5)
    , Character (MuseumPoints 6)
    ]


flashyDeck : List Card
flashyDeck =
    [ OneShot (GeneratePoints <| MuseumPoints 10)
    , OneShot (GeneratePoints <| MuseumPoints 20)
    , OneShot (GeneratePoints <| MuseumPoints 30)
    , OneShot (GeneratePoints <| MuseumPoints 20)
    , OneShot (GeneratePoints <| MuseumPoints 10)
    , OneShot (GeneratePoints <| MuseumPoints 10)
    ]


shuffle : List Card -> Random.Generator (List ( Int, Card ))
shuffle cards =
    cards
        |> Array.fromList
        |> Random.Array.shuffle
        |> Random.map Array.toList
        |> Random.map (List.indexedMap Tuple.pair)


otherDeck : Deck -> Deck
otherDeck deck =
    case deck of
        SlowAndSteady ->
            Flashy

        Flashy ->
            SlowAndSteady


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


withNoCmd : Model -> ( Model, Cmd Msg )
withNoCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectDeck deck ->
            ( model, Random.generate StartPlaying (shuffleAndStart deck) )

        StartPlaying state ->
            ( Playing state, Cmd.none )

        EndTurn ->
            model
                |> map playCurrentCharacters
                |> andThen checkWin
                |> map swapPlayers
                |> withNoCmd

        PlayCard cardId card ->
            model
                |> map (playCard ( cardId, card ))
                |> map (removeFromHand cardId)
                |> andThen checkWin
                |> withNoCmd


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
        [ h3 [] [ text <| otherPlayer.name ]
        , div [] [ viewScore otherPlayer.score ]
        , cardListBack 210 <| Dict.toList otherPlayer.cards
        , cardList 210 <| Dict.toList otherPlayer.characters
        , hr [] []
        , cardList 210 <| Dict.toList currentPlayer.characters
        , cardList 210 <| Dict.toList currentPlayer.cards
        , div [] [ viewScore currentPlayer.score ]
        , h3 [] [ text <| "Me - " ++ deckName currentPlayer.deck ]
        , button [ onClick EndTurn ] [ text "End Turn" ]
        ]


viewScore : Int -> Html a
viewScore score =
    div []
        [ text (String.fromInt score)
        , Html.progress [ max "100", value (String.fromInt score) ]
            [ text (String.fromInt score) ]
        ]


cardName : Card -> String
cardName card =
    case card of
        Character _ ->
            "Character"

        OneShot _ ->
            "OneShot"


magnitude : Card -> MuseumPoints
magnitude card =
    case card of
        Character points ->
            points

        OneShot (GeneratePoints points) ->
            points


magnitudeString : Card -> String
magnitudeString =
    String.fromInt << rawPoints << magnitude


cardRatio : Float
cardRatio =
    1.4


imageRatio : Float
imageRatio =
    1.2


cardImage : Int -> Element Msg
cardImage cardHeight =
    let
        imageWidth =
            round <| (toFloat cardHeight / cardRatio) / imageRatio

        imageHeight =
            round <| toFloat imageWidth / cardRatio

        placeholderString =
            String.fromInt imageWidth ++ "x" ++ String.fromInt imageHeight
    in
    Element.image []
        { src = "https://via.placeholder.com/" ++ placeholderString
        , description = "placeholder"
        }


cardBack : Int -> Element a
cardBack cardHeight =
    column
        [ spacing 5
        , padding 5
        , Border.width 4
        , Border.rounded 10
        , width <| px <| round <| toFloat cardHeight / cardRatio
        , height (px cardHeight)
        ]
        []


cardElement : Int -> ( Int, Card ) -> Element Msg
cardElement cardHeight ( id, card ) =
    column
        [ spacing 5
        , padding 5
        , Border.width 4
        , Border.rounded 10
        , width <| px <| round <| toFloat cardHeight / cardRatio
        , height (px cardHeight)
        , Events.onClick (PlayCard id card)
        ]
        [ row [ width fill ]
            [ el [ alignLeft ] <| Element.text (cardName card)
            , el [ alignRight ] <| Element.text (magnitudeString card)
            ]
        , el [ centerX, padding 5 ] (cardImage cardHeight)
        , paragraph [ centerX, Font.italic, Font.size 14 ]
            [ Element.text "Some colorful flavor text that keeps going and going and going" ]
        ]


cardListBack : Int -> List a -> Html msg
cardListBack cardHeight cards =
    Element.layout [] (row [ spacing 10 ] <| List.map (always <| cardBack cardHeight) cards)


cardList : Int -> List ( Int, Card ) -> Html Msg
cardList cardHeight cards =
    Element.layout [] (row [ spacing 10 ] <| List.map (cardElement cardHeight) cards)
