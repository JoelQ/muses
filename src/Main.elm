module Main exposing (main)

import Browser
import Card exposing (Card(..), Deck)
import Dict
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
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
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Game exposing (Game, GameState)
import GameSlot exposing (GameSlot(..))
import Html exposing (Html)
import Html.Attributes
import MuseumPoints exposing (MuseumPoints(..))
import Player exposing (Player)
import Random


type alias Model =
    Game


type Msg
    = SelectDeck Card.Deck
    | StartPlaying GameState
    | PlayCard Int Card
    | SelectCard Int
    | PlayCardToSlot Int Card GameSlot
    | EndTurn


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
    ( Game.Choosing, Cmd.none )


withNoCmd : Model -> ( Model, Cmd Msg )
withNoCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectDeck deck ->
            ( model, Random.generate StartPlaying (Game.shuffleAndStart deck) )

        StartPlaying state ->
            ( Game.Playing state, Cmd.none )

        EndTurn ->
            model
                |> Game.map Game.playCurrentCharacters
                |> Game.andThen Game.checkWin
                |> Game.map Game.drawCard
                |> Game.map Game.resetCurrentPlayer
                |> Game.map Game.swapPlayers
                |> withNoCmd

        PlayCard cardId card ->
            model
                |> Game.map (Game.playCard ( cardId, card ))
                |> Game.andThen Game.checkWin
                |> withNoCmd

        SelectCard cardId ->
            model
                |> Game.map (Game.selectCard cardId)
                |> withNoCmd

        PlayCardToSlot cardId card slot ->
            model
                |> Game.map (Game.playCardToSlot ( cardId, card ) slot)
                |> withNoCmd


view : Model -> Browser.Document Msg
view model =
    { title = "Muses"
    , body = [ Html.h1 [] [ Html.text "Muses" ], viewGame model ]
    }


viewGame : Game -> Html Msg
viewGame game =
    case game of
        Game.Choosing ->
            choiceScreen

        Game.Playing state ->
            Element.layout [] <| viewPlaying state

        Game.Complete winner ->
            winScreen winner


winScreen : Player -> Html a
winScreen winner =
    Element.layout [] <|
        column [ centerX, spacing 20 ]
            [ Element.text "The winner is:"
            , portrait [] musePortraitHeight winner.deck
            ]


musePortraitHeight : Int
musePortraitHeight =
    300


choiceScreen : Html Msg
choiceScreen =
    Element.layout [] <|
        column [ centerX, spacing 20 ]
            [ Element.text "Choose a deck"
            , choices
            ]


choices : Element Msg
choices =
    row [ spacing 10 ] <|
        List.map (choice musePortraitHeight) Card.deckChoices


choice : Int -> Card.Deck -> Element Msg
choice cardHeight deck =
    portrait [ Events.onClick <| SelectDeck deck ] cardHeight deck


portrait : List (Element.Attribute a) -> Int -> Card.Deck -> Element a
portrait attributes cardHeight deck =
    column [ spacing 10 ]
        [ cardOutline cardHeight
            ((Background.image <| Card.deckPortrait deck) :: attributes)
            []
        , el [ centerX ] <| Element.text <| Card.deckName deck
        ]


viewPlaying : GameState -> Element Msg
viewPlaying { currentPlayer, otherPlayer, slots } =
    column [ width Element.fill ]
        [ otherPlayerView otherPlayer
        , centerSection currentPlayer slots
        , currentPlayerView currentPlayer
        ]


blanchedAlmond : Element.Color
blanchedAlmond =
    Element.rgb255 255 235 205


burlywood : Element.Color
burlywood =
    Element.rgb255 222 184 135


centerSection : Player -> List GameSlot -> Element Msg
centerSection player slots =
    row
        [ width Element.fill
        , Background.color blanchedAlmond
        , padding 15
        ]
        [ el [ alignLeft, centerY, padding 10 ] <|
            Input.button []
                { onPress = Just EndTurn, label = Element.text "End Turn" }
        , el [ centerX ] <| gameSlots 200 (Player.selectedCard player) slots
        ]


otherPlayerView : Player -> Element Msg
otherPlayerView otherPlayer =
    row
        [ width Element.fill
        , padding 15
        , Background.color burlywood
        ]
        [ el [ alignLeft, centerY ] <| playerHighlights otherPlayer
        , el [ centerX ] <| playerHandBacks otherPlayer
        ]


currentPlayerView : Player -> Element Msg
currentPlayerView currentPlayer =
    row
        [ width Element.fill
        , Background.color burlywood
        , padding 15
        ]
        [ el [ alignLeft ] <| playerHighlights currentPlayer
        , el [ centerX ] <| playerHand currentPlayer
        ]


playerHighlights : Player -> Element a
playerHighlights player =
    column [ padding 10, spacing 15 ]
        [ Element.text <| Card.deckName player.deck
        , deckLogo player.deck
        , el [] <| viewScore player.score
        ]


playerHandBacks : Player -> Element Msg
playerHandBacks player =
    cardListBack 200 <| Dict.toList player.hand


playerHand : Player -> Element Msg
playerHand player =
    cardList 200 player.selected <| Dict.toList player.hand


deckLogo : Deck -> Element a
deckLogo deck =
    el
        [ width <| px 100
        , height <| px 100
        , Background.image <| Card.deckSymbol deck
        , Border.width 3
        , Border.rounded 10
        ]
        Element.none


gameSlots : Int -> Maybe Card.WithId -> List GameSlot -> Element Msg
gameSlots cardHeight selected slots =
    row [ spacing 10, centerX ] <|
        List.map (gameSlot cardHeight selected) slots


gameSlot : Int -> Maybe Card.WithId -> GameSlot -> Element Msg
gameSlot cardHeight selected slot =
    case slot of
        Filled _ _ card ->
            cardOutline cardHeight [] (cardContents cardHeight card)

        Open requirements ->
            case selected of
                Just ( id, (Character _ _ traits) as card ) ->
                    if List.member requirements traits then
                        selectableSlot cardHeight id card slot

                    else
                        emptySlot cardHeight slot

                _ ->
                    emptySlot cardHeight slot


selectableSlot : Int -> Int -> Card -> GameSlot -> Element Msg
selectableSlot cardHeight id card slot =
    cardOutline cardHeight
        [ Border.dashed
        , Border.color (Element.rgb 0 1 0)
        , Events.onClick (PlayCardToSlot id card slot)
        ]
        [ el [ centerX, centerY ] (Element.text <| GameSlot.name slot)
        ]


emptySlot : Int -> GameSlot -> Element a
emptySlot cardHeight slot =
    cardOutline cardHeight
        [ Border.dashed ]
        [ el [ centerX, centerY ] (Element.text <| GameSlot.name slot)
        ]


viewScore : Int -> Element a
viewScore score =
    row [ spacing 5 ]
        [ el [] <| Element.text (String.fromInt score)
        , el [] <| Element.html <| scoreProgressBar score
        ]


scoreProgressBar : Int -> Html a
scoreProgressBar score =
    Html.progress
        [ Html.Attributes.max "100"
        , Html.Attributes.value (String.fromInt score)
        ]
        [ Html.text (String.fromInt score) ]


cardRatio : Float
cardRatio =
    1.4


imageRatio : Float
imageRatio =
    1.2


cardImage : Int -> Card -> Element a
cardImage cardHeight card =
    let
        imageWidth =
            round <| (toFloat cardHeight / cardRatio) / imageRatio

        imageHeight =
            round <| toFloat imageWidth / cardRatio
    in
    Element.image [ width <| px imageWidth, height <| px imageHeight ]
        { src = Card.imagePath card
        , description = Card.name card
        }


cardBack : Int -> Element a
cardBack cardHeight =
    cardOutline cardHeight
        [ Background.image "images/muses-meta/card-back.jpg"
        ]
        []


cardOutline :
    Int
    -> List (Element.Attribute msg)
    -> List (Element msg)
    -> Element msg
cardOutline cardHeight extrAttrs contents =
    let
        defaultAttrs =
            [ spacing 5
            , padding 5
            , Border.width 4
            , Border.rounded 10
            , width <| px <| round <| toFloat cardHeight / cardRatio
            , height (px cardHeight)
            ]
    in
    column (defaultAttrs ++ extrAttrs) contents


cardContents : Int -> Card -> List (Element a)
cardContents cardHeight card =
    [ row [ width fill ]
        [ el [ alignLeft ] <| Element.text (Card.name card)
        , el [ alignRight ] <| Element.text (Card.magnitudeString card)
        ]
    , el [ centerX, padding 5 ] (cardImage cardHeight card)
    , paragraph [ centerX, Font.italic, Font.size 14 ]
        [ Element.text <| Card.flavorText card ]
    ]


clickEvent : Card.WithId -> Element.Attribute Msg
clickEvent ( id, card ) =
    case card of
        OneShot _ _ ->
            Events.onClick (PlayCard id card)

        Character _ _ _ ->
            Events.onClick (SelectCard id)


cardElement : Int -> Maybe Int -> Card.WithId -> Element Msg
cardElement cardHeight selected ( id, card ) =
    cardOutline cardHeight
        [ clickEvent ( id, card ), borderColor selected id ]
        (cardContents cardHeight card)


borderColor : Maybe Int -> Int -> Element.Attribute a
borderColor selected currentId =
    if selected == Just currentId then
        Border.color (Element.rgb 0 1 0)

    else
        Border.color (Element.rgb 0 0 0)


cardListBack : Int -> List a -> Element msg
cardListBack cardHeight cards =
    row [ spacing 10, centerX ] <|
        List.map (always <| cardBack cardHeight) cards


cardList : Int -> Maybe Int -> List Card.WithId -> Element Msg
cardList cardHeight selected cards =
    row [ spacing 10, centerX ] <|
        List.map (cardElement cardHeight selected) cards
