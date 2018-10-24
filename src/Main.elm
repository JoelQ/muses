module Main exposing (main)

import Browser
import Card exposing (Card(..))
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
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Game exposing (Game, GameSlot, GameState)
import Html exposing (..)
import Html.Attributes exposing (max, value)
import Html.Events exposing (onClick)
import MuseumPoints exposing (MuseumPoints(..))
import Player exposing (Player)
import Random


type alias Model =
    Game


type Msg
    = SelectDeck Card.Deck
    | StartPlaying GameState
    | PlayCard Int Card
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
                |> Game.map Game.resetPlayedCardCount
                |> Game.map Game.swapPlayers
                |> withNoCmd

        PlayCard cardId card ->
            model
                |> Game.map (Game.playCard ( cardId, card ))
                |> Game.andThen Game.checkWin
                |> withNoCmd


view : Model -> Browser.Document Msg
view model =
    { title = "Muses"
    , body = [ h1 [] [ text "Muses" ], viewGame model ]
    }


viewGame : Game -> Html Msg
viewGame game =
    case game of
        Game.Choosing ->
            choices

        Game.Playing state ->
            viewPlaying state

        Game.Complete ->
            text "Someone won!"


choices : Html Msg
choices =
    ul [] <| List.map choice Card.deckChoices


choice : Card.Deck -> Html Msg
choice deck =
    li [ onClick <| SelectDeck deck ] [ text <| Card.deckName deck ]


viewPlaying : GameState -> Html Msg
viewPlaying { currentPlayer, otherPlayer, slots } =
    div []
        [ h3 [] [ text <| otherPlayer.name ++ " " ++ Card.deckName otherPlayer.deck ]
        , div [] [ viewScore otherPlayer.score ]
        , cardListBack 210 <| Dict.toList otherPlayer.hand
        , hr [] []
        , gameSlots 210 slots
        , hr [] []
        , cardList 210 <| Dict.toList currentPlayer.hand
        , div [] [ viewScore currentPlayer.score ]
        , h3 [] [ text <| "Me - " ++ Card.deckName currentPlayer.deck ]
        , button [ onClick EndTurn ] [ text "End Turn" ]
        ]


gameSlots : Int -> List GameSlot -> Html Msg
gameSlots cardHeight slots =
    Element.layout [] <|
        row [ spacing 10 ] <|
            List.map (gameSlot cardHeight) slots


gameSlot : Int -> GameSlot -> Element Msg
gameSlot cardHeight slot =
    case slot.card of
        Just card ->
            cardOutline cardHeight [] (cardContents cardHeight card)

        Nothing ->
            cardOutline cardHeight
                [ Border.dashed ]
                [ el [ centerX, centerY ] (Element.text <| Game.slotName slot)
                ]


viewScore : Int -> Html a
viewScore score =
    div []
        [ text (String.fromInt score)
        , Html.progress [ max "100", value (String.fromInt score) ]
            [ text (String.fromInt score) ]
        ]


cardRatio : Float
cardRatio =
    1.4


imageRatio : Float
imageRatio =
    1.2


cardImage : Int -> Element a
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
    cardOutline cardHeight [] []


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
    , el [ centerX, padding 5 ] (cardImage cardHeight)
    , paragraph [ centerX, Font.italic, Font.size 14 ]
        [ Element.text "Some colorful flavor text that keeps going and going and going" ]
    ]


cardElement : Int -> Card.WithId -> Element Msg
cardElement cardHeight ( id, card ) =
    cardOutline cardHeight
        [ Events.onClick (PlayCard id card) ]
        (cardContents cardHeight card)


cardListBack : Int -> List a -> Html msg
cardListBack cardHeight cards =
    Element.layout [] (row [ spacing 10 ] <| List.map (always <| cardBack cardHeight) cards)


cardList : Int -> List Card.WithId -> Html Msg
cardList cardHeight cards =
    Element.layout [] (row [ spacing 10 ] <| List.map (cardElement cardHeight) cards)
