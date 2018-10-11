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
import Game exposing (Game, GameState)
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
                |> Game.map Game.swapPlayers
                |> withNoCmd

        PlayCard cardId card ->
            model
                |> Game.map (Game.playCard ( cardId, card ))
                |> Game.map (Game.removeFromHand cardId)
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
viewPlaying { currentPlayer, otherPlayer } =
    div []
        [ h3 [] [ text <| otherPlayer.name ++ " " ++ Card.deckName otherPlayer.deck ]
        , div [] [ viewScore otherPlayer.score ]
        , cardListBack 210 <| Dict.toList otherPlayer.cards
        , cardList 210 <| Dict.toList otherPlayer.characters
        , hr [] []
        , cardList 210 <| Dict.toList currentPlayer.characters
        , cardList 210 <| Dict.toList currentPlayer.cards
        , div [] [ viewScore currentPlayer.score ]
        , h3 [] [ text <| "Me - " ++ Card.deckName currentPlayer.deck ]
        , button [ onClick EndTurn ] [ text "End Turn" ]
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


cardElement : Int -> Card.WithId -> Element Msg
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
            [ el [ alignLeft ] <| Element.text (Card.name card)
            , el [ alignRight ] <| Element.text (Card.magnitudeString card)
            ]
        , el [ centerX, padding 5 ] (cardImage cardHeight)
        , paragraph [ centerX, Font.italic, Font.size 14 ]
            [ Element.text "Some colorful flavor text that keeps going and going and going" ]
        ]


cardListBack : Int -> List a -> Html msg
cardListBack cardHeight cards =
    Element.layout [] (row [ spacing 10 ] <| List.map (always <| cardBack cardHeight) cards)


cardList : Int -> List Card.WithId -> Html Msg
cardList cardHeight cards =
    Element.layout [] (row [ spacing 10 ] <| List.map (cardElement cardHeight) cards)
