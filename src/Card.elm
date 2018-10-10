module Card exposing
    ( Card(..)
    , Deck(..)
    , Power(..)
    , WithId
    , deckChoices
    , deckName
    , flashyDeck
    , magnitude
    , magnitudeString
    , name
    , otherDeck
    , shuffle
    , slowAndSteadyDeck
    )

import Array
import MuseumPoints exposing (MuseumPoints(..))
import Random
import Random.Array


type alias WithId =
    ( Int, Card )


type Card
    = Character MuseumPoints
    | OneShot Power


type Power
    = GeneratePoints MuseumPoints


name : Card -> String
name card =
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
    String.fromInt << MuseumPoints.toInt << magnitude


shuffle : List Card -> Random.Generator (List WithId)
shuffle cards =
    cards
        |> Array.fromList
        |> Random.Array.shuffle
        |> Random.map Array.toList
        |> Random.map (List.indexedMap Tuple.pair)



-- DECK


type Deck
    = Flashy
    | SlowAndSteady


otherDeck : Deck -> Deck
otherDeck deck =
    case deck of
        SlowAndSteady ->
            Flashy

        Flashy ->
            SlowAndSteady


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


flashyDeck : List Card
flashyDeck =
    [ OneShot (GeneratePoints <| MuseumPoints 10)
    , OneShot (GeneratePoints <| MuseumPoints 20)
    , OneShot (GeneratePoints <| MuseumPoints 30)
    , OneShot (GeneratePoints <| MuseumPoints 20)
    , OneShot (GeneratePoints <| MuseumPoints 10)
    , OneShot (GeneratePoints <| MuseumPoints 10)
    ]


slowAndSteadyDeck : List Card
slowAndSteadyDeck =
    [ Character (MuseumPoints 1)
    , Character (MuseumPoints 2)
    , Character (MuseumPoints 3)
    , Character (MuseumPoints 4)
    , Character (MuseumPoints 5)
    , Character (MuseumPoints 6)
    ]
