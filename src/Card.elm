module Card exposing
    ( Card(..)
    , Deck(..)
    , Power(..)
    , WithId
    , cardsForDeck
    , deckChoices
    , deckName
    , magnitude
    , magnitudeString
    , name
    , opponentDeck
    , shuffle
    )

import Array
import MuseumPoints exposing (MuseumPoints(..))
import Random
import Random.Array


type alias WithId =
    ( Int, Card )


type Card
    = Character String MuseumPoints
    | OneShot String Power


type Power
    = GeneratePoints MuseumPoints


name : Card -> String
name card =
    case card of
        Character n _ ->
            n

        OneShot n _ ->
            n


magnitude : Card -> MuseumPoints
magnitude card =
    case card of
        Character _ points ->
            points

        OneShot _ (GeneratePoints points) ->
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
    = Tragedy
    | Comedy
    | EpicPoetry
    | DivinePoetry


opponentDeck : Deck -> Random.Generator Deck
opponentDeck myDeck =
    Random.uniform myDeck (otherDecks myDeck)


otherDecks : Deck -> List Deck
otherDecks deck =
    List.filter (\choice -> choice /= deck) deckChoices


deckChoices : List Deck
deckChoices =
    [ Tragedy, Comedy, EpicPoetry, DivinePoetry ]


deckName : Deck -> String
deckName deck =
    case deck of
        Tragedy ->
            "Melpomene (Tragedy)"

        Comedy ->
            "Thalia (Comedy)"

        EpicPoetry ->
            "Calliope (Epic Poetry)"

        DivinePoetry ->
            "Polyhymnia (Divine Poetry)"


cardsForDeck : Deck -> List Card
cardsForDeck deck =
    case deck of
        Tragedy ->
            tragedyDeck

        Comedy ->
            comedyDeck

        EpicPoetry ->
            epicPoetryDeck

        DivinePoetry ->
            divinePoetryDeck


tragedyDeck : List Card
tragedyDeck =
    [ OneShot "One Shot" (GeneratePoints <| MuseumPoints 10)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 20)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 30)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 20)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 10)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 10)
    ]


comedyDeck : List Card
comedyDeck =
    [ OneShot "One Shot" (GeneratePoints <| MuseumPoints 10)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 20)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 30)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 20)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 10)
    , OneShot "One Shot" (GeneratePoints <| MuseumPoints 10)
    ]


epicPoetryDeck : List Card
epicPoetryDeck =
    [ Character "Atalanta" (MuseumPoints 2)
    , Character "Jason" (MuseumPoints 3)
    , Character "Theseus" (MuseumPoints 2)
    ]


divinePoetryDeck : List Card
divinePoetryDeck =
    [ Character "Character" (MuseumPoints 1)
    , Character "Character" (MuseumPoints 2)
    , Character "Character" (MuseumPoints 3)
    , Character "Character" (MuseumPoints 4)
    , Character "Character" (MuseumPoints 5)
    , Character "Character" (MuseumPoints 6)
    ]
