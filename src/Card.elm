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
    case otherDecks myDeck of
        -- play a mirror if there are no other decks
        [] ->
            Random.constant myDeck

        first :: rest ->
            Random.uniform first rest


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
    [ Character "Sophocles" (MuseumPoints 1)
    , Character "Erinyes" (MuseumPoints 2)
    , Character "Agammemnon" (MuseumPoints 3)
    , Character "Antigone" (MuseumPoints 2)
    , Character "Siren" (MuseumPoints 1)
    , Character "Siren" (MuseumPoints 1)
    , Character "Siren" (MuseumPoints 1)
    ]


comedyDeck : List Card
comedyDeck =
    [ Character "Aristophanes" (MuseumPoints 1)
    , Character "Amphitheater" (MuseumPoints 1)
    , Character "Amphitheater" (MuseumPoints 1)
    , OneShot "Satyr Play" (GeneratePoints <| MuseumPoints 10)
    , OneShot "Satyr Play" (GeneratePoints <| MuseumPoints 10)
    , OneShot "Satyr Play" (GeneratePoints <| MuseumPoints 10)
    ]


epicPoetryDeck : List Card
epicPoetryDeck =
    [ Character "Atalanta" (MuseumPoints 2)
    , Character "Jason" (MuseumPoints 3)
    , Character "Theseus" (MuseumPoints 2)
    , Character "Homer" (MuseumPoints 1)
    , Character "Arctinus" (MuseumPoints 1)
    ]


divinePoetryDeck : List Card
divinePoetryDeck =
    [ Character "Hesiod" (MuseumPoints 1)
    , Character "Prometheus" (MuseumPoints 2)
    , Character "Artemis" (MuseumPoints 3)
    , Character "Poseidon" (MuseumPoints 4)
    , Character "Apollo" (MuseumPoints 5)
    , OneShot "Hymn" (GeneratePoints <| MuseumPoints 10)
    , OneShot "Hymn" (GeneratePoints <| MuseumPoints 10)
    , OneShot "Hymn" (GeneratePoints <| MuseumPoints 10)
    ]
