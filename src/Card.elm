module Card exposing
    ( Card(..)
    , Deck(..)
    , Power(..)
    , Trait(..)
    , WithId
    , cardsForDeck
    , deckChoices
    , deckName
    , magnitude
    , magnitudeString
    , name
    , opponentDeck
    , shuffle
    , traitName
    )

import Array
import MuseumPoints exposing (MuseumPoints(..))
import Random
import Random.Array


type alias WithId =
    ( Int, Card )


type Card
    = Character String MuseumPoints (List Trait)
    | OneShot String Power


type Trait
    = Female
    | GreatPoet
    | Seafarer
    | Underworld
    | Ruler
    | Beast
    | Small


traitName : Trait -> String
traitName trait =
    case trait of
        Female ->
            "Female"

        GreatPoet ->
            "Great Poet"

        Seafarer ->
            "Seafarer"

        Underworld ->
            "Underworld"

        Ruler ->
            "Ruler"

        Beast ->
            "Beast"

        Small ->
            "Small"


type Power
    = GeneratePoints MuseumPoints


name : Card -> String
name card =
    case card of
        Character n _ _ ->
            n

        OneShot n _ ->
            n


magnitude : Card -> MuseumPoints
magnitude card =
    case card of
        Character _ points _ ->
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
    [ sophocles
    , erinyes
    , agammemnon
    , antigone
    , siren
    , siren
    , siren
    ]


comedyDeck : List Card
comedyDeck =
    [ aristophanes
    , amphitheater
    , amphitheater
    , satyrPlay
    , satyrPlay
    , satyrPlay
    ]


epicPoetryDeck : List Card
epicPoetryDeck =
    [ atalanta
    , jason
    , theseus
    , homer
    , arctinus
    ]


divinePoetryDeck : List Card
divinePoetryDeck =
    [ hesiod
    , prometheus
    , artemis
    , poseidon
    , apollo
    , hymn
    , hymn
    , hymn
    ]



-- INDIVIDUAL CARDS


sophocles : Card
sophocles =
    Character "Sophocles" (MuseumPoints 1) [ GreatPoet ]


erinyes : Card
erinyes =
    Character "Erinyes" (MuseumPoints 2) []


agammemnon : Card
agammemnon =
    Character "Agammemnon" (MuseumPoints 3) [ Ruler ]


antigone : Card
antigone =
    Character "Antigone" (MuseumPoints 2) [ Female ]


siren : Card
siren =
    Character "Siren" (MuseumPoints 1) [ Female ]


aristophanes : Card
aristophanes =
    Character "Aristophanes" (MuseumPoints 1) [ GreatPoet ]


amphitheater : Card
amphitheater =
    Character "Amphitheater" (MuseumPoints 1) []


satyrPlay : Card
satyrPlay =
    OneShot "Satyr Play" (GeneratePoints <| MuseumPoints 10)


atalanta : Card
atalanta =
    Character "Atalanta" (MuseumPoints 2) [ Female, Seafarer ]


jason : Card
jason =
    Character "Jason" (MuseumPoints 3) [ Seafarer ]


theseus : Card
theseus =
    Character "Theseus" (MuseumPoints 2) [ Seafarer, Underworld ]


homer : Card
homer =
    Character "Homer" (MuseumPoints 1) [ GreatPoet ]


arctinus : Card
arctinus =
    Character "Arctinus" (MuseumPoints 1) [ GreatPoet ]


hesiod : Card
hesiod =
    Character "Hesiod" (MuseumPoints 1) [ GreatPoet ]


prometheus : Card
prometheus =
    Character "Prometheus" (MuseumPoints 2) []


artemis : Card
artemis =
    Character "Artemis" (MuseumPoints 3) [ Female ]


poseidon : Card
poseidon =
    Character "Poseidon" (MuseumPoints 4) [ Seafarer ]


apollo : Card
apollo =
    Character "Apollo" (MuseumPoints 5) []


hymn : Card
hymn =
    OneShot "Hymn" (GeneratePoints <| MuseumPoints 10)
