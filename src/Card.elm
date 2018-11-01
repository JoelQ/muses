module Card exposing
    ( Card(..)
    , Deck(..)
    , Power(..)
    , Trait(..)
    , WithId
    , cardsForDeck
    , deckChoices
    , deckName
    , deckPortrait
    , deckSymbol
    , flavorText
    , imagePath
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
    = Character Config MuseumPoints (List Trait)
    | OneShot Config Power


type alias Config =
    { name : String
    , image : String
    , flavorText : String
    }


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


config : Card -> Config
config card =
    case card of
        Character conf _ _ ->
            conf

        OneShot conf _ ->
            conf


name : Card -> String
name =
    .name << config


imageFilename : Card -> String
imageFilename =
    .image << config


flavorText : Card -> String
flavorText =
    .flavorText << config


imagePath : Card -> String
imagePath card =
    "../images/muses-characters/" ++ imageFilename card


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


deckPortrait : Deck -> String
deckPortrait deck =
    case deck of
        Tragedy ->
            "../images/muses-meta/melpomene.jpg"

        Comedy ->
            "../images/muses-meta/thalia.jpg"

        EpicPoetry ->
            "../images/muses-meta/calliope.jpg"

        DivinePoetry ->
            "../images/muses-meta/polyhymnia.jpg"


deckSymbol : Deck -> String
deckSymbol deck =
    case deck of
        Tragedy ->
            "../images/muses-meta/tragedy.jpg"

        Comedy ->
            "../images/muses-meta/comedy.jpg"

        EpicPoetry ->
            "../images/muses-meta/epic-poetry.jpg"

        DivinePoetry ->
            "../images/muses-meta/divine-poetry.jpg"


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
    , hubris
    , hubris
    , agamemnon
    , antigone
    , siren
    , siren
    , siren
    , trilogy
    , trilogy
    , katharsis
    , katharsis
    ]


comedyDeck : List Card
comedyDeck =
    [ aristophanes
    , amphitheater
    , amphitheater
    , satyrPlay
    , satyrPlay
    , satyrPlay
    , appleOfDiscord
    , fates
    , laughter
    , laughter
    , newComedy
    ]


epicPoetryDeck : List Card
epicPoetryDeck =
    [ atalanta
    , theseus
    , homer
    , arctinus
    , heros
    , heros
    , heros
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
    , lightning
    , lightning
    , sacrifice
    , sacrifice
    ]



-- INDIVIDUAL CARDS


fates : Card
fates =
    Character
        { name = "Fates"
        , image = "fates.jpg"
        , flavorText = "Past, present, and future"
        }
        (MuseumPoints 2)
        [ Female ]


sophocles : Card
sophocles =
    Character
        { name = "Sophocles"
        , image = "sophocles.jpg"
        , flavorText = "Master of tragedy, by tragedy mastered"
        }
        (MuseumPoints 1)
        [ GreatPoet ]


erinyes : Card
erinyes =
    Character
        { name = "Erinyes"
        , image = "erinyes.jpg"
        , flavorText = "Revenge should have no bounds"
        }
        (MuseumPoints 2)
        []


agamemnon : Card
agamemnon =
    Character
        { name = "Agammemnon"
        , image = "agamemnon.jpg"
        , flavorText = "Leader of the Trojan war, murdered by his wife"
        }
        (MuseumPoints 3)
        [ Ruler ]


antigone : Card
antigone =
    Character
        { name = "Antigone"
        , image = "antigone.jpg"
        , flavorText = "Oedipus was her father"
        }
        (MuseumPoints 2)
        [ Female ]


siren : Card
siren =
    Character
        { name = "Siren"
        , image = "sirens.jpg"
        , flavorText = "Going towards death was never so sublime"
        }
        (MuseumPoints 1)
        [ Female ]


aristophanes : Card
aristophanes =
    Character
        { name = "Aristophanes"
        , image = "aristophanes.jpg"
        , flavorText = "Making poop jokes in the 5th century BCE"
        }
        (MuseumPoints 1)
        [ GreatPoet ]


amphitheater : Card
amphitheater =
    Character
        { name = "Amphitheater"
        , image = "amphitheater.jpg"
        , flavorText = "This is where things are happening"
        }
        (MuseumPoints 1)
        []


trilogy : Card
trilogy =
    OneShot
        { name = "Trilogy"
        , image = "trilogy.jpg"
        , flavorText = "Improves our sales numbers"
        }
        (GeneratePoints <| MuseumPoints 6)


sacrifice : Card
sacrifice =
    OneShot
        { name = "Sacrifice"
        , image = "sacrifice.jpg"
        , flavorText = "A gift to the gods"
        }
        (GeneratePoints <| MuseumPoints 6)


katharsis : Card
katharsis =
    OneShot
        { name = "Katharsis"
        , image = "katharsis.jpg"
        , flavorText = "κάθαρσις - a purification"
        }
        (GeneratePoints <| MuseumPoints 8)


newComedy : Card
newComedy =
    OneShot
        { name = "New Comedy"
        , image = "new-comedy.jpg"
        , flavorText = "Finding humor in every day life"
        }
        (GeneratePoints <| MuseumPoints 12)


lightning : Card
lightning =
    OneShot
        { name = "Lightning"
        , image = "lightning.jpg"
        , flavorText = "Zeus wields these mighty bolts"
        }
        (GeneratePoints <| MuseumPoints 6)


laughter : Card
laughter =
    OneShot
        { name = "Laughter"
        , image = "laughter.jpg"
        , flavorText = "Every comic author's dream"
        }
        (GeneratePoints <| MuseumPoints 8)


hubris : Card
hubris =
    OneShot
        { name = "Hubris"
        , image = "hubris.jpg"
        , flavorText = "ὕβρις - even the gods can't match this!"
        }
        (GeneratePoints <| MuseumPoints 8)


heros : Card
heros =
    OneShot
        { name = "Heros"
        , image = "heros.jpg"
        , flavorText = "ἥρως - veneration of a person's great deeds"
        }
        (GeneratePoints <| MuseumPoints 6)


satyrPlay : Card
satyrPlay =
    OneShot
        { name = "Satyr Play"
        , image = "satyr.jpg"
        , flavorText = "Need something funny after watching a tragedy"
        }
        (GeneratePoints <| MuseumPoints 10)


perseus : Card
perseus =
    Character
        { name = "Perseus"
        , image = "perseus.jpg"
        , flavorText = "Don't look!"
        }
        (MuseumPoints 2)
        []


atalanta : Card
atalanta =
    Character
        { name = "Atalanta"
        , image = "atalanta.jpg"
        , flavorText = "Do not attempt to take her on in a footrace"
        }
        (MuseumPoints 2)
        [ Female, Seafarer ]


jason : Card
jason =
    Character
        { name = "Jason"
        , image = "jason.jpg"
        , flavorText = "JSON"
        }
        (MuseumPoints 3)
        [ Seafarer ]


theseus : Card
theseus =
    Character
        { name = "Theseus"
        , image = "theseus.jpg"
        , flavorText = "Killer of the Minotaur"
        }
        (MuseumPoints 2)
        [ Seafarer, Underworld ]


homer : Card
homer =
    Character
        { name = "Homer"
        , image = "homer.jpg"
        , flavorText = "The OG poet"
        }
        (MuseumPoints 1)
        [ GreatPoet ]


arctinus : Card
arctinus =
    Character
        { name = "Arctinus"
        , image = "arctinus.jpg"
        , flavorText = "Homer should have written more sequels"
        }
        (MuseumPoints 1)
        [ GreatPoet ]


hesiod : Card
hesiod =
    Character
        { name = "Hesiod"
        , image = "hesiod.jpg"
        , flavorText = "The gods need to be organized too"
        }
        (MuseumPoints 1)
        [ GreatPoet ]


prometheus : Card
prometheus =
    Character
        { name = "Prometheus"
        , image = "prometheus.jpg"
        , flavorText = "🔥🔥🔥"
        }
        (MuseumPoints 2)
        []


blessedVintage : Card
blessedVintage =
    OneShot
        { name = "Blessed Vintage"
        , image = "blessed-vintage.jpg"
        , flavorText = "The harvest is amazing this year!"
        }
        (GeneratePoints <| MuseumPoints 10)


appleOfDiscord : Card
appleOfDiscord =
    OneShot
        { name = "Apple of Discord"
        , image = "apple-discord.jpg"
        , flavorText = "Who is the fairest one of all?"
        }
        (GeneratePoints <| MuseumPoints 7)


artemis : Card
artemis =
    Character
        { name = "Artemis"
        , image = "artemis.jpg"
        , flavorText = "Goddess of the hunt"
        }
        (MuseumPoints 3)
        [ Female ]


poseidon : Card
poseidon =
    Character
        { name = "Poseidon"
        , image = "poseidon.jpg"
        , flavorText = "Lord of the sea"
        }
        (MuseumPoints 4)
        [ Seafarer ]


apollo : Card
apollo =
    Character
        { name = "Apollo"
        , image = "apollo.jpg"
        , flavorText = "God of too many things"
        }
        (MuseumPoints 5)
        []


hymn : Card
hymn =
    OneShot
        { name = "Hymn"
        , image = "hymn.jpg"
        , flavorText = "Sacred music to the gods"
        }
        (GeneratePoints <| MuseumPoints 10)
