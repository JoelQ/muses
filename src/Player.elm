module Player exposing
    ( Player
    , addPointsFromCharacters
    , buildInitial
    , drawCard
    , increaseScore
    , playCharacter
    , playOneShot
    , randomOpponent
    , randomPlayer
    , removeFromHand
    , resetPlayedCardCount
    , selectCard
    , selectedCard
    )

import Card exposing (Card)
import Dict exposing (Dict)
import MuseumPoints
import Random


type alias Player =
    { name : String
    , deck : Card.Deck
    , cardPile : List Card.WithId
    , hand : Dict Int Card
    , score : Int
    , characters : Dict Int Card
    , cardsPlayed : Int
    , selected : Maybe Int
    }



-- BUILD


cardsDealt : Int
cardsDealt =
    3


buildInitial : String -> Card.Deck -> List Card.WithId -> Player
buildInitial name deck cards =
    { name = name
    , deck = deck
    , cardPile = List.drop cardsDealt cards
    , hand = Dict.fromList (List.take cardsDealt cards)
    , score = 0
    , characters = Dict.empty
    , cardsPlayed = 0
    , selected = Nothing
    }


randomPlayer : String -> Card.Deck -> Random.Generator Player
randomPlayer name deck =
    Random.map (buildInitial name deck) (Card.shuffle <| Card.cardsForDeck deck)


randomOpponent : String -> Card.Deck -> Random.Generator Player
randomOpponent name otherDeck =
    Card.opponentDeck otherDeck
        |> Random.andThen (\deck -> randomPlayer name deck)


scoreFromCharacters : Player -> Int
scoreFromCharacters { characters } =
    Dict.values characters
        |> List.map Card.magnitude
        |> List.map MuseumPoints.toInt
        |> List.sum


addPointsFromCharacters : Player -> Player
addPointsFromCharacters player =
    increaseScore (scoreFromCharacters player) player


increaseScore : Int -> Player -> Player
increaseScore n player =
    { player | score = player.score + n }


playOneShot : Int -> Player -> Player
playOneShot n player =
    { player
        | score = player.score + n
        , cardsPlayed = player.cardsPlayed + 1
    }


playCharacter : Int -> Player -> Player
playCharacter cardId player =
    { player
        | hand = Dict.remove cardId player.hand
        , cardsPlayed = player.cardsPlayed + 1
        , selected = Nothing
    }


removeFromHand : Int -> Player -> Player
removeFromHand cardId player =
    { player | hand = Dict.remove cardId player.hand }


selectCard : Int -> Player -> Player
selectCard id player =
    { player | selected = Just id }


resetPlayedCardCount : Player -> Player
resetPlayedCardCount player =
    { player | cardsPlayed = 0 }


drawCard : Player -> Player
drawCard player =
    case player.cardPile of
        [] ->
            player

        ( id, card ) :: rest ->
            { player | cardPile = rest, hand = Dict.insert id card player.hand }


selectedCard : Player -> Maybe Card.WithId
selectedCard player =
    player.selected
        |> Maybe.andThen
            (\id ->
                Dict.get id player.hand
                    |> Maybe.andThen (\card -> Just ( id, card ))
            )
