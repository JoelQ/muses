module Player exposing
    ( Player
    , addPointsFromCharacters
    , buildInitial
    , drawCard
    , increaseScore
    , playCharacter
    , randomOpponent
    , randomPlayer
    , removeFromHand
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


playCharacter : Int -> Card -> Player -> Player
playCharacter id card player =
    { player | characters = Dict.insert id card player.characters }


removeFromHand : Int -> Player -> Player
removeFromHand cardId player =
    { player | hand = Dict.remove cardId player.hand }


drawCard : Player -> Player
drawCard player =
    case player.cardPile of
        [] ->
            player

        ( id, card ) :: rest ->
            { player | cardPile = rest, hand = Dict.insert id card player.hand }
