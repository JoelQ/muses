module Player exposing
    ( Player
    , addPointsFromCharacters
    , buildInitial
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
    , cards : Dict Int Card
    , score : Int
    , characters : Dict Int Card
    }



-- BUILD


buildInitial : String -> Card.Deck -> List Card.WithId -> Player
buildInitial name deck cards =
    Player name deck (Dict.fromList cards) 0 Dict.empty


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
    { player | cards = Dict.remove cardId player.cards }
