module Player exposing
    ( Player
    , addPointsFromCharacters
    , buildInitial
    , increaseScore
    , playCharacter
    , removeFromHand
    )

import Card exposing (Card)
import Dict exposing (Dict)
import MuseumPoints


type alias Player =
    { name : String
    , deck : Card.Deck
    , cards : Dict Int Card
    , score : Int
    , characters : Dict Int Card
    }


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


buildInitial : String -> Card.Deck -> List Card.WithId -> Player
buildInitial name deck cards =
    Player name deck (Dict.fromList cards) 0 Dict.empty


removeFromHand : Int -> Player -> Player
removeFromHand cardId player =
    { player | cards = Dict.remove cardId player.cards }
