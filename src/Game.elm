module Game exposing
    ( Game(..)
    , GameState
    , andThen
    , checkWin
    , drawCard
    , map
    , playCard
    , playCurrentCharacters
    , removeFromHand
    , resetPlayedCardCount
    , shuffleAndStart
    , swapPlayers
    )

import Card
import MuseumPoints exposing (MuseumPoints(..))
import Player exposing (Player)
import Random


type Game
    = Choosing
    | Playing GameState
    | Complete


checkWin : GameState -> Game
checkWin ({ currentPlayer, otherPlayer } as state) =
    if currentPlayer.score >= 100 || otherPlayer.score >= 100 then
        Complete

    else
        Playing state


andThen : (GameState -> Game) -> Game -> Game
andThen function game =
    case game of
        Playing state ->
            function state

        _ ->
            game


map : (GameState -> GameState) -> Game -> Game
map function =
    andThen (Playing << function)



-- GAME STATE


type alias GameState =
    { currentPlayer : Player
    , otherPlayer : Player
    }


playCard : Card.WithId -> GameState -> GameState
playCard ( cardId, card ) state =
    if state.currentPlayer.cardsPlayed == 0 then
        state
            |> executeCardEffects ( cardId, card )
            |> removeFromHand cardId

    else
        state


executeCardEffects : Card.WithId -> GameState -> GameState
executeCardEffects ( id, card ) state =
    case card of
        Card.OneShot _ (Card.GeneratePoints (MuseumPoints n)) ->
            modifyCurrentPlayer (Player.increaseScore n) state

        Card.Character _ _ ->
            modifyCurrentPlayer (Player.playCharacter id card) state


shuffleAndStart : Card.Deck -> Random.Generator GameState
shuffleAndStart myDeck =
    Random.map2 GameState
        (Player.randomPlayer "Player 1" myDeck)
        (Player.randomOpponent "Player 2" myDeck)


removeFromHand : Int -> GameState -> GameState
removeFromHand cardId state =
    modifyCurrentPlayer (Player.removeFromHand cardId) state


playCurrentCharacters : GameState -> GameState
playCurrentCharacters =
    modifyCurrentPlayer Player.addPointsFromCharacters


swapPlayers : GameState -> GameState
swapPlayers { currentPlayer, otherPlayer } =
    { currentPlayer = otherPlayer, otherPlayer = currentPlayer }


drawCard : GameState -> GameState
drawCard =
    modifyCurrentPlayer Player.drawCard


resetPlayedCardCount : GameState -> GameState
resetPlayedCardCount =
    modifyCurrentPlayer Player.resetPlayedCardCount


modifyCurrentPlayer : (Player -> Player) -> GameState -> GameState
modifyCurrentPlayer playerFunction state =
    { state | currentPlayer = playerFunction state.currentPlayer }
