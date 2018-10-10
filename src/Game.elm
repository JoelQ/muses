module Game exposing
    ( Game(..)
    , GameState
    , andThen
    , checkWin
    , map
    , playCard
    , playCurrentCharacters
    , removeFromHand
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
playCard ( id, card ) state =
    case card of
        Card.OneShot (Card.GeneratePoints (MuseumPoints n)) ->
            modifyCurrentPlayer (Player.increaseScore n) state

        Card.Character _ ->
            modifyCurrentPlayer (Player.playCharacter id card) state


startGame : Card.Deck -> List Card.WithId -> List Card.WithId -> GameState
startGame selectedDeck p1Cards p2Cards =
    { currentPlayer = Player.buildInitial "Player 1" selectedDeck p1Cards
    , otherPlayer = Player.buildInitial "Player 2" (Card.otherDeck selectedDeck) p2Cards
    }


shuffleAndStart : Card.Deck -> Random.Generator GameState
shuffleAndStart deck =
    case deck of
        Card.Flashy ->
            Random.map2 (startGame deck)
                (Card.shuffle Card.flashyDeck)
                (Card.shuffle Card.slowAndSteadyDeck)

        Card.SlowAndSteady ->
            Random.map2 (startGame deck)
                (Card.shuffle Card.slowAndSteadyDeck)
                (Card.shuffle Card.flashyDeck)


removeFromHand : Int -> GameState -> GameState
removeFromHand cardId state =
    modifyCurrentPlayer (Player.removeFromHand cardId) state


playCurrentCharacters : GameState -> GameState
playCurrentCharacters =
    modifyCurrentPlayer Player.addPointsFromCharacters


swapPlayers : GameState -> GameState
swapPlayers { currentPlayer, otherPlayer } =
    { currentPlayer = otherPlayer, otherPlayer = currentPlayer }


modifyCurrentPlayer : (Player -> Player) -> GameState -> GameState
modifyCurrentPlayer playerFunction state =
    { state | currentPlayer = playerFunction state.currentPlayer }