module Game exposing
    ( Game(..)
    , GameState
    , andThen
    , checkWin
    , drawCard
    , map
    , playCard
    , playCardToSlot
    , playCurrentCharacters
    , removeFromHand
    , resetCurrentPlayer
    , selectCard
    , shuffleAndStart
    , swapPlayers
    )

import Card exposing (Card)
import GameSlot exposing (GameSlot(..))
import MuseumPoints exposing (MuseumPoints(..))
import Player exposing (Player)
import Random


type Game
    = Choosing
    | Playing GameState
    | Complete Player


checkWin : GameState -> Game
checkWin ({ currentPlayer, otherPlayer } as state) =
    if currentPlayer.score >= 100 then
        Complete currentPlayer

    else if otherPlayer.score >= 100 then
        Complete otherPlayer

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
    , slots : List GameSlot
    }


playCard : Card.WithId -> GameState -> GameState
playCard ( cardId, card ) state =
    if state.currentPlayer.cardsPlayed == 0 then
        case card of
            Card.OneShot _ _ ->
                state
                    |> executeCardEffects ( cardId, card )
                    |> removeFromHand cardId

            Card.Character _ _ _ ->
                state

    else
        state


selectCard : Int -> GameState -> GameState
selectCard cardId state =
    if state.currentPlayer.cardsPlayed == 0 then
        modifyCurrentPlayer (Player.selectCard cardId) state

    else
        state


addCardToSlot : String -> Card -> GameSlot -> List GameSlot -> List GameSlot
addCardToSlot owner card targetSlot =
    List.map
        (\currentSlot ->
            if targetSlot == currentSlot then
                GameSlot.fillWith owner card currentSlot

            else
                currentSlot
        )


playCardToSlot : Card.WithId -> GameSlot -> GameState -> GameState
playCardToSlot ( cardId, card ) slot state =
    { state | slots = addCardToSlot state.currentPlayer.name card slot state.slots }
        |> modifyCurrentPlayer (Player.playCharacter cardId)


executeCardEffects : Card.WithId -> GameState -> GameState
executeCardEffects ( id, card ) state =
    case card of
        Card.OneShot _ (Card.GeneratePoints (MuseumPoints n)) ->
            modifyCurrentPlayer (Player.playOneShot n) state

        Card.Character _ _ _ ->
            state


shuffleAndStart : Card.Deck -> Random.Generator GameState
shuffleAndStart myDeck =
    Random.map3 GameState
        (Player.randomPlayer "Player 1" myDeck)
        (Player.randomOpponent "Player 2" myDeck)
        GameSlot.initial


removeFromHand : Int -> GameState -> GameState
removeFromHand cardId state =
    modifyCurrentPlayer (Player.removeFromHand cardId) state


playCurrentCharacters : GameState -> GameState
playCurrentCharacters ({ currentPlayer, slots } as state) =
    modifyCurrentPlayer
        (Player.addPointsFromCharacters <|
            GameSlot.cardsOwnedBy currentPlayer.name slots
        )
        state


swapPlayers : GameState -> GameState
swapPlayers state =
    { state
        | currentPlayer = state.otherPlayer
        , otherPlayer = state.currentPlayer
    }


drawCard : GameState -> GameState
drawCard =
    modifyCurrentPlayer Player.drawCard


resetCurrentPlayer : GameState -> GameState
resetCurrentPlayer =
    modifyCurrentPlayer Player.endOfTurnReset


modifyCurrentPlayer : (Player -> Player) -> GameState -> GameState
modifyCurrentPlayer playerFunction state =
    { state | currentPlayer = playerFunction state.currentPlayer }
