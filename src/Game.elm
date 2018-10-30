module Game exposing
    ( Game(..)
    , GameSlot
    , GameState
    , andThen
    , checkWin
    , drawCard
    , map
    , playCard
    , playCardToSlot
    , playCurrentCharacters
    , removeFromHand
    , resetPlayedCardCount
    , shuffleAndStart
    , slotName
    , swapPlayers
    )

import Card exposing (Card)
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


type alias GameSlot =
    { requirements : Card.Trait, card : Maybe Card }


slotName : GameSlot -> String
slotName { requirements } =
    Card.traitName requirements


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
                modifyCurrentPlayer (Player.selectCard cardId) state

    else
        state


addCardToSlot : Card -> GameSlot -> List GameSlot -> List GameSlot
addCardToSlot card slot =
    List.map
        (\s ->
            if s.requirements == slot.requirements then
                { s | card = Just card }

            else
                s
        )


playCardToSlot : Card.WithId -> GameSlot -> GameState -> GameState
playCardToSlot ( cardId, card ) slot state =
    { state | slots = addCardToSlot card slot state.slots }
        |> modifyCurrentPlayer (Player.playCharacter cardId)


executeCardEffects : Card.WithId -> GameState -> GameState
executeCardEffects ( id, card ) state =
    case card of
        Card.OneShot _ (Card.GeneratePoints (MuseumPoints n)) ->
            modifyCurrentPlayer (Player.increaseScore n) state

        Card.Character _ _ _ ->
            state


shuffleAndStart : Card.Deck -> Random.Generator GameState
shuffleAndStart myDeck =
    Random.map3 GameState
        (Player.randomPlayer "Player 1" myDeck)
        (Player.randomOpponent "Player 2" myDeck)
        (Random.constant [ GameSlot Card.Female Nothing, GameSlot Card.Small Nothing ])


removeFromHand : Int -> GameState -> GameState
removeFromHand cardId state =
    modifyCurrentPlayer (Player.removeFromHand cardId) state


playCurrentCharacters : GameState -> GameState
playCurrentCharacters =
    modifyCurrentPlayer Player.addPointsFromCharacters


swapPlayers : GameState -> GameState
swapPlayers state =
    { state
        | currentPlayer = state.otherPlayer
        , otherPlayer = state.currentPlayer
    }


drawCard : GameState -> GameState
drawCard =
    modifyCurrentPlayer Player.drawCard


resetPlayedCardCount : GameState -> GameState
resetPlayedCardCount =
    modifyCurrentPlayer Player.resetPlayedCardCount


modifyCurrentPlayer : (Player -> Player) -> GameState -> GameState
modifyCurrentPlayer playerFunction state =
    { state | currentPlayer = playerFunction state.currentPlayer }
