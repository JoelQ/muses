module GameSlot exposing
    ( GameSlot(..)
    , cardsOwnedBy
    , fillWith
    , name
    )

import Card exposing (Card)


type GameSlot
    = Open Card.Trait
    | Filled Card.Trait String Card


cardsOwnedBy : String -> List GameSlot -> List Card
cardsOwnedBy owner slots =
    List.filterMap (cardOwnedBy owner) slots


cardOwnedBy : String -> GameSlot -> Maybe Card
cardOwnedBy owner slot =
    case slot of
        Open _ ->
            Nothing

        Filled _ slotOwner card ->
            if owner == slotOwner then
                Just card

            else
                Nothing


fillWith : String -> Card -> GameSlot -> GameSlot
fillWith owner card slot =
    case slot of
        Open reqs ->
            Filled reqs owner card

        Filled reqs _ _ ->
            Filled reqs owner card


name : GameSlot -> String
name =
    Card.traitName << requirements


requirements : GameSlot -> Card.Trait
requirements slot =
    case slot of
        Open reqs ->
            reqs

        Filled reqs _ _ ->
            reqs
