module MuseumPoints exposing (MuseumPoints(..), toInt)


type MuseumPoints
    = MuseumPoints Int


toInt : MuseumPoints -> Int
toInt (MuseumPoints n) =
    n
