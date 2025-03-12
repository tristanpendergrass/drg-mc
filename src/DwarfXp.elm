module DwarfXp exposing (..)

import Quantity exposing (Quantity(..))
import Utils.Percent exposing (Percent)



-- Dwarf XP Buttons


type DwarfXpPoint
    = DwarfXpPoint


type alias DwarfXp =
    Quantity Float DwarfXpPoint


float : Float -> DwarfXp
float =
    Quantity


toFloat : DwarfXp -> Float
toFloat (Quantity xp) =
    xp


toString : DwarfXp -> String
toString (Quantity xp) =
    String.fromInt (floor xp)
