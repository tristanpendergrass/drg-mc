module DwarfXp exposing (..)

import Config
import List.Extra
import Quantity exposing (Quantity(..))
import Types exposing (..)
import Utils.Percent exposing (Percent)


float : Float -> DwarfXp
float =
    Quantity


toFloat : DwarfXp -> Float
toFloat (Quantity xp) =
    xp


toString : DwarfXp -> String
toString (Quantity xp) =
    String.fromInt (floor xp)


{-| A list of for each level how much xp is required to reach the next level.
We should normally use Config.dwarfLevelingSchedule if you need the xp for a given level, but having it in order in a List helps with a lot of other utility functions in this file.
-}
xpTable : List ( Int, DwarfXp )
xpTable =
    let
        helper : Int -> List ( Int, DwarfXp ) -> List ( Int, DwarfXp )
        helper lvl accum =
            case Config.dwarfLevelingSchedule lvl of
                Ok (EarnDwarfXp xpToNextLevel) ->
                    helper (lvl + 1) (accum ++ [ ( lvl, float xpToNextLevel ) ])

                _ ->
                    accum
    in
    helper 1 []


maxLevel : Int
maxLevel =
    xpTable
        |> List.Extra.last
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 1


level : DwarfXp -> Int
level xp =
    let
        helper : Int -> DwarfXp -> List ( Int, DwarfXp ) -> Int
        helper lvl (Quantity remainingXp) remainingLevels =
            case remainingLevels of
                [] ->
                    lvl

                ( _, Quantity xpToNextLevel ) :: rest ->
                    if remainingXp < xpToNextLevel then
                        lvl

                    else
                        helper (lvl + 1) (float (remainingXp - xpToNextLevel)) rest
    in
    helper 1 xp xpTable


xpToLevel : Int -> DwarfXp
xpToLevel lvl =
    List.Extra.dropWhileRight (\( l, _ ) -> l >= lvl) xpTable
        |> List.foldl (\( _, Quantity xp ) acc -> Quantity.plus acc (float xp)) (float 0)


flatXpInCurrentLevel : DwarfXp -> DwarfXp
flatXpInCurrentLevel xp =
    let
        currentLevel : Int
        currentLevel =
            level xp

        xpInCurrentLevel : DwarfXp
        xpInCurrentLevel =
            Quantity.difference xp (xpToLevel currentLevel)
    in
    xpInCurrentLevel


percentInLevel : DwarfXp -> Percent
percentInLevel xp =
    let
        currentLevel : Int
        currentLevel =
            level xp

        xpInCurrentLevel : DwarfXp
        xpInCurrentLevel =
            flatXpInCurrentLevel xp
    in
    case Config.dwarfLevelingSchedule currentLevel of
        Ok (EarnDwarfXp total) ->
            Utils.Percent.float (toFloat xpInCurrentLevel / total)

        _ ->
            Utils.Percent.float 1
