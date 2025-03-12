module Utils.Unlocks exposing (..)

import Config
import Dict exposing (Dict)
import Types exposing (..)
import Utils.Record


missionIsUnlocked : Int -> Mission -> Bool
missionIsUnlocked currentLevel mission =
    case (Utils.Record.getByMission mission Config.missionStats).unlock of
        Nothing ->
            True

        Just missionUnlock ->
            kindIsUnlocked currentLevel missionUnlock


abyssBarFeatureUnlocked : Int -> Bool
abyssBarFeatureUnlocked level =
    kindIsUnlocked level UnlockAbyssBar


nextUnlock : Int -> Dict Int (List LevelUnlockStats) -> Maybe ( Int, LevelUnlockStats )
nextUnlock currentLevel stats =
    stats
        |> Dict.keys
        |> List.filter (\level -> level > currentLevel)
        |> List.sort
        |> List.head
        |> Maybe.andThen
            (\nextLevel ->
                Dict.get nextLevel stats
                    |> Maybe.andThen List.head
                    |> Maybe.map (\unlockStats -> ( nextLevel, unlockStats ))
            )


themeIsUnlocked : Int -> Theme -> Bool
themeIsUnlocked currentLevel theme =
    kindIsUnlocked currentLevel (UnlockTheme theme)


kindIsUnlocked : Int -> Unlock -> Bool
kindIsUnlocked currentLevel kind =
    Config.levelUnlockStats
        |> Dict.toList
        |> List.any
            (\( unlockLevel, unlockStatsList ) ->
                List.any (.kind >> (==) kind) unlockStatsList && unlockLevel <= currentLevel
            )


dwarfXpButtonIsUnlocked : Int -> DwarfXpButton -> Bool
dwarfXpButtonIsUnlocked currentLevel dwarfXpButton =
    case (dwarfXpButtonStats dwarfXpButton).unlock of
        Nothing ->
            True

        Just unlockCondition ->
            kindIsUnlocked currentLevel unlockCondition


dwarfXpButtonsFeatureUnlocked : Int -> Bool
dwarfXpButtonsFeatureUnlocked level =
    kindIsUnlocked level UnlockDwarfXpButtons
