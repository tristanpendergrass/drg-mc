module Config exposing (..)

import Dict exposing (Dict)
import Duration exposing (Duration)
import FeatherIcons
import Quantity exposing (Quantity(..))
import Types exposing (..)
import Utils.Record


type Env
    = Dev
    | Prod


env : Env
env =
    -- Prod
    Dev


isProd : Bool
isProd =
    env == Prod


isDev : Bool
isDev =
    env == Dev


levelUnlockStats : Dict Int (List LevelUnlockStats)
levelUnlockStats =
    Dict.fromList
        [ ( 2, [ { kind = UnlockHaz2, title = "Haz 2", category = UnlockActivity } ] )
        , ( 3, [ { kind = UnlockHaz3, title = "Haz 3", category = UnlockActivity } ] )
        , ( 4, [ { kind = UnlockTheme Retro, title = "Retro Theme", category = UnlockCosmetic } ] )
        , ( 5
          , [ { kind = UnlockHaz4, title = "Haz 4", category = UnlockActivity }
            , { kind = UnlockDwarfXpButtons, title = "Dwarf Xp Buttons", category = UnlockFeature }
            ]
          )
        , ( 6
          , [ { kind = UnlockTheme Black, title = "Theme: Black", category = UnlockCosmetic }
            , { kind = UnlockDwarfXpButton2, title = "Dwarf Xp Button 2", category = UnlockActivity }
            ]
          )
        , ( 7, [ { kind = UnlockHaz5, title = "Haz 5", category = UnlockActivity } ] )
        , ( 8, [ { kind = UnlockTheme Luxury, title = "Theme: Luxury", category = UnlockCosmetic } ] )
        , ( 10, [ { kind = UnlockDwarfXpButton3, title = "Dwarf Xp Button 3", category = UnlockActivity } ] )
        , ( 11, [ { kind = UnlockDwarfXpButton4, title = "Dwarf Xp Button 4", category = UnlockActivity } ] )
        , ( 12, [ { kind = UnlockDwarfXpButton5, title = "Dwarf Xp Button 5", category = UnlockActivity } ] )
        , ( 20, [ { kind = UnlockTheme Cyberpunk, title = "Theme: Cyberpunk", category = UnlockCosmetic } ] )
        ]


levelingSchedule : Int -> LevelRequirements
levelingSchedule level =
    if level < 6 then
        EarnCredits (toFloat level * 5.0)

    else if level < 12 then
        EarnCredits (toFloat level * 10.0)

    else if level < 14 then
        EarnCredits (toFloat level * 15.0)

    else if level < 20 then
        EarnCredits (toFloat level * 20.0)

    else
        AtMaxLevel


{-| This is the xp needed to reach the next level after the given level.
Thus, it has a meaningful result for levels 1 through ([max level] - 1).
-}
dwarfLevelingSchedule : Int -> Result () DwarfLevelRequirements
dwarfLevelingSchedule level =
    if level < 1 then
        Err ()

    else if level < 2 then
        Ok (EarnDwarfXp 1)

    else if level < 3 then
        Ok (EarnDwarfXp 2)

    else if level < 4 then
        Ok (EarnDwarfXp 3)

    else if level < 11 then
        Ok (EarnDwarfXp 5)

    else if level < 20 then
        Ok (EarnDwarfXp 8)

    else if level < 25 then
        Ok (EarnDwarfXp 12)

    else if level == 25 then
        Ok AtMaxDwarfLevel

    else
        Err ()


missionStats : MissionRecord MissionStats
missionStats =
    { haz1 =
        { title = "Haz 1"
        , unlock = Nothing
        , duration = Duration.minutes 1
        , yield = { credits = 1.25, resources = Utils.Record.resourceRecord 0 }
        }
    , haz2 =
        { title = "Haz 2"
        , unlock = Just UnlockHaz2
        , duration = Duration.minutes 5
        , yield = { credits = 2.5, resources = Utils.Record.resourceRecord 0 }
        }
    , haz3 =
        { title = "Haz 3"
        , unlock = Just UnlockHaz3
        , duration = Duration.minutes 30
        , yield = { credits = 4.0, resources = Utils.Record.resourceRecord 0 }
        }
    , haz4 =
        { title = "Haz 4"
        , unlock = Just UnlockHaz4
        , duration = Duration.hours 3
        , yield = { credits = 7.75, resources = Utils.Record.resourceRecord 0 }
        }
    , haz5 =
        { title = "Haz 5"
        , unlock = Just UnlockHaz5
        , duration = Duration.hours 20
        , yield = { credits = 12, resources = Utils.Record.resourceRecord 0 }
        }
    }


dwarfStats : DwarfRecord DwarfStats
dwarfStats =
    { scout = { name = "Scout", imgSrc = "scout.webp" }
    , gunner = { name = "Gunner", imgSrc = "gunner.webp" }
    , engineer = { name = "Engineer", imgSrc = "engineer.webp" }
    , driller = { name = "Driller", imgSrc = "driller.webp" }
    }


dwarfXpButtonStats : DwarfXpButtonRecord DwarfXpButtonStats
dwarfXpButtonStats =
    { dwarfXpButton1 = { id_ = "dwarfXpButton1", xp = Quantity 1, unlock = Nothing, duration = Duration.minutes 1 }
    , dwarfXpButton2 = { id_ = "dwarfXpButton2", xp = Quantity 2, unlock = Just UnlockDwarfXpButton2, duration = Duration.minutes 5 }
    , dwarfXpButton3 = { id_ = "dwarfXpButton3", xp = Quantity 3, unlock = Just UnlockDwarfXpButton3, duration = Duration.hours 1 }
    , dwarfXpButton4 = { id_ = "dwarfXpButton4", xp = Quantity 4, unlock = Just UnlockDwarfXpButton4, duration = Duration.hours 6 }
    , dwarfXpButton5 = { id_ = "dwarfXpButton5", xp = Quantity 5, unlock = Just UnlockDwarfXpButton5, duration = Duration.hours 24 }
    }



-- Tabs


tabStats : TabRecord TabStats
tabStats =
    { missionsTab =
        { title = "Missions"
        , icon = FeatherIcons.target
        }
    , commendationsTab =
        { title = "Dwarf Leveling"
        , icon = FeatherIcons.award
        }
    , settingsTab =
        { title = "Settings"
        , icon = FeatherIcons.settings
        }
    }


defaultDebugSettings : DebugSettings
defaultDebugSettings =
    { gameSpeed = 1
    , addedTime = Quantity.zero
    , buttonCooldownInstant = False
    }
