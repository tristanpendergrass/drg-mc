module Config exposing (..)

import AssocList as Dict exposing (Dict)
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
        , ( 15, [ { kind = UnlockAbyssBar, title = "Abyss Bar", category = UnlockFeature } ] )
        , ( 20, [ { kind = UnlockBiomeTier BiomeUnlockTier1, title = "Biomes", category = UnlockFeature } ] )
        , ( 22, [ { kind = UnlockBiomeTier BiomeUnlockTier2, title = "Biomes Tier II", category = UnlockBiomes } ] )
        , ( 24, [ { kind = UnlockBiomeTier BiomeUnlockTier3, title = "Biomes Tier III", category = UnlockBiomes } ] )
        , ( 25, [ { kind = UnlockTheme Cyberpunk, title = "Theme: Cyberpunk", category = UnlockCosmetic } ] )
        ]


levelingSchedule : Int -> LevelRequirements
levelingSchedule level =
    if level < 6 then
        GainMorkite (toFloat level * 5.0)

    else if level < 12 then
        GainMorkite (toFloat level * 10.0)

    else if level < 14 then
        GainMorkite (toFloat level * 15.0)

    else if level < 20 then
        GainMorkite (toFloat level * 20.0)

    else if level < 25 then
        GainMorkite (toFloat level * 40.0)

    else
        AtMaxLevel


{-| This is the xp needed to reach the next level after the given level.
Thus, it has a meaningful result for levels 1 through ([max level] - 1).
-}
dwarfLevelingSchedule : Int -> Result () DwarfLevelRequirements
dwarfLevelingSchedule level =
    if level < 1 then
        Err ()

    else if level < 5 then
        Ok (EarnDwarfXp 5)

    else if level < 11 then
        Ok (EarnDwarfXp 10)

    else if level < 20 then
        Ok (EarnDwarfXp 15)

    else if level < 25 then
        Ok (EarnDwarfXp 25)

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
        , morkite = 1.5
        }
    , haz2 =
        { title = "Haz 2"
        , unlock = Just UnlockHaz2
        , duration = Duration.minutes 5
        , morkite = 2.5
        }
    , haz3 =
        { title = "Haz 3"
        , unlock = Just UnlockHaz3
        , duration = Duration.minutes 30
        , morkite = 4.0
        }
    , haz4 =
        { title = "Haz 4"
        , unlock = Just UnlockHaz4
        , duration = Duration.hours 3
        , morkite = 7.75
        }
    , haz5 =
        { title = "Haz 5"
        , unlock = Just UnlockHaz5
        , duration = Duration.hours 20
        , morkite = 12
        }
    }


dwarfStats : DwarfRecord DwarfStats
dwarfStats =
    { scout = { name = "Scout", imgSrc = "scout.webp" }
    , gunner = { name = "Gunner", imgSrc = "gunner.webp" }
    , engineer = { name = "Engineer", imgSrc = "engineer.webp" }
    , driller = { name = "Driller", imgSrc = "driller.webp" }
    }



-- Tabs


tabStats : TabRecord TabStats
tabStats =
    { missionsTab =
        { title = "Missions"
        , maybeIcon = Nothing
        }
    , commendationsTab =
        { title = "Dwarf Leveling"
        , maybeIcon = Nothing
        }
    , projectsTab =
        { title = "Projects"
        , maybeIcon = Nothing
        }
    , abyssBarTab =
        { title = "Abyss Bar"
        , maybeIcon = Nothing
        }
    , settingsTab =
        { title = "Settings"
        , maybeIcon = Just FeatherIcons.settings
        }
    }


defaultDebugSettings : DebugSettings
defaultDebugSettings =
    { gameSpeed = 1
    , addedTime = Quantity.zero
    , buttonCooldownInstant = False
    }


dailySpecialCooldown : Duration
dailySpecialCooldown =
    Duration.hours 20


dailySpecialBuffDuration : Duration
dailySpecialBuffDuration =
    Duration.day
