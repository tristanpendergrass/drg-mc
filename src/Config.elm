module Config exposing (..)

import Dict exposing (Dict)
import Duration exposing (Duration)
import Types exposing (..)
import Utils.Record


type Env
    = Dev
    | Prod


env : Env
env =
    -- Dev
    Prod


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
        , ( 5, [ { kind = UnlockHaz4, title = "Haz 4", category = UnlockActivity } ] )
        , ( 6, [ { kind = UnlockTheme Black, title = "Theme: Black", category = UnlockCosmetic } ] )
        , ( 7, [ { kind = UnlockHaz5, title = "Haz 5", category = UnlockActivity } ] )
        , ( 8, [ { kind = UnlockTheme Luxury, title = "Theme: Luxury", category = UnlockCosmetic } ] )
        , ( 12, [ { kind = UnlockTheme Cyberpunk, title = "Theme: Cyberpunk", category = UnlockCosmetic } ] )
        ]


levelingSchedule : Int -> LevelRequirements
levelingSchedule level =
    -- case level of
    --     1 ->
    --         EarnCredits 5.0
    --     2 ->
    --         EarnCredits 10.0
    --     _ ->
    --         AtMaxLevel
    if level < 6 then
        EarnCredits (toFloat level * 5.0)

    else if level < 12 then
        EarnCredits (toFloat level * 10.0)

    else
        AtMaxLevel


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
