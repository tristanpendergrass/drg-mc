module Types exposing (..)

import Duration exposing (Duration)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Time
import Utils.Timer exposing (Timer)



-- App


type alias Flags =
    { now : Int
    , initialGame : D.Value
    }


{-|

  - Themes only work if in the list in tailwind.config.js.alias
  - Also, DONT FORGET to update allThemes in Theme.elm when updating this list

-}
type Theme
    = Default
    | DefaultLight
    | DefaultDark
    | Retro
    | Cyberpunk
    | Black
    | Luxury


type alias Model =
    { currentTime : Time.Posix
    , saveTimer : Timer
    , theme : Theme
    , level : Int
    , credits : Float -- All the credits ever earned by the player, equivalent to experience points
    , resources : ResourceRecord Int -- Current resource stocks
    , missionStatuses : MissionRecord MissionStatus
    , gameSpeed : Float
    , debugAddedTime : Duration
    , animations : List (Maybe Animation)
    }


type Msg
    = NoOp
    | HandleAnimationFrame Time.Posix
    | HandleStartMissionClick Mission
    | HandleClaimCargoClick Mission Pointer.Event
    | HandleSetThemeClick Theme
    | DebugSetGameSpeed Float
    | DebugAdvanceTime Duration
    | DebugGainLevel
    | DebugLevelToMax
    | ResetGame



-- Missions


type Mission
    = Haz1
    | Haz2
    | Haz3
    | Haz4
    | Haz5


type alias MissionRecord a =
    { haz1 : a
    , haz2 : a
    , haz3 : a
    , haz4 : a
    , haz5 : a
    }


type alias MissionYield =
    { credits : Float
    , resources : ResourceRecord Int
    }


type alias MissionStats =
    { title : String
    , unlock : Maybe UnlockKind
    , duration : Duration
    , yield : MissionYield
    }


type MissionStatus
    = MissionComplete -- Missions start this way too...
    | MissionInProgress Timer



-- Resources


type Resource
    = Gold


type alias ResourceRecord a =
    { gold : a
    }



-- Level Requirements


type LevelRequirements
    = EarnCredits Float
    | AtMaxLevel



-- Level Unlocks


type UnlockKind
    = UnlockHaz2
    | UnlockHaz3
    | UnlockHaz4
    | UnlockHaz5
    | UnlockTheme Theme


type UnlockCategory
    = UnlockFeature
    | UnlockActivity
    | UnlockCosmetic


type alias LevelUnlockStats =
    { kind : UnlockKind
    , title : String
    , category : UnlockCategory
    }


type alias AnimationLocation =
    ( Float, Float )


type AnimationSubject
    = AnimateCreditsGain Float


type Animation
    = Animation Utils.Timer.Timer Duration AnimationLocation AnimationSubject
