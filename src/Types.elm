module Types exposing (..)

import Duration exposing (Duration)
import FeatherIcons
import Html exposing (a)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Quantity exposing (Quantity(..))
import Random
import Time
import Utils.Timer exposing (Timer)



-- App


type alias Flags =
    { now : Int
    , initialGame : D.Value
    , initialSeed : Int
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
    { seed : Random.Seed
    , currentTime : Time.Posix
    , currentTab : Tab
    , saveTimer : Timer
    , theme : Maybe Theme -- Nothing means the user has never adjusted this setting
    , level : Int
    , credits : Float -- All the credits ever earned by the player, equivalent to experience points
    , resources : ResourceRecord Int -- Current resource stocks
    , missionStatuses : MissionRecord ButtonStatus
    , dwarfXpButtonStatuses : DwarfXpButtonRecord ButtonStatus
    , gameSpeed : Float
    , debugAddedTime : Duration
    , animations : List (Maybe Animation)
    , dwarfXp : DwarfRecord DwarfXp
    }


type Msg
    = NoOp
    | HandleAnimationFrame Time.Posix
    | HandleMissionClick Mission Pointer.Event
    | HandleSetThemeClick Theme
    | DebugSetGameSpeed Float
    | DebugAdvanceTime Duration
    | DebugGainLevel
    | DebugLevelToMax
    | ResetGame
    | HandleTabClick Tab
    | HandleDwarfXpButtonClick DwarfXpButton Pointer.Event



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


type ButtonStatus
    = ButtonReady -- Missions start this way too...
    | ButtonOnCooldown Timer



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


type DwarfLevelRequirements
    = EarnDwarfXp Float
    | AtMaxDwarfLevel



-- Level Unlocks


type UnlockKind
    = UnlockHaz2
    | UnlockHaz3
    | UnlockHaz4
    | UnlockHaz5
    | UnlockTheme Theme
    | UnlockDwarfXpButtons -- Once this is unlocked the whole feature becomes available along with Dwarf Xp Button 1
    | UnlockDwarfXpButton2
    | UnlockDwarfXpButton3
    | UnlockDwarfXpButton4
    | UnlockDwarfXpButton5


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


type AlertType
    = AlertSuccess
    | AlertWarning
    | AlertError
    | AlertInfo


type AnimationSubject
    = AnimateCreditsGain Float AnimationLocation
    | AnimateDwarfXp Dwarf DwarfXp AnimationLocation
    | AnimateAlert String AlertType


type Animation
    = Animation Utils.Timer.Timer Duration AnimationSubject



-- Dwarfs


type Dwarf
    = Scout
    | Gunner
    | Engineer
    | Driller


type alias DwarfStats =
    { name : String
    , imgSrc : String
    }


type alias DwarfRecord a =
    { scout : a
    , gunner : a
    , engineer : a
    , driller : a
    }



-- Dwarf XP Buttons


type DwarfXpPoint
    = DwarfXpPoint


type alias DwarfXp =
    Quantity Float DwarfXpPoint


type DwarfXpButton
    = DwarfXpButton1
    | DwarfXpButton2
    | DwarfXpButton3
    | DwarfXpButton4
    | DwarfXpButton5


type alias DwarfXpButtonStats =
    { id_ : String
    , xp : DwarfXp
    , unlock : Maybe UnlockKind
    , duration : Duration
    }


type alias DwarfXpButtonRecord a =
    { dwarfXpButton1 : a
    , dwarfXpButton2 : a
    , dwarfXpButton3 : a
    , dwarfXpButton4 : a
    , dwarfXpButton5 : a
    }



-- Tabs


type Tab
    = MissionsTab
    | CommendationsTab


type alias TabStats =
    { title : String
    , icon : FeatherIcons.Icon
    }


type alias TabRecord a =
    { missionsTab : a
    , commendationsTab : a
    }
