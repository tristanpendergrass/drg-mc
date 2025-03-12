module Types exposing (..)

import Duration exposing (Duration)
import DwarfXp exposing (DwarfXp)
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


type alias DebugSettings =
    { gameSpeed : Float
    , addedTime : Duration -- When we let the player "fast forward" a set duration we increment this number, which is added to the current time before all calculations
    , buttonCooldownInstant : Bool
    }


type alias Model =
    { seed : Random.Seed
    , debugSettings : DebugSettings
    , currentTime : Time.Posix
    , currentTab : Tab
    , saveTimer : Timer
    , theme : Maybe Theme -- Nothing means the user has never adjusted this setting
    , level : Int
    , credits : Float -- All the credits ever earned by the player, equivalent to experience points
    , resources : ResourceRecord Int -- Current resource stocks
    , missionStatuses : MissionRecord ButtonStatus
    , dwarfXpButtonStatuses : DwarfXpButtonRecord ButtonStatus
    , dwarfXp : DwarfRecord DwarfXp
    }


type Msg
    = NoOp
    | HandleAnimationFrame Time.Posix
    | HandleMissionClick Mission Pointer.Event
    | HandleSetThemeClick Theme
    | DebugSetGameSpeed Float
    | DebugAdvanceTime Duration
    | DebugSetButtonCooldownInstant Bool
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
    , unlock : Maybe Unlock
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


type UnlockCategory
    = UnlockFeature
    | UnlockActivity
    | UnlockCosmetic


type alias LevelUnlockStats =
    { kind : Unlock
    , title : String
    , category : UnlockCategory
    }


type AlertType
    = AlertSuccess
    | AlertWarning
    | AlertError
    | AlertInfo



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



-- Tabs


type
    Tab
    -- Don't forget to update allTabs too!
    = MissionsTab
    | CommendationsTab
    | AbyssBarTab
    | SettingsTab


type alias TabStats =
    { title : String
    , icon : FeatherIcons.Icon
    }


type alias TabRecord a =
    { missionsTab : a
    , commendationsTab : a
    , abyssBarTab : a
    , settingsTab : a
    }


{-|

  - Themes only work if in the list in src/main.css. In the DaisyUI plugin part.
  - Also, don't forget to update allThemes in Theme.elm when updating this list

-}
type Theme
    = Default
    | DefaultLight
    | DefaultDark
    | Retro
    | Cyberpunk
    | Black
    | Luxury


type Unlock
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
    | UnlockAbyssBar



{--| A kind is a game concept and it always has the following format:

-- Things
type Thing
    = Thing1
    | Thing2
    | Thing3

allThings : List Thing
allThings =
    [ Thing1, Thing2, Thing3 ]

type alias ThingStats =
    { kind : Thing
    , title : String -- If the concept has a human-readable name, we call it title
    , icon : String -- If the concept has an icon, we call it icon
    , -- other stuff
    }

type alias ThingRecord a =
    { thing1 : a
    , thing2 : a
    , thing3 : a
    }

thingRecord : a -> ThingRecord a
thingRecord a =
    { thing1 = a
    , thing2 = a
    , thing3 = a
    }

getByThing : ThingRecord a -> Thing -> a
getByThing record kind =
    case kind of
        Thing1 -> record.thing1
        Thing2 -> record.thing2
        Thing3 -> record.thing3

setByThing : a -> Thing -> ThingRecord a -> ThingRecord a
setByThing value kind record =
    case kind of
        Thing1 -> { record | thing1 = value }
        Thing2 -> { record | thing2 = value }
        Thing3 -> { record | thing3 = value }

updateByThing : (a -> a) -> ThingRecord a -> Thing -> -> ThingRecord a
updateByThing f record kind =
    setByThing (f (getByThing kind record)) thing record

thingStats : Thing -> ThingStats
thingStats =
    getByThing allThingStats
-}
-- Dwarf XP Buttons


type DwarfXpButton
    = DwarfXpButton1
    | DwarfXpButton2
    | DwarfXpButton3
    | DwarfXpButton4
    | DwarfXpButton5


allDwarfXpButtons : List DwarfXpButton
allDwarfXpButtons =
    [ DwarfXpButton1, DwarfXpButton2, DwarfXpButton3, DwarfXpButton4, DwarfXpButton5 ]


type alias DwarfXpButtonStats =
    { id_ : String
    , xp : DwarfXp
    , unlock : Maybe Unlock
    , duration : Duration
    }


type alias DwarfXpButtonRecord a =
    { dwarfXpButton1 : a
    , dwarfXpButton2 : a
    , dwarfXpButton3 : a
    , dwarfXpButton4 : a
    , dwarfXpButton5 : a
    }


dwarfXpButtonRecord : a -> DwarfXpButtonRecord a
dwarfXpButtonRecord a =
    { dwarfXpButton1 = a
    , dwarfXpButton2 = a
    , dwarfXpButton3 = a
    , dwarfXpButton4 = a
    , dwarfXpButton5 = a
    }


getByDwarfXpButton : DwarfXpButtonRecord a -> DwarfXpButton -> a
getByDwarfXpButton record kind =
    case kind of
        DwarfXpButton1 ->
            record.dwarfXpButton1

        DwarfXpButton2 ->
            record.dwarfXpButton2

        DwarfXpButton3 ->
            record.dwarfXpButton3

        DwarfXpButton4 ->
            record.dwarfXpButton4

        DwarfXpButton5 ->
            record.dwarfXpButton5


setByDwarfXpButton : a -> DwarfXpButton -> DwarfXpButtonRecord a -> DwarfXpButtonRecord a
setByDwarfXpButton value kind record =
    case kind of
        DwarfXpButton1 ->
            { record | dwarfXpButton1 = value }

        DwarfXpButton2 ->
            { record | dwarfXpButton2 = value }

        DwarfXpButton3 ->
            { record | dwarfXpButton3 = value }

        DwarfXpButton4 ->
            { record | dwarfXpButton4 = value }

        DwarfXpButton5 ->
            { record | dwarfXpButton5 = value }


updateByDwarfXpButton : (a -> a) -> DwarfXpButtonRecord a -> DwarfXpButton -> DwarfXpButtonRecord a
updateByDwarfXpButton f record kind =
    setByDwarfXpButton (f (getByDwarfXpButton record kind)) kind record


allDwarfXpButtonStats : DwarfXpButtonRecord DwarfXpButtonStats
allDwarfXpButtonStats =
    { dwarfXpButton1 = { id_ = "dwarfXpButton1", xp = DwarfXp.float 1, unlock = Nothing, duration = Duration.minutes 1 }
    , dwarfXpButton2 = { id_ = "dwarfXpButton2", xp = DwarfXp.float 2, unlock = Just UnlockDwarfXpButton2, duration = Duration.minutes 5 }
    , dwarfXpButton3 = { id_ = "dwarfXpButton3", xp = DwarfXp.float 3, unlock = Just UnlockDwarfXpButton3, duration = Duration.minutes 60 }
    , dwarfXpButton4 = { id_ = "dwarfXpButton4", xp = DwarfXp.float 4, unlock = Just UnlockDwarfXpButton4, duration = Duration.minutes 360 }
    , dwarfXpButton5 = { id_ = "dwarfXpButton5", xp = DwarfXp.float 5, unlock = Just UnlockDwarfXpButton5, duration = Duration.minutes 1440 }
    }


dwarfXpButtonStats : DwarfXpButton -> DwarfXpButtonStats
dwarfXpButtonStats kind =
    getByDwarfXpButton allDwarfXpButtonStats kind
