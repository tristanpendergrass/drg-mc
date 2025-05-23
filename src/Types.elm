module Types exposing (..)

import AssocList as Dict exposing (Dict)
import Duration exposing (Duration)
import DwarfXp exposing (DwarfXp)
import FeatherIcons
import Html exposing (a)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Quantity exposing (Quantity(..))
import Random
import Time
import Utils.Percent exposing (Percent)
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
    , morkite : Float
    , missionStatuses : MissionRecord ButtonStatus
    , dwarfXpButtonStatuses : DwarfXpButtonRecord ButtonStatus
    , dwarfXp : DwarfRecord DwarfXp
    , activeDailySpecials : List ( DailySpecial, Timer )
    , dailySpecialCooldown : ButtonStatus
    , dailySpecialOptions : List DailySpecial
    , maybeInitDecodeErr : Maybe D.Error
    , minerals : MineralRecord Float
    , missionBiome : Maybe Biome
    , projectLevels : ProjectRecord Int
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
    | DebugGiveMinerals
    | ResetGame
    | HandleTabClick Tab
    | HandleDwarfXpButtonClick DwarfXpButton Pointer.Event
    | HandleDailySpecialClick DailySpecial
    | HandleMissionBiomeSelection Biome
    | HandleClearBiomeSelection
    | HandleProjectUpgrade Project
    | OpenModal String



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
    { morkite : Float
    , minerals : Dict Mineral Float
    }


type alias MissionStats =
    { title : String
    , unlock : Maybe Unlock
    , duration : Duration
    , morkite : Float
    }


type ButtonStatus
    = ButtonReady -- Missions start this way too...
    | ButtonOnCooldown Timer



-- Level Requirements


type LevelRequirements
    = GainMorkite Float
    | AtMaxLevel


type DwarfLevelRequirements
    = EarnDwarfXp Float
    | AtMaxDwarfLevel



-- Level Unlocks


type UnlockCategory
    = UnlockFeature
    | UnlockActivity
    | UnlockCosmetic
    | UnlockBiomes


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
    | ProjectsTab
    | AbyssBarTab
    | SettingsTab


type alias TabStats =
    { title : String
    , maybeIcon : Maybe FeatherIcons.Icon
    }


type alias TabRecord a =
    { missionsTab : a
    , commendationsTab : a
    , projectsTab : a
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
    | UnlockBiomeTier BiomeUnlockTier
    | UnlockProjects -- Projects tab unlocks at the same time as biomes



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


type Mod
    = ModMissionYield Percent
    | ModMissionSpeed Percent
    | ModDailySpecialBuffStrength Percent
    | ModDwarfXpGain Percent


type alias Buff =
    { title : String
    , icon : String
    , description : String
    , mod : Mod
    , mult : Int -- How many times the buff is applied. 1 is normal, 2 is double, etc.
    }



-- Daily Specials


type DailySpecial
    = DarkMorkite
    | PotsOGold
    | RedRockBlaster
    | RockyMountain


allDailySpecials : List DailySpecial
allDailySpecials =
    [ DarkMorkite, PotsOGold, RedRockBlaster, RockyMountain ]


type alias DailySpecialStats =
    { id_ : String
    , title : String
    , icon : String
    , buff : Buff
    }


type alias DailySpecialRecord a =
    { darkMorkite : a
    , potsOGold : a
    , redRockBlaster : a
    , rockyMountain : a
    }


dailySpecialRecord : a -> DailySpecialRecord a
dailySpecialRecord a =
    { darkMorkite = a
    , potsOGold = a
    , redRockBlaster = a
    , rockyMountain = a
    }


getByDailySpecial : DailySpecialRecord a -> DailySpecial -> a
getByDailySpecial record kind =
    case kind of
        DarkMorkite ->
            record.darkMorkite

        PotsOGold ->
            record.potsOGold

        RedRockBlaster ->
            record.redRockBlaster

        RockyMountain ->
            record.rockyMountain


setByDailySpecial : a -> DailySpecial -> DailySpecialRecord a -> DailySpecialRecord a
setByDailySpecial value kind record =
    case kind of
        DarkMorkite ->
            { record | darkMorkite = value }

        PotsOGold ->
            { record | potsOGold = value }

        RedRockBlaster ->
            { record | redRockBlaster = value }

        RockyMountain ->
            { record | rockyMountain = value }


allDailySpecialStats : DailySpecialRecord DailySpecialStats
allDailySpecialStats =
    { darkMorkite =
        { id_ = "darkMorkite"
        , title = "Dark Morkite"
        , icon = "beer/dark_morkite.webp"

        -- , buff = ModMissionYield (Utils.Percent.float 0.1)
        , buff =
            { title = "Daily Special"
            , icon = "beer/beer2.png"
            , description = "Drinking Dark Morkite increases the yield of missions"
            , mod = ModMissionYield (Utils.Percent.float 0.1)
            , mult = 1
            }
        }
    , potsOGold =
        { id_ = "potsOGold"
        , title = "Pots o' Gold"
        , icon = "beer/pots_o_gold.webp"
        , buff =
            { title = "Daily Special"
            , icon = "beer/beer2.png"
            , description = "Drinking Pots o' Gold increases the XP gain of dwarfs"
            , mod = ModDwarfXpGain (Utils.Percent.float 1.0)
            , mult = 1
            }
        }
    , redRockBlaster =
        { id_ = "redRockBlaster"
        , title = "Red Rock Blaster"
        , icon = "beer/red_rock_blaster.webp"
        , buff =
            { title = "Daily Special"
            , icon = "beer/beer2.png"
            , description = "Drinking Red Rock Blaster increases the yield of missions"
            , mod = ModMissionYield (Utils.Percent.float 0.3)
            , mult = 1
            }
        }
    , rockyMountain =
        { id_ = "rockyMountain"
        , title = "Rocky Mountain"
        , icon = "beer/rocky_mountain.webp"
        , buff =
            { title = "Daily Special"
            , icon = "beer/beer2.png"
            , description = "Drinking Rocky Mountain increases the speed of missions"
            , mod = ModMissionSpeed (Utils.Percent.float 0.4)
            , mult = 1
            }
        }
    }


dailySpecialStats : DailySpecial -> DailySpecialStats
dailySpecialStats kind =
    getByDailySpecial allDailySpecialStats kind



-- Minerals


type Mineral
    = Jadiz
    | Bismor
    | EnorPearl
    | Croppa
    | Magnite
    | Umanite


allMinerals : List Mineral
allMinerals =
    [ Jadiz, Bismor, EnorPearl, Croppa, Magnite, Umanite ]


type alias MineralStats =
    { name : String
    , icon : String
    }


type alias MineralRecord a =
    { jadiz : a
    , bismor : a
    , enorPearl : a
    , croppa : a
    , magnite : a
    , umanite : a
    }


mineralRecord : a -> MineralRecord a
mineralRecord a =
    { jadiz = a
    , bismor = a
    , enorPearl = a
    , croppa = a
    , magnite = a
    , umanite = a
    }


getByMineral : MineralRecord a -> Mineral -> a
getByMineral record kind =
    case kind of
        Jadiz ->
            record.jadiz

        Bismor ->
            record.bismor

        EnorPearl ->
            record.enorPearl

        Croppa ->
            record.croppa

        Magnite ->
            record.magnite

        Umanite ->
            record.umanite


setByMineral : a -> Mineral -> MineralRecord a -> MineralRecord a
setByMineral value kind record =
    case kind of
        Jadiz ->
            { record | jadiz = value }

        Bismor ->
            { record | bismor = value }

        EnorPearl ->
            { record | enorPearl = value }

        Croppa ->
            { record | croppa = value }

        Magnite ->
            { record | magnite = value }

        Umanite ->
            { record | umanite = value }


updateByMineral : (a -> a) -> MineralRecord a -> Mineral -> MineralRecord a
updateByMineral f record kind =
    setByMineral (f (getByMineral record kind)) kind record


allMineralStats : MineralRecord MineralStats
allMineralStats =
    { jadiz = { name = "Jadiz", icon = "minerals/jadiz.webp" }
    , bismor = { name = "Bismor", icon = "minerals/bismor.webp" }
    , enorPearl = { name = "Enor Pearl", icon = "minerals/enorPearl.webp" }
    , croppa = { name = "Croppa", icon = "minerals/croppa.webp" }
    , magnite = { name = "Magnite", icon = "minerals/magnite.webp" }
    , umanite = { name = "Umanite", icon = "minerals/umanite.webp" }
    }


mineralStats : Mineral -> MineralStats
mineralStats kind =
    getByMineral allMineralStats kind



-- Biomes


type Biome
    = CrystallineCaverns
    | HollowBough
    | SaltPits
    | SandblastedCorridors
    | FungusBogs
    | AzureWeald
    | GlacialStrata
    | MagmaCore
    | DenseBiozone
    | RadioactiveExclusionZone


allBiomes : List Biome
allBiomes =
    [ CrystallineCaverns, HollowBough, SaltPits, SandblastedCorridors, FungusBogs, AzureWeald, GlacialStrata, MagmaCore, DenseBiozone, RadioactiveExclusionZone ]


type alias BiomeStats =
    { id_ : String
    , name : String
    , image : String
    , icon : String
    , abundantMineral : Mineral
    , scarceMineral : Mineral
    , unlockTier : BiomeUnlockTier
    }


type BiomeUnlockTier
    = BiomeUnlockTier1
    | BiomeUnlockTier2
    | BiomeUnlockTier3


type alias BiomeRecord a =
    { crystallineCaverns : a
    , hollowBough : a
    , saltPits : a
    , sandblastedCorridors : a
    , fungusBogs : a
    , azureWeald : a
    , glacialStrata : a
    , magmaCore : a
    , denseBiozone : a
    , radioactiveExclusionZone : a
    }


biomeRecord : a -> BiomeRecord a
biomeRecord a =
    { crystallineCaverns = a
    , hollowBough = a
    , saltPits = a
    , sandblastedCorridors = a
    , fungusBogs = a
    , azureWeald = a
    , glacialStrata = a
    , magmaCore = a
    , denseBiozone = a
    , radioactiveExclusionZone = a
    }


getByBiome : BiomeRecord a -> Biome -> a
getByBiome record kind =
    case kind of
        CrystallineCaverns ->
            record.crystallineCaverns

        HollowBough ->
            record.hollowBough

        SaltPits ->
            record.saltPits

        SandblastedCorridors ->
            record.sandblastedCorridors

        FungusBogs ->
            record.fungusBogs

        AzureWeald ->
            record.azureWeald

        GlacialStrata ->
            record.glacialStrata

        MagmaCore ->
            record.magmaCore

        DenseBiozone ->
            record.denseBiozone

        RadioactiveExclusionZone ->
            record.radioactiveExclusionZone


setByBiome : a -> Biome -> BiomeRecord a -> BiomeRecord a
setByBiome value kind record =
    case kind of
        CrystallineCaverns ->
            { record | crystallineCaverns = value }

        HollowBough ->
            { record | hollowBough = value }

        SaltPits ->
            { record | saltPits = value }

        SandblastedCorridors ->
            { record | sandblastedCorridors = value }

        FungusBogs ->
            { record | fungusBogs = value }

        AzureWeald ->
            { record | azureWeald = value }

        GlacialStrata ->
            { record | glacialStrata = value }

        MagmaCore ->
            { record | magmaCore = value }

        DenseBiozone ->
            { record | denseBiozone = value }

        RadioactiveExclusionZone ->
            { record | radioactiveExclusionZone = value }


updateByBiome : (a -> a) -> BiomeRecord a -> Biome -> BiomeRecord a
updateByBiome f record kind =
    setByBiome (f (getByBiome record kind)) kind record


allBiomeStats : BiomeRecord BiomeStats
allBiomeStats =
    { crystallineCaverns =
        { id_ = "crystallineCaverns"
        , name = "Crystalline Caverns"
        , image = "biomes/crystallineCaverns_image4.webp"
        , icon = "biomes/crystallineCaverns_icon.webp"
        , abundantMineral = Jadiz
        , scarceMineral = Bismor
        , unlockTier = BiomeUnlockTier1
        }
    , hollowBough =
        { id_ = "hollowBough"
        , name = "Hollow Bough"
        , image = "biomes/hollowBough_image.webp"
        , icon = "biomes/hollowBough_icon.webp"
        , abundantMineral = Jadiz
        , scarceMineral = Croppa
        , unlockTier = BiomeUnlockTier1
        }
    , saltPits =
        { id_ = "saltPits"
        , name = "Salt Pits"
        , image = "biomes/saltPits_image.webp"
        , icon = "biomes/saltPits_icon.webp"
        , abundantMineral = EnorPearl
        , scarceMineral = Bismor
        , unlockTier = BiomeUnlockTier1
        }
    , sandblastedCorridors =
        { id_ = "sandblastedCorridors"
        , name = "Sandblasted Corridors"
        , image = "biomes/sanblastedCorridors_image.webp"
        , icon = "biomes/sanblastedCorridors_icon.webp"
        , abundantMineral = EnorPearl
        , scarceMineral = Magnite
        , unlockTier = BiomeUnlockTier1
        }
    , fungusBogs =
        { id_ = "fungusBogs"
        , name = "Fungus Bogs"
        , image = "biomes/fungusBogs_image.webp"
        , icon = "biomes/fungusBogs_icon.webp"
        , abundantMineral = Croppa
        , scarceMineral = Jadiz
        , unlockTier = BiomeUnlockTier2
        }
    , azureWeald =
        { id_ = "azureWeald"
        , name = "Azure Weald"
        , image = "biomes/azureWeald_image.webp"
        , icon = "biomes/azureWeald_icon.webp"
        , abundantMineral = Croppa
        , scarceMineral = Umanite
        , unlockTier = BiomeUnlockTier2
        }
    , glacialStrata =
        { id_ = "glacialStrata"
        , name = "Glacial Strata"
        , image = "biomes/glacialStrata_image.webp"
        , icon = "biomes/glacialStrata_icon.webp"
        , abundantMineral = Magnite
        , scarceMineral = Umanite
        , unlockTier = BiomeUnlockTier2
        }
    , magmaCore =
        { id_ = "magmaCore"
        , name = "Magma Core"
        , image = "biomes/magmaCore_image.webp"
        , icon = "biomes/magmaCore_icon.webp"
        , abundantMineral = Magnite
        , scarceMineral = Croppa
        , unlockTier = BiomeUnlockTier2
        }
    , denseBiozone =
        { id_ = "denseBiozone"
        , name = "Dense Biozone"
        , image = "biomes/denseBiozone_picture.webp"
        , icon = "biomes/denseBiozone_icon.webp"
        , abundantMineral = Bismor
        , scarceMineral = Umanite
        , unlockTier = BiomeUnlockTier3
        }
    , radioactiveExclusionZone =
        { id_ = "radioactiveExclusionZone"
        , name = "Radioactive Exclusion Zone"
        , image = "biomes/radioactiveExclusionZone_image.webp"
        , icon = "biomes/radioactiveExclusionZone_icon.webp"
        , abundantMineral = Umanite
        , scarceMineral = EnorPearl
        , unlockTier = BiomeUnlockTier3
        }
    }


biomeStats : Biome -> BiomeStats
biomeStats kind =
    getByBiome allBiomeStats kind



-- Projects


type Project
    = Mule -- M.U.L.E. upgrades increase the yield of missions
    | Lloyd -- Lloyd upgrades increase the stats of daily specials
    | Jukebox -- Jukebox upgrades increase the amount of dwarf xp
    | Pickaxes -- Pickaxes upgrades increase the speed of missions


allProjects : List Project
allProjects =
    [ Mule, Lloyd, Jukebox, Pickaxes ]


type alias ProjectStats =
    { name : String
    , buff : Buff
    , costs : Dict Mineral Float
    , maxLevels : Int
    }


type alias ProjectRecord a =
    { mule : a
    , lloyd : a
    , jukebox : a
    , pickaxes : a
    }


projectRecord : a -> ProjectRecord a
projectRecord a =
    { mule = a
    , lloyd = a
    , jukebox = a
    , pickaxes = a
    }


getByProject : ProjectRecord a -> Project -> a
getByProject record kind =
    case kind of
        Mule ->
            record.mule

        Lloyd ->
            record.lloyd

        Jukebox ->
            record.jukebox

        Pickaxes ->
            record.pickaxes


setByProject : a -> Project -> ProjectRecord a -> ProjectRecord a
setByProject value kind record =
    case kind of
        Mule ->
            { record | mule = value }

        Lloyd ->
            { record | lloyd = value }

        Jukebox ->
            { record | jukebox = value }

        Pickaxes ->
            { record | pickaxes = value }


updateByProject : (a -> a) -> ProjectRecord a -> Project -> ProjectRecord a
updateByProject f record kind =
    setByProject (f (getByProject record kind)) kind record


allProjectStats : ProjectRecord ProjectStats
allProjectStats =
    { mule =
        { name = "M.U.L.E. storage"
        , buff =
            { title = "M.U.L.E."
            , icon = "mule.webp"
            , description = "Increases mission yield"
            , mod = ModMissionYield (Utils.Percent.float 1.0)
            , mult = 1
            }
        , costs = Dict.fromList [ ( Bismor, 20 ), ( Croppa, 20 ) ]
        , maxLevels = 2
        }
    , lloyd =
        { name = "Lloyd"
        , buff =
            { title = "Frothier Drinks"
            , icon = "lloyd.jpg"
            , description = "Increases the strength of daily specials"
            , mod = ModDailySpecialBuffStrength (Utils.Percent.float 1.0)
            , mult = 1
            }
        , costs = Dict.fromList [ ( Magnite, 40 ), ( Umanite, 40 ) ]
        , maxLevels = 2
        }
    , jukebox =
        { name = "Jukebox"
        , buff =
            { title = "Better Tunes"
            , icon = "jukebox.jpg"
            , description = "Increases dwarf XP gain"
            , mod = ModDwarfXpGain (Utils.Percent.float 2.0)
            , mult = 1
            }
        , costs = Dict.fromList [ ( Jadiz, 40 ), ( EnorPearl, 40 ) ]
        , maxLevels = 2
        }
    , pickaxes =
        { name = "Pickaxes"
        , buff =
            { title = "Pickaxes upgrades"
            , icon = "pickaxe.webp"
            , description = "Increases mission speed"
            , mod = ModMissionSpeed (Utils.Percent.float 0.35)
            , mult = 1
            }
        , costs = Dict.fromList [ ( Bismor, 30 ), ( Magnite, 30 ) ]
        , maxLevels = 2
        }
    }


projectStats : Project -> ProjectStats
projectStats kind =
    getByProject allProjectStats kind
