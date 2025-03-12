module Kinds exposing (..)

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
    , xp : Float -- Simplified from DwarfXp for this example
    , unlock : Maybe Int -- Simplified from Maybe UnlockKind for this example
    , duration : Float -- Simplified from Duration for this example
    }


type alias DwarfXpButtonRecord a =
    { dwarfXpButton1 : a
    , dwarfXpButton2 : a
    , dwarfXpButton3 : a
    , dwarfXpButton4 : a
    , dwarfXpButton5 : a
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



-- DwarfXpButtonStats from Config.elm


allDwarfXpButtonStats : DwarfXpButtonRecord DwarfXpButtonStats
allDwarfXpButtonStats =
    { dwarfXpButton1 = { id_ = "dwarfXpButton1", xp = 1, unlock = Nothing, duration = 1 }
    , dwarfXpButton2 = { id_ = "dwarfXpButton2", xp = 2, unlock = Just 5, duration = 5 }
    , dwarfXpButton3 = { id_ = "dwarfXpButton3", xp = 3, unlock = Just 10, duration = 60 }
    , dwarfXpButton4 = { id_ = "dwarfXpButton4", xp = 4, unlock = Just 11, duration = 360 }
    , dwarfXpButton5 = { id_ = "dwarfXpButton5", xp = 5, unlock = Just 12, duration = 1440 }
    }


dwarfXpButtonStats : DwarfXpButton -> DwarfXpButtonStats
dwarfXpButtonStats kind =
    getByDwarfXpButton allDwarfXpButtonStats kind
