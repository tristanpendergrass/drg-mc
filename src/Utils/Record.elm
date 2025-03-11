module Utils.Record exposing (..)

import Types exposing (..)



-- Missions


allMissions : List Mission
allMissions =
    [ Haz1, Haz2, Haz3, Haz4, Haz5 ]


getByMission : Mission -> MissionRecord a -> a
getByMission mission record =
    case mission of
        Haz1 ->
            record.haz1

        Haz2 ->
            record.haz2

        Haz3 ->
            record.haz3

        Haz4 ->
            record.haz4

        Haz5 ->
            record.haz5


setByMission : Mission -> a -> MissionRecord a -> MissionRecord a
setByMission mission value record =
    case mission of
        Haz1 ->
            { record | haz1 = value }

        Haz2 ->
            { record | haz2 = value }

        Haz3 ->
            { record | haz3 = value }

        Haz4 ->
            { record | haz4 = value }

        Haz5 ->
            { record | haz5 = value }


updateByMission : Mission -> (a -> a) -> MissionRecord a -> MissionRecord a
updateByMission mission f record =
    setByMission mission (f (getByMission mission record)) record



-- Resources


resourceRecord : a -> ResourceRecord a
resourceRecord a =
    { gold = a }


getByResource : Resource -> ResourceRecord a -> a
getByResource resource record =
    case resource of
        Gold ->
            record.gold


mapResources : (Resource -> a -> b) -> ResourceRecord a -> ResourceRecord b
mapResources f record =
    { gold = f Gold record.gold }


addResourceRecords : ResourceRecord Int -> ResourceRecord Int -> ResourceRecord Int
addResourceRecords a b =
    mapResources (\r x -> x + getByResource r b) a



-- Dwarfs


allDwarfs : List Dwarf
allDwarfs =
    [ Scout, Gunner, Engineer, Driller ]


dwarfRecord : a -> DwarfRecord a
dwarfRecord a =
    { scout = a
    , gunner = a
    , engineer = a
    , driller = a
    }


getByDwarf : Dwarf -> DwarfRecord a -> a
getByDwarf dwarf =
    case dwarf of
        Scout ->
            .scout

        Gunner ->
            .gunner

        Engineer ->
            .engineer

        Driller ->
            .driller


setByDwarf : Dwarf -> a -> DwarfRecord a -> DwarfRecord a
setByDwarf dwarf value record =
    case dwarf of
        Scout ->
            { record | scout = value }

        Gunner ->
            { record | gunner = value }

        Engineer ->
            { record | engineer = value }

        Driller ->
            { record | driller = value }


updateByDwarf : Dwarf -> (a -> a) -> DwarfRecord a -> DwarfRecord a
updateByDwarf dwarf f record =
    setByDwarf dwarf (f (getByDwarf dwarf record)) record



-- Dwarf Xp Buttons


allDwarfXpButtons : List DwarfXpButton
allDwarfXpButtons =
    [ DwarfXpButton1
    , DwarfXpButton2
    , DwarfXpButton3
    , DwarfXpButton4
    , DwarfXpButton5
    ]


dwarfXpButtonRecord : a -> DwarfXpButtonRecord a
dwarfXpButtonRecord a =
    { dwarfXpButton1 = a
    , dwarfXpButton2 = a
    , dwarfXpButton3 = a
    , dwarfXpButton4 = a
    , dwarfXpButton5 = a
    }


getByDwarfXpButton : DwarfXpButton -> DwarfXpButtonRecord a -> a
getByDwarfXpButton dwarfXpButton record =
    case dwarfXpButton of
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


setByDwarfXpButton : DwarfXpButton -> a -> DwarfXpButtonRecord a -> DwarfXpButtonRecord a
setByDwarfXpButton dwarfXpButton value record =
    case dwarfXpButton of
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


updateByDwarfXpButton : DwarfXpButton -> (a -> a) -> DwarfXpButtonRecord a -> DwarfXpButtonRecord a
updateByDwarfXpButton dwarfXpButton f record =
    setByDwarfXpButton dwarfXpButton (f (getByDwarfXpButton dwarfXpButton record)) record



-- Tabs


allTabs : List Tab
allTabs =
    [ MissionsTab
    , CommendationsTab
    , AbyssBarTab
    , SettingsTab
    ]


tabRecord : a -> TabRecord a
tabRecord a =
    { missionsTab = a
    , commendationsTab = a
    , abyssBarTab = a
    , settingsTab = a
    }


getByTab : Tab -> TabRecord a -> a
getByTab tab record =
    case tab of
        MissionsTab ->
            record.missionsTab

        CommendationsTab ->
            record.commendationsTab

        AbyssBarTab ->
            record.abyssBarTab

        SettingsTab ->
            record.settingsTab


setByTab : Tab -> a -> TabRecord a -> TabRecord a
setByTab tab value record =
    case tab of
        MissionsTab ->
            { record | missionsTab = value }

        CommendationsTab ->
            { record | commendationsTab = value }

        AbyssBarTab ->
            { record | abyssBarTab = value }

        SettingsTab ->
            { record | settingsTab = value }
