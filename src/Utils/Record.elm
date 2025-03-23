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



-- Tabs


allTabs : List Tab
allTabs =
    [ MissionsTab
    , CommendationsTab
    , ProjectsTab
    , AbyssBarTab
    , SettingsTab
    ]


tabRecord : a -> TabRecord a
tabRecord a =
    { missionsTab = a
    , commendationsTab = a
    , projectsTab = a
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

        ProjectsTab ->
            record.projectsTab

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

        ProjectsTab ->
            { record | projectsTab = value }

        AbyssBarTab ->
            { record | abyssBarTab = value }

        SettingsTab ->
            { record | settingsTab = value }
