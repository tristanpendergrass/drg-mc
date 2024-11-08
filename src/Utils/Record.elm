module Utils.Record exposing (..)

import Types exposing (..)



-- Missions


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
