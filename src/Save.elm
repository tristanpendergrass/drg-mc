module Save exposing (..)

import Config
import DwarfXp
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import Json.Encode.Extra
import List
import List.Extra
import Quantity
import Random
import Theme
import Time
import Types exposing (..)
import Utils.Percent
import Utils.Record
import Utils.Timer



-- Versions


decodeAnyVersion : Random.Seed -> Decoder Model
decodeAnyVersion seed =
    D.oneOf
        [ decoder seed
        , v0_2Decoder seed
        , v0_1Decoder
        ]



-- Helpers


posixDecoder : Decoder Time.Posix
posixDecoder =
    D.map Time.millisToPosix D.int


posixEncoder : Time.Posix -> E.Value
posixEncoder posix =
    E.int (Time.posixToMillis posix)



-- v0.1
-- v0_1ResourcesDecoder : Decoder (ResourceRecord Int)
-- v0_1ResourcesDecoder =
--     D.map ResourceRecord <|
--         D.field "gold" D.int


percentDecoder : Decoder Utils.Percent.Percent
percentDecoder =
    D.field "current" <|
        D.map Utils.Percent.float D.float


buttonStatusDecoder : Decoder ButtonStatus
buttonStatusDecoder =
    D.oneOf
        [ D.null ButtonReady
        , D.map ButtonOnCooldown Utils.Timer.timerDecoder
        ]


v0_1MissionStatusesDecoder : Decoder (MissionRecord ButtonStatus)
v0_1MissionStatusesDecoder =
    D.map5 MissionRecord
        (D.field "haz1" buttonStatusDecoder)
        (D.field "haz2" buttonStatusDecoder)
        (D.field "haz3" buttonStatusDecoder)
        (D.field "haz4" buttonStatusDecoder)
        (D.field "haz5" buttonStatusDecoder)


themeDecoder : Decoder Theme
themeDecoder =
    D.string
        |> D.andThen
            (\themeString ->
                case Theme.stringToTheme themeString of
                    Nothing ->
                        D.fail ("Unknown theme: " ++ themeString)

                    Just theme ->
                        D.succeed theme
            )


v0_1Decoder : Decoder Model
v0_1Decoder =
    D.map6
        (\_ currentTime currentTab level credits missionStatuses ->
            let
                model : Model
                model =
                    { seed = Random.initialSeed 0
                    , debugSettings = Config.defaultDebugSettings
                    , currentTime = currentTime
                    , currentTab = currentTab
                    , theme = Just Default
                    , level = level
                    , morkite = credits
                    , missionStatuses = missionStatuses
                    , saveTimer = Utils.Timer.create
                    , dwarfXp = Utils.Record.dwarfRecord (DwarfXp.float 0)
                    , dwarfXpButtonStatuses = dwarfXpButtonRecord ButtonReady
                    , activeDailySpecials = []
                    , dailySpecialCooldown = ButtonReady
                    , dailySpecialOptions = [ DarkMorkite, RockyMountain ]
                    , maybeInitDecodeErr = Nothing
                    , minerals = mineralRecord 0
                    , missionBiome = Nothing
                    , projectLevels = projectRecord 0
                    }
            in
            model
        )
        -- Note we later revised the convention to "v0.1" but in users' localstorage it will be v1.
        -- After all old users of "v1" are on v0.2 or later we can remove this in preparation for the real "v1" someday.
        (D.field "version" (versionDecoder "v1"))
        (D.field "currentTime" posixDecoder)
        (D.succeed MissionsTab)
        (D.field "level" D.int)
        (D.field "credits" D.float)
        (D.field "missionStatuses" v0_1MissionStatusesDecoder)


versionDecoder : String -> Decoder ()
versionDecoder expectedVersion =
    D.string
        |> D.andThen
            (\decodedVersion ->
                if decodedVersion == expectedVersion then
                    D.succeed ()

                else
                    D.fail ("This is the parser for " ++ expectedVersion ++ " but this content is " ++ decodedVersion)
            )


v0_2Decoder : Random.Seed -> Decoder Model
v0_2Decoder initialSeed =
    D.field "v0.2" <|
        (D.succeed
            (\currentTime currentTab theme level credits missionStatuses dwarfXp dwarfXpButtonStatuses ->
                let
                    model : Model
                    model =
                        { seed = initialSeed
                        , debugSettings = Config.defaultDebugSettings
                        , currentTime = currentTime
                        , currentTab = currentTab
                        , theme = theme
                        , level = level
                        , morkite = credits
                        , missionStatuses = missionStatuses
                        , saveTimer = Utils.Timer.create
                        , dwarfXp = dwarfXp
                        , dwarfXpButtonStatuses = dwarfXpButtonStatuses
                        , activeDailySpecials = []
                        , dailySpecialCooldown = ButtonReady
                        , dailySpecialOptions = [ DarkMorkite, RockyMountain ]
                        , maybeInitDecodeErr = Nothing
                        , minerals = mineralRecord 0
                        , missionBiome = Nothing
                        , projectLevels = projectRecord 0
                        }
                in
                model
            )
            |> required "currentTime" posixDecoder
            |> required "currentTab" v0_2TabDecoder
            |> optional "theme" (D.map Just themeDecoder) Nothing
            |> required "level" D.int
            |> required "credits" D.float
            |> required "missionStatuses" v0_1MissionStatusesDecoder
            |> required "dwarfXp" (v0_2DwarfRecordDecoder (D.map DwarfXp.float D.float))
            |> required "dwarfXpButtonStatuses" (v0_2DwarfXpButtonRecordDecoder buttonStatusDecoder)
        )


v0_2DwarfXpButtonRecordDecoder : Decoder a -> Decoder (DwarfXpButtonRecord a)
v0_2DwarfXpButtonRecordDecoder d =
    D.map5
        (\dwarfXpButton1 dwarfXpButton2 dwarfXpButton3 dwarfXpButton4 dwarfXpButton5 ->
            { dwarfXpButton1 = dwarfXpButton1
            , dwarfXpButton2 = dwarfXpButton2
            , dwarfXpButton3 = dwarfXpButton3
            , dwarfXpButton4 = dwarfXpButton4
            , dwarfXpButton5 = dwarfXpButton5
            }
        )
        (D.field "dwarfXpButton1" d)
        (D.field "dwarfXpButton2" d)
        (D.field "dwarfXpButton3" d)
        (D.field "dwarfXpButton4" d)
        (D.field "dwarfXpButton5" d)


v0_2TabDecoder : Decoder Tab
v0_2TabDecoder =
    D.string
        |> D.andThen
            (\tabString ->
                case List.Extra.find (\t -> (Utils.Record.getByTab t Config.tabStats).title == tabString) Utils.Record.allTabs of
                    Just tab ->
                        D.succeed tab

                    Nothing ->
                        D.fail ("Unknown tab: " ++ tabString)
            )


v0_2DwarfRecordDecoder : Decoder a -> Decoder (DwarfRecord a)
v0_2DwarfRecordDecoder valueDecoder =
    D.map4
        (\scoutVal gunnerVal engineerVal drillerVal ->
            { scout = scoutVal
            , gunner = gunnerVal
            , engineer = engineerVal
            , driller = drillerVal
            }
        )
        (D.field "scout" valueDecoder)
        (D.field "gunner" valueDecoder)
        (D.field "engineer" valueDecoder)
        (D.field "driller" valueDecoder)


decoder : Random.Seed -> Decoder Model
decoder initialSeed =
    D.field "v0.3" <|
        (D.succeed
            (\currentTime currentTab theme level morkite missionStatuses dwarfXp dwarfXpButtonStatuses activeDailySpecials dailySpecialOptions dailySpecialCooldown minerals missionBiome projectLevels ->
                let
                    model : Model
                    model =
                        { seed = initialSeed
                        , debugSettings = Config.defaultDebugSettings
                        , currentTime = currentTime
                        , currentTab = currentTab
                        , theme = theme
                        , level = level
                        , morkite = morkite
                        , missionStatuses = missionStatuses
                        , saveTimer = Utils.Timer.create
                        , dwarfXp = dwarfXp
                        , dwarfXpButtonStatuses = dwarfXpButtonStatuses
                        , activeDailySpecials = activeDailySpecials
                        , dailySpecialCooldown = dailySpecialCooldown
                        , dailySpecialOptions = dailySpecialOptions
                        , maybeInitDecodeErr = Nothing
                        , minerals = minerals
                        , missionBiome = missionBiome
                        , projectLevels = projectLevels
                        }
                in
                model
            )
            |> required "currentTime" posixDecoder
            |> required "currentTab" v0_2TabDecoder
            |> optional "theme" (D.map Just themeDecoder) Nothing
            |> required "level" D.int
            |> required "morkite" D.float
            |> required "missionStatuses" v0_1MissionStatusesDecoder
            |> required "dwarfXp" (v0_2DwarfRecordDecoder (D.map DwarfXp.float D.float))
            |> required "dwarfXpButtonStatuses" (v0_2DwarfXpButtonRecordDecoder buttonStatusDecoder)
            |> required "activeDailySpecials" (D.list (D.map2 Tuple.pair dailySpecialDecoder Utils.Timer.timerDecoder))
            |> required "dailySpecialOptions" (D.list dailySpecialDecoder)
            |> required "dailySpecialCooldown" buttonStatusDecoder
            |> required "minerals" mineralRecordDecoder
            |> optional "missionBiome" (D.map Just biomeDecoder) Nothing
            |> optional "projectLevels" (v0_3ProjectRecordDecoder D.int) (projectRecord 0)
        )


activeDailySpecialDecoder : Decoder ( DailySpecial, Utils.Timer.Timer )
activeDailySpecialDecoder =
    D.map2 Tuple.pair
        (D.field "dailySpecial" dailySpecialDecoder)
        (D.field "timer" Utils.Timer.timerDecoder)


dailySpecialDecoder : Decoder DailySpecial
dailySpecialDecoder =
    -- Just one of the daily specials by id
    D.string
        |> D.andThen
            (\dailySpecialId ->
                case List.Extra.find (\dailySpecial -> (dailySpecialStats dailySpecial).id_ == dailySpecialId) allDailySpecials of
                    Just dailySpecial ->
                        D.succeed dailySpecial

                    Nothing ->
                        D.fail ("Unknown daily special: " ++ dailySpecialId)
            )


mineralRecordDecoder : Decoder (MineralRecord Float)
mineralRecordDecoder =
    D.map6 MineralRecord
        (D.field "jadiz" D.float)
        (D.field "bismor" D.float)
        (D.field "enorPearl" D.float)
        (D.field "croppa" D.float)
        (D.field "magnite" D.float)
        (D.field "umanite" D.float)



-- Encoders


buttonStatusEncoder : ButtonStatus -> E.Value
buttonStatusEncoder status =
    case status of
        ButtonReady ->
            E.null

        ButtonOnCooldown timer ->
            Utils.Timer.timerEncoder timer


v0_2TabEncoder : Tab -> E.Value
v0_2TabEncoder tab =
    let
        stats : TabStats
        stats =
            Utils.Record.getByTab tab Config.tabStats
    in
    E.string stats.title


v0_2EncodeButtonStatus : ButtonStatus -> E.Value
v0_2EncodeButtonStatus status =
    case status of
        ButtonReady ->
            E.null

        ButtonOnCooldown timer ->
            Utils.Timer.timerEncoder timer


v0_2DwarfXpButtonsEncoder : DwarfXpButtonRecord ButtonStatus -> E.Value
v0_2DwarfXpButtonsEncoder buttons =
    E.object
        (allDwarfXpButtons
            |> List.map
                (\dwarfXpButton ->
                    let
                        stats : DwarfXpButtonStats
                        stats =
                            dwarfXpButtonStats dwarfXpButton

                        buttonStatus : ButtonStatus
                        buttonStatus =
                            getByDwarfXpButton buttons dwarfXpButton
                    in
                    ( stats.id_, v0_2EncodeButtonStatus buttonStatus )
                )
        )


v0_2ThemeEncoder : Maybe Theme -> E.Value
v0_2ThemeEncoder maybeTheme =
    case maybeTheme of
        Nothing ->
            E.null

        Just theme ->
            E.string (Theme.themeToString theme)


dailySpecialEncoder : DailySpecial -> E.Value
dailySpecialEncoder dailySpecial =
    E.string (dailySpecialStats dailySpecial).id_


encoder : Model -> E.Value
encoder model =
    E.object
        [ ( "v0.3"
          , E.object
                [ ( "currentTime", posixEncoder model.currentTime )
                , ( "currentTab", v0_2TabEncoder model.currentTab )
                , ( "theme", v0_2ThemeEncoder model.theme )
                , ( "level", E.int model.level )
                , ( "morkite", E.float model.morkite )
                , ( "missionStatuses"
                  , E.object
                        [ ( "haz1", buttonStatusEncoder model.missionStatuses.haz1 )
                        , ( "haz2", buttonStatusEncoder model.missionStatuses.haz2 )
                        , ( "haz3", buttonStatusEncoder model.missionStatuses.haz3 )
                        , ( "haz4", buttonStatusEncoder model.missionStatuses.haz4 )
                        , ( "haz5", buttonStatusEncoder model.missionStatuses.haz5 )
                        ]
                  )
                , ( "dwarfXpButtonStatuses"
                  , v0_2DwarfXpButtonsEncoder model.dwarfXpButtonStatuses
                  )
                , ( "dwarfXp"
                  , E.object
                        [ ( "scout", E.float (DwarfXp.toFloat model.dwarfXp.scout) )
                        , ( "gunner", E.float (DwarfXp.toFloat model.dwarfXp.gunner) )
                        , ( "engineer", E.float (DwarfXp.toFloat model.dwarfXp.engineer) )
                        , ( "driller", E.float (DwarfXp.toFloat model.dwarfXp.driller) )
                        ]
                  )
                , ( "activeDailySpecials"
                  , E.list
                        (\( dailySpecial, timer ) ->
                            E.object
                                [ ( "dailySpecial", E.string (dailySpecialStats dailySpecial).id_ )
                                , ( "timer", Utils.Timer.timerEncoder timer )
                                ]
                        )
                        model.activeDailySpecials
                  )
                , ( "dailySpecialCooldown", v0_2EncodeButtonStatus model.dailySpecialCooldown )
                , ( "dailySpecialOptions", E.list dailySpecialEncoder model.dailySpecialOptions )
                , ( "minerals"
                  , E.object
                        [ ( "jadiz", E.float model.minerals.jadiz )
                        , ( "bismor", E.float model.minerals.bismor )
                        , ( "enorPearl", E.float model.minerals.enorPearl )
                        , ( "croppa", E.float model.minerals.croppa )
                        , ( "magnite", E.float model.minerals.magnite )
                        , ( "umanite", E.float model.minerals.umanite )
                        ]
                  )
                , ( "missionBiome"
                  , case model.missionBiome of
                        Just biome ->
                            E.string (biomeStats biome).id_

                        Nothing ->
                            E.null
                  )
                , ( "projectLevels"
                  , E.object
                        [ ( "mule1", E.int model.projectLevels.mule1 )
                        , ( "mule2", E.int model.projectLevels.mule2 )
                        ]
                  )
                ]
          )
        ]


v0_3ProjectRecordDecoder : Decoder a -> Decoder (ProjectRecord a)
v0_3ProjectRecordDecoder d =
    D.map2
        (\mule1 mule2 ->
            { mule1 = mule1
            , mule2 = mule2
            }
        )
        (D.field "mule1" d)
        (D.field "mule2" d)


biomeDecoder : Decoder Biome
biomeDecoder =
    D.string
        |> D.andThen
            (\biomeId ->
                case List.Extra.find (\biome -> (biomeStats biome).id_ == biomeId) allBiomes of
                    Just biome ->
                        D.succeed biome

                    Nothing ->
                        D.fail ("Unknown biome: " ++ biomeId)
            )
