module Save exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra
import Json.Encode as E
import Json.Encode.Extra
import Quantity
import Theme
import Time
import Types exposing (..)
import Utils.Percent
import Utils.Timer



-- Versions


decodeAnyVersion : Decoder Model
decodeAnyVersion =
    D.oneOf
        [ v1Decoder
        ]



-- Helpers


posixDecoder : Decoder Time.Posix
posixDecoder =
    D.map Time.millisToPosix D.int


posixEncoder : Time.Posix -> E.Value
posixEncoder posix =
    E.int (Time.posixToMillis posix)



-- V1


v1ResourcesDecoder : Decoder (ResourceRecord Int)
v1ResourcesDecoder =
    D.map ResourceRecord <|
        D.field "gold" D.int


percentDecoder : Decoder Utils.Percent.Percent
percentDecoder =
    D.field "current" <|
        D.map Utils.Percent.float D.float


timerDecoder : Decoder Utils.Timer.Timer
timerDecoder =
    D.field "current" <|
        D.map Utils.Timer.createAtPercent percentDecoder


v1MissionStatusesDecoder : Decoder (MissionRecord MissionStatus)
v1MissionStatusesDecoder =
    let
        missionStatusDecoder : Decoder MissionStatus
        missionStatusDecoder =
            D.oneOf
                [ D.null MissionComplete
                , D.map MissionInProgress timerDecoder
                ]
    in
    D.map5 MissionRecord
        (D.field "haz1" missionStatusDecoder)
        (D.field "haz2" missionStatusDecoder)
        (D.field "haz3" missionStatusDecoder)
        (D.field "haz4" missionStatusDecoder)
        (D.field "haz5" missionStatusDecoder)


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


v1Decoder : Decoder Model
v1Decoder =
    D.map6
        (\currentTime theme level credits resources missionStatuses ->
            let
                model : Model
                model =
                    { currentTime = currentTime
                    , theme = theme
                    , level = level
                    , credits = credits
                    , resources = resources
                    , missionStatuses = missionStatuses
                    , saveTimer = Utils.Timer.create
                    , gameSpeed = 1
                    , debugAddedTime = Quantity.zero
                    }
            in
            model
        )
        (D.field "currentTime" posixDecoder)
        (D.oneOf
            [ D.field "theme" themeDecoder
            , D.succeed Default
            ]
        )
        (D.field "level" D.int)
        (D.field "credits" D.float)
        (D.field "resources" v1ResourcesDecoder)
        (D.field "missionStatuses" v1MissionStatusesDecoder)


percentEncoder : Utils.Percent.Percent -> E.Value
percentEncoder percent =
    E.float (Utils.Percent.toFloat percent)


timerEncoder : Utils.Timer.Timer -> E.Value
timerEncoder timer =
    E.object
        [ ( "current", percentEncoder (Utils.Timer.percentComplete timer) )
        ]


v1MissionStatusEncoder : MissionStatus -> E.Value
v1MissionStatusEncoder status =
    case status of
        MissionComplete ->
            E.null

        MissionInProgress timer ->
            E.object
                [ ( "current", timerEncoder timer )
                ]


v1Encoder : Model -> E.Value
v1Encoder model =
    E.object
        [ ( "version", E.string "v1" )
        , ( "currentTime", posixEncoder model.currentTime )
        , ( "level", E.int model.level )
        , ( "credits", E.float model.credits )
        , ( "resources", E.object [ ( "gold", E.int model.resources.gold ) ] )
        , ( "missionStatuses"
          , E.object
                [ ( "haz1", v1MissionStatusEncoder model.missionStatuses.haz1 )
                , ( "haz2", v1MissionStatusEncoder model.missionStatuses.haz2 )
                , ( "haz3", v1MissionStatusEncoder model.missionStatuses.haz3 )
                , ( "haz4", v1MissionStatusEncoder model.missionStatuses.haz4 )
                , ( "haz5", v1MissionStatusEncoder model.missionStatuses.haz5 )
                ]
          )
        ]
