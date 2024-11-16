port module Main exposing (main)

import Browser
import Browser.Events
import Config
import Dict exposing (Dict)
import Duration exposing (Duration, hours)
import FeatherIcons
import Float.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Quantity
import Save
import Test.Html.Query exposing (has)
import Theme
import Time
import Types exposing (..)
import Utils.Percent exposing (Percent)
import Utils.Record
import Utils.Timer exposing (durationLeft, hasTickedAVeryShortTime)
import Utils.Unlocks


port saveGame : E.Value -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



{--| Returns the amount of credits required to level up-}


defaultModel : Time.Posix -> Model
defaultModel now =
    { currentTime = now
    , saveTimer = Utils.Timer.create
    , theme = Default
    , level = 1
    , credits = 0
    , resources = { gold = 0 }
    , missionStatuses =
        { haz1 = MissionComplete
        , haz2 = MissionComplete
        , haz3 = MissionComplete
        , haz4 = MissionComplete
        , haz5 = MissionComplete
        }
    , gameSpeed = 1.0
    , debugAddedTime = Quantity.zero
    , animations = []
    }


init : Flags -> ( Model, Cmd Msg )
init { now, initialGame } =
    case D.decodeValue Save.decodeAnyVersion initialGame of
        Err err ->
            ( defaultModel (Time.millisToPosix now)
            , Cmd.none
            )

        Ok model ->
            ( model, Cmd.none )


allMissions : List Mission
allMissions =
    [ Haz1, Haz2, Haz3, Haz4, Haz5 ]



-- UPDATE


animationDuration : Duration
animationDuration =
    Duration.seconds 2


updateStatus : Duration -> Mission -> MissionStatus -> MissionStatus
updateStatus delta mission status =
    case status of
        MissionComplete ->
            status

        MissionInProgress oldTimer ->
            let
                missionStats : MissionStats
                missionStats =
                    Utils.Record.getByMission mission Config.missionStats

                ( newTimer, completions ) =
                    Utils.Timer.increment missionStats.duration delta oldTimer

                newStatus : MissionStatus
                newStatus =
                    if completions > 0 then
                        MissionComplete

                    else
                        MissionInProgress newTimer
            in
            newStatus


addCredits : Float -> { a | credits : Float, level : Int } -> { a | credits : Float, level : Int }
addCredits amount model =
    case Config.levelingSchedule model.level of
        EarnCredits creditsToNextLevel ->
            let
                newTotalCredits : Float
                newTotalCredits =
                    model.credits + amount

                remainder : Float
                remainder =
                    newTotalCredits - creditsToNextLevel
            in
            if remainder < 0 then
                -- Not enough to level
                { model | credits = newTotalCredits }

            else
                -- Enough to level once or more
                addCredits remainder { model | credits = 0, level = model.level + 1 }

        AtMaxLevel ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noOp : ( Model, Cmd Msg )
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            noOp

        ResetGame ->
            ( defaultModel model.currentTime, Cmd.none )

        HandleAnimationFrame n ->
            let
                newCurrentTime : Time.Posix
                newCurrentTime =
                    Time.posixToMillis n
                        + floor (Duration.inMilliseconds model.debugAddedTime)
                        |> Time.millisToPosix

                delta : Duration
                delta =
                    Time.posixToMillis newCurrentTime
                        - Time.posixToMillis model.currentTime
                        |> toFloat
                        |> (\x -> x * model.gameSpeed)
                        |> Duration.milliseconds

                updateMissionStatuses : Mission -> MissionRecord MissionStatus -> MissionRecord MissionStatus
                updateMissionStatuses mission statuses =
                    Utils.Record.updateByMission mission (updateStatus delta mission) statuses

                timeBetweenSaves : Duration
                timeBetweenSaves =
                    Duration.seconds 1

                ( newSaveTimer, saveTimerCompletions ) =
                    Utils.Timer.increment timeBetweenSaves delta model.saveTimer

                saveGameMsg : Maybe (Cmd Msg)
                saveGameMsg =
                    if saveTimerCompletions >= 1 then
                        Just (saveGame (Save.v1Encoder model))

                    else
                        Nothing

                updateAnimation : Maybe Animation -> List (Maybe Animation) -> List (Maybe Animation)
                updateAnimation maybeAnimation animations =
                    case maybeAnimation of
                        Nothing ->
                            Nothing :: animations

                        Just (Animation timer duration location subject) ->
                            let
                                ( newTimer, completions ) =
                                    Utils.Timer.increment duration delta timer

                                newAnimation : Animation
                                newAnimation =
                                    Animation newTimer duration location subject
                            in
                            if completions > 0 then
                                Nothing :: animations

                            else
                                Just newAnimation :: animations

                newAnimations : List (Maybe Animation)
                newAnimations =
                    List.foldr updateAnimation [] model.animations
                        |> (\animations ->
                                -- We remove all the animations if at some point they're all finished (i.e. all are Nothing) because Nothing animations render as empty divs if not removed
                                -- We don't remove them if all animations aren't finished because I saw odd visual bugs when the List of animations would change what position an animation was at.
                                -- N.B. we also don't let there be more than 100 animations at once. This is purely speculative that this would matter but it seems prudent.
                                if List.length animations > 100 || List.Extra.unique animations == [ Nothing ] then
                                    []

                                else
                                    animations
                           )
            in
            ( { model
                | currentTime = newCurrentTime
                , missionStatuses = List.foldl updateMissionStatuses model.missionStatuses allMissions
                , saveTimer = newSaveTimer
                , animations = newAnimations
              }
            , Cmd.batch
                (List.filterMap identity
                    [ saveGameMsg
                    ]
                )
            )

        HandleStartMissionClick mission ->
            let
                newMissionStatuses : MissionRecord MissionStatus
                newMissionStatuses =
                    Utils.Record.setByMission mission (MissionInProgress Utils.Timer.create) model.missionStatuses
            in
            ( { model | missionStatuses = newMissionStatuses }, Cmd.none )

        HandleClaimCargoClick mission event ->
            let
                stats : MissionStats
                stats =
                    Utils.Record.getByMission mission Config.missionStats

                newMissionStatuses : MissionRecord MissionStatus
                newMissionStatuses =
                    Utils.Record.setByMission mission (MissionInProgress Utils.Timer.create) model.missionStatuses

                newResources : ResourceRecord Int
                newResources =
                    let
                        yield : MissionYield
                        yield =
                            (Utils.Record.getByMission mission Config.missionStats).yield
                    in
                    Utils.Record.addResourceRecords yield.resources model.resources

                addCreditsResult : Model
                addCreditsResult =
                    addCredits stats.yield.credits model

                adjustClientPos : ( Float, Float ) -> ( Float, Float )
                adjustClientPos ( x, y ) =
                    ( x + 50, y - 25 )

                newAnimation : Animation
                newAnimation =
                    Animation Utils.Timer.create animationDuration (adjustClientPos event.pointer.clientPos) (AnimateCreditsGain stats.yield.credits)
            in
            ( { model
                | missionStatuses = newMissionStatuses
                , resources = newResources
                , credits = addCreditsResult.credits
                , level = addCreditsResult.level
                , animations = List.append model.animations [ Just newAnimation ]
              }
            , Cmd.none
            )

        HandleSetThemeClick theme ->
            ( { model | theme = theme }, Cmd.none )

        DebugSetGameSpeed speed ->
            ( { model | gameSpeed = speed }, Cmd.none )

        DebugAdvanceTime duration ->
            ( { model | debugAddedTime = Quantity.plus model.debugAddedTime duration }, Cmd.none )

        DebugGainLevel ->
            ( { model | level = model.level + 1 }, Cmd.none )

        DebugLevelToMax ->
            let
                maxLevel : Int
                maxLevel =
                    -- Get highest level key from levelUnlockStats
                    Config.levelUnlockStats
                        |> Dict.keys
                        |> List.maximum
                        |> Maybe.withDefault 1
            in
            ( { model | level = maxLevel }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame HandleAnimationFrame
        ]



-- VIEW


proseClass : Attribute Msg
proseClass =
    class "prose prose-sm md:prose-base"


renderDuration : Duration -> Bool -> Html Msg
renderDuration d hasTickedAVeryShortTime =
    let
        totalDurationMoreThanMinute : Bool
        totalDurationMoreThanMinute =
            Quantity.greaterThan Duration.minute d

        duration =
            if hasTickedAVeryShortTime && totalDurationMoreThanMinute then
                Quantity.plus d Duration.second

            else
                d

        hours : Int
        hours =
            floor (Duration.inHours duration)

        minutes : Int
        minutes =
            floor (Duration.inMinutes duration)
                |> modBy 60

        seconds : Int
        seconds =
            if not totalDurationMoreThanMinute && hasTickedAVeryShortTime then
                60

            else
                floor (Duration.inSeconds duration)
                    |> modBy 60

        renderTimeSegment : Int -> String -> Html Msg
        renderTimeSegment value unit =
            div [ class "flex flex-col" ]
                [ span [ class "countdown font-mono text-2xl" ]
                    [ span [ attribute "style" ("--value:" ++ String.fromInt value) ] [] ]
                , text unit
                ]

        wrapper : List (Html Msg) -> Html Msg
        wrapper =
            div [ class "grid auto-cols-max grid-flow-col gap-3 text-center" ]

        hide : List (Html Msg) -> Html Msg
        hide =
            div [ class "invisible" ]
    in
    if hours > 0 then
        wrapper
            [ renderTimeSegment hours "hours"
            , renderTimeSegment minutes "min"
            , renderTimeSegment seconds "sec"
            ]

    else if minutes > 0 then
        wrapper
            [ hide [ renderTimeSegment hours "hours" ]
            , renderTimeSegment minutes "min"
            , renderTimeSegment seconds "sec"
            ]

    else
        wrapper
            [ hide [ renderTimeSegment hours "hours" ]
            , hide [ renderTimeSegment minutes "min" ]
            , renderTimeSegment seconds "sec"
            ]


credits3Img : Html Msg
credits3Img =
    img [ src "credits3.png", class "w-6 inline-block" ] []


credits1Img : Html Msg
credits1Img =
    img [ src "credits1.png", class "w-6 inline-block" ] []


renderMissionRow : Model -> Mission -> Html Msg
renderMissionRow model mission =
    let
        stats : MissionStats
        stats =
            Utils.Record.getByMission mission Config.missionStats

        missionStatus : MissionStatus
        missionStatus =
            Utils.Record.getByMission mission model.missionStatuses

        yield : MissionYield
        yield =
            stats.yield

        icon : String -> Html Msg
        icon iconSrc =
            img [ src iconSrc, class "w-6 inline" ] []

        icons : Html Msg
        icons =
            case mission of
                Haz1 ->
                    div [ class "flex items-center" ]
                        [ icon "haz1.png" ]

                Haz2 ->
                    div [ class "flex items-center space-x-[-8px]" ]
                        [ icon "haz1.png"
                        , icon "haz2.png"
                        ]

                Haz3 ->
                    div [ class "flex items-center space-x-[-8px]" ]
                        [ icon "haz1.png"
                        , icon "haz2.png"
                        , icon "haz3.png"
                        ]

                Haz4 ->
                    div [ class "flex items-center space-x-[-8px]" ]
                        [ icon "haz1.png"
                        , icon "haz2.png"
                        , icon "haz3.png"
                        , icon "haz4.png"
                        ]

                Haz5 ->
                    div [ class "flex items-center space-x-[-8px]" ]
                        [ icon "haz1.png"
                        , icon "haz2.png"
                        , icon "haz3.png"
                        , icon "haz4.png"
                        , icon "haz5.png"
                        ]
    in
    tr [ class "h-20 relative" ]
        [ td []
            [ div [ class "flex items-center gap-1" ]
                [ icons
                ]
            ]

        -- , td [ class "flex h-20 items-center gap-1" ] [ creditsImg, span [] [ text (floatToString yield.credits), text "m" ] ]
        , td [ class "overflow-hidden relative" ]
            [ case missionStatus of
                MissionComplete ->
                    div [ class "flex items-center gap-8" ]
                        [ div [ class "relative" ]
                            [ button
                                [ class "px-4 py-4 bg-primary text-primary-content rounded-xl shadow animate-fade-in flex items-center gap-2 w-[200px] justify-center"
                                , class "absolute top-1/2 transform -translate-y-1/2"
                                , Pointer.onDown (HandleClaimCargoClick mission)
                                ]
                                [ text "Gain "
                                , span [] [ text " ", text (floatToString yield.credits), text "m credits" ]
                                , credits3Img
                                ]
                            ]
                        ]

                MissionInProgress timer ->
                    let
                        durationLeft : Duration
                        durationLeft =
                            Utils.Timer.durationLeft stats.duration timer

                        hasTickedAVeryShortTime : Bool
                        hasTickedAVeryShortTime =
                            Utils.Timer.hasTickedAVeryShortTime stats.duration timer
                    in
                    div [ class "flex items-center gap-8" ] [ renderDuration durationLeft hasTickedAVeryShortTime ]
            ]
        ]


renderGameSpeedButton : Model -> Float -> Html Msg
renderGameSpeedButton model speed =
    button
        [ class "btn btn-square"
        , onClick (DebugSetGameSpeed speed)
        , classList [ ( "btn-primary", model.gameSpeed == speed ) ]
        ]
        [ text (String.fromFloat speed ++ "x") ]


floatToString : Float -> String
floatToString input =
    input
        |> Float.Extra.toFixedDecimalPlaces 2
        |> String.toList
        |> List.Extra.dropWhileRight ((==) '0')
        |> List.Extra.dropWhileRight ((==) '.')
        |> String.fromList


renderProgressBar : Model -> Html Msg
renderProgressBar model =
    let
        filledPortion : Percent -> Html Msg
        filledPortion percentComplete =
            div
                [ class "absolute left-0 top-0 h-full bg-primary/25 themed-rounded-borders"
                , classList [ ( "border-r", Utils.Percent.toFloat percentComplete /= 1.0 && Utils.Percent.toFloat percentComplete /= 0.0 ) ]
                , style "width" (String.fromFloat (Basics.min 100 (Utils.Percent.toPercentage percentComplete)) ++ "%")
                , id "xp-bar" -- We attach the animation using this id
                ]
                []

        outerBarClasses : Attribute Msg
        outerBarClasses =
            class "w-full h-12 border border-neutral relative flex items-center justify-center gap-1 overflow-hidden themed-rounded-borders"
    in
    case Config.levelingSchedule model.level of
        AtMaxLevel ->
            div [ outerBarClasses ]
                [ span [ class "z-10 uppercase" ] [ text "max level reached" ]
                , filledPortion (Utils.Percent.float 1.0)
                ]

        EarnCredits creditsToNextLevel ->
            let
                percentComplete : Utils.Percent.Percent
                percentComplete =
                    Utils.Percent.float (model.credits / creditsToNextLevel)
            in
            div [ outerBarClasses ]
                [ span [ class "z-10 relative text-3xl flex items-center gap-2 leading-none text-xl" ]
                    [ span [ class "font-semibold text-3xl leading-none font-mono mr-2" ] [ text (floatToString model.credits ++ " / " ++ floatToString creditsToNextLevel) ]
                    , text "credits"
                    , span [ class "flex items-center" ] [ credits1Img ]
                    , text "to next level"
                    ]
                , filledPortion percentComplete
                ]


renderNextUnlock : Model -> Int -> LevelUnlockStats -> Html Msg
renderNextUnlock model unlockLevel unlockStats =
    let
        unlockCategoryString : String
        unlockCategoryString =
            case unlockStats.category of
                UnlockFeature ->
                    "Feature"

                UnlockActivity ->
                    "Activity"

                UnlockCosmetic ->
                    "Cosmetic"
    in
    div [ class "absolute left-0 bottom-[-8px] flex-col items-start text-lg" ]
        [ span []
            [ text "Unlock new "
            , strong [ class "text-primary" ] [ text unlockCategoryString ]
            , span [] [ text (" at level " ++ String.fromInt unlockLevel) ]
            ]
        ]


renderHeader : Model -> Html Msg
renderHeader model =
    div [ class "w-full min-w-full bg-base-200 flex items-center justify-center p-4" ]
        [ div [ class "flex flex-col items-center w-full min-w-full gap-4" ]
            [ label [] [ text "Department level" ]
            , div [ class "w-full flex justify-center items-center relative" ]
                [ h1 [ class "text-4xl font-extrabold relative inline-block" ]
                    [ text ("Level " ++ String.fromInt model.level)
                    , button [ class "absolute top-0 right-0 -mr-8 opacity-25 mt-1/2 btn btn-xs btn-square btn-ghost", classList [ ( "hidden", Config.env /= Config.Dev ) ], onClick DebugLevelToMax ]
                        [ FeatherIcons.arrowUpCircle
                            |> FeatherIcons.withSize 20
                            |> FeatherIcons.toHtml []
                        ]
                    , button [ class "absolute top-0 right-0 -mr-8 mt-6 opacity-25 mt-1/2 btn btn-xs btn-square btn-ghost", classList [ ( "hidden", Config.env /= Config.Dev ) ], onClick DebugGainLevel ]
                        [ FeatherIcons.plus
                            |> FeatherIcons.withSize 20
                            |> FeatherIcons.toHtml []
                        ]
                    ]
                , case Utils.Unlocks.nextUnlock model.level Config.levelUnlockStats of
                    Nothing ->
                        div [] []

                    Just ( unlockLevel, unlockStats ) ->
                        renderNextUnlock model unlockLevel unlockStats
                ]
            , renderProgressBar model
            ]
        ]


renderAnimation : Maybe Animation -> Html Msg
renderAnimation maybeAnimation =
    case maybeAnimation of
        Nothing ->
            div [] []

        Just (Animation _ _ ( x, y ) subject) ->
            let
                content : Html Msg
                content =
                    case subject of
                        AnimateCreditsGain amount ->
                            div [ class "flex items-center gap-1" ] [ text "+ ", text " ", text (floatToString amount), text " credits", credits1Img ]
            in
            div
                [ class "fixed bg-opacity-0 text-primary drop-shadow-[0_1.2px_1.2px_rgba(0,0,0,0.8)] font-extrabold text-xl animate-smokeRise pointer-events-none"
                , style "left" (String.fromFloat x ++ "px")
                , style "top" (String.fromFloat y ++ "px")
                ]
                [ content ]


view : Model -> Html Msg
view model =
    let
        unlockedMissions : List Mission
        unlockedMissions =
            List.filter (Utils.Unlocks.missionIsUnlocked model.level) allMissions
    in
    div [ class "w-screen h-screen overflow-hidden flex flex-col items-center bg-base-100" ]
        [ -- Header
          renderHeader model

        -- Body
        , div [ class "flex flex-col items-center gap-8 p-8" ]
            [ div [ proseClass ]
                [ h2 [] [ text "Missions" ]
                ]
            , table [ class "table table-fixed w-[750px]" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Hazard Level" ]
                        , th [] [ text "Status" ]
                        ]
                    ]
                , tbody []
                    (List.map (renderMissionRow model) unlockedMissions)
                ]
            , div [ class "fixed bottom-0 left-0 ml-6 mb-6" ] [ Theme.renderThemeDropdown model model.theme ]

            -- Game speed controls
            , div
                [ class "fixed bottom-0 mb-4 left-[50%] transform -translate-x-1/2 flex items-center gap-6"
                , classList [ ( "hidden", not Config.isDev ) ]
                ]
                [ button [ class "btn", onClick (DebugAdvanceTime (Duration.hours 1)) ] [ text "+1 hour" ]
                , button [ class "btn", onClick (DebugAdvanceTime (Duration.hours 6)) ] [ text "+6 hours" ]
                , button [ class "btn", onClick (DebugAdvanceTime (Duration.hours 12)) ] [ text "+12 hours" ]
                , button [ class "btn", onClick (DebugAdvanceTime (Duration.hours 24)) ] [ text "+24 hours" ]
                , button [ class "btn btn-error", onClick ResetGame ] [ text "Reset" ]

                -- [ renderGameSpeedButton model 1.0
                -- , renderGameSpeedButton model 2.0
                -- , renderGameSpeedButton model 8.0
                -- , renderGameSpeedButton model 60.0
                -- , renderGameSpeedButton model 3600.0
                ]
            ]
        , div []
            (List.map renderAnimation model.animations)
        ]
