port module Main exposing (defaultModel, main)

import AssocList as Dict exposing (Dict)
import Browser
import Browser.Events
import Config
import Duration exposing (Duration, hours)
import DwarfXp exposing (DwarfXp)
import FeatherIcons
import Float.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Quantity exposing (Quantity(..))
import Random
import Random.List
import Save
import Test.Html.Query exposing (has)
import Theme
import Time
import Types exposing (..)
import Utils.Percent exposing (Percent)
import Utils.Record
import Utils.Timer exposing (Timer, durationLeft, hasTickedAVeryShortTime)
import Utils.Unlocks


port saveGame : E.Value -> Cmd msg


port closePopover : String -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



{--| Returns the amount of credits required to level up-}


dailySpecialOptionsGenerator : Random.Generator (List DailySpecial)
dailySpecialOptionsGenerator =
    Random.List.shuffle allDailySpecials
        |> Random.map (List.take 2)


defaultModel : Random.Seed -> Time.Posix -> Model
defaultModel seed1 now =
    let
        ( dailySpecialOptions, seed2 ) =
            Random.step dailySpecialOptionsGenerator seed1
    in
    { seed = seed2
    , debugSettings = Config.defaultDebugSettings
    , currentTime = now
    , currentTab = MissionsTab
    , saveTimer = Utils.Timer.create
    , theme = Nothing
    , level = 1
    , credits = 0
    , missionStatuses =
        { haz1 = ButtonReady
        , haz2 = ButtonReady
        , haz3 = ButtonReady
        , haz4 = ButtonReady
        , haz5 = ButtonReady
        }
    , dwarfXp = Utils.Record.dwarfRecord (DwarfXp.float 0)
    , dwarfXpButtonStatuses = dwarfXpButtonRecord ButtonReady
    , activeDailySpecials = []
    , dailySpecialCooldown = ButtonReady
    , dailySpecialOptions = dailySpecialOptions
    , maybeInitDecodeErr = Nothing
    , minerals = mineralRecord 0
    , missionBiome = Nothing
    }


init : Flags -> ( Model, Cmd Msg )
init { initialSeed, now, initialGame } =
    let
        randomSeed : Random.Seed
        randomSeed =
            Random.initialSeed initialSeed
    in
    case D.decodeValue (Save.decodeAnyVersion randomSeed) initialGame of
        Err err ->
            let
                model1 : Model
                model1 =
                    defaultModel randomSeed (Time.millisToPosix now)
            in
            ( { model1 | maybeInitDecodeErr = Just err }, Cmd.none )

        Ok model ->
            ( model, Cmd.none )



-- UPDATE


updateButton : Duration -> Duration -> ButtonStatus -> ButtonStatus
updateButton delta buttonDuration status =
    case status of
        ButtonReady ->
            status

        ButtonOnCooldown oldTimer ->
            let
                ( newTimer, completions ) =
                    Utils.Timer.increment buttonDuration delta oldTimer

                newStatus : ButtonStatus
                newStatus =
                    if completions > 0 then
                        ButtonReady

                    else
                        ButtonOnCooldown newTimer
            in
            newStatus


{-| A list of for each level how much xp is required to reach the next level.
We should normally use Config.dwarfLevelingSchedule if you need the xp for a given level, but having it in order in a List helps with a lot of other utility functions in this file.
-}
dwarfXpTable : List ( Int, DwarfXp )
dwarfXpTable =
    let
        helper : Int -> List ( Int, DwarfXp ) -> List ( Int, DwarfXp )
        helper lvl accum =
            case Config.dwarfLevelingSchedule lvl of
                Ok (EarnDwarfXp xpToNextLevel) ->
                    helper (lvl + 1) (accum ++ [ ( lvl, DwarfXp.float xpToNextLevel ) ])

                _ ->
                    accum
    in
    helper 1 []


dwarfMaxLevel : Int
dwarfMaxLevel =
    dwarfXpTable
        |> List.Extra.last
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 1


dwarfLevel : DwarfXp -> Int
dwarfLevel xp =
    let
        helper : Int -> DwarfXp -> List ( Int, DwarfXp ) -> Int
        helper lvl (Quantity remainingXp) remainingLevels =
            case remainingLevels of
                [] ->
                    lvl

                ( _, Quantity xpToNextLevel ) :: rest ->
                    if remainingXp < xpToNextLevel then
                        lvl

                    else
                        helper (lvl + 1) (DwarfXp.float (remainingXp - xpToNextLevel)) rest
    in
    helper 1 xp dwarfXpTable


xpToLevel : Int -> DwarfXp
xpToLevel lvl =
    List.Extra.dropWhileRight (\( l, _ ) -> l >= lvl) dwarfXpTable
        |> List.foldl (\( _, Quantity xp ) acc -> Quantity.plus acc (DwarfXp.float xp)) (DwarfXp.float 0)


flatXpInCurrentLevel : DwarfXp -> DwarfXp
flatXpInCurrentLevel xp =
    let
        currentLevel : Int
        currentLevel =
            dwarfLevel xp

        xpInCurrentLevel : DwarfXp
        xpInCurrentLevel =
            Quantity.difference xp (xpToLevel currentLevel)
    in
    xpInCurrentLevel


percentInLevel : DwarfXp -> Percent
percentInLevel xp =
    let
        currentLevel : Int
        currentLevel =
            dwarfLevel xp

        xpInCurrentLevel : DwarfXp
        xpInCurrentLevel =
            flatXpInCurrentLevel xp
    in
    case Config.dwarfLevelingSchedule currentLevel of
        Ok (EarnDwarfXp total) ->
            Utils.Percent.float (DwarfXp.toFloat xpInCurrentLevel / total)

        _ ->
            Utils.Percent.float 1


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


adjustClientPos : ( Float, Float ) -> ( Float, Float )
adjustClientPos ( x, y ) =
    -- This is about how much we need to adjust the client position to put the animation where we want relative to the pointer
    ( x + 50, y - 25 )


setButtonCooldown : Model -> ButtonStatus
setButtonCooldown model =
    if model.debugSettings.buttonCooldownInstant then
        ButtonReady

    else
        ButtonOnCooldown Utils.Timer.create


getAllBuffs : Model -> List Buff
getAllBuffs model =
    let
        squadBonusPercent : Percent
        squadBonusPercent =
            squadBonus model

        squadMods : List Buff
        squadMods =
            if Quantity.greaterThan Quantity.zero squadBonusPercent then
                [ { title = "Squad bonus", icon = "engineer.webp", description = "Increase the yield of missions", mod = ModMissionYield squadBonusPercent } ]

            else
                []

        dailySpecialMods : List Buff
        dailySpecialMods =
            List.map (\( dailySpecial, _ ) -> (dailySpecialStats dailySpecial).buff) model.activeDailySpecials
    in
    List.concat [ squadMods, dailySpecialMods ]


getAllMods : Model -> List Mod
getAllMods model =
    List.map .mod (getAllBuffs model)


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
            ( defaultModel model.seed model.currentTime, Cmd.none )

        HandleAnimationFrame n ->
            let
                newCurrentTime : Time.Posix
                newCurrentTime =
                    Time.posixToMillis n
                        + floor (Duration.inMilliseconds model.debugSettings.addedTime)
                        |> Time.millisToPosix

                delta : Duration
                delta =
                    Time.posixToMillis newCurrentTime
                        - Time.posixToMillis model.currentTime
                        |> toFloat
                        |> (\x -> x * model.debugSettings.gameSpeed)
                        |> Duration.milliseconds

                updateMissionStatuses : Mission -> MissionRecord ButtonStatus -> MissionRecord ButtonStatus
                updateMissionStatuses mission statuses =
                    let
                        buttonDuration : Duration
                        buttonDuration =
                            (Utils.Record.getByMission mission Config.missionStats).duration
                    in
                    Utils.Record.updateByMission mission (updateButton delta buttonDuration) statuses

                updateDwarfXpButtonStatuses : DwarfXpButton -> DwarfXpButtonRecord ButtonStatus -> DwarfXpButtonRecord ButtonStatus
                updateDwarfXpButtonStatuses dwarfXpButton statuses =
                    let
                        buttonDuration : Duration
                        buttonDuration =
                            (dwarfXpButtonStats dwarfXpButton).duration
                    in
                    updateByDwarfXpButton (updateButton delta buttonDuration) statuses dwarfXpButton

                newDailySpecialCooldown : ButtonStatus
                newDailySpecialCooldown =
                    updateButton delta Config.dailySpecialCooldown model.dailySpecialCooldown

                newDailySpecials : List ( DailySpecial, Timer )
                newDailySpecials =
                    -- Iterate over all daily specials, calculate the updated timer, and if it has completed remove the daily special from the list
                    List.filterMap
                        (\( dailySpecial, timer ) ->
                            let
                                ( newTimer, completions ) =
                                    Utils.Timer.increment Config.dailySpecialBuffDuration delta timer
                            in
                            if completions > 0 then
                                Nothing

                            else
                                Just ( dailySpecial, newTimer )
                        )
                        model.activeDailySpecials

                timeBetweenSaves : Duration
                timeBetweenSaves =
                    Duration.seconds 1

                ( newSaveTimer, saveTimerCompletions ) =
                    Utils.Timer.increment timeBetweenSaves delta model.saveTimer

                saveGameMsg : Maybe (Cmd Msg)
                saveGameMsg =
                    if saveTimerCompletions >= 1 then
                        Just (saveGame (Save.encoder model))

                    else
                        Nothing
            in
            ( { model
                | currentTime = newCurrentTime
                , missionStatuses = List.foldl updateMissionStatuses model.missionStatuses Utils.Record.allMissions
                , dwarfXpButtonStatuses = List.foldl updateDwarfXpButtonStatuses model.dwarfXpButtonStatuses allDwarfXpButtons
                , saveTimer = newSaveTimer
                , activeDailySpecials = newDailySpecials
                , dailySpecialCooldown = newDailySpecialCooldown
              }
            , Cmd.batch
                (List.filterMap identity
                    [ saveGameMsg
                    ]
                )
            )

        HandleMissionClick mission event ->
            let
                stats : MissionStats
                stats =
                    Utils.Record.getByMission mission Config.missionStats

                yield : MissionYield
                yield =
                    { credits = stats.credits, minerals = Dict.empty }

                modifiedYield : MissionYield
                modifiedYield =
                    modifyYield model yield

                newMissionStatuses : MissionRecord ButtonStatus
                newMissionStatuses =
                    Utils.Record.setByMission mission (setButtonCooldown model) model.missionStatuses

                addCreditsResult : Model
                addCreditsResult =
                    addCredits modifiedYield.credits model
            in
            ( { model
                | missionStatuses = newMissionStatuses
                , credits = addCreditsResult.credits
                , level = addCreditsResult.level
              }
            , Cmd.none
            )

        HandleDwarfXpButtonClick dwarfXpButton event ->
            let
                stats : DwarfXpButtonStats
                stats =
                    dwarfXpButtonStats dwarfXpButton

                newStatuses : DwarfXpButtonRecord ButtonStatus
                newStatuses =
                    setByDwarfXpButton (setButtonCooldown model) dwarfXpButton model.dwarfXpButtonStatuses

                ( ( dwarf, newDwarfXp ), newSeed ) =
                    Random.step (dwarfXpGenerator stats.xp model.dwarfXp) model.seed

                currentLevel : Int
                currentLevel =
                    dwarfLevel (Utils.Record.getByDwarf dwarf model.dwarfXp)

                newLevel : Int
                newLevel =
                    dwarfLevel (Utils.Record.getByDwarf dwarf newDwarfXp)
            in
            ( { model
                | dwarfXpButtonStatuses = newStatuses
                , dwarfXp = newDwarfXp
                , seed = newSeed
              }
            , Cmd.none
            )

        HandleSetThemeClick theme ->
            ( { model | theme = Just theme }, Cmd.none )

        DebugSetGameSpeed speed ->
            let
                debugSettings : DebugSettings
                debugSettings =
                    model.debugSettings
            in
            ( { model | debugSettings = { debugSettings | gameSpeed = speed } }, Cmd.none )

        DebugAdvanceTime duration ->
            let
                debugSettings : DebugSettings
                debugSettings =
                    model.debugSettings
            in
            ( { model | debugSettings = { debugSettings | addedTime = Quantity.plus debugSettings.addedTime duration } }, Cmd.none )

        DebugSetButtonCooldownInstant newVal ->
            let
                debugSettings : DebugSettings
                debugSettings =
                    model.debugSettings
            in
            ( { model | debugSettings = { debugSettings | buttonCooldownInstant = newVal } }, Cmd.none )

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
            ( { model | level = dwarfMaxLevel }, Cmd.none )

        HandleTabClick tab ->
            ( { model | currentTab = tab }, Cmd.none )

        HandleDailySpecialClick dailySpecial ->
            let
                ( newDailySpecialOptions, seed2 ) =
                    Random.step dailySpecialOptionsGenerator model.seed
            in
            ( { model
                | activeDailySpecials = ( dailySpecial, Utils.Timer.create ) :: model.activeDailySpecials
                , dailySpecialCooldown = ButtonOnCooldown Utils.Timer.create
                , seed = seed2
                , dailySpecialOptions = newDailySpecialOptions
              }
            , Cmd.none
            )

        HandleMissionBiomeSelection biome ->
            ( { model | missionBiome = Just biome }
            , closePopover "popover-1"
            )


modifyYield : Model -> MissionYield -> MissionYield
modifyYield model yield =
    let
        yieldBonus : Percent
        yieldBonus =
            getYieldBonus (getAllMods model)
    in
    { yield | credits = yield.credits * (1 + Utils.Percent.toFloat yieldBonus) }


dwarfXpGenerator : DwarfXp -> DwarfRecord DwarfXp -> Random.Generator ( Dwarf, DwarfRecord DwarfXp )
dwarfXpGenerator xp dwarfXp =
    Random.uniform Scout Utils.Record.allDwarfs
        |> Random.map
            (\dwarf ->
                let
                    oldXp : DwarfXp
                    oldXp =
                        Utils.Record.getByDwarf dwarf dwarfXp

                    newXp : DwarfXp
                    newXp =
                        Quantity.plus oldXp xp
                in
                ( dwarf, Utils.Record.setByDwarf dwarf newXp dwarfXp )
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame HandleAnimationFrame
        ]



-- VIEW


proseClass : Attribute Msg
proseClass =
    class "prose"


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
            div [ class "flex w-full gap-3 text-center justify-end" ]

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


credits1Img : Html Msg
credits1Img =
    img [ src "credits1.png", class "w-6 inline-block" ] []


renderMissionRow : Model -> Mission -> Html Msg
renderMissionRow model mission =
    let
        stats : MissionStats
        stats =
            Utils.Record.getByMission mission Config.missionStats

        missionStatus : ButtonStatus
        missionStatus =
            Utils.Record.getByMission mission model.missionStatuses

        icon : String -> Html Msg
        icon iconSrc =
            img [ src iconSrc, class "w-4 inline" ] []

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

        buttonText : String
        buttonText =
            case missionStatus of
                ButtonReady ->
                    "Gain " ++ floatToFixedDecimalString stats.credits 2 ++ "m credits"

                ButtonOnCooldown _ ->
                    "On cooldown"
    in
    tr []
        [ td [ class "h-[70px]" ]
            [ div [ class "flex h-full items-center gap-2" ]
                (List.concat
                    [ [ span [] [ text stats.title ] ]
                    , [ icons ]
                    ]
                )
            ]
        , td [ class "overflow-hidden relative flex justify-end items-center h-[70px]" ]
            [ renderButton model missionStatus stats.duration (HandleMissionClick mission) ButtonPrimary [ text buttonText ] ]
        ]


type ButtonVariant
    = ButtonPrimary
    | ButtonSecondary


renderButton : Model -> ButtonStatus -> Duration -> (Pointer.Event -> Msg) -> ButtonVariant -> List (Html Msg) -> Html Msg
renderButton model buttonStatus buttonDuration msg variant children =
    case buttonStatus of
        ButtonReady ->
            let
                buttonVariantClass : Attribute Msg
                buttonVariantClass =
                    case variant of
                        ButtonPrimary ->
                            class "btn-primary"

                        ButtonSecondary ->
                            class "btn-secondary"
            in
            div [ class "flex items-center justify-end gap-8 w-full" ]
                [ div [ class "relative inline-block" ]
                    [ button
                        [ class "btn"
                        , buttonVariantClass
                        , Pointer.onUp msg
                        ]
                        children
                    ]
                ]

        ButtonOnCooldown timer ->
            let
                durationLeft : Duration
                durationLeft =
                    Utils.Timer.durationLeft buttonDuration timer

                hasTickedAVeryShortTime : Bool
                hasTickedAVeryShortTime =
                    Utils.Timer.hasTickedAVeryShortTime buttonDuration timer
            in
            div [ class "flex items-end gap-8 w-full h-full" ] [ renderDuration durationLeft hasTickedAVeryShortTime ]


renderGameSpeedButton : Model -> Float -> Html Msg
renderGameSpeedButton model speed =
    button
        [ class "btn btn-square"
        , onClick (DebugSetGameSpeed speed)
        , classList [ ( "btn-primary", model.debugSettings.gameSpeed == speed ) ]
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


floatToFixedDecimalString : Float -> Int -> String
floatToFixedDecimalString input decimalPlaces =
    input
        |> Float.Extra.toFixedDecimalPlaces decimalPlaces
        |> String.toList
        |> String.fromList


renderProgressBar : Model -> Html Msg
renderProgressBar model =
    let
        filledPortion : Percent -> Html Msg
        filledPortion percentComplete =
            div
                [ class "absolute left-0 top-0 h-full bg-primary themed-rounded-borders"
                , classList [ ( "border-r", Utils.Percent.toFloat percentComplete /= 1.0 && Utils.Percent.toFloat percentComplete /= 0.0 ) ]
                , style "width" (String.fromFloat (Basics.min 100 (Utils.Percent.toPercentage percentComplete)) ++ "%")
                , id "xp-bar" -- We attach the animation using this id
                ]
                []

        outerBarClasses : Attribute Msg
        outerBarClasses =
            class "w-full h-12 border border-neutral relative flex items-center justify-center gap-1 overflow-hidden themed-rounded-borders"

        progressBarTextClass : Attribute Msg
        progressBarTextClass =
            class "px-3 py-1 rounded-sm bg-base-100 text-base-content text-3xl leading-none text-xl"
    in
    case Config.levelingSchedule model.level of
        AtMaxLevel ->
            div [ outerBarClasses ]
                [ span [ class "z-10 uppercase", progressBarTextClass ] [ text "max level reached" ]
                , filledPortion (Utils.Percent.float 1.0)
                ]

        EarnCredits creditsToNextLevel ->
            let
                percentComplete : Utils.Percent.Percent
                percentComplete =
                    Utils.Percent.float (model.credits / creditsToNextLevel)
            in
            div [ outerBarClasses ]
                [ span [ class "z-10 relative flex items-center gap-2", progressBarTextClass ]
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
    div [ class "flex-col items-start text-lg" ]
        [ span []
            [ text "Unlock new "
            , strong [ class "text-primary" ] [ text unlockCategoryString ]
            , span [] [ text (" at level " ++ String.fromInt unlockLevel) ]
            ]
        ]


renderHeader : Model -> Html Msg
renderHeader model =
    div [ class "w-full min-w-full bg-base-200 flex items-center justify-center p-4" ]
        [ div [ class "flex items-end gap-4 w-full" ]
            [ div [ class "flex flex-col items-center" ]
                [ label [ class "text-sm" ] [ text "level" ]
                , div [ class "w-full flex justify-center items-center relative min-w-12" ]
                    [ div [ class "text-6xl leading-none font-extrabold relative inline-block z-10" ]
                        [ text (String.padLeft 2 '0' (String.fromInt model.level))
                        , button [ class "absolute top-0 right-0 -mr-8 opacity-25 mt-1/2 btn btn-xs btn-square btn-ghost", classList [ ( "hidden", Config.env /= Config.Dev ) ], onClick DebugLevelToMax ]
                            [ FeatherIcons.chevronsUp
                                |> FeatherIcons.withSize 20
                                |> FeatherIcons.toHtml []
                            ]
                        , button [ class "absolute top-0 right-0 -mr-8 mt-6 opacity-25 mt-1/2 btn btn-xs btn-square btn-ghost", classList [ ( "hidden", Config.env /= Config.Dev ) ], onClick DebugGainLevel ]
                            [ FeatherIcons.chevronUp
                                |> FeatherIcons.withSize 20
                                |> FeatherIcons.toHtml []
                            ]
                        ]
                    ]
                ]
            , div [ class "pb-1 w-full h-full flex flex-col items-end relative" ]
                [ case Utils.Unlocks.nextUnlock model.level Config.levelUnlockStats of
                    Nothing ->
                        div [] []

                    Just ( unlockLevel, unlockStats ) ->
                        renderNextUnlock model unlockLevel unlockStats
                , renderProgressBar model
                ]
            , div [ class "grid grid-cols-2 grid-rows-3 gap-2 min-w-64" ]
                (allMinerals
                    |> List.map
                        (\mineral ->
                            let
                                stats : MineralStats
                                stats =
                                    mineralStats mineral

                                amount : Float
                                amount =
                                    getByMineral model.minerals mineral
                            in
                            div [ class "flex items-center w-full" ]
                                [ img [ src stats.icon, class "w-4 mr-1" ] []
                                , span [ class "text-xs inline-block w-full" ] [ text stats.name ]
                                , span [ class "text-xs font-mono" ] [ text (floatToString amount) ]
                                ]
                        )
                )
            ]
        ]


renderDwarf : Model -> Dwarf -> Html Msg
renderDwarf model dwarf =
    let
        stats : DwarfStats
        stats =
            Utils.Record.getByDwarf dwarf Config.dwarfStats

        xp : DwarfXp
        xp =
            Utils.Record.getByDwarf dwarf model.dwarfXp

        level : Int
        level =
            dwarfLevel xp

        xpInLevelNumerator : DwarfXp
        xpInLevelNumerator =
            flatXpInCurrentLevel xp

        progressInLevelSpan : Html Msg
        progressInLevelSpan =
            case Config.dwarfLevelingSchedule level of
                Ok (EarnDwarfXp xpToNextLevel) ->
                    span [ class "text-sm" ]
                        [ text (floatToString (DwarfXp.toFloat xpInLevelNumerator) ++ " / " ++ floatToString xpToNextLevel ++ " xp to next level") ]

                _ ->
                    span [ class "text-sm" ]
                        [ text "Max level reached" ]

        dwarfImgSrc : String
        dwarfImgSrc =
            stats.imgSrc
    in
    div [ class "bg-base-300 text-base-content flex flex-col items-center relative overflow-hidden themed-rounded-borders shadow-sm" ]
        [ span [ class "w-full flex items-center justify-center gap-2" ]
            [ span [ class "text-lg" ] [ text stats.name ]
            ]
        , img [ src dwarfImgSrc, class "h-12 rounded-sm " ] []
        , span []
            [ progressInLevelSpan
            ]
        , div [ class "tooltip tooltip-left absolute top-0 right-0", attribute "data-tip" ("Level " ++ String.fromInt level) ]
            [ span
                [ class "inline-block bg-secondary text-secondary-content px-3 py-1 text-2xl border border-secondary-content themed-rounded-borders"
                ]
                [ text (String.fromInt level) ]
            ]
        , progress [ class "progress progress-secondary xp-bar", value (String.fromFloat (Utils.Percent.toPercentage (percentInLevel xp))), attribute "max" "100" ] []
        ]


renderBuff : Buff -> Html Msg
renderBuff { title, icon, description, mod } =
    -- Note: had a tooltip here at one point but I didn't like the effect
    -- Might revisit it later or something similar that lets users learn about the buffs
    div [ class "flex flex-col items-center border border-content rounded-sm p-1 bg-info text-info-content" ]
        [ div [ class "flex items-center gap-1" ]
            [ img [ src icon, class "w-4" ] []
            , div [ class "text-sm leading-none" ] [ text title ]
            ]
        , div [] [ text (modToString mod) ]
        ]


modToString : Mod -> String
modToString mod =
    case mod of
        ModMissionYield percent ->
            "+" ++ Utils.Percent.toString percent ++ "% yield"


tabLayout =
    { container = class "flex flex-col items-center grow overflow-scroll"
    , headerWrapper = class "px-8 py-4 w-full flex items-center justify-between"
    , bonusesArea = class "w-full flex items-center gap-4 px-8 h-14"
    , contentWrapper = class "pt-0 w-full flex justify-center px-8"
    }


renderBiomeDropdownContent : Maybe Biome -> Html Msg
renderBiomeDropdownContent maybeSelectedBiome =
    ul [ class "menu menu-lg w-80 rounded-box bg-base-100 shadow-sm" ]
        (List.concat
            [ [ li [ class "menu-title" ] [ text "Select a biome" ] ]
            , List.map
                (\biome ->
                    let
                        stats =
                            biomeStats biome
                    in
                    li [ class "flex flex-col" ]
                        [ a [ onClick (HandleMissionBiomeSelection biome) ]
                            [ img [ src stats.icon, class "w-6" ] []
                            , text stats.name
                            , div [ class "flex items-center gap-1 ml-2" ]
                                [ div [ class "flex items-center" ]
                                    (List.map (\mineral -> img [ src (mineralStats mineral).icon, class "w-4 h-4" ] []) stats.abundantMinerals)
                                , div [ class "flex items-center" ]
                                    (List.map (\mineral -> img [ src (mineralStats mineral).icon, class "w-4 h-4 opacity-50" ] []) stats.scarceMinerals)
                                ]
                            ]
                        ]
                )
                allBiomes
            ]
        )


renderActiveBiome : Biome -> Html Msg
renderActiveBiome biome =
    div [ class "w-72 h-12 rounded overflow-hidden relative shadow" ]
        [ div
            [ style "background-image" ("url(" ++ (biomeStats biome).image ++ ")")
            , class "w-full h-full bg-cover bg-no-repeat bg-center"
            ]
            []
        , div [ class "absolute top-0 left-0 bg-base-100 text-xs px-1 py-0.5 leading-none opacity-60" ] [ text (biomeStats biome).name ]
        ]


renderMissionsTab : Model -> Html Msg
renderMissionsTab model =
    let
        unlockedMissions : List Mission
        unlockedMissions =
            List.filter (Utils.Unlocks.missionIsUnlocked model.level) Utils.Record.allMissions

        bonuses : List (Html Msg)
        bonuses =
            List.map renderBuff (getAllBuffs model)
    in
    div [ tabLayout.container ]
        [ div [ tabLayout.headerWrapper ]
            [ div [ class "w-full" ]
                [ div [ class "w-full flex items-center justify-between" ]
                    [ div [ proseClass ] [ h1 [] [ text "Missions" ] ]
                    , div [ class "flex items-center gap-2" ]
                        [ case model.missionBiome of
                            Just biome ->
                                renderActiveBiome biome

                            Nothing ->
                                div [ class "h-12 flex items-center" ] [ text "Select a biome" ]
                        , button
                            [ class "btn btn-md btn-square"
                            , attribute "style" "anchor-name:--anchor-1"
                            , attribute "popovertarget" "popover-1"
                            ]
                            [ FeatherIcons.chevronDown
                                |> FeatherIcons.toHtml []
                            ]
                        ]
                    ]
                , div [ class "dropdown", attribute "popover" "", id "popover-1", attribute "style" "position-anchor:--anchor-1" ]
                    [ renderBiomeDropdownContent model.missionBiome ]
                ]
            ]
        , div [ tabLayout.bonusesArea ]
            bonuses
        , div [ tabLayout.contentWrapper ]
            [ table [ class "table w-[750px] max-w-full" ]
                [ tbody []
                    (List.map (renderMissionRow model) unlockedMissions)
                ]
            ]
        ]


renderCommendationsTab : Model -> Html Msg
renderCommendationsTab model =
    let
        unlockedXpButtons : List DwarfXpButton
        unlockedXpButtons =
            List.filter (Utils.Unlocks.dwarfXpButtonIsUnlocked model.level) allDwarfXpButtons
    in
    div [ tabLayout.container ]
        [ div [ tabLayout.headerWrapper ]
            [ div [ proseClass ]
                [ h1 [] [ text "Dwarf Leveling" ]
                ]
            ]
        , div [ tabLayout.bonusesArea ]
            []
        , div [ tabLayout.contentWrapper ]
            [ div [ class "flex flex-col item-center gap-4 w-[300px]" ]
                (unlockedXpButtons
                    |> List.map
                        (\dwarfXpButton ->
                            let
                                stats : DwarfXpButtonStats
                                stats =
                                    dwarfXpButtonStats dwarfXpButton
                            in
                            renderButton
                                model
                                (getByDwarfXpButton model.dwarfXpButtonStatuses dwarfXpButton)
                                stats.duration
                                (HandleDwarfXpButtonClick dwarfXpButton)
                                ButtonSecondary
                                [ text "+", text (DwarfXp.toString stats.xp), text " xp to random dwarf" ]
                        )
                )
            ]
        ]


renderSettingsTab : Model -> Html Msg
renderSettingsTab model =
    div [ tabLayout.container ]
        [ div [ tabLayout.headerWrapper ]
            [ div [ proseClass ]
                [ h1 [] [ text "Settings" ]
                ]
            ]
        , div [ tabLayout.bonusesArea ]
            []
        , div [ tabLayout.contentWrapper ]
            [ div [ class "flex flex-col items-center gap-4 w-full max-w-[750px]" ]
                [ button [ class "btn btn-error", onClick ResetGame ] [ text "Reset game" ]
                , div [ class "divider" ] []
                , div [ proseClass ]
                    [ h2 [] [ text "Admin crimes" ]
                    , div [ class "flex items-center gap-1" ]
                        [ label [ for "button-no-cd" ] [ text "No CD button" ]
                        , input
                            [ type_ "checkbox"
                            , class "checkbox"
                            , id "button-no-cd"
                            , checked model.debugSettings.buttonCooldownInstant
                            , onCheck DebugSetButtonCooldownInstant
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


renderDrawerTabRow : Model -> Tab -> Html Msg
renderDrawerTabRow model tab =
    let
        stats : TabStats
        stats =
            Utils.Record.getByTab tab Config.tabStats

        isActive : Bool
        isActive =
            model.currentTab == tab

        -- hasActionsInTab : Bool
        -- hasActionsInTab =
        --     numActiveItemsInTab model tab > 0
    in
    li
        [ classList [ ( "hidden", not (isTabUnlocked model tab) ) ] ]
        [ a
            [ onClick (HandleTabClick tab)
            , classList
                [ ( "menu-active", isActive )
                ]
            ]
            (List.concat
                [ case stats.maybeIcon of
                    Just icon ->
                        [ icon
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                        ]

                    Nothing ->
                        []
                , [ span [] [ text stats.title ] ]
                ]
            )
        ]


numActiveItemsInTab : Model -> Tab -> Int
numActiveItemsInTab model tab =
    case tab of
        MissionsTab ->
            Utils.Record.allMissions
                |> List.filter (Utils.Unlocks.missionIsUnlocked model.level)
                |> List.filter (\mission -> Utils.Record.getByMission mission model.missionStatuses == ButtonReady)
                |> List.length

        CommendationsTab ->
            allDwarfXpButtons
                |> List.filter (Utils.Unlocks.dwarfXpButtonIsUnlocked model.level)
                |> List.filter (\dwarfXpButton -> getByDwarfXpButton model.dwarfXpButtonStatuses dwarfXpButton == ButtonReady)
                |> List.length

        AbyssBarTab ->
            0

        SettingsTab ->
            0


squadBonus : Model -> Percent
squadBonus model =
    Utils.Record.allDwarfs
        |> List.map (\dwarf -> dwarfLevel (Utils.Record.getByDwarf dwarf model.dwarfXp))
        |> List.sum
        |> (\s -> s - 4)
        -- Divide by 100 since it's a percentage i.e. sum of 10 levels = 0.1 multiplier
        |> toFloat
        |> (\x -> x / 100)
        |> Utils.Percent.float


getYieldBonus : List Mod -> Percent
getYieldBonus mods =
    List.filterMap
        (\mod ->
            case mod of
                ModMissionYield percent ->
                    Just percent
        )
        mods
        |> List.foldl Quantity.plus Quantity.zero


renderLogo : Html Msg
renderLogo =
    div [ class "flex-1 flex items-center justify-between gap-2 px-4" ]
        [ div [ class "flex-0 px-2 flex flex-col items-center" ]
            [ div [ class "font-title text-primary inline-flex text-lg transition-all duration-200 md:text-3xl flex gap-1 items-center rounded-t-xl overflow-hidden p-1 border border-primary border-b-4" ]
                [ span [ class "uppercase text-primary font-extrabold leading-none px-1" ] [ text "DRG" ]
                , div [ class "text-primary text-xs font-bold t-column gap-0 leading-2xs text-primary font-mono" ]
                    [ text "Mission Control"
                    ]
                ]
            ]
        ]


isTabUnlocked : Model -> Tab -> Bool
isTabUnlocked model tab =
    case tab of
        MissionsTab ->
            True

        CommendationsTab ->
            Utils.Unlocks.dwarfXpButtonsFeatureUnlocked model.level

        AbyssBarTab ->
            Utils.Unlocks.abyssBarFeatureUnlocked model.level

        SettingsTab ->
            True


renderAbyssBarTab : Model -> Html Msg
renderAbyssBarTab model =
    div [ tabLayout.container ]
        [ div [ tabLayout.headerWrapper ]
            [ div [ proseClass ]
                [ h1 [] [ text "Abyss Bar" ]
                ]
            ]
        , div [ tabLayout.bonusesArea ]
            []
        , div [ tabLayout.contentWrapper ]
            [ div [ class "flex flex-col items-center gap-4 max-w-[750px]" ]
                [ case model.dailySpecialCooldown of
                    ButtonReady ->
                        div [ class "flex flex-col gap-4" ]
                            [ p [] [ text "Select a ", strong [] [ text "daily special" ], text " to give your crew an ", strong [ class "underline" ] [ text "awesome buff" ] ]
                            , div [ class "flex items-center gap-4" ]
                                (List.map (renderDailySpecialOption model) model.dailySpecialOptions)
                            ]

                    ButtonOnCooldown cooldown ->
                        let
                            hasTickedAVeryShortTime : Bool
                            hasTickedAVeryShortTime =
                                Utils.Timer.hasTickedAVeryShortTime Config.dailySpecialCooldown cooldown
                        in
                        div [ class "flex flex-col items-center gap-2" ]
                            [ p [] [ text "Select a new daily special in" ]
                            , renderDuration (Utils.Timer.durationLeft Config.dailySpecialCooldown cooldown) hasTickedAVeryShortTime
                            ]
                ]
            ]
        ]


renderDailySpecialOption : Model -> DailySpecial -> Html Msg
renderDailySpecialOption model option =
    let
        stats : DailySpecialStats
        stats =
            dailySpecialStats option
    in
    div [ class "card card-sm bg-base-300 w-72 shadow-lg" ]
        [ figure [ class "pt-2 bg-warning" ]
            [ img [ src stats.icon, alt stats.title ] []
            , img [ src "beer/beer.png", class "w-24 -ml-6" ] []
            ]
        , div [ class "card-body" ]
            [ h2 [ class "card-title" ] [ text "Daily Special: ", span [ class "underline" ] [ text stats.title ] ]
            , p [] [ text (modToString stats.buff.mod) ]
            , div [ class "card-actions justify-end" ]
                [ button [ class "btn btn-warning", onClick (HandleDailySpecialClick option) ] [ text "Select" ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen overflow-hidden flex flex-col items-center bg-base-100" ]
        [ -- Header
          renderHeader model

        -- Body
        , div [ class "flex w-full drawer drawer-open" ]
            (List.concat
                [ [ input [ id "my-drawer", type_ "checkbox", class "drawer-toggle" ] []
                  , div [ class "drawer-side w-48", attribute "style" "scroll-behavior: smooth; scroll-padding-top:5rem" ]
                        [ label [ for "my-drawer", class "drawer-overlay", attribute "aria-label" "close sidebar" ] []
                        , aside
                            [ class "bg-base-100 overflow-y-scroll h-full w-full"
                            ]
                            [ div [ class "sticky top-0 z-10 w-full bg-opacity-90 py-3 backdrop-blur-sm flex" ]
                                [ renderLogo ]
                            , ul
                                [ class "menu w-full px-4 py-0"

                                -- , classList [ ( "hidden", not (Utils.Unlocks.dwarfXpButtonsFeatureUnlocked model.level) ) ]
                                ]
                                [ renderDrawerTabRow model MissionsTab
                                , renderDrawerTabRow model CommendationsTab
                                , renderDrawerTabRow model AbyssBarTab
                                , li [] [] -- This renders as a divider in the drawer
                                , renderDrawerTabRow model SettingsTab
                                ]
                            ]
                        ]
                  , div [ class "drawer-content w-full h-full" ]
                        [ div [ class "flex h-full" ]
                            [ case model.currentTab of
                                MissionsTab ->
                                    renderMissionsTab model

                                CommendationsTab ->
                                    renderCommendationsTab model

                                AbyssBarTab ->
                                    renderAbyssBarTab model

                                SettingsTab ->
                                    renderSettingsTab model
                            , div [ class "h-full w-48 min-w-48 items-center p-4 overflow-y-scroll" ]
                                [ div
                                    [ class "flex flex-col gap-4"
                                    , classList
                                        [ ( "hidden", not (Utils.Unlocks.dwarfXpButtonsFeatureUnlocked model.level) )
                                        ]
                                    ]
                                    (List.concat
                                        [ [ div [ class "w-full flex items-center justify-center prose" ] [ h3 [] [ text "Your crew" ] ] ]
                                        , List.map
                                            (renderDwarf model)
                                            Utils.Record.allDwarfs
                                        ]
                                    )
                                ]
                            ]
                        ]
                  ]
                , [ div [ class "fixed bottom-0 left-0 ml-6 mb-6" ] [ Theme.renderThemeDropdown model model.theme ] ]
                , case model.maybeInitDecodeErr of
                    Just err ->
                        [ div [ class "fixed bottom-0 left-0 bg-error text-error-content rounded-lg p-4" ] [ text (D.errorToString err) ] ]

                    Nothing ->
                        []
                ]
            )
        ]
