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


port openModal : String -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


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
    , morkite = 0
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
    , projectLevels = projectRecord 0
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


addMorkite : Float -> { a | morkite : Float, level : Int } -> { a | morkite : Float, level : Int }
addMorkite amount model =
    case Config.levelingSchedule model.level of
        GainMorkite morkiteToNextLevel ->
            let
                newTotalMorkite : Float
                newTotalMorkite =
                    model.morkite + amount

                remainder : Float
                remainder =
                    newTotalMorkite - morkiteToNextLevel
            in
            if remainder < 0 then
                -- Not enough to level
                { model | morkite = newTotalMorkite }

            else
                -- Enough to level once or more
                addMorkite remainder { model | morkite = 0, level = model.level + 1 }

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


getBuffStrengthMultiplierFromProjects : Model -> Float
getBuffStrengthMultiplierFromProjects model =
    let
        buffStrengthBonus : Percent
        buffStrengthBonus =
            getDailySpecialBuffStrength (getAllMods model)

        buffStrengthMultiplier : Float
        buffStrengthMultiplier =
            1 + Utils.Percent.toFloat buffStrengthBonus
    in
    buffStrengthMultiplier


getEnhancedDailySpecialBuff : Model -> DailySpecial -> Buff
getEnhancedDailySpecialBuff model dailySpecial =
    let
        baseBuff : Buff
        baseBuff =
            (dailySpecialStats dailySpecial).buff

        buffStrengthMultiplier : Float
        buffStrengthMultiplier =
            getBuffStrengthMultiplierFromProjects model
    in
    { baseBuff
        | mod =
            case baseBuff.mod of
                ModMissionYield percent ->
                    ModMissionYield (Quantity.multiplyBy buffStrengthMultiplier percent)

                ModMissionSpeed percent ->
                    ModMissionSpeed (Quantity.multiplyBy buffStrengthMultiplier percent)

                ModDailySpecialBuffStrength percent ->
                    ModDailySpecialBuffStrength (Quantity.multiplyBy buffStrengthMultiplier percent)

                ModDwarfXpGain percent ->
                    ModDwarfXpGain (Quantity.multiplyBy buffStrengthMultiplier percent)
    }


getAllBuffs : Model -> List Buff
getAllBuffs model =
    let
        squadBonusPercent : Percent
        squadBonusPercent =
            squadBonus model

        squadMods : List Buff
        squadMods =
            if Quantity.greaterThan Quantity.zero squadBonusPercent then
                [ { title = "Squad bonus", icon = "engineer.webp", description = "Increase the yield of missions", mod = ModMissionYield squadBonusPercent, mult = 1 } ]

            else
                []

        dailySpecialMods : List Buff
        dailySpecialMods =
            List.map (\( dailySpecial, _ ) -> getEnhancedDailySpecialBuff model dailySpecial) model.activeDailySpecials

        projectMods : List Buff
        projectMods =
            allProjects
                |> List.filterMap
                    (\project ->
                        let
                            stats =
                                projectStats project

                            level =
                                getByProject model.projectLevels project
                        in
                        if level > 0 then
                            let
                                baseBuff : Buff
                                baseBuff =
                                    stats.buff

                                adjustedBuff : Buff
                                adjustedBuff =
                                    { baseBuff | mult = level }
                            in
                            Just adjustedBuff

                        else
                            Nothing
                    )
    in
    List.concat [ squadMods, dailySpecialMods, projectMods ]


getModFromBuff : Buff -> Mod
getModFromBuff buff =
    case buff.mod of
        ModMissionYield percent ->
            ModMissionYield (Quantity.multiplyBy (toFloat buff.mult) percent)

        ModMissionSpeed percent ->
            ModMissionSpeed (Quantity.multiplyBy (toFloat buff.mult) percent)

        ModDailySpecialBuffStrength percent ->
            ModDailySpecialBuffStrength (Quantity.multiplyBy (toFloat buff.mult) percent)

        ModDwarfXpGain percent ->
            ModDwarfXpGain (Quantity.multiplyBy (toFloat buff.mult) percent)


getAllMods : Model -> List Mod
getAllMods model =
    List.map .mod (getAllBuffs model)


mineralYieldGenerator : Mission -> Biome -> Random.Generator (Dict Mineral Float)
mineralYieldGenerator mission biome =
    let
        { abundantMineral, scarceMineral } =
            biomeStats biome

        { morkite } =
            Utils.Record.getByMission mission Config.missionStats
    in
    Random.map3
        (\abundantMineralAmount scarceMineralAmount didGetScarce ->
            if didGetScarce then
                Dict.fromList [ ( scarceMineral, scarceMineralAmount * morkite ), ( abundantMineral, abundantMineralAmount * morkite ) ]

            else
                Dict.fromList [ ( abundantMineral, abundantMineralAmount * morkite ) ]
        )
        (Random.float 0.5 1)
        (Random.float 0.25 0.75)
        (probabilityGenerator 0.5)


probabilityGenerator : Float -> Random.Generator Bool
probabilityGenerator probability =
    Random.float 0 1
        |> Random.map (\x -> x < probability)


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

        OpenModal modalId ->
            ( model, openModal modalId )

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
                        baseDuration : Duration
                        baseDuration =
                            (Utils.Record.getByMission mission Config.missionStats).duration

                        buttonDuration : Duration
                        buttonDuration =
                            calculateAdjustedDuration baseDuration (getAllMods model)
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

                ( mineralYield, seed2 ) =
                    case model.missionBiome of
                        Just biome ->
                            Random.step (mineralYieldGenerator mission biome) model.seed

                        Nothing ->
                            ( Dict.empty, model.seed )

                yield : MissionYield
                yield =
                    { morkite = stats.morkite, minerals = mineralYield }

                modifiedYield : MissionYield
                modifiedYield =
                    modifyYield model yield

                newMissionStatuses : MissionRecord ButtonStatus
                newMissionStatuses =
                    Utils.Record.setByMission mission (setButtonCooldown model) model.missionStatuses

                addMorkiteResult : Model
                addMorkiteResult =
                    addMorkite modifiedYield.morkite model
            in
            ( { model
                | seed = seed2
                , missionStatuses = newMissionStatuses
                , morkite = addMorkiteResult.morkite
                , level = addMorkiteResult.level
                , minerals = addMineralDictToRecord model.minerals modifiedYield.minerals
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
                    Random.step (dwarfXpGenerator stats.xp model.dwarfXp (getAllMods model)) model.seed

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
            ( { model | missionBiome = Just biome }, closePopover "popover-1" )

        HandleProjectUpgrade project ->
            let
                stats =
                    projectStats project

                currentLevel =
                    getByProject model.projectLevels project

                maxLevelReached =
                    currentLevel >= stats.maxLevels

                -- Check if we have enough minerals
                canUpgrade =
                    if maxLevelReached then
                        False

                    else
                        Dict.foldl
                            (\mineral cost canAfford ->
                                canAfford && (getByMineral model.minerals mineral >= cost)
                            )
                            True
                            stats.costs

                -- Only upgrade if we can afford it and haven't reached max level
                updatedModel =
                    if canUpgrade && not maxLevelReached then
                        -- Subtract costs
                        let
                            updatedMinerals =
                                Dict.foldl
                                    (\mineral cost minerals ->
                                        updateByMineral (\current -> current - cost) minerals mineral
                                    )
                                    model.minerals
                                    stats.costs

                            -- Increment project level
                            updatedProjectLevels =
                                updateByProject (\level -> level + 1) model.projectLevels project
                        in
                        { model
                            | minerals = updatedMinerals
                            , projectLevels = updatedProjectLevels
                        }

                    else
                        model
            in
            ( updatedModel, Cmd.none )

        DebugGiveMinerals ->
            ( { model | minerals = mineralRecord 100 }, Cmd.none )


modifyYield : Model -> MissionYield -> MissionYield
modifyYield model yield =
    let
        -- Get bonuses from all buffs (daily specials, squad bonus, projects)
        totalBonus : Percent
        totalBonus =
            getYieldBonus (getAllMods model)

        -- Calculate the bonus multiplier
        bonusMultiplier : Float
        bonusMultiplier =
            1 + Utils.Percent.toFloat totalBonus

        -- Apply bonus to minerals
        modifiedMinerals : Dict Mineral Float
        modifiedMinerals =
            Dict.map (\_ value -> value * bonusMultiplier) yield.minerals
    in
    { morkite = yield.morkite * bonusMultiplier
    , minerals = modifiedMinerals
    }


addMineralDictToRecord : MineralRecord Float -> Dict Mineral Float -> MineralRecord Float
addMineralDictToRecord record minerals =
    Dict.foldl (\k v acc -> updateByMineral ((+) v) acc k) record minerals


addDicts : Dict a Float -> Dict a Float -> Dict a Float
addDicts dict1 dict2 =
    Dict.merge
        (\k v -> Dict.insert k v)
        (\k v1 v2 -> Dict.insert k (v1 + v2))
        (\k v -> Dict.insert k v)
        dict1
        dict2
        Dict.empty


dwarfXpGenerator : DwarfXp -> DwarfRecord DwarfXp -> List Mod -> Random.Generator ( Dwarf, DwarfRecord DwarfXp )
dwarfXpGenerator baseXp dwarfXp mods =
    let
        -- Calculate the XP gain bonus from all active mods
        xpGainBonus : Percent
        xpGainBonus =
            getDwarfXpGainBonus mods

        -- Apply the bonus to the base XP amount
        bonusMultiplier : Float
        bonusMultiplier =
            1 + Utils.Percent.toFloat xpGainBonus

        adjustedXp : DwarfXp
        adjustedXp =
            Quantity.multiplyBy bonusMultiplier baseXp
    in
    Random.uniform Scout Utils.Record.allDwarfs
        |> Random.map
            (\dwarf ->
                let
                    oldXp : DwarfXp
                    oldXp =
                        Utils.Record.getByDwarf dwarf dwarfXp

                    newXp : DwarfXp
                    newXp =
                        Quantity.plus oldXp adjustedXp
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


morkiteImg : Html Msg
morkiteImg =
    img [ src "minerals/morkite.webp", class "w-6 inline-block" ] []


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
                    "Unload Cargo"

                ButtonOnCooldown _ ->
                    "On cooldown"

        -- Base yield without bonuses
        baseYield : MissionYield
        baseYield =
            { morkite = stats.morkite
            , minerals = Dict.empty
            }

        -- Modified yield with all bonuses applied
        modifiedYield : MissionYield
        modifiedYield =
            modifyYield model baseYield

        -- Calculate the bonus percentage to display
        bonusPercent : Float
        bonusPercent =
            if stats.morkite > 0 then
                ((modifiedYield.morkite / stats.morkite) - 1) * 100

            else
                0

        adjustedDuration : Duration
        adjustedDuration =
            calculateAdjustedDuration stats.duration (getAllMods model)

        -- Format the values for display
        morkiteYieldString : String
        morkiteYieldString =
            if bonusPercent > 0 then
                floatToFixedDecimalString modifiedYield.morkite 1 ++ " morkite"

            else
                floatToFixedDecimalString stats.morkite 1 ++ " morkite"

        bonusText : Html Msg
        bonusText =
            if bonusPercent > 0 then
                span [ class "text-xs text-success ml-1" ]
                    [ text ("+" ++ String.fromFloat (round bonusPercent |> toFloat) ++ "%") ]

            else
                text ""

        -- Estimate mineral yield ranges for display
        estimateMineral : Mineral -> Float -> Float -> ( String, String )
        estimateMineral mineral minFactor maxFactor =
            let
                baseEstimateMin =
                    stats.morkite * minFactor

                baseEstimateMax =
                    stats.morkite * maxFactor

                bonusMultiplier =
                    1 + Utils.Percent.toFloat (getYieldBonus (getAllMods model))

                modifiedEstimateMin =
                    round (baseEstimateMin * bonusMultiplier)

                modifiedEstimateMax =
                    round (baseEstimateMax * bonusMultiplier)

                rangeDisplay =
                    String.fromInt modifiedEstimateMin ++ "-" ++ String.fromInt modifiedEstimateMax

                -- For scarce mineral we just use average
                averageEstimate =
                    round ((baseEstimateMin + baseEstimateMax) / 2 * bonusMultiplier)

                singleDisplay =
                    String.fromInt averageEstimate
            in
            ( rangeDisplay, singleDisplay )
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
        , td []
            [ div [ class "flex items-center gap-1" ]
                [ span [ class "text-sm" ] [ text morkiteYieldString ]
                , bonusText
                , span [ class "text-sm", classList [ ( "hidden", model.missionBiome == Nothing ) ] ] [ text "//" ]
                , div [ class "flex items-center gap-1" ]
                    (case model.missionBiome of
                        Just biome ->
                            let
                                biomeInfo =
                                    biomeStats biome

                                ( abundantRange, abundantAvg ) =
                                    estimateMineral biomeInfo.abundantMineral 0.5 1.0

                                ( scarceRange, scarceAvg ) =
                                    estimateMineral biomeInfo.scarceMineral 0.25 0.75

                                abundantMineral =
                                    div [ class "flex items-center tooltip", attribute "data-tip" "Estimated yield range" ]
                                        [ img
                                            [ src (mineralStats biomeInfo.abundantMineral).icon
                                            , class "w-5"
                                            ]
                                            []
                                        , span [ class "text-xs ml-1" ] [ text abundantRange ]
                                        ]

                                scarceMineral =
                                    div [ class "flex items-center ml-1 text-xs opacity-50" ]
                                        [ span [] [ text "(" ]
                                        , img
                                            [ src (mineralStats biomeInfo.scarceMineral).icon
                                            , class "w-5 opacity-50 mx-1"
                                            ]
                                            []
                                        , span [] [ text ")" ]
                                        ]
                            in
                            [ abundantMineral, scarceMineral ]

                        Nothing ->
                            []
                    )
                ]
            ]
        , td [ class "overflow-hidden relative flex justify-end items-center h-[70px]" ]
            [ renderButton model missionStatus adjustedDuration (HandleMissionClick mission) ButtonPrimary [ text buttonText ] ]
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
            class "px-3 py-1 rounded-sm bg-base-100 text-base-content text-3xl leading-none text-xl opacity-75"
    in
    case Config.levelingSchedule model.level of
        AtMaxLevel ->
            div [ outerBarClasses ]
                [ span [ class "z-10 uppercase", progressBarTextClass ] [ text "max level reached" ]
                , filledPortion (Utils.Percent.float 1.0)
                ]

        GainMorkite morkiteToNextLevel ->
            let
                percentComplete : Utils.Percent.Percent
                percentComplete =
                    Utils.Percent.float (model.morkite / morkiteToNextLevel)
            in
            div [ outerBarClasses ]
                [ span [ class "z-10 relative flex items-center gap-2", progressBarTextClass ]
                    [ span [ class "font-semibold text-3xl leading-none font-mono mr-2" ] [ text (floatToString model.morkite ++ " / " ++ floatToString morkiteToNextLevel) ]
                    , text "morkite"
                    , span [ class "flex items-center" ] [ morkiteImg ]
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

                UnlockBiomes ->
                    "Biomes"
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
            , div
                [ class "grid grid-cols-2 grid-rows-3 gap-2 min-w-64"
                , classList [ ( "hidden", not (Utils.Unlocks.biomesFeatureIsUnlocked model.level) ) ]
                ]
                (List.concat
                    [ allMinerals
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
                    , [ button
                            [ class "opacity-25 btn btn-xs btn-square btn-ghost"
                            , classList [ ( "hidden", Config.env /= Config.Dev ) ]
                            , onClick DebugGiveMinerals
                            ]
                            [ FeatherIcons.package
                                |> FeatherIcons.withSize 20
                                |> FeatherIcons.toHtml []
                            ]
                      ]
                    ]
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
                    span [ class "text-xs" ]
                        [ text (floatToString (DwarfXp.toFloat xpInLevelNumerator) ++ " / " ++ floatToString xpToNextLevel ++ " xp to next level") ]

                _ ->
                    span [ class "text-xs" ]
                        [ text "Max level reached" ]

        dwarfImgSrc : String
        dwarfImgSrc =
            stats.imgSrc

        percentComplete : Utils.Percent.Percent
        percentComplete =
            percentInLevel xp
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
        , div [ class "w-full h-2 relative bg-secondary/15" ]
            [ div
                [ class "absolute left-0 top-0 h-full bg-secondary xp-bar themed-rounded-borders"
                , style "width" (String.fromFloat (Basics.min 100 (Utils.Percent.toPercentage percentComplete)) ++ "%")
                ]
                []
            ]
        ]


unlockedBiomes : Int -> List Biome
unlockedBiomes level =
    List.filter (Utils.Unlocks.biomeIsUnlocked level) allBiomes


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

        ModMissionSpeed percent ->
            "+" ++ Utils.Percent.toString percent ++ "% speed"

        ModDailySpecialBuffStrength percent ->
            "+" ++ Utils.Percent.toString percent ++ "% buff strength"

        ModDwarfXpGain percent ->
            "+" ++ Utils.Percent.toString percent ++ "% xp gain"


tabLayout =
    { container = class "flex flex-col items-center grow overflow-scroll"
    , headerWrapper = class "px-8 py-4 w-full flex items-center justify-between"
    , bonusesArea = class "w-full flex items-center gap-4 px-8 h-14"
    , contentWrapper = class "pt-0 w-full flex justify-center px-8"
    }


renderBiomeDropdownContent : Model -> Html Msg
renderBiomeDropdownContent model =
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
                                    [ img [ src (mineralStats stats.abundantMineral).icon, class "w-4 h-4" ] [] ]
                                , div [ class "flex items-center" ]
                                    [ img [ src (mineralStats stats.scarceMineral).icon, class "w-4 h-4 opacity-50" ] [] ]
                                ]
                            ]
                        ]
                )
                (unlockedBiomes model.level)
            ]
        )


renderActiveBiome : Biome -> Html Msg
renderActiveBiome biome =
    button
        [ class "btn btn-md w-72 h-12 rounded overflow-hidden relative shadow"
        , attribute "style" "anchor-name:--anchor-1"
        , attribute "popovertarget" "popover-1"
        , style "background-image" ("url(" ++ (biomeStats biome).image ++ ")")
        , class "bg-cover bg-no-repeat bg-center"
        ]
        [ div [ class "absolute top-0 left-0 bg-base-100 text-xs px-1 py-0.5 leading-none opacity-60" ] [ text (biomeStats biome).name ]
        ]


renderMissionsTab : Model -> Html Msg
renderMissionsTab model =
    let
        unlockedMissions : List Mission
        unlockedMissions =
            List.filter (Utils.Unlocks.missionIsUnlocked model.level) Utils.Record.allMissions

        bonuses : List (Html Msg)
        bonuses =
            List.map renderBuff
                (List.filter
                    (\buff ->
                        case buff.mod of
                            ModMissionYield _ ->
                                True

                            ModMissionSpeed _ ->
                                True

                            ModDailySpecialBuffStrength _ ->
                                False

                            ModDwarfXpGain _ ->
                                False
                    )
                    (getAllBuffs model)
                )
    in
    div [ tabLayout.container ]
        [ div [ tabLayout.headerWrapper ]
            [ div [ class "w-full" ]
                [ div [ class "w-full flex items-center justify-between" ]
                    [ div [ proseClass ] [ h1 [] [ text "Missions" ] ]
                    , div
                        [ class "flex items-center gap-2"
                        , classList [ ( "hidden", List.length (unlockedBiomes model.level) <= 1 ) ]
                        ]
                        [ case model.missionBiome of
                            Just biome ->
                                renderActiveBiome biome

                            Nothing ->
                                button
                                    [ class "btn btn-md"
                                    , attribute "style" "anchor-name:--anchor-1"
                                    , attribute "popovertarget" "popover-1"
                                    ]
                                    [ text "Select a biome" ]
                        , button
                            [ class "btn btn-md btn-square"
                            , attribute "popovertarget" "popover-1"
                            ]
                            [ FeatherIcons.chevronDown
                                |> FeatherIcons.toHtml []
                            ]
                        ]
                    ]
                , div [ class "dropdown", attribute "popover" "", id "popover-1", attribute "style" "position-anchor:--anchor-1" ]
                    [ renderBiomeDropdownContent model ]
                ]
            ]
        , div [ tabLayout.bonusesArea ]
            bonuses
        , div [ tabLayout.contentWrapper ]
            [ table [ class "table table-sm w-[750px] max-w-full" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Hazard level" ]
                        , th [] [ text "Yield" ]
                        , th [ class "text-end" ] [ text "Status" ]
                        ]
                    ]
                , tbody []
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

        -- Get XP gain bonuses to display
        xpGainBonuses : List Buff
        xpGainBonuses =
            getAllBuffs model
                |> List.filter
                    (\buff ->
                        case buff.mod of
                            ModDwarfXpGain _ ->
                                True

                            _ ->
                                False
                    )
    in
    div [ tabLayout.container ]
        [ div [ tabLayout.headerWrapper ]
            [ div [ proseClass ]
                [ h1 [] [ text "Dwarf Leveling" ]
                ]
            ]
        , div [ tabLayout.bonusesArea ]
            (List.map renderBuff xpGainBonuses)
        , div [ tabLayout.contentWrapper ]
            [ div [ class "flex flex-col item-center gap-4 w-[300px]" ]
                (unlockedXpButtons
                    |> List.map
                        (\dwarfXpButton ->
                            let
                                stats : DwarfXpButtonStats
                                stats =
                                    dwarfXpButtonStats dwarfXpButton

                                -- Display adjusted XP if there's a bonus
                                xpGainBonus =
                                    getDwarfXpGainBonus (getAllMods model)

                                bonusMultiplier =
                                    1 + Utils.Percent.toFloat xpGainBonus

                                adjustedXp =
                                    Quantity.multiplyBy bonusMultiplier stats.xp

                                xpDisplay =
                                    if Utils.Percent.toFloat xpGainBonus > 0 then
                                        "+" ++ DwarfXp.toString adjustedXp ++ " xp"

                                    else
                                        "+" ++ DwarfXp.toString stats.xp ++ " xp"
                            in
                            renderButton
                                model
                                (getByDwarfXpButton model.dwarfXpButtonStatuses dwarfXpButton)
                                stats.duration
                                (HandleDwarfXpButtonClick dwarfXpButton)
                                ButtonSecondary
                                [ text xpDisplay, text " to random dwarf" ]
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
                [ button [ class "btn", onClick (OpenModal "my_modal_1") ] [ text "Reset game" ]
                ]
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

        ProjectsTab ->
            0

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


calculateAdjustedDuration : Duration -> List Mod -> Duration
calculateAdjustedDuration baseDuration mods =
    let
        -- Apply speed bonus to reduce mission duration
        speedBonus : Percent
        speedBonus =
            getMissionSpeedBonus mods

        speedMultiplier : Float
        speedMultiplier =
            1 + Utils.Percent.toFloat speedBonus
    in
    Quantity.divideBy speedMultiplier baseDuration


getYieldBonus : List Mod -> Percent
getYieldBonus mods =
    List.filterMap
        (\mod ->
            case mod of
                ModMissionYield percent ->
                    Just percent

                _ ->
                    Nothing
        )
        mods
        |> List.foldl Quantity.plus Quantity.zero


getDwarfXpGainBonus : List Mod -> Percent
getDwarfXpGainBonus mods =
    List.filterMap
        (\mod ->
            case mod of
                ModDwarfXpGain percent ->
                    Just percent

                _ ->
                    Nothing
        )
        mods
        |> List.foldl Quantity.plus Quantity.zero


getMissionSpeedBonus : List Mod -> Percent
getMissionSpeedBonus mods =
    List.filterMap
        (\mod ->
            case mod of
                ModMissionSpeed percent ->
                    Just percent

                _ ->
                    Nothing
        )
        mods
        |> List.foldl Quantity.plus Quantity.zero


getDailySpecialBuffStrength : List Mod -> Percent
getDailySpecialBuffStrength mods =
    List.filterMap
        (\mod ->
            case mod of
                ModDailySpecialBuffStrength percent ->
                    Just percent

                _ ->
                    Nothing
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

        ProjectsTab ->
            Utils.Unlocks.kindIsUnlocked model.level UnlockProjects

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
            (List.map renderBuff
                (List.filter
                    (\buff ->
                        case buff.mod of
                            ModDailySpecialBuffStrength _ ->
                                True

                            _ ->
                                False
                    )
                    (getAllBuffs model)
                )
            )
        , div [ tabLayout.contentWrapper ]
            [ div [ class "flex flex-col items-center gap-4 max-w-[750px]" ]
                [ if not (List.isEmpty model.activeDailySpecials) then
                    div [ class "card bg-base-300 shadow-lg w-full" ]
                        [ div [ class "card-body" ]
                            [ h2 [ class "card-title" ] [ text "Current Daily Special" ]
                            , div [ class "flex flex-wrap gap-4" ]
                                (List.map
                                    (\( dailySpecial, timer ) ->
                                        let
                                            stats =
                                                dailySpecialStats dailySpecial

                                            enhancedBuff =
                                                getEnhancedDailySpecialBuff model dailySpecial

                                            hasTickedAVeryShortTime =
                                                Utils.Timer.hasTickedAVeryShortTime Config.dailySpecialBuffDuration timer

                                            durationLeft =
                                                Utils.Timer.durationLeft Config.dailySpecialBuffDuration timer

                                            hours =
                                                floor (Duration.inHours durationLeft)

                                            minutes =
                                                floor (Duration.inMinutes durationLeft)
                                                    |> modBy 60

                                            seconds =
                                                if not (Quantity.greaterThan Duration.minute durationLeft) && hasTickedAVeryShortTime then
                                                    60

                                                else
                                                    floor (Duration.inSeconds durationLeft)
                                                        |> modBy 60

                                            timeString =
                                                String.padLeft 2 '0' (String.fromInt hours)
                                                    ++ ":"
                                                    ++ String.padLeft 2 '0' (String.fromInt minutes)
                                                    ++ ":"
                                                    ++ String.padLeft 2 '0' (String.fromInt seconds)
                                        in
                                        div [ class "flex items-center gap-2" ]
                                            [ img [ src stats.icon, class "w-8 h-8" ] []
                                            , div [ class "flex flex-col" ]
                                                [ span [ class "font-medium" ] [ text stats.title ]
                                                , span [ class "text-sm opacity-70" ] [ text (modToString enhancedBuff.mod) ]
                                                , span [ class "text-xs opacity-50" ] [ text timeString ]
                                                ]
                                            ]
                                    )
                                    model.activeDailySpecials
                                )
                            ]
                        ]

                  else
                    text ""
                , case model.dailySpecialCooldown of
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

        enhancedBuff : Buff
        enhancedBuff =
            getEnhancedDailySpecialBuff model option
    in
    div [ class "card card-sm bg-base-300 w-72 shadow-lg" ]
        [ figure [ class "pt-2 bg-warning" ]
            [ img [ src stats.icon, alt stats.title ] []
            , img [ src "beer/beer.png", class "w-24 -ml-6" ] []
            ]
        , div [ class "card-body" ]
            [ h2 [ class "card-title" ] [ text "Daily Special: ", span [ class "underline" ] [ text stats.title ] ]
            , p [] [ text (modToString enhancedBuff.mod) ]
            , div [ class "card-actions justify-end" ]
                [ button [ class "btn btn-warning", onClick (HandleDailySpecialClick option) ] [ text "Select" ]
                ]
            ]
        ]


renderProjectsTab : Model -> Html Msg
renderProjectsTab model =
    div [ tabLayout.container ]
        [ div [ tabLayout.headerWrapper ]
            [ div [ proseClass ]
                [ h1 [] [ text "Projects" ]
                ]
            ]
        , div [ tabLayout.contentWrapper ]
            [ div [ class "list bg-base-100 rounded-box shadow-md" ]
                [ div [ class "p-4 pb-2 text-xs opacity-60 tracking-wide" ]
                    [ text "Available Projects" ]
                , div [] (List.map (renderProjectRow model) allProjects)
                ]
            ]
        ]


renderProjectRow : Model -> Project -> Html Msg
renderProjectRow model project =
    let
        stats : ProjectStats
        stats =
            projectStats project

        currentLevel : Int
        currentLevel =
            getByProject model.projectLevels project

        maxLevelReached : Bool
        maxLevelReached =
            currentLevel >= stats.maxLevels

        -- Calculate if we have enough minerals for upgrade
        canUpgrade : Bool
        canUpgrade =
            if maxLevelReached then
                False

            else
                Dict.foldl
                    (\mineral cost canAfford ->
                        canAfford && (getByMineral model.minerals mineral >= cost)
                    )
                    True
                    stats.costs

        -- Calculate current effect based on level
        currentEffect : String
        currentEffect =
            if currentLevel <= 0 then
                "No effect yet"

            else
                case stats.buff.mod of
                    ModMissionYield percent ->
                        let
                            effectivePercent =
                                Utils.Percent.toFloat percent * toFloat currentLevel
                        in
                        "+" ++ String.fromFloat (effectivePercent * 100 |> round |> toFloat |> (\n -> n / 1)) ++ "% yield"

                    ModMissionSpeed percent ->
                        let
                            effectivePercent =
                                Utils.Percent.toFloat percent * toFloat currentLevel
                        in
                        "+" ++ String.fromFloat (effectivePercent * 100 |> round |> toFloat |> (\n -> n / 1)) ++ "% speed"

                    ModDailySpecialBuffStrength percent ->
                        let
                            effectivePercent =
                                Utils.Percent.toFloat percent * toFloat currentLevel
                        in
                        "+" ++ String.fromFloat (effectivePercent * 100 |> round |> toFloat |> (\n -> n / 1)) ++ "% buff strength"

                    ModDwarfXpGain percent ->
                        let
                            effectivePercent =
                                Utils.Percent.toFloat percent * toFloat currentLevel
                        in
                        "+" ++ String.fromFloat (effectivePercent * 100 |> round |> toFloat |> (\n -> n / 1)) ++ "% xp gain"

        -- Render mineral costs
        mineralCosts : List (Html Msg)
        mineralCosts =
            Dict.toList stats.costs
                |> List.map
                    (\( mineral, cost ) ->
                        let
                            available =
                                getByMineral model.minerals mineral

                            hasEnough =
                                available >= cost
                        in
                        div
                            [ class "flex items-center gap-1"
                            , classList [ ( "opacity-50", not hasEnough ) ]
                            ]
                            [ img
                                [ class "size-4"
                                , src (mineralStats mineral).icon
                                ]
                                []
                            , span
                                [ class "text-xs"
                                , classList [ ( "text-error", not hasEnough ) ]
                                ]
                                [ text (String.fromFloat cost) ]
                            ]
                    )
    in
    li [ class "list-row" ]
        [ div []
            [ img
                [ class "size-10 rounded-box"
                , src stats.buff.icon
                ]
                []
            ]
        , div []
            [ div [] [ text stats.name ]
            , div [ class "text-xs uppercase font-semibold opacity-60" ]
                [ text stats.buff.description ]
            ]
        , div [ class "flex flex-col items-center gap-1" ]
            [ div [ class "text-sm font-bold" ]
                [ text ("Level " ++ String.fromInt currentLevel ++ "/" ++ String.fromInt stats.maxLevels) ]
            , div [ class "text-xs" ]
                [ text currentEffect ]
            ]
        , if maxLevelReached then
            div [ class "text-sm font-bold text-success" ] [ text "MAX LEVEL" ]

          else
            div [ class "flex flex-col gap-1" ] mineralCosts
        , button
            [ class "btn btn-sm btn-primary"
            , classList
                [ ( "btn-disabled", not canUpgrade || maxLevelReached )
                , ( "opacity-50", not canUpgrade || maxLevelReached )
                ]
            , onClick (HandleProjectUpgrade project)
            ]
            [ if maxLevelReached then
                text "MAX"

              else
                text "UPGRADE"
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
                                , renderDrawerTabRow model ProjectsTab
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

                                ProjectsTab ->
                                    renderProjectsTab model

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
        , Html.node "dialog"
            [ id "my_modal_1", class "modal" ]
            [ div [ class "modal-box" ]
                [ h3 [ class "text-lg font-bold" ] [ text "Reset Game" ]
                , p [ class "py-4" ] [ text "Are you sure you want to reset the game? All progress will be lost." ]
                , div [ class "modal-action" ]
                    [ Html.form [ method "dialog" ]
                        [ button
                            [ class "btn btn-error", onClick ResetGame ]
                            [ text "Yes, Reset Game" ]
                        ]
                    , Html.form [ method "dialog" ]
                        [ button [ class "btn" ] [ text "Cancel" ] ]
                    ]
                ]
            , Html.form [ Html.Attributes.attribute "method" "dialog", class "modal-backdrop" ]
                [ button [] [ text "close" ]
                ]
            ]
        ]
