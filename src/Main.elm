port module Main exposing (defaultModel, main)

import Browser
import Browser.Events
import Config
import Dict exposing (Dict)
import Duration exposing (Duration, hours)
import DwarfXp
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
import Random
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


defaultModel : Random.Seed -> Time.Posix -> Model
defaultModel initialSeed now =
    { seed = initialSeed
    , currentTime = now
    , currentTab = MissionsTab
    , saveTimer = Utils.Timer.create
    , theme = Nothing
    , level = 1
    , credits = 0
    , resources = { gold = 0 }
    , missionStatuses =
        { haz1 = ButtonReady
        , haz2 = ButtonReady
        , haz3 = ButtonReady
        , haz4 = ButtonReady
        , haz5 = ButtonReady
        }
    , gameSpeed = 1.0
    , debugAddedTime = Quantity.zero
    , animations = []
    , dwarfXp = Utils.Record.dwarfRecord (DwarfXp.float 0)
    , dwarfXpButtonStatuses = Utils.Record.dwarfXpButtonRecord ButtonReady
    }


init : Flags -> ( Model, Cmd Msg )
init { initialSeed, now, initialGame } =
    let
        randomSeed : Random.Seed
        randomSeed =
            Random.initialSeed initialSeed
    in
    case D.decodeValue (Save.decodeAnyVersion randomSeed) initialGame of
        Err _ ->
            ( defaultModel randomSeed (Time.millisToPosix now)
            , Cmd.none
            )

        Ok model ->
            ( model, Cmd.none )



-- UPDATE


animationDuration : Duration
animationDuration =
    Duration.seconds 2


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
                        + floor (Duration.inMilliseconds model.debugAddedTime)
                        |> Time.millisToPosix

                delta : Duration
                delta =
                    Time.posixToMillis newCurrentTime
                        - Time.posixToMillis model.currentTime
                        |> toFloat
                        |> (\x -> x * model.gameSpeed)
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
                            (Utils.Record.getByDwarfXpButton dwarfXpButton Config.dwarfXpButtonStats).duration
                    in
                    Utils.Record.updateByDwarfXpButton dwarfXpButton (updateButton delta buttonDuration) statuses

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

                updateAnimation : Maybe Animation -> List (Maybe Animation) -> List (Maybe Animation)
                updateAnimation maybeAnimation animations =
                    case maybeAnimation of
                        Nothing ->
                            Nothing :: animations

                        Just (Animation timer duration subject) ->
                            let
                                ( newTimer, completions ) =
                                    Utils.Timer.increment duration delta timer

                                newAnimation : Animation
                                newAnimation =
                                    Animation newTimer duration subject
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
                , missionStatuses = List.foldl updateMissionStatuses model.missionStatuses Utils.Record.allMissions
                , dwarfXpButtonStatuses = List.foldl updateDwarfXpButtonStatuses model.dwarfXpButtonStatuses Utils.Record.allDwarfXpButtons
                , saveTimer = newSaveTimer
                , animations = newAnimations
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

                modifiedYield : MissionYield
                modifiedYield =
                    modifyYield model stats.yield

                newMissionStatuses : MissionRecord ButtonStatus
                newMissionStatuses =
                    Utils.Record.setByMission mission (ButtonOnCooldown Utils.Timer.create) model.missionStatuses

                newResources : ResourceRecord Int
                newResources =
                    Utils.Record.addResourceRecords modifiedYield.resources model.resources

                addCreditsResult : Model
                addCreditsResult =
                    addCredits modifiedYield.credits model

                newAnimation : Animation
                newAnimation =
                    Animation Utils.Timer.create animationDuration (AnimateCreditsGain modifiedYield.credits (adjustClientPos event.pointer.clientPos))
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

        HandleDwarfXpButtonClick dwarfXpButton event ->
            let
                stats : DwarfXpButtonStats
                stats =
                    Utils.Record.getByDwarfXpButton dwarfXpButton Config.dwarfXpButtonStats

                newStatuses : DwarfXpButtonRecord ButtonStatus
                newStatuses =
                    Utils.Record.setByDwarfXpButton dwarfXpButton (ButtonOnCooldown Utils.Timer.create) model.dwarfXpButtonStatuses

                ( ( dwarf, newDwarfXp ), newSeed ) =
                    Random.step (dwarfXpGenerator stats.xp model.dwarfXp) model.seed

                currentLevel : Int
                currentLevel =
                    DwarfXp.level (Utils.Record.getByDwarf dwarf model.dwarfXp)

                newLevel : Int
                newLevel =
                    DwarfXp.level (Utils.Record.getByDwarf dwarf newDwarfXp)

                xpGainAnimation : Animation
                xpGainAnimation =
                    Animation Utils.Timer.create animationDuration (AnimateDwarfXp dwarf stats.xp (adjustClientPos event.pointer.clientPos))

                levelUpAnimation : Animation
                levelUpAnimation =
                    let
                        message : String
                        message =
                            (Utils.Record.getByDwarf dwarf Config.dwarfStats).name ++ " leveled up (" ++ String.fromInt currentLevel ++ " -> " ++ String.fromInt newLevel ++ ")"
                    in
                    Animation Utils.Timer.create (Duration.seconds 5) (AnimateAlert message AlertInfo)

                newAnimations : List (Maybe Animation)
                newAnimations =
                    if newLevel > currentLevel then
                        [ Just xpGainAnimation, Just levelUpAnimation ]

                    else
                        [ Just xpGainAnimation ]
            in
            ( { model
                | dwarfXpButtonStatuses = newStatuses
                , dwarfXp = newDwarfXp
                , seed = newSeed
                , animations = List.append model.animations newAnimations
              }
            , Cmd.none
            )

        HandleSetThemeClick theme ->
            ( { model | theme = Just theme }, Cmd.none )

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

        HandleTabClick tab ->
            ( { model | currentTab = tab }, Cmd.none )


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
            div [ class "flex w-full gap-3 text-center foobar justify-center" ]

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


modifyYield : Model -> MissionYield -> MissionYield
modifyYield model yield =
    let
        creditsMultiplier : Float
        creditsMultiplier =
            squadMultiplier model
    in
    { yield | credits = yield.credits * creditsMultiplier }


renderMissionRow : Model -> Mission -> Html Msg
renderMissionRow model mission =
    let
        stats : MissionStats
        stats =
            Utils.Record.getByMission mission Config.missionStats

        missionStatus : ButtonStatus
        missionStatus =
            Utils.Record.getByMission mission model.missionStatuses

        yield : MissionYield
        yield =
            stats.yield

        modifiedYield : MissionYield
        modifiedYield =
            modifyYield model yield

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

        buttonText : String
        buttonText =
            case missionStatus of
                ButtonReady ->
                    "Gain " ++ floatToFixedDecimalString modifiedYield.credits 2 ++ "m credits"

                ButtonOnCooldown _ ->
                    "On cooldown"
    in
    tr [ class "h-20 relative" ]
        [ td []
            [ div [ class "flex items-center gap-2" ]
                (List.concat
                    [ [ span [] [ text stats.title ] ]
                    , [ icons ]
                    ]
                )
            ]
        , td [ class "overflow-hidden relative" ]
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
            div [ class "flex items-center gap-8 w-full" ]
                [ div [ class "relative" ]
                    [ button
                        [ class "btn text-xl"
                        , buttonVariantClass
                        , Pointer.onDown msg
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
            div [ class "flex items-center gap-8 w-full" ] [ renderDuration durationLeft hasTickedAVeryShortTime ]


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
            class "w-full h-16 border border-neutral relative flex items-center justify-center gap-1 overflow-hidden themed-rounded-borders"

        progressBarTextClass : Attribute Msg
        progressBarTextClass =
            class "px-3 py-1 rounded bg-base-100 text-base-content text-3xl leading-none text-xl"
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
                , case Utils.Unlocks.nextUnlock model.level Config.levelUnlockStats of
                    Nothing ->
                        div [] []

                    Just ( unlockLevel, unlockStats ) ->
                        renderNextUnlock model unlockLevel unlockStats
                ]
            , renderProgressBar model
            ]
        ]


renderAtLocation : AnimationLocation -> List (Html Msg) -> Html Msg
renderAtLocation ( x, y ) children =
    div
        [ class "fixed animate-smokeRise pointer-events-none"
        , style "left" (String.fromFloat x ++ "px")
        , style "top" (String.fromFloat y ++ "px")
        ]
        children


renderAnimation : Maybe Animation -> Html Msg
renderAnimation maybeAnimation =
    case maybeAnimation of
        Nothing ->
            div [] []

        Just (Animation _ _ subject) ->
            let
                gainTextClass : Attribute Msg
                gainTextClass =
                    class "text-xl pointer-events-none bg-opacity-0 drop-shadow-[0_1.2px_1.2px_rgba(0,0,0,0.8)] font-extrabold"
            in
            case subject of
                AnimateCreditsGain amount location ->
                    renderAtLocation location
                        [ span [ class "text-primary", gainTextClass ]
                            [ text "+ "
                            , text (floatToString amount)
                            , text " credits "
                            , credits1Img
                            ]
                        ]

                AnimateDwarfXp dwarf xp location ->
                    let
                        dwarfStats : DwarfStats
                        dwarfStats =
                            Utils.Record.getByDwarf dwarf Config.dwarfStats
                    in
                    renderAtLocation location
                        [ span [ gainTextClass, class "text-secondary" ]
                            [ text dwarfStats.name
                            , text " gained "
                            , text (DwarfXp.toString xp)
                            , text " xp"
                            ]
                        ]

                _ ->
                    div [] []


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
            DwarfXp.level xp

        percentInLevel : Percent
        percentInLevel =
            DwarfXp.percentInLevel xp

        xpInLevelNumerator : DwarfXp
        xpInLevelNumerator =
            DwarfXp.flatXpInCurrentLevel xp

        progressInLevelSpan : Html Msg
        progressInLevelSpan =
            case Config.dwarfLevelingSchedule (DwarfXp.level xp) of
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
    div [ class "bg-base-300 text-base-content flex flex-col items-center relative overflow-hidden themed-rounded-borders shadow" ]
        [ span [ class "w-full flex items-center justify-center gap-2" ]
            [ span [ class "text-lg" ] [ text stats.name ]
            ]
        , img [ src dwarfImgSrc, class "h-24 rounded " ] []
        , span []
            [ progressInLevelSpan
            ]
        , div [ class "tooltip tooltip-left absolute top-0 right-0", attribute "data-tip" ("Level " ++ String.fromInt level) ]
            [ span
                [ class "inline-block bg-secondary text-secondary-content px-3 py-1 text-2xl border border-secondary-content themed-rounded-borders"
                ]
                [ text (String.fromInt level) ]
            ]
        , progress [ class "progress progress-secondary xp-bar", value (String.fromFloat (Utils.Percent.toPercentage percentInLevel)), attribute "max" "100" ] []
        ]


renderMissionsTab : Model -> Html Msg
renderMissionsTab model =
    let
        unlockedMissions : List Mission
        unlockedMissions =
            List.filter (Utils.Unlocks.missionIsUnlocked model.level) Utils.Record.allMissions
    in
    div [ class "flex flex-col items-center gap-8 p-8 flex-grow overflow-scroll" ]
        [ div [ proseClass ]
            [ h2 [] [ text "Missions" ]
            ]
        , table [ class "table table-fixed w-[750px] max-w-full" ]
            [ tbody []
                (List.map (renderMissionRow model) unlockedMissions)
            ]
        ]


renderCommendationsTab : Model -> Html Msg
renderCommendationsTab model =
    let
        unlockedXpButtons : List DwarfXpButton
        unlockedXpButtons =
            List.filter (Utils.Unlocks.dwarfXpButtonIsUnlocked model.level) Utils.Record.allDwarfXpButtons
    in
    div [ class "flex flex-col items-center gap-8 p-8 flex-grow overflow-y-scroll" ]
        [ div [ proseClass ]
            [ h2 [] [ text "Dwarf Leveling" ]
            ]
        , div [ class "flex flex-col item-center gap-4 w-[300px]" ]
            (unlockedXpButtons
                |> List.map
                    (\dwarfXpButton ->
                        let
                            stats : DwarfXpButtonStats
                            stats =
                                Utils.Record.getByDwarfXpButton dwarfXpButton Config.dwarfXpButtonStats
                        in
                        renderButton
                            model
                            (Utils.Record.getByDwarfXpButton dwarfXpButton model.dwarfXpButtonStatuses)
                            stats.duration
                            (HandleDwarfXpButtonClick dwarfXpButton)
                            ButtonSecondary
                            [ text "+", text (DwarfXp.toString stats.xp), text " xp to random dwarf" ]
                    )
            )
        ]


renderDrawerTabRow : Model -> Tab -> Html Msg
renderDrawerTabRow model tab =
    let
        stats : TabStats
        stats =
            Utils.Record.getByTab tab Config.tabStats
    in
    li [ onClick (HandleTabClick tab), class "relative" ]
        [ span
            [ class "flex gap-4 items-center"
            , classList [ ( "active", model.currentTab == tab ) ]
            ]
            [ span [ class "flex-none" ]
                [ stats.icon
                    |> FeatherIcons.toHtml []
                ]
            , span [ class "flex-1" ] [ text stats.title ]
            ]
        , span
            [ class "flex h-3 w-3 absolute top-0 right-0 -mt-1 -mr-1 p-0"
            , classList [ ( "hidden", numActiveItemsInTab model tab == 0 ) ]
            ]
            [ span [ class "animate-ping absolute inline-flex h-full w-full rounded-full bg-primary opacity-75" ] []
            , span [ class "relative inline-flex rounded-full h-3 w-3 bg-primary" ] []
            ]
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
            Utils.Record.allDwarfXpButtons
                |> List.filter (Utils.Unlocks.dwarfXpButtonIsUnlocked model.level)
                |> List.filter (\dwarfXpButton -> Utils.Record.getByDwarfXpButton dwarfXpButton model.dwarfXpButtonStatuses == ButtonReady)
                |> List.length


squadMultiplier : Model -> Float
squadMultiplier model =
    Utils.Record.allDwarfs
        |> List.map (\dwarf -> DwarfXp.level (Utils.Record.getByDwarf dwarf model.dwarfXp))
        |> List.sum
        |> (\s -> s - 4)
        -- Divide by 100 since it's a percentage i.e. sum of 10 levels = 0.1 multiplier
        |> toFloat
        |> (\x -> x / 100)
        |> (+) 1


renderSquadMultiplier : Model -> Html Msg
renderSquadMultiplier model =
    div [ class "flex flex-col items-center" ]
        [ span [ class "w-full flex items-center justify-center gap-1" ]
            [ span [ class "text-sm" ] [ text "Squad Multiplier" ]

            -- , div [ class "tooltip tooltip-left", attribute "data-tip" "Squad Multiplier increases yield from missions.\nIncrease it by leveling up dwarfs." ]
            , div [ class "tooltip tooltip-left", attribute "data-tip" "Increases mission yield" ]
                [ FeatherIcons.info
                    |> FeatherIcons.withSize 16
                    |> FeatherIcons.toHtml []
                ]
            ]
        , div [ class "text-xl font-bold" ]
            [ text ("x" ++ floatToString (squadMultiplier model)) ]
        ]


renderAnimationAsAlert : AnimationSubject -> Html Msg
renderAnimationAsAlert animation =
    case animation of
        AnimateAlert message alertType ->
            let
                alertClass : Attribute Msg
                alertClass =
                    case alertType of
                        AlertSuccess ->
                            class "alert-success"

                        AlertError ->
                            class "alert-error"

                        AlertWarning ->
                            class "alert-warning"

                        AlertInfo ->
                            class "alert-info"
            in
            div
                [ class "alert shadow mb-2"
                , alertClass
                ]
                [ text message ]

        _ ->
            div [] []


view : Model -> Html Msg
view model =
    let
        heightMinusHeader : Attribute Msg
        heightMinusHeader =
            -- Don't fully understand why this is necessary to make elements have the right height and scroll properly
            style "height" "calc(100vh - 192px)"
    in
    div [ class "w-screen h-screen overflow-hidden flex flex-col items-center bg-base-100" ]
        [ -- Header
          renderHeader model

        -- Body
        , div [ class "flex w-full drawer lg:drawer-open", heightMinusHeader ]
            [ input [ id "my-drawer", type_ "checkbox", class "drawer-toggle" ] []
            , div [ class "drawer-side w-64 min-w-64", attribute "style" "scroll-behavior: smooth; scroll-padding-top:5rem" ]
                [ label [ for "my-drawer", class "drawer-overlay", attribute "aria-label" "close sidebar" ] []
                , aside
                    [ class "bg-base-200 overflow-y-scroll h-full"
                    , heightMinusHeader
                    ]
                    [ div [ class "bg-base-200 sticky top-0 z-10 w-full bg-opacity-90 py-3 px-2 backdrop-blur flex" ]
                        [ div [ class "flex-1 flex items-center justify-between gap-2 px-4" ]
                            [ div [ class "flex-0 px-2 flex flex-col items-center" ]
                                [ div [ class "font-title text-primary inline-flex text-lg transition-all duration-200 md:text-3xl flex gap-1 items-center rounded-t-xl overflow-hidden p-1 border border-primary border-b-4" ]
                                    [ span [ class "uppercase text-base-content text-primary-content bg-primary leading-none px-1" ] [ text "DRG" ]
                                    , div [ class "text-primary text-sm font-bold t-column gap-0 leading-xs text-primary" ] [ span [] [ text "Mission Control" ] ]
                                    ]
                                , div [ class "w-full border border-primary flex justify-center" ] [ div [ class "text-xs" ] [ text "An ", strong [ class "text-primary" ] [ text "Ulta Idle" ], text " experience" ] ]
                                ]
                            ]

                        -- Close drawer button
                        , label [ for "drawer", class "btn btn-square btn-ghost drawer-button lg:hidden" ]
                            [ FeatherIcons.x
                                |> FeatherIcons.toHtml []
                            ]
                        ]
                    , div [ class "h-4" ] []
                    , ul
                        [ class "menu flex flex-col p-0 px-4 w-64"
                        , classList [ ( "hidden", not (Utils.Unlocks.dwarfXpButtonsFeatureUnlocked model.level) ) ]
                        ]
                        [ renderDrawerTabRow model MissionsTab
                        , renderDrawerTabRow model CommendationsTab
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
                    , div [ class "h-full w-[250px] bg-base-200 items-center p-4 overflow-y-scroll" ]
                        [ div
                            [ class "flex flex-col gap-2"
                            , classList
                                [ ( "hidden", not (Utils.Unlocks.dwarfXpButtonsFeatureUnlocked model.level) )
                                ]
                            ]
                            (List.concat
                                [ [ renderSquadMultiplier model ]
                                , List.map
                                    (renderDwarf model)
                                    Utils.Record.allDwarfs
                                ]
                            )
                        ]
                    ]
                ]
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
            ]
        , div []
            (List.map renderAnimation model.animations)
        , div
            [ class "toast toast-center toast-bottom"
            , class "gap-0" -- Note we do the zero gap plus margin bottom on toast strategy because we render toasts as empty divs after they expire for a period of time. It looks weird if the empty divs add space through non-zero gap.
            ]
            (model.animations
                |> List.filterMap identity
                |> List.map (\(Animation _ _ subject) -> subject)
                |> List.map renderAnimationAsAlert
            )
        ]
