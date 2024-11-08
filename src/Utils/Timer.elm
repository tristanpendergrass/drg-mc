module Utils.Timer exposing (..)

import Duration exposing (Duration)
import Quantity exposing (Quantity)
import Utils.Percent exposing (Percent)


type Timer
    = Timer
        { current : Percent
        , hasEverTicked : Bool
        }


create : Timer
create =
    Timer { current = Utils.Percent.zero, hasEverTicked = False }


createAtPercent : Percent -> Timer
createAtPercent value =
    Timer { current = value, hasEverTicked = False }


hasTickedAVeryShortTime : Duration -> Timer -> Bool
hasTickedAVeryShortTime totalDuration (Timer { current }) =
    -- For some animations on clocks, we want to know if the timer has ticked a very short time to show a slightly different number
    let
        timeElapsed : Duration
        timeElapsed =
            Quantity.timesUnitless
                (Quantity.float (Utils.Percent.toFloat current))
                totalDuration
    in
    Quantity.lessThan (Duration.seconds 0.05) timeElapsed


percentComplete : Timer -> Percent
percentComplete (Timer { current }) =
    current


durationLeft : Duration -> Timer -> Duration
durationLeft totalDuration timer =
    let
        percentLeft : Percent
        percentLeft =
            Quantity.difference (Utils.Percent.float 1) (Utils.Percent.capAtHundred (percentComplete timer))
    in
    Quantity.timesUnitless
        (Quantity.float (Utils.Percent.toFloat percentLeft))
        totalDuration


increment : Duration -> Duration -> Timer -> ( Timer, Int )
increment totalDuration delta (Timer { current }) =
    let
        additionalPercent =
            Quantity.ratio delta totalDuration
                |> Utils.Percent.float

        sum =
            Quantity.plus current additionalPercent

        completions =
            sum
                |> Utils.Percent.toFloat
                |> floor

        newCurrent =
            Utils.Percent.toFloat sum
                - toFloat (floor (Utils.Percent.toFloat sum))
                |> Utils.Percent.float
    in
    ( Timer { current = newCurrent, hasEverTicked = True }, completions )


{-| Increments timer by duration unless timer would go beyond 100% in which case it sets
the timer to 0 and returns the remaining time
-}
incrementUntilComplete : Duration -> Duration -> Timer -> ( Timer, Duration )
incrementUntilComplete totalDuration delta timer =
    let
        ( timer_, completions ) =
            increment totalDuration delta timer
    in
    if completions > 0 then
        let
            percentRemainder =
                percentComplete timer_

            remainder =
                Quantity.timesUnitless
                    (Quantity.float (Utils.Percent.toFloat percentRemainder))
                    totalDuration

            completionRemainder =
                Quantity.timesUnitless
                    (Quantity.float (toFloat (completions - 1)))
                    totalDuration

            totalRemainder =
                Quantity.plus remainder completionRemainder
        in
        ( create, totalRemainder )

    else
        ( timer_, Quantity.zero )
