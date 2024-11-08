module TimerTest exposing (..)

import Duration exposing (Duration)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Utils.Percent exposing (Percent)
import Utils.Timer


timerTest : Test
timerTest =
    describe "Timer"
        [ test "percentComplete works" <|
            \() ->
                let
                    testPercent : Percent
                    testPercent =
                        Utils.Percent.float 0.5
                in
                Expect.equal (Utils.Timer.percentComplete (Utils.Timer.createAtPercent testPercent)) testPercent
        , describe "increment" <|
            [ test "increment sets the correct percent complete" <|
                \() ->
                    let
                        totalDuration : Duration
                        totalDuration =
                            Duration.seconds 10

                        delta : Duration
                        delta =
                            Duration.seconds 1

                        initialTimer : Utils.Timer.Timer
                        initialTimer =
                            Utils.Timer.create

                        incrementedTimer : Utils.Timer.Timer
                        incrementedTimer =
                            let
                                ( newTimer, _ ) =
                                    Utils.Timer.increment totalDuration delta initialTimer
                            in
                            newTimer
                    in
                    Expect.equal (Utils.Timer.percentComplete incrementedTimer) (Utils.Percent.float 0.1)
            , test "increment past 100% has the correct percent complete" <|
                \() ->
                    let
                        totalDuration : Duration
                        totalDuration =
                            Duration.seconds 10

                        delta : Duration
                        delta =
                            Duration.seconds 15

                        initialTimer : Utils.Timer.Timer
                        initialTimer =
                            Utils.Timer.create

                        incrementedTimer : Utils.Timer.Timer
                        incrementedTimer =
                            let
                                ( newTimer, _ ) =
                                    Utils.Timer.increment totalDuration delta initialTimer
                            in
                            newTimer
                    in
                    Expect.equal (Utils.Timer.percentComplete incrementedTimer) (Utils.Percent.float 0.5)
            , test "increment past 100% registers a completion" <|
                \() ->
                    let
                        totalDuration : Duration
                        totalDuration =
                            Duration.seconds 10

                        delta : Duration
                        delta =
                            Duration.seconds 15

                        initialTimer : Utils.Timer.Timer
                        initialTimer =
                            Utils.Timer.create

                        ( _, completions ) =
                            Utils.Timer.increment totalDuration delta initialTimer
                    in
                    Expect.equal completions 1
            , test "increment past 200% registers two completions" <|
                \() ->
                    let
                        totalDuration : Duration
                        totalDuration =
                            Duration.seconds 10

                        delta : Duration
                        delta =
                            Duration.seconds 25

                        initialTimer : Utils.Timer.Timer
                        initialTimer =
                            Utils.Timer.create

                        ( _, completions ) =
                            Utils.Timer.increment totalDuration delta initialTimer
                    in
                    Expect.equal completions 2
            ]
        , describe "durationLeft" <|
            [ test "durationLeft returns correct value" <|
                \() ->
                    let
                        totalDuration : Duration
                        totalDuration =
                            Duration.seconds 10

                        delta : Duration
                        delta =
                            Duration.seconds 1

                        ( newTimer, _ ) =
                            Utils.Timer.increment totalDuration delta Utils.Timer.create
                    in
                    Expect.equal (Utils.Timer.durationLeft totalDuration newTimer) (Duration.seconds 9)
            ]
        ]
