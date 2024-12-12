module DwarfXpTest exposing (..)

import DwarfXp
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Types exposing (..)
import Utils.Percent


dwarfXpTest : Test
dwarfXpTest =
    describe "DwarfXp"
        [ describe "level function"
            [ test "has a minimum at level 1" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float -1
                    in
                    Expect.equal (DwarfXp.level xp) 1
            , test "level 1 at zero xp" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float 0
                    in
                    Expect.equal (DwarfXp.level xp) 1
            , test "level 1 at one xp" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float 1
                    in
                    Expect.equal (DwarfXp.level xp) 1
            , test "level 2 at two xp" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float 2
                    in
                    Expect.equal (DwarfXp.level xp) 2
            , test "max level is at 25" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float 1000000
                    in
                    Expect.equal (DwarfXp.level xp) 25
            ]
        , describe "xpTolevel" <|
            [ test "is 0 at level 1" <|
                \() ->
                    Expect.equal (DwarfXp.toFloat (DwarfXp.xpToLevel 1)) 0
            , test "is 2 at level 2" <|
                \() ->
                    Expect.equal (DwarfXp.toFloat (DwarfXp.xpToLevel 2)) 2
            , test "is 4 at level 3" <|
                \() ->
                    Expect.equal (DwarfXp.toFloat (DwarfXp.xpToLevel 3)) 4
            ]
        , describe "percentInLevel" <|
            [ test "is 0 at zero xp" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float 0
                    in
                    Expect.equal (DwarfXp.percentInLevel xp) Utils.Percent.zero
            , test "is 0.5 at half the xp to the next level" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float 3
                    in
                    Expect.equal (DwarfXp.percentInLevel xp) (Utils.Percent.float 0.5)
            ]
        , describe "flatXpInCurrentLevel" <|
            [ test "is 0 at zero xp" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float 0
                    in
                    Expect.equal (DwarfXp.toFloat (DwarfXp.flatXpInCurrentLevel xp)) 0
            , test "is 0 at the start of a level" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float 2
                    in
                    Expect.equal (DwarfXp.toFloat (DwarfXp.flatXpInCurrentLevel xp)) 0
            , test "is 1 at 3 xp" <|
                \() ->
                    let
                        xp =
                            DwarfXp.float 3
                    in
                    Expect.equal (DwarfXp.toFloat (DwarfXp.flatXpInCurrentLevel xp)) 1
            ]
        ]
