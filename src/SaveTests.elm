module SaveTests exposing (..)

import DwarfXp
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Json.Encode as E
import Main exposing (defaultModel)
import Random
import Save
import SaveTestsData
import Test exposing (..)
import Time
import Types exposing (..)
import Utils.Percent
import Utils.Record
import Utils.Timer



{--| For these tests we want to keep a test of the decoder for all previous versions we still want to support. We have one or more examples for each version
that we run through the same current decoder and verify we get something correct out of it.

For the current version we should test both the decoder and encoder (it doesn't make sense to test the encoder for previous versions). Probably the easiest way
to go about this is take a given model, encode it then decode it, and verify it's still the same model. We might need to make some adjustments because the
encoding process ignores some things like saveTimer on purpose that don't need to be saved.
-}


expectLevel : Int -> Result D.Error Model -> Expectation
expectLevel expectedLevel result =
    case result of
        Err _ ->
            Expect.fail "Failed to decode"

        Ok model ->
            Expect.equal expectedLevel model.level


v0_1DecoderTest : Test
v0_1DecoderTest =
    describe "v0.1 decoder"
        [ describe "example1"
            [ test "decodes ok" <|
                \() ->
                    let
                        result : Result D.Error Model
                        result =
                            D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0)) SaveTestsData.v0_1Example1
                    in
                    Expect.ok result
            , test "decodes the right player level" <|
                \() ->
                    let
                        result : Result D.Error Model
                        result =
                            D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0)) SaveTestsData.v0_1Example1
                    in
                    expectLevel 1 result
            ]
        ]


{-| Tests the encoder and decoder of current version by encoding then decoding.
-}
currentVersionTest : Test
currentVersionTest =
    describe "current version"
        [ describe "example1"
            [ test "encodes and decodes ok" <|
                \() ->
                    let
                        model : Model
                        model =
                            defaultModel (Random.initialSeed 0) (Time.millisToPosix 0)
                    in
                    Save.encoder model
                        |> E.encode 0
                        |> D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0))
                        |> Expect.ok
            , test "encodes and decodes to the same model" <|
                \() ->
                    let
                        model : Model
                        model =
                            defaultModel (Random.initialSeed 0) (Time.millisToPosix 0)
                                |> (\m -> { m | level = 6 })
                    in
                    Save.encoder model
                        |> E.encode 0
                        |> D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0))
                        |> Expect.equal (Ok model)
            , test "encodes and decodes to the same model after changing the dwarf xp" <|
                \() ->
                    let
                        model : Model
                        model =
                            defaultModel (Random.initialSeed 0) (Time.millisToPosix 0)
                                |> (\m -> { m | level = 6, dwarfXp = Utils.Record.dwarfRecord (DwarfXp.float 2) })
                    in
                    Save.encoder model
                        |> E.encode 0
                        |> D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0))
                        |> Expect.equal (Ok model)
            , test "encodes and decodes to the same model after activating a dwarf xp boost" <|
                \() ->
                    let
                        model : Model
                        model =
                            defaultModel (Random.initialSeed 0) (Time.millisToPosix 0)
                                |> (\m -> { m | dwarfXpButtonStatuses = Utils.Record.dwarfXpButtonRecord (ButtonOnCooldown (Utils.Timer.createAtPercent (Utils.Percent.float 0.5))) })
                    in
                    Save.encoder model
                        |> E.encode 0
                        |> D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0))
                        |> Expect.equal (Ok model)
            , test "encodes and decodes to the same model after changing the theme" <|
                \() ->
                    let
                        model : Model
                        model =
                            defaultModel (Random.initialSeed 0) (Time.millisToPosix 0)
                                |> (\m -> { m | theme = Just DefaultDark })
                    in
                    Save.encoder model
                        |> E.encode 0
                        |> D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0))
                        |> Expect.equal (Ok model)
            ]
        ]
