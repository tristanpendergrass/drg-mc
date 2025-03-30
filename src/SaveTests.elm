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


{-| Version-specific tests like this one act on mock data from SaveTestsData.elm. That file contains examples of the data found in local storage on our users' machines.
All future versions of the game that want to support loading saves from this version will need to pass this test.
-}
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


v_02DecoderTest : Test
v_02DecoderTest =
    describe "v0.2 decoder"
        [ test "decodes ok" <|
            \() ->
                let
                    result : Result D.Error Model
                    result =
                        D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0)) SaveTestsData.v_02Example1
                in
                Expect.ok result
        , test "decodes the right player level" <|
            \() ->
                let
                    result : Result D.Error Model
                    result =
                        D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0)) SaveTestsData.v_02Example1
                in
                expectLevel 1 result
        ]


expectEqualToModel : Model -> Result D.Error Model -> Expectation
expectEqualToModel expected result =
    let
        expected2 =
            { expected | seed = Random.initialSeed 0 }

        result2 =
            Result.map (\r -> { r | seed = Random.initialSeed 0 }) result
    in
    Expect.equal (Ok expected2) result2


testEncodeDecode : String -> (Model -> Model) -> Test
testEncodeDecode description modifyModel =
    test description <|
        \() ->
            let
                model : Model
                model =
                    defaultModel (Random.initialSeed 0) (Time.millisToPosix 0)
                        |> modifyModel
            in
            Save.encoder model
                |> E.encode 0
                |> D.decodeString (Save.decodeAnyVersion (Random.initialSeed 0))
                |> expectEqualToModel model


{-| The currentVersionTest does not use test mock data, but instead creates a json string then parses it, all using the current version encoder and decoder.
There's usually a benefit to testing all the fields in the model.
-}
currentVersionTest : Test
currentVersionTest =
    describe "current version"
        [ testEncodeDecode "encodes and decodes to the same model after changing the level"
            (\m -> { m | level = 6 })
        , testEncodeDecode "encodes and decodes to the same model after changing the dwarf xp"
            (\m -> { m | dwarfXp = Utils.Record.dwarfRecord (DwarfXp.float 2) })
        , testEncodeDecode "encodes and decodes to the same model after activating a dwarf xp boost"
            (\m -> { m | dwarfXpButtonStatuses = dwarfXpButtonRecord (ButtonOnCooldown (Utils.Timer.createAtPercent (Utils.Percent.float 0.5))) })
        , testEncodeDecode "encodes and decodes to the same model after changing the theme"
            (\m -> { m | theme = Just DefaultDark })
        , testEncodeDecode "encodes and decodes to the same model after changing current tab"
            (\m -> { m | currentTab = ProjectsTab })
        , testEncodeDecode "encodes and decodes to the same model after changing morkite amount"
            (\m -> { m | morkite = 1000.0 })
        , testEncodeDecode "encodes and decodes to the same model after changing active daily specials"
            (\m -> { m | activeDailySpecials = [ ( DarkMorkite, Utils.Timer.create ) ] })
        , testEncodeDecode "encodes and decodes to the same model after changing mission biome"
            (\m -> { m | missionBiome = Just FungusBogs })
        , testEncodeDecode "encodes and decodes to the same model after changing mission statuses"
            (\m ->
                { m
                    | missionStatuses =
                        { haz1 = ButtonOnCooldown (Utils.Timer.createAtPercent (Utils.Percent.float 0.3))
                        , haz2 = ButtonReady
                        , haz3 = ButtonReady
                        , haz4 = ButtonReady
                        , haz5 = ButtonReady
                        }
                }
            )
        , testEncodeDecode "encodes and decodes to the same model after changing daily special cooldown"
            (\m -> { m | dailySpecialCooldown = ButtonOnCooldown (Utils.Timer.createAtPercent (Utils.Percent.float 0.7)) })
        , testEncodeDecode "encodes and decodes to the same model after changing daily special options"
            (\m -> { m | dailySpecialOptions = [ DarkMorkite, RockyMountain, PotsOGold ] })
        , testEncodeDecode "encodes and decodes to the same model after changing minerals"
            (\m -> { m | minerals = mineralRecord 500.0 })
        , testEncodeDecode "encodes and decodes to the same model after changing project levels"
            (\m -> { m | projectLevels = projectRecord 3 })
        ]
