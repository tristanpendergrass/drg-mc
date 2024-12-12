module Theme exposing (..)

import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)
import Utils.Unlocks


allThemes : List Theme
allThemes =
    [ Default, DefaultLight, DefaultDark, Retro, Cyberpunk, Black, Luxury ]


themeToString : Theme -> String
themeToString theme =
    -- Don't forget to update stringToTheme and allThemes as well! Compiler won't catch this!
    case theme of
        Default ->
            "default"

        DefaultLight ->
            "light"

        DefaultDark ->
            "dark"

        Retro ->
            "retro"

        Cyberpunk ->
            "cyberpunk"

        Black ->
            "black"

        Luxury ->
            "luxury"


stringToTheme : String -> Maybe Theme
stringToTheme str =
    case str of
        "default" ->
            Just Default

        "light" ->
            Just DefaultLight

        "dark" ->
            Just DefaultDark

        "retro" ->
            Just Retro

        "cyberpunk" ->
            Just Cyberpunk

        "black" ->
            Just Black

        "luxury" ->
            Just Luxury

        _ ->
            Nothing


defaultTheme : Theme
defaultTheme =
    Default


renderThemeDropdown : Model -> Maybe Theme -> Html Msg
renderThemeDropdown model maybeActiveTheme =
    let
        filteredThemes : List Theme
        filteredThemes =
            List.concat
                [ [ Default, DefaultLight, DefaultDark ]
                , List.filter (\theme -> Utils.Unlocks.themeIsUnlocked model.level theme) allThemes
                ]
    in
    div
        [ class "dropdown dropdown-top dropdown-start" ]
        [ div
            [ tabindex 0
            , attribute "role" "button"
            , class "btn m-1"
            ]
            [ text "Theme"
            , FeatherIcons.chevronUp
                |> FeatherIcons.withSize 16
                |> FeatherIcons.withClass "inline-block opacity-60"
                |> FeatherIcons.toHtml []

            -- , svg
            --     [ width "12px"
            --     , height "12px"
            --     , class "inline-block h-2 w-2 fill-current opacity-60"
            --     , xmlns "http://www.w3.org/2000/svg"
            --     , viewBox "0 0 2048 2048"
            --     ]
            --     [ path
            --         [ d "M1799 349l242 241-1017 1017L7 590l242-241 775 775 775-775z"
            --         ]
            --         []
            --     ]
            ]
        , ul
            [ tabindex 0
            , class "dropdown-content bg-base-300 rounded-box z-[1] w-52 p-2 shadow-2xl"
            ]
            (List.map
                (\theme ->
                    let
                        isActive : Bool
                        isActive =
                            case maybeActiveTheme of
                                Just activeTheme ->
                                    theme == activeTheme

                                Nothing ->
                                    theme == defaultTheme
                    in
                    li []
                        [ input
                            [ type_ "radio"
                            , name "theme-dropdown"
                            , class "theme-controller btn btn-sm btn-block btn-ghost justify-start"
                            , attribute "aria-label" (themeToString theme)
                            , value (themeToString theme)
                            , checked isActive
                            , onClick (HandleSetThemeClick theme)
                            ]
                            []
                        ]
                )
                filteredThemes
            )
        ]



-- <div class="dropdown mb-72">
--   <div tabindex="0" role="button" class="btn m-1">
--     Theme
--     <svg
--       width="12px"
--       height="12px"
--       class="inline-block h-2 w-2 fill-current opacity-60"
--       xmlns="http://www.w3.org/2000/svg"
--       viewBox="0 0 2048 2048">
--       <path d="M1799 349l242 241-1017 1017L7 590l242-241 775 775 775-775z"></path>
--     </svg>
--   </div>
--   <ul tabindex="0" class="dropdown-content bg-base-300 rounded-box z-[1] w-52 p-2 shadow-2xl">
--     <li>
--       <input
--         type="radio"
--         name="theme-dropdown"
--         class="theme-controller btn btn-sm btn-block btn-ghost justify-start"
--         aria-label="Default"
--         value="default" />
--     </li>
--     <li>
--       <input
--         type="radio"
--         name="theme-dropdown"
--         class="theme-controller btn btn-sm btn-block btn-ghost justify-start"
--         aria-label="Retro"
--         value="retro" />
--     </li>
--     <li>
--       <input
--         type="radio"
--         name="theme-dropdown"
--         class="theme-controller btn btn-sm btn-block btn-ghost justify-start"
--         aria-label="Cyberpunk"
--         value="cyberpunk" />
--     </li>
--     <li>
--       <input
--         type="radio"
--         name="theme-dropdown"
--         class="theme-controller btn btn-sm btn-block btn-ghost justify-start"
--         aria-label="Valentine"
--         value="valentine" />
--     </li>
--     <li>
--       <input
--         type="radio"
--         name="theme-dropdown"
--         class="theme-controller btn btn-sm btn-block btn-ghost justify-start"
--         aria-label="Aqua"
--         value="aqua" />
--     </li>
--   </ul>
-- </div>
