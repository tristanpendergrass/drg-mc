module Views.Logo exposing (renderLogo)

import Html exposing (Html, div, span, strong, text)
import Html.Attributes exposing (class)
import Types exposing (Msg)


renderLogo : Html Msg
renderLogo =
    div [ class "flex-0 px-2 flex flex-col items-center" ]
        [ div [ class "font-title text-primary inline-flex text-lg transition-all duration-200 text-8xl flex gap-1 items-center rounded-t-xl overflow-hidden p-1 border border-primary border-b-4" ]
            [ span [ class "uppercase text-base-content text-primary-content bg-primary leading-none px-1" ] [ text "DRG" ]
            , div [ class "text-primary text-sm font-bold t-column gap-0 leading-xs text-primary" ] [ span [] [ text "Mission Control" ] ]
            ]
        , div [ class "w-full border border-primary flex justify-center" ] [ div [ class "text-xs" ] [ text "An ", strong [ class "text-primary" ] [ text "Ulta Idle" ], text " experience" ] ]
        ]
