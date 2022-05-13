module Forms exposing (formControl)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List exposing (any)


type alias FormControlErrors =
    List String


formControl : String -> String -> FormControlErrors -> String -> Bool -> (String -> msg) -> Html msg
formControl labelFor valueFor errors hint isDisabled toMsg =
    div [ class "form-control mb-2" ]
        [ label [ class "label font-bold" ]
            [ span [ class "label-text text-gray-700" ] [ text labelFor ]
            ]
        , input [ type_ "text", placeholder labelFor, value valueFor, disabled isDisabled, class "input input-bordered", onInput toMsg ] []
        , renderHint hint
        , div []
            (List.map renderError errors)
        ]


renderHint : String -> Html msg
renderHint hintText =
    span [ class "label-text ml-2 mt-1" ] [ text hintText ]


renderError : String -> Html msg
renderError error =
    label [ class "label" ]
        [ span [ class "label-text text-error" ] [ text error ]
        ]
