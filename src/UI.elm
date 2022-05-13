module UI exposing (cardBody, cardContainer, cardHeader, cardHeaderToggleButton, cardTitle, cardsGrid, pageHeader, resourceLink, resourcesContainer)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


pageHeader : List (Html msg) -> Html msg
pageHeader content =
    div [ class "text-xl sm:text-2xl md:text-3xl font-bold mb-4 text-center w-full p-2" ] content


cardHeaderToggleButton : Bool -> String -> String -> msg -> Html msg
cardHeaderToggleButton isActive activeTitle inactiveTitle toMsg =
    if isActive then
        div [] [ text activeTitle ]

    else
        button [ class "btn btn-sm", onClick toMsg ] [ text inactiveTitle ]


cardHeader : Bool -> List (Html msg) -> Html msg
cardHeader isActive content =
    div [ class "mb-4 text-white px-4 py-2 sm:flex justify-between items-center uppercase", classList [ ( "bg-green-800", isActive ), ( "bg-gray-800", isActive == False ) ] ] content


cardTitle : String -> Html msg
cardTitle title =
    div [ class "font-bold text-2xl mb-1 sm:mb-0" ] [ text title ]


cardsGrid : Int -> List (Html msg) -> Html msg
cardsGrid columnCount content =
    div [ class "lg:grid grid-cols-3 gap-4" ] content


cardContainer : List (Html msg) -> Html msg
cardContainer content =
    div [ class "bg-gray-200 pb-6" ] content


cardBody : List (Html msg) -> Html msg
cardBody content =
    div [ class "px-6 py-2" ] content


resourcesContainer : String -> String -> List (Html msg) -> Html msg
resourcesContainer title subhead content =
    div [ class "mt-8 text-lg align-left" ]
        [ div [ class "font-bold" ] [ text title ]
        , div [ class "mb-2" ] [ text subhead ]
        , div [] content
        ]


resourceLink : String -> msg -> Html msg
resourceLink title toMsg =
    div [] [ button [ class "text-left text-blue-500 mb-2", onClick toMsg ] [ text title ] ]
