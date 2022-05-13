module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.EnergyCostCalculator as EnergyCostCalculator
import Pages.PowerTimeEnergyCalculator as PowerTimeEnergyCalculator
import Pages.VoltageCurrentPowerCalculator as VoltageCurrentPowerCalculator
import Routes exposing (Route(..))
import Time
import Url exposing (Url)


type alias Config =
    { currentTime : Int
    }


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    , config : Config
    }


type Page
    = NotFound
    | PowerTimeEnergyCalculator PowerTimeEnergyCalculator.Model
    | VoltageCurrentPowerCalculator VoltageCurrentPowerCalculator.Model
    | EnergyCostCalculator EnergyCostCalculator.Model


type Msg
    = PowerTimeEnergyCalculatorMsg PowerTimeEnergyCalculator.Msg
    | VoltageCurrentPowerCalculatorMsg VoltageCurrentPowerCalculator.Msg
    | EnergyCostCalculatorMsg EnergyCostCalculator.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url


main : Program { currentTime : Int } Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : { currentTime : Int } -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = Routes.parseUrl url
            , page = NotFound
            , navKey = navKey
            , config = flags
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Routes.NotFound ->
                    ( NotFound, Cmd.none )

                Routes.Home ->
                    let
                        ( pageModel, pageCmds ) =
                            PowerTimeEnergyCalculator.init model.config
                    in
                    ( PowerTimeEnergyCalculator pageModel, Cmd.map PowerTimeEnergyCalculatorMsg pageCmds )

                Routes.PowerTimeEnergyCalculator ->
                    let
                        ( pageModel, pageCmds ) =
                            PowerTimeEnergyCalculator.init model.config
                    in
                    ( PowerTimeEnergyCalculator pageModel, Cmd.map PowerTimeEnergyCalculatorMsg pageCmds )

                Routes.VoltageCurrentPowerCalculator ->
                    let
                        ( pageModel, pageCmds ) =
                            VoltageCurrentPowerCalculator.init model.config
                    in
                    ( VoltageCurrentPowerCalculator pageModel, Cmd.map VoltageCurrentPowerCalculatorMsg pageCmds )

                Routes.EnergyCostCalculator ->
                    let
                        ( pageModel, pageCmds ) =
                            EnergyCostCalculator.init model.config
                    in
                    ( EnergyCostCalculator pageModel, Cmd.map EnergyCostCalculatorMsg pageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


view : Model -> Document Msg
view model =
    let
        currentView =
            case model.page of
                NotFound ->
                    notFoundView

                PowerTimeEnergyCalculator pageModel ->
                    PowerTimeEnergyCalculator.view pageModel
                        |> Html.map PowerTimeEnergyCalculatorMsg

                VoltageCurrentPowerCalculator pageModel ->
                    VoltageCurrentPowerCalculator.view pageModel
                        |> Html.map VoltageCurrentPowerCalculatorMsg

                EnergyCostCalculator pageModel ->
                    EnergyCostCalculator.view pageModel
                        |> Html.map EnergyCostCalculatorMsg
    in
    { title = "CalcOOlators"
    , body =
        [ div [ class "min-h-full" ]
            [ nav [ class "bg-black border-b border-gray-200" ]
                [ div [ class "mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "flex justify-between h-16" ]
                        [ div [ class "flex" ]
                            [ div [ class "flex-shrink-0 flex items-center" ]
                                [ a [ href "/power-time-energy" ] [ img [ class "lg:block h-6 w-auto", src "/images/calcoolators-logo.svg" ] [] ] ]
                            , div [ class "-my-px ml-10 flex space-x-8" ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "p-8" ]
                [ div [ class "bg-blue-100 py-2 px-4 mb-4 rounded-lg" ]
                    [ text "CalcOOlators is in beta. Standby for more calculators for different topics. In the meantime, you can play around with a few energy-related calculators:"
                    , ul []
                        [ li [] [ a [ href "/power-time-energy", class "link" ] [ text "Power, Time, and Energy" ] ]
                        , li [] [ a [ href "/energy-cost", class "link" ] [ text "Energy Cost" ] ]
                        , li [] [ a [ href "/voltage-current-power", class "link" ] [ text "Voltage, Current, and Power" ] ]
                        ]
                    ]
                , currentView
                ]
            ]
        ]
    }


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( PowerTimeEnergyCalculatorMsg subMsg, PowerTimeEnergyCalculator pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    PowerTimeEnergyCalculator.update subMsg pageModel
            in
            ( { model | page = PowerTimeEnergyCalculator updatedPageModel }
            , Cmd.map PowerTimeEnergyCalculatorMsg updatedCmd
            )

        ( VoltageCurrentPowerCalculatorMsg subMsg, VoltageCurrentPowerCalculator pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    VoltageCurrentPowerCalculator.update subMsg pageModel
            in
            ( { model | page = VoltageCurrentPowerCalculator updatedPageModel }
            , Cmd.map VoltageCurrentPowerCalculatorMsg updatedCmd
            )

        ( EnergyCostCalculatorMsg subMsg, EnergyCostCalculator pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    EnergyCostCalculator.update subMsg pageModel
            in
            ( { model | page = EnergyCostCalculator updatedPageModel }
            , Cmd.map EnergyCostCalculatorMsg updatedCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Routes.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( _, _ ) ->
            ( model, Cmd.none )
