module Pages.EnergyCostCalculator exposing (Model, Msg, init, update, view)

import Forms exposing (formControl)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (cardBody, cardContainer, cardHeader, cardHeaderToggleButton, cardTitle, cardsGrid, pageHeader, resourceLink, resourcesContainer)
import Units.Currency exposing (Currency(..), currencyToFloat, formatCurrency)
import Units.Electricity
    exposing
        ( GigawattHourCost(..)
        , GigawattHours(..)
        , KilowattHourCost(..)
        , KilowattHours(..)
        , MegawattHourCost(..)
        , MegawattHours(..)
        , WattHourCost(..)
        , WattHours(..)
        , formatGigawattHourCost
        , formatGigawattHours
        , formatKilowattHourCost
        , formatKilowattHours
        , formatMegawattHourCost
        , formatMegawattHours
        , formatWattHourCost
        , formatWattHours
        , gigawattHourCostToFloat
        , gigawattHourCostToWattHourCost
        , gigawattHoursToFloat
        , gigawattHoursToWattHours
        , kilowattHourCostToFloat
        , kilowattHourCostToWattHourCost
        , kilowattHoursToFloat
        , kilowattHoursToWattHours
        , megawattHourCostToFloat
        , megawattHourCostToWattHourCost
        , megawattHoursToFloat
        , megawattHoursToWattHours
        , wattHourCostToFloat
        , wattHourCostToGigawattHourCost
        , wattHourCostToKilowattHourCost
        , wattHourCostToMegawattHourCost
        , wattHoursToFloat
        , wattHoursToGigawattHours
        , wattHoursToKilowattHours
        , wattHoursToMegawattHours
        )
import Units.Number exposing (numberStringToFloat)


type Field
    = WattHoursField
    | KilowattHoursField
    | MegawattHoursField
    | GigawattHoursField
    | WattHourCostField
    | KilowattHourCostField
    | MegawattHourCostField
    | GigawattHourCostField
    | TotalCostField
    | NoActiveField


type alias Model =
    { activeField : Field
    , formStatus : CalculationResult
    , typedValue : String
    , wattHours : WattHours
    , wattHourCost : WattHourCost
    , totalCost : Currency
    , solveMethod : SolveMethod
    }


type CalculationResult
    = Valid
    | Invalid String


type SolveMethod
    = EnergySolve
    | EnergyCostSolve
    | TotalCostSolve


type Example
    = EnergyExample Currency WattHourCost
    | WattHourCostExample Currency WattHours
    | TotalCostExample WattHours WattHourCost



-- INIT


init : { currentTime : Int } -> ( Model, Cmd Msg )
init { currentTime } =
    let
        wattHours =
            WattHours 30000

        wattHourCost =
            KilowattHourCost 0.12 |> kilowattHourCostToWattHourCost
    in
    ( { activeField = NoActiveField
      , formStatus = Valid
      , typedValue = ""
      , wattHours = wattHours
      , wattHourCost = wattHourCost
      , totalCost = calculateTotalCost wattHours wattHourCost
      , solveMethod = TotalCostSolve
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        altClass =
            "text-gray-400 mx-2"

        formula =
            case model.solveMethod of
                EnergySolve ->
                    [ text "Total Cost"
                    , span [ class altClass ] [ text "/" ]
                    , text "Energy Cost"
                    , span [ class altClass ] [ text "=" ]
                    , text "Energy"
                    ]

                EnergyCostSolve ->
                    [ text "Total Cost"
                    , span [ class altClass ] [ text "/" ]
                    , text "Energy"
                    , span [ class altClass ] [ text "=" ]
                    , text "Energy Cost"
                    ]

                TotalCostSolve ->
                    [ text "Energy"
                    , span [ class altClass ] [ text "x" ]
                    , text "Energy Cost"
                    , span [ class altClass ] [ text "=" ]
                    , text "Total Cost"
                    ]
    in
    div []
        [ pageHeader formula
        , cardsGrid 3
            [ cardContainer
                [ cardHeader (model.solveMethod == EnergySolve)
                    [ cardTitle "Energy"
                    , cardHeaderToggleButton (model.solveMethod == EnergySolve) "Solving for Energy" "Solve for Energy" (SetSolveMethod EnergySolve)
                    ]
                , cardBody
                    [ renderField model WattHoursField "Watt Hours" (model.wattHours |> wattHoursToFloat) (model.wattHours |> formatWattHours) EnergySolve
                    , renderField model KilowattHoursField "Kilowatt Hours" (model.wattHours |> wattHoursToKilowattHours |> kilowattHoursToFloat) (model.wattHours |> wattHoursToKilowattHours |> formatKilowattHours) EnergySolve
                    , renderField model MegawattHoursField "Megawatt Hours" (model.wattHours |> wattHoursToMegawattHours |> megawattHoursToFloat) (model.wattHours |> wattHoursToMegawattHours |> formatMegawattHours) EnergySolve
                    , renderField model GigawattHoursField "Gigawatt Hours" (model.wattHours |> wattHoursToGigawattHours |> gigawattHoursToFloat) (model.wattHours |> wattHoursToGigawattHours |> formatGigawattHours) EnergySolve
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == EnergyCostSolve)
                    [ cardTitle "Energy Cost"
                    , cardHeaderToggleButton (model.solveMethod == EnergyCostSolve) "Solving for Energy Cost" "Solve for Energy Cost" (SetSolveMethod EnergyCostSolve)
                    ]
                , cardBody
                    [ renderField model WattHourCostField "Cost Per Watt Hour" (model.wattHourCost |> wattHourCostToFloat) (model.wattHourCost |> formatWattHourCost) EnergyCostSolve
                    , renderField model KilowattHourCostField "Cost Per Kilowatt Hour" (model.wattHourCost |> wattHourCostToKilowattHourCost |> kilowattHourCostToFloat) (model.wattHourCost |> wattHourCostToKilowattHourCost |> formatKilowattHourCost) EnergyCostSolve
                    , renderField model MegawattHourCostField "Cost Per Megawatt Hour" (model.wattHourCost |> wattHourCostToMegawattHourCost |> megawattHourCostToFloat) (model.wattHourCost |> wattHourCostToMegawattHourCost |> formatMegawattHourCost) EnergyCostSolve
                    , renderField model GigawattHourCostField "Cost Per Gigawatt Hour" (model.wattHourCost |> wattHourCostToGigawattHourCost |> gigawattHourCostToFloat) (model.wattHourCost |> wattHourCostToGigawattHourCost |> formatGigawattHourCost) EnergyCostSolve
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == TotalCostSolve)
                    [ cardTitle "Total Cost"
                    , cardHeaderToggleButton (model.solveMethod == TotalCostSolve) "Solving for Total Cost" "Solve for Total Cost" (SetSolveMethod TotalCostSolve)
                    ]
                , cardBody
                    [ renderField model TotalCostField "Total Cost" (model.totalCost |> currencyToFloat) (model.totalCost |> formatCurrency) TotalCostSolve
                    ]
                ]
            ]
        , resourcesContainer "Examples"
            "Click the questions below to auto-fill the form with the solution:"
            [ resourceLink "If the cost of energy is 12 cents per kilowatt hour, how many kilowatt hours would your central air need to use to reach a total cost of $50?" (SetExample (EnergyExample (Currency 50) (KilowattHourCost 0.12 |> kilowattHourCostToWattHourCost)))
            , resourceLink "If your home typically uses 900 kilowatt hours of energy per month, what would be the cost per kilowatt in order to get your total bill below $100?" (SetExample (WattHourCostExample (Currency 99.99) (KilowattHours 900 |> kilowattHoursToWattHours)))
            , resourceLink "The average US home uses 30kWh of energy per day. If energy costs 12 cents per kilowatt hour, what would be the daily cost?" (SetExample (TotalCostExample (KilowattHours 30 |> kilowattHoursToWattHours) (KilowattHourCost 0.12 |> kilowattHourCostToWattHourCost)))
            ]
        ]


calculateEnergy : Currency -> WattHourCost -> WattHours
calculateEnergy (Currency totalCost) (WattHourCost wattHourCost) =
    WattHours (totalCost / wattHourCost)


calculateWattHourCost : Currency -> WattHours -> WattHourCost
calculateWattHourCost (Currency totalCostFloat) (WattHours wattHoursFloat) =
    WattHourCost (totalCostFloat / wattHoursFloat)


calculateTotalCost : WattHours -> WattHourCost -> Currency
calculateTotalCost (WattHours wattHoursFloat) (WattHourCost wattHourCostFloat) =
    Currency (wattHoursFloat * wattHourCostFloat)


renderField : Model -> Field -> String -> Float -> String -> SolveMethod -> Html Msg
renderField model field label forInput forHint solveMethod =
    let
        ( value, errors, hint ) =
            case model.formStatus of
                Valid ->
                    if model.activeField == field then
                        ( model.typedValue, [], forHint )

                    else
                        ( String.fromFloat forInput, [], forHint )

                Invalid errorMsg ->
                    if model.activeField == field then
                        ( model.typedValue, [ errorMsg ], "" )

                    else
                        ( String.fromFloat forInput, [], forHint )
    in
    formControl label value errors hint (model.solveMethod == solveMethod) (UpdateField field)



-- UPDATE


type Msg
    = UpdateField Field String
    | SetSolveMethod SolveMethod
    | SetExample Example


updateWattHours : Model -> WattHours -> Model
updateWattHours model wattHours =
    case model.solveMethod of
        EnergyCostSolve ->
            { model | wattHours = wattHours, wattHourCost = calculateWattHourCost model.totalCost wattHours }

        TotalCostSolve ->
            { model | wattHours = wattHours, totalCost = calculateTotalCost wattHours model.wattHourCost }

        _ ->
            model


updateWattHourCost : Model -> WattHourCost -> Model
updateWattHourCost model wattHourCost =
    case model.solveMethod of
        EnergySolve ->
            { model | wattHourCost = wattHourCost, wattHours = calculateEnergy model.totalCost wattHourCost }

        TotalCostSolve ->
            { model | wattHourCost = wattHourCost, totalCost = calculateTotalCost model.wattHours wattHourCost }

        _ ->
            model


updateTotalCost : Model -> Currency -> Model
updateTotalCost model totalCost =
    case model.solveMethod of
        EnergySolve ->
            { model | totalCost = totalCost, wattHours = calculateEnergy totalCost model.wattHourCost }

        EnergyCostSolve ->
            { model | totalCost = totalCost, wattHourCost = calculateWattHourCost totalCost model.wattHours }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSolveMethod solveMethod ->
            ( { model | solveMethod = solveMethod }, Cmd.none )

        UpdateField field typedValue ->
            case numberStringToFloat typedValue of
                Just floatValue ->
                    let
                        newModel =
                            case field of
                                WattHoursField ->
                                    updateWattHours model (WattHours floatValue)

                                KilowattHoursField ->
                                    updateWattHours model (KilowattHours floatValue |> kilowattHoursToWattHours)

                                MegawattHoursField ->
                                    updateWattHours model (MegawattHours floatValue |> megawattHoursToWattHours)

                                GigawattHoursField ->
                                    updateWattHours model (GigawattHours floatValue |> gigawattHoursToWattHours)

                                WattHourCostField ->
                                    updateWattHourCost model (WattHourCost floatValue)

                                KilowattHourCostField ->
                                    updateWattHourCost model (KilowattHourCost floatValue |> kilowattHourCostToWattHourCost)

                                MegawattHourCostField ->
                                    updateWattHourCost model (MegawattHourCost floatValue |> megawattHourCostToWattHourCost)

                                GigawattHourCostField ->
                                    updateWattHourCost model (GigawattHourCost floatValue |> gigawattHourCostToWattHourCost)

                                TotalCostField ->
                                    updateTotalCost model (Currency floatValue)

                                NoActiveField ->
                                    model
                    in
                    ( { newModel | formStatus = Valid, activeField = field, typedValue = typedValue }, Cmd.none )

                Nothing ->
                    ( { model | formStatus = Invalid "must be a number", activeField = field, typedValue = typedValue }, Cmd.none )

        SetExample example ->
            let
                newModel =
                    case example of
                        EnergyExample totalCost wattHourCost ->
                            { model | solveMethod = EnergySolve, totalCost = totalCost, wattHourCost = wattHourCost, wattHours = calculateEnergy totalCost wattHourCost }

                        WattHourCostExample totalCost wattHours ->
                            { model | solveMethod = EnergyCostSolve, totalCost = totalCost, wattHours = wattHours, wattHourCost = calculateWattHourCost totalCost wattHours }

                        TotalCostExample wattHours wattHourCost ->
                            { model | solveMethod = TotalCostSolve, wattHours = wattHours, wattHourCost = wattHourCost, totalCost = calculateTotalCost wattHours wattHourCost }
            in
            ( { newModel | formStatus = Valid, activeField = NoActiveField }, Cmd.none )
