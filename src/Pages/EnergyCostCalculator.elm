module Pages.EnergyCostCalculator exposing (Model, Msg, init, update, view)

import Forms exposing (formControl)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (cardBody, cardContainer, cardHeader, cardHeaderToggleButton, cardTitle, cardsGrid, pageHeader, resourceLink, resourcesContainer)
import Units.Currency exposing (Currency(..), currencyToFloat, formatCurrency)
import Units.Electricity
    exposing
        ( WattHourCost(..)
        , WattHours(..)
        , energyCostToFloat
        , energyToFloat
        , floatToEnergy
        , floatToEnergyCost
        , formatEnergy
        , formatEnergyCost
        )
import Units.Metric exposing (Prefix(..))
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



-- MODEL


type alias Model =
    { activeField : Field
    , formStatus : CalculationResult
    , typedValue : String
    , energy : WattHours
    , energyCost : WattHourCost
    , totalCost : Currency
    , solveMethod : SolveMethod
    }



-- INIT


init : { currentTime : Int } -> ( Model, Cmd Msg )
init { currentTime } =
    let
        energy =
            WattHours 30000

        energyCost =
            floatToEnergyCost Kilo 0.12
    in
    ( { activeField = NoActiveField
      , formStatus = Valid
      , typedValue = ""
      , energy = energy
      , energyCost = energyCost
      , totalCost = calculateTotalCost energy energyCost
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
                    [ renderMetricField model WattHoursField model.energy Base "Watt Hours" energyToFloat formatEnergy EnergySolve
                    , renderMetricField model KilowattHoursField model.energy Kilo "Kilowatt Hours" energyToFloat formatEnergy EnergySolve
                    , renderMetricField model MegawattHoursField model.energy Mega "Megawatt Hours" energyToFloat formatEnergy EnergySolve
                    , renderMetricField model GigawattHoursField model.energy Giga "Gigawatt Hours" energyToFloat formatEnergy EnergySolve
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == EnergyCostSolve)
                    [ cardTitle "Energy Cost"
                    , cardHeaderToggleButton (model.solveMethod == EnergyCostSolve) "Solving for Energy Cost" "Solve for Energy Cost" (SetSolveMethod EnergyCostSolve)
                    ]
                , cardBody
                    [ renderMetricField model WattHourCostField model.energyCost Base "Cost Per Watt Hour" energyCostToFloat formatEnergyCost EnergyCostSolve
                    , renderMetricField model KilowattHourCostField model.energyCost Kilo "Cost Per Kilowatt Hour" energyCostToFloat formatEnergyCost EnergyCostSolve
                    , renderMetricField model MegawattHourCostField model.energyCost Mega "Cost Per Megawatt Hour" energyCostToFloat formatEnergyCost EnergyCostSolve
                    , renderMetricField model GigawattHourCostField model.energyCost Giga "Cost Per Gigawatt Hour" energyCostToFloat formatEnergyCost EnergyCostSolve
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
            [ resourceLink "If the cost of energy is 12 cents per kilowatt hour, how many kilowatt hours would your central air need to use to reach a total cost of $50?" (SetExample (EnergyExample (Currency 50) (floatToEnergyCost Kilo 0.12)))
            , resourceLink "If your home typically uses 900 kilowatt hours of energy per month, what would be the cost per kilowatt in order to get your total bill below $100?" (SetExample (WattHourCostExample (Currency 99.99) (floatToEnergy Kilo 900)))
            , resourceLink "The average US home uses 30kWh of energy per day. If energy costs 12 cents per kilowatt hour, what would be the daily cost?" (SetExample (TotalCostExample (floatToEnergy Kilo 30) (floatToEnergyCost Kilo 0.12)))
            ]
        ]


renderMetricField : Model -> Field -> unit -> Prefix -> String -> (Prefix -> unit -> Float) -> (Prefix -> unit -> String) -> SolveMethod -> Html Msg
renderMetricField model field unit prefix label inputFn hintFn solveMethod =
    renderField model field label (inputFn prefix unit) (hintFn prefix unit) solveMethod


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
                                    updateWattHours model (floatToEnergy Kilo floatValue)

                                MegawattHoursField ->
                                    updateWattHours model (floatToEnergy Mega floatValue)

                                GigawattHoursField ->
                                    updateWattHours model (floatToEnergy Giga floatValue)

                                WattHourCostField ->
                                    updateWattHourCost model (WattHourCost floatValue)

                                KilowattHourCostField ->
                                    updateWattHourCost model (floatToEnergyCost Kilo floatValue)

                                MegawattHourCostField ->
                                    updateWattHourCost model (floatToEnergyCost Mega floatValue)

                                GigawattHourCostField ->
                                    updateWattHourCost model (floatToEnergyCost Giga floatValue)

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
                        EnergyExample totalCost energyCost ->
                            { model | solveMethod = EnergySolve, totalCost = totalCost, energyCost = energyCost, energy = calculateEnergy totalCost energyCost }

                        WattHourCostExample totalCost energy ->
                            { model | solveMethod = EnergyCostSolve, totalCost = totalCost, energy = energy, energyCost = calculateWattHourCost totalCost energy }

                        TotalCostExample energy energyCost ->
                            { model | solveMethod = TotalCostSolve, energy = energy, energyCost = energyCost, totalCost = calculateTotalCost energy energyCost }
            in
            ( { newModel | formStatus = Valid, activeField = NoActiveField }, Cmd.none )


updateWattHours : Model -> WattHours -> Model
updateWattHours model energy =
    case model.solveMethod of
        EnergyCostSolve ->
            { model | energy = energy, energyCost = calculateWattHourCost model.totalCost energy }

        TotalCostSolve ->
            { model | energy = energy, totalCost = calculateTotalCost energy model.energyCost }

        _ ->
            model


updateWattHourCost : Model -> WattHourCost -> Model
updateWattHourCost model energyCost =
    case model.solveMethod of
        EnergySolve ->
            { model | energyCost = energyCost, energy = calculateEnergy model.totalCost energyCost }

        TotalCostSolve ->
            { model | energyCost = energyCost, totalCost = calculateTotalCost model.energy energyCost }

        _ ->
            model


updateTotalCost : Model -> Currency -> Model
updateTotalCost model totalCost =
    case model.solveMethod of
        EnergySolve ->
            { model | totalCost = totalCost, energy = calculateEnergy totalCost model.energyCost }

        EnergyCostSolve ->
            { model | totalCost = totalCost, energyCost = calculateWattHourCost totalCost model.energy }

        _ ->
            model


calculateEnergy : Currency -> WattHourCost -> WattHours
calculateEnergy (Currency totalCost) (WattHourCost energyCost) =
    WattHours (totalCost / energyCost)


calculateWattHourCost : Currency -> WattHours -> WattHourCost
calculateWattHourCost (Currency totalCostFloat) (WattHours wattHoursFloat) =
    WattHourCost (totalCostFloat / wattHoursFloat)


calculateTotalCost : WattHours -> WattHourCost -> Currency
calculateTotalCost (WattHours wattHoursFloat) (WattHourCost wattHourCostFloat) =
    Currency (wattHoursFloat * wattHourCostFloat)
