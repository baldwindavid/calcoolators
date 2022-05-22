module Pages.VoltageCurrentPowerCalculator exposing (Model, Msg, init, update, view)

import Forms exposing (formControl)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (cardBody, cardContainer, cardHeader, cardHeaderToggleButton, cardTitle, cardsGrid, pageHeader, resourceLink, resourcesContainer)
import Units.Electricity
    exposing
        ( Amps(..)
        , Volts(..)
        , Watts(..)
        , currentToFloat
        , floatToCurrent
        , floatToPower
        , floatToVoltage
        , formatCurrent
        , formatPower
        , formatVoltage
        , powerToFloat
        , voltageToFloat
        )
import Units.Metric exposing (Prefix(..))
import Units.Number exposing (numberStringToFloat)


type Field
    = WattsField
    | KilowattsField
    | MegawattsField
    | GigawattsField
    | VoltsField
    | KilovoltsField
    | MegavoltsField
    | GigavoltsField
    | AmpsField
    | KiloampsField
    | MegaampsField
    | GigaampsField
    | NoActiveField


type CalculationResult
    = Valid
    | Invalid String


type SolveMethod
    = PowerSolve
    | VoltageSolve
    | CurrentSolve


type Example
    = VoltageExample Watts Amps
    | CurrentExample Watts Volts
    | PowerExample Volts Amps



-- MODEL


type alias Model =
    { power : Watts
    , voltage : Volts
    , current : Amps
    , activeField : Field
    , formStatus : CalculationResult
    , typedValue : String
    , solveMethod : SolveMethod
    }



-- INIT


init : { currentTime : Int } -> ( Model, Cmd Msg )
init { currentTime } =
    let
        voltage =
            Volts 30000

        current =
            Amps 1000
    in
    ( { power = calculatePower voltage current
      , activeField = NoActiveField
      , formStatus = Valid
      , typedValue = ""
      , voltage = voltage
      , current = current
      , solveMethod = PowerSolve
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
                VoltageSolve ->
                    [ text "Power"
                    , span [ class altClass ] [ text "/" ]
                    , text "Current"
                    , span [ class altClass ] [ text "=" ]
                    , text "Voltage"
                    ]

                CurrentSolve ->
                    [ text "Power"
                    , span [ class altClass ] [ text "/" ]
                    , text "Voltage"
                    , span [ class altClass ] [ text "=" ]
                    , text "Current"
                    ]

                PowerSolve ->
                    [ text "Voltage"
                    , span [ class altClass ] [ text "x" ]
                    , text "Current"
                    , span [ class altClass ] [ text "=" ]
                    , text "Power"
                    ]
    in
    div []
        [ pageHeader formula
        , cardsGrid 3
            [ cardContainer
                [ cardHeader (model.solveMethod == VoltageSolve)
                    [ cardTitle "Voltage"
                    , cardHeaderToggleButton (model.solveMethod == VoltageSolve) "Solving for Voltage" "Solve for Voltage" (SetSolveMethod VoltageSolve)
                    ]
                , cardBody
                    [ renderField model VoltsField model.voltage Base "Volts" voltageToFloat formatVoltage VoltageSolve
                    , renderField model KilovoltsField model.voltage Kilo "Kilovolts" voltageToFloat formatVoltage VoltageSolve
                    , renderField model MegavoltsField model.voltage Mega "Megavolts" voltageToFloat formatVoltage VoltageSolve
                    , renderField model GigavoltsField model.voltage Giga "Gigavolts" voltageToFloat formatVoltage VoltageSolve
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == CurrentSolve)
                    [ cardTitle "Current"
                    , cardHeaderToggleButton (model.solveMethod == CurrentSolve) "Solving for Current" "Solve for Current" (SetSolveMethod CurrentSolve)
                    ]
                , cardBody
                    [ renderField model AmpsField model.current Base "Amps" currentToFloat formatCurrent CurrentSolve
                    , renderField model KiloampsField model.current Kilo "Kiloamps" currentToFloat formatCurrent CurrentSolve
                    , renderField model MegaampsField model.current Mega "Megaamps" currentToFloat formatCurrent CurrentSolve
                    , renderField model GigaampsField model.current Giga "Gigaamps" currentToFloat formatCurrent CurrentSolve
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == PowerSolve)
                    [ cardTitle "Power"
                    , cardHeaderToggleButton (model.solveMethod == PowerSolve) "Solving for Power" "Solve for Power" (SetSolveMethod PowerSolve)
                    ]
                , cardBody
                    [ renderField model WattsField model.power Base "Watt" powerToFloat formatPower PowerSolve
                    , renderField model KilowattsField model.power Kilo "Kilowatt" powerToFloat formatPower PowerSolve
                    , renderField model MegawattsField model.power Mega "Megawatt" powerToFloat formatPower PowerSolve
                    , renderField model GigawattsField model.power Giga "Gigawatt" powerToFloat formatPower PowerSolve
                    ]
                ]
            ]
        , resourcesContainer "Examples"
            "Click the questions below to auto-fill the form with the solution:"
            [ resourceLink "What would be the voltage for a central air unit running at 3000 watts and using 12.5 amps?" (SetExample (VoltageExample (Watts 3000) (Amps 12.5)))
            , resourceLink "What would be the current for a 100 watt light bulb operating at 120 volts?" (SetExample (CurrentExample (Watts 100) (Volts 120)))
            , resourceLink "What would be the power for a refrigerator with a voltage of 240 volts and current of 2.5 amps" (SetExample (PowerExample (Volts 240) (Amps 2.5)))
            ]
        ]


renderField : Model -> Field -> unit -> Prefix -> String -> (Prefix -> unit -> Float) -> (Prefix -> unit -> String) -> SolveMethod -> Html Msg
renderField model field unit prefix label inputFn hintFn solveMethod =
    let
        ( value, errors, hint ) =
            case model.formStatus of
                Valid ->
                    if model.activeField == field then
                        ( model.typedValue, [], hintFn prefix unit )

                    else
                        ( String.fromFloat (inputFn prefix unit), [], hintFn prefix unit )

                Invalid errorMsg ->
                    if model.activeField == field then
                        ( model.typedValue, [ errorMsg ], "" )

                    else
                        ( String.fromFloat (inputFn prefix unit), [], hintFn prefix unit )
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
                                WattsField ->
                                    updatePower model (Watts floatValue)

                                KilowattsField ->
                                    updatePower model (floatToPower Kilo floatValue)

                                MegawattsField ->
                                    updatePower model (floatToPower Mega floatValue)

                                GigawattsField ->
                                    updatePower model (floatToPower Giga floatValue)

                                VoltsField ->
                                    updateVoltage model (Volts floatValue)

                                KilovoltsField ->
                                    updateVoltage model (floatToVoltage Kilo floatValue)

                                MegavoltsField ->
                                    updateVoltage model (floatToVoltage Mega floatValue)

                                GigavoltsField ->
                                    updateVoltage model (floatToVoltage Giga floatValue)

                                AmpsField ->
                                    updateCurrent model (Amps floatValue)

                                KiloampsField ->
                                    updateCurrent model (floatToCurrent Kilo floatValue)

                                MegaampsField ->
                                    updateCurrent model (floatToCurrent Mega floatValue)

                                GigaampsField ->
                                    updateCurrent model (floatToCurrent Giga floatValue)

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
                        VoltageExample power current ->
                            { model | solveMethod = VoltageSolve, power = power, current = current, voltage = calculateVoltage power current }

                        CurrentExample power voltage ->
                            { model | solveMethod = CurrentSolve, power = power, voltage = voltage, current = calculateCurrent power voltage }

                        PowerExample voltage current ->
                            { model | solveMethod = PowerSolve, voltage = voltage, current = current, power = calculatePower voltage current }
            in
            ( { newModel | formStatus = Valid, activeField = NoActiveField }, Cmd.none )


updatePower : Model -> Watts -> Model
updatePower model power =
    case model.solveMethod of
        VoltageSolve ->
            { model | power = power, voltage = calculateVoltage power model.current }

        CurrentSolve ->
            { model | power = power, current = calculateCurrent power model.voltage }

        _ ->
            model


updateVoltage : Model -> Volts -> Model
updateVoltage model voltage =
    case model.solveMethod of
        PowerSolve ->
            { model | voltage = voltage, power = calculatePower voltage model.current }

        CurrentSolve ->
            { model | voltage = voltage, current = calculateCurrent model.power voltage }

        _ ->
            model


updateCurrent : Model -> Amps -> Model
updateCurrent model current =
    case model.solveMethod of
        PowerSolve ->
            { model | current = current, power = calculatePower model.voltage current }

        VoltageSolve ->
            { model | current = current, voltage = calculateVoltage model.power current }

        _ ->
            model


calculateVoltage : Watts -> Amps -> Volts
calculateVoltage (Watts wattsFloat) (Amps ampsValue) =
    Volts (wattsFloat / ampsValue)


calculateCurrent : Watts -> Volts -> Amps
calculateCurrent (Watts wattsValue) (Volts voltsValue) =
    Amps (wattsValue / voltsValue)


calculatePower : Volts -> Amps -> Watts
calculatePower (Volts voltsValue) (Amps ampsValue) =
    Watts (voltsValue * ampsValue)
