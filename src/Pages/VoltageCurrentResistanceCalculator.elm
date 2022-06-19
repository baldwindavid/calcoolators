module Pages.VoltageCurrentResistanceCalculator exposing (Model, Msg, init, update, view)

import Forms exposing (formControl)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (cardBody, cardContainer, cardHeader, cardHeaderToggleButton, cardTitle, cardsGrid, pageHeader, resourceLink, resourcesContainer)
import Units.Electricity
    exposing
        ( Amps(..)
        , Ohms(..)
        , Volts(..)
        , currentToFloat
        , floatToCurrent
        , floatToResistance
        , floatToVoltage
        , formatCurrent
        , formatResistance
        , formatVoltage
        , resistanceToFloat
        , voltageToFloat
        )
import Units.Metric exposing (Prefix(..))
import Units.Number exposing (numberStringToFloat)


type Field
    = OhmsField
    | KiloohmsField
    | MegaohmsField
    | GigaohmsField
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
    = ResistanceSolve
    | VoltageSolve
    | CurrentSolve


type Example
    = VoltageExample Ohms Amps
    | CurrentExample Ohms Volts
    | ResistanceExample Volts Amps



-- MODEL


type alias Model =
    { resistance : Ohms
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
    ( { resistance = calculateResistance voltage current
      , activeField = NoActiveField
      , formStatus = Valid
      , typedValue = ""
      , voltage = voltage
      , current = current
      , solveMethod = ResistanceSolve
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
                    [ text "Resistance"
                    , span [ class altClass ] [ text "x" ]
                    , text "Current"
                    , span [ class altClass ] [ text "=" ]
                    , text "Voltage"
                    ]

                CurrentSolve ->
                    [ text "Voltage"
                    , span [ class altClass ] [ text "/" ]
                    , text "Resistance"
                    , span [ class altClass ] [ text "=" ]
                    , text "Current"
                    ]

                ResistanceSolve ->
                    [ text "Voltage"
                    , span [ class altClass ] [ text "/" ]
                    , text "Current"
                    , span [ class altClass ] [ text "=" ]
                    , text "Resistance"
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
                [ cardHeader (model.solveMethod == ResistanceSolve)
                    [ cardTitle "Resistance"
                    , cardHeaderToggleButton (model.solveMethod == ResistanceSolve) "Solving for Resistance" "Solve for Resistance" (SetSolveMethod ResistanceSolve)
                    ]
                , cardBody
                    [ renderField model OhmsField model.resistance Base "Ohms" resistanceToFloat formatResistance ResistanceSolve
                    , renderField model KiloohmsField model.resistance Kilo "Kiloohms" resistanceToFloat formatResistance ResistanceSolve
                    , renderField model MegaohmsField model.resistance Mega "Megaohms" resistanceToFloat formatResistance ResistanceSolve
                    , renderField model GigaohmsField model.resistance Giga "Gigaohms" resistanceToFloat formatResistance ResistanceSolve
                    ]
                ]
            ]
        , resourcesContainer "Examples"
            "Click the questions below to auto-fill the form with the solution:"
            [ resourceLink "What would be the voltage for a lightbulb using 4.16 amps of current with a resistance of 57.7 ohms?" (SetExample (VoltageExample (Ohms 57.7) (Amps 4.16)))
            , resourceLink "What would be the current for a flashlight bulb with a resistance of 4 ohms operating at 3 volts" (SetExample (CurrentExample (Ohms 4) (Volts 3)))
            , resourceLink "What would be the resistance of a lightbulb operating at 120 volts using 0.83 amps of current?" (SetExample (ResistanceExample (Volts 120) (Amps 0.83)))
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
                                OhmsField ->
                                    updateResistance model (Ohms floatValue)

                                KiloohmsField ->
                                    updateResistance model (floatToResistance Kilo floatValue)

                                MegaohmsField ->
                                    updateResistance model (floatToResistance Mega floatValue)

                                GigaohmsField ->
                                    updateResistance model (floatToResistance Giga floatValue)

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
                        VoltageExample resistance current ->
                            { model | solveMethod = VoltageSolve, resistance = resistance, current = current, voltage = calculateVoltage resistance current }

                        CurrentExample resistance voltage ->
                            { model | solveMethod = CurrentSolve, resistance = resistance, voltage = voltage, current = calculateCurrent resistance voltage }

                        ResistanceExample voltage current ->
                            { model | solveMethod = ResistanceSolve, voltage = voltage, current = current, resistance = calculateResistance voltage current }
            in
            ( { newModel | formStatus = Valid, activeField = NoActiveField }, Cmd.none )


updateResistance : Model -> Ohms -> Model
updateResistance model resistance =
    case model.solveMethod of
        VoltageSolve ->
            { model | resistance = resistance, voltage = calculateVoltage resistance model.current }

        CurrentSolve ->
            { model | resistance = resistance, current = calculateCurrent resistance model.voltage }

        _ ->
            model


updateVoltage : Model -> Volts -> Model
updateVoltage model voltage =
    case model.solveMethod of
        ResistanceSolve ->
            { model | voltage = voltage, resistance = calculateResistance voltage model.current }

        CurrentSolve ->
            { model | voltage = voltage, current = calculateCurrent model.resistance voltage }

        _ ->
            model


updateCurrent : Model -> Amps -> Model
updateCurrent model current =
    case model.solveMethod of
        ResistanceSolve ->
            { model | current = current, resistance = calculateResistance model.voltage current }

        VoltageSolve ->
            { model | current = current, voltage = calculateVoltage model.resistance current }

        _ ->
            model


calculateVoltage : Ohms -> Amps -> Volts
calculateVoltage (Ohms ohmsValue) (Amps ampsValue) =
    Volts (ohmsValue * ampsValue)


calculateCurrent : Ohms -> Volts -> Amps
calculateCurrent (Ohms ohmsValue) (Volts voltsValue) =
    Amps (voltsValue / ohmsValue)


calculateResistance : Volts -> Amps -> Ohms
calculateResistance (Volts voltsValue) (Amps ampsValue) =
    Ohms (voltsValue / ampsValue)
