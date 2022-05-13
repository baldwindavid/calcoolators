module Pages.VoltageCurrentPowerCalculator exposing (Model, Msg, init, update, view)

import Forms exposing (formControl)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (cardBody, cardContainer, cardHeader, cardHeaderToggleButton, cardTitle, cardsGrid, pageHeader, resourceLink, resourcesContainer)
import Units.Electricity
    exposing
        ( Amps(..)
        , Gigaamps(..)
        , Gigavolts(..)
        , Gigawatts(..)
        , Kiloamps(..)
        , Kilovolts(..)
        , Kilowatts(..)
        , Megaamps(..)
        , Megavolts(..)
        , Megawatts(..)
        , Volts(..)
        , Watts(..)
        , ampsToFloat
        , ampsToGigaamps
        , ampsToKiloamps
        , ampsToMegaamps
        , formatAmps
        , formatGigaamps
        , formatGigavolts
        , formatGigawatts
        , formatKiloamps
        , formatKilovolts
        , formatKilowatts
        , formatMegaamps
        , formatMegavolts
        , formatMegawatts
        , formatVolts
        , formatWatts
        , gigaampsToAmps
        , gigaampsToFloat
        , gigavoltsToFloat
        , gigavoltsToVolts
        , gigawattsToFloat
        , gigawattsToWatts
        , kiloampsToAmps
        , kiloampsToFloat
        , kilovoltsToFloat
        , kilovoltsToVolts
        , kilowattsToFloat
        , kilowattsToWatts
        , megaampsToAmps
        , megaampsToFloat
        , megavoltsToFloat
        , megavoltsToVolts
        , megawattsToFloat
        , megawattsToWatts
        , voltsToFloat
        , voltsToGigavolts
        , voltsToKilovolts
        , voltsToMegavolts
        , wattsToFloat
        , wattsToGigawatts
        , wattsToKilowatts
        , wattsToMegawatts
        )
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


type alias Model =
    { watts : Watts
    , activeField : Field
    , formStatus : CalculationResult
    , typedValue : String
    , volts : Volts
    , amps : Amps
    , solveMethod : SolveMethod
    }


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



-- INIT


init : { currentTime : Int } -> ( Model, Cmd Msg )
init { currentTime } =
    let
        volts =
            Volts 30000

        amps =
            Amps 1000
    in
    ( { watts = calculatePower volts amps
      , activeField = NoActiveField
      , formStatus = Valid
      , typedValue = ""
      , volts = volts
      , amps = amps
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
                    [ renderField model VoltsField "Volts" (model.volts |> voltsToFloat) (model.volts |> formatVolts) VoltageSolve
                    , renderField model KilovoltsField "Kilovolts" (model.volts |> voltsToKilovolts |> kilovoltsToFloat) (model.volts |> voltsToKilovolts |> formatKilovolts) VoltageSolve
                    , renderField model MegavoltsField "Megavolts" (model.volts |> voltsToMegavolts |> megavoltsToFloat) (model.volts |> voltsToMegavolts |> formatMegavolts) VoltageSolve
                    , renderField model GigavoltsField "Gigavolts" (model.volts |> voltsToGigavolts |> gigavoltsToFloat) (model.volts |> voltsToGigavolts |> formatGigavolts) VoltageSolve
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == CurrentSolve)
                    [ cardTitle "Current"
                    , cardHeaderToggleButton (model.solveMethod == CurrentSolve) "Solving for Current" "Solve for Current" (SetSolveMethod CurrentSolve)
                    ]
                , cardBody
                    [ renderField model AmpsField "Amps" (model.amps |> ampsToFloat) (model.amps |> formatAmps) CurrentSolve
                    , renderField model KiloampsField "Kiloamps" (model.amps |> ampsToKiloamps |> kiloampsToFloat) (model.amps |> ampsToKiloamps |> formatKiloamps) CurrentSolve
                    , renderField model MegaampsField "Megaamps" (model.amps |> ampsToMegaamps |> megaampsToFloat) (model.amps |> ampsToMegaamps |> formatMegaamps) CurrentSolve
                    , renderField model GigaampsField "Gigaamps" (model.amps |> ampsToGigaamps |> gigaampsToFloat) (model.amps |> ampsToGigaamps |> formatGigaamps) CurrentSolve
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == PowerSolve)
                    [ cardTitle "Power"
                    , cardHeaderToggleButton (model.solveMethod == PowerSolve) "Solving for Power" "Solve for Power" (SetSolveMethod PowerSolve)
                    ]
                , cardBody
                    [ renderField model WattsField "Watt" (model.watts |> wattsToFloat) (model.watts |> formatWatts) PowerSolve
                    , renderField model KilowattsField "Kilowatt" (model.watts |> wattsToKilowatts |> kilowattsToFloat) (model.watts |> wattsToKilowatts |> formatKilowatts) PowerSolve
                    , renderField model MegawattsField "Megawatt" (model.watts |> wattsToMegawatts |> megawattsToFloat) (model.watts |> wattsToMegawatts |> formatMegawatts) PowerSolve
                    , renderField model GigawattsField "Gigawatt" (model.watts |> wattsToGigawatts |> gigawattsToFloat) (model.watts |> wattsToGigawatts |> formatGigawatts) PowerSolve
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


calculateVoltage : Watts -> Amps -> Volts
calculateVoltage (Watts wattsFloat) (Amps ampsValue) =
    Volts (wattsFloat / ampsValue)


calculateCurrent : Watts -> Volts -> Amps
calculateCurrent (Watts wattsValue) (Volts voltsValue) =
    Amps (wattsValue / voltsValue)


calculatePower : Volts -> Amps -> Watts
calculatePower (Volts voltsValue) (Amps ampsValue) =
    Watts (voltsValue * ampsValue)


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


updateWatts : Model -> Watts -> Model
updateWatts model watts =
    case model.solveMethod of
        VoltageSolve ->
            { model | watts = watts, volts = calculateVoltage watts model.amps }

        CurrentSolve ->
            { model | watts = watts, amps = calculateCurrent watts model.volts }

        _ ->
            model


updateVolts : Model -> Volts -> Model
updateVolts model volts =
    case model.solveMethod of
        PowerSolve ->
            { model | volts = volts, watts = calculatePower volts model.amps }

        CurrentSolve ->
            { model | volts = volts, amps = calculateCurrent model.watts volts }

        _ ->
            model


updateAmps : Model -> Amps -> Model
updateAmps model amps =
    case model.solveMethod of
        PowerSolve ->
            { model | amps = amps, watts = calculatePower model.volts amps }

        VoltageSolve ->
            { model | amps = amps, volts = calculateVoltage model.watts amps }

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
                                WattsField ->
                                    updateWatts model (Watts floatValue)

                                KilowattsField ->
                                    updateWatts model (Kilowatts floatValue |> kilowattsToWatts)

                                MegawattsField ->
                                    updateWatts model (Megawatts floatValue |> megawattsToWatts)

                                GigawattsField ->
                                    updateWatts model (Gigawatts floatValue |> gigawattsToWatts)

                                VoltsField ->
                                    updateVolts model (Volts floatValue)

                                KilovoltsField ->
                                    updateVolts model (Kilovolts floatValue |> kilovoltsToVolts)

                                MegavoltsField ->
                                    updateVolts model (Megavolts floatValue |> megavoltsToVolts)

                                GigavoltsField ->
                                    updateVolts model (Gigavolts floatValue |> gigavoltsToVolts)

                                AmpsField ->
                                    updateAmps model (Amps floatValue)

                                KiloampsField ->
                                    updateAmps model (Kiloamps floatValue |> kiloampsToAmps)

                                MegaampsField ->
                                    updateAmps model (Megaamps floatValue |> megaampsToAmps)

                                GigaampsField ->
                                    updateAmps model (Gigaamps floatValue |> gigaampsToAmps)

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
                        VoltageExample watts amps ->
                            { model | solveMethod = VoltageSolve, watts = watts, amps = amps, volts = calculateVoltage watts amps }

                        CurrentExample watts volts ->
                            { model | solveMethod = CurrentSolve, watts = watts, volts = volts, amps = calculateCurrent watts volts }

                        PowerExample volts amps ->
                            { model | solveMethod = PowerSolve, volts = volts, amps = amps, watts = calculatePower volts amps }
            in
            ( { newModel | formStatus = Valid, activeField = NoActiveField }, Cmd.none )
