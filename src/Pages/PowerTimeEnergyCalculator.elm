module Pages.PowerTimeEnergyCalculator exposing (Model, Msg, init, update, view)

import Forms exposing (formControl)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (cardBody, cardContainer, cardHeader, cardHeaderToggleButton, cardTitle, cardsGrid, pageHeader, resourceLink, resourcesContainer)
import Units.Electricity
    exposing
        ( WattHours(..)
        , Watts(..)
        , energyToFloat
        , floatToEnergy
        , floatToPower
        , formatEnergy
        , formatPower
        , powerToFloat
        )
import Units.Metric exposing (Prefix(..))
import Units.Number exposing (formatFloat, formatInt, numberStringToFloat)
import Units.Time
    exposing
        ( Days(..)
        , Hours(..)
        , Minutes(..)
        , Seconds(..)
        , daysToFloat
        , daysToSeconds
        , formatDays
        , formatHours
        , formatMinutes
        , formatSeconds
        , hoursToFloat
        , hoursToSeconds
        , minutesToFloat
        , minutesToSeconds
        , secondsToDays
        , secondsToFloat
        , secondsToHours
        , secondsToMinutes
        )


type Field
    = WattsField
    | KilowattsField
    | MegawattsField
    | GigawattsField
    | WattHoursField
    | KilowattHoursField
    | MegawattHoursField
    | GigawattHoursField
    | DaysField
    | HoursField
    | MinutesField
    | SecondsField
    | NoActiveField


type CalculationResult
    = Valid
    | Invalid String


type SolveMethod
    = PowerSolve
    | EnergySolve
    | DurationSolve


type Example
    = EnergyExample Watts Seconds
    | PowerExample WattHours Seconds
    | DurationExample Watts WattHours



-- MODEL


type alias Model =
    { power : Watts
    , energy : WattHours
    , duration : Seconds
    , activeField : Field
    , typedValue : String
    , formStatus : CalculationResult
    , solveMethod : SolveMethod
    }



-- INIT


init : { currentTime : Int } -> ( Model, Cmd Msg )
init { currentTime } =
    let
        power =
            Watts 30000

        energy =
            WattHours 30000
    in
    ( { power = power
      , activeField = NoActiveField
      , typedValue = ""
      , formStatus = Valid
      , energy = energy
      , duration = Seconds (60 * 60)
      , solveMethod = EnergySolve
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
                PowerSolve ->
                    [ text "Energy"
                    , span [ class altClass ] [ text "/" ]
                    , text "Time"
                    , span [ class altClass ] [ text "=" ]
                    , text "Power"
                    ]

                DurationSolve ->
                    [ text "Energy"
                    , span [ class altClass ] [ text "/" ]
                    , text "Power"
                    , span [ class altClass ] [ text "=" ]
                    , text "Time"
                    ]

                EnergySolve ->
                    [ text "Power"
                    , span [ class altClass ] [ text "x" ]
                    , text "Time"
                    , span [ class altClass ] [ text "=" ]
                    , text "Energy"
                    ]
    in
    div []
        [ pageHeader formula
        , cardsGrid 3
            [ cardContainer
                [ cardHeader (model.solveMethod == PowerSolve)
                    [ cardTitle "Power"
                    , cardHeaderToggleButton (model.solveMethod == PowerSolve) "Solving for Power" "Solve for Power" (SetSolveMethod PowerSolve)
                    ]
                , cardBody
                    [ renderMetricField model WattsField model.power Base "Watts" powerToFloat formatPower PowerSolve
                    , renderMetricField model KilowattsField model.power Kilo "Kilowatts" powerToFloat formatPower PowerSolve
                    , renderMetricField model MegawattsField model.power Mega "Megawatts" powerToFloat formatPower PowerSolve
                    , renderMetricField model GigawattsField model.power Giga "Gigawatts" powerToFloat formatPower PowerSolve
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == DurationSolve)
                    [ cardTitle "Time"
                    , cardHeaderToggleButton (model.solveMethod == DurationSolve) "Solving for Time" "Solve for Time" (SetSolveMethod DurationSolve)
                    ]
                , cardBody
                    [ renderField model SecondsField "Seconds" (model.duration |> secondsToFloat) (model.duration |> formatSeconds) DurationSolve
                    , renderField model MinutesField "Minutes" (model.duration |> secondsToMinutes |> minutesToFloat) (model.duration |> secondsToMinutes |> formatMinutes) DurationSolve
                    , renderField model HoursField "Hours" (model.duration |> secondsToHours |> hoursToFloat) (model.duration |> secondsToHours |> formatHours) DurationSolve
                    , renderField model DaysField "Days" (model.duration |> secondsToDays |> daysToFloat) (model.duration |> secondsToDays |> formatDays) DurationSolve
                    ]
                ]
            , cardContainer
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
            ]
        , resourcesContainer "Examples"
            "Click the questions below to auto-fill the form with the solution:"
            [ resourceLink "The average US home uses 30kWh of energy per day. What is the average power?" (SetExample (PowerExample (floatToEnergy Kilo 30) (Days 1 |> daysToSeconds)))
            , resourceLink "How long would it take for a central air unit running at 3000 watts to reach 30 kilowatt hours of energy?" (SetExample (DurationExample (Watts 3000) (floatToEnergy Kilo 30)))
            , resourceLink "How much energy would be used by a 100w light bulb in 10 hours?" (SetExample (EnergyExample (Watts 100) (Hours 10 |> hoursToSeconds)))
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
                                WattsField ->
                                    updatePower model (Watts floatValue)

                                KilowattsField ->
                                    updatePower model (floatToPower Kilo floatValue)

                                MegawattsField ->
                                    updatePower model (floatToPower Mega floatValue)

                                GigawattsField ->
                                    updatePower model (floatToPower Giga floatValue)

                                WattHoursField ->
                                    updateEnergy model (WattHours floatValue)

                                KilowattHoursField ->
                                    updateEnergy model (floatToEnergy Kilo floatValue)

                                MegawattHoursField ->
                                    updateEnergy model (floatToEnergy Mega floatValue)

                                GigawattHoursField ->
                                    updateEnergy model (floatToEnergy Giga floatValue)

                                SecondsField ->
                                    updateDuration model (Seconds floatValue)

                                MinutesField ->
                                    updateDuration model (Minutes floatValue |> minutesToSeconds)

                                HoursField ->
                                    updateDuration model (Hours floatValue |> hoursToSeconds)

                                DaysField ->
                                    updateDuration model (Days floatValue |> daysToSeconds)

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
                        EnergyExample power duration ->
                            { model | solveMethod = EnergySolve, power = power, duration = duration, energy = calculateEnergy power duration }

                        PowerExample energy duration ->
                            { model | solveMethod = PowerSolve, power = calculatePower energy duration, duration = duration, energy = energy }

                        DurationExample power energy ->
                            { model | solveMethod = DurationSolve, power = power, duration = calculateDuration power energy, energy = energy }
            in
            ( { newModel | formStatus = Valid, activeField = NoActiveField }, Cmd.none )


updatePower : Model -> Watts -> Model
updatePower model power =
    case model.solveMethod of
        EnergySolve ->
            { model | power = power, energy = calculateEnergy power model.duration }

        DurationSolve ->
            { model | power = power, duration = calculateDuration power model.energy }

        _ ->
            model


updateEnergy : Model -> WattHours -> Model
updateEnergy model energy =
    case model.solveMethod of
        PowerSolve ->
            { model | energy = energy, power = calculatePower energy model.duration }

        DurationSolve ->
            { model | energy = energy, duration = calculateDuration model.power energy }

        _ ->
            model


updateDuration : Model -> Seconds -> Model
updateDuration model duration =
    case model.solveMethod of
        PowerSolve ->
            { model | duration = duration, power = calculatePower model.energy duration }

        EnergySolve ->
            { model | duration = duration, energy = calculateEnergy model.power duration }

        _ ->
            model


calculatePower : WattHours -> Seconds -> Watts
calculatePower (WattHours wattHoursValue) duration =
    Watts (wattHoursValue / (duration |> secondsToHours |> hoursToFloat))


calculateDuration : Watts -> WattHours -> Seconds
calculateDuration (Watts wattsFloat) (WattHours wattHoursFloat) =
    Seconds (wattHoursFloat / wattsFloat * 3600)


calculateEnergy : Watts -> Seconds -> WattHours
calculateEnergy (Watts wattsFloat) duration =
    WattHours (wattsFloat * (duration |> secondsToHours |> hoursToFloat))
