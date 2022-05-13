module Pages.PowerTimeEnergyCalculator exposing (Model, Msg, init, update, view)

import Forms exposing (formControl)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (cardBody, cardContainer, cardHeader, cardHeaderToggleButton, cardTitle, cardsGrid, pageHeader, resourceLink, resourcesContainer)
import Units.Electricity
    exposing
        ( GigawattHours(..)
        , Gigawatts(..)
        , KilowattHours(..)
        , Kilowatts(..)
        , MegawattHours(..)
        , Megawatts(..)
        , WattHours(..)
        , Watts(..)
        , formatGigawattHours
        , formatGigawatts
        , formatKilowattHours
        , formatKilowatts
        , formatMegawattHours
        , formatMegawatts
        , formatWattHours
        , formatWatts
        , gigawattHoursToFloat
        , gigawattHoursToWattHours
        , gigawattsToFloat
        , gigawattsToWatts
        , kilowattHoursToFloat
        , kilowattHoursToWattHours
        , kilowattsToFloat
        , kilowattsToWatts
        , megawattHoursToFloat
        , megawattHoursToWattHours
        , megawattsToFloat
        , megawattsToWatts
        , wattHoursToFloat
        , wattHoursToGigawattHours
        , wattHoursToKilowattHours
        , wattHoursToMegawattHours
        , wattsToFloat
        , wattsToGigawatts
        , wattsToKilowatts
        , wattsToMegawatts
        )
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


type alias Model =
    { watts : Watts
    , wattHours : WattHours
    , seconds : Seconds
    , activeField : Field
    , typedValue : String
    , formStatus : CalculationResult
    , solveMethod : SolveMethod
    }


type CalculationResult
    = Valid
    | Invalid String


type SolveMethod
    = PowerSolve
    | EnergySolve
    | TimeSolve


type Example
    = EnergyExample Watts Seconds
    | PowerExample WattHours Seconds
    | TimeExample Watts WattHours



-- INIT


init : { currentTime : Int } -> ( Model, Cmd Msg )
init { currentTime } =
    let
        watts =
            Watts 30000

        wattHours =
            WattHours 30000
    in
    ( { watts = watts
      , activeField = NoActiveField
      , typedValue = ""
      , formStatus = Valid
      , wattHours = wattHours
      , seconds = Seconds (60 * 60)
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

                TimeSolve ->
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
                    [ renderField model WattsField "Watts" (model.watts |> wattsToFloat) (model.watts |> formatWatts) PowerSolve
                    , renderField model KilowattsField "Kilowatts" (model.watts |> wattsToKilowatts |> kilowattsToFloat) (model.watts |> wattsToKilowatts |> formatKilowatts) PowerSolve
                    , renderField model MegawattsField "Megawatts" (model.watts |> wattsToMegawatts |> megawattsToFloat) (model.watts |> wattsToMegawatts |> formatMegawatts) PowerSolve
                    , renderField model GigawattsField "Gigawatts" (model.watts |> wattsToGigawatts |> gigawattsToFloat) (model.watts |> wattsToGigawatts |> formatGigawatts) PowerSolve
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == TimeSolve)
                    [ cardTitle "Time"
                    , cardHeaderToggleButton (model.solveMethod == TimeSolve) "Solving for Time" "Solve for Time" (SetSolveMethod TimeSolve)
                    ]
                , cardBody
                    [ renderField model SecondsField "Seconds" (model.seconds |> secondsToFloat) (model.seconds |> formatSeconds) TimeSolve
                    , renderField model MinutesField "Minutes" (model.seconds |> secondsToMinutes |> minutesToFloat) (model.seconds |> secondsToMinutes |> formatMinutes) TimeSolve
                    , renderField model HoursField "Hours" (model.seconds |> secondsToHours |> hoursToFloat) (model.seconds |> secondsToHours |> formatHours) TimeSolve
                    , renderField model DaysField "Days" (model.seconds |> secondsToDays |> daysToFloat) (model.seconds |> secondsToDays |> formatDays) TimeSolve
                    ]
                ]
            , cardContainer
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
            ]
        , resourcesContainer "Examples"
            "Click the questions below to auto-fill the form with the solution:"
            [ resourceLink "The average US home uses 30kWh of energy per day. What is the average power?" (SetExample (PowerExample (KilowattHours 30 |> kilowattHoursToWattHours) (Days 1 |> daysToSeconds)))
            , resourceLink "How long would it take for a central air unit running at 3000 watts to reach 30 kilowatt hours of energy?" (SetExample (TimeExample (Watts 3000) (KilowattHours 30 |> kilowattHoursToWattHours)))
            , resourceLink "How much energy would be used by a 100w light bulb in 10 hours?" (SetExample (EnergyExample (Watts 100) (Hours 10 |> hoursToSeconds)))
            ]
        ]


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
                                    updateWatts model (Watts floatValue)

                                KilowattsField ->
                                    updateWatts model (Kilowatts floatValue |> kilowattsToWatts)

                                MegawattsField ->
                                    updateWatts model (Megawatts floatValue |> megawattsToWatts)

                                GigawattsField ->
                                    updateWatts model (Gigawatts floatValue |> gigawattsToWatts)

                                WattHoursField ->
                                    updateWattHours model (WattHours floatValue)

                                KilowattHoursField ->
                                    updateWattHours model (KilowattHours floatValue |> kilowattHoursToWattHours)

                                MegawattHoursField ->
                                    updateWattHours model (MegawattHours floatValue |> megawattHoursToWattHours)

                                GigawattHoursField ->
                                    updateWattHours model (GigawattHours floatValue |> gigawattHoursToWattHours)

                                SecondsField ->
                                    updateSeconds model (Seconds floatValue)

                                MinutesField ->
                                    updateSeconds model (Minutes floatValue |> minutesToSeconds)

                                HoursField ->
                                    updateSeconds model (Hours floatValue |> hoursToSeconds)

                                DaysField ->
                                    updateSeconds model (Days floatValue |> daysToSeconds)

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
                        EnergyExample watts seconds ->
                            { model | solveMethod = EnergySolve, watts = watts, seconds = seconds, wattHours = calculateEnergy watts seconds }

                        PowerExample wattHours seconds ->
                            { model | solveMethod = PowerSolve, watts = calculatePower wattHours seconds, seconds = seconds, wattHours = wattHours }

                        TimeExample watts wattHours ->
                            { model | solveMethod = TimeSolve, watts = watts, seconds = calculateTime watts wattHours, wattHours = wattHours }
            in
            ( { newModel | formStatus = Valid, activeField = NoActiveField }, Cmd.none )


updateWatts : Model -> Watts -> Model
updateWatts model watts =
    case model.solveMethod of
        EnergySolve ->
            { model | watts = watts, wattHours = calculateEnergy watts model.seconds }

        TimeSolve ->
            { model | watts = watts, seconds = calculateTime watts model.wattHours }

        _ ->
            model


updateWattHours : Model -> WattHours -> Model
updateWattHours model wattHours =
    case model.solveMethod of
        PowerSolve ->
            { model | wattHours = wattHours, watts = calculatePower wattHours model.seconds }

        TimeSolve ->
            { model | wattHours = wattHours, seconds = calculateTime model.watts wattHours }

        _ ->
            model


updateSeconds : Model -> Seconds -> Model
updateSeconds model seconds =
    case model.solveMethod of
        PowerSolve ->
            { model | seconds = seconds, watts = calculatePower model.wattHours seconds }

        EnergySolve ->
            { model | seconds = seconds, wattHours = calculateEnergy model.watts seconds }

        _ ->
            model


calculatePower : WattHours -> Seconds -> Watts
calculatePower (WattHours wattHoursValue) seconds =
    Watts (wattHoursValue / (seconds |> secondsToHours |> hoursToFloat))


calculateTime : Watts -> WattHours -> Seconds
calculateTime (Watts wattsFloat) (WattHours wattHoursFloat) =
    Seconds (wattHoursFloat / wattsFloat * 3600)


calculateEnergy : Watts -> Seconds -> WattHours
calculateEnergy (Watts wattsFloat) seconds =
    WattHours (wattsFloat * (seconds |> secondsToHours |> hoursToFloat))
