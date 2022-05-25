module Pages.FrequencyRpmPolesCalculator exposing (Model, Msg, init, update, view)

import Forms exposing (formControl)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (cardBody, cardContainer, cardHeader, cardHeaderToggleButton, cardTitle, cardsGrid, pageHeader, resourceLink, resourcesContainer)
import Units.Currency exposing (Currency(..), currencyToFloat, formatCurrency)
import Units.Electricity
    exposing
        ( Hertz(..)
        , floatToFrequency
        , formatFrequency
        , frequencyToFloat
        )
import Units.Metric exposing (Prefix(..))
import Units.Number exposing (formatInt, isEven, numberStringToFloat, numberStringToInt)


type Field
    = HertzField
    | RpmField
    | PolesField
    | NoActiveField


type CalculationResult
    = Valid
    | Invalid String


type SolveMethod
    = FrequencySolve
    | RpmSolve
    | PolesSolve


type Rpm
    = Rpm Int


type Poles
    = Poles Int


type Example
    = FrequencyExample Rpm Poles
    | RpmExample Hertz Poles
    | PolesExample Hertz Rpm



-- MODEL


type alias Model =
    { activeField : Field
    , formStatus : CalculationResult
    , typedValue : String
    , frequency : Hertz
    , rpm : Rpm
    , poles : Poles
    , solveMethod : SolveMethod
    }



-- INIT


init : { currentTime : Int } -> ( Model, Cmd Msg )
init { currentTime } =
    let
        frequency =
            Hertz 60

        poles =
            Poles 2
    in
    ( { activeField = NoActiveField
      , formStatus = Valid
      , typedValue = ""
      , frequency = frequency
      , poles = poles
      , rpm = calculateRpm frequency poles
      , solveMethod = RpmSolve
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
                FrequencySolve ->
                    [ text "RPMs"
                    , span [ class altClass ] [ text "/" ]
                    , text "Poles"
                    , span [ class altClass ] [ text "=" ]
                    , text "Frequency"
                    ]

                PolesSolve ->
                    [ text "RPMs"
                    , span [ class altClass ] [ text "/" ]
                    , text "Frequency"
                    , span [ class altClass ] [ text "=" ]
                    , text "Poles"
                    ]

                RpmSolve ->
                    [ text "Frequency"
                    , span [ class altClass ] [ text "x" ]
                    , text "Poles"
                    , span [ class altClass ] [ text "=" ]
                    , text "RPMs"
                    ]
    in
    div []
        [ pageHeader formula
        , cardsGrid 3
            [ cardContainer
                [ cardHeader (model.solveMethod == FrequencySolve)
                    [ cardTitle "Frequency"
                    , cardHeaderToggleButton (model.solveMethod == FrequencySolve) "Solving for Frequency" "Solve for Frequency" (SetSolveMethod FrequencySolve)
                    ]
                , cardBody
                    [ renderMetricField model HertzField model.frequency Base "Hertz" frequencyToFloat formatFrequency FrequencySolve UpdateHertzField
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == PolesSolve)
                    [ cardTitle "Rotor Poles"
                    , cardHeaderToggleButton (model.solveMethod == PolesSolve) "Solving for Rotor Poles" "Solve for Rotor Poles" (SetSolveMethod PolesSolve)
                    ]
                , cardBody
                    [ renderField model PolesField "# Poles" (polesToInt model.poles |> String.fromInt) (formatPoles model.poles) PolesSolve UpdatePolesField
                    ]
                ]
            , cardContainer
                [ cardHeader (model.solveMethod == RpmSolve)
                    [ cardTitle "RPMs"
                    , cardHeaderToggleButton (model.solveMethod == RpmSolve) "Solving for RPMs" "Solve for RPMs" (SetSolveMethod RpmSolve)
                    ]
                , cardBody
                    [ renderField model RpmField "RPMs" (rpmToInt model.rpm |> String.fromInt) (formatRpm model.rpm) RpmSolve UpdateRpmField
                    ]
                ]
            ]
        , resourcesContainer "Examples"
            "Click the questions below to auto-fill the form with the solution:"
            [ resourceLink "For a frequency of 60Hz with 2 rotor poles what would be the RPMs?" (SetExample (RpmExample (Hertz 60) (Poles 2)))
            , resourceLink "At 3000 RPMs with 2 rotor poles, what is the frequency?" (SetExample (FrequencyExample (Rpm 3000) (Poles 2)))
            , resourceLink "How many rotor poles would there be for 7200 RPMs at a frequency of 60 Hz?" (SetExample (PolesExample (Hertz 60) (Rpm 7200)))
            ]
        ]


polesToInt : Poles -> Int
polesToInt (Poles value) =
    value


formatPoles : Poles -> String
formatPoles (Poles value) =
    formatInt value ++ " poles"


rpmToInt : Rpm -> Int
rpmToInt (Rpm value) =
    value


formatRpm : Rpm -> String
formatRpm (Rpm value) =
    formatInt value ++ " rpm"


renderMetricField : Model -> Field -> unit -> Prefix -> String -> (Prefix -> unit -> Float) -> (Prefix -> unit -> String) -> SolveMethod -> (String -> Msg) -> Html Msg
renderMetricField model field unit prefix label inputFn hintFn solveMethod toMsg =
    renderField model field label (inputFn prefix unit |> String.fromFloat) (hintFn prefix unit) solveMethod toMsg


renderField : Model -> Field -> String -> String -> String -> SolveMethod -> (String -> Msg) -> Html Msg
renderField model field label forInput forHint solveMethod toMsg =
    let
        ( value, errors, hint ) =
            case model.formStatus of
                Valid ->
                    if model.activeField == field then
                        ( model.typedValue, [], forHint )

                    else
                        ( forInput, [], forHint )

                Invalid errorMsg ->
                    if model.activeField == field then
                        ( model.typedValue, [ errorMsg ], "" )

                    else
                        ( forInput, [], forHint )
    in
    formControl label value errors hint (model.solveMethod == solveMethod) toMsg



-- UPDATE


type Msg
    = UpdateHertzField String
    | UpdatePolesField String
    | UpdateRpmField String
    | SetSolveMethod SolveMethod
    | SetExample Example


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSolveMethod solveMethod ->
            ( { model | solveMethod = solveMethod }, Cmd.none )

        UpdateHertzField typedValue ->
            case numberStringToFloat typedValue of
                Just floatValue ->
                    let
                        frequency =
                            Hertz floatValue

                        newModel =
                            case model.solveMethod of
                                PolesSolve ->
                                    { model | frequency = frequency, poles = calculatePoles frequency model.rpm }

                                RpmSolve ->
                                    { model | frequency = frequency, rpm = calculateRpm frequency model.poles }

                                _ ->
                                    model
                    in
                    ( { newModel | formStatus = Valid, activeField = HertzField, typedValue = typedValue }, Cmd.none )

                Nothing ->
                    ( { model | formStatus = Invalid "must be a number", activeField = HertzField, typedValue = typedValue }, Cmd.none )

        UpdatePolesField typedValue ->
            case numberStringToInt typedValue of
                Just intValue ->
                    if isEven intValue then
                        let
                            poles =
                                Poles intValue

                            newModel =
                                case model.solveMethod of
                                    FrequencySolve ->
                                        { model | poles = poles, frequency = calculateFrequency poles model.rpm }

                                    RpmSolve ->
                                        { model | poles = poles, rpm = calculateRpm model.frequency poles }

                                    _ ->
                                        model
                        in
                        ( { newModel | formStatus = Valid, activeField = PolesField, typedValue = typedValue }, Cmd.none )

                    else
                        ( { model | formStatus = Invalid "must be an even integer", activeField = PolesField, typedValue = typedValue }, Cmd.none )

                Nothing ->
                    ( { model | formStatus = Invalid "must be an even integer", activeField = PolesField, typedValue = typedValue }, Cmd.none )

        UpdateRpmField typedValue ->
            case numberStringToInt typedValue of
                Just intValue ->
                    let
                        rpm =
                            Rpm intValue

                        newModel =
                            case model.solveMethod of
                                FrequencySolve ->
                                    { model | rpm = rpm, frequency = calculateFrequency model.poles rpm }

                                PolesSolve ->
                                    { model | rpm = rpm, poles = calculatePoles model.frequency rpm }

                                _ ->
                                    model
                    in
                    ( { newModel | formStatus = Valid, activeField = RpmField, typedValue = typedValue }, Cmd.none )

                Nothing ->
                    ( { model | formStatus = Invalid "must be an integer", activeField = RpmField, typedValue = typedValue }, Cmd.none )

        SetExample example ->
            let
                newModel =
                    case example of
                        FrequencyExample rpm poles ->
                            { model | solveMethod = FrequencySolve, rpm = rpm, poles = poles, frequency = calculateFrequency poles rpm }

                        RpmExample frequency poles ->
                            { model | solveMethod = RpmSolve, frequency = frequency, poles = poles, rpm = calculateRpm frequency poles }

                        PolesExample frequency rpm ->
                            { model | solveMethod = PolesSolve, frequency = frequency, rpm = rpm, poles = calculatePoles frequency rpm }
            in
            ( { newModel | formStatus = Valid, activeField = NoActiveField }, Cmd.none )


calculateFrequency : Poles -> Rpm -> Hertz
calculateFrequency (Poles polesValue) (Rpm rpmValue) =
    Hertz (toFloat rpmValue / toFloat (polesValue // 2) / 60)


calculatePoles : Hertz -> Rpm -> Poles
calculatePoles (Hertz hertzValue) (Rpm rpmValue) =
    Poles (toFloat rpmValue / hertzValue / 60 * 2 |> truncate)


calculateRpm : Hertz -> Poles -> Rpm
calculateRpm (Hertz hertzValue) (Poles polesValue) =
    Rpm (hertzValue / toFloat (polesValue // 2) * 60 |> truncate)
