module Units.Electricity exposing
    ( Amps(..)
    , Hertz(..)
    , Ohms(..)
    , Volts(..)
    , WattHourCost(..)
    , WattHours(..)
    , Watts(..)
    , currentToFloat
    , energyCostToFloat
    , energyToFloat
    , floatToCurrent
    , floatToEnergy
    , floatToEnergyCost
    , floatToFrequency
    , floatToPower
    , floatToResistance
    , floatToVoltage
    , formatCurrent
    , formatEnergy
    , formatEnergyCost
    , formatFrequency
    , formatPower
    , formatResistance
    , formatVoltage
    , frequencyToFloat
    , powerToFloat
    , resistanceToFloat
    , voltageToFloat
    )

import Units.Metric exposing (Direction(..), Prefix(..), convertPrefix, prefixToLabel)
import Units.Number exposing (formatFloat)



-- FREQUENCY


type Hertz
    = Hertz Float


floatToFrequency : Prefix -> Float -> Hertz
floatToFrequency oldPrefix value =
    Hertz (convertPrefix DESC value oldPrefix Base)


frequencyToFloat : Prefix -> Hertz -> Float
frequencyToFloat newPrefix (Hertz value) =
    convertPrefix DESC value Base newPrefix


formatFrequency : Prefix -> Hertz -> String
formatFrequency newPrefix hertz =
    (hertz |> frequencyToFloat newPrefix |> formatFloat) ++ " " ++ prefixToLabel newPrefix ++ "Hz"



-- CURRENT


type Amps
    = Amps Float


floatToCurrent : Prefix -> Float -> Amps
floatToCurrent oldPrefix value =
    Amps (convertPrefix DESC value oldPrefix Base)


currentToFloat : Prefix -> Amps -> Float
currentToFloat newPrefix (Amps value) =
    convertPrefix DESC value Base newPrefix


formatCurrent : Prefix -> Amps -> String
formatCurrent newPrefix amps =
    (amps |> currentToFloat newPrefix |> formatFloat) ++ " " ++ prefixToLabel newPrefix ++ "A"



-- VOLTAGE


type Volts
    = Volts Float


floatToVoltage : Prefix -> Float -> Volts
floatToVoltage oldPrefix value =
    Volts (convertPrefix DESC value oldPrefix Base)


voltageToFloat : Prefix -> Volts -> Float
voltageToFloat newPrefix (Volts value) =
    convertPrefix DESC value Base newPrefix


formatVoltage : Prefix -> Volts -> String
formatVoltage newPrefix volts =
    (volts |> voltageToFloat newPrefix |> formatFloat) ++ " " ++ prefixToLabel newPrefix ++ "V"



-- POWER


type Watts
    = Watts Float


floatToPower : Prefix -> Float -> Watts
floatToPower oldPrefix value =
    Watts (convertPrefix DESC value oldPrefix Base)


powerToFloat : Prefix -> Watts -> Float
powerToFloat newPrefix (Watts value) =
    convertPrefix DESC value Base newPrefix


formatPower : Prefix -> Watts -> String
formatPower newPrefix watts =
    (watts |> powerToFloat newPrefix |> formatFloat) ++ " " ++ prefixToLabel newPrefix ++ "W"



-- RESISTANCE


type Ohms
    = Ohms Float


floatToResistance : Prefix -> Float -> Ohms
floatToResistance oldPrefix value =
    Ohms (convertPrefix DESC value oldPrefix Base)


resistanceToFloat : Prefix -> Ohms -> Float
resistanceToFloat newPrefix (Ohms value) =
    convertPrefix DESC value Base newPrefix


formatResistance : Prefix -> Ohms -> String
formatResistance newPrefix watts =
    (watts |> resistanceToFloat newPrefix |> formatFloat) ++ " " ++ prefixToLabel newPrefix ++ "Ω"



-- ENERGY


type WattHours
    = WattHours Float


floatToEnergy : Prefix -> Float -> WattHours
floatToEnergy oldPrefix value =
    WattHours (convertPrefix DESC value oldPrefix Base)


energyToFloat : Prefix -> WattHours -> Float
energyToFloat newPrefix (WattHours value) =
    convertPrefix DESC value Base newPrefix


formatEnergy : Prefix -> WattHours -> String
formatEnergy newPrefix wattHours =
    (wattHours |> energyToFloat newPrefix |> formatFloat) ++ " " ++ prefixToLabel newPrefix ++ "Wh"



-- ENERGY COST


type WattHourCost
    = WattHourCost Float


floatToEnergyCost : Prefix -> Float -> WattHourCost
floatToEnergyCost oldPrefix value =
    WattHourCost (convertPrefix ASC value oldPrefix Base)


energyCostToFloat : Prefix -> WattHourCost -> Float
energyCostToFloat newPrefix (WattHourCost value) =
    convertPrefix ASC value Base newPrefix


formatEnergyCost : Prefix -> WattHourCost -> String
formatEnergyCost newPrefix wattHourCost =
    (wattHourCost |> energyCostToFloat newPrefix |> formatFloat) ++ "/" ++ prefixToLabel newPrefix ++ "Wh"
