module Units.Electricity exposing
    ( Amps(..)
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
    , floatToPower
    , floatToVoltage
    , formatCurrent
    , formatEnergy
    , formatEnergyCost
    , formatPower
    , formatVoltage
    , powerToFloat
    , voltageToFloat
    )

import Units.Metric exposing (Direction(..), Prefix(..), convertPrefix, prefixToLabel)
import Units.Number exposing (formatFloat)



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
