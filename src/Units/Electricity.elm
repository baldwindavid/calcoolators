module Units.Electricity exposing
    ( Amps(..)
    , Gigaamps(..)
    , Gigavolts(..)
    , GigawattHourCost(..)
    , GigawattHours(..)
    , Gigawatts(..)
    , Kiloamps(..)
    , Kilovolts(..)
    , KilowattHourCost(..)
    , KilowattHours(..)
    , Kilowatts(..)
    , Megaamps(..)
    , Megavolts(..)
    , MegawattHourCost(..)
    , MegawattHours(..)
    , Megawatts(..)
    , Volts(..)
    , WattHourCost(..)
    , WattHours(..)
    , Watts(..)
    , ampsToFloat
    , ampsToGigaamps
    , ampsToKiloamps
    , ampsToMegaamps
    , formatAmps
    , formatGigaamps
    , formatGigavolts
    , formatGigawattHourCost
    , formatGigawattHours
    , formatGigawatts
    , formatKiloamps
    , formatKilovolts
    , formatKilowattHourCost
    , formatKilowattHours
    , formatKilowatts
    , formatMegaamps
    , formatMegavolts
    , formatMegawattHourCost
    , formatMegawattHours
    , formatMegawatts
    , formatVolts
    , formatWattHourCost
    , formatWattHours
    , formatWatts
    , gigaampsToAmps
    , gigaampsToFloat
    , gigavoltsToFloat
    , gigavoltsToVolts
    , gigawattHourCostToFloat
    , gigawattHourCostToWattHourCost
    , gigawattHoursToFloat
    , gigawattHoursToWattHours
    , gigawattsToFloat
    , gigawattsToWatts
    , kiloampsToAmps
    , kiloampsToFloat
    , kilovoltsToFloat
    , kilovoltsToVolts
    , kilowattHourCostToFloat
    , kilowattHourCostToWattHourCost
    , kilowattHoursToFloat
    , kilowattHoursToWattHours
    , kilowattsToFloat
    , kilowattsToWatts
    , megaampsToAmps
    , megaampsToFloat
    , megavoltsToFloat
    , megavoltsToVolts
    , megawattHourCostToFloat
    , megawattHourCostToWattHourCost
    , megawattHoursToFloat
    , megawattHoursToWattHours
    , megawattsToFloat
    , megawattsToWatts
    , voltsToFloat
    , voltsToGigavolts
    , voltsToKilovolts
    , voltsToMegavolts
    , wattHourCostToFloat
    , wattHourCostToGigawattHourCost
    , wattHourCostToKilowattHourCost
    , wattHourCostToMegawattHourCost
    , wattHoursToFloat
    , wattHoursToGigawattHours
    , wattHoursToKilowattHours
    , wattHoursToMegawattHours
    , wattsToFloat
    , wattsToGigawatts
    , wattsToKilowatts
    , wattsToMegawatts
    )

import Units.Currency exposing (formatFloatAsCurrency)
import Units.Metric exposing (baseToGiga, baseToKilo, baseToMega, gigaToBase, kiloToBase, megaToBase)
import Units.Number exposing (formatFloat, formatInt)


type Volts
    = Volts Float


type Kilovolts
    = Kilovolts Float


type Megavolts
    = Megavolts Float


type Gigavolts
    = Gigavolts Float


type Amps
    = Amps Float


type Kiloamps
    = Kiloamps Float


type Megaamps
    = Megaamps Float


type Gigaamps
    = Gigaamps Float


type WattHourCost
    = WattHourCost Float


type KilowattHourCost
    = KilowattHourCost Float


type MegawattHourCost
    = MegawattHourCost Float


type GigawattHourCost
    = GigawattHourCost Float


type Watts
    = Watts Float


type Kilowatts
    = Kilowatts Float


type Megawatts
    = Megawatts Float


type Gigawatts
    = Gigawatts Float


type WattHours
    = WattHours Float


type KilowattHours
    = KilowattHours Float


type MegawattHours
    = MegawattHours Float


type GigawattHours
    = GigawattHours Float


voltsToKilovolts : Volts -> Kilovolts
voltsToKilovolts (Volts value) =
    Kilovolts (baseToKilo value)


voltsToMegavolts : Volts -> Megavolts
voltsToMegavolts (Volts value) =
    Megavolts (baseToMega value)


voltsToGigavolts : Volts -> Gigavolts
voltsToGigavolts (Volts value) =
    Gigavolts (baseToGiga value)


kilovoltsToVolts : Kilovolts -> Volts
kilovoltsToVolts (Kilovolts value) =
    Volts (kiloToBase value)


megavoltsToVolts : Megavolts -> Volts
megavoltsToVolts (Megavolts value) =
    Volts (megaToBase value)


gigavoltsToVolts : Gigavolts -> Volts
gigavoltsToVolts (Gigavolts value) =
    Volts (gigaToBase value)


voltsToFloat : Volts -> Float
voltsToFloat (Volts value) =
    value


kilovoltsToFloat : Kilovolts -> Float
kilovoltsToFloat (Kilovolts value) =
    value


megavoltsToFloat : Megavolts -> Float
megavoltsToFloat (Megavolts value) =
    value


gigavoltsToFloat : Gigavolts -> Float
gigavoltsToFloat (Gigavolts value) =
    value


formatVolts : Volts -> String
formatVolts volts =
    (volts |> voltsToFloat |> formatFloat) ++ " V"


formatKilovolts : Kilovolts -> String
formatKilovolts kilovolts =
    (kilovolts |> kilovoltsToFloat |> formatFloat) ++ " kV"


formatMegavolts : Megavolts -> String
formatMegavolts megavolts =
    (megavolts |> megavoltsToFloat |> formatFloat) ++ " mV"


formatGigavolts : Gigavolts -> String
formatGigavolts gigavolts =
    (gigavolts |> gigavoltsToFloat |> formatFloat) ++ " gV"


ampsToKiloamps : Amps -> Kiloamps
ampsToKiloamps (Amps value) =
    Kiloamps (baseToKilo value)


ampsToMegaamps : Amps -> Megaamps
ampsToMegaamps (Amps value) =
    Megaamps (baseToMega value)


ampsToGigaamps : Amps -> Gigaamps
ampsToGigaamps (Amps value) =
    Gigaamps (baseToGiga value)


kiloampsToAmps : Kiloamps -> Amps
kiloampsToAmps (Kiloamps value) =
    Amps (kiloToBase value)


megaampsToAmps : Megaamps -> Amps
megaampsToAmps (Megaamps value) =
    Amps (megaToBase value)


gigaampsToAmps : Gigaamps -> Amps
gigaampsToAmps (Gigaamps value) =
    Amps (gigaToBase value)


ampsToFloat : Amps -> Float
ampsToFloat (Amps value) =
    value


kiloampsToFloat : Kiloamps -> Float
kiloampsToFloat (Kiloamps value) =
    value


megaampsToFloat : Megaamps -> Float
megaampsToFloat (Megaamps value) =
    value


gigaampsToFloat : Gigaamps -> Float
gigaampsToFloat (Gigaamps value) =
    value


formatAmps : Amps -> String
formatAmps amps =
    (amps |> ampsToFloat |> formatFloat) ++ " A"


formatKiloamps : Kiloamps -> String
formatKiloamps kiloamps =
    (kiloamps |> kiloampsToFloat |> formatFloat) ++ " kA"


formatMegaamps : Megaamps -> String
formatMegaamps megaamps =
    (megaamps |> megaampsToFloat |> formatFloat) ++ " mA"


formatGigaamps : Gigaamps -> String
formatGigaamps gigaamps =
    (gigaamps |> gigaampsToFloat |> formatFloat) ++ " gA"


wattsToKilowatts : Watts -> Kilowatts
wattsToKilowatts (Watts value) =
    Kilowatts (baseToKilo value)


wattsToMegawatts : Watts -> Megawatts
wattsToMegawatts (Watts value) =
    Megawatts (baseToMega value)


wattsToGigawatts : Watts -> Gigawatts
wattsToGigawatts (Watts value) =
    Gigawatts (baseToGiga value)


kilowattsToWatts : Kilowatts -> Watts
kilowattsToWatts (Kilowatts value) =
    Watts (kiloToBase value)


megawattsToWatts : Megawatts -> Watts
megawattsToWatts (Megawatts value) =
    Watts (megaToBase value)


gigawattsToWatts : Gigawatts -> Watts
gigawattsToWatts (Gigawatts value) =
    Watts (gigaToBase value)


wattHoursToKilowattHours : WattHours -> KilowattHours
wattHoursToKilowattHours (WattHours value) =
    KilowattHours (baseToKilo value)


wattHoursToMegawattHours : WattHours -> MegawattHours
wattHoursToMegawattHours (WattHours value) =
    MegawattHours (baseToMega value)


wattHoursToGigawattHours : WattHours -> GigawattHours
wattHoursToGigawattHours (WattHours value) =
    GigawattHours (baseToGiga value)


kilowattHoursToWattHours : KilowattHours -> WattHours
kilowattHoursToWattHours (KilowattHours value) =
    WattHours (kiloToBase value)


megawattHoursToWattHours : MegawattHours -> WattHours
megawattHoursToWattHours (MegawattHours value) =
    WattHours (megaToBase value)


gigawattHoursToWattHours : GigawattHours -> WattHours
gigawattHoursToWattHours (GigawattHours value) =
    WattHours (gigaToBase value)


wattHourCostToKilowattHourCost : WattHourCost -> KilowattHourCost
wattHourCostToKilowattHourCost (WattHourCost value) =
    KilowattHourCost (kiloToBase value)


wattHourCostToMegawattHourCost : WattHourCost -> MegawattHourCost
wattHourCostToMegawattHourCost (WattHourCost value) =
    MegawattHourCost (megaToBase value)


wattHourCostToGigawattHourCost : WattHourCost -> GigawattHourCost
wattHourCostToGigawattHourCost (WattHourCost value) =
    GigawattHourCost (gigaToBase value)


kilowattHourCostToWattHourCost : KilowattHourCost -> WattHourCost
kilowattHourCostToWattHourCost (KilowattHourCost value) =
    WattHourCost (baseToKilo value)


megawattHourCostToWattHourCost : MegawattHourCost -> WattHourCost
megawattHourCostToWattHourCost (MegawattHourCost value) =
    WattHourCost (baseToMega value)


gigawattHourCostToWattHourCost : GigawattHourCost -> WattHourCost
gigawattHourCostToWattHourCost (GigawattHourCost value) =
    WattHourCost (baseToGiga value)


wattsToFloat : Watts -> Float
wattsToFloat (Watts value) =
    value


kilowattsToFloat : Kilowatts -> Float
kilowattsToFloat (Kilowatts value) =
    value


megawattsToFloat : Megawatts -> Float
megawattsToFloat (Megawatts value) =
    value


gigawattsToFloat : Gigawatts -> Float
gigawattsToFloat (Gigawatts value) =
    value


wattHoursToFloat : WattHours -> Float
wattHoursToFloat (WattHours value) =
    value


kilowattHoursToFloat : KilowattHours -> Float
kilowattHoursToFloat (KilowattHours value) =
    value


megawattHoursToFloat : MegawattHours -> Float
megawattHoursToFloat (MegawattHours value) =
    value


gigawattHoursToFloat : GigawattHours -> Float
gigawattHoursToFloat (GigawattHours value) =
    value


wattHourCostToFloat : WattHourCost -> Float
wattHourCostToFloat (WattHourCost value) =
    value


kilowattHourCostToFloat : KilowattHourCost -> Float
kilowattHourCostToFloat (KilowattHourCost value) =
    value


megawattHourCostToFloat : MegawattHourCost -> Float
megawattHourCostToFloat (MegawattHourCost value) =
    value


gigawattHourCostToFloat : GigawattHourCost -> Float
gigawattHourCostToFloat (GigawattHourCost value) =
    value


formatWatts : Watts -> String
formatWatts watts =
    (watts |> wattsToFloat |> formatFloat) ++ " W"


formatKilowatts : Kilowatts -> String
formatKilowatts kilowatts =
    (kilowatts |> kilowattsToFloat |> formatFloat) ++ " kW"


formatMegawatts : Megawatts -> String
formatMegawatts megawatts =
    (megawatts |> megawattsToFloat |> formatFloat) ++ " mW"


formatGigawatts : Gigawatts -> String
formatGigawatts gigawatts =
    (gigawatts |> gigawattsToFloat |> formatFloat) ++ " gW"


formatWattHours : WattHours -> String
formatWattHours wattHours =
    (wattHours |> wattHoursToFloat |> formatFloat) ++ " Wh"


formatKilowattHours : KilowattHours -> String
formatKilowattHours kilowattHours =
    (kilowattHours |> kilowattHoursToFloat |> formatFloat) ++ " kWh"


formatMegawattHours : MegawattHours -> String
formatMegawattHours megawattHours =
    (megawattHours |> megawattHoursToFloat |> formatFloat) ++ " mWh"


formatGigawattHours : GigawattHours -> String
formatGigawattHours gigawattHours =
    (gigawattHours |> gigawattHoursToFloat |> formatFloat) ++ " gWh"


formatWattHourCost : WattHourCost -> String
formatWattHourCost wattHourCost =
    (wattHourCost |> wattHourCostToFloat |> formatFloatAsCurrency) ++ "/Wh"


formatKilowattHourCost : KilowattHourCost -> String
formatKilowattHourCost kilowattHourCost =
    (kilowattHourCost |> kilowattHourCostToFloat |> formatFloatAsCurrency) ++ "/kWh"


formatMegawattHourCost : MegawattHourCost -> String
formatMegawattHourCost megawattHourCost =
    (megawattHourCost |> megawattHourCostToFloat |> formatFloatAsCurrency) ++ "/mWh"


formatGigawattHourCost : GigawattHourCost -> String
formatGigawattHourCost gigawattHourCost =
    (gigawattHourCost |> gigawattHourCostToFloat |> formatFloatAsCurrency) ++ "/gWh"
