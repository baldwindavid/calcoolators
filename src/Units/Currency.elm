module Units.Currency exposing (Currency(..), currencyToFloat, formatCurrency, formatFloatAsCurrency)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)


type Currency
    = Currency Float


currencyToFloat : Currency -> Float
currencyToFloat (Currency value) =
    value


formatCurrency : Currency -> String
formatCurrency currency =
    currency |> currencyToFloat |> formatFloatAsCurrency


formatFloatAsCurrency : Float -> String
formatFloatAsCurrency value =
    "$" ++ format { usLocale | decimals = Min 2 } value
