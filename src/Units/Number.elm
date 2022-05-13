module Units.Number exposing (formatFloat, formatInt, numberStringToFloat)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)


formatFloat : Float -> String
formatFloat value =
    format { usLocale | decimals = Max 6 } value


formatInt : Int -> String
formatInt value =
    format { usLocale | decimals = Exact 0 } (toFloat value)


numberStringToFloat : String -> Maybe Float
numberStringToFloat numberString =
    numberString
        |> String.replace "," ""
        |> String.toFloat
