module Units.Number exposing (formatFloat, formatInt, isEven, numberStringToFloat, numberStringToInt)

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


numberStringToInt : String -> Maybe Int
numberStringToInt numberString =
    numberString
        |> String.replace "," ""
        |> String.toInt


isEven : Int -> Bool
isEven value =
    remainderBy 2 value == 0
