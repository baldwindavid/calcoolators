module Routes exposing (Route(..), parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | EnergyCostCalculator
    | PowerTimeEnergyCalculator
    | VoltageCurrentPowerCalculator
    | VoltageCurrentResistanceCalculator
    | FrequencyRpmPolesCalculator


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Home top
        , map PowerTimeEnergyCalculator (s "electricity" </> s "power-time-energy")
        , map VoltageCurrentPowerCalculator (s "electricity" </> s "voltage-current-power")
        , map VoltageCurrentResistanceCalculator (s "electricity" </> s "voltage-current-resistance")
        , map EnergyCostCalculator (s "electricity" </> s "energy-cost")
        , map FrequencyRpmPolesCalculator (s "electricity" </> s "frequency-revolutions-poles")
        ]
