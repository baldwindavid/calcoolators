module Routes exposing (Route(..), parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | EnergyCostCalculator
    | PowerTimeEnergyCalculator
    | VoltageCurrentPowerCalculator
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
        , map PowerTimeEnergyCalculator (s "power-time-energy")
        , map VoltageCurrentPowerCalculator (s "voltage-current-power")
        , map EnergyCostCalculator (s "energy-cost")
        , map FrequencyRpmPolesCalculator (s "frequency-revolutions-poles")
        ]
