module Units.Metric exposing (Direction(..), Prefix(..), convertPrefix, prefixToLabel)


type Prefix
    = Base
    | Kilo
    | Mega
    | Giga


type Direction
    = ASC
    | DESC


convertPrefix : Direction -> Float -> Prefix -> Prefix -> Float
convertPrefix direction value oldPrefix newPrefix =
    let
        oldFactor =
            prefixToFactor oldPrefix

        newFactor =
            prefixToFactor newPrefix

        multiplier =
            case direction of
                ASC ->
                    newFactor / oldFactor

                DESC ->
                    oldFactor / newFactor
    in
    value * multiplier


prefixToLabel : Prefix -> String
prefixToLabel prefix =
    case prefix of
        Base ->
            ""

        Kilo ->
            "k"

        Mega ->
            "m"

        Giga ->
            "g"


prefixToFactor : Prefix -> Float
prefixToFactor prefix =
    case prefix of
        Base ->
            1

        Kilo ->
            1000

        Mega ->
            1000000

        Giga ->
            1000000000
