module Units.Metric exposing (baseToGiga, baseToKilo, baseToMega, gigaToBase, kiloToBase, megaToBase)


baseToKilo : Float -> Float
baseToKilo value =
    value / 1000


baseToMega : Float -> Float
baseToMega value =
    value / 1000000


baseToGiga : Float -> Float
baseToGiga value =
    value / 1000000000


kiloToBase : Float -> Float
kiloToBase value =
    value * 1000


megaToBase : Float -> Float
megaToBase value =
    value * 1000000


gigaToBase : Float -> Float
gigaToBase value =
    value * 1000000000
