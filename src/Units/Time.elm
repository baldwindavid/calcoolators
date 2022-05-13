module Units.Time exposing
    ( Days(..)
    , Hours(..)
    , Minutes(..)
    , Seconds(..)
    , daysToFloat
    , daysToSeconds
    , formatDays
    , formatHours
    , formatMinutes
    , formatSeconds
    , hoursToFloat
    , hoursToSeconds
    , minutesToFloat
    , minutesToSeconds
    , secondsToDays
    , secondsToFloat
    , secondsToHours
    , secondsToHumanReadable
    , secondsToMinutes
    )

import Units.Number exposing (formatFloat, formatInt)
import Units.Text exposing (pluralize)


type Seconds
    = Seconds Float


type Minutes
    = Minutes Float


type Hours
    = Hours Float


type Days
    = Days Float


secondsToHumanReadable : Int -> String
secondsToHumanReadable totalSeconds =
    let
        secondsInDay =
            60 * 60 * 24

        secondsInHour =
            60 * 60

        secondsInMinute =
            60

        days =
            totalSeconds // secondsInDay

        secondsForDays =
            days * secondsInDay

        hours =
            (totalSeconds - secondsForDays) // secondsInHour

        secondsForHours =
            hours * secondsInHour

        minutes =
            (totalSeconds - secondsForDays - secondsForHours) // secondsInMinute

        secondsForMinutes =
            minutes * secondsInMinute

        seconds =
            totalSeconds - secondsForDays - secondsForHours - secondsForMinutes

        segments =
            [ ( "day", days )
            , ( "hour", hours )
            , ( "minute", minutes )
            , ( "second", seconds )
            ]
                |> List.filter (\( _, n ) -> n > 0)

        segmentsString =
            segments
                |> List.map (\( label, count ) -> formatInt count ++ " " ++ pluralize label count)
                |> String.join ", "
    in
    if List.isEmpty segments then
        "less than a second"

    else
        segmentsString


minutesToSeconds : Minutes -> Seconds
minutesToSeconds (Minutes value) =
    Seconds (value * 60)


hoursToSeconds : Hours -> Seconds
hoursToSeconds (Hours value) =
    Seconds (value * 60 * 60)


daysToSeconds : Days -> Seconds
daysToSeconds (Days value) =
    Seconds (value * 24 * 60 * 60)


secondsToMinutes : Seconds -> Minutes
secondsToMinutes (Seconds value) =
    Minutes (value / 60)


secondsToHours : Seconds -> Hours
secondsToHours (Seconds value) =
    Hours (value / 60 / 60)


secondsToDays : Seconds -> Days
secondsToDays (Seconds value) =
    Days (value / 60 / 60 / 24)


secondsToFloat : Seconds -> Float
secondsToFloat (Seconds value) =
    value


minutesToFloat : Minutes -> Float
minutesToFloat (Minutes value) =
    value


hoursToFloat : Hours -> Float
hoursToFloat (Hours value) =
    value


daysToFloat : Days -> Float
daysToFloat (Days value) =
    value


formatSeconds : Seconds -> String
formatSeconds (Seconds value) =
    formatFloat value ++ " secs"


formatMinutes : Minutes -> String
formatMinutes (Minutes value) =
    formatFloat value ++ " mins"


formatHours : Hours -> String
formatHours (Hours value) =
    formatFloat value ++ " hrs"


formatDays : Days -> String
formatDays (Days value) =
    formatFloat value ++ " days"
