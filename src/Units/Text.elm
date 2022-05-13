module Units.Text exposing (pluralize)


pluralize : String -> Int -> String
pluralize word count =
    if count > 1 then
        word ++ "s"

    else
        word
