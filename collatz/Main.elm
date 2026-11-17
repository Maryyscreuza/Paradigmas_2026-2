module Main exposing (..)


collatzLength : Int -> Int
collatzLength n =
    if n <= 0 then
        0

    else if n == 1 then
        1

    else if modBy 2 n == 0 then
        1 + collatzLength (n // 2)

    else
        1 + collatzLength (3 * n + 1)
