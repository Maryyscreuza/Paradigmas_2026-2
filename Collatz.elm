module Collatz exposing (collatzLength)


collatzLength : Int -> Int
collatzLength number =
    if number <= 0 then
        0

    else if number == 1 then
        1

    else if modBy 2 number == 0 then
        1 + collatzLength (number // 2)

    else
        1 + collatzLength (3 * number + 1)