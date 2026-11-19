module Main exposing (..)


isValidQueens : List Int -> Bool
isValidQueens qs =
    validRows (List.length qs) [] qs


validRows : Int -> List Int -> List Int -> Bool
validRows boardSize previousRows remainingRows =
    case remainingRows of
        [] ->
            True

        row :: rest ->
            row >= 1
                && row <= boardSize
                && safeFromPrevious row previousRows
                && validRows boardSize (row :: previousRows) rest


safeFromPrevious : Int -> List Int -> Bool
safeFromPrevious row previousRows =
    checkDistances row 1 previousRows


checkDistances : Int -> Int -> List Int -> Bool
checkDistances row distance previousRows =
    case previousRows of
        [] ->
            True

        previousRow :: rest ->
            row /= previousRow
                && abs (row - previousRow) /= distance
                && checkDistances row (distance + 1) rest
