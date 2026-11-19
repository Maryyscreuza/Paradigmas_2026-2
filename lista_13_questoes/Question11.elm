module Main exposing (..)


type alias Board =
    List (List Int)


puzzle : Board
puzzle =
    [ [ 1, 0, 0, 4 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 3, 2, 0 ]
    ]


solve : Board -> List Board
solve board =
    if hasNoConflicts board then
        solveValid board

    else
        []


solveValid : Board -> List Board
solveValid board =
    case findEmpty board of
        Nothing ->
            [ board ]

        Just ( rowIndex, columnIndex ) ->
            [ 1, 2, 3, 4 ]
                |> List.filter (\candidate -> canPlace board rowIndex columnIndex candidate)
                |> List.concatMap
                    (\candidate ->
                        setCell rowIndex columnIndex candidate board
                            |> solveValid
                    )


hasNoConflicts : Board -> Bool
hasNoConflicts board =
    board
        |> List.indexedMap
            (\rowIndex row ->
                row
                    |> List.indexedMap
                        (\columnIndex value -> ( rowIndex, columnIndex, value ))
            )
        |> List.concat
        |> List.all
            (\( rowIndex, columnIndex, value ) ->
                value == 0
                    || (value >= 1
                            && value <= 4
                            && canPlace (setCell rowIndex columnIndex 0 board) rowIndex columnIndex value
                       )
            )


findEmpty : Board -> Maybe ( Int, Int )
findEmpty board =
    findEmptyInRows 0 board


findEmptyInRows : Int -> Board -> Maybe ( Int, Int )
findEmptyInRows rowIndex rows =
    case rows of
        [] ->
            Nothing

        row :: remainingRows ->
            case findEmptyInRow 0 row of
                Just columnIndex ->
                    Just ( rowIndex, columnIndex )

                Nothing ->
                    findEmptyInRows (rowIndex + 1) remainingRows


findEmptyInRow : Int -> List Int -> Maybe Int
findEmptyInRow columnIndex values =
    case values of
        [] ->
            Nothing

        0 :: _ ->
            Just columnIndex

        _ :: remainingValues ->
            findEmptyInRow (columnIndex + 1) remainingValues


canPlace : Board -> Int -> Int -> Int -> Bool
canPlace board rowIndex columnIndex candidate =
    let
        rowValues =
            itemAt rowIndex board
                |> Maybe.withDefault []

        columnValues =
            List.filterMap (itemAt columnIndex) board

        boxValues =
            blockValues rowIndex columnIndex board
    in
    not
        (List.member candidate rowValues
            || List.member candidate columnValues
            || List.member candidate boxValues
        )


blockValues : Int -> Int -> Board -> List Int
blockValues rowIndex columnIndex board =
    let
        firstRow =
            (rowIndex // 2) * 2

        firstColumn =
            (columnIndex // 2) * 2
    in
    board
        |> List.drop firstRow
        |> List.take 2
        |> List.concatMap (\row -> row |> List.drop firstColumn |> List.take 2)


setCell : Int -> Int -> Int -> Board -> Board
setCell rowIndex columnIndex value board =
    updateAt rowIndex (updateAt columnIndex (\_ -> value)) board


itemAt : Int -> List item -> Maybe item
itemAt index items =
    List.drop index items |> List.head


updateAt : Int -> (item -> item) -> List item -> List item
updateAt index transform items =
    case items of
        [] ->
            []

        first :: remaining ->
            if index == 0 then
                transform first :: remaining

            else
                first :: updateAt (index - 1) transform remaining
