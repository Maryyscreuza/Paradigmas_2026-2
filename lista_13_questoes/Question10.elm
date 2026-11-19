module Main exposing (..)


type alias Board =
    List (List Int)


puzzle : Board
puzzle =
    [ [ 1, 0, 3, 4 ]
    , [ 0, 4, 1, 0 ]
    , [ 2, 1, 0, 3 ]
    , [ 4, 0, 2, 0 ]
    ]


solveBruteForce : Board -> List Board
solveBruteForce board =
    generateBoards board
        |> List.filter isValidSudoku


generateBoards : Board -> List Board
generateBoards board =
    case board of
        [] ->
            [ [] ]

        row :: remainingRows ->
            let
                rowChoices =
                    generateRow row

                remainingBoards =
                    generateBoards remainingRows
            in
            List.concatMap
                (\rowChoice ->
                    List.map (\remainingBoard -> rowChoice :: remainingBoard) remainingBoards
                )
                rowChoices


generateRow : List Int -> List (List Int)
generateRow values =
    case values of
        [] ->
            [ [] ]

        0 :: remainingValues ->
            List.concatMap
                (\value ->
                    List.map (\row -> value :: row) (generateRow remainingValues)
                )
                [ 1, 2, 3, 4 ]

        value :: remainingValues ->
            List.map (\row -> value :: row) (generateRow remainingValues)


isValidSudoku : Board -> Bool
isValidSudoku board =
    case board of
        [ [ r1c1, r1c2, r1c3, r1c4 ]
            , [ r2c1, r2c2, r2c3, r2c4 ]
            , [ r3c1, r3c2, r3c3, r3c4 ]
            , [ r4c1, r4c2, r4c3, r4c4 ]
            ] ->
            let
                columns =
                    [ [ r1c1, r2c1, r3c1, r4c1 ]
                    , [ r1c2, r2c2, r3c2, r4c2 ]
                    , [ r1c3, r2c3, r3c3, r4c3 ]
                    , [ r1c4, r2c4, r3c4, r4c4 ]
                    ]

                blocks =
                    [ [ r1c1, r1c2, r2c1, r2c2 ]
                    , [ r1c3, r1c4, r2c3, r2c4 ]
                    , [ r3c1, r3c2, r4c1, r4c2 ]
                    , [ r3c3, r3c4, r4c3, r4c4 ]
                    ]
            in
            List.all isValidGroup (board ++ columns ++ blocks)

        _ ->
            False


isValidGroup : List Int -> Bool
isValidGroup values =
    List.sort values == [ 1, 2, 3, 4 ]
