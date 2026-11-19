module Main exposing (..)


type alias Board =
    List (List Int)


solved : Board
solved =
    [ [ 1, 2, 3, 4 ]
    , [ 3, 4, 1, 2 ]
    , [ 2, 1, 4, 3 ]
    , [ 4, 3, 2, 1 ]
    ]


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
