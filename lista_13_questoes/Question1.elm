module Main exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Son =
    String


type alias Father =
    String


type alias Family =
    { males : Set String
    , females : Set String
    , parent : Dict String (List String)
    }


simpsons : Family
simpsons =
    { males = Set.fromList [ "abe", "homer", "bart" ]
    , females = Set.fromList [ "mona", "marge", "lisa", "maggie" ]
    , parent =
        Dict.fromList
            [ ( "abe", [ "homer" ] )
            , ( "mona", [ "homer" ] )
            , ( "homer", [ "bart", "lisa", "maggie" ] )
            , ( "marge", [ "bart", "lisa", "maggie" ] )
            ]
    }


isFatherOf : Family -> Son -> Father -> Bool
isFatherOf family son father =
    Set.member father family.males
        && (Dict.get father family.parent
                |> Maybe.withDefault []
                |> List.member son
           )
