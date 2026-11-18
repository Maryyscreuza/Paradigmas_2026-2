module Question2 exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


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


ancestors : Family -> String -> Set String
ancestors family person =
    collectAncestors family (Set.singleton person) (parentsOf family person)
        |> Set.remove person


parentsOf : Family -> String -> Set String
parentsOf family child =
    family.parent
        |> Dict.toList
        |> List.filter (\( parent, children ) -> List.member child children)
        |> List.map Tuple.first
        |> Set.fromList


collectAncestors : Family -> Set String -> Set String -> Set String
collectAncestors family visited frontier =
    let
        newAncestors =
            Set.diff frontier visited

        updatedVisited =
            Set.union visited newAncestors

        nextFrontier =
            Set.foldl
                (\ancestor accumulated ->
                    Set.union accumulated (parentsOf family ancestor)
                )
                Set.empty
                newAncestors
    in
    if Set.isEmpty newAncestors then
        visited

    else
        collectAncestors family updatedVisited nextFrontier
