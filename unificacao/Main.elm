module Main exposing (..)

import Dict exposing (Dict)


type Term
    = Atom String
    | Var String
    | Compound String (List Term)


type alias Subst =
    Dict String Term


apply : Subst -> Term -> Term
apply subst term =
    case term of
        Atom _ ->
            term

        Var name ->
            case Dict.get name subst of
                Just replacement ->
                    apply subst replacement

                Nothing ->
                    term

        Compound functor arguments ->
            Compound functor (List.map (apply subst) arguments)


unify : Term -> Term -> Maybe Subst
unify a b =
    unifyWith Dict.empty a b

unifyWith : Subst -> Term -> Term -> Maybe Subst
unifyWith subst a b =
    case ( apply subst a, apply subst b ) of
        ( Atom aName, Atom bName ) ->
            if aName == bName then
                Just subst

            else
                Nothing

        ( Var aName, Var bName ) ->
            if aName == bName then
                Just subst

            else
                bind aName (Var bName) subst

        ( Var name, term ) ->
            bind name term subst

        ( term, Var name ) ->
            bind name term subst

        ( Compound aFunctor aArgs, Compound bFunctor bArgs ) ->
            if aFunctor == bFunctor then
                unifyArgs subst aArgs bArgs

            else
                Nothing

        _ ->
            Nothing


unifyArgs : Subst -> List Term -> List Term -> Maybe Subst
unifyArgs subst aArgs bArgs =
    case ( aArgs, bArgs ) of
        ( [], [] ) ->
            Just subst

        ( a :: remainingA, b :: remainingB ) ->
            unifyWith subst a b
                |> Maybe.andThen (\nextSubst -> unifyArgs nextSubst remainingA remainingB)

        _ ->
            Nothing


bind : String -> Term -> Subst -> Maybe Subst
bind name term subst =
    let
        resolvedTerm =
            apply subst term
    in
    if resolvedTerm == Var name then
        Just subst

    else if occurs name resolvedTerm then
        Nothing

    else
        let
            extended =
                Dict.insert name resolvedTerm subst

            normalized =
                Dict.map (\_ value -> apply extended value) extended
        in
        Just normalized


occurs : String -> Term -> Bool
occurs name term =
    case term of
        Atom _ ->
            False

        Var otherName ->
            name == otherName

        Compound _ arguments ->
            List.any (occurs name) arguments
