module Main exposing (..)

import Dict exposing (Dict)


type Expr
    = Num Float
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr


type alias Env =
    Dict String Float


type Error
    = DomainError
    | NameError String


eval : Env -> Expr -> Result Error Float
eval env expr =
    case expr of
        Num value ->
            Ok value

        Var name ->
            case Dict.get name env of
                Just value ->
                    Ok value

                Nothing ->
                    Err (NameError name)

        Add left right ->
            evalBinary (\leftValue rightValue -> Ok (leftValue + rightValue)) env left right

        Sub left right ->
            evalBinary (\leftValue rightValue -> Ok (leftValue - rightValue)) env left right

        Mul left right ->
            evalBinary (\leftValue rightValue -> Ok (leftValue * rightValue)) env left right

        Div left right ->
            evalBinary
                (\leftValue rightValue ->
                    if rightValue == 0 then
                        Err DomainError

                    else
                        Ok (leftValue / rightValue)
                )
                env
                left
                right


evalBinary : (Float -> Float -> Result Error Float) -> Env -> Expr -> Expr -> Result Error Float
evalBinary operation env left right =
    eval env left
        |> Result.andThen
            (\leftValue ->
                eval env right
                    |> Result.andThen (operation leftValue)
            )
