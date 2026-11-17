module Main exposing (..)


type Expr
    = Num Int
    | Soma Expr Expr
    | Mult Expr Expr


avaliar : Expr -> Int
avaliar expr =
    case expr of
        Num n ->
            n

        Soma left right ->
            avaliar left + avaliar right

        Mult left right ->
            avaliar left * avaliar right


tamanho : Expr -> Int
tamanho expr =
    case expr of
        Num _ ->
            1

        Soma left right ->
            1 + tamanho left + tamanho right

        Mult left right ->
            1 + tamanho left + tamanho right

