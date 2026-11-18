module Question4 exposing (..)


type Expr
    = Num Float
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr


eval : Expr -> Float
eval expr =
    case expr of
        Num value ->
            value

        Add left right ->
            eval left + eval right

        Sub left right ->
            eval left - eval right

        Mul left right ->
            eval left * eval right

        Div left right ->
            eval left / eval right
