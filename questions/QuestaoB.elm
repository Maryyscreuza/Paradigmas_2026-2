module Main exposing (..)


produtoDosDigitos : Int -> Int
produtoDosDigitos n =
    produtoDosDigitosAux n 1


produtoDosDigitosAux : Int -> Int -> Int
produtoDosDigitosAux n produto =
    if n < 10 then
        produto * n

    else
        produtoDosDigitosAux (n // 10) (produto * (modBy 10 n))


persistencia : Int -> Int
persistencia n =
    persistenciaAux n 0


persistenciaAux : Int -> Int -> Int
persistenciaAux n quantidade =
    if n < 10 then
        quantidade

    else
        persistenciaAux (produtoDosDigitos n) (quantidade + 1)


contar : { ate : Int, persistencia : Int } -> Int
contar opcoes =
    contarAux opcoes.ate opcoes.persistencia 0


contarAux : Int -> Int -> Int -> Int
contarAux ate persistenciaDesejada acumulado =
    if ate <= 0 then
        acumulado

    else if persistencia ate == persistenciaDesejada then
        contarAux (ate - 1) persistenciaDesejada (acumulado + 1)

    else
        contarAux (ate - 1) persistenciaDesejada acumulado
