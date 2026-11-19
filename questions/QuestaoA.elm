module Main exposing (..)


type Erro
    = FechouSemAbrir Int
    | FicouAberto Int


profundidade : String -> Result Erro Int
profundidade texto =
    verificar (String.toList texto) 1 0 0


verificar : List Char -> Int -> Int -> Int -> Result Erro Int
verificar caracteres posicao abertas maxima =
    case caracteres of
        [] ->
            if abertas == 0 then
                Ok maxima

            else
                Err (FicouAberto abertas)

        caracter :: restantes ->
            if caracter == '[' then
                let
                    novasAbertas =
                        abertas + 1

                    novaMaxima =
                        max maxima novasAbertas
                in
                verificar restantes (posicao + 1) novasAbertas novaMaxima

            else if caracter == ']' then
                if abertas == 0 then
                    Err (FechouSemAbrir posicao)

                else
                    verificar restantes (posicao + 1) (abertas - 1) maxima

            else
                verificar restantes (posicao + 1) abertas maxima
