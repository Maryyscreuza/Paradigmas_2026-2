module Main exposing (..)

import Json.Decode as D exposing (Decoder)


type Estado
    = Pendente
    | Feita String


type alias Tarefa =
    { id : Int
    , descricao : String
    , estado : Estado
    }


decodeEstado : Decoder Estado
decodeEstado =
    D.field "tipo" D.string
        |> D.andThen
            (\tipo ->
                case tipo of
                    "pendente" ->
                        D.succeed Pendente

                    "feita" ->
                        D.map Feita (D.field "em" D.string)

                    _ ->
                        D.fail ("tipo de estado desconhecido: " ++ tipo)
            )


decodeTarefa : Decoder Tarefa
decodeTarefa =
    D.map3 Tarefa
        (D.field "id" D.int)
        (D.field "descricao" D.string)
        (D.field "estado" decodeEstado)
