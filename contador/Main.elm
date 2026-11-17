module Main exposing (..)


type alias Model =
    { valor : Int
    , historico : List Int
    }


type Msg
    = Incrementa
    | Decrementa
    | Zera
    | Desfazer


init : Model
init =
    { valor = 0, historico = [] }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Incrementa ->
            { model
                | valor = model.valor + 1
                , historico = model.valor :: model.historico
            }

        Decrementa ->
            if model.valor == 0 then
                model

            else
                { model
                    | valor = model.valor - 1
                    , historico = model.valor :: model.historico
                }

        Zera ->
            if model.valor == 0 then
                model

            else
                { model
                    | valor = 0
                    , historico = model.valor :: model.historico
                }

        Desfazer ->
            case model.historico of
                [] ->
                    model

                anterior :: restante ->
                    { model | valor = anterior, historico = restante }
