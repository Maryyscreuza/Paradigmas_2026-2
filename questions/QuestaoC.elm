module Main exposing (..)


type Msg
    = Incrementa
    | Decrementa
    | Zera


atualizar : Msg -> Int -> Int
atualizar msg valor =
    case msg of
        Incrementa ->
            valor + 1

        Decrementa ->
            max 0 (valor - 1)

        Zera ->
            0


type alias Historico m =
    { atual : m
    , passado : List m
    }


type Comando msg
    = Acao msg
    | Desfazer


init : Historico Int
init =
    { atual = 0, passado = [] }


comHistorico : (msg -> m -> m) -> Comando msg -> Historico m -> Historico m
comHistorico atualizarBase comando historico =
    case comando of
        Acao msg ->
            let
                novoAtual =
                    atualizarBase msg historico.atual
            in
            if novoAtual == historico.atual then
                historico

            else
                { atual = novoAtual
                , passado = historico.atual :: historico.passado
                }

        Desfazer ->
            case historico.passado of
                [] ->
                    historico

                anterior :: restante ->
                    { atual = anterior
                    , passado = restante
                    }


update : Comando Msg -> Historico Int -> Historico Int
update =
    comHistorico atualizar
