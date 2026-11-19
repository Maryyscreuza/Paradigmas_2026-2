module Main exposing (..)


type Item
    = Arquivo String Int
    | Pasta String (List Item)


exemplo : Item
exemplo =
    Pasta "home"
        [ Arquivo "notas.txt" 4
        , Pasta "fotos"
            [ Arquivo "praia.jpg" 900
            , Arquivo "serra.jpg" 750
            ]
        , Pasta "vazia" []
        , Arquivo "video.mp4" 900
        ]


tamanhoTotal : Item -> Int
tamanhoTotal item =
    case item of
        Arquivo _ tamanho ->
            tamanho

        Pasta _ itens ->
            List.foldl (+) 0 (List.map tamanhoTotal itens)


maiorArquivo : Item -> Maybe ( String, Int )
maiorArquivo item =
    case item of
        Arquivo nome tamanho ->
            Just ( nome, tamanho )

        Pasta _ itens ->
            maiorNosItens itens Nothing


maiorNosItens : List Item -> Maybe ( String, Int ) -> Maybe ( String, Int )
maiorNosItens itens atual =
    case itens of
        [] ->
            atual

        item :: restantes ->
            maiorNosItens restantes (maiorEntre atual (maiorArquivo item))


maiorEntre : Maybe ( String, Int ) -> Maybe ( String, Int ) -> Maybe ( String, Int )
maiorEntre atual candidato =
    case ( atual, candidato ) of
        ( Nothing, Just novo ) ->
            Just novo

        ( Just ( nomeAtual, tamanhoAtual ), Just ( _, tamanhoNovo ) ) ->
            if tamanhoNovo > tamanhoAtual then
                candidato

            else
                atual

        _ ->
            atual
