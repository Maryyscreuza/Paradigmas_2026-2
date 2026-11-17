module Main exposing (..)


lerNota : String -> Result String Float
lerNota texto =
    let
        notaTexto =
            String.trim texto
    in
    String.toFloat notaTexto
        |> Result.fromMaybe ("nota inválida: " ++ notaTexto)
        |> Result.andThen
            (\nota ->
                if nota >= 0 && nota <= 10 then
                    Ok nota

                else
                    Err ("nota fora do intervalo: " ++ notaTexto)
            )


lerTodas : List String -> Result String (List Float)
lerTodas textos =
    List.foldr (\t acc -> Result.map2 (::) (lerNota t) acc) (Ok []) textos


media : List String -> Result String Float
media textos =
    case textos of
        [] ->
            Err "nenhuma nota informada"

        _ ->
            lerTodas textos
                |> Result.map
                    (\notas ->
                        List.sum notas / toFloat (List.length notas)
                    )
