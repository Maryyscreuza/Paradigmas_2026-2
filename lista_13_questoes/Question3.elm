module Question3 exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)


type alias Family =
    { males : Set String
    , females : Set String
    , parent : Dict String (List String)
    }


type Sexo
    = M
    | F


type alias Pessoa =
    { nome : String
    , sexo : Sexo
    , filhos : List String
    }


simpsons : Family
simpsons =
    { males = Set.fromList [ "abe", "homer", "bart" ]
    , females = Set.fromList [ "mona", "marge", "lisa", "maggie" ]
    , parent =
        Dict.fromList
            [ ( "abe", [ "homer" ] )
            , ( "mona", [ "homer" ] )
            , ( "homer", [ "bart", "lisa", "maggie" ] )
            , ( "marge", [ "bart", "lisa", "maggie" ] )
            ]
    }


simpsonsJson : String
simpsonsJson =
    """
    [ {"nome": "abe", "sexo": "M", "filhos": ["homer"]}
    , {"nome": "mona", "sexo": "F", "filhos": ["homer"]}
    , {"nome": "homer", "sexo": "M", "filhos": ["bart", "lisa", "maggie"]}
    , {"nome": "marge", "sexo": "F", "filhos": ["bart", "lisa", "maggie"]}
    , {"nome": "bart", "sexo": "M"}
    , {"nome": "lisa", "sexo": "F", "filhos": []}
    , {"nome": "maggie", "sexo": "F"}
    ]
    """


sexoInvalido : String
sexoInvalido =
    """
    [ {"nome": "homer", "sexo": "X", "filhos": ["bart"]}
    , {"nome": "bart", "sexo": "M"}
    ]
    """


filhosNaoLista : String
filhosNaoLista =
    """
    [ {"nome": "homer", "sexo": "M", "filhos": "bart"}
    , {"nome": "bart", "sexo": "M"}
    ]
    """


filhoDesconhecido : String
filhoDesconhecido =
    """
    [ {"nome": "homer", "sexo": "M", "filhos": ["bart", "lisa"]}
    , {"nome": "bart", "sexo": "M"}
    ]
    """


isErr : Result e a -> Bool
isErr result =
    case result of
        Ok _ ->
            False

        Err _ ->
            True


sexo : Decoder Sexo
sexo =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "M" ->
                        Decode.succeed M

                    "F" ->
                        Decode.succeed F

                    _ ->
                        Decode.fail ("Sexo inválido: " ++ value)
            )


pessoa : Decoder Pessoa
pessoa =
    Decode.map3 Pessoa
        (Decode.field "nome" Decode.string)
        (Decode.field "sexo" sexo)
        filhosDecoder


filhosDecoder : Decoder (List String)
filhosDecoder =
    Decode.keyValuePairs Decode.value
        |> Decode.andThen
            (\fields ->
                case List.filter (\( name, _ ) -> name == "filhos") fields of
                    [] ->
                        Decode.succeed []

                    ( _, value ) :: _ ->
                        case Decode.decodeValue (Decode.list Decode.string) value of
                            Ok children ->
                                Decode.succeed children

                            Err error ->
                                Decode.fail (Decode.errorToString error)
            )


familia : Decoder Family
familia =
    Decode.list pessoa
        |> Decode.andThen buildFamily


buildFamily : List Pessoa -> Decoder Family
buildFamily people =
    let
        knownPeople =
            people
                |> List.map .nome
                |> Set.fromList

        unknownChildren =
            people
                |> List.concatMap .filhos
                |> Set.fromList
                |> Set.diff knownPeople
    in
    if Set.isEmpty unknownChildren then
        Decode.succeed
            { males =
                people
                    |> List.filter (\person -> person.sexo == M)
                    |> List.map .nome
                    |> Set.fromList
            , females =
                people
                    |> List.filter (\person -> person.sexo == F)
                    |> List.map .nome
                    |> Set.fromList
            , parent =
                people
                    |> List.filter (\person -> not (List.isEmpty person.filhos))
                    |> List.map (\person -> ( person.nome, person.filhos ))
                    |> Dict.fromList
            }

    else
        Decode.fail
            ("Filhos sem pessoa correspondente: "
                ++ (unknownChildren |> Set.toList |> String.join ", ")
            )
