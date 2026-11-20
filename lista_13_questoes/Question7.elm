module Question7 exposing (..)


type alias Parser a =
    String -> Maybe ( a, String )


type Expr
    = Num Float
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr


lazy : (() -> Parser a) -> Parser a
lazy thunk =
    \input -> thunk () input


succeed : a -> Parser a
succeed value =
    \input -> Just ( value, input )


satisfy : (Char -> Bool) -> Parser Char
satisfy predicate =
    \input ->
        case String.uncons input of
            Just ( character, rest ) ->
                if predicate character then 
                    Just ( character, rest )

                else
                    Nothing

            Nothing ->
                Nothing


symbol : Char -> Parser Char
symbol c =
    satisfy (\character -> character == c)


map : (a -> b) -> Parser a -> Parser b
map f parser =
    \input ->
        case parser input of
            Just ( value, rest ) ->
                Just ( f value, rest )

            Nothing ->
                Nothing


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f pa pb =
    \input ->
        case pa input of
            Just ( a, rest ) ->
                case pb rest of
                    Just ( b, finalRest ) ->
                        Just ( f a b, finalRest )

                    Nothing ->
                        Nothing

            Nothing ->
                Nothing


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f parser =
    \input ->
        case parser input of
            Just ( value, rest ) ->
                f value rest

            Nothing ->
                Nothing


oneOf : List (Parser a) -> Parser a
oneOf parsers =
    \input ->
        case parsers of 
            [] ->
                Nothing

            parser :: remaining ->
                case parser input of
                    Just result ->
                        Just result

                    Nothing ->
                        oneOf remaining input


many : Parser a -> Parser (List a)
many parser =
    \input ->
        case parser input of
            Just ( value, rest ) ->
                if rest == input then
                    Just ( [], input )

                else
                    case many parser rest of
                        Just ( values, finalRest ) ->
                            Just ( value :: values, finalRest )

                        Nothing ->
                            Nothing
            Nothing ->
                Just ( [], input )


number : Parser Float
number =
    map2
    (\firstDigit remainingDigits ->
        String.fromList (firstDigit :: remainingDigits)
            |> String.toFloat
            |> Maybe.withDefault 0
    )
    (satisfy Char.isDigit)
    (many (satisfy Char.isDigit))


spaces : Parser ()
spaces =
    map (\_ -> ()) (many (satisfy (\character -> character == ' ')))


token : Parser a -> Parser a
token parser = 
    map2 (\value _ -> value) parser spaces


expr : Parser Expr
expr = 
    map2 combineTerms term (many termWithOperator)


termWithOperator : Parser ( Char, Expr )
termWithOperator =
    map2 Tuple.pair
        (token (oneOf [ symbol '+', symbol '-' ]))
        term


combineTerms : Expr -> List ( Char, Expr ) -> Expr
combineTerms first rest =
    List.foldl
        (\( operator, value ) accumulated ->
            case operator of
                '+' ->
                    Add accumulated value

                '-' ->
                    Sub accumulated value

                _ ->
                    accumulated
        )
        first
        rest


term : Parser Expr
term = 
    map2 combineFactors factor (many factorWithOperator)


factorWithOperator : Parser ( Char, Expr )
factorWithOperator = 
    map2 Tuple.pair
        (token (oneOf [ symbol '*', symbol '/' ]))
        factor


combineFactors : Expr -> List ( Char, Expr ) -> Expr
combineFactors first rest =
    List.foldl
        (\( operator, value ) accumulated ->
            case operator of
                '*' ->
                    Mul accumulated value

                '/' ->
                    Div accumulated value

                _ ->
                    accumulated
        )
        first
        rest


factor : Parser Expr
factor =
    oneOf
        [ map Num (token number)
        , map2
            (\_ value -> value)
            (token (symbol '('))
            (map2
                (\value _ -> value)
                (lazy (\_ -> expr))
                (token (symbol ')'))
            )
        ]
        

parseExpr : String -> Maybe Expr
parseExpr input =
    case spaces input of
        Just ( _, withoutLeadingSpaces ) ->
            case expr withoutLeadingSpaces of
                Just ( value, rest ) ->
                    if String.isEmpty rest then
                        Just value

                    else
                        Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing