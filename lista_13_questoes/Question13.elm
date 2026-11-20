module Main exposing (..)


type Stream a
    = Cons a (() -> Stream a)


isEven : Int -> Bool
isEven n =
    modBy 2 n == 0


head : Stream a -> a
head stream =
    case stream of
        Cons value _ ->
            value


tail : Stream a -> Stream a
tail stream =
    case stream of
        Cons _ rest ->
            rest ()


take : Int -> Stream a -> List a
take n stream =
    if n <= 0 then
        []

    else
        head stream :: take (n - 1) (tail stream)


iterate : (a -> a) -> a -> Stream a
iterate f x =
    Cons x (\_ -> iterate f (f x))


map : (a -> b) -> Stream a -> Stream b
map f stream =
    case stream of
        Cons value rest ->
            Cons (f value) (\_ -> map f (rest ()))


filter : (a -> Bool) -> Stream a -> Stream a
filter p stream =
    case stream of
        Cons value rest ->
            if p value then
                Cons value (\_ -> filter p (rest ()))

            else
                filter p (rest ())


zipWith : (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys =
    case ( xs, ys ) of
        ( Cons x restX, Cons y restY ) ->
            Cons (f x y) (\_ -> zipWith f (restX ()) (restY ()))


from : Int -> Stream Int
from n =
    Cons n (\_ -> from (n + 1))


nats : Stream Int
nats =
    from 0


fibs : Stream Int
fibs =
    iterate (\( a, b ) -> ( b, a + b )) ( 0, 1 )
        |> map Tuple.first


primes : Stream Int
primes =
    sieve (from 2)


sieve : Stream Int -> Stream Int
sieve stream =
    case stream of
        Cons prime rest ->
            Cons prime
                (\_ ->
                    sieve
                        (filter (\candidate -> modBy prime candidate /= 0) (rest ()))
                )
