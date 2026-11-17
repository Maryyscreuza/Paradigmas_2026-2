module Primes exposing (primes)


primes : Int -> List Int
primes count =
    findPrimes count 2 []


findPrimes : Int -> Int -> List Int -> List Int
findPrimes remaining candidate found =
    if remaining <= 0 then
        List.reverse found

    else if isPrime candidate then
        findPrimes (remaining - 1) (candidate + 1) (candidate :: found)

    else
        findPrimes remaining (candidate + 1) found


isPrime : Int -> Bool
isPrime candidate =
    hasDivisor candidate 2


hasDivisor : Int -> Int -> Bool
hasDivisor candidate divisor =
    if divisor > candidate // divisor then
        True

    else if modBy divisor candidate == 0 then
        False

    else
        hasDivisor candidate (divisor + 1)