module Palindrome exposing (isPalindrome)


isPalindrome : Int -> Bool
isPalindrome number =
    let
        digits =
            String.fromInt number
    in
    digits == String.reverse digits