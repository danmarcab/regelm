module Regelm.Matcher exposing (Matcher, matches, oneOf, range, char, digit, nonDigit, wordChar, nonWordChar, fuzzer)

import Char
import Fuzz exposing (Fuzzer)


type Matcher
    = Only Char
    | Range Char Char
    | Enum (List Char)
    | OneOf (List Matcher)
    | Not Matcher
    | Any


matches : Char -> Matcher -> Bool
matches theChar matcher =
    case matcher of
        Only char ->
            char == theChar

        Range start end ->
            (start <= theChar) && (theChar <= end)

        Enum chars ->
            List.member theChar chars

        OneOf matchers ->
            List.any (matches theChar) matchers

        Not matcher ->
            not (matches theChar matcher)

        Any ->
            True


oneOf : List Matcher -> Matcher
oneOf =
    OneOf


range : Char -> Char -> Matcher
range =
    Range


char : Char -> Matcher
char char =
    Only char


digit : Matcher
digit =
    Range '0' '9'


nonDigit : Matcher
nonDigit =
    Not digit


wordChar : Matcher
wordChar =
    OneOf [ Range 'A' 'Z', Range 'a' 'z', Only '_' ]


nonWordChar : Matcher
nonWordChar =
    Not wordChar


fuzzer : Matcher -> Fuzzer Char
fuzzer matcher =
    case matcher of
        Only c ->
            Fuzz.constant c

        Range a b ->
            Fuzz.map Char.fromCode <| Fuzz.intRange (Char.toCode a) (Char.toCode b)

        Enum chars ->
            List.map Fuzz.constant chars
                |> Fuzz.oneOf

        OneOf matchers ->
            List.map fuzzer matchers
                |> Fuzz.oneOf

        Not invMatcher ->
            let
                bruteForce char =
                    if not <| matches char invMatcher then
                        char
                    else
                        bruteForce <| Char.fromCode (Char.toCode char + 1)
            in
                Fuzz.conditional
                    { retries = 100
                    , fallback = bruteForce
                    , condition = \char -> not <| matches char invMatcher
                    }
                    Fuzz.char

        Any ->
            Fuzz.char
