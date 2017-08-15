module Regelm.Matcher
    exposing
        ( Matcher
        , matches
        , oneOf
        , inverse
        , range
        , char
        , digit
        , nonDigit
        , wordChar
        , nonWordChar
        , whiteSpace
        , nonWhiteSpace
        , generator
        )

import Char
import Random.Pcg as Random exposing (Seed, Generator)


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


inverse : Matcher -> Matcher
inverse =
    Not


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
    OneOf [ Range 'A' 'Z', Range 'a' 'z', Range '0' '9', Only '_' ]


nonWordChar : Matcher
nonWordChar =
    Not wordChar


whiteSpace : Matcher
whiteSpace =
    Enum [ ' ', '\n', '\t', '\x0C', '\x0D', '\x0B' ]


nonWhiteSpace : Matcher
nonWhiteSpace =
    Not whiteSpace


generator : Matcher -> Generator Char
generator matcher =
    case matcher of
        Only c ->
            Random.constant c

        Range a b ->
            Random.map Char.fromCode <| Random.int (Char.toCode a) (Char.toCode b)

        Enum chars ->
            List.map Random.constant chars
                |> Random.choices

        OneOf matchers ->
            List.map generator matchers
                |> Random.choices

        Not invMatcher ->
            Random.filter (\char -> not <| matches char invMatcher) charGenerator

        Any ->
            charGenerator


charGenerator : Generator Char
charGenerator =
    Random.frequency
        [ ( 8, Random.map Char.fromCode (Random.int 32 126) )
        , ( 1, Random.map Char.fromCode (Random.int Random.minInt Random.maxInt) )
        , ( 1, Random.map Char.fromCode (Random.int Random.minInt Random.maxInt) )
        ]
