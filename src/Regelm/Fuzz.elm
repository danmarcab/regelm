module Regelm.Fuzz exposing (pattern)

{-| Generate strings that follow a pattern and use them in your Fuzz tests.
-}

import Array
import Char
import Fuzz exposing (Fuzzer)
import Random exposing (Seed)
import Regelm.Program as Program exposing (Regex(..), Inst(..))
import Regelm.Matcher as Matcher


{-| Get a `Fuzzer String` from a `Regex`. This is useful when you want to create random
strings that match a pattern.
-}
pattern : Regex -> Fuzzer String
pattern (Regex program) =
    Fuzz.int
        |> Fuzz.andThen
            (\i ->
                runRandomly (Random.initialSeed i) program
                    |> List.map Matcher.fuzzer
                    |> sequence
                    |> Fuzz.map String.fromList
            )


runRandomly : Seed -> Program.Program -> List Matcher.Matcher
runRandomly seed prog =
    execInst 0 prog seed []


execInst : Int -> Program.Program -> Seed -> List Matcher.Matcher -> List Matcher.Matcher
execInst idx prog seed matchers =
    case Program.getInst idx prog of
        Just (Match matcher) ->
            execInst (idx + 1) prog seed (matcher :: matchers)

        Just (Jump a) ->
            execInst a prog seed matchers

        Just (Split a b) ->
            let
                ( val, newSeed ) =
                    Random.step Random.bool seed
            in
                execInst
                    (if val then
                        a
                     else
                        b
                    )
                    prog
                    newSeed
                    matchers

        Just Start ->
            execInst (idx + 1) prog seed matchers

        Just End ->
            execInst (idx + 1) prog seed matchers

        Just Matched ->
            matchers

        Nothing ->
            matchers


{-| from <https://github.com/NoRedInk/json-elm-schema/blob/2.2.0/src/Fuzz/Extra.elm>
-}
sequence : List (Fuzzer a) -> Fuzzer (List a)
sequence fuzzers =
    List.foldl
        (\fuzzer listFuzzer ->
            Fuzz.constant (::)
                |> Fuzz.andMap fuzzer
                |> Fuzz.andMap listFuzzer
        )
        (Fuzz.constant [])
        fuzzers
