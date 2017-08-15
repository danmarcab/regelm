module Regelm.Random exposing (pattern)

{-| Generate strings that follow a pattern.

Note that all random types show here are from
[`mgold/elm-random-pcg`](`http://package.elm-lang.org/packages/mgold/elm-random-pcg`) so you will need to install
that package instead of using `Random` from core.

@docs pattern

-}

import Array
import Char
import Random.Pcg as Random exposing (Seed, Generator)
import Regelm.Program as Program exposing (Regex(..), Inst(..))
import Regelm.Matcher as Matcher


{-| Get a `Generator String` from a `Regex`. This is useful when you want to create random
strings that match a pattern.
-}
pattern : Regex -> Generator String
pattern (Regex program) =
    Random.int Random.minInt Random.maxInt
        |> Random.andThen
            (\i ->
                runRandomly (Random.initialSeed i) program
                    |> List.map Matcher.generator
                    |> sequence
                    |> Random.map String.fromList
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


sequence : List (Random.Generator a) -> Random.Generator (List a)
sequence generators =
    List.foldl
        (\generator genList ->
            Random.constant (::)
                |> Random.andMap generator
                |> Random.andMap genList
        )
        (Random.constant [])
        generators
