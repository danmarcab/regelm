module Regelm exposing (Regex, contains, regex)

{-| Use regular expressions in Elm.

At the moment only checking if a regular expression matches a pattern is supported
([`contains`](#contains)). Capturing matched data is next on the roadmap.

You can also see `Regelm.Random` and `Regelm.Fuzz` for random generators of strings
that match a regular expression.


# Parsing a regular expression

@docs Regex, regex


# Matching a string with a regular expression

@docs contains

-}

import Regelm.Program as Program
import Regelm.Parser as Parser
import Regelm.Compiler as Compiler


{-| Holds a regular expression. It is a opaque type, to create `Regex` use [`regex`](#regex)
-}
type alias Regex =
    Program.Regex


{-| Get a [`regex`](#regex) from a `String`. Can fail so it will be wrapped in a `Result`.

    > regex "[0-9"
    Err "Invalid Regex"

    > regex "[0-9]"
    Ok Regex

-}
regex : String -> Result String Regex
regex str =
    Parser.parse str
        |> Result.map Compiler.compile
        |> Result.map Program.Regex


{-| Match a string with a regular expression

    -- exp : Regex
    -- created from "[0-9]+"

    > contains exp "abc"
    True

    > contains exp "123"
    False

-}
contains : Regex -> String -> Bool
contains (Program.Regex prog) str =
    Program.contains prog str
