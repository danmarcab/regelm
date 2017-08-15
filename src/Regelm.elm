module Regelm exposing (Regex, contains, regex)

{-| This library lets you have a retroactive model. This means you can undo / redo
changes in your model. Retroactive means you can have different branches of future (redo).

The library can be use as a [`Program`](#using-program), so all your model will be retroactive, or
[`not using Program`](#not-using-program), where you decide what parts of your model you want to make retroactive.


# Parsing a regular expression

@docs Regex, regex


# Matching a string with a regular expression

@docs contains

-}

import Regelm.Program as Program
import Regelm.Parser as Parser


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
    Parser.parse str |> Result.map Program.Regex


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
