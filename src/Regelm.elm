module Regelm exposing (Regex, contains, regex)

import Regelm.Program as Program
import Regelm.Parser as Parser


type alias Regex =
    Program.Regex


regex : String -> Result String Regex
regex str =
    Parser.parse str |> Result.map Program.Regex


contains : Regex -> String -> Bool
contains (Program.Regex prog) str =
    Program.contains prog str
