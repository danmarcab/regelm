module Regelm.Fuzz exposing (pattern)

{-| Generate strings that follow a pattern and use them in your Fuzz tests.

@docs pattern

-}

import Regelm exposing (Regex)
import Regelm.Random
import Fuzz exposing (Fuzzer)
import Shrink


{-| Get a `Fuzzer String` from a `Regex`. This is useful when you want to create random
strings that match a pattern in your tests.
-}
pattern : Regex -> Fuzzer String
pattern regex =
    Fuzz.custom
        (Regelm.Random.pattern regex)
        (Shrink.keepIf (Regelm.contains regex) Shrink.string)
