module Regelm.FuzzTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Test exposing (..)
import Regelm.Fuzz as Fuzz
import Char
import Regelm


patternTest : Test
patternTest =
    describe "pattern"
        ([ "aabbb"
         , "aa?bbb"
         , "aa(bbb)?"
         , "aa(bbb)+"
         , "aa(bbb)? \\d* \\w+"
         , "^aa(bbb)+$"
         , "^((\\d?\\d )?(((Jan)|(Feb)|(Mar)|(Apr)|(May)|(Jun)|(Jul)|(Aug)|(Sep)|(Oct)|(Nov)|(Dec)) ))?\\d?\\d?\\d?\\d$"
         , "^\\d{4}$"
         , "^\\d{2,4}$"
         , "^\\d{,3}$"
         , "^\\d{3,}$"
         ]
            |> List.map
                (\regex ->
                    let
                        compiled =
                            unsafeRegex regex
                    in
                        fuzz (Fuzz.pattern compiled) ("pattern " ++ regex) <|
                            \str ->
                                str
                                    |> Regelm.contains compiled
                                    |> Expect.equal True
                )
        )


unsafeRegex regex =
    case Regelm.regex regex of
        Err err ->
            Debug.crash err

        Ok reg ->
            reg
