module RegelmTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Regelm


containsTest : Test
containsTest =
    describe "contains"
        ([ ( "aabbb", [ "aabbb", "dsyuaabbbbfg" ], [ "aabb", "fsdf" ] )
         , ( "aa?bbb", [ "abbb", "aabbb", "svsfabbbbg", "sdfaabbbdsf" ], [ "abb", "aabbcvx" ] )
         , ( "aa(bbb)?", [ "aa", "aabbb", "svsfaabbbbg", "sdfaabdsf" ], [ "abb", "babbbcvx" ] )
         , ( "aa(bbb)+", [ "aabbb", "aabbbbbb", "svsfaabbbbbg", "sdfaabbbbbbbbbbbbbbbdsf" ], [ "aa", "abb", "babbbcvx" ] )
         , ( "\\d* \\w+", [ " g", "5 h", "465656 cdsvsd", "fsfds6 gt hjk" ], [ "5f", " ", "5 " ] )
         , ( "^aa(bbb)*$", [ "aa", "aabbb", "aabbbbbbbbb" ], [ "faa", "aab", "aabbbb" ] )
         , ( "^((\\d?\\d )?(((Jan)|(Feb)|(Mar)|(Apr)|(May)|(Jun)|(Jul)|(Aug)|(Sep)|(Oct)|(Nov)|(Dec)) ))?\\d?\\d?\\d?\\d$"
           , [ "1 Mar 2013", "21 Sep 86", "Dec 99", "2013" ]
           , [ "222 Jun 99", "23 Mar", "Mar" ]
           )
         , ( "[abcg-i]", [ "a", "b", "c", "g", "h", "i" ], [ "j", "d", "6" ] )
         , ( "[g-iu-y]", [ "g", "h", "i", "u", "w", "y" ], [ "j", "a", "6", "z", "Z" ] )
         , ( "^\\d{3}$", [ "124", "345" ], [ "23", "2342", "6", "bvdfz" ] )
         , ( "^\\d{2,4}$", [ "24", "345", "3455" ], [ "3", "23424", "2342346", "bvdfz" ] )
         , ( "^\\d{,4}$", [ "", "4", "24", "345", "3455" ], [ "23424", "2342346", "bvdfz" ] )
         , ( "^\\d{3,}$", [ "345", "3455", "4534343634" ], [ "", "2", "24", "bvdfz" ] )
         ]
            |> List.map
                (\( regex, accepted, rejected ) ->
                    (List.map
                        (\str ->
                            test (str ++ " contains " ++ regex) <|
                                \_ ->
                                    Regelm.contains (unsafeRegex regex) str
                                        |> Expect.equal True
                        )
                        accepted
                    )
                        ++ (List.map
                                (\str ->
                                    test (str ++ " doesn't contains " ++ regex) <|
                                        \_ ->
                                            Regelm.contains (unsafeRegex regex) str
                                                |> Expect.equal False
                                )
                                rejected
                           )
                )
            |> List.concat
        )


unsafeRegex regex =
    case Regelm.regex regex of
        Err err ->
            Debug.crash err

        Ok reg ->
            reg
