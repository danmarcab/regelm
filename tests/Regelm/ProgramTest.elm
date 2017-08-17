module Regelm.ProgramTest exposing (..)

import Array.Hamt as Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Regelm.Program as Program exposing (..)
import Regelm.Matcher as Matcher


containsTest : Test
containsTest =
    describe "contains"
        ([ ( "aabbb", testRegex, [ "aabbb", "aaabbb" ], [ "aacbb" ] )
         , ( "aa(bbb)? \\d? \\w+", testRegex2, [ "aa  k", "aabbb 7 sdfhiu" ], [ "aabb" ] )
         , ( "^aabb$", testRegex3, [ "aabb" ], [ "aaabbb" ] )
         ]
            |> List.map
                (\( regex, prog, accepted, rejected ) ->
                    (List.map
                        (\str ->
                            test (str ++ " contains " ++ regex) <|
                                \_ ->
                                    Program.contains prog str
                                        |> Expect.equal True
                        )
                        accepted
                    )
                        ++ (List.map
                                (\str ->
                                    test (str ++ " doesn't contains " ++ regex) <|
                                        \_ ->
                                            Program.contains prog str
                                                |> Expect.equal False
                                )
                                rejected
                           )
                )
            |> List.concat
        )


{-| aabbb
-}
testRegex : Program.Program
testRegex =
    Array.fromList
        [ Match (Matcher.char 'a')
        , Match (Matcher.char 'a')
        , Match (Matcher.char 'b')
        , Match (Matcher.char 'b')
        , Match (Matcher.char 'b')
        , Matched
        ]


{-| aa(bbb)? \d? \w+
-}
testRegex2 : Program.Program
testRegex2 =
    Array.fromList
        [ Match (Matcher.char 'a')
        , Match (Matcher.char 'a')
        , Split [ 3, 6 ]
        , Match (Matcher.char 'b')
        , Match (Matcher.char 'b')

        --        5
        , Match (Matcher.char 'b')
        , Match (Matcher.char ' ')
        , Split [ 8, 9 ]
        , Match (Matcher.range '0' '9')
        , Match (Matcher.char ' ')

        --        10
        , Match (Matcher.oneOf [ (Matcher.range 'a' 'z'), (Matcher.range 'A' 'Z') ])
        , Split [ 10, 12 ]
        , Matched
        ]


{-| ^aabb$
-}
testRegex3 : Program.Program
testRegex3 =
    Array.fromList
        [ Start
        , Match (Matcher.char 'a')
        , Match (Matcher.char 'a')
        , Match (Matcher.char 'b')
        , Match (Matcher.char 'b')
        , End
        , Matched
        ]
