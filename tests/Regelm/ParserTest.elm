module Regelm.ParserTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Regelm.Parser as Parser exposing (..)
import Regelm.Program as Pr
import Regelm.Matcher as Matcher
import Char


preparseTest : Test
preparseTest =
    describe "toAst"
        ([ ( "aabbb", Ok testAst )
         , ( "aa?bbb", Ok testAst2 )
         , ( "aa(bbb)?", Ok testAst3 )
         , ( "aa(bbb)+", Ok testAst4 )
         , ( "aa(bbb)? \\d* \\w+", Ok testAst5 )
         , ( "^aa(bbb)+$", Ok testAst6 )
         , ( "a|b|c", Ok testAst7 )
         , ( "Jan|Feb|Mar", Ok testAst8 )
         ]
            |> List.map
                (\( regex, result ) ->
                    test ("toAst " ++ regex) <|
                        \_ ->
                            Parser.toAst regex
                                |> Expect.equal result
                )
        )


astToProgramTest : Test
astToProgramTest =
    describe "toProgram"
        ([ ( testAst, testProg )
         , ( testAst2, testProg2 )
         , ( testAst3, testProg3 )
         , ( testAst4, testProg4 )
         , ( testAst5, testProg5 )
         , ( testAst6, testProg6 )
         , ( testAst7, testProg7 )
         , ( testAst8, testProg8 )
         ]
            |> List.map
                (\( ast, prog ) ->
                    test ("toProgram " ++ (toString ast)) <|
                        \_ ->
                            Parser.toProgram ast
                                |> Expect.equal prog
                )
        )


{-| aabbb
-}
testAst : Parser.AST
testAst =
    [ Char 'a', Char 'a', Char 'b', Char 'b', Char 'b' ]


testProg : Pr.Program
testProg =
    Array.fromList
        [ Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Matched
        ]


{-| aa?bbb
-}
testAst2 : Parser.AST
testAst2 =
    [ Char 'a', Opt (Char 'a'), Char 'b', Char 'b', Char 'b' ]


testProg2 : Pr.Program
testProg2 =
    Array.fromList
        [ Pr.Match (Matcher.char 'a')
        , Pr.Split 2 3
        , Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Matched
        ]


{-| aa(bbb)?
-}
testAst3 : Parser.AST
testAst3 =
    [ Char 'a', Char 'a', Opt (Group [ Char 'b', Char 'b', Char 'b' ]) ]


testProg3 : Pr.Program
testProg3 =
    Array.fromList
        [ Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'a')
        , Pr.Split 3 6
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Matched
        ]


{-| aa(bbb)+
-}
testAst4 : Parser.AST
testAst4 =
    [ Char 'a', Char 'a', Plus (Group [ Char 'b', Char 'b', Char 'b' ]) ]


testProg4 : Pr.Program
testProg4 =
    Array.fromList
        [ Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Split 2 6
        , Pr.Matched
        ]


{-| aa(bbb)? \d* \w+
-}
testAst5 : Parser.AST
testAst5 =
    [ Char 'a', Char 'a', Opt (Group [ Char 'b', Char 'b', Char 'b' ]), Char ' ', Star Digit, Char ' ', Plus WordChar ]


testProg5 : Pr.Program
testProg5 =
    Array.fromList
        [ Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'a')
        , Pr.Split 3 6
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')

        -- 5
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char ' ')
        , Pr.Split 8 10
        , Pr.Match (Matcher.digit)
        , Pr.Jump 7

        -- 10
        , Pr.Match (Matcher.char ' ')
        , Pr.Match (Matcher.wordChar)
        , Pr.Split 11 13
        , Pr.Matched
        ]


{-| ^aa(bbb)+$
-}
testAst6 : Parser.AST
testAst6 =
    [ Start, Char 'a', Char 'a', Plus (Group [ Char 'b', Char 'b', Char 'b' ]), End ]


testProg6 : Pr.Program
testProg6 =
    Array.fromList
        [ Pr.Start
        , Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Match (Matcher.char 'b')
        , Pr.Split 3 7
        , Pr.End
        , Pr.Matched
        ]


{-| a|b|c
-}
testAst7 : Parser.AST
testAst7 =
    [ Alt (Char 'a') (Alt (Char 'b') (Char 'c')) ]


testProg7 : Pr.Program
testProg7 =
    Array.fromList
        [ Pr.Split 1 3
        , Pr.Match (Matcher.char 'a')
        , Pr.Jump 7
        , Pr.Split 4 6
        , Pr.Match (Matcher.char 'b')
        , Pr.Jump 7
        , Pr.Match (Matcher.char 'c')
        , Pr.Matched
        ]


{-| Jan|Feb|Mar
-}
testAst8 : Parser.AST
testAst8 =
    [ Alt
        (Group [ Char 'J', Char 'a', Char 'n' ])
        (Alt
            (Group [ Char 'F', Char 'e', Char 'b' ])
            (Group [ Char 'M', Char 'a', Char 'r' ])
        )
    ]


testProg8 : Pr.Program
testProg8 =
    Array.fromList
        [ Pr.Split 1 5
        , Pr.Match (Matcher.char 'J')
        , Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'n')
        , Pr.Jump 13
        , Pr.Split 6 10
        , Pr.Match (Matcher.char 'F')
        , Pr.Match (Matcher.char 'e')
        , Pr.Match (Matcher.char 'b')
        , Pr.Jump 13
        , Pr.Match (Matcher.char 'M')
        , Pr.Match (Matcher.char 'a')
        , Pr.Match (Matcher.char 'r')
        , Pr.Matched
        ]
