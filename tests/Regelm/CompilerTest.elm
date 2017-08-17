module Regelm.CompilerTest exposing (..)

import Array.Hamt as Array exposing (Array)
import Expect exposing (Expectation)
import Test exposing (..)
import Regelm.Parser as Parser exposing (..)
import Regelm.Compiler as Compiler exposing (..)
import Regelm.Program as Pr
import Regelm.Matcher as Matcher exposing (char)
import Char


compileTest : Test
compileTest =
    describe "compile"
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
                    test ("compile " ++ (toString ast)) <|
                        \_ ->
                            Compiler.compile ast
                                |> Expect.equal prog
                )
        )


{-| aabbb
-}
testAst : Parser.AST
testAst =
    [ Matcher <| char 'a'
    , Matcher <| char 'a'
    , Matcher <| char 'b'
    , Matcher <| char 'b'
    , Matcher <| char 'b'
    ]


testProg : Pr.Program
testProg =
    Array.fromList
        [ Pr.Match (char 'a')
        , Pr.Match (char 'a')
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Matched
        ]


{-| aa?bbb
-}
testAst2 : Parser.AST
testAst2 =
    [ Matcher <| char 'a'
    , Opt (Matcher <| char 'a')
    , Matcher <| char 'b'
    , Matcher <| char 'b'
    , Matcher <| char 'b'
    ]


testProg2 : Pr.Program
testProg2 =
    Array.fromList
        [ Pr.Match (char 'a')
        , Pr.Split [ 2, 3 ]
        , Pr.Match (char 'a')
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Matched
        ]


{-| aa(bbb)?
-}
testAst3 : Parser.AST
testAst3 =
    [ Matcher <| char 'a'
    , Matcher <| char 'a'
    , Opt
        (MatchGroup
            [ Matcher <| char 'b'
            , Matcher <| char 'b'
            , Matcher <| char 'b'
            ]
        )
    ]


testProg3 : Pr.Program
testProg3 =
    Array.fromList
        [ Pr.Match (char 'a')
        , Pr.Match (char 'a')
        , Pr.Split [ 3, 6 ]
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Matched
        ]


{-| aa(bbb)+
-}
testAst4 : Parser.AST
testAst4 =
    [ Matcher <| char 'a'
    , Matcher <| char 'a'
    , Plus
        (MatchGroup
            [ Matcher <| char 'b'
            , Matcher <| char 'b'
            , Matcher <| char 'b'
            ]
        )
    ]


testProg4 : Pr.Program
testProg4 =
    Array.fromList
        [ Pr.Match (char 'a')
        , Pr.Match (char 'a')
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Split [ 2, 6 ]
        , Pr.Matched
        ]


{-| aa(bbb)? \d* \w+
-}
testAst5 : Parser.AST
testAst5 =
    [ Matcher <| char 'a'
    , Matcher <| char 'a'
    , Opt
        (MatchGroup
            [ Matcher <| char 'b'
            , Matcher <| char 'b'
            , Matcher <| char 'b'
            ]
        )
    , Matcher <| char ' '
    , Star (Matcher Matcher.digit)
    , Matcher <| char ' '
    , Plus (Matcher Matcher.wordChar)
    ]


testProg5 : Pr.Program
testProg5 =
    Array.fromList
        [ Pr.Match (char 'a')
        , Pr.Match (char 'a')
        , Pr.Split [ 3, 6 ]
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')

        -- 5
        , Pr.Match (char 'b')
        , Pr.Match (char ' ')
        , Pr.Split [ 8, 10 ]
        , Pr.Match (Matcher.digit)
        , Pr.Jump 7

        -- 10
        , Pr.Match (char ' ')
        , Pr.Match (Matcher.wordChar)
        , Pr.Split [ 11, 13 ]
        , Pr.Matched
        ]


{-| ^aa(bbb)+$
-}
testAst6 : Parser.AST
testAst6 =
    [ Start, Matcher <| char 'a', Matcher <| char 'a', Plus (MatchGroup [ Matcher <| char 'b', Matcher <| char 'b', Matcher <| char 'b' ]), End ]


testProg6 : Pr.Program
testProg6 =
    Array.fromList
        [ Pr.Start
        , Pr.Match (char 'a')
        , Pr.Match (char 'a')
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Match (char 'b')
        , Pr.Split [ 3, 7 ]
        , Pr.End
        , Pr.Matched
        ]


{-| a|b|c
-}
testAst7 : Parser.AST
testAst7 =
    [ Alt
        [ Matcher <| char 'a'
        , Matcher <| char 'b'
        , Matcher <| char 'c'
        ]
    ]


testProg7 : Pr.Program
testProg7 =
    Array.fromList
        [ Pr.Split [ 1, 3, 5 ]
        , Pr.Match (char 'a')
        , Pr.Jump 7
        , Pr.Match (char 'b')
        , Pr.Jump 7
        , Pr.Match (char 'c')
        , Pr.Jump 7
        , Pr.Matched
        ]


{-| Jan|Feb|Mar
-}
testAst8 : Parser.AST
testAst8 =
    [ Alt
        [ Group [ Matcher <| char 'J', Matcher <| char 'a', Matcher <| char 'n' ]
        , Group [ Matcher <| char 'F', Matcher <| char 'e', Matcher <| char 'b' ]
        , Group [ Matcher <| char 'M', Matcher <| char 'a', Matcher <| char 'r' ]
        ]
    ]


testProg8 : Pr.Program
testProg8 =
    Array.fromList
        [ Pr.Split [ 1, 5, 9 ]
        , Pr.Match (char 'J')
        , Pr.Match (char 'a')
        , Pr.Match (char 'n')
        , Pr.Jump 13
        , Pr.Match (char 'F')
        , Pr.Match (char 'e')
        , Pr.Match (char 'b')
        , Pr.Jump 13
        , Pr.Match (char 'M')
        , Pr.Match (char 'a')
        , Pr.Match (char 'r')
        , Pr.Jump 13
        , Pr.Matched
        ]
