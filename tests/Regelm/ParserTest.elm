module Regelm.ParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Regelm.Parser as Parser exposing (..)
import Regelm.Matcher as Matcher exposing (char)
import Parser as P


parseTest : Test
parseTest =
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
                            Parser.parse regex
                                |> Expect.equal result
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


{-| ^aa(bbb)+$
-}
testAst6 : Parser.AST
testAst6 =
    [ Start, Matcher <| char 'a', Matcher <| char 'a', Plus (MatchGroup [ Matcher <| char 'b', Matcher <| char 'b', Matcher <| char 'b' ]), End ]


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


matcherParserTest : Test
matcherParserTest =
    describe "matcherParser"
        ([ ( "[]", Nothing )
         , ( "[]]", Nothing )
         , ( "[-]", Just <| Matcher.oneOf [ char '-' ] )
         , ( "[+]", Just <| Matcher.oneOf [ char '+' ] )
         , ( "[\\d]", Just <| Matcher.oneOf [ Matcher.digit ] )
         , ( "[-+]", Just <| Matcher.oneOf [ char '-', char '+' ] )
         , ( "[a-n]", Just <| Matcher.oneOf [ Matcher.range 'a' 'n' ] )
         , ( "[-a-n]", Just <| Matcher.oneOf [ char '-', Matcher.range 'a' 'n' ] )
         , ( "[+a-n-]", Just <| Matcher.oneOf [ char '+', Matcher.range 'a' 'n', char '-' ] )
         ]
            |> List.map
                (\( regex, result ) ->
                    test ("matcherParser " ++ regex) <|
                        \_ ->
                            P.run Parser.matcherParser regex
                                |> Result.toMaybe
                                |> Expect.equal result
                )
        )
