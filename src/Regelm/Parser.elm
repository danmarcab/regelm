module Regelm.Parser exposing (..)

import Array
import Char
import Parser exposing (Parser, (|.), (|=))
import Regelm.Program as Program
import Regelm.Matcher as Matcher


type alias AST =
    List Node


type Node
    = Matcher Matcher.Matcher
    | Start
    | End
    | Opt Node
    | Plus Node
    | Star Node
    | RepeatExactly Int Node
    | RepeatBetween Int Int Node
    | RepeatAtLeast Int Node
    | RepeatAtMost Int Node
    | Alt Node Node
    | Group AST
    | MatchGroup AST


parse : String -> Result String Program.Program
parse srt =
    toAst srt
        |> Result.map toProgram



-- Parsing AST from string --


toAst : String -> Result String AST
toAst str =
    Parser.run astParser str
        |> Result.mapError toString
        |> Debug.log "ast"


astParser : Parser AST
astParser =
    Parser.lazy (\_ -> Parser.repeat Parser.zeroOrMore (Parser.lazy (\_ -> nodeParser)))
        |> Parser.andThen
            (\ast ->
                Parser.oneOf
                    [ Parser.succeed
                        (\other_ast ->
                            let
                                astoToNode a =
                                    case a of
                                        [ n ] ->
                                            n

                                        _ ->
                                            Group a
                            in
                                [ Alt (astoToNode ast) (astoToNode other_ast) ]
                        )
                        |. Parser.keyword "|"
                        |= Parser.lazy (\_ -> astParser)
                    , Parser.succeed ast
                    ]
            )


nodeParser : Parser Node
nodeParser =
    Parser.lazy (\_ -> simpleNodeParser)
        |> Parser.andThen
            (\node ->
                modifierParser
                    |> Parser.andThen
                        (\modifier ->
                            Parser.succeed (modifier node)
                        )
            )


modifierParser : Parser (Node -> Node)
modifierParser =
    Parser.oneOf
        [ Parser.succeed Opt |. Parser.keyword "?"
        , Parser.succeed Plus |. Parser.keyword "+"
        , Parser.succeed Star |. Parser.keyword "*"
        , repeatParser
        , Parser.succeed identity
        ]


repeatParser : Parser (Node -> Node)
repeatParser =
    Parser.succeed identity
        |. Parser.keyword "{"
        |= Parser.oneOf
            [ Parser.succeed RepeatAtMost
                |. Parser.keyword ","
                |= Parser.int
            , Parser.int
                |> Parser.andThen
                    (\n ->
                        Parser.oneOf
                            [ Parser.succeed identity
                                |. Parser.keyword ","
                                |= Parser.oneOf
                                    [ Parser.map (RepeatBetween n) Parser.int
                                    , Parser.succeed (RepeatAtLeast n)
                                    ]
                            , Parser.succeed (RepeatExactly n)
                            ]
                    )
            ]
        |. Parser.keyword "}"


simpleNodeParser : Parser Node
simpleNodeParser =
    Parser.oneOf
        [ Parser.lazy (\_ -> groupParser)
        , charParser
        ]


groupParser : Parser Node
groupParser =
    Parser.succeed (\const ast -> const ast)
        |= Parser.oneOf
            [ Parser.succeed Group |. Parser.keyword "(?:"
            , Parser.succeed MatchGroup |. Parser.keyword "("
            ]
        |= Parser.lazy (\_ -> astParser)
        |. Parser.keyword ")"


charParser : Parser Node
charParser =
    Parser.oneOf
        [ Parser.succeed Matcher |= matcherParser
        , Parser.succeed Start |. Parser.keyword "^"
        , Parser.succeed End |. Parser.keyword "$"
        , Parser.succeed Matcher |. Parser.keyword "\\" |= specialCharParser
        , Parser.map (Matcher << Matcher.char) (oneCharParser notSpecial)
        ]


specialCharParser : Parser Matcher.Matcher
specialCharParser =
    Parser.oneOf
        [ Parser.succeed Matcher.digit |. Parser.keyword "d"
        , Parser.succeed Matcher.nonDigit |. Parser.keyword "D"
        , Parser.succeed Matcher.wordChar |. Parser.keyword "w"
        , Parser.succeed Matcher.nonWordChar |. Parser.keyword "W"
        , Parser.succeed Matcher.whiteSpace |. Parser.keyword "s"
        , Parser.succeed Matcher.nonWhiteSpace |. Parser.keyword "S"
        , Parser.succeed Matcher.char |. Parser.keyword "n" |= Parser.succeed '\n'
        , Parser.succeed Matcher.char |. Parser.keyword "t" |= Parser.succeed '\t'
        , Parser.succeed Matcher.char |. Parser.keyword "f" |= Parser.succeed '\x0C'
        , Parser.succeed Matcher.char |. Parser.keyword "r" |= Parser.succeed '\x0D'
        , Parser.succeed Matcher.char |. Parser.keyword "v" |= Parser.succeed '\x0B'
        , Parser.succeed Matcher.char |= oneCharParser (always True)
        ]


matcherParser : Parser Matcher.Matcher
matcherParser =
    Parser.succeed (\cons matchers -> cons <| Matcher.oneOf matchers)
        |= Parser.oneOf
            [ Parser.succeed Matcher.inverse |. Parser.keyword "[^"
            , Parser.succeed identity |. Parser.keyword "["
            ]
        |= Parser.repeat Parser.oneOrMore
            (Parser.oneOf
                [ Parser.succeed identity |. Parser.keyword "\\" |= specialCharParser
                , simpleMatcherParser
                ]
            )
        |. Parser.keyword "]"


simpleMatcherParser : Parser Matcher.Matcher
simpleMatcherParser =
    (oneCharParser (\char -> char /= ']'))
        |> Parser.andThen
            (\char ->
                if allowedInRange char then
                    Parser.oneOf
                        [ Parser.succeed (Matcher.range char)
                            |. Parser.keyword "-"
                            |= (oneCharParser allowedInRange)
                        , Parser.succeed <| Matcher.char char
                        ]
                else
                    Parser.succeed <| Matcher.char char
            )


allowedInRange : Char -> Bool
allowedInRange char =
    Char.isDigit char || Char.isLower char || Char.isUpper char


oneCharParser : (Char -> Bool) -> Parser Char
oneCharParser pred =
    Parser.map
        (\str ->
            case String.toList str |> List.head of
                Just char ->
                    char

                Nothing ->
                    Debug.crash "There should always one Char"
        )
        (Parser.keep (Parser.Exactly 1) pred)


notSpecial : Char -> Bool
notSpecial c =
    not <| List.member c [ '(', ')', '?', '+', '*', '|', '^', '$', '[', ']' ]



-- Conversion from AST to Program --


toProgram : AST -> Program.Program
toProgram ast =
    astToProgram ast
        |> Program.add Program.Matched


astToProgram : AST -> Program.Program
astToProgram ast =
    List.map nodeToProgram ast
        |> Program.sequence


nodeToProgram : Node -> Program.Program
nodeToProgram node =
    case node of
        Matcher matcher ->
            Program.singleton (Program.Match matcher)

        Start ->
            Program.singleton Program.Start

        End ->
            Program.singleton Program.End

        Opt node ->
            let
                prog =
                    nodeToProgram node

                optProg =
                    Program.singleton (Program.Split 1 (Program.length prog + 1))
            in
                Program.append optProg prog

        Plus node ->
            let
                prog =
                    nodeToProgram node
            in
                prog
                    |> Program.add (Program.Split 0 (Program.length prog + 1))

        Star node ->
            let
                prog =
                    nodeToProgram node

                optProg =
                    Program.singleton (Program.Split 1 (Program.length prog + 2))
            in
                Program.append optProg
                    (prog
                        |> Program.add (Program.Jump -1)
                    )

        RepeatExactly n node ->
            let
                prog =
                    nodeToProgram node
            in
                List.foldl
                    (\_ finalProg -> Program.append finalProg prog)
                    Program.empty
                    (List.range 1 n)

        RepeatBetween n m node ->
            let
                prog =
                    nodeToProgram (Opt node)
            in
                List.foldl
                    (\_ finalProg -> Program.append finalProg prog)
                    (nodeToProgram <| RepeatExactly n node)
                    (List.range (n + 1) m)

        RepeatAtLeast n node ->
            let
                prog =
                    nodeToProgram (Star node)
            in
                Program.append (nodeToProgram <| RepeatExactly n node) prog

        RepeatAtMost n node ->
            let
                prog =
                    nodeToProgram (Opt node)
            in
                List.foldl
                    (\_ finalProg -> Program.append finalProg prog)
                    Program.empty
                    (List.range 1 n)

        Alt node1 node2 ->
            let
                prog1 =
                    nodeToProgram node1

                prog2 =
                    nodeToProgram node2

                optProg =
                    Program.singleton (Program.Split 1 (Program.length prog1 + 2))

                firstPart =
                    Program.append optProg
                        (prog1
                            |> Program.add (Program.Jump (Program.length prog1 + Program.length prog2 + 1))
                        )
            in
                Program.append firstPart prog2

        Group ast ->
            astToProgram ast

        MatchGroup ast ->
            astToProgram ast
