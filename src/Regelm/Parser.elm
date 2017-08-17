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
    | Alt (List Node)
    | Group AST
    | MatchGroup AST


parse : String -> Result String AST
parse str =
    Parser.run astParser str
        |> Result.mapError toString


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
                                case astoToNode other_ast of
                                    Alt nodes ->
                                        [ Alt <| astoToNode ast :: nodes ]

                                    node ->
                                        [ Alt <| astoToNode ast :: [ node ] ]
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
