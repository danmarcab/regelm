module Regelm.Compiler exposing (..)

import Regelm.Parser as Parser exposing (AST, Node(..))
import Regelm.Program as Program
import Regelm.Matcher as Matcher


-- Conversion from AST to Program --


compile : AST -> Program.Program
compile ast =
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

        Alt nodes ->
            let
                programs =
                    nodes
                        |> List.map
                            (\node ->
                                let
                                    prog =
                                        nodeToProgram node

                                    optProg =
                                        Program.singleton (Program.Split 1 (Program.length prog + 2))

                                    firstPart =
                                        Program.append optProg prog
                                in
                                    firstPart
                            )

                jumpTo =
                    programs
                        |> List.map (\p -> Program.length p + 1)
                        |> List.sum
            in
                List.foldl
                    (\prog ( finalProg, shift ) ->
                        let
                            newProg =
                                prog
                                    |> Program.shift shift
                                    |> Program.add (Program.Jump jumpTo)
                                    |> Program.appendNoShift finalProg
                        in
                            ( newProg, shift + (Program.length prog) + 1 )
                    )
                    ( Program.empty, 0 )
                    programs
                    |> Tuple.first

        Group ast ->
            astToProgram ast

        MatchGroup ast ->
            astToProgram ast
