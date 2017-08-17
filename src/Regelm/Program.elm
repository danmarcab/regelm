module Regelm.Program exposing (..)

import Array.Hamt as Array exposing (Array)
import Set exposing (Set)
import Regelm.Matcher as Matcher


type Regex
    = Regex Program


type alias Program =
    Array Inst


type Inst
    = Start
    | Match Matcher.Matcher
    | End
    | Jump Int
    | Split (List Int)
    | Matched



-- Functions to work with programs --


empty : Program
empty =
    Array.empty


singleton : Inst -> Program
singleton inst =
    add inst empty


add : Inst -> Program -> Program
add inst prog =
    Array.push inst prog


sequence : List Program -> Program
sequence programs =
    List.foldl (\prog finalProg -> append finalProg prog) empty programs


append : Program -> Program -> Program
append prog other =
    Array.append prog (shift (length prog) other)


appendNoShift : Program -> Program -> Program
appendNoShift prog other =
    Array.append prog other


length : Program -> Int
length prog =
    Array.length prog


shift : Int -> Program -> Program
shift n prog =
    Array.map (shiftInst n) prog


shiftInst : Int -> Inst -> Inst
shiftInst n inst =
    case inst of
        Jump a ->
            Jump (a + n)

        Split list ->
            Split (List.map (\a -> a + n) list)

        _ ->
            inst


getInst : Int -> Program -> Maybe Inst
getInst idx program =
    Array.get idx program



-- Execute program --


contains : Program -> String -> Bool
contains regex str =
    List.foldl
        (\char threads ->
            step char (threads |> Set.insert 0 |> Set.toList) Set.empty regex
        )
        (initialStep regex)
        (String.toList str)
        |> (\threads -> finalStep (threads |> Set.insert 0 |> Set.toList) Set.empty regex)
        |> matched regex


initialStep : Program -> Set Int
initialStep prog =
    case Array.get 0 prog of
        Nothing ->
            Set.empty

        Just Start ->
            Set.singleton 1

        _ ->
            Set.singleton 0


finalStep : List Int -> Set Int -> Program -> Set Int
finalStep now later prog =
    case now of
        [] ->
            later

        idx :: rest ->
            let
                newState =
                    case Array.get idx prog of
                        Just End ->
                            { now = rest, later = Set.insert (idx + 1) later }

                        Just Matched ->
                            { now = rest, later = Set.insert idx later }

                        Just (Jump a) ->
                            { now = a :: rest, later = later }

                        Just (Split list) ->
                            { now = list ++ rest, later = later }

                        -- cases not relevant for final step
                        Just Start ->
                            { now = rest, later = later }

                        Just (Match f) ->
                            { now = rest, later = later }

                        Nothing ->
                            { now = rest, later = later }
            in
                finalStep newState.now newState.later prog


step : Char -> List Int -> Set Int -> Program -> Set Int
step char now later prog =
    case now of
        [] ->
            later

        idx :: rest ->
            let
                newState =
                    case Array.get idx prog of
                        Just (Match matcher) ->
                            if Matcher.matches char matcher then
                                { now = rest, later = Set.insert (idx + 1) later }
                            else
                                { now = rest, later = later }

                        Just Matched ->
                            { now = rest, later = Set.insert idx later }

                        Just (Jump a) ->
                            { now = a :: rest, later = later }

                        Just (Split list) ->
                            { now = list ++ rest, later = later }

                        -- cases not relevant for final step
                        Just Start ->
                            { now = rest, later = later }

                        Just End ->
                            { now = rest, later = later }

                        Nothing ->
                            { now = rest, later = later }
            in
                step char newState.now newState.later prog


matched : Program -> Set Int -> Bool
matched prog threads =
    threads
        |> Set.toList
        |> List.filterMap (\i -> Array.get i prog)
        |> List.any ((==) Matched)
