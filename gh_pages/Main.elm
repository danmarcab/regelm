module Main exposing (..)

import Html exposing (Html, div, input, li, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Regelm exposing (Regex)
import Regelm.Random
import Random.Pcg as Random exposing (Seed)
import Set


type alias Model =
    { inputRegex : String
    , compiledRegex : Maybe Regex
    , testString : String
    , examples : List String
    , seed : Seed
    }


init : ( Model, Cmd Msg )
init =
    ( Model ""
        Nothing
        ""
        []
        (Random.initialSeed 0)
    , Random.generate SetSeed <| Random.independentSeed
    )


type Msg
    = UpdateInput String
    | UpdateTest String
    | SetSeed Seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput str ->
            let
                compiled =
                    Regelm.regex str
                        |> Result.toMaybe

                examples =
                    Maybe.map (generateExamples 10 model.seed) compiled
                        |> Maybe.withDefault []
            in
                ( { model
                    | inputRegex = str
                    , compiledRegex = compiled
                    , examples = examples
                  }
                , Cmd.none
                )

        UpdateTest str ->
            ( { model | testString = str }, Cmd.none )

        SetSeed seed ->
            ( { model | seed = seed }, Cmd.none )


generateExamples : Int -> Seed -> Regex -> List String
generateExamples n seed regex =
    generateExamplesWithLimit 1000 n [] seed regex
        |> List.sort


generateExamplesWithLimit : Int -> Int -> List String -> Seed -> Regex -> List String
generateExamplesWithLimit remainingAttempts n list seed regex =
    if remainingAttempts == 0 || List.length list == n then
        list
    else
        let
            ( str, newSeed ) =
                Random.step (Regelm.Random.pattern regex) seed

            newList =
                if List.member str list then
                    list
                else
                    str :: list
        in
            generateExamplesWithLimit (remainingAttempts - 1) n newList newSeed regex


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput UpdateInput, value model.inputRegex ] []
        , text (Maybe.map (\_ -> "valid") model.compiledRegex |> Maybe.withDefault "invalid")
        , input [ onInput UpdateTest, value model.testString ] []
        , examplesView model.examples
        ]


examplesView : List String -> Html Msg
examplesView examples =
    ul [] (List.map (\str -> li [] [ text str ]) examples)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }
