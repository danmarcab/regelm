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
                    Maybe.map (generateExamples model.seed) compiled
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


generateExamples : Seed -> Regex -> List String
generateExamples seed regex =
    List.foldl
        (\_ ( seed, list ) ->
            let
                ( str, newSeed ) =
                    Random.step (Regelm.Random.pattern regex) seed
            in
                ( newSeed, str :: list )
        )
        ( seed, [] )
        (List.range 1 50)
        |> Tuple.second
        |> Set.fromList
        |> Set.toList
        |> List.take 10


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput UpdateInput, value model.inputRegex ] []
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
