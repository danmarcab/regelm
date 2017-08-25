module Main exposing (..)

--import Html exposing (Html, div, input, li, text, ul)
--import Html.Attributes exposing (value)
--import Html.Events exposing (onInput)

import Regelm exposing (Regex)
import Regelm.Random
import Random.Pcg as Random exposing (Seed)
import Set
import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition


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


{-| A synonym for creating tuples. This will be included in the standard library soon.

1 => 2 == (1, 2)

-}
(=>) =
    (,)


{-| The type we use for identifiers for our styles.
-}
type Styles
    = None
    | Main
    | Page
    | Logo
    | NavOption
    | Box
    | Container
    | Label


{-| First, we create a stylesheet.

Styles only deal with properties that are not related to layout, position, or size.

Generally all properties only have one allowed unit, which is usually px.

If you want to use something like em

-}
stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None [] -- It's handy to have a blank style
        , style Main
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , style Page
            [ Border.all 5
            , Border.solid
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            ]
        , style Label
            [ Font.size 25 -- set font size to 25 px
            , Font.center
            ]
        , style Logo
            [ Font.size 25
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style NavOption
            [ Font.size 16
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style Box
            [ Transition.all
            , Color.text Color.white
            , Color.background Color.blue
            , Color.border Color.blue
            , Border.rounded 3 -- round all borders to 3px
            , paddingHint 5
            , hover
                [ Color.text Color.white
                , Color.background Color.red
                , Color.border Color.red
                , cursor "pointer"
                ]
            ]
        , style Container
            [ Color.text Color.black
            , Color.background Color.lightGrey
            , Color.border Color.lightGrey
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }


{-| Our view is made up of `Element`s,

which you can think of as Html with layout, positioning, and spacing built in.

-}
view model =
    Element.root stylesheet <|
        column None
            []
            [ navigation
            , el None [ center, width (px 800) ] <|
                column Main
                    [ spacing 50, paddingTop 50, paddingBottom 50 ]
                    (List.concat
                        [ viewTextLayout model
                        ]
                    )
            ]


navigation =
    row None
        [ justify, paddingXY 80 20 ]
        [ el Logo [] (text "Regelm")
        , row None
            [ spacing 20 ]
            [ el NavOption [ alignBottom ] (text "share")
            , el NavOption [ alignBottom ] (text "about")
            ]
        ]


{-| A text layout
-}
viewTextLayout model =
    [ el Label [] (text "First, Some Text")
    , textLayout None
        [ spacingXY 25 25
        , padding 60
        ]
        [ paragraph None [] [ text "bullshhit" ]
        , row None
            []
            [ label None [ width <| percent 50 ] (text "Enter your regex") <|
                inputText None [ onInput UpdateInput ] model.inputRegex
            , label None [ width <| percent 50 ] (text "Enter your string") <|
                inputText None [ onInput UpdateTest ] model.testString
            ]
        , column
            Box
            [ spacing 5 ]
            (List.map
                (\str ->
                    el None [] (text str)
                )
             <|
                model.examples
            )
        ]
    ]
