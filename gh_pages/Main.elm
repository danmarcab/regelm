module Main exposing (..)

import Html exposing (Html, div, input, li, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Regelm exposing (Regex)
import Regelm.Random
import Random.Pcg as Random exposing (Seed)
import Set
import Element
import Element.Attributes
import Style exposing (StyleSheet)
import Style.Border as Border
import Color
import Style.Color as Color
import Style.Font as Font


type Styles
    = NoStyle
    | Main
    | Box


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ Style.style NoStyle []
        , Style.style Main
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , Style.style Box
            [ Color.text Color.white
            , Color.background Color.blue
            , Color.border Color.blue
            , Border.rounded 3 -- round all borders to 3px

            --            , paddingHint 20
            --            , hover
            --                [ Color.text Color.white
            --                , Color.background Color.red
            --                , Color.border Color.red
            --                , cursor "pointer"
            --                ]
            --            ]
            ]
        ]


view : Model -> Html Msg
view model =
    Element.root stylesheet <|
        Element.column Main
            [ Element.Attributes.center, Element.Attributes.width (Element.Attributes.px 800) ]
            [ header
            , inputs model
            , examples model
            ]


header =
    Element.text "Regelm"


inputs model =
    Element.row Box
        []
        [ Element.html (input [ onInput UpdateInput, value model.inputRegex ] [])

        --            , Element.html (text (Maybe.map (\_ -> "valid") model.compiledRegex |> Maybe.withDefault "invalid"))
        ]


examples model =
    Element.el Box [] (Element.html (examplesView model.examples))



--            div
--            []
--            [ input [ onInput UpdateInput, value model.inputRegex ] []
--            ,
--            , input [ onInput UpdateTest, value model.testString ] []
--            , examplesView model.examples
--            ]


examplesView : List String -> Html Msg
examplesView examples =
    ul [] (List.map (\str -> li [] [ text str ]) examples)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }
