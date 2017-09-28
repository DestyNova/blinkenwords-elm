module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import String exposing (join, split)
import Regex exposing (contains, regex)
import Task
import Process


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { words : List String
    , wpm : Int
    , wordSpan : Int
    , position : Int
    , playing : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { words = [], wpm = 270, wordSpan = 3, position = 0, playing = False }, Cmd.none )



-- UPDATE


type Msg
    = Tick ()
    | Change String
    | SpeedUp
    | SpeedDown
    | SpanUp
    | SpanDown
    | Rew
    | Fw
    | Pause


step : Model -> Model
step model =
    { model | position = Basics.min (length model.words - 1) (model.position + model.wordSpan) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.playing then
                let
                    newModel =
                        step model

                    atEnd =
                        newModel.position + newModel.wordSpan >= length newModel.words
                in
                    if atEnd then
                        ( { newModel | playing = False }, Cmd.none )
                    else
                        ( newModel, waitNext newModel )
            else
                ( model, Cmd.none )

        Change newContent ->
            ( { model | words = split " " newContent, position = 0, playing = True }, waitNext model )

        Pause ->
            let
                playing =
                    not model.playing
            in
                ( { model | playing = playing }
                , if playing then
                    waitNext model
                  else
                    Cmd.none
                )

        Rew ->
            ( { model | playing = False, position = Basics.max 0 (model.position - model.wordSpan) }, Cmd.none )

        Fw ->
            let
                newModel =
                    step model
            in
                ( { newModel | playing = False }, Cmd.none )

        SpeedDown ->
            ( { model | wpm = Basics.max 100 (model.wpm - 5) }, Cmd.none )

        SpeedUp ->
            ( { model | wpm = Basics.min 1000 (model.wpm + 5) }, Cmd.none )

        SpanDown ->
            ( { model | wordSpan = Basics.max 1 (model.wordSpan - 1) }, Cmd.none )

        SpanUp ->
            ( { model | wordSpan = Basics.min 8 (model.wordSpan + 1) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- TASKS


waitNext : Model -> Cmd Msg
waitNext model =
    let
        extraDelay =
            if containsBreak (nextWords model) then
                120000 // model.wpm
            else
                0

        delay =
            (toFloat (model.wordSpan * 60000 // model.wpm + extraDelay))
    in
        Task.perform Tick <| Process.sleep delay


containsBreak : List String -> Bool
containsBreak words =
    contains (regex "[,.-]") (join "" words)


nextWords : Model -> List String
nextWords model =
    take model.wordSpan <| drop model.position model.words



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "0 auto" )
            , ( "maxWidth", "50em" )
            , ( "fontFamily", "'Helvetica', 'Arial', 'sans-serif'" )
            , ( "textAlign", "center" )
            , ( "padding", "8px" )
            ]
        ]
        [ div
            [ style
                [ ( "background", "#eeeeee" ) ]
            ]
            [ input [ placeholder "Source text", onInput Change ] []
            , button [ onClick Pause ]
                [ text
                    (if model.playing then
                        "⏸"
                     else
                        "▶"
                    )
                ]
            , button [ onClick Rew ] [ text "<<" ]
            , button [ onClick Fw ] [ text ">>" ]
            , text "WPM:"
            , button [ onClick SpeedDown ] [ text "-" ]
            , text (toString model.wpm)
            , button [ onClick SpeedUp ] [ text "+" ]
            , text "Span:"
            , button [ onClick SpanDown ] [ text "-" ]
            , text (toString model.wordSpan)
            , button [ onClick SpanUp ] [ text "+" ]
            , a [ href "http://github.com/DestyNova/blinkenwords-elm" ] [ text "Source" ]
            ]
        , div []
            [ makeProgressBar (model.position + model.wordSpan) (length model.words)
            , makeReadingPane <| List.concatMap (\w -> [ text w, br [] [] ]) (nextWords model)
            ]
        ]


makeProgressBar : Int -> Int -> Html Msg
makeProgressBar position numWords =
    div
        [ style
            [ ( "border-radius", "15px" )
            , ( "height", "12px" )
            , ( "background", "#555" )
            , ( "box-shadow", "inset 0 -1px 1px rgba(255,255,255,0.3)" )
            , ( "padding", "2px" )
            , ( "position", "relative" )
            ]
        ]
        [ span
            [ style
                [ ( "height", "100%" )
                , ( "display", "block" )
                , ( "height", "100%" )
                , ( "border-top-right-radius", "8px" )
                , ( "border-bottom-right-radius", "8px" )
                , ( "border-top-left-radius", "20px" )
                , ( "border-bottom-left-radius", "20px" )
                , ( "background-color", "rgb(43,194,83)" )
                , ( "background-image", "linear-gradient( center bottom, rgb(43,194,83) 37%, rgb(84,240,84) 69%)" )
                , ( "box-shadow", "inset 0 2px 9px  rgba(255,255,255,0.3), inset 0 -2px 6px rgba(0,0,0,0.4)" )
                , ( "position", "relative" )
                , ( "overflow", "hidden" )
                , ( "width", toString (Basics.min 100 <| position * 100 // numWords) ++ "%" )
                ]
            ]
            []
        ]


makeReadingPane : List (Html Msg) -> Html Msg
makeReadingPane nextChunk =
    div
        [ style
            [ ( "color", "red" )
            , ( "backgroundColor", "cornsilk" )
            , ( "border", "1px solid black" )
            , ( "box-shadow", "4px 4px 4px 0px rgba(0,0,0,0.75)" )
            , ( "lineHeight", "150%" )
            , ( "height", "4.5em" )
            , ( "margin", "0.5em" )
            ]
        , onClick Pause
        ]
        nextChunk
