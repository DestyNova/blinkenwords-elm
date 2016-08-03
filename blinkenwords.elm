import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import String exposing (join, split)
import Regex exposing (contains, regex)
import Task
import Process

main =
  App.program
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

init : (Model, Cmd Msg)
init =
  ({ words = [], wpm = 270, wordSpan = 3, position = 0, playing = False }, Cmd.none)


-- UPDATE

type Msg
  = Tick () | TickFail () | Change String | SpeedUp | SpeedDown | SpanUp | SpanDown | Rew | Fw | Pause

step : Model -> Model
step model =
  { model | position = Basics.min (length model.words - model.wordSpan) (model.position + model.wordSpan) }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      if model.playing then
        let
          newModel = step model
          atEnd = newModel.position + newModel.wordSpan >= length newModel.words
        in
          if atEnd then
            ({newModel | playing = False}, Cmd.none)
          else
            (newModel, waitNext newModel)

      else
        (model, Cmd.none)

    TickFail _ ->
      (model, Cmd.none)

    Change newContent ->
      ({ model | words = split " " newContent, position = 0, playing = True }, waitNext model)

    Pause ->
      let playing = not model.playing in
        ({ model | playing = playing }, if playing then waitNext model else Cmd.none)

    Rew ->
      ({ model | playing = False, position = Basics.max 0 (model.position - model.wordSpan) }, Cmd.none)

    Fw ->
      let newModel = step model in
        ({newModel | playing = False}, Cmd.none)

    SpeedDown ->
      ({ model | position = Basics.max 100 (model.wpm - 5) }, Cmd.none)

    SpeedUp ->
      ({ model | position = Basics.min 1000 (model.wpm + 5) }, Cmd.none)

    SpanDown ->
      ({ model | wordSpan = Basics.max 1 (model.wordSpan - 1) }, Cmd.none)

    SpanUp ->
      ({ model | wordSpan = Basics.min 8 (model.wordSpan + 1) }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- TASKS

waitNext : Model -> Cmd Msg
waitNext model =
  let
    extraDelay = if containsBreak (nextWords model) then 120000 // model.wpm else 0
    delay = (toFloat (model.wordSpan * 60000 // model.wpm + extraDelay))
  in
    Task.perform TickFail Tick <| Process.sleep delay

containsBreak : List String -> Bool
containsBreak words =
  contains (regex "[,.-]") (join "" words)

nextWords : Model -> List String
nextWords model =
  take model.wordSpan <| drop model.position model.words

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Source text", onInput Change ] []
    , button [ onClick Pause ] [ text (if model.playing then "⏸" else "▶") ]
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
    , a [href "http://github.com/DestyNova/blinkenwords-elm"] [text "Source"]
    , div [style
        [("textAlign", "center")
        ,("color", "red")
        ,("backgroundColor", "cornsilk")
        ,("lineHeight", "150%")
        ], onClick Pause] <| List.concatMap (\w -> [text w, br [] []]) (nextWords model)
    ]
