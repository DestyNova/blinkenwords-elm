import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import String
import Time exposing (Time, millisecond)


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
  = Tick Time | Change String | SpeedUp | SpeedDown | SpanUp | SpanDown | Rew | Fw | Pause

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      if model.playing then
        update Fw model
      else
        (model, Cmd.none)

    Change newContent ->
      ({ model | words = String.split " " newContent, position = 0, playing = True }, Cmd.none)

    Pause ->
      ({ model | playing = not model.playing }, Cmd.none)

    Rew ->
      ({ model | position = Basics.max 0 (model.position - model.wordSpan) }, Cmd.none)

    Fw ->
      ({ model | position = Basics.min (List.length model.words - model.wordSpan) (model.position + model.wordSpan) }, Cmd.none)

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
  Time.every (toFloat (model.wordSpan * 60000 // model.wpm) * millisecond) Tick

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Source text", onInput Change ] []
    , button [ onClick Pause ] [ text "|>" ]
    , button [ onClick Rew ] [ text "<<" ]
    , button [ onClick Fw ] [ text ">>" ]
    , button [ onClick SpeedDown ] [ text "-" ]
    , button [ onClick SpeedUp ] [ text "+" ]
    , button [ onClick SpanDown ] [ text "-" ]
    , button [ onClick SpanUp ] [ text "+" ]
    , div [style [("textAlign", "center")]] <| List.concatMap (\w -> [text w, br [] []]) (take model.wordSpan <| drop model.position model.words)
    ]
