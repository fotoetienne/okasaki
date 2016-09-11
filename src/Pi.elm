module Pi exposing (..)

import Html.App as App
import Html exposing (Html)
import Time exposing (Time, millisecond)
import Random exposing (generate, map2, float)
import Collage as C exposing (Form)
import Element exposing (Element, empty, toHtml)
import Text
import Color
import String

type Msg
  = Tick Time
  | NewPoint Point

type alias Point = { x:Float, y:Float }
type alias Model = ((Int, List Point), (Int, List Point))

initModel = ((0,[]), (0,[]))

init : (Model, Cmd Msg)
init = (initModel, Cmd.none)

upstate : Point -> Model -> Model
upstate pt ((hc, hs), (mc, ms)) =
  let
    distance = sqrt (pt.x^2 + pt.y^2)
  in
    if distance <= 1.0 then
      ((hc+1, pt :: hs), (mc, ms))

    else
      ((hc, hs), (mc+1, pt :: ms))

genPoint : Cmd Msg
genPoint =
  let
    gen = float -1 1
  in
    generate NewPoint (map2 Point gen gen)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      (model, genPoint)

    NewPoint point ->
      (upstate point model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every (Time.millisecond) Tick

view : Model -> Html Msg
view ((hitCount, hs), (missCount, ms)) =
  let
    h = 200
    w = 200

    collageSize = toFloat h / 2 - 20

    pointSize = 4

    toHit point =
      pointSize
      |> C.circle
      |> C.filled Color.darkBlue
      |> C.move (point.x * collageSize, point.y * collageSize)

    toMiss point =
      pointSize
      |> C.circle
      |> C.filled Color.red
      |> C.move (point.x * collageSize, point.y * collageSize)

    number =
      (toFloat hitCount / toFloat (hitCount + missCount)) * 4.0
      |> toString
      |> Text.fromString
      |> C.text
      |> C.move (collageSize, collageSize)
  in
    C.collage w h ((List.map toHit hs) ++ (List.map toMiss ms) ++ [number])
    |> Element.toHtml

main : Program Never
main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
