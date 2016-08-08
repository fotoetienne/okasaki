module Pi where

import Html exposing (Html)
import Html.App as App
import Time exposing (Time, second) 
import Collage as C 
import Element as E
import Color
--import Element exposing (Element, empty)
--import Element

--import String
--import Window
-- TODO: modify/add imports as needed

type alias Point = { x:Float, y:Float }
type alias Model = ((Int, List Point), (Int, List Point))

type Msg = Tick Time

main =
  App.program 
  {
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

init : (Model, Cmd Msg)
init =
  (initState, Cmd.none)

initState = ((0,[]), (0,[]))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Tick newTime ->
      (model, Cmd.none)

upstate : Point -> Model -> Model
upstate pt ((hc, hs), (mc, ms)) =
  if pt.x^2 + pt.y^2 <= 1 then
    ((hc+1, pt :: hs), (mc, ms))

  else
    ((hc, hs), (mc+1, pt :: ms))

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

view : Model -> Html Msg
view model =
  let
    circ = C.filled Color.darkBlue (C.circle 20)
  in
    C.collage 10 10 (circ)


--view : (Int,Int) -> State -> Element
--view (w,h) st =
--  -- TODO
--  empty

--genPoint : Random.Seed -> (Point, Random.Seed)
--genPoint s =
--  step (map2 Point (float 0 1) (float 0 1)) s

--signalPointSeed : Model -> 
--signalPointSeed =
--  Time.every second Tick
--  -- TODO
--  Signal.constant (genPoint (Random.initialSeed 0))

--signalPoint : Signal Point
--signalPoint =
--  -- TODO
--  Signal.constant {x=0,y=0}

--main : Signal Element
--  Signal.map2 view Window.dimensions
--  (Signal.foldp upstate initState signalPoint)
