module Pi where


import Random exposing (generate, map2, float)
import Signal
import Graphics.Element exposing (Element, empty)
import Graphics.Collage as C exposing (Form)
import Text
import Color
import String
import Window
import Time exposing (millisecond)

type alias Point = { x:Float, y:Float }
type alias State = ((Int, List Point), (Int, List Point))

initState = ((0,[]), (0,[]))

upstate : Point -> State -> State
upstate pt ((hc, hs), (mc, ms)) =
  let
    distance = sqrt (pt.x^2 + pt.y^2)
  in
    if distance <= 1.0 then
      ((hc+1, pt :: hs), (mc, ms))

    else
      ((hc, hs), (mc+1, pt :: ms))

view : (Int, Int) -> State -> Element
view (w, h) ((hitCount, hs), (missCount, ms)) =
  let
    collageSize = toFloat h / 2 - 20

    pointSize = 10

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

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s =
  let
    gen = float -1 1
  in
    generate (map2 Point gen gen) s

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
  let
    initial = ({x=0, y=0}, Random.initialSeed 17)

    next _ (_, seed) = genPoint seed
  in
    Signal.foldp next initial (Time.every 10)

signalPoint : Signal Point
signalPoint =
  signalPointSeed
  |> Signal.map fst

main : Signal Element
main =
  Signal.map2 view Window.dimensions
  (Signal.foldp upstate initState signalPoint)
