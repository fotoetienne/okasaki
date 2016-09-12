module Pi where

import Random exposing (generate, float, initialSeed)
import Signal exposing (foldp)
import Graphics.Collage exposing (Form, collage, circle, move)
import Graphics.Element exposing (Element, empty)
import Color exposing (red, grey)
import String
import Window
import Time exposing (fps, inMilliseconds)

type alias Point = { x:Float, y:Float }

type alias State = ((Int, List Point), (Int, List Point))

initState = ((0,[]), (0,[]))

euclidean : Point -> Float
euclidean {x,y} = sqrt (x^2 + y^2)

upstate : Point -> State -> State
upstate pt ((hitCount, hitList), (missCount, missList)) =
    if euclidean pt < 1 then
        ((hitCount + 1, pt :: hitList), (missCount, missList))
    else
        ((hitCount, hitList), (missCount + 1, pt :: missList))

greySquare = Graphics.Collage.filled Color.grey (Graphics.Collage.square 400)

dot = Graphics.Collage.filled Color.red (Graphics.Collage.circle 1)

scale x = (x - 0.5) * 400

renderPoint : Point -> Form
renderPoint {x,y} = move (scale x, scale y) dot

view : (Int,Int) -> State -> Element
view (w,h) ((hitCount, hits), (missCount, misses)) =
  collage w h <| greySquare :: (List.map renderPoint hits)

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s0 =
    let (x, s1) = generate (float 0 1) s0 in
    let (y, s2) = generate (float 0 1) s1 in
    ({x=x,y=y}, s2)

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
    Signal.map (inMilliseconds >> floor >> initialSeed >> genPoint) (fps 30)

signalPoint : Signal Point
signalPoint = Signal.map fst signalPointSeed

main : Signal Element
main =
  Signal.map2 view Window.dimensions
  (Signal.foldp upstate initState signalPoint)
