module Pi where

import Random exposing (generate, float, initialSeed)
import Signal exposing (foldp)
import Graphics.Collage as C exposing (Form, collage, circle, move)
import Graphics.Element as E exposing (Element, empty)
import Color exposing (Color, red, grey, black, white)
import String
import Window
import Time exposing (fps, inMilliseconds)
import Text exposing (fromString, typeface, height, bold)

type alias Point = { x:Float, y:Float }

type alias State = ((Int, List Point), (Int, List Point))

initState = ((0,[]), (0,[]))

euclidean : Point -> Float
euclidean {x,y} = x^2 + y^2

upstate : Point -> State -> State
upstate pt ((hitCount, hitList), (missCount, missList)) =
    if euclidean pt <= 1 then
        ((hitCount + 1, pt :: hitList), (missCount, missList))
    else
        ((hitCount, hitList), (missCount + 1, pt :: missList))

mupstate : List (Point) -> State -> State
mupstate pts st = List.foldl upstate st pts

scale w x = x * (toFloat w)

greyRect w = C.filled grey <| C.rect (scale w 2) (scale w 1)

dot w color = C.filled color <| C.circle <| toFloat w / 150

estimatePi : Int -> Int -> Float
estimatePi hitCount totalCount = 4 * (toFloat hitCount) / (toFloat totalCount)

renderPoint : Int -> Color -> Point -> Form
renderPoint w color {x,y} = move (scale w x, scale w (y - 0.5)) (dot w color)

renderPoints: Int -> List Point -> Color -> List Form
renderPoints s points color = (List.map (renderPoint s color) points)

whiteRect s = [ move (0, -(toFloat s / 20)) <| C.filled white
                    <| C.rect (toFloat s * 2) (toFloat s / 3)]

piText: Int -> Int -> Int -> List Form
piText s hitCount totalCount =
    [C.text (bold <| height (toFloat s / 3) <| typeface ["helvetica"] <| fromString <|
                     String.left 7 <| toString <| estimatePi hitCount totalCount),
     move (0,-(toFloat s / 3)) <| C.text (height (toFloat s / 8) <| fromString <| toString <| totalCount)]

view : (Int,Int) -> State -> Element
view (w,h) ((hitCount, hits), (missCount, misses)) =
    let s = min (w // 2) h in
    let totalCount = hitCount + missCount in
    E.layers <| List.map (collage w h)
        [renderPoints s hits red,
        [C.toForm <| E.opacity 0.5 <| C.collage w h <| whiteRect s],
         renderPoints s misses black,
         piText s hitCount totalCount]


genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s0 =
    let (x, s1) = generate (float -0.9999 0.9999) s0 in
    let (y, s2) = generate (float -0.0001 0.9999) s1 in
    ({x=x,y=y}, s2)

genPoints : Int -> Random.Seed -> (List Point, Random.Seed)
genPoints n s0 =
    List.foldl
      (\_ (pts, s) -> let (pt, s') = genPoint s in (pt :: pts, s'))
      ([],s0) [1..n]

signalPointSeed : Signal (List Point, Random.Seed)
signalPointSeed =
    foldp (\_ (_,s) -> genPoints 99 s) (42 |> initialSeed |> (genPoints 99)) (fps 25)

signalPoint : Signal (List Point)
signalPoint = Signal.map fst signalPointSeed

main : Signal Element
main =
  Signal.map2 view Window.dimensions
  (Signal.foldp mupstate initState signalPoint)
