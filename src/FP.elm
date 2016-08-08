module FP where

import Html
import List
import String

digitsOfInt : Int -> List Int
digitsOfInt n =
  if n > 0 then
    case n of
      0 -> []
      _ -> digitsOfInt (n // 10) ++ [n % 10]

  else
    []

sum : Int -> Int
sum n =
  n
  |> digitsOfInt
  |> List.sum

genSequence : Int -> List Int
genSequence n =
  if n > 9 then
    genSequence (sum n) ++ [n]

  else
    [n]

digitalRoot : Int -> Int
digitalRoot n =
  let
    seq =
      n
      |> genSequence
  in
    case List.head seq of
      Just first -> first
      Nothing -> 0

additivePersistence : Int -> Int
additivePersistence n =
  List.length (genSequence n) - 1

subsequences : List a -> List (List a)
subsequences xs =
  case xs of
    [] -> [[]]
    h::t -> subsequences t ++ List.map (\x -> h :: x) (subsequences t)

take : Int -> List a -> Result String (List a)
take k xs =
  if List.length xs < k then
    Err "not enough elements" 

  else if k < 0 then
    Err "negative index"

  else
    Ok (List.take k xs)

main =
  Html.text "Hello world"
