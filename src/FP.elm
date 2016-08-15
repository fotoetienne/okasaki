module FP (digitsOfInt, additivePersistence, digitalRoot, subsequences, take) where

import List exposing (sum, drop, map2, length, repeat)

digitsOfInt : Int -> List Int
digitsOfInt n =
    if n < 0 then
        []
    else if n < 10 then
        [n]
    else
        digitsOfInt (n // 10) ++ [(n % 10)]


digitalSeq : Int -> List Int
digitalSeq n =
    let n1 = n |> digitsOfInt |> sum in
    if n1 < 10 then
        [n1]
    else
        digitalSeq n1 ++ [n1]

additivePersistence : Int -> Int
additivePersistence n =
    List.length (digitalSeq n)

digitalRoot : Int -> Int
digitalRoot n =
    let drs = digitalSeq n in
    case List.head drs of
        Just h -> h
        Nothing -> 0

subsequences : List a -> List (List a)
subsequences xs =
  let l = length xs in
  map2 drop [0..l] (repeat (l + 1) xs)

take : Int -> List a -> Result String (List a)
take k xs =
  let l = length xs in
  if k < 0 then
      Err "negative index"
  else if k > l then
      Err "not enough elements"
  else
      Ok (List.take k xs)
