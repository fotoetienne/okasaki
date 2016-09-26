module LHeaps where

{-- Copied from LeftistHeaps.elm from class. DO NOT MODIFY. -------------}

type alias Rank = Int

type Heap = E | T Rank Int Heap Heap

rank : Heap -> Rank
rank h =
  case h of
    E         -> 0
    T r _ _ _ -> r

makeT : Int -> Heap -> Heap -> Heap
makeT x h1 h2 =
  let (left,right) =
    if rank h1 >= rank h2
      then (h1, h2)
      else (h2, h1)
  in
  T (1 + rank right) x left right

merge : Heap -> Heap -> Heap
merge h1 h2 = case (h1, h2) of
  (_, E) -> h1
  (E, _) -> h2
  (T _ x1 left1 right1, T _ x2 left2 right2) ->
    if x1 <= x2
      then makeT x1 left1 (merge right1 h2)
      else makeT x2 left2 (merge h1 right2)

{------------------------------------------------------------------------}

fromList : List Int -> Heap
fromList xs = case makePass <| List.map (\x -> makeT x E E) xs of
  [] -> E
  x::xs -> x

mergePairs : List Heap -> List Heap
mergePairs heaps = case heaps of
  [] -> []
  h :: [] -> [h]
  h0 :: h1 :: hs -> merge h0 h1 :: mergePairs hs

makePass   : List Heap -> List Heap
makePass heaps = case heaps of
  [] -> []
  h :: [] -> [h]
  h :: hs -> makePass <| mergePairs heaps
