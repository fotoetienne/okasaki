module ListsAndTrees where

suffixes : List a -> List (List a)
suffixes xs = case xs of
  [] -> [[]]
  x :: suffix -> xs :: suffixes suffix

-- Runs in O(n) time because suffixes is called exactly once for each x in xs
-- Runs in O(n) memory because no state is retained except xs and suffixes

type Tree = Empty | Node Int Tree Tree

mem : Int -> Tree -> Bool
mem x t = case t of
  Empty -> False
  Node n left right ->
      if x < n then
          mem x left
      else
          mem x right || x == n

fullTree : Int -> Int -> Tree
fullTree x h =
  if h <= 0 then
      Empty
  else let leaf = fullTree x (h - 1) in
      Node x leaf leaf

balancedTree : Int -> Int -> Tree
balancedTree x h =
  if h <= 0 then
      Empty
  else Node x (balancedTree x <| (h - 1) // 2)
              (balancedTree x <| (h - 1) // 2 + (h - 1) % 2)

create2 : Int -> Int -> (Tree, Tree)
create2 _ _ =
  -- TODO
  (Empty, Empty)

balancedTrees : Int -> Int -> List Tree
balancedTrees _ _ =
  -- TODO
  []

completeTrees : Int -> Int -> List Tree
completeTrees _ _ =
  -- TODO
  []

almostCompleteTrees : Int -> Int -> List Tree
almostCompleteTrees _ _ =
  -- TODO
  []
