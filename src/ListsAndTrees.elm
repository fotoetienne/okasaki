module ListsAndTrees exposing (..)

-- Since we're using cons to pattern match the first element of the list and
-- we never destruct the remainder of the list we only call `suffixes` once
-- for each item in the list. Thus it runs in O(n) time.
suffixes list =
  case list of
    (x::xs) -> [list] ++ (suffixes xs)
    []      -> [[]]

type Tree = Empty | Node Int Tree Tree

mem : Int -> Tree -> Bool
mem x tree =
  case tree of
    Node y l r ->
           if x < y then (mem x l)
      else if x > y then (mem x r)
      else               True

    Empty ->
      False

fullTree : Int -> Int -> Tree
fullTree x h =
  if h > 0 then
    let
      tree = fullTree x (h-1)
    in
      Node x tree tree
  else
    Empty

balancedTree : Int -> Int -> Tree
balancedTree _ _ =
  -- TODO
  Empty

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

testSuffixes =
  suffixes [1..4] == [[1,2,3,4],[2,3,4],[3,4],[4],[]]
