module ListsAndTrees where

suffixes l =
    case l of
        []    -> [[]]
        (_::xs) -> l :: suffixes xs

type Tree = Empty | Node Int Tree Tree

mem : Int -> Tree -> Bool
mem x t =
   case t of
       Node y lt rt ->
           if x < y then
               mem x lt
           else if x > y then
               mem x rt
           else
               True
       Empty -> False

fullTree : Int -> Int -> Tree
fullTree _ _ =
  -- TODO
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
