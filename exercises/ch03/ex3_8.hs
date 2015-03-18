module Tree where

{-- snippet Tree --}
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)
