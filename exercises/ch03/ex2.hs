-- file: ch03/Tree.hs
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)


printData = Node "parent" Nothing (Just (Node "right child" Nothing Nothing))