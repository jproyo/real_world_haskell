-- file ch02/ex1.hs
lastButOne :: [a] -> a
lastButOne (x:[xs]) = x
lastButOne (x:xs) = lastButOne xs