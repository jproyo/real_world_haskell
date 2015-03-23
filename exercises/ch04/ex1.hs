-- file: ch04/ex1.hs

import Data.Maybe
import Data.List

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs
safeLast _ = Nothing

safeInit :: [a] -> Maybe [a]
safeInit [x] = Just []
safeInit (x:xs) = Just (x : fromMaybe xs (safeInit xs))
safeInit _ = Nothing

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred (x:xs) | not (pred x) = splitWith pred xs
splitWith pred xs = (takeWhile pred xs):(splitWith pred next)
                    where rest = dropWhile pred xs
                          rev pred x = not (pred x)
                          next = dropWhile (rev pred) rest