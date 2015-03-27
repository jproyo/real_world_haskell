-- file: ch04/ch04.exercises.hs

import Data.Char (digitToInt)

asInt_fold :: String -> Int
asInt_fold [] = error "Not a valid number"
asInt_fold l@(x:xs) | x == '-' = negate (asInt_fold xs)
										| otherwise = foldl convertAndSum 0 l
	where convertAndSum acc x = acc * 10 + digitToInt x

