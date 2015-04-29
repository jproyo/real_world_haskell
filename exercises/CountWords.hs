groupByLength :: Int -> String -> [String]
groupByLength l [] = []
groupByLength l word | l > 0 = (take l word) : groupByLength l (drop l word)
	| otherwise = []

splitAndCount :: [Int] -> Int -> String -> Int
splitAndCount _ _ [] = 0
splitAndCount groups top word = head (filter (<=top) (map lengthGroup groups))
    where lengthGroup x = length $ groupByLength x word


countWord :: Int -> Int -> Int -> Int -> String -> Int
countWord n k l m word 
  | 2 > k || k > l || l > 26 = 0
  | m < 1 = 0
  | n < 1 = 0
  | otherwise = splitAndCount [k..l] m (take n word)

main = print $ countWord 6 2 3 4 "ababab"