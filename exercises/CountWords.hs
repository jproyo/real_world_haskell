groupByLength :: Int -> String -> [String]
groupByLength l [] = []
groupByLength l word | l > 0 = (take l word) : groupByLength l (drop l word)
	| otherwise = []

splitAndCount :: Int -> [Int] -> Int -> String -> Int
splitAndCount _ _ _ [] = 0
splitAndCount maxLengthWord groupsLength maximumTop word = maximumHead groupsLength maximumTop (take maxLengthWord word)
    where maximumHead _ _ [] = 0 
          maximumHead (x:xs) top wordTruncated = head (filter (<=top) ((length $ groupByLength x wordTruncated) : [maximumHead xs top wordTruncated]))


countWord :: Int -> Int -> Int -> Int -> String -> Int
countWord n k l m word 
  | 2 > k || k > l || l > 26 = 0
  | m < 1 = 0
  | n < 1 = 0
  | otherwise = splitAndCount n [k..l] m word

main = print $ countWord 6 2 3 4 "ababab"