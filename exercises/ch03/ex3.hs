-- file: ch03/ex3.hs
import Data.List

myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength _ = 0

mean :: [Double] -> Double
mean xs = (total xs) / fromIntegral (length xs)
          where total :: [Double] -> Double
                total [] = 0.0
                total (x:xs) = x + total xs

palindrome :: [a] -> [a]
palindrome [] = []
palindrome [x] = x : [x]
palindrome (x:xs) = x : palindrome xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) 
	| x == last xs = isPalindrome (init xs)
	| otherwise = False


sortListByLenght :: [[a]] -> [[a]]
sortListByLenght xs = sortBy byLength xs
    where 
    	byLength one other = compare (length one) (length other)

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = x
myIntersperse sep (x:xs) = x ++ [sep] ++ (myIntersperse sep xs)




              
main = print "Hello"					