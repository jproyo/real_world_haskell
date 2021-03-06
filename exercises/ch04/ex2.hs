-- file: ch04/ch04.exercises.hs

import Data.Char (digitToInt,isDigit)
import Data.Either

asInt_fold :: String -> Int
asInt_fold [] = error "Not a valid number"
asInt_fold l@(x:xs) | x == '-' = negate (asInt_fold xs)
										| otherwise = foldl convertAndSum 0 l
	where convertAndSum acc x = acc * 10 + digitToInt x


type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Left "Empty Input"
asInt_either ('-':xs) = case result of 
													Left err -> Left err
													Right val -> Right (-val)
	where result = asInt_either xs
asInt_either xs = foldl convertAndSum (Right 0) xs
  where convertAndSum (Right acc) x | (x >= '0') && (x <= '9') = Right (acc * 10 + digitToInt x)
                                    | otherwise = Left ("Not a digit '" ++ [x] ++ "'")
        convertAndSum (Left err) _ = Left err

-- file: ch04/ch04.exercises.hs
concatFoldr :: [[a]] -> [a]
concatFoldr xs = foldr step [] xs       
  where step l x = l ++ x


takeWhileRecur :: (a -> Bool) -> [a] -> [a]
takeWhileRecur f [] = []
takeWhileRecur f (x:xs) | f x = x : (takeWhileRecur f xs)
                        | otherwise = takeWhileRecur f xs 

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold f xs = foldr step [] xs
  where step x l | f x = x : l
                 | otherwise = l

groupBy_foldl :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_foldl p xs = foldl step [] xs
  where step [] x = [[x]]
        step acc x | p (head (last acc)) x = (init acc) ++ [((last acc) ++ [x])]
                   | otherwise             = acc ++ [[x]]

anyFold :: (a -> Bool) -> [a] -> Bool
anyFold f xs = foldl step False xs
  where step acc x | f x = True 
                   | otherwise = acc

cycleFold :: [a] -> [a]
cycleFold xs = (foldr (:) [] xs) ++ (cycleFold xs)




