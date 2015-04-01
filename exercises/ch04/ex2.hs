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