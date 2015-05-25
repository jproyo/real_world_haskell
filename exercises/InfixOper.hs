import System.Environment

infix 9 +*

(+*) :: String -> String -> String
(+*) a b = a ++ b


main = do
	args <- getArgs
	print $ foldr concatSpace [] args 
            where concatSpace a [] = a
                  concatSpace a b = a +* " " ++ b