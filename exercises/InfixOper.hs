import System.Environment

infix 9 +*

(+*) :: String -> String -> String
(+*) a b = a ++ b

main = getArgs >>= print . foldr concatSpace []
                             where concatSpace a [] = a
                                   concatSpace a b = a +* " " ++ b