import Data.Char

newtype MyData a = MyData {
	string :: a -> a
}

translate :: MyData String
translate = MyData (\s -> s)

-- execute :: 

main = do
	str <- getLine
	print $ string translate str