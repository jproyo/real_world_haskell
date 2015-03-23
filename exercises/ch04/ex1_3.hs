-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)

headOrDefault :: [a] -> a -> a
headOrDefault [] a = a
headOrDefault (x:xs) _ = x

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

firstWords input = unlines (fisrtWordsLines (lines input))
	where 
		fisrtWordsLines :: [String] -> [String]
		fisrtWordsLines [] = []
		fisrtWordsLines (x:xs) = (headOrDefault (words x) "") : fisrtWordsLines xs

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = firstWords

