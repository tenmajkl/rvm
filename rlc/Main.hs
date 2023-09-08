module Main where
import Parser (parse)
import Lexer (lexString)
import Generator (generate)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

compile :: String -> String 
compile x = fromMaybe "Error" (lexString x >>= parse >>= generate)

parseArgs :: [String] -> IO () 
parseArgs [target, result] = do
    content <- readFile target
    writeFile result (compile content)

parseArgs _ = putStrLn "Error"

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
