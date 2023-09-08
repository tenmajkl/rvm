module Main where
import Parser (parse)
import Lexer (lexString)
import Generator (generate)

main :: IO ()
main = print (lexString "(let parek int 10) (let rizek int parek) (out rizek)" >>= parse >>= generate)
