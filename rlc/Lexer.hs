module Rlc.Lexer where
import Data.Char (isDigit, isAlphaNum, isSymbol)

data Token = 
    Number Int
    | Char Char
    | OpenBracket 
    | CloseBracket
    | Word String
    | Whitespace

lexWith :: String -> (Char -> Bool) -> (String -> Token) -> Maybe [Token]
lexWith text f h = let (content, rest) = span f text in 
                        lexString rest >>= (\x -> Just (x ++ [h content]))
                

lexString :: String -> Maybe [Token]
lexString ('\'':c:'\'':rest) = lexString rest >>= (\x -> Just (x ++ [Char c]))
lexString ('(':rest) = lexString rest >>= (\x -> Just (x ++ [OpenBracket]))
lexString (')':rest) = lexString rest >>= (\x -> Just (x ++ [CloseBracket]))
lexString (x:rest)
    | isDigit x = lexWith (x:rest) isDigit (\y -> Number (read y :: Int))
    | isSymbol x || isAlphaNum x = lexWith (x:rest) (\y -> isSymbol y || isAlphaNum y) Word
    | x == ' ' || x == '\n' = lexWith (x:rest) (\y -> y == ' ' || y == '\n') (const Whitespace)
    | otherwise = Nothing
