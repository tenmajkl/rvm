module Lexer where
import Data.Char (isDigit, isAlphaNum, isSymbol, isPunctuation)

data Token = 
    Number Int
    | Char Char
    | OpenBracket 
    | CloseBracket
    | Word String
    | Whitespace
    deriving Show

lexWith :: String -> (Char -> Bool) -> (String -> Token) -> Maybe [Token]
lexWith text f h = let (content, rest) = span f text in 
                        lexString rest >>= (\x -> Just (h content : x))

isWordChar :: Char -> Bool
isWordChar c = c `elem` "abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*_+{}[];:,<>./?=-"

lexString :: String -> Maybe [Token]
lexString ('\'':c:'\'':rest) = lexString rest >>= (\x -> Just (Char c : x))
lexString ('(':rest) = lexString rest >>= (\x -> Just (OpenBracket : x))
lexString (')':rest) = lexString rest >>= (\x -> Just (CloseBracket : x))
lexString ('-':'-':rest) = lexString $ dropWhile (/= '\n') rest
lexString (x:rest)
    | isDigit x = lexWith (x:rest) isDigit (\y -> Number (read y :: Int))
    | isWordChar x = lexWith (x:rest) isWordChar Word
    | x == ' ' || x == '\n' = lexWith (x:rest) (\y -> y == ' ' || y == '\n') (const Whitespace)
    | otherwise = Nothing
lexString [] = Just []
