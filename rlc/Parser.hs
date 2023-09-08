module Parser where
import Lexer (Token (Number, Char, Word, OpenBracket, Whitespace, CloseBracket))
import Control.Applicative

data AST =
    NumberNode Int
    | CharNode Char
    | WordNode String
    | ListNode String [AST]
    deriving Show

type Parser = [Token] -> Maybe (AST, [Token])

parseExpression :: Parser
parseExpression x = 
    parseNumber x
    <|> parseChar x
    <|> parseWord x 
    <|> parseList x

parseNumber :: Parser 
parseNumber ((Number n):rest) = Just (NumberNode n, rest)
parseNumber _ = Nothing

parseChar :: Parser
parseChar ((Char c):rest) = Just (CharNode c, rest)
parseChar _ = Nothing

parseWord :: Parser
parseWord ((Word w):rest) = Just (WordNode w, rest)
parseWord _ = Nothing

parseList :: Parser
parseList (OpenBracket:(Word w):rest) = parseArgs rest >>= (\(nodes, rest) -> Just (ListNode w nodes, rest))
parseList (OpenBracket:Whitespace:(Word w):rest) = parseArgs rest >>= (\(nodes, rest) -> Just (ListNode w nodes, rest))
parseList (Whitespace:OpenBracket:(Word w):rest) = parseArgs rest >>= (\(nodes, rest) -> Just (ListNode w nodes, rest))
parseList (Whitespace:OpenBracket:Whitespace:(Word w):rest) = parseArgs rest >>= (\(nodes, rest) -> Just (ListNode w nodes, rest))
parseList _ = Nothing

parseArgs :: [Token] -> Maybe ([AST], [Token])
parseArgs (CloseBracket:rest) = Just ([], rest)
parseArgs (Whitespace:CloseBracket:rest) = Just ([], rest)
parseArgs (Whitespace:rest) = 
        parseExpression rest 
        >>= (\(node, rest) -> parseArgs rest >>= (\(nodes, rest) -> Just (node : nodes, rest)))
parseArgs _ = Nothing

parse :: [Token] -> Maybe [AST]
parse [] = Just []
parse x = parseExpression x >>= (\(node, rest) -> parse rest >>= (\rest -> Just (node : rest)))
