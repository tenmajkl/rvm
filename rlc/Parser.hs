module Rlc.Parser where
import Rlc.Lexer (Token)

data AST =
    NumberNode Int
    | CharNode Char
    | WordNode String
    | ListNode String [AST]

parseNumber :: [Token] -> (AST, [Token])
par
