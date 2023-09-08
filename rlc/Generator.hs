module Generator where
import Parser (AST (ListNode, NumberNode, WordNode, CharNode))
import Data.Char (ord)
import Data.Foldable (find)

data Type = 
    Int
    | Char
    deriving Eq

type Variable = (String, Int, Type)

type Generator = AST -> [Variable] -> Maybe (String, [Variable])

generate :: [AST] -> Maybe String
generate x = do 
    statements <- generateStatements x []
    return ("srv 0 0\nsrv 1 0\n" ++ statements ++ "hlt")

generateStatements :: [AST] -> [Variable] -> Maybe String
generateStatements [] y = Just ""
generateStatements (x:xs) y = do 
    (statement, v) <- generateStatement x y
    statements <- generateStatements xs v
    return (statement ++ statements)

generateStatement :: Generator 
generateStatement (ListNode name args) variables = generateList (ListNode name args) variables
generateStatement _ _ = Nothing

-- basicaly our goal is to convert any expression to string of statements that result to setting numeric value to register 4 which is variable input
generateExpression :: AST -> [Variable] -> Int -> Maybe String
generateExpression (NumberNode n) v reg = Just ("srv " ++ show reg ++ " " ++ show n ++ "\n")
generateExpression (CharNode c) v reg = Just ("srv " ++ show reg ++ " " ++ show (ord c) ++ "\n")
generateExpression (WordNode w) v reg = do 
    (_, address, t) <- find (\(x, _, _) -> x == w) v
    return ("srv 6 " ++ show address ++ "\nadd 1 6 4\nget 4 " ++ show reg ++ "\n")
generateExpression _ _ _ = Nothing

-- todo better conversion
generateList :: Generator 
generateList (ListNode "let" [WordNode x, WordNode y, z]) variables = do 
    t <- wordToType y
    e <- generateExpression z variables 5
    return (e ++ "srv 4 1\nset 0 5\nadd 0 4 0\n", (x, length variables, t) : variables)
generateList (ListNode "out" [x]) variables = do 
    e <- generateExpression x variables 5
    return (e ++ "out 5\n", variables)
generateList _ _ = Nothing

-- Constant for code that provides popping
generatePop :: String 
generatePop = "srv 5 1\nsub 0 5 0"

wordToType :: String -> Maybe Type
wordToType "int" = Just Int
wordToType "char" = Just Char
wordToType _ = Nothing 
