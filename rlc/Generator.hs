module Generator where
import Parser (AST (ListNode, NumberNode, WordNode, CharNode))
import Data.Char (ord)
import Data.Foldable (find)

data Type = 
    Int
    | Char
    deriving Eq

type Variable = (String, Int, Type)
type Ifs = Int

type Context = ([Variable], Ifs)

type Generator = AST -> Context -> Maybe (String, Context)

generate :: [AST] -> Maybe String
generate x = do 
    statements <- generateStatements x ([], 0)
    return ("srv 0 0\nsrv 1 0\n" ++ statements ++ "hlt")

generateStatements :: [AST] -> Context -> Maybe String
generateStatements [] y = Just ""
generateStatements (x:xs) y = do 
    (statement, v) <- generateStatement x y
    statements <- generateStatements xs v
    return (statement ++ statements)

generateStatement :: Generator 
generateStatement (ListNode name args) variables = generateList (ListNode name args) variables
generateStatement _ _ = Nothing

-- basicaly our goal is to convert any expression to string of statements that result to setting numeric value to register 4 which is variable input
generateExpression :: AST -> Context -> Int -> Maybe String
generateExpression (NumberNode n) v reg = Just ("srv " ++ show reg ++ " " ++ show n ++ "\n")
generateExpression (CharNode c) v reg = Just ("srv " ++ show reg ++ " " ++ show (ord c) ++ "\n")
generateExpression (WordNode w) (v, _) reg = do 
    (_, address, t) <- find (\(x, _, _) -> x == w) v
    return ("srv 4 " ++ show address ++ "\nadd 1 4 4\nget 4 " ++ show reg ++ "\n")
generateExpression x v reg = do
    (expr, _) <- generateList x v
    return (expr ++ "add 5 15 " ++ show reg ++ "\n")

generateOperator :: AST -> AST -> String -> Context -> Maybe (String, Context)
generateOperator x y instruction variables = do
    x <- generateExpression x variables 6 
    y <- generateExpression y variables 7
    return (x ++ y ++ instruction ++ " 6 7 5\n", variables)

-- todo better conversion
generateList :: Generator 
generateList (ListNode "let" [WordNode x, WordNode y, z]) (variables, ifs) = do 
    t <- wordToType y
    e <- generateExpression z (variables, ifs) 5
    return (e ++ "srv 4 1\nset 0 5\nadd 0 4 0\n", ((x, length variables, t) : variables, ifs))
generateList (ListNode "out" [x]) variables = do 
    e <- generateExpression x variables 5
    return (e ++ "out 5\n", variables)
generateList (ListNode "putchar" [x]) variables = do 
    e <- generateExpression x variables 5
    return (e ++ "cht 5\n", variables)
generateList (ListNode "+" [x, y]) variables = generateOperator x y "add" variables
generateList (ListNode "-" [x, y]) variables = generateOperator x y "sub" variables
generateList (ListNode "*" [x, y]) variables = generateOperator x y "mul" variables
generateList (ListNode "/" [x, y]) variables = generateOperator x y "div" variables
generateList (ListNode "|" [x, y]) variables = generateOperator x y "oor" variables
generateList (ListNode "&" [x, y]) variables = generateOperator x y "and" variables
generateList (ListNode "^" [x, y]) variables = generateOperator x y "xor" variables
generateList (ListNode "=" [x, y]) variables = generateOperator x y "sub" variables
generateList (ListNode "if" [x, y, z]) (variables, ifs) = do
    e <- generateExpression x (variables, ifs) 5
    (true, _) <- generateStatement y (variables, ifs)  
    (false, _) <- generateStatement z (variables, ifs)  
    return (e ++ "jrz 5 _else" ++ show ifs ++ "\n" ++ true ++ "jmp _end" ++ show ifs ++ "\n_else" ++ show ifs ++ ":\n" ++ false ++ "_end" ++ show ifs ++ ":\n", (variables, ifs + 1))
generateList _ _ = Nothing

-- Constant for code that provides popping
generatePop :: String 
generatePop = "srv 5 1\nsub 0 5 0"

wordToType :: String -> Maybe Type
wordToType "int" = Just Int
wordToType "char" = Just Char
wordToType _ = Nothing 
