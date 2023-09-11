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
type Whiles = Int

type Context = ([Variable], Ifs, Whiles)

type Generator = AST -> Context -> Maybe (String, Context)

generate :: [AST] -> Maybe String
generate x = do 
    statements <- generateStatements x ([], 0, 0)
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
generateExpression (WordNode w) (v, _, _) reg = do 
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

generateBlock :: [AST] -> Context -> Maybe (String, Context)
generateBlock [] x = Just ("", x)
generateBlock (x:xs) y = do
    (statement, ctx) <- generateStatement x y 
    (rest, new_ctx) <- generateBlock xs ctx
    return (statement ++ rest, new_ctx)

generateVariableAddress :: AST -> [Variable] -> Int ->  Maybe String 
generateVariableAddress (WordNode w) vs reg = do 
    (_, address, t) <- find (\(x, _, _) -> x == w) vs
    return ("srv " ++ show reg ++ " " ++ show address ++ "\n")
generateVariableAddress (ListNode "@" [e]) vs reg = do
    e <- generateVariableAddress e vs 4
    return (e ++ "get 4 " ++ show reg ++ "\n")
generateVariableAddress _ _ _ = Nothing 


-- todo better conversion
generateList :: Generator 
generateList (ListNode "let" [WordNode x, WordNode y, z]) (variables, ifs, whiles) = do 
    t <- wordToType y
    e <- generateExpression z (variables, ifs, whiles) 5
    return (e ++ "srv 4 1\nset 0 5\nadd 0 4 0\n", ((x, length variables, t) : variables, ifs, whiles))
generateList (ListNode "set" [x, y]) (variables, ifs, whiles) = do 
    e <- generateExpression y (variables, ifs, whiles) 5 
    addr <- generateVariableAddress x variables 4
    return (e ++ addr ++ "set 4 5\n", (variables, ifs, whiles))
generateList (ListNode "out" [x]) variables = do 
    e <- generateExpression x variables 5
    return (e ++ "sys 0 5\n", variables)
generateList (ListNode "putchar" [x]) variables = do 
    e <- generateExpression x variables 5
    return (e ++ "sys 1 5\n", variables)
generateList (ListNode "+" [x, y]) variables = generateOperator x y "add" variables
generateList (ListNode "-" [x, y]) variables = generateOperator x y "sub" variables
generateList (ListNode "*" [x, y]) variables = generateOperator x y "mul" variables
generateList (ListNode "/" [x, y]) variables = generateOperator x y "div" variables
generateList (ListNode "|" [x, y]) variables = generateOperator x y "oor" variables
generateList (ListNode "&" [x, y]) variables = generateOperator x y "and" variables
generateList (ListNode "^" [x, y]) variables = generateOperator x y "xor" variables
generateList (ListNode "=" [x, y]) variables = generateOperator x y "equ" variables
generateList (ListNode "!" [x]) ctx = do 
    expr <- generateExpression x ctx 5 
    return (expr ++ "srv 4 1\nxor 5 4 5\n", ctx)
generateList (ListNode "if" [x, y, z]) (variables, ifs, whiles) = do
    e <- generateExpression x (variables, ifs, whiles) 5
    (true, (true_variables, ifs, whiles)) <- generateStatement y (variables, ifs, whiles)  
    (false, (false_variables, ifs, whiles)) <- generateStatement z (variables, ifs, whiles)  
    return (e ++ "jrz 5 _else" ++ show ifs ++ "\n" ++ true ++ generateStackCleaning variables true_variables ++ "jmp _end" ++ show ifs ++ "\n_else" ++ show ifs ++ ":\n" ++ false ++ generateStackCleaning variables false_variables ++ "_end" ++ show ifs ++ ":\n", (variables, ifs + 1, whiles))
generateList (ListNode "while" [x, y]) (variables, ifs, whiles) = do 
    e <- generateExpression x (variables, ifs, whiles) 5
    (code, (new_variables, ifs, whiles)) <- generateStatement y (variables, ifs, whiles)
    return ("_while" ++ show whiles ++ ":\n" ++ e ++ "jrz 5 _endwhile" ++ show whiles ++ "\n" ++ code ++ "jmp _while" ++ show whiles ++ "\n_endwhile"++ show whiles ++ ":\n" ++ generateStackCleaning variables new_variables, (variables, ifs, whiles + 1))
generateList (ListNode "{-}" x) (variables, ifs, whiles) = do 
    (code, (new_variables, new_ifs, new_whiles)) <- generateBlock x (variables, ifs, whiles) 
    return (code, (variables, new_ifs, new_whiles))
generateList (ListNode "$" [w]) (variables, ifs, whiles) = do
    v <- generateVariableAddress w variables 5
    return (v, (variables, ifs, whiles))
generateList _ _ = Nothing

-- Constant for code that provides popping
generatePop :: String 
generatePop = "srv 5 1\nsub 0 5 0\n"

generateStackCleaning :: [Variable] -> [Variable] -> String
generateStackCleaning variables new_variables = foldr (++) "" (map (const generatePop) [0..(length new_variables - length variables)])

wordToType :: String -> Maybe Type
wordToType "int" = Just Int
wordToType "char" = Just Char
wordToType _ = Nothing 
