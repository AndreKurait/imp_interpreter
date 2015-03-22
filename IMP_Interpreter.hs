module IMP_Interpreter where

import IMP_Parser

import System.Environment
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

data IType = IInt Integer
           | IBool Bool
           | IString String
           | IUndef

instance Show IType where
        show (IInt i)    = show i
        show (IBool b)   = show b
        show (IString s) = s
        show IUndef      = "undef"

type Scope  = M.Map String IType

getVariable :: Scope -> String -> IType
getVariable scope var =
    case M.lookup var scope of
         Just x -> x
         Nothing -> error $ "Variable " ++ var ++ " is not in scope"

evalAExpr :: Scope -> AExpr -> Integer
evalAExpr scope (IntConst val) = val
evalAExpr scope (Neg expr) = negate $ evalAExpr scope expr
evalAExpr scope (IVar var) = 
    case getVariable scope var of
        IInt x -> x
        IUndef -> error $ "Variable " ++ var ++ " undefined"
        _      -> error $ "Variable " ++ var ++ " is not of type Int"
evalAExpr scope (ABinary op exp1 exp2) =
    case op of
        Addition       -> evalAExpr scope exp1 + evalAExpr scope exp2
        Multiplication -> evalAExpr scope exp1 * evalAExpr scope exp2
        Subtraction    -> evalAExpr scope exp1 - evalAExpr scope exp2
        Division       -> evalAExpr scope exp1 `div` evalAExpr scope exp2

evalBExpr :: Scope -> BExpr -> Bool
evalBExpr scope (BoolConst val) = val
evalBExpr scope (Not expr) = not $ evalBExpr scope expr
evalBExpr scope (BVar var) =
    case getVariable scope var of
        IBool x -> x
        IUndef  -> error $ "Variable " ++ var ++ " undefined"
        _       -> error $ "Variable " ++ var ++ " is not of type Bool"
evalBExpr scope (BBinary op exp1 exp2) =
    case op of
        And -> evalBExpr scope exp1 && evalBExpr scope exp2
        Or  -> evalBExpr scope exp1 || evalBExpr scope exp2
evalBExpr scope (RBinary op exp1 exp2) =
    case op of
        Less    -> evalAExpr scope exp1 <  evalAExpr scope exp2
        Greater -> evalAExpr scope exp1 >  evalAExpr scope exp2
        Equal   -> evalAExpr scope exp1 == evalAExpr scope exp2

evalSExpr :: Scope -> SExpr -> String
evalSExpr scope (StringConst val) = val
evalSExpr scope (SVar var) =
    case getVariable scope var of
        IString x -> x
        IUndef    -> error $ "Variable " ++ var ++ " undefined"
        _         -> error $ "Variable " ++ var ++ " is not of type String"
evalSExpr scope (Concat exp1 exp2) = 
    evalSExpr scope exp1 ++ evalSExpr scope exp2

evalExpr :: Scope -> Expr -> IType
evalExpr scope expr =
    case expr of
        IntExpr    expr -> IInt    $ evalAExpr scope expr
        BoolExpr   expr -> IBool   $ evalBExpr scope expr
        StringExpr expr -> IString $ evalSExpr scope expr
        VarExpr    var  -> getVariable scope var

eval :: Stmt -> StateT Scope IO ()
eval Skip = return ()

eval (Chain stmts) = mapM_ eval stmts

eval (Declaration vars) =
    modify (flip (foldl (\m v -> M.insert v IUndef m)) vars)

eval (Assign var value) = do
    scope <- get
    modify (M.insert var $ evalExpr scope value)

eval (Print expr) = do
    scope <- get
    lift $ putStrLn $ show $ evalExpr scope expr

eval (If bexpr stmt1 stmt2) = do
    scope <- get
    if evalBExpr scope bexpr
        then eval stmt1
        else eval stmt2

eval while@(While bexpr body) = do
    scope <- get
    when (evalBExpr scope bexpr) $ eval body >> eval while

interpret :: String -> IO ()
interpret filename = do
    source <- readFile filename
    let program = parseSource source
    evalStateT (eval program) M.empty

main :: IO ()
main = do
    [filename] <- getArgs
    interpret filename
