module IMP_Parser where

import IMP_Lexer

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

data ABinOp = Addition 
            | Multiplication
            | Division 
            | Subtraction
            deriving (Show)

data BBinOp = And 
            | Or 
            deriving (Show)

data RBinOp = Less
            | Greater
            | Equal
            deriving (Show)

data AExpr = IntConst Integer
           | IVar String
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data BExpr = BoolConst Bool
           | BVar String
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving (Show)

data SExpr = StringConst String
           | SVar String
           | Concat SExpr SExpr
           deriving (Show)

data Expr = IntExpr    AExpr 
          | BoolExpr   BExpr
          | StringExpr SExpr
          | VarExpr    String
          deriving (Show)

data Stmt = Skip
          | Assign String Expr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Print Expr
          | Chain [Stmt]
          | Declaration [String]
          deriving (Show)

---------------------------------------------
-- Parser starts here
---------------------------------------------

statement :: Parser Stmt
statement = parens statement <|> chainStatements

statement' :: Parser Stmt
statement' =  ifStmt 
          <|> whileStmt 
          <|> skipStmt 
          <|> assignStmt 
          <|> printStmt 
          <|> declarationStmt

chainStatements :: Parser Stmt
chainStatements = do
        list <- sepBy1 statement' semi
        return $ if length list == 1 then head list else Chain list

ifStmt :: Parser Stmt
ifStmt = do
        reserved "if"
        cond <- bExpr
        reserved "then"
        stmt1 <- statement
        reserved "else"
        stmt2 <- statement
        return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
        reserved "while"
        cond <- bExpr
        reserved "do"
        stmt <- statement
        return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt = do
        var <- identifier
        reservedOp ":="
        expr <- anyExpr
        return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

declarationStmt :: Parser Stmt
declarationStmt = do
        reserved "var"
        vars <- sepBy1 identifier comma
        return $ Declaration vars

printStmt :: Parser Stmt
printStmt = do
        reserved "print" 
        value <- parens anyExpr
        return $ Print value

anyExpr :: Parser Expr
anyExpr =  liftM VarExpr identifier
       <|> try (liftM IntExpr    aExpr) 
       <|> try (liftM BoolExpr   bExpr)
       <|> try (liftM StringExpr sExpr)

bExpr :: Parser BExpr
bExpr = buildExpressionParser bOperators bTerm

aExpr :: Parser AExpr
aExpr = buildExpressionParser aOperators aTerm

sExpr :: Parser SExpr
sExpr = buildExpressionParser sOperators sTerm

rExpr :: Parser BExpr
rExpr = do
        a1 <- aExpr
        op <- relation
        a2 <- aExpr
        return $ RBinary op a1 a2

relation :: Parser RBinOp
relation =  (reservedOp "<" >> return Less)
        <|> (reservedOp ">" >> return Greater)
        <|> (reservedOp "=" >> return Equal)

aOperators = [ [ Prefix (reservedOp "-" >> return Neg) ]
             , [ Infix  (reservedOp "*" >> return (ABinary Multiplication)) AssocLeft
             ,   Infix  (reservedOp "/" >> return (ABinary Division)) AssocLeft ]
             , [ Infix  (reservedOp "+" >> return (ABinary Addition)) AssocLeft 
             ,   Infix  (reservedOp "-" >> return (ABinary Subtraction)) AssocLeft ]
             ]

bOperators = [ [ Prefix (reservedOp "not" >> return Not) ]
             , [ Infix  (reservedOp "and" >> return (BBinary And)) AssocLeft ]
             , [ Infix  (reservedOp "or" >> return (BBinary Or)) AssocLeft ]
             ]

sOperators = [ [ Infix (reservedOp "++" >> return Concat) AssocLeft ]
             ]


aTerm :: Parser AExpr
aTerm = parens aExpr <|> liftM IVar identifier <|> liftM IntConst integer

sTerm :: Parser SExpr
sTerm = parens sExpr <|> liftM SVar identifier <|> liftM StringConst stringLiteral

bTerm :: Parser BExpr
bTerm =   parens bExpr 
     <|> (reserved "true"  >> return (BoolConst True))
     <|> (reserved "false" >> return (BoolConst False))
     <|> try rExpr
     <|> liftM BVar identifier
     
---------------------------------------

parseSource :: String -> Stmt
parseSource code = 
        case parse whileParser "" code of
            Left e -> error $ show e
            Right r -> r
    where
        whileParser = whiteSpace >> statement
