
module IMP_Parser where

data ABinOp = Addition 
            | Multiplication
            | Division 
            | Subtraction
            deriving (Show)

data BBinOp = And 
            | Or 
            deriving (Show)

data AExpr = IntConst Int
           | Var String
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary BBinOp AExpr AExpr
           deriving (Show)

data Value = IntValue AExpr
           | BoolValue BExpr
           | StringValue String
           deriving (Show)


data Stmt = Skip
          | Assign String Value
          | Chain Stmt Stmt
          | IfExpr BExpr Stmt Stmt
          | While BExpr Stmt
          deriving (Show)

data Prog = Prog [String] Stmt 
          deriving (Show)
