module Ast where

type Program = [Stmt]


data Stmt = Assign String Expr | Def String [String] Stmt
          | Block [Stmt] | Break | Continue | Print String | Return Expr
          | While BExpr Stmt | If BExpr Stmt | IfElse BExpr Stmt Stmt
          deriving (Eq,Show)

data BExpr = Or BExpr BExpr | And BExpr BExpr | Not BExpr
           | Eq Expr Expr | Ne Expr Expr | Lt Expr Expr | Gt Expr Expr | Le Expr Expr | Ge Expr Expr
           deriving (Eq,Show)

data Expr = Plus Expr Expr | Minus Expr Expr | Uminus Expr
          | Mult Expr Expr | Div Expr Expr | Mod Expr Expr
          | ValInt Integer | Var String | ValFloat Integer Integer | Call String Expr
          deriving (Eq,Show)



prettyShow :: Program -> String
prettyShow [] = ""
prettyShow [x] = prettyStmt x
prettyShow (x:xs) = (prettyStmt x) ++ (prettyShow xs)


prettyStmt :: Stmt -> String
prettyStmt (Assign s e) = s ++ "=" ++ (prettyExpr e) ++ ";"
prettyStmt (Def n a s) = "def " ++ n ++ "(" ++ concat(a) ++ ")" ++ "{"++ (prettyStmt s) ++ "}"
prettyStmt (Block []) = ""
prettyStmt (Block (x:xs)) = "  " ++ (prettyStmt x) ++ (prettyStmt (Block xs))
prettyStmt (Break) = "break" ++ ";"
prettyStmt (Continue) = "continue" ++ ";"
prettyStmt (Print s) = "print " ++ s ++ ";"
prettyStmt (Return e) = "return " ++ (prettyExpr e) ++ ";"
prettyStmt (While b s) = "while " ++ "(" ++ (prettyBExpr b) ++ ")" ++ " {" ++ (prettyStmt s) ++ " }"
prettyStmt (If b s) = "if " ++ "(" ++ (prettyBExpr b) ++ ")" ++ " {" ++ (prettyStmt s) ++ " }"
prettyStmt (IfElse b t e) = "if " ++ "(" ++ (prettyBExpr b) ++ ")" ++ " {" ++ (prettyStmt t) ++ " }"
                            ++ "else" ++ " {" ++ (prettyStmt e) ++ " }"


prettyExpr :: Expr -> String
prettyExpr (ValInt i) = show i
prettyExpr (ValFloat a b) = show a ++ "." ++ show b
prettyExpr (Var s) = s
prettyExpr (Call s e) = s ++ "(" ++ (prettyExpr e) ++ ")"
prettyExpr (Uminus e) = "(" ++ "-" ++ (prettyExpr e) ++ ")"
prettyExpr (Plus l r) = "(" ++ (prettyExpr l) ++ "+" ++ (prettyExpr r) ++ ")"
prettyExpr (Minus l r) = "(" ++ (prettyExpr l) ++ "-" ++ (prettyExpr r) ++ ")"
prettyExpr (Mult l r) = "(" ++ (prettyExpr l) ++ "*" ++ (prettyExpr r) ++ ")"
prettyExpr (Div l r) = "(" ++ (prettyExpr l) ++ "/" ++ (prettyExpr r) ++ ")"
prettyExpr (Mod l r) = "(" ++ (prettyExpr l) ++ "%" ++ (prettyExpr r) ++ ")"


prettyBExpr :: BExpr -> String
prettyBExpr (Eq l r) = (prettyExpr l) ++ "==" ++ (prettyExpr r)
prettyBExpr (Ne l r) = (prettyExpr l) ++ "!=" ++ (prettyExpr r)
prettyBExpr (Lt l r) = (prettyExpr l) ++ "<" ++ (prettyExpr r)
prettyBExpr (Gt l r) = (prettyExpr l) ++ ">" ++ (prettyExpr r)
prettyBExpr (Le l r) = (prettyExpr l) ++ "<=" ++ (prettyExpr r)
prettyBExpr (Ge l r) = (prettyExpr l) ++ ">=" ++ (prettyExpr r)
prettyBExpr (Or l r) = (prettyBExpr l) ++ " || " ++ (prettyBExpr r)
prettyBExpr (And l r) = (prettyBExpr l) ++ " && " ++ (prettyBExpr r)
prettyBExpr (Not b) =  "!" ++ (prettyBExpr b)
