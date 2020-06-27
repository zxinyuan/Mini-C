module CParser where

import Ast
import ParserMonad


getPro :: Parser Program -> String -> Program
getPro (Parser pa) = \s -> case (pa s) of (Just (p, "")) -> p
                                          _ -> []


parser :: Parser Program
parser = do f <- rep statementParser
            return f


keywords = ["if","then","else","while","break","print","return","true","false"]


vars :: Parser Expr
vars = do s <- token $ varParser
          if s `elem` keywords
            then failParse
          else return $ Var s


ints :: Parser Expr
ints = do x <- intParser
          return (ValInt x)


floats :: Parser Expr
floats = do x <- intParser
            token $ literal "."
            y <- intParser
            return (ValFloat x y)
          
 
uminus :: Parser Expr
uminus = do token $ literal "-"
            x <- atoms
            return (Uminus x)


callExpr :: Parser Expr
callExpr = do n <- varParser
              token $ literal "("
              a <- callExpr <||> addSubExpr
              token $ literal ")"
              return (Call n a)


addSubExpr :: Parser Expr
addSubExpr = withInfix multDivExpr [("+",Plus),("-",Minus)]


multDivExpr :: Parser Expr
multDivExpr = withInfix atoms [("*",Mult),("/",Div),("%",Mod)]


atoms:: Parser Expr
atoms = parens <||> callExpr <||> uminus <||> floats <||> ints <||> vars

condParser = eqExp <||> neExp <||> ltExp <||> gtExp <||> leExp <||> geExp


eqExp :: Parser BExpr
eqExp = do l <- addSubExpr
           token $ literal "=="
           r <- addSubExpr
           return (Eq l r)
        <||>
        do token $ literal "("
           l <- addSubExpr
           token $ literal "=="
           r <- addSubExpr
           token $ literal ")"
           return (Eq l r)


neExp :: Parser BExpr
neExp = do l <- addSubExpr
           token $ literal "!="
           r <- addSubExpr
           return (Ne l r)
        <||>
        do token $ literal "("
           l <- addSubExpr
           token $ literal "!="
           r <- addSubExpr
           token $ literal ")"
           return (Ne l r)


ltExp :: Parser BExpr
ltExp = do l <- addSubExpr
           token $ literal "<"
           r <- addSubExpr
           return (Lt l r)
        <||>
        do token $ literal "("
           l <- addSubExpr
           token $ literal "<"
           r <- addSubExpr
           token $ literal ")"
           return (Lt l r)


gtExp :: Parser BExpr
gtExp = do l <- addSubExpr
           token $ literal ">"
           r <- addSubExpr
           return (Gt l r)
        <||>
        do token $ literal "("
           l <- addSubExpr
           token $ literal ">"
           r <- addSubExpr
           token $ literal ")"
           return (Gt l r)


leExp :: Parser BExpr
leExp = do l <- addSubExpr
           token $ literal "<="
           r <- addSubExpr
           return (Le l r)
        <||>
        do token $ literal "("
           l <- addSubExpr
           token $ literal "<="
           r <- addSubExpr
           token $ literal ")"
           return (Le l r)


geExp :: Parser BExpr
geExp = do l <- addSubExpr
           token $ literal ">="
           r <- addSubExpr
           return (Ge l r)
        <||>
        do token $ literal "("
           l <- addSubExpr
           token $ literal ">="
           r <- addSubExpr
           token $ literal ")"
           return (Ge l r)


orExpr :: Parser BExpr
orExpr = withInfix (andExpr <||> notExp <||> condParser)  [("||",Or)]


andExpr :: Parser BExpr
andExpr = withInfix (notExp <||> condParser) [("&&",And)]


notExp :: Parser BExpr
notExp = do token $ literal "!"
            a <- condParser
            return (Not a)


bools :: Parser BExpr
bools = orExpr <||> andExpr <||> notExp <||> condParser


assignParser :: Parser Stmt
assignParser = do v <- varParser
                  token $ literal "="
                  i <- addSubExpr <||> callExpr
                  token $ literal ";"
                  return (Assign v i)


breakParser :: Parser Stmt
breakParser = do token $ literal "break"
                 token $ literal ";"
                 return Break
                 
                 
conParser :: Parser Stmt
conParser = do token $ literal "continue"
               token $ literal ";"
               return Continue


printParser :: Parser Stmt
printParser = do token $ literal "print"
                 token $ literal "("
                 p <- varParser
                 token $ literal ")"
                 token $ literal ";"
                 return (Print p)
              <||>
              do token $ literal "print"
                 p <- varParser
                 token $ literal ";"
                 return (Print p)


returnParser :: Parser Stmt
returnParser = do token $ literal "return"
                  r <- addSubExpr
                  token $ literal ";"
                  return (Return r)


statementParser :: Parser Stmt
statementParser = ifElseParser <||> ifParser <||> whileParser <||> funcParser
                <||> assignParser <||> breakParser <||> conParser <||> printParser <||> returnParser

ifParser :: Parser Stmt
ifParser = do token $ literal "if"
              token $ literal "("
              b <- bools
              token $ literal ")"
              token $ literal "{"
              t <- rep statementParser
              token $ literal "}"
              return (If b (Block t))


ifElseParser :: Parser Stmt
ifElseParser = do token $ literal "if"
                  token $ literal "("
                  b <- bools
                  token $ literal ")"
                  token $ literal "{"
                  t <- rep statementParser
                  token $ literal "}"
                  token $ literal "else"
                  token $ literal "{"
                  e <- rep statementParser
                  token $ literal "}"
                  return (IfElse b (Block t) (Block e))


whileParser :: Parser Stmt
whileParser = do token $ literal "while"
                 token $ literal "("
                 b <- bools
                 token $ literal ")"
                 token $ literal "{"
                 a <- rep statementParser
                 token $ literal "}"
                 return (While b (Block a))


parens :: Parser Expr
parens = do token $ literal "("
            e <- addSubExpr
            token $ literal ")"
            return e


funcParser :: Parser Stmt
funcParser = do token $ literal "def"
                n <- varParser
                token $ literal "("
                a <- rep varParser
                token $ literal ")"
                token $ literal "{"
                s <- rep statementParser
                token $ literal "}"
                return (Def n a (Block s))
             <||>
             do token $ literal "def"
                n <- varParser
                token $ literal "("
                token $ literal ")"
                token $ literal "{"
                s <- rep statementParser
                token $ literal "}"
                return (Def n [] (Block s))
