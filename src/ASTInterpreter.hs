module ASTInterpreter where

import Prelude hiding (lookup)
import Ast
import StatefulUnsafeMonad
import CParser
import TestsProject


type State = [(String,Val)]


data Val = I Integer | S Stmt | F ([String], Val) | P Integer | E String deriving (Eq,Show)


eval :: Program -> [String]
eval p = getPrints $ snd $ app (eval' p) []


getPrints :: State -> [String]
getPrints [] = []
getPrints ((s,v):xs) = case v of (P p) -> (getPrints xs) ++ [s ++ " = " ++ (show p)]
                                 _ -> getPrints xs


eval' :: Program -> StatefulUnsafe State Integer
eval' [] = err "not a program"
eval' [x] = evalStep x
eval' (x:xs) = do evalStep x
                  eval' xs


evalStep :: Stmt -> StatefulUnsafe State Integer
evalStep (Assign s e) = do e' <- evalExpr e
                           withVal s (I e') (evalStep Break)
evalStep (Def "main" [] s) = evalStep s
evalStep (Def n a s) = withVal n (F (a, (S s))) (evalStep Break)
evalStep (Block s) = eval' s
evalStep Break = return 0
evalStep Continue = return 0
evalStep (Print s) = do v <- (evalExpr (Var s))
                        withVal s (P v) (evalStep Break)
evalStep (Return e) = do e' <- evalExpr e
                         deleteVal (evalStep Break)
                         return e'
evalStep (While b s) = do b' <- evalBExpr b
                          if b' then
                            do evalStep s
                               evalStep (While b s)
                          else
                            evalStep Break
evalStep (If b s) = do b' <- evalBExpr b
                       if b' then
                         evalStep s
                       else
                         evalStep Break
evalStep (IfElse b t e) = do b' <- evalBExpr b
                             if b' then
                               evalStep t
                             else
                               evalStep e


-- insert a string and its val to a state
insert :: String -> Val -> State -> State
insert var val s = [(var,val)] ++ s


-- look for a string and its val in a state
lookup :: String -> State -> Val
lookup var [] = E ("no variable named " ++ var)
lookup var ((k,v):xs) | var == k = v
                      | otherwise = lookup var xs
                      
                      
-- delete a string and its val from a state
delete :: State -> State
delete [] = []
delete (x:xs) = case x of (_, (I _)) -> xs
                          _ -> x:xs


-- find a val in the environment
valOf :: String -> StatefulUnsafe State Val
valOf var = StatefulUnsafe (\s -> case (lookup var s) of
                                    (E e) -> ((Error e), s)
                                    x -> (Ok x, s))


-- add a val into the environment
withVal :: String -> Val -> StatefulUnsafe State a -> StatefulUnsafe State a
withVal var i comp = case comp of (StatefulUnsafe eu) -> StatefulUnsafe (\e -> eu (insert var i e))


-- delete a val from the environment
deleteVal :: StatefulUnsafe State a -> StatefulUnsafe State a
deleteVal comp = case comp of (StatefulUnsafe eu) -> StatefulUnsafe (\e -> eu (delete e))


-- helper functions for eval
evalExpr :: Expr -> StatefulUnsafe State Integer
evalExpr (ValInt i) = return i
evalExpr (ValFloat a b) = return a
evalExpr (Var s) = do s' <- valOf s
                      case s' of (I i) -> return i
                                 (P p) -> return p
                                 _ -> err "not a value for variable"
evalExpr (Call n e) = do e' <- evalExpr e
                         (F ([a], S s)) <- valOf n
                         withVal a (I e') (evalStep s)          
evalExpr (Plus l r) = do l' <- evalExpr l
                         r' <- evalExpr r
                         return (l' + r')
evalExpr (Minus l r) = do l' <- evalExpr l
                          r' <- evalExpr r
                          return (l' - r')
evalExpr (Mult l r) = do l' <- evalExpr l
                         r' <- evalExpr r
                         return (l' * r')
evalExpr (Div l r) = do l' <- evalExpr l
                        r' <- evalExpr r
                        return (l' `div` r')
evalExpr (Mod l r) = do l' <- evalExpr l
                        r' <- evalExpr r
                        return (l' `mod` r')
evalExpr (Uminus e) = do e' <- evalExpr e
                         return (-e')


evalBExpr :: BExpr -> StatefulUnsafe State Bool
evalBExpr (Or l r) = do l' <- evalBExpr l
                        r' <- evalBExpr r
                        return (l' || r')
evalBExpr (And l r) = do l' <- evalBExpr l
                         r' <- evalBExpr r
                         return (l' && r')
evalBExpr (Not b) = do b' <- evalBExpr b
                       return (not b')
evalBExpr (Eq l r) = do l' <- evalExpr l
                        r' <- evalExpr r
                        return (l' == r')
evalBExpr (Ne l r) = do l' <- evalExpr l
                        r' <- evalExpr r
                        return (l' /= r')
evalBExpr (Lt l r) = do l' <- evalExpr l
                        r' <- evalExpr r
                        return (l' < r')
evalBExpr (Gt l r) = do l' <- evalExpr l
                        r' <- evalExpr r
                        return (l' > r')
evalBExpr (Le l r) = do l' <- evalExpr l
                        r' <- evalExpr r
                        return (l' <= r')
evalBExpr (Ge l r) = do l' <- evalExpr l
                        r' <- evalExpr r
                        return (l' >= r')