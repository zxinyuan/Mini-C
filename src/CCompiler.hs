module CCompiler where

import Ast
import CParser
import ICInterpreter
import TestsProject


-- getPro: to see the Ast after parsing (source code in CParser.hs)
-- compile: to see the IC_Program after compiling (implemented here)
-- execute: to see the result after interpreting (source code in ICInterpreter.hs)


-- temporaries
type Temp = [String]


allTemp :: Temp
allTemp = ["_t" ++ (show i) | i <- [1..20]]


getTemp :: Temp -> String
getTemp [] = "No regs available"
getTemp (x:xs) = x


deleteTemp :: String -> Temp -> Temp
deleteTemp _ [] = []
deleteTemp _ (x:xs) = xs


freeTemp :: String -> Temp -> Temp
freeTemp s t = if (s /= getTemp t) then s:t else t

--  current address, [(name of the vairable in Def, ICP, [(name of the func, start address of func)])]
type Scope = (Int,[(String, IC_Program, [(String,Int)])])

scope :: Scope
scope = (0,[("",[],[("",0)])])

compile :: Program -> IC_Program
compile [] = []
compile p = compile' scope p

compile' :: Scope -> Program -> IC_Program
compile' _ [] = []
compile' ss (x:xs) = case (compS ss x) of (a, (c,[(s,ic,f)])) -> a ++ (compile' (c,[(s,ic,f)]) xs)

compS :: Scope -> Stmt -> (IC_Program, Scope)
compS (c,[(n,ic,f)]) (Def "main" [] s) = case (compS (c+3,[(n,ic,f)]) s) of (a, (c,[(ns,icb,cb)])) -> ([(Push'), (Call' (length (ic) +3)) , (Halt')]++ic++a, (c+3,[(ns,icb,cb)]))
compS (c,f) (Def s a bod) = let (ic, (cs,[(ns,icb,cb)])) = compS (c+3,f) bod
                                       in ([], (cs-3,([((head a),icb,[(s,(c+3))]++cb)])))
compS (c,[(n,ic,f)]) (Block []) = ([], (c,[(n,ic,f)]))
compS (c,[(n,ic,f)]) (Block (x:xs)) = let (ic1, (c1,[(ns1,ics1,fs1)])) = (compS (c,[(n,ic,f)]) x)  ------Where do we update the function start addres? and how? also remember to change the (Call' 3) to fx after modified.
                                      in let (ic2, (c2,[(ns2,ics2,fs2)])) = (compS (c1,[(ns1,ics1,(fs1))]) (Block xs))
                                         in ((ic1 ++ ic2), (c2,[(n,ics1++ic1++ic2,fs2)]))
compS (c,[(n,ic',f)]) (Assign s e) = let (ic, t, _, (cs,fs)) =  (compE allTemp (c,[(n,ic',f)]) e)
                                     in ((ic ++ [(Assign' (Var' s) t)]), (cs+1,fs))
compS (c,[(n,ic,f)]) (Print s) = ([(Print' (s ++ " = ") (Var' s))], (c+1,[(n,ic,f)]))

compS (c,[(n,ic',f)]) (Return e) = let (ic, t, _, (cs,[(ns,ics,fs)])) = (compE allTemp (c,[(n,ic',f)]) e)
                                   in (ic ++ [(Return' t)], (cs+1,[(ns,ics,fs)]))
compS (c,[(n,ic,f)]) (Continue) = ([(Jump' (c+2))],(c+1,[(n,ic,f)]))
compS (c,[(n,ic,f)]) (Break) = ([(Jump' (c+2))],(c+1,[(n,ic,f)]))
compS (c,[(n,ic,f)]) (While b bod) = let (icb, t, _, (cb,fb)) = compBE allTemp (c,[(n,ic,f)]) b
                                     in let (ics, (cs,fs)) = compS (cb+2,fb) bod
                                        in ((icb ++ [(Bzero' t (cs+1)) , (Jump' (cb+2))] ++ ics ++[(Jump' (c))]), (cs+1,fs))
compS (c,[(n,ic,f)]) (If b bod) = let (icb, t, _, (cb,fb)) = compBE allTemp (c,[(n,ic,f)]) b
                                  in let (ics, (cs,fs)) = compS (cb+2,fb) bod
                                     in ((icb ++ [(Bzero' t (cs)) , (Jump' (cb+2))] ++ ics), (cs,fb))
compS (c,[(n,ic,f)]) (IfElse b th el) = let (icb, t, _, (cb,fb)) = compBE allTemp (c,[(n,ic,f)]) b
                                        in let (ics1, (cs1,fs1)) = compS (cb+2,fb) th
                                           in let (ics2, (cs2,fs2)) = compS (cs1+1,fs1) el
                                              in ((icb ++ [(Bzero' t (cs1+1)) , (Jump' (cb+2))] ++ ics1 ++ [(Jump' (cs2))] ++ ics2), (cs2,fs2))

compBE :: Temp -> Scope -> BExpr -> ([IC_Instruction], Op, Temp, Scope)
compBE t (c,f) (Eq x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                        in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx) y
                             in (icx ++ icy ++ [(Equal' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (deleteTemp (getTemp sy) sy), (cy+1,f))
compBE t (c,f)  (Ne x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                            in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx)y
                                in (icx ++ icy ++ [(NotEq' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (deleteTemp (getTemp sy) sy), (cy+1,f))
compBE t (c,f)  (Lt x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                        in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx)y
                             in (icx ++ icy ++ [(Lt'  (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (deleteTemp (getTemp sy) sy), (cy+1,f))
compBE t (c,f) (Gt x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                            in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx)y
                                in (icx ++ icy ++ [(Gt' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (deleteTemp (getTemp sy) sy), (cy+1,f))
compBE t (c,f) (Le x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                           in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx) y
                             in (icx ++ icy ++ [(Le' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (deleteTemp (getTemp sy) sy), (cy+1,f))
compBE t (c,f) (Ge x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                            in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx) y
                                 in (icx ++ icy ++ [(Ge' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (deleteTemp (getTemp sy) sy), (cy+1,f))
compBE t (c,f) (Or x y) = let (icx, tx, sx, (cx,fx)) = compBE t (c,f) x
                            in let (icy, ty, sy, (cy,fy)) = compBE sx (cx+2,fx) y
                             in (icx  ++ [(Bzero' (Var' (getTemp t)) (cx)), (Jump' (cy))] ++  icy, (Var' (getTemp sx)), (deleteTemp (getTemp sx) sx), (cy-2,f))
compBE t (c,f) (And x y) = let (icx, tx, sx,(cx,fx)) = compBE t (c,f) x
                             in let (icy, ty, sy, (cy,fy)) = compBE sx (cx+2,f) y
                                  in (icx  ++ [(Bzero' (Var' (getTemp t)) (cy+2)), (Jump' (cx+2))] ++ icy , (Var' (getTemp sx)), (deleteTemp (getTemp sx) sx), (cy+2,f))
compBE t (c,f) (Not x) = let (icx, tx, sx, (cx,fx)) = compBE t (c,f) x
                          in (icx ++ [(Not' (Var' (head (deleteTemp (getTemp sx) sx))) tx )], (Var' (head (deleteTemp (getTemp sx) sx))), (freeTemp (head (deleteTemp (getTemp sx) sx)) sx), (cx+1,f))


compE :: Temp -> Scope -> Expr -> ([IC_Instruction], Op, Temp, Scope)
compE t (c,f) (ValInt i) = ([], (Val' (fromIntegral i)), t, (c,f))
compE t (c,f) (Var s) = ([], (Var' s), t, (c,f))
compE t (c,f) (ValFloat a b) = ([], (Val' (fromIntegral a)), t, (c,f))
compE t (c,f) (Uminus x) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                             in (icx ++ [(Uminus' (Var'  (head (deleteTemp (getTemp sx) sx))) tx )], (Var' (head (deleteTemp (getTemp sx) sx))), (freeTemp (head (deleteTemp (getTemp sx) sx)) sx), (cx+1,fx))
compE t (c,f) (Plus x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                             in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx) y
                                  in (icx ++ icy ++ [(Plus' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (freeTemp (getTemp sx) sy), (cy+1,fy))
compE t (c,f) (Minus x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                              in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx) y
                                  in (icx ++ icy ++ [(Minus' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (freeTemp (getTemp sx) sy), (cy+1,fy))
compE t (c,f) (Mult x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                            in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx) y
                                in (icx ++ icy ++ [(Times' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (freeTemp (getTemp sx) sy), (cy+1,fy))
compE t (c,f) (Div x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                           in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx) y
                               in (icx ++ icy ++ [(Div' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (freeTemp (getTemp sx) sy), (cy+1,fy))
compE t (c,f) (Mod x y) = let (icx, tx, sx, (cx,fx)) = compE t (c,f) x
                           in let (icy, ty, sy, (cy,fy)) = compE sx (cx,fx) y
                               in (icx ++ icy ++ [(Mod' (Var' (getTemp sy)) tx ty)], (Var' (getTemp sy)), (freeTemp (getTemp sx) sy), (cy+1,fy))
compE t (c,f) (Call s e) = let (icx, tx, sx, (cx,[(n,ic,fx)])) = compE t (c,f) e
                             in (icx++ [(Push'),(Assign' (Var' (n)) (tx)),(case (lookup s fx) of (Just q) -> Call' (q)),(Assign' (Var' (getTemp (tail sx))) (Var' "_ret_val") )] , (Var' (getTemp (tail sx))), (deleteTemp (getTemp (tail sx))  (tail sx)), (cx+4,[(n,ic,fx)]) )
