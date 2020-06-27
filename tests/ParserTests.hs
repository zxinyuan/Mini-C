module ParserTests where

-- TODO: this is where you prove to us you implemented the additional features
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck


import ParserMonad

import Ast
import CParser
import TestsProject

getPro' :: Parser Program -> String -> Program
getPro' (Parser pa) = \s -> case (pa s) of (Just (p, "")) -> p
                                           _ -> []

unitTests =
  testGroup
    "ParserTests"
     [
     
       testCase "prettyShow parse for test1" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test1))) $ (getPro' parser test1),
       testCase "prettyShow parse for test2" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test2))) $ (getPro' parser test2),
       testCase "prettyShow parse for test3" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test3))) $ (getPro' parser test3),
       testCase "prettyShow parse for test4" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test4))) $ (getPro' parser test4),
       testCase "prettyShow parse for test5" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test5))) $ (getPro' parser test5),
       testCase "prettyShow parse for test6" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test6))) $ (getPro' parser test6),
       testCase "prettyShow parse for test7" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test7))) $ (getPro' parser test7),
       testCase "prettyShow parse for test8" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test8))) $ (getPro' parser test8),
       testCase "prettyShow parse for test9" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test9))) $ (getPro parser test9),
       testCase "prettyShow parse for test10" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test10))) $ (getPro' parser test10),
       testCase "prettyShow parse for test11" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test11))) $ (getPro' parser test11),
       testCase "prettyShow parse for test12" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test12))) $ (getPro' parser test12),
       testCase "prettyShow parse for test13" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test13))) $ (getPro' parser test13),
       testCase "prettyShow parse for test14" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test14))) $ (getPro' parser test14),
       testCase "prettyShow parse for test15" $ assertEqual [] (getPro' parser (prettyShow (getPro' parser test15))) $ (getPro' parser test15)

     ]


