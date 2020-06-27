module AstInterperterTests where

-- TODO: this is where you prove to us you implemented the additional features
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck


import ParserMonad

import Ast
import CParser
import TestsProject
import ASTInterpreter



unitTests =
  testGroup
    "AstInterperterTests"
     [
     
       testCase "ast interpret for test1" $ assertEqual [] ["w = 24"] $ (eval (getPro parser test1)),
       testCase "ast interpret for test2" $ assertEqual [] ["x = 4","z = -1","z = -1","x = 4","y = 2"] $ (eval (getPro parser test2)),
       testCase "ast interpret for test3" $ assertEqual [] ["sum = 55"] $ (eval (getPro parser test3)),
       testCase "ast interpret for test4" $ assertEqual [] ["count = 7"] $ (eval (getPro parser test4)),
       testCase "ast interpret for test5" $ assertEqual [] ["n = 2","n = 3","n = 5","n = 7","n = 11","n = 13","n = 17","n = 19","n = 23","n = 29","n = 31"] $ (eval (getPro parser test5)),
       testCase "ast interpret for test6" $ assertEqual [] ["x = 3","x = 3","x = 3"] $ (eval (getPro parser test6)),
       testCase "ast interpret for test7" $ assertEqual [] ["z = 12"] $ (eval (getPro parser test7)),
       testCase "ast interpret for test8" $ assertEqual [] ["z = 11"] $ (eval (getPro parser test8)),
       testCase "ast interpret for test9" $ assertEqual [] ["z = 8"] $ (eval (getPro parser test9)),
       testCase "ast interpret for test10" $ assertEqual [] ["z = 22"] $ (eval (getPro parser test10)),
       testCase "ast interpret for test11" $ assertEqual [] ["res = 2"] $ (eval (getPro parser test11)),
       testCase "ast interpret for test12" $ assertEqual [] ["q = 1","q = 1","q = 2","q = 3","q = 3","q = 4","q = 5","q = 5","q = 6","q = 6","q = 6","q = 8","q = 8","q = 8","q = 10","q = 9","q = 10","q = 11","q = 11"] $ (eval (getPro parser test12))
       
     ]

