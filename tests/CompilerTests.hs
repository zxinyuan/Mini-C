module CompilerTests where

-- TODO: this is where you prove to us you implemented the additional features
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck


import Ast
import CParser
import TestsProject
import CCompiler
import ICInterpreter


takeRet :: (a, [String]) -> [String]
takeRet (_, s) = s


unitTests =
  testGroup
    "CompilerTests"
     [
     
       testCase "compile for test1" $ assertEqual [] ["w = 24"] $ takeRet (execute (compile (getPro parser test1))),
       testCase "compile for test2" $ assertEqual [] ["x = 4","z = -1","z = -1","x = 4","y = 2"] $ takeRet (execute (compile (getPro parser test2))),
       testCase "compile for test3" $ assertEqual [] ["sum = 55"] $ takeRet (execute (compile (getPro parser test3))),
       testCase "compile for test4" $ assertEqual [] ["count = 7"] $ takeRet (execute (compile (getPro parser test4))),
       testCase "compile for test5" $ assertEqual [] ["n = 2","n = 3","n = 5","n = 7","n = 11","n = 13","n = 17","n = 19","n = 23","n = 29","n = 31"] $ takeRet (execute (compile (getPro parser test5))),
       testCase "compile for test6" $ assertEqual [] ["x = 3","x = 3","x = 3"] $ takeRet (execute (compile (getPro parser test6))),
       testCase "compile for test7" $ assertEqual [] ["z = 12"] $ takeRet (execute (compile (getPro parser test7))),
       testCase "compile for test8" $ assertEqual [] ["z = 11"] $ takeRet (execute (compile (getPro parser test8))),
       testCase "compile for test9" $ assertEqual [] ["z = 8"] $ takeRet (execute (compile (getPro parser test9)))
         
     ]

