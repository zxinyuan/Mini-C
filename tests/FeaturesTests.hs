module FeaturesTests where

-- TODO: this is where you prove to us you implemented the additional features
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck
import Ast
import CParser
import CCompiler
import TestsProject
import ICInterpreter
import IcInterperterExamples


takeRet :: (a, [String]) -> [String]
takeRet (_, s) = s
   
example1 = compile (getPro parser test1)
example2 = compile (getPro parser test2)
example3 = compile (getPro parser test3)
example4 = compile (getPro parser test4)
example5 = compile (getPro parser test5)
example7 = compile (getPro parser test7)
example8 = compile (getPro parser test8)
example9 = compile (getPro parser test9)


unitTests =
  testGroup
    "FeaturesTests"
     [
       --tests for optimization
       testCase "shorter intermediate code" $ assertBool [] ((length icTest1) >= (length example1)),
       testCase "shorter intermediate code" $ assertBool [] ((length icTest2) >= (length example2)),
       testCase "shorter intermediate code" $ assertBool [] ((length icTest3) >= (length example3)),
       testCase "shorter intermediate code" $ assertBool [] ((length icTest4) >= (length example4)),
       testCase "shorter intermediate code" $ assertBool [] ((length icTest5) >= (length example5)),
       testCase "shorter intermediate code" $ assertBool [] ((length icTest5) >= (length example7)),
       testCase "shorter intermediate code" $ assertBool [] ((length icTest5) >= (length example8)),
       testCase "shorter intermediate code" $ assertBool [] ((length icTest5) >= (length example9)),

       
       --tests for float implementation
       testCase "parser for floats" $ assertEqual [] (getPro parser (prettyShow (getPro parser test13))) $ (getPro parser test13),
       testCase "calculation with floats" $ assertEqual [] ["z = 3"] $ takeRet (execute (compile (getPro parser test13)))
     ]

