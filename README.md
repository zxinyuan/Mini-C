# Mini-C

Project Files:
src:
Ast.hs - grammar design and pretty show

CParser.hs - parser for Ast

ASTInterpreter.hs - evaluation for Ast

CCompiler.hs - compilation for Ast to Intermediate Code

tests:
ParserTests.hs - 15 test cases that show pretty show parses to the same result

AstInterperterTests.hs - 12 test cases that evaluate to the same result as ICInterpreter

CompilerTests.hs - 9 test cases that show executable compilation 

FeaturesTests.hs - 8 test cases that show shorter or equal length IC_program with the examples given, 2 test cases on the implementation of floats
