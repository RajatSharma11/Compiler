# Compiler
## Compiler for Compiler Course Lab (CS3310)


Team Members:
- Rajat Sharma (111501024)
- Aryan Raj (111501007)


- C_ast.cml : Ast File Contains the abstract syntax. 
- C.lex : C.lex file a lexer file. 
- C.grm : C.grm is the actual grammar file.


Functions with variable Argument are supported. Functions doesn't have return type.
Folder IntermediateCode contains files for intermediate code generation (targeted to javascript). Currently Intermediate code only works for single statement in if, else, and while block. Code Translation is not supported for function calls.

For Compiling: - 1) Go to SML Command Prompt - 2) execute: CM.make "sources.cm" - 3) execute val a = Parser.parse"test" (* test is just a demo file *) - 4) For Intermediate code in javascript execute: IntermediateCode.compile a (* where a is the abstract syntax tree *)

