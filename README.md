# Compiler
## Compiler for Compiler Course Lab (CS3310)


Team Members:
- Rajat Sharma (111501024)
- Aryan Raj (111501007)


Files Used:
- C_ast.cml : Ast File Contains the abstract syntax. 
- C.lex : C.lex file a lexer file. 
- C.grm : C.grm is the actual grammar file.


Functions with variable Argument are supported. Functions doesn't have return type.
Folder IntermediateCode contains files for intermediate code generation (targeted to javascript). Currently Intermediate code only works for single statement in if, else, and while block.

For Compiling: 
- Go to SML Command Prompt 
- execute: CM.make "sources.cm" 
- execute val a = Parser.parse"test" (* test is just a demo file *) 
- For Intermediate code in javascript execute: IntermediateCode.compile a (* where a is the abstract syntax tree *)

