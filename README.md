# Compiler
## Compiler for Compiler Course Lab (CS3310)


Team Members:
- Rajat Sharma (111501024)
- Aryan Raj (111501007)


Files Used:
- C_ast.cml : Ast File Contains the abstract syntax. 
- C.lex : C.lex file a lexer file. 
- C.grm : C.grm is the actual grammar file.
- sources.cm : Used for compiling all the files
- errormsg.sml : Used for error msgs on getting errors
- test : Sample code for testing purpose
- syntaxError : Sample code with syntax error (unknown syntax ^ added in file)
- grammarError : Sample code with grammar error (unkown grammar rule added in file)


Functions with variable Argument are supported. Functions doesn't have return type.
Folder IntermediateCode contains files for intermediate code generation (targeted to javascript). Currently Intermediate code only works for single statement in if, else, and while block.

## For Compiling: 
For Parsing:
- Go to SML Command Prompt 
- Execute: CM.make "sources.cm" 
- Execute val a = Parser.parse"test" (* test is just a demo file *) 


For Intermediate Code
- Go to SML Command Prompt 
- Execute: CM.make "sources.cm"
- Execute Driver.drive "test"  (* test is just a demo file *) 
- Output will be written in output.txt file

