structure Ast = 
struct 
datatype Operator = Plus | Minus | Division | Times;
datatype Expression = Const of int | Operation of Expression * Operator * Expression;
datatype Statement = Int of string
| Assignment of string * Expression
| If of Expression * Statement
| IfElse of Expression * Statement * Statement
| While of Expression * Statement

fun plus a b = Operation(a, Plus, b)
fun minus a b = Operation(a, Minus, b)
fun mul a b = Operation(a, Times, b)
fun divi a b = Operation(a, Division, b)
end;
