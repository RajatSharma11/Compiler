structure Ast = 
struct 
datatype Operator = Plus | Minus | Division | Times;
datatype Expression = Const of int | Operation of Expression * Operator * Expression;
datatype Statement = Int of string
| Assignment of string * Expression
| Assign of string * string
| If of Expression * Statement
| IfElse of Expression * Statement * Statement
| While of Expression * Statement
| Scanf of string
| Printf of string

fun plus a b = Operation(a, Plus, b)
fun minus a b = Operation(a, Minus, b)
fun mul a b = Operation(a, Times, b)
fun divi a b = Operation(a, Division, b)
end;
