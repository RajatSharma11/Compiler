structure Ast = 
struct 
datatype Operator = Plus | Minus | Divison | Times;
datatype Expression = Const of int | Operation of Expression * Operator * Expression;
datatype Statement = Assignment of string * Expression
| If of Expression * Statement
| IfElse of Expression * Statement * Statement
| While of Expression * Statement;

fun plus a b = Operation(a, Plus, b)
fun minus a b = Operation(a, Minus, b)
fun mul a b = Operation(a, Times, b)
fun div a b = Operation(a, Division, b)
