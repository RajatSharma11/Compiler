structure Ast = 
struct 
datatype types =INT 
datatype Operator = Plus | Minus | Division | Times;
datatype RelationalOp = Lt | Gt | Eq | Neq | Le | Ge;
datatype Expression = String of string | Const of int | Operation of Expression * Operator * Expression;
datatype ConditionalExpr = rOperation of Expression * RelationalOp * Expression

datatype Statement = Assignment of string * Expression
| Assign of string * string
| If of ConditionalExpr * Statement list
| IfElse of ConditionalExpr * Statement list * Statement list
| While of ConditionalExpr * Statement list
| Scanf of string
| Printf of string
| Decl of types * string * Expression
| Dec of types * string * string
 

 
fun declexp x_type x exp = Decl(x_type ,x,exp)
fun declvar x_type x str= Dec (x_type ,x , str)
fun plus a b = Operation(a, Plus, b)
fun minus a b = Operation(a, Minus, b)
fun mul a b = Operation(a, Times, b)
fun divi a b = Operation(a, Division, b)

fun lt a b = rOperation(a, Lt, b)
fun gt a b = rOperation(a, Gt, b)
fun lte a b = rOperation(a, Le, b)
fun gte a b = rOperation(a, Ge, b)
fun eq a b = rOperation(a, Eq, b)
fun neq a b = rOperation(a, Neq, b)
end;
