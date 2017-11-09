structure Ast = 
struct 
datatype types =INT 
datatype Operator = Plus | Minus | Division | Times;
datatype Expression = Const of int | Operation of Expression * Operator * Expression;
datatype Statement = Int of string
| Assignment of string * Expression
| Assign of string * string
| If of Expression * Statement list
| IfElse of Expression * Statement list * Statement list
| While of Expression * Statement list
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
end;
