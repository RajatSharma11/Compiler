structure IntermediateCode =
struct
fun Expr (Ast.Const (c)) = Int.toString c
| Expr (Ast.Var(c)) = c
| Expr (Ast.String(c)) = c
| Expr (Ast.Operation (a,Ast.Plus,b)) = let
					val a = Expr a
					val b = Expr b
					val c = String.concat [a,"+"]
					val d = String.concat [c,b]
				in
					d
				end
| Expr (Ast.Operation(a,Ast.Minus,b)) = let
					val a = Expr a
					val b = Expr b
					val c = String.concat [a,"-"]
					val d = String.concat [c,b]
				in
					d
				end
| Expr (Ast.Operation(a,Ast.Times,b)) = let
					val a = Expr a
					val b = Expr b
					val c = String.concat [a,"*"]
					val d = String.concat [c,b]
				in
					d
				end
| Expr (Ast.Operation(a,Ast.Division,b)) = let
					val a = Expr a
					val b = Expr b
					val c = String.concat [a,"/"]
					val d = String.concat [c,b]
				in
					d
				end
fun ConditionalExpr (Ast.rOperation (a,Ast.Lt,b)) = let 
							val a = Expr a
							val b = Expr b
							val c = String.concat [a ,"<"]
							val d = String.concat[c, b]
						in
							d
						end
| ConditionalExpr (Ast.rOperation (a,Ast.Gt,b)) = let 
							val a = Expr a
							val b = Expr b
							val c = String.concat [a ,">"]
							val d = String.concat[c, b]
						in
							d
						end
| ConditionalExpr (Ast.rOperation (a,Ast.Le,b)) = let 
							val a = Expr a
							val b = Expr b
							val c = String.concat [a ,"<="]
							val d = String.concat[c, b]
						in
							d
						end
| ConditionalExpr (Ast.rOperation (a,Ast.Ge,b)) = let 
							val a = Expr a
							val b = Expr b
							val c = String.concat [a ,">="]
							val d = String.concat[c, b]
						in
							d
						end
| ConditionalExpr (Ast.rOperation (a,Ast.Eq,b)) = let 
							val a = Expr a
							val b = Expr b
							val c = String.concat [a ,"=="]
							val d = String.concat[c, b]
						in
							d
						end
| ConditionalExpr (Ast.rOperation (a,Ast.Neq,b)) = let 
							val a = Expr a
							val b = Expr b
							val c = String.concat [a ,"!="]
							val d = String.concat[c, b]
						in
							d
						end
| ConditionalExpr (Ast.lOperation (a,Ast.And,b)) = let 
							val a = ConditionalExpr a
							val b = ConditionalExpr b
							val c = String.concat [a ,"&&"]
							val d = String.concat[c, b]
						in
							d
						end
| ConditionalExpr (Ast.lOperation (a,Ast.Or,b)) = let 
							val a = ConditionalExpr a
							val b = ConditionalExpr b
							val c = String.concat [a ,"||"]
							val d = String.concat[c, b]
						in
							d
						end
fun compileExpr (Ast.Decl(_,id,expr)) = let
						val expr = Expr expr
					   in
					   	[String.concat ["var ",String.concat[id,"="], String.concat [expr,";"]]]
					   end
| compileExpr (Ast.Assignment (id,expr)) = let
						val expr = Expr expr
					   in
					   	[String.concat [String.concat[id,"="], String.concat [expr,";"]]]
					   end
| compileExpr (Ast.If (expr,stmt)) = let
						val expr = ConditionalExpr expr
						val stmt = compileExpr stmt
						val stmt = List.nth (stmt,0)
						val a = String.concat["if(",expr]
						val a = String.concat [a,") {"]
						val a = String.concat [a,stmt]
						val a = String.concat [a,"}"]
					in 
						[a]
					end
| compileExpr (Ast.IfElse (expr,stmt1,stmt2)) = let
						val expr = ConditionalExpr expr
						val stmt1 = compileExpr stmt1
						val stmt1 = List.nth (stmt1,0)
						val stmt2 = compileExpr stmt2
						val stmt2 = List.nth (stmt2,0)
						val a = String.concat["if(",expr]
						val a = String.concat [a,") {"]
						val a = String.concat [a,stmt1]
						val a = String.concat [a,"}else{"]
						val a = String.concat [a,stmt2]
						val a = String.concat [a,"}"];
					in 
						[a]
					end
| compileExpr (Ast.While (expr,stmt)) = let
						val expr = ConditionalExpr expr
						val stmt = compileExpr stmt
						val stmt = List.nth (stmt,0)
						val a = String.concat["while(",expr]
						val a = String.concat [a,") {"]
						val a = String.concat [a,stmt]
						val a = String.concat [a,"}"]
					in
						[a]
					end
| compileExpr (Ast.Printf (stmt)) = let
					val a = String.concat["console.log(",stmt]
					val b = String.concat[a,")"]
					in 
					[b]
					end
| compileExpr (Ast.Scanf (stmt)) = let
					val a = String.concat["prompt(",stmt]
					val b = String.concat[a,")"]
					in 
					[b]
					end		

(* | compileExpr (Ast.Fun1 (var,name,arg)) = let
						val a = String.concat[var,"=",name,"(",arg,")",";"];
					in
						[a]
					end
| compileExpr (Ast.Fun2 (var,name,arg1,arg2)) = let
						val a = String.concat[var,"=",name,"(",arg1,",",arg2,")",";"];
					in
						[a]
					end *)



fun compileStmts []        = []
  | compileStmts (x :: xs) = (compileExpr x)  @ (compileStmts xs)

fun getArgs [Ast.aRg(types, id)] = id
|   getArgs (Ast.aRg(types, id) :: xs) = let
					val a = String.concat [id,","]
					val b = getArgs xs
				in
					String.concat[a,b]
				end
fun compileFuncs [] = []
| compileFuncs (Ast.Func (a,b,c)::xs) = 
						let
							val a = String.concat ["function ",a]
							val a = String.concat [a," ("]
							val x = getArgs b
							val a = String.concat[a,x]
							val a = String.concat[a, ")"]
							val a = String.concat[a, "{"]
							val x = String.concat (compileStmts c)
							val a = String.concat [a , x,"return ",";","}"]
							val x = compileFuncs xs
						in
							(a::x)
						end
							
fun compile (main,funcs) = 
				let 
					val mainLst = compileStmts main
					val funcLst = compileFuncs funcs
				in
					mainLst@ funcLst
					
				end
end
