structure Translate =
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
end
