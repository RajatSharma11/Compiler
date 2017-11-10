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

end
