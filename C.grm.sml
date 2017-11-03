functor CLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : C_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\008\000\000\000\
\\001\000\002\000\007\000\025\000\006\000\027\000\005\000\000\000\
\\001\000\003\000\016\000\000\000\
\\001\000\005\000\013\000\024\000\012\000\000\000\
\\001\000\005\000\025\000\012\000\022\000\013\000\021\000\014\000\020\000\
\\015\000\019\000\000\000\
\\001\000\005\000\034\000\000\000\
\\001\000\005\000\035\000\000\000\
\\001\000\005\000\040\000\000\000\
\\001\000\006\000\010\000\000\000\
\\001\000\006\000\011\000\000\000\
\\001\000\007\000\023\000\012\000\022\000\013\000\021\000\014\000\020\000\
\\015\000\019\000\000\000\
\\001\000\007\000\024\000\012\000\022\000\013\000\021\000\014\000\020\000\
\\015\000\019\000\000\000\
\\001\000\026\000\037\000\000\000\
\\001\000\033\000\009\000\000\000\
\\001\000\033\000\030\000\000\000\
\\001\000\033\000\031\000\000\000\
\\001\000\033\000\038\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\033\000\036\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\014\000\020\000\015\000\019\000\000\000\
\\052\000\014\000\020\000\015\000\019\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\"
val actionRowNumbers =
"\002\000\001\000\014\000\009\000\
\\010\000\004\000\018\000\002\000\
\\003\000\003\000\003\000\020\000\
\\019\000\011\000\025\000\012\000\
\\005\000\003\000\003\000\003\000\
\\003\000\015\000\016\000\021\000\
\\029\000\028\000\027\000\026\000\
\\002\000\002\000\006\000\007\000\
\\024\000\022\000\013\000\017\000\
\\002\000\008\000\023\000\000\000"
val gotoT =
"\
\\002\000\002\000\003\000\001\000\004\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\002\000\003\000\012\000\000\000\
\\001\000\013\000\000\000\
\\001\000\015\000\000\000\
\\001\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\024\000\000\000\
\\001\000\025\000\000\000\
\\001\000\026\000\000\000\
\\001\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\030\000\000\000\
\\002\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\037\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 40
val numrules = 13
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | INT of  (int)
 | ID of  (string) | CODE of  (Ast.Statement list)
 | STATEMENTS of  (Ast.Statement list) | STATEMENT of  (Ast.Statement)
 | EXPRESSION of  (Ast.Expression)
end
type svalue = MlyValue.svalue
type result = Ast.Statement list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 26) => true | (T 27) => true | (T 24) => true | (T 25) => true
 | (T 28) => true | (T 33) => true | (T 34) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "COMMA"
  | (T 4) => "SEMICOLON"
  | (T 5) => "LPAREN"
  | (T 6) => "RPAREN"
  | (T 7) => "LBRACK"
  | (T 8) => "RBRACK"
  | (T 9) => "LBRACE"
  | (T 10) => "RBRACE"
  | (T 11) => "PLUS"
  | (T 12) => "MINUS"
  | (T 13) => "TIMES"
  | (T 14) => "DIVIDE"
  | (T 15) => "EQ"
  | (T 16) => "NEQ"
  | (T 17) => "LT"
  | (T 18) => "LE"
  | (T 19) => "GT"
  | (T 20) => "GE"
  | (T 21) => "AND"
  | (T 22) => "OR"
  | (T 23) => "ASSIGN"
  | (T 24) => "IF"
  | (T 25) => "ELSE"
  | (T 26) => "WHILE"
  | (T 27) => "BREAK"
  | (T 28) => "CONTINUE"
  | (T 29) => "FOR"
  | (T 30) => "RETURN"
  | (T 31) => "VAR"
  | (T 32) => "NEWLINE"
  | (T 33) => "PRINTF"
  | (T 34) => "SCANF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.STATEMENTS STATEMENTS, STATEMENTS1left, 
STATEMENTS1right)) :: rest671)) => let val  result = MlyValue.CODE (
STATEMENTS)
 in ( LrTable.NT 3, ( result, STATEMENTS1left, STATEMENTS1right), 
rest671)
end
|  ( 1, ( ( _, ( _, _, EOF1right)) :: ( _, ( MlyValue.STATEMENTS 
STATEMENTS, STATEMENTS1left, _)) :: rest671)) => let val  result = 
MlyValue.CODE (STATEMENTS)
 in ( LrTable.NT 3, ( result, STATEMENTS1left, EOF1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.STATEMENTS STATEMENTS, _, STATEMENTS1right))
 :: _ :: ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, _)) :: 
rest671)) => let val  result = MlyValue.STATEMENTS (
STATEMENT :: STATEMENTS)
 in ( LrTable.NT 2, ( result, STATEMENT1left, STATEMENTS1right), 
rest671)
end
|  ( 3, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.ID ID, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT (
Ast.Int(ID))
 in ( LrTable.NT 1, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.EXPRESSION
 EXPRESSION, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.STATEMENT (
Ast.Assignment (ID,EXPRESSION))
 in ( LrTable.NT 1, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 5, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.STATEMENT 
STATEMENT, _, _)) :: _ :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION,
 _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.STATEMENT ( Ast.If (EXPRESSION,STATEMENT) )
 in ( LrTable.NT 1, ( result, IF1left, SEMICOLON1right), rest671)
end
|  ( 6, ( ( _, ( _, _, SEMICOLON2right)) :: ( _, ( MlyValue.STATEMENT 
STATEMENT2, _, _)) :: _ :: _ :: _ :: _ :: ( _, ( MlyValue.STATEMENT 
STATEMENT1, _, _)) :: _ :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION,
 _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.STATEMENT (
 Ast.IfElse  (EXPRESSION,STATEMENT1,STATEMENT2) )
 in ( LrTable.NT 1, ( result, IF1left, SEMICOLON2right), rest671)
end
|  ( 7, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.STATEMENT 
STATEMENT, _, _)) :: _ :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION,
 _, _)) :: _ :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  
result = MlyValue.STATEMENT ( Ast.While (EXPRESSION,STATEMENT))
 in ( LrTable.NT 1, ( result, WHILE1left, SEMICOLON1right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.EXPRESSION (Ast.Const INT)
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right)
) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _))
 :: rest671)) => let val  result = MlyValue.EXPRESSION (
Ast.plus EXPRESSION1 EXPRESSION2)
 in ( LrTable.NT 0, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (
Ast.minus EXPRESSION1 EXPRESSION2)
 in ( LrTable.NT 0, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (
Ast.mul EXPRESSION1 EXPRESSION2)
 in ( LrTable.NT 0, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (
Ast.divi EXPRESSION1 EXPRESSION2)
 in ( LrTable.NT 0, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.CODE x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : C_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT i,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun CONTINUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun RETURN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINTF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun SCANF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
end
end
