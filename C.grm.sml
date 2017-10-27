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
(*#line 1.2 "C.grm"*)structure A = Absyn


(*#line 14.1 "C.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\083\000\006\000\028\000\008\000\083\000\010\000\083\000\
\\012\000\083\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\023\000\083\000\024\000\083\000\027\000\083\000\000\000\
\\001\000\001\000\084\000\006\000\028\000\008\000\084\000\010\000\084\000\
\\012\000\084\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\023\000\084\000\024\000\084\000\027\000\084\000\000\000\
\\001\000\001\000\085\000\006\000\028\000\008\000\085\000\010\000\085\000\
\\012\000\085\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\023\000\085\000\024\000\085\000\027\000\085\000\000\000\
\\001\000\001\000\086\000\006\000\028\000\008\000\086\000\010\000\086\000\
\\012\000\086\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\023\000\086\000\024\000\086\000\027\000\086\000\000\000\
\\001\000\001\000\087\000\006\000\028\000\008\000\087\000\010\000\087\000\
\\012\000\087\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\023\000\087\000\024\000\087\000\027\000\087\000\000\000\
\\001\000\001\000\088\000\006\000\028\000\008\000\088\000\010\000\088\000\
\\012\000\088\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\023\000\088\000\024\000\088\000\027\000\088\000\000\000\
\\001\000\002\000\014\000\003\000\013\000\004\000\012\000\007\000\011\000\
\\008\000\034\000\011\000\010\000\014\000\009\000\026\000\008\000\
\\028\000\007\000\030\000\006\000\000\000\
\\001\000\002\000\014\000\003\000\013\000\004\000\012\000\007\000\011\000\
\\011\000\010\000\014\000\009\000\026\000\008\000\028\000\007\000\
\\030\000\006\000\000\000\
\\001\000\006\000\028\000\008\000\052\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\000\000\
\\001\000\006\000\028\000\010\000\054\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\000\000\
\\001\000\006\000\028\000\010\000\056\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\000\000\
\\001\000\006\000\028\000\012\000\061\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\000\000\
\\001\000\009\000\029\000\000\000\
\\001\000\011\000\057\000\000\000\
\\001\000\012\000\032\000\000\000\
\\001\000\025\000\015\000\000\000\
\\001\000\025\000\058\000\000\000\
\\063\000\006\000\028\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\022\000\018\000\023\000\017\000\
\\024\000\016\000\000\000\
\\073\000\009\000\035\000\000\000\
\\074\000\006\000\028\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\006\000\028\000\015\000\025\000\016\000\024\000\000\000\
\\080\000\006\000\028\000\015\000\025\000\016\000\024\000\000\000\
\\081\000\006\000\028\000\000\000\
\\082\000\006\000\028\000\000\000\
\\089\000\006\000\028\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\090\000\006\000\028\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\091\000\006\000\028\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\022\000\018\000\023\000\017\000\
\\024\000\016\000\000\000\
\\092\000\000\000\
\\093\000\006\000\028\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\022\000\018\000\023\000\017\000\
\\024\000\016\000\000\000\
\\094\000\006\000\028\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\022\000\018\000\023\000\017\000\
\\024\000\016\000\000\000\
\\095\000\006\000\028\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\022\000\018\000\023\000\017\000\
\\024\000\016\000\027\000\051\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\006\000\028\000\013\000\027\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\022\000\018\000\023\000\017\000\
\\024\000\016\000\000\000\
\"
val actionRowNumbers =
"\008\000\032\000\016\000\018\000\
\\037\000\013\000\008\000\008\000\
\\015\000\007\000\022\000\021\000\
\\019\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\035\000\020\000\024\000\009\000\
\\023\000\008\000\033\000\030\000\
\\029\000\004\000\003\000\006\000\
\\005\000\002\000\001\000\028\000\
\\027\000\026\000\025\000\039\000\
\\010\000\008\000\038\000\011\000\
\\014\000\034\000\017\000\008\000\
\\008\000\012\000\031\000\036\000\
\\000\000"
val gotoT =
"\
\\005\000\003\000\008\000\002\000\009\000\001\000\010\000\060\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\028\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\029\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\005\000\031\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\034\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\035\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\036\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\037\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\038\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\039\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\040\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\041\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\042\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\043\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\044\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\045\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\046\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\047\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\048\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\051\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\053\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\057\000\008\000\002\000\009\000\001\000\000\000\
\\005\000\058\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 61
val numrules = 37
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
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
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
datatype svalue = VOID | ntVOID of unit ->  unit | STRING of unit ->  (string) | INT of unit ->  (int) | ID of unit ->  (string) | program of unit ->  (A.exp) | lvalue_not_id of unit ->  (A.var) | lvalue of unit ->  (A.var) | vardec of unit ->  (A.dec) | explist of unit ->  (A.exp list) | exp of unit ->  (A.exp) | fundec of unit ->  (A.dec) | decs of unit ->  (A.dec list) | dec of unit ->  (A.dec)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 27) => true | (T 28) => true | (T 29) => true | (T 25) => true | (T 26) => true | (T 30) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 26))::
(nil
,nil
 $$ (T 6))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "SEMICOLON"
  | (T 6) => "LPAREN"
  | (T 7) => "RPAREN"
  | (T 8) => "LBRACK"
  | (T 9) => "RBRACK"
  | (T 10) => "LBRACE"
  | (T 11) => "RBRACE"
  | (T 12) => "PLUS"
  | (T 13) => "MINUS"
  | (T 14) => "TIMES"
  | (T 15) => "DIVIDE"
  | (T 16) => "EQ"
  | (T 17) => "NEQ"
  | (T 18) => "LT"
  | (T 19) => "LE"
  | (T 20) => "GT"
  | (T 21) => "GE"
  | (T 22) => "AND"
  | (T 23) => "OR"
  | (T 24) => "ASSIGN"
  | (T 25) => "IF"
  | (T 26) => "ELSE"
  | (T 27) => "WHILE"
  | (T 28) => "FOR"
  | (T 29) => "BREAK"
  | (T 30) => "CONTINUE"
  | (T 31) => "UMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1) = exp1 ()
 in ((*#line 52.16 "C.grm"*)exp(*#line 329.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 9, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 53.11 "C.grm"*)(*#line 335.1 "C.grm.sml"*)
))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.dec dec1, dec1left, dec1right)) :: rest671)) => let val  result = MlyValue.decs (fn _ => let val  (dec as dec1) = dec1 ()
 in ((*#line 54.48 "C.grm"*)[dec](*#line 339.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, dec1left, dec1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.dec dec1, _, dec1right)) :: ( _, ( MlyValue.decs decs1, decs1left, _)) :: rest671)) => let val  result = MlyValue.decs (fn _ => let val  (decs as decs1) = decs1 ()
 val  (dec as dec1) = dec1 ()
 in ((*#line 55.48 "C.grm"*)case (dec, decs) of
                                              (A.TypeDec [t], A.TypeDec ts :: ds) => A.TypeDec (t :: ts) :: ds
                                              | (A.FunctionDec [f], A.FunctionDec fs :: ds) => A.FunctionDec (f :: fs) :: ds
                                              | (d, ds) => d :: ds
					      (*#line 345.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, decs1left, dec1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.vardec vardec1, vardec1left, vardec1right)) :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (vardec as vardec1) = vardec1 ()
 in ((*#line 60.25 "C.grm"*)vardec(*#line 356.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, vardec1left, vardec1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right)) :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (fundec as fundec1) = fundec1 ()
 in ((*#line 61.24 "C.grm"*)fundec(*#line 362.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, fundec1left, fundec1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 64.44 "C.grm"*)A.VarDec { name = Symbol.symbol ID, typ = NONE, init = exp, pos = VARright, 						      escape = ref true }(*#line 368.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, ID1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID empty1, empty1left, empty1right)) :: rest671)) => let val  result = MlyValue.explist (fn _ => let val  empty1 = empty1 ()
 in ((*#line 67.48 "C.grm"*)nil(*#line 375.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, empty1left, empty1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.explist (fn _ => let val  (exp as exp1) = exp1 ()
 in ((*#line 68.48 "C.grm"*)exp :: nil(*#line 381.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, exp1left, exp1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.explist explist1, explist1left, _)) :: rest671)) => let val  result = MlyValue.explist (fn _ => let val  (explist as explist1) = explist1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 69.48 "C.grm"*)exp :: explist(*#line 387.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, explist1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 71.48 "C.grm"*)A.VarExp (A.SimpleVar (Symbol.symbol ID, IDleft))(*#line 394.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, MINUS1left, MINUSright)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in ((*#line 72.48 "C.grm"*)A.OpExp {left = A.IntExp 0, oper = A.MinusOp, right = exp, pos = MINUSright}(*#line 400.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, MINUS1left, exp1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = INT1 ()
 in ((*#line 73.48 "C.grm"*)A.IntExp INT(*#line 406.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.STRING STRING1, STRING1left, (STRINGright as STRING1right))) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (STRING as STRING1) = STRING1 ()
 in ((*#line 74.48 "C.grm"*)A.StringExp (STRING, STRINGright)(*#line 412.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, STRING1left, STRING1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => ((*#line 75.48 "C.grm"*)A.SeqExp [](*#line 418.1 "C.grm.sml"*)
))
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => ((*#line 76.48 "C.grm"*)A.SeqExp {}(*#line 422.1 "C.grm.sml"*)
))
 in ( LrTable.NT 4, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, PLUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 77.48 "C.grm"*)A.OpExp {left = exp1, oper = A.PlusOp, right = exp2, pos = PLUSleft}(*#line 426.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, MINUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 78.48 "C.grm"*)A.OpExp {left = exp1, oper = A.MinusOp, right = exp2, pos = MINUSleft}(*#line 433.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, TIMESleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 79.48 "C.grm"*)A.OpExp {left = exp1, oper = A.TimesOp, right = exp2, pos = TIMESleft}(*#line 440.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, DIVIDEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 80.48 "C.grm"*)A.OpExp {left = exp1, oper = A.DivideOp, right = exp2, pos = DIVIDEleft}(*#line 447.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, EQleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 81.48 "C.grm"*)A.OpExp {left = exp1, oper = A.EqOp, right = exp2, pos = EQleft}(*#line 454.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, NEQleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 82.48 "C.grm"*)A.OpExp {left = exp1, oper = A.NeqOp, right = exp2, pos = NEQleft}(*#line 461.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, GTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 83.48 "C.grm"*)A.OpExp {left = exp1, oper = A.GtOp, right = exp2, pos = GTleft}(*#line 468.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, GEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 84.48 "C.grm"*)A.OpExp {left = exp1, oper = A.GeOp, right = exp2, pos = GEleft}(*#line 475.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, LTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 85.48 "C.grm"*)A.OpExp {left = exp1, oper = A.LtOp, right = exp2, pos = LTleft}(*#line 482.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, LEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 86.48 "C.grm"*)A.OpExp {left = exp1, oper = A.LeOp, right = exp2, pos = LEleft}(*#line 489.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, ANDleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 87.48 "C.grm"*)A.IfExp { test = exp1, then' = exp2, else' = SOME (A.IntExp 0), pos = 						      ANDleft }(*#line 496.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, ORleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 88.48 "C.grm"*)A.IfExp { test = exp1, then' = A.IntExp 1, else' = SOME exp2, pos = ORleft }(*#line 503.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, _, LBRACKright)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 89.52 "C.grm"*)A.ArrayExp { typ = Symbol.symbol ID, size = exp1, init = exp2, pos = 						      LBRACKright }(*#line 510.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, ID1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.lvalue_not_id lvalue_not_id1, lvalue_not_id1left, lvalue_not_id1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (lvalue_not_id as lvalue_not_id1) = lvalue_not_id1 ()
 in ((*#line 90.48 "C.grm"*)A.VarExp lvalue_not_id(*#line 518.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, lvalue_not_id1left, lvalue_not_id1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, _, ASSIGNright)) :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 91.48 "C.grm"*)A.AssignExp { var = lvalue, exp = exp, pos = ASSIGNright }(*#line 524.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, lvalue1left, exp1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, IF1left, IFright)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 92.45 "C.grm"*)A.IfExp { test = exp1, else' = SOME exp2, pos = IFright }(*#line 531.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, IF1left, exp2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, IF1left, IFright)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 in ((*#line 93.48 "C.grm"*)A.IfExp { test = exp1, else' = NONE, pos = IFright }(*#line 538.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, IF1left, exp1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.exp exp2, _, _)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( _, WHILE1left, WHILEright)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 94.54 "C.grm"*)A.WhileExp { test = exp1, body = exp2, pos = WHILEright }(*#line 544.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, WHILE1left, RBRACE1right), rest671)
end
|  ( 34, ( ( _, ( _, BREAK1left, (BREAKright as BREAK1right))) :: rest671)) => let val  result = MlyValue.exp (fn _ => ((*#line 95.48 "C.grm"*)A.BreakExp BREAKright(*#line 551.1 "C.grm.sml"*)
))
 in ( LrTable.NT 4, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in ((*#line 96.48 "C.grm"*)exp(*#line 555.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.exp exp2, exp2left, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 97.48 "C.grm"*)case exp2 of
                                               A.SeqExp [] => A.SeqExp ((exp1, exp1right) :: [ (A.SeqExp [], exp2left) ])
                                               | A.SeqExp exps => A.SeqExp ((exp1, exp1right) :: exps)
                                               | _ => A.SeqExp [ (exp1, exp1right), (exp2, exp2right) ]
					      (*#line 561.1 "C.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : C_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun CONTINUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
end
end
