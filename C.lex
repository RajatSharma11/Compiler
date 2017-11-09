type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun str2int(s) = foldl (fn(a,r) => ord(a)-ord(#"0")+10*r) 0 (explode s)
fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end 

%%
	COMMENT=\/\*.*\*\/;
	DIGIT=[0-9]+;
	IDENTIFIER=[a-zA-Z][a-zA-Z0-9]*;
	QUOTE=[\"];
	NONQUOTE=[^\"];
	SPACE=[\ \t]+;
	%header (functor CLexFun(structure Tokens: Comp_TOKENS));
%%

\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

"+"     => (Tokens.PLUS(yypos,yypos+1));
"-"     => (Tokens.MINUS(yypos,yypos+1));
"*"     => (Tokens.TIMES(yypos,yypos+1));
"/"     => (Tokens.DIVIDE(yypos,yypos+1));
"," 	=> (Tokens.COMMA(yypos,yypos+1));
"&"	=> (Tokens.AND(yypos,yypos+1));
"|"     => (Tokens.OR(yypos,yypos+1));
"="     => (Tokens.ASSIGN(yypos,yypos+1));
">="	=> (Tokens.GE(yypos,yypos+2));
">"	=> (Tokens.GT(yypos,yypos+1));
"<="	=> (Tokens.LE(yypos,yypos+2));
"<"	=> (Tokens.LT(yypos,yypos+1));
"!="	=> (Tokens.NEQ(yypos,yypos+2));
";"     => (Tokens.SEMICOLON(yypos,yypos+1));
"=="	=> (Tokens.EQ(yypos,yypos+2));
"{"     => (Tokens.LBRACE(yypos,yypos+1));
"}"     => (Tokens.RBRACE(yypos,yypos+1));
"("     => (Tokens.LPAREN(yypos,yypos+1));
")"     => (Tokens.RPAREN(yypos,yypos+1));
"["     => (Tokens.LBRACK(yypos,yypos+1));
"]"     => (Tokens.RBRACK(yypos,yypos+1));
"int"		=> ( Tokens.INT (yypos,yypos+3) );
"var"   => (Tokens.VAR(yypos, yypos + 3)); 
"return" => (Tokens.RETURN(yypos, yypos + 6));
"while"    => (Tokens.WHILE(yypos,yypos+5));
"printf"   => (Tokens.PRINTF(yypos,yypos+6));
"scanf"	  => (Tokens.SCANF(yypos,yypos+5));
"break" 	 => (Tokens.BREAK(yypos,yypos+5));
"continue" => (Tokens.CONTINUE(yypos,yypos+8));
"if" 	 => (Tokens.IF(yypos,yypos+2));
"else"     => (Tokens.ELSE(yypos,yypos+4));


{COMMENT} => (continue());
{DIGIT} => (Tokens.CONST(str2int yytext, yypos, yypos + size yytext));
{IDENTIFIER} => (Tokens.ID(yytext, yypos, yypos + size yytext));
{SPACE} => (continue());
{QUOTE}{NONQUOTE}*{QUOTE} => (Tokens.STRING(yytext,yypos,yypos+size yytext));
. => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
