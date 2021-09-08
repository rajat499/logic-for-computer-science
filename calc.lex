structure Tokens= Tokens
  val lexed_tokens = ref "[";

  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => (print(!lexed_tokens^"] \n");Tokens.EOF(!pos, !pos))
  val error = fn (e, l:int, c) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString (l+1)) ^ ": " ^ (Int.toString c) ^ ": " ^ e ^ "\n");

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))

  
%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");

%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{alpha}+ => ( if yytext="TRUE" orelse yytext="FALSE" then (lexed_tokens:= (!lexed_tokens)^ "CONST \""^yytext^"\", "; Tokens.ID(yytext,!pos,yypos))
             else if yytext="NOT" then (lexed_tokens:=(!lexed_tokens)^ "NOT \""^"NOT"^"\", "; Tokens.NOT(!pos,yypos)) 
             else if yytext="AND" then (lexed_tokens:=(!lexed_tokens)^ "AND \""^"AND"^"\", "; Tokens.AND(!pos,yypos))
             else if yytext="OR"  then (lexed_tokens:=(!lexed_tokens)^ "OR \""^"OR"^"\", "; Tokens.OR(!pos,yypos))
             else if yytext="XOR" then (lexed_tokens:=(!lexed_tokens)^ "XOR \""^"XOR"^"\", "; Tokens.XOR(!pos,yypos))
             else if yytext="EQUALS" then (lexed_tokens:=(!lexed_tokens)^ "EQUALS \""^"EQUALS"^"\", "; Tokens.EQUALS(!pos,yypos))
             else if yytext="IMPLIES" then (lexed_tokens:=(!lexed_tokens)^ "IMPLIES \""^"IMPLIES"^"\", "; Tokens.IMPLIES(!pos,yypos))
             else if yytext="IF" then (lexed_tokens:=(!lexed_tokens)^ "IF \""^"IF"^"\", "; Tokens.IF(!pos,yypos))
             else if yytext="THEN" then (lexed_tokens:=(!lexed_tokens)^ "THEN \""^"THEN"^"\", "; Tokens.THEN(!pos,yypos))
             else if yytext="ELSE" then (lexed_tokens:=(!lexed_tokens)^ "ELSE \""^"ELSE"^"\", "; Tokens.ELSE(!pos,yypos))
             else (lexed_tokens:=(!lexed_tokens)^ "ID \""^yytext^"\", "; Tokens.ID(yytext,!pos,yypos)));
{eol} => (pos := (!pos) + 1; Tokens.EOL(!pos,yypos));
";" => (lexed_tokens:=(!lexed_tokens)^ "TERM \""^";"^"\", ";Tokens.TERM(!pos,yypos));
")" => (lexed_tokens:=(!lexed_tokens)^ "RPAREN \""^")"^"\", ";Tokens.RPAREN(!pos,yypos));
"(" => (lexed_tokens:=(!lexed_tokens)^ "LPAREN \""^"("^"\", ";Tokens.LPAREN(!pos,yypos));

.      => (error ("Encountered bad character, Ignoring "^yytext,!pos,yypos); lex() );
    