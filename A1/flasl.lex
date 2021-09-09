
exception ScanError
structure Tokens = Tokens

type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type svalue = Tokens.svalue
type lexresult = (svalue, pos) token

val line = ref 1
val column = ref 1
val pos = ref 0
fun error (x) = TextIO.output(TextIO.stdOut, x^"\n")
fun init () = ()
fun eof () = Tokens.EOF(!line, !column)


%%
%header (functor FlaslLexFun(structure Tokens: flasl_TOKENS));

period = "\.";
lp = "\(";
rp = "\)";
dq = "\"";
words = ([A-Za-z0-9\{\}\|~!#\$%\;\<\>\=\*&\+'\/:\?\_\\`@\^]|"\["|"\]"|"\-"|",");
ws = [\ \t];
eol = ("\r\n"|"\n");
%%

{eol}       => (line := (!line) + 1; column := 1; lex());
{ws}+       => (column := (!column) + (size yytext); lex());
{period}    => (column := (!column) + (size yytext); Tokens.PERIOD(!line, !column));
{dq}        => (column := (!column) + (size yytext); Tokens.DQ(!line, !column));
{lp}        => (column := (!column) + (size yytext); Tokens.LPAREN(!line, !column));
{rp}        => (column := (!column) + (size yytext); Tokens.RPAREN(!line, !column));
"IF"        => (column := (!column) + (size yytext); Tokens.IF(!line, !column));
"THEN"      => (column := (!column) + (size yytext); Tokens.THEN(!line, !column));
"ELSE"      => (column := (!column) + (size yytext); Tokens.ELSE(!line, !column));
"AND"       => (column := (!column) + (size yytext); Tokens.AND(!line, !column));
"OR"        => (column := (!column) + (size yytext); Tokens.OR(!line, !column));
"NOT"       => (column := (!column) + (size yytext); Tokens.NOT(!line, !column));
"IFF"       => (column := (!column) + (size yytext); Tokens.IFF(!line, !column));
"THEREFORE" => (column := (!column) + (size yytext); Tokens.THEREFORE(!line, !column));
{words}+    => (column := (!column) + (size yytext); Tokens.ATOM(yytext, !line, !column));
.           => (error ("ScanError. Line: " ^ (Int.toString(!line)) ^ ", Column:" ^(Int.toString(!column))^". Error Character:'"^yytext^"'."); column := (!column) + (size yytext); raise ScanError; lex());