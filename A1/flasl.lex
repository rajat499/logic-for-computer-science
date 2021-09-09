
structure Tokens = Tokens

fun error (x) = TextIO.output(TextIO.stdOut, x^"\n")
fun init () = ()
fun eof () = Tokens.EOF

val line = ref 1
val column = ref 1

%%
%header (functor FlaslLexFun(structure Tokens: flasl_TOKENS));

period = "\.";
lp = "\(";
rp = "\)";
dq = "\"";
comma = ",";
words = ([A-Za-z0-9\{\}\|~!#\$%\;\<\>\=\*&\+'\/:\?\_\\`@\^]|"\["|"\]"|"\-");
ws = [\ \t];
eol = ("\r\n"|"\n");
%%

{eol}       => (line := (!line) + 1; column := 1; lex());
{ws}+       => (column := (!column) + (size yytext); lex());
{period}    => (column := (!column) + (size yytext); Tokens.PERIOD);
{comma}     => (column := (!column) + (size yytext); Tokens.COMMA);
{dq}        => (column := (!column) + (size yytext); Tokens.DQ);
{lp}        => (column := (!column) + (size yytext); Tokens.LPAREN);
{rp}        => (column := (!column) + (size yytext); Tokens.RPAREN);
"IF"        => (column := (!column) + (size yytext); Tokens.IF);
"THEN"      => (column := (!column) + (size yytext); Tokens.THEN);
"ELSE"      => (column := (!column) + (size yytext); Tokens.ELSE);
"AND"       => (column := (!column) + (size yytext); Tokens.AND);
"OR"        => (column := (!column) + (size yytext); Tokens.OR);
"NOT"       => (column := (!column) + (size yytext); Tokens.NOT);
"IFF"       => (column := (!column) + (size yytext); Tokens.IFF);
"THEREFORE" => (column := (!column) + (size yytext); Tokens.THEREFORE);
{words}+    => (column := (!column) + (size yytext); Tokens.ATOM yytext);
.           => (error ("ScanError. Line: " ^ (Int.toString(!line)) ^ ", Column:" ^(Int.toString(!column))^". Error Character:'"^yytext^"'."); column := (!column) + (size yytext); raise ScanError; lex());