

datatype lexresult= IF | THEN | ELSE | AND | OR | NOT | IFF | THEREFORE | ATOM of string | SPACE | PERIOD | COMMA | LPAREN | RPAREN | EOF
fun error (x) = TextIO.output(TextIO.stdOut, x^"\n")
fun init () = ()
fun eof () = EOF

val line = ref 1
val column = ref 1

%%
%structure FlaslLex
period = "\.";
lp = "\(";
rp = "\)";
comma = ",";
words = ([A-Za-z0-9\{\}\|~!#\$%\;\<\>\=\*&\+'\/:\?\_\\`@\^]|"\["|"\]"|"\-");
ws = [\ \t];
eol = ("\r\n"|"\n");


%%
{eol}       => (line := (!line) + 1; column := 1; lex());
{ws}+       => (column := (!column) + (size yytext); SPACE);
{period}    => (column := (!column) + (size yytext); PERIOD);
{comma}     => (column := (!column) + (size yytext); COMMA);
{lp}        => (column := (!column) + (size yytext); LPAREN);
{rp}        => (column := (!column) + (size yytext); RPAREN);
"IF"        => (column := (!column) + (size yytext); IF);
"THEN"      => (column := (!column) + (size yytext); THEN);
"ELSE"      => (column := (!column) + (size yytext); ELSE);
"AND"       => (column := (!column) + (size yytext); AND);
"OR"        => (column := (!column) + (size yytext); OR);
"NOT"       => (column := (!column) + (size yytext); NOT);
"IFF"       => (column := (!column) + (size yytext); IFF);
"THEREFORE" => (column := (!column) + (size yytext); THEREFORE);
{words}+    => (column := (!column) + (size yytext); ATOM yytext);
.           => (error ("ScanError. Line: " ^ (Int.toString(!line)) ^ ", Column:" ^(Int.toString(!column))^". Error Character:'"^yytext^"'."); column := (!column) + (size yytext); lex());