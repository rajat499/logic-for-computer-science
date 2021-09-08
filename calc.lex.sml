(*#line 18.10 "calc.lex"*)functor CalcLexFun(structure Tokens:Calc_TOKENS)(*#line 1.1 "calc.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "calc.lex"*)structure Tokens= Tokens
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

  
(*#line 22.1 "calc.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\009\013\003\003\011\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\009\003\003\003\003\003\003\003\008\007\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\006\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\012\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 20)], trans = 0},
{fin = [(N 7),(N 20)], trans = 4},
{fin = [(N 7)], trans = 4},
{fin = [(N 14),(N 20)], trans = 0},
{fin = [(N 16),(N 20)], trans = 0},
{fin = [(N 18),(N 20)], trans = 0},
{fin = [(N 4),(N 20)], trans = 9},
{fin = [(N 4)], trans = 9},
{fin = [(N 12),(N 20)], trans = 11},
{fin = [(N 12)], trans = 0},
{fin = [(N 1),(N 12)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => ((*#line 25.14 "calc.lex"*)pos := (!pos) + 1; lex()(*#line 152.1 "calc.lex.sml"*)
)
| 12 => ((*#line 38.11 "calc.lex"*)pos := (!pos) + 1; Tokens.EOL(!pos,yypos)(*#line 154.1 "calc.lex.sml"*)
)
| 14 => ((*#line 39.9 "calc.lex"*)lexed_tokens:=(!lexed_tokens)^ "TERM \""^";"^"\", ";Tokens.TERM(!pos,yypos)(*#line 156.1 "calc.lex.sml"*)
)
| 16 => ((*#line 40.9 "calc.lex"*)lexed_tokens:=(!lexed_tokens)^ "RPAREN \""^")"^"\", ";Tokens.RPAREN(!pos,yypos)(*#line 158.1 "calc.lex.sml"*)
)
| 18 => ((*#line 41.9 "calc.lex"*)lexed_tokens:=(!lexed_tokens)^ "LPAREN \""^"("^"\", ";Tokens.LPAREN(!pos,yypos)(*#line 160.1 "calc.lex.sml"*)
)
| 20 => let val yytext=yymktext() in (*#line 43.12 "calc.lex"*)error ("Encountered bad character, Ignoring "^yytext,!pos,yypos); lex() (*#line 162.1 "calc.lex.sml"*)
 end
| 4 => ((*#line 26.14 "calc.lex"*)lex()(*#line 164.1 "calc.lex.sml"*)
)
| 7 => let val yytext=yymktext() in (*#line 27.14 "calc.lex"*) if yytext="TRUE" orelse yytext="FALSE" then (lexed_tokens:= (!lexed_tokens)^ "CONST \""^yytext^"\", "; Tokens.ID(yytext,!pos,yypos))
             else if yytext="NOT" then (lexed_tokens:=(!lexed_tokens)^ "NOT \""^"NOT"^"\", "; Tokens.NOT(!pos,yypos)) 
             else if yytext="AND" then (lexed_tokens:=(!lexed_tokens)^ "AND \""^"AND"^"\", "; Tokens.AND(!pos,yypos))
             else if yytext="OR"  then (lexed_tokens:=(!lexed_tokens)^ "OR \""^"OR"^"\", "; Tokens.OR(!pos,yypos))
             else if yytext="XOR" then (lexed_tokens:=(!lexed_tokens)^ "XOR \""^"XOR"^"\", "; Tokens.XOR(!pos,yypos))
             else if yytext="EQUALS" then (lexed_tokens:=(!lexed_tokens)^ "EQUALS \""^"EQUALS"^"\", "; Tokens.EQUALS(!pos,yypos))
             else if yytext="IMPLIES" then (lexed_tokens:=(!lexed_tokens)^ "IMPLIES \""^"IMPLIES"^"\", "; Tokens.IMPLIES(!pos,yypos))
             else if yytext="IF" then (lexed_tokens:=(!lexed_tokens)^ "IF \""^"IF"^"\", "; Tokens.IF(!pos,yypos))
             else if yytext="THEN" then (lexed_tokens:=(!lexed_tokens)^ "THEN \""^"THEN"^"\", "; Tokens.THEN(!pos,yypos))
             else if yytext="ELSE" then (lexed_tokens:=(!lexed_tokens)^ "ELSE \""^"ELSE"^"\", "; Tokens.ELSE(!pos,yypos))
             else (lexed_tokens:=(!lexed_tokens)^ "ID \""^yytext^"\", "; Tokens.ID(yytext,!pos,yypos))(*#line 176.1 "calc.lex.sml"*)
 end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
