functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "calc.yacc"*)(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

fun listToString([]) = ""
  | listToString(x::lis) = x ^ "\n" ^ listToString(lis);



(*#line 20.1 "calc.yacc.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\016\000\004\000\015\000\005\000\014\000\006\000\013\000\
\\007\000\012\000\008\000\011\000\000\000\
\\001\000\002\000\010\000\003\000\009\000\009\000\008\000\011\000\007\000\
\\013\000\006\000\000\000\
\\001\000\004\000\015\000\005\000\014\000\006\000\013\000\007\000\012\000\
\\008\000\011\000\010\000\027\000\000\000\
\\001\000\004\000\015\000\005\000\014\000\006\000\013\000\007\000\012\000\
\\008\000\011\000\014\000\026\000\000\000\
\\001\000\004\000\015\000\005\000\014\000\006\000\013\000\007\000\012\000\
\\008\000\011\000\015\000\029\000\000\000\
\\001\000\012\000\000\000\000\000\
\\032\000\000\000\
\\033\000\000\000\
\\034\000\002\000\010\000\003\000\009\000\009\000\008\000\011\000\007\000\
\\013\000\006\000\000\000\
\\035\000\000\000\
\\036\000\004\000\015\000\005\000\014\000\006\000\013\000\007\000\012\000\
\\008\000\011\000\000\000\
\\037\000\004\000\015\000\005\000\014\000\006\000\013\000\007\000\012\000\
\\008\000\011\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\"
val actionRowNumbers =
"\001\000\006\000\000\000\008\000\
\\001\000\018\000\001\000\001\000\
\\019\000\001\000\001\000\001\000\
\\001\000\001\000\009\000\007\000\
\\003\000\002\000\016\000\011\000\
\\015\000\014\000\013\000\012\000\
\\001\000\017\000\004\000\001\000\
\\010\000\005\000"
val gotoT =
"\
\\001\000\029\000\002\000\003\000\003\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\003\000\003\000\002\000\007\000\015\000\000\000\
\\003\000\016\000\000\000\
\\000\000\
\\003\000\017\000\000\000\
\\003\000\018\000\000\000\
\\000\000\
\\003\000\019\000\000\000\
\\003\000\020\000\000\000\
\\003\000\021\000\000\000\
\\003\000\022\000\000\000\
\\003\000\023\000\000\000\
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
\\003\000\026\000\000\000\
\\000\000\
\\000\000\
\\003\000\028\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 30
val numrules = 14
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
datatype svalue = VOID | ntVOID of unit ->  unit | ID of unit ->  (string) | CONST of unit ->  (string) | STATEMENT_LIST of unit ->  (string) | EXPTHREE of unit ->  (string) | EXPTWO of unit ->  (string) | EXP of unit ->  (string) | FORMULA of unit ->  (string) | STATEMENT of unit ->  (string) | PROGRAM of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = string
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 11) => true | _ => false
val showTerminal =
fn (T 0) => "TERM"
  | (T 1) => "CONST"
  | (T 2) => "NOT"
  | (T 3) => "AND"
  | (T 4) => "OR"
  | (T 5) => "XOR"
  | (T 6) => "EQUALS"
  | (T 7) => "IMPLIES"
  | (T 8) => "LPAREN"
  | (T 9) => "RPAREN"
  | (T 10) => "ID"
  | (T 11) => "EOF"
  | (T 12) => "IF"
  | (T 13) => "THEN"
  | (T 14) => "ELSE"
  | (T 15) => "EOL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.STATEMENT_LIST STATEMENT_LIST1, STATEMENT_LIST1left, STATEMENT_LIST1right)) :: rest671)) => let val  result = MlyValue.PROGRAM (fn _ => let val  STATEMENT_LIST1 = STATEMENT_LIST1 ()
 in ((*#line 44.28 "calc.yacc"*)STATEMENT_LIST1(*#line 209.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, STATEMENT_LIST1left, STATEMENT_LIST1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.STATEMENT_LIST STATEMENT_LIST1, _, STATEMENT_LIST1right)) :: ( _, ( MlyValue.STATEMENT STATEMENT1, STATEMENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT_LIST (fn _ => let val  (STATEMENT as STATEMENT1) = STATEMENT1 ()
 val  (STATEMENT_LIST as STATEMENT_LIST1) = STATEMENT_LIST1 ()
 in ((*#line 45.46 "calc.yacc"*)STATEMENT ^ ", " ^ STATEMENT_LIST(*#line 215.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, STATEMENT1left, STATEMENT_LIST1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.STATEMENT STATEMENT1, STATEMENT1left, STATEMENT1right)) :: rest671)) => let val  result = MlyValue.STATEMENT_LIST (fn _ => let val  (STATEMENT as STATEMENT1) = STATEMENT1 ()
 in ((*#line 46.32 "calc.yacc"*)STATEMENT(*#line 222.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, STATEMENT1left, STATEMENT1right), rest671)
end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT (fn _ => let val  FORMULA1 = FORMULA1 ()
 in ((*#line 47.28 "calc.yacc"*)FORMULA1 ^ "TERM \";\""(*#line 228.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, FORMULA1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.FORMULA FORMULA3, _, FORMULA3right)) :: _ :: ( _, ( MlyValue.FORMULA FORMULA2, _, _)) :: _ :: ( _, ( MlyValue.FORMULA FORMULA1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 val  FORMULA3 = FORMULA3 ()
 in ((*#line 48.50 "calc.yacc"*)"IF \"IF\"" ^ FORMULA1 ^ " THEN \"THEN\"" ^ FORMULA2 ^ " ELSE \"ELSE\"" ^ FORMULA3 ^ "FORMULA --> IF FORMULA THEN FORMULA ELSE FORMULA"(*#line 234.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, IF1left, FORMULA3right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ :: ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in ((*#line 49.36 "calc.yacc"*)FORMULA1 ^ "IMPLIES \"IMPLIES\" " ^ FORMULA2 ^ ", FORMULA ---> IMPLIES "(*#line 242.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ :: ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in ((*#line 50.32 "calc.yacc"*)FORMULA1 ^ "AND \"AND\", FORMULA ---> BINOP, " ^FORMULA2(*#line 249.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ :: ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in ((*#line 51.31 "calc.yacc"*)FORMULA1 ^ "OR \"OR\", FORMULA ---> BINOP, " ^FORMULA2(*#line 256.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ :: ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in ((*#line 52.32 "calc.yacc"*)FORMULA1 ^ " XOR \"XOR\", FORMULA ---> BINOP, " ^FORMULA2(*#line 263.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ :: ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in ((*#line 53.35 "calc.yacc"*)FORMULA1 ^ " EQUALS, FORMULA ---> BINOP, " ^FORMULA2(*#line 270.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.FORMULA FORMULA1, _, FORMULA1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1 ()
 in ((*#line 54.24 "calc.yacc"*)"NOT \" NOT \"" ^ ", FORMULA ---> NOT, "^FORMULA1 (*#line 277.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, NOT1left, FORMULA1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.FORMULA FORMULA1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1 ()
 in ((*#line 55.34 "calc.yacc"*)"("^ ", FORMULA ---> LPAREN, "^FORMULA^")"^", FORMULA ---> RPAREN, "(*#line 283.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 56.15 "calc.yacc"*)"ID " ^ ID1 ^ ", FORMULA ---> ID, "(*#line 289.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (CONST as CONST1) = CONST1 ()
 in ((*#line 57.18 "calc.yacc"*)"CONST, " ^ CONST1 ^ "," ^ " FORMULA ---> CONST, "(*#line 295.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, CONST1left, CONST1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun EOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
end
end
