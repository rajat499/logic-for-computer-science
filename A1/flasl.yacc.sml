functor flaslLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : flasl_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*
    The precedence order is NOT>AND>OR>IF-Expression[if then/if]>IFF.
    IF then Else has been handle by suitable paranthesisation in IF-Expression itself. 
    For the EBNF(Extended Backus-Naur Form) the terminals and non terminals are as defined below in code.
    Parentheses enclose a derivation
    
    ==================EBNF========================

    main -> [propList] therefore_stmnt
    therefore_stmnt -> THEREFORE prop
    propList -> prop [propList]
    prop -> iff_expression PERIOD
    iff_expression -> if_expression [IFF iff_expression]
    if_expression -> IF or_expression THEN if_expresssion [ELSE if_expression]  | [if_expression IF] or_expression
    or_expression -> [or_expression OR] and_expression
    and_expression -> [and_expression AND] not_expression
    not_expression -> NOT not_expression | parantheses
    parantheses -> LPAREN iff_expression RPAREN | atomic_expression
    atomic_expression -> DQ [atomic_list] DQ
    atomic_list -> ATOM [atomic_list]
*)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\017\000\006\000\016\000\008\000\015\000\011\000\014\000\
\\013\000\013\000\000\000\
\\001\000\001\000\017\000\006\000\016\000\011\000\014\000\013\000\013\000\000\000\
\\001\000\002\000\039\000\005\000\019\000\000\000\
\\001\000\006\000\016\000\011\000\014\000\013\000\013\000\000\000\
\\001\000\008\000\015\000\000\000\
\\001\000\009\000\022\000\000\000\
\\001\000\012\000\038\000\000\000\
\\001\000\013\000\027\000\015\000\026\000\000\000\
\\001\000\013\000\036\000\000\000\
\\001\000\014\000\000\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\\048\000\001\000\017\000\006\000\016\000\011\000\014\000\013\000\013\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\001\000\021\000\007\000\020\000\000\000\
\\052\000\001\000\021\000\003\000\041\000\000\000\
\\053\000\005\000\019\000\000\000\
\\054\000\001\000\021\000\000\000\
\\055\000\005\000\019\000\000\000\
\\056\000\004\000\018\000\000\000\
\\057\000\004\000\018\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\000\000\
\\064\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\\067\000\015\000\026\000\000\000\
\"
val actionRowNumbers =
"\000\000\029\000\027\000\025\000\
\\023\000\021\000\017\000\005\000\
\\014\000\004\000\011\000\007\000\
\\001\000\001\000\003\000\003\000\
\\003\000\003\000\001\000\003\000\
\\015\000\013\000\010\000\008\000\
\\033\000\031\000\006\000\012\000\
\\026\000\002\000\024\000\022\000\
\\016\000\019\000\030\000\032\000\
\\028\000\001\000\018\000\001\000\
\\020\000\009\000"
val gotoT =
"\
\\001\000\041\000\002\000\010\000\003\000\009\000\004\000\008\000\
\\005\000\007\000\006\000\006\000\007\000\005\000\008\000\004\000\
\\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\021\000\004\000\008\000\005\000\007\000\006\000\006\000\
\\007\000\005\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\002\000\022\000\000\000\
\\000\000\
\\012\000\023\000\000\000\
\\005\000\026\000\006\000\006\000\007\000\005\000\008\000\004\000\
\\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\004\000\027\000\005\000\007\000\006\000\006\000\007\000\005\000\
\\008\000\004\000\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\009\000\028\000\010\000\002\000\011\000\001\000\000\000\
\\007\000\029\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\009\000\030\000\010\000\002\000\011\000\001\000\000\000\
\\008\000\031\000\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\005\000\032\000\006\000\006\000\007\000\005\000\008\000\004\000\
\\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\007\000\033\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\035\000\000\000\
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
\\006\000\038\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\006\000\040\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 42
val numrules = 24
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
datatype svalue = VOID | ntVOID of unit ->  unit
 | ATOM of unit ->  (string) | atomic_list of unit ->  (string)
 | atomic_expression of unit ->  (AST.Prop)
 | parentheses of unit ->  (AST.Prop)
 | not_expr of unit ->  (AST.Prop) | and_expr of unit ->  (AST.Prop)
 | or_expr of unit ->  (AST.Prop) | if_expr of unit ->  (AST.Prop)
 | iff_expr of unit ->  (AST.Prop) | prop of unit ->  (AST.Prop)
 | propList of unit ->  (AST.Prop list)
 | therefore_stmnt of unit ->  (AST.Prop)
 | main of unit ->  (AST.Argument)
end
type svalue = MlyValue.svalue
type result = AST.Argument
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
fn (T 13) => true | _ => false
val showTerminal =
fn (T 0) => "IF"
  | (T 1) => "THEN"
  | (T 2) => "ELSE"
  | (T 3) => "AND"
  | (T 4) => "OR"
  | (T 5) => "NOT"
  | (T 6) => "IFF"
  | (T 7) => "THEREFORE"
  | (T 8) => "PERIOD"
  | (T 9) => "COMMA"
  | (T 10) => "LPAREN"
  | (T 11) => "RPAREN"
  | (T 12) => "DQ"
  | (T 13) => "EOF"
  | (T 14) => "ATOM"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.therefore_stmnt therefore_stmnt1, _, 
therefore_stmnt1right)) :: ( _, ( MlyValue.propList propList1, 
propList1left, _)) :: rest671)) => let val  result = MlyValue.main (fn
 _ => let val  (propList as propList1) = propList1 ()
 val  (therefore_stmnt as therefore_stmnt1) = therefore_stmnt1 ()
 in (AST.HENCE(propList, therefore_stmnt))
end)
 in ( LrTable.NT 0, ( result, propList1left, therefore_stmnt1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.therefore_stmnt therefore_stmnt1, 
therefore_stmnt1left, therefore_stmnt1right)) :: rest671)) => let val 
 result = MlyValue.main (fn _ => let val  (therefore_stmnt as 
therefore_stmnt1) = therefore_stmnt1 ()
 in (AST.HENCE([], therefore_stmnt))
end)
 in ( LrTable.NT 0, ( result, therefore_stmnt1left, 
therefore_stmnt1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.prop prop1, _, prop1right)) :: ( _, ( _, 
THEREFORE1left, _)) :: rest671)) => let val  result = 
MlyValue.therefore_stmnt (fn _ => let val  (prop as prop1) = prop1 ()
 in (prop)
end)
 in ( LrTable.NT 1, ( result, THEREFORE1left, prop1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.propList propList1, _, propList1right)) :: (
 _, ( MlyValue.prop prop1, prop1left, _)) :: rest671)) => let val  
result = MlyValue.propList (fn _ => let val  (prop as prop1) = prop1
 ()
 val  (propList as propList1) = propList1 ()
 in (prop::propList)
end)
 in ( LrTable.NT 2, ( result, prop1left, propList1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.prop prop1, prop1left, prop1right)) :: 
rest671)) => let val  result = MlyValue.propList (fn _ => let val  (
prop as prop1) = prop1 ()
 in ([prop])
end)
 in ( LrTable.NT 2, ( result, prop1left, prop1right), rest671)
end
|  ( 5, ( ( _, ( _, _, PERIOD1right)) :: ( _, ( MlyValue.iff_expr 
iff_expr1, iff_expr1left, _)) :: rest671)) => let val  result = 
MlyValue.prop (fn _ => let val  (iff_expr as iff_expr1) = iff_expr1 ()
 in (iff_expr)
end)
 in ( LrTable.NT 3, ( result, iff_expr1left, PERIOD1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.iff_expr iff_expr1, _, iff_expr1right)) :: _
 :: ( _, ( MlyValue.if_expr if_expr1, if_expr1left, _)) :: rest671))
 => let val  result = MlyValue.iff_expr (fn _ => let val  (if_expr as 
if_expr1) = if_expr1 ()
 val  (iff_expr as iff_expr1) = iff_expr1 ()
 in (AST.BIC(if_expr, iff_expr))
end)
 in ( LrTable.NT 4, ( result, if_expr1left, iff_expr1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.if_expr if_expr1, if_expr1left, 
if_expr1right)) :: rest671)) => let val  result = MlyValue.iff_expr
 (fn _ => let val  (if_expr as if_expr1) = if_expr1 ()
 in (if_expr)
end)
 in ( LrTable.NT 4, ( result, if_expr1left, if_expr1right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.if_expr if_expr1, _, if_expr1right)) :: _ ::
 ( _, ( MlyValue.or_expr or_expr1, _, _)) :: ( _, ( _, IF1left, _)) ::
 rest671)) => let val  result = MlyValue.if_expr (fn _ => let val  (
or_expr as or_expr1) = or_expr1 ()
 val  (if_expr as if_expr1) = if_expr1 ()
 in (AST.COND(or_expr, if_expr))
end)
 in ( LrTable.NT 5, ( result, IF1left, if_expr1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.or_expr or_expr1, _, or_expr1right)) :: _ ::
 ( _, ( MlyValue.if_expr if_expr1, if_expr1left, _)) :: rest671)) =>
 let val  result = MlyValue.if_expr (fn _ => let val  (if_expr as 
if_expr1) = if_expr1 ()
 val  (or_expr as or_expr1) = or_expr1 ()
 in (AST.COND(or_expr, if_expr))
end)
 in ( LrTable.NT 5, ( result, if_expr1left, or_expr1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.if_expr if_expr2, _, if_expr2right)) :: _
 :: ( _, ( MlyValue.if_expr if_expr1, _, _)) :: _ :: ( _, ( 
MlyValue.or_expr or_expr1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671
)) => let val  result = MlyValue.if_expr (fn _ => let val  (or_expr
 as or_expr1) = or_expr1 ()
 val  if_expr1 = if_expr1 ()
 val  if_expr2 = if_expr2 ()
 in (AST.ITE(or_expr, if_expr1, if_expr2))
end)
 in ( LrTable.NT 5, ( result, IF1left, if_expr2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.or_expr or_expr1, or_expr1left, 
or_expr1right)) :: rest671)) => let val  result = MlyValue.if_expr (fn
 _ => let val  (or_expr as or_expr1) = or_expr1 ()
 in (or_expr)
end)
 in ( LrTable.NT 5, ( result, or_expr1left, or_expr1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.and_expr and_expr1, _, and_expr1right)) ::
 _ :: ( _, ( MlyValue.or_expr or_expr1, or_expr1left, _)) :: rest671))
 => let val  result = MlyValue.or_expr (fn _ => let val  (or_expr as 
or_expr1) = or_expr1 ()
 val  (and_expr as and_expr1) = and_expr1 ()
 in (AST.OR(or_expr, and_expr))
end)
 in ( LrTable.NT 6, ( result, or_expr1left, and_expr1right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.and_expr and_expr1, and_expr1left, 
and_expr1right)) :: rest671)) => let val  result = MlyValue.or_expr
 (fn _ => let val  (and_expr as and_expr1) = and_expr1 ()
 in (and_expr)
end)
 in ( LrTable.NT 6, ( result, and_expr1left, and_expr1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.not_expr not_expr1, _, not_expr1right)) ::
 _ :: ( _, ( MlyValue.and_expr and_expr1, and_expr1left, _)) :: 
rest671)) => let val  result = MlyValue.and_expr (fn _ => let val  (
and_expr as and_expr1) = and_expr1 ()
 val  (not_expr as not_expr1) = not_expr1 ()
 in (AST.AND(and_expr, not_expr))
end)
 in ( LrTable.NT 7, ( result, and_expr1left, not_expr1right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.not_expr not_expr1, not_expr1left, 
not_expr1right)) :: rest671)) => let val  result = MlyValue.and_expr
 (fn _ => let val  (not_expr as not_expr1) = not_expr1 ()
 in (not_expr)
end)
 in ( LrTable.NT 7, ( result, not_expr1left, not_expr1right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.not_expr not_expr1, _, not_expr1right)) :: 
( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.not_expr (fn _ => let val  (not_expr as not_expr1) = 
not_expr1 ()
 in (AST.NOT(not_expr))
end)
 in ( LrTable.NT 8, ( result, NOT1left, not_expr1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.parentheses parentheses1, parentheses1left,
 parentheses1right)) :: rest671)) => let val  result = 
MlyValue.not_expr (fn _ => let val  (parentheses as parentheses1) = 
parentheses1 ()
 in (parentheses)
end)
 in ( LrTable.NT 8, ( result, parentheses1left, parentheses1right), 
rest671)
end
|  ( 18, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.iff_expr 
iff_expr1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.parentheses (fn _ => let val  (iff_expr as 
iff_expr1) = iff_expr1 ()
 in (iff_expr)
end)
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.atomic_expression atomic_expression1, 
atomic_expression1left, atomic_expression1right)) :: rest671)) => let
 val  result = MlyValue.parentheses (fn _ => let val  (
atomic_expression as atomic_expression1) = atomic_expression1 ()
 in (atomic_expression)
end)
 in ( LrTable.NT 9, ( result, atomic_expression1left, 
atomic_expression1right), rest671)
end
|  ( 20, ( ( _, ( _, _, DQ2right)) :: ( _, ( MlyValue.atomic_list 
atomic_list1, _, _)) :: ( _, ( _, DQ1left, _)) :: rest671)) => let
 val  result = MlyValue.atomic_expression (fn _ => let val  (
atomic_list as atomic_list1) = atomic_list1 ()
 in (AST.ATOM(atomic_list))
end)
 in ( LrTable.NT 10, ( result, DQ1left, DQ2right), rest671)
end
|  ( 21, ( ( _, ( _, _, DQ2right)) :: ( _, ( _, DQ1left, _)) :: 
rest671)) => let val  result = MlyValue.atomic_expression (fn _ => (
AST.ATOM("")))
 in ( LrTable.NT 10, ( result, DQ1left, DQ2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.atomic_list atomic_list1, _, 
atomic_list1right)) :: ( _, ( MlyValue.ATOM ATOM1, ATOM1left, _)) :: 
rest671)) => let val  result = MlyValue.atomic_list (fn _ => let val 
 (ATOM as ATOM1) = ATOM1 ()
 val  (atomic_list as atomic_list1) = atomic_list1 ()
 in (ATOM ^ " " ^ atomic_list)
end)
 in ( LrTable.NT 11, ( result, ATOM1left, atomic_list1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.ATOM ATOM1, ATOM1left, ATOM1right)) :: 
rest671)) => let val  result = MlyValue.atomic_list (fn _ => let val 
 (ATOM as ATOM1) = ATOM1 ()
 in (ATOM)
end)
 in ( LrTable.NT 11, ( result, ATOM1left, ATOM1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.main x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : flasl_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun IFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun THEREFORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun PERIOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun DQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ATOM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.ATOM (fn () => i),p1,p2))
end
end
