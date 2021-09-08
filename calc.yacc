(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

fun listToString([]) = ""
  | listToString(x::lis) = x ^ "\n" ^ listToString(lis);


%%
(* required declarations *)
%name Calc

%term   TERM | CONST of string   | NOT  | AND    | OR
             | XOR | EQUALS | IMPLIES   | LPAREN | RPAREN
             | ID of string | EOF | IF  | THEN   | ELSE | EOL

%nonterm PROGRAM of string | STATEMENT of string | FORMULA of string | EXP of string
        | EXPTWO of string | EXPTHREE  of string | STATEMENT_LIST of string

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF



(* %header  *)


%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT


(* %right *)
  (* %nonassoc*)
%start PROGRAM

%verbose

%%
  PROGRAM: STATEMENT_LIST (STATEMENT_LIST1)
  STATEMENT_LIST : STATEMENT STATEMENT_LIST (STATEMENT ^ ", " ^ STATEMENT_LIST)
                  | STATEMENT (STATEMENT)
  STATEMENT: FORMULA TERM (FORMULA1 ^ "TERM \";\"") 
  FORMULA: IF FORMULA THEN FORMULA ELSE FORMULA ("IF \"IF\"" ^ FORMULA1 ^ " THEN \"THEN\"" ^ FORMULA2 ^ " ELSE \"ELSE\"" ^ FORMULA3 ^ "FORMULA --> IF FORMULA THEN FORMULA ELSE FORMULA")
        | FORMULA IMPLIES FORMULA (FORMULA1 ^ "IMPLIES \"IMPLIES\" " ^ FORMULA2 ^ ", FORMULA ---> IMPLIES ")
        | FORMULA AND FORMULA (FORMULA1 ^ "AND \"AND\", FORMULA ---> BINOP, " ^FORMULA2)
        | FORMULA OR FORMULA (FORMULA1 ^ "OR \"OR\", FORMULA ---> BINOP, " ^FORMULA2)
        | FORMULA XOR FORMULA (FORMULA1 ^ " XOR \"XOR\", FORMULA ---> BINOP, " ^FORMULA2)
        | FORMULA EQUALS FORMULA (FORMULA1 ^ " EQUALS, FORMULA ---> BINOP, " ^FORMULA2)
        | NOT FORMULA ("NOT \" NOT \"" ^ ", FORMULA ---> NOT, "^FORMULA1 )
        | LPAREN FORMULA RPAREN ("("^ ", FORMULA ---> LPAREN, "^FORMULA^")"^", FORMULA ---> RPAREN, ")
        | ID ("ID " ^ ID1 ^ ", FORMULA ---> ID, ")
        | CONST ("CONST, " ^ CONST1 ^ "," ^ " FORMULA ---> CONST, ")