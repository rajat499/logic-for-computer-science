val space  = " "
type loc = (int * int) * (int * int)
%%
%name Bool

%term 
    IF | THEN | ELSE | NOT | AND | OR | IFF | THEREFORE 

/* Starter Code */

%nonterm  
  start of AST.exp list 
| program of AST.exp list
| formula of AST.exp
| expression of AST.exp
| lambda of AST.exp
| typ of AST.typ

%pos int

%eop EOF
%noshift EOF

(* header *)

%right LET IN END ASSIGN
%right ARROWDEF COLON FN FUN ARROWTYP

%right IF THEN ELSE FI
%right IMPLIES
%left  AND OR XOR 
%left  EQUALS 
%left  LESSTHAN GREATERTHAN (* TODO *)
%left  PLUS MINUS
%left  TIMES
%right NOT NEGATE

%nonassoc LPAREN RPAREN

%start start

%verbose

%%
start       :   program (program)
program     :   expression TERM program (expression :: program) | expression ([expression]) | expression TERM ([expression])
expression  :   formula (formula) 
            |   FUN ID LPAREN ID COLON typ RPAREN COLON typ ARROWDEF formula (AST.FunctionExp( (AST.VarExp ID1), (AST.VarExp ID2), typ1, typ2, formula, (#1 FUN) ))

typ         :   INT (AST.Int) | BOOL (AST.Bool) | typ ARROWTYP typ (AST.Arrow(typ1, typ2)) | LPAREN typ RPAREN (typ)
lambda      :   FN LPAREN ID COLON typ RPAREN COLON typ ARROWDEF formula (AST.LambdaExp((AST.VarExp ID), typ1, typ2, formula, (#1 FN)))

formula     :   IF formula THEN formula ELSE formula FI (AST.CondExp(formula1, formula2, formula3, (#1 IF, #2 FI) ))
            |   LET ID ASSIGN formula IN formula END (AST.LetExp( (AST.VarExp ID), formula1, formula2, (#1 LET, #2 END) ))
            |   lambda (lambda)
            |   LPAREN formula formula RPAREN (AST.AppExp(formula1, formula2, (#1 LPAREN, #2 RPAREN)) )

            |   LPAREN formula RPAREN (formula1)

            |   formula IMPLIES formula (AST.BinExp(AST.Implies(IMPLIES), formula1, formula2))
            |   formula AND formula (AST.BinExp(AST.And(AND), formula1, formula2))
            |   formula OR formula (AST.BinExp(AST.Or(OR), formula1, formula2))
            |   formula XOR formula (AST.BinExp(AST.Xor(XOR), formula1, formula2))
            |   formula EQUALS formula (AST.BinExp(AST.Equals(EQUALS), formula1, formula2))

            |   formula PLUS formula (AST.BinExp(AST.Plus(PLUS), formula1, formula2))
            |   formula MINUS formula (AST.BinExp(AST.Minus(MINUS), formula1, formula2))
            |   formula TIMES formula (AST.BinExp(AST.Times(TIMES), formula1, formula2))

            |   formula GREATERTHAN formula (AST.BinExp(AST.GreaterThan(GREATERTHAN), formula1, formula2))
            |   formula LESSTHAN formula (AST.BinExp(AST.LessThan(LESSTHAN), formula1, formula2))

			      |	  NEGATE formula (AST.UnaryExp(AST.Negate(NEGATE), formula, #1 NEGATE))            
            |   NOT formula (AST.UnaryExp( AST.Not(NOT), formula, #1 NOT))
            |   ID (AST.VarExp(ID))
            |   BOOLCONST (AST.BoolExp(BOOLCONST))
            |   INTCONST (AST.NumExp(INTCONST))