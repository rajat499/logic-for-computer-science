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

%%

%name flasl

%term IF | THEN | ELSE | AND | OR | NOT | IFF | THEREFORE | PERIOD | LPAREN | RPAREN | DQ | EOF | ATOM of string

%left AND OR
%right IF THEN ELSE IFF

%eop EOF
%noshift EOF

%nonterm main of AST.Argument | therefore_stmnt of AST.Prop | propList of AST.Prop list 
            | prop of AST.Prop | iff_expr of AST.Prop | if_expr of AST.Prop
            | or_expr of AST.Prop | and_expr of AST.Prop | not_expr of AST.Prop
            | parentheses of AST.Prop | atomic_expression of AST.Prop 
            | atomic_list of string 

%pos int

%verbose
%%
main:
        propList therefore_stmnt        (AST.HENCE(propList, therefore_stmnt))
    |   therefore_stmnt                 (AST.HENCE([], therefore_stmnt))

therefore_stmnt:
        THEREFORE prop                  (prop)

propList:
        prop propList                   (prop::propList)
    |   prop                            ([prop])

prop:
        iff_expr PERIOD                 (iff_expr)

iff_expr:
        if_expr IFF iff_expr            (AST.BIC(if_expr, iff_expr))
    |   if_expr                         (if_expr)

if_expr:
        IF or_expr THEN if_expr                 (AST.COND(or_expr, if_expr))
    |   if_expr IF or_expr                      (AST.COND(or_expr, if_expr))
    |   IF or_expr THEN if_expr ELSE if_expr    (AST.ITE(or_expr, if_expr1, if_expr2))
    |   or_expr                                 (or_expr)

or_expr:
        or_expr OR and_expr             (AST.OR(or_expr, and_expr))
    |   and_expr                        (and_expr)

and_expr:
        and_expr AND not_expr           (AST.AND(and_expr, not_expr))
    |   not_expr                        (not_expr)

not_expr:
        NOT not_expr                    (AST.NOT(not_expr))
    |   parentheses                     (parentheses)

parentheses:
        LPAREN iff_expr RPAREN          (iff_expr)
    |   atomic_expression               (atomic_expression)

atomic_expression:
        DQ atomic_list DQ               (AST.ATOM(atomic_list))
    |   DQ DQ                           (AST.ATOM(""))

atomic_list:
        ATOM atomic_list                (ATOM ^ " " ^ atomic_list)
    |   ATOM                            (ATOM)