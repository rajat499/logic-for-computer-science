
(* 
====================
Self Made Cases
==================== 
 *)

(* Case 1 *)
(* val arg = arg2Tree (HENCE([ ATOM("p", [CONST("a")]) ] , EX( VAR "x", ATOM("p", [VAR("x")])) )); *)

(* Case 2 *)
(* val t1 = ALL(VAR("x"), AND(NOT(ATOM("p", [VAR("x")])), NOT(ATOM("q", [VAR("x")]))) );
val t2 =  AND(EX(VAR("x"), ATOM("p", [VAR("x")])), EX(VAR("y"), ATOM("q", [VAR("y")])));
val arg = AND(t1, t2); *)

(* Case 3 *)
(* val arg = AND(EX(VAR("x"), ATOM("p", [VAR("x")])), ALL(VAR("y"), AND(OR(NOT(ATOM("p", [VAR("y")])), ATOM("r", [VAR("y")])), ALL(VAR("z"), ATOM("s", [VAR("y"), VAR("z")]))))) ; *)

(* Case 4 *)
(* val arg = arg2Tree (HENCE([ ATOM("p", [FUN("f", [FUN("f", [CONST("c")])])]) , ALL(VAR("x"), OR(ATOM("p", [VAR("x")]), NOT(ATOM("p", [FUN("f", [VAR("x")])])) ))  ] , ATOM("p", [CONST("c")]) )); *)

(* Case 5 *)
(* val arg = arg2Tree (HENCE([ ALL(VAR("x"), COND(ATOM("p", [VAR("x")]), ATOM("q", [VAR("x")]))) ], COND(ALL(VAR("x"), ATOM("p", [VAR("x")])), ALL(VAR("x"), ATOM("q", [VAR("x")]))))); *)

(* Case 6 *)
(* val t1 = ALL(VAR("x"), ATOM("p", [VAR("x")]))
val t2 = EX(VAR("x"), NOT(ATOM("p", [FUN("f", [FUN("g", [VAR("x")])])])))
val arg = AND(t1, t2) *)

(* Case 7 *)
(* val t1 = ALL(VAR("x"), ATOM("p", [VAR("x")]))
val t2 = OR(NOT(ATOM("p", [CONST("a")])), NOT(ATOM("p", [CONST("b")])))
val arg = AND(t1, t2) *)


(* ================================================================================= *)


(* 
====================
Assignment Test Case
==================== 
*)


(*Test Case 1*)
(* val arg = arg2Tree (HENCE([ALL(VAR("x"), COND(ATOM("F", [VAR("x")]), ATOM("G", [VAR("x")]))), ALL(VAR("x"), ATOM("F", [VAR("x")]))],ATOM("G", [CONST("a")]))); *)

(*Test Case 2*)
(* val arg = arg2Tree (HENCE([ALL(VAR("x"), COND(ATOM("F", [VAR("x")]), ATOM("G", [VAR("x")]))), ALL(VAR("x"), ATOM("G", [VAR("x")]))],ATOM("F", [CONST("a")]))); *)



(*Test Case 3*)
(* val arg = arg2Tree (HENCE([ALL(VAR("x"), COND(ATOM("F", [VAR("x")]), ATOM("G", [VAR("x")]))), EX(VAR("x"), AND(ATOM("F", [VAR("x")]), ATOM("H", [VAR("x")])))], EX(VAR("x"), AND(ATOM("F", [VAR("x")]), ATOM("H", [VAR("x")])))) ); *)

(*Test Case 4*)
(* val arg = arg2Tree (HENCE([], BIC(EX(VAR("x"), AND(ATOM("P", [VAR("x")]), ATOM("Q", [VAR("x")]))), AND(EX(VAR("x"), ATOM("P", [VAR("x")])), EX(VAR("x"), ATOM("Q", [VAR("x")])) )) )); *)

(*Test Case 5*)
(* val arg = arg2Tree (HENCE([AND(EX(VAR("x"), ATOM("F", [VAR("x")])), EX(VAR("x"), ATOM("G", [VAR("x")])))], EX(VAR("x"), AND(ATOM("P", [VAR("x")]), ATOM("Q", [VAR("x")])) ))); *)

(*Test Case 6*)
(* val arg = arg2Tree (HENCE([EX(VAR("x"), ALL(VAR("y"), ATOM("L", [VAR("x"), VAR("y")])))], ALL(VAR("x"), EX(VAR("y"), ATOM("L", [VAR("y"), VAR("x")]))) )) *)

(*Test Case 7*)
val p1 = ALL(VAR("x"), COND(ATOM("p", [VAR("x")]), EX( VAR("y"), AND(ATOM("p", [VAR("y")]), ATOM("q", [VAR("y"), VAR("x")]) ) )));

val p2 = ALL(VAR("x"), COND(ATOM("p", [VAR("x")]), ATOM("r", [VAR("x"), CONST("a")]) ));

val px = COND(ATOM("p", [VAR("x")]), COND(ATOM("p", [VAR("y")]), COND(ATOM("r", [VAR("y"), VAR("x")]), NOT(ATOM("q", [VAR("x"), VAR("y")])))));

val p3 = ALL(VAR("x"), ALL(VAR("y"), px));


val arg = arg2Tree (HENCE([p1, p2, p3], NOT(ATOM("p", [CONST("a")]))));
