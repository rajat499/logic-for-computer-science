
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "fol.sml";

open FOL
fun arg2Tableau arg = formTableau [([arg], 0)] 0 0
(* val arg = arg2Tree (HENCE([ ATOM("p", [CONST("a")]) ] , EX( VAR "x", ATOM("p", [VAR("x")])) ));
 *)
(* val t1 = ALL(VAR("x"), AND(NOT(ATOM("p", [VAR("x")])), NOT(ATOM("q", [VAR("x")]))) );
val t2 =  AND(EX(VAR("x"), ATOM("p", [VAR("x")])), EX(VAR("y"), ATOM("q", [VAR("y")])));
val arg = AND(t1, t2); *)
val arg = AND(EX(VAR("x"), ATOM("p", [VAR("x")])), ALL(VAR("y"), AND(OR(NOT(ATOM("p", [VAR("y")])), ATOM("r", [VAR("y")])), ALL(VAR("z"), ATOM("s", [VAR("y"), VAR("z")]))))) ;
val ans = arg2Tableau arg;

OS.Process.exit(OS.Process.success);