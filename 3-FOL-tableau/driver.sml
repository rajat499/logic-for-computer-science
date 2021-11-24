
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "fol.sml";
use "arg.sml";

open FOL
fun arg2Tableau arg = formTableau [([arg], 0)] 0 0

val ans = arg2Tableau arg;
val ctr = (#2 (List.hd ans));

fun falsifyingBranches2Str [] n = ""
|   falsifyingBranches2Str ((a, b)::xs) n = "Branch#"^(Int.toString n)^"\n\n"^(branch2Str a)^"\n\nContradiction with Branch\n\n"^(contradiction2Str b)^"\n\n"^(falsifyingBranches2Str xs (n+1))
 
fun printToFile(string_to_print, filename) = 
    let val file = TextIO.openOut filename
    in (TextIO.output (file, string_to_print); TextIO.closeOut file)
    end

val _ = printToFile((falsifyingBranches2Str ans 0), "temp" );

OS.Process.exit(OS.Process.success);