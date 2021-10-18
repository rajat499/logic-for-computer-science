
val argv = CommandLine.arguments();

Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "flasl.yacc.sig";
use "flasl.yacc.sml";
use "flasl.lex.sml";
use "driver.sml";

fun printToFile(string_to_print, filename) = 
    let val file = TextIO.openOut filename
    in (TextIO.output (file, string_to_print); TextIO.closeOut file)
    end

fun ast2flasl(ast_input) = printToFile((AST.ast2flasl(ast_input)), "arg-out.flasl")

val parseOutput = Driver.parser (List.hd argv)
val _ = printToFile((AST.ast2flasl(parseOutput)) , (List.hd(List.tl argv)));
val _ = printToFile((AST.ast2tree(parseOutput)) , "arg.sml");

OS.Process.exit(OS.Process.success);