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

fun formTableau [] = []
|   formTableau (branch::branchList) = let
                                            val indexToProcess  = (#2 branch)
                                            val propList        = (#1 branch)
                                        in
                                            if indexToProcess >= (List.length propList)
                                            then (propList::(formTableau branchList))
                                            else 
                                                let
                                                    val prop = List.nth(propList, indexToProcess)
                                                    val n = indexToProcess
                                                in
                                                        case prop of
                                                        AST.AND(p1, p2)             =>  formTableau ((propList@[p1, p2], n+1)::branchList)
                                                    |   AST.NOT(AST.AND(p1, p2))    =>  formTableau ((propList@[AST.NOT(p1)], n+1)::(propList@[AST.NOT(p2)], n+1)::branchList)
                                                    |   AST.OR(p1, p2)              =>  formTableau ((propList@[p1], n+1)::(propList@[p2], n+1)::branchList)
                                                    |   AST.NOT(AST.OR(p1, p2))     =>  formTableau ((propList@[AST.NOT(p1), AST.NOT(p2)], n+1)::branchList)
                                                    |   AST.COND(p1, p2)            =>  formTableau ((propList@[AST.NOT(p1)], n+1)::(propList@[p2], n+1)::branchList)
                                                    |   AST.NOT(AST.COND(p1, p2))   =>  formTableau ((propList@[p1, AST.NOT(p2)], n+1)::branchList)
                                                    |   AST.BIC(p1, p2)             =>  formTableau ((propList@[p1, p2], n+1)::(propList@[AST.NOT(p1), AST.NOT(p2)], n+1)::branchList)
                                                    |   AST.NOT(AST.BIC(p1, p2))    =>  formTableau ((propList@[p1, AST.NOT(p2)], n+1)::(propList@[AST.NOT(p1), p2], n+1)::branchList)
                                                    |   AST.NOT(AST.NOT(p))         =>  formTableau ((propList@[p], n+1)::branchList)
                                                    |   _                           =>  formTableau ((propList, n+1)::branchList)
                                                end
                                        end

fun extractLiterals []                            = []
|   extractLiterals (AST.ATOM(s)::xs)             = (AST.ATOM(s) :: (extractLiterals xs))
|   extractLiterals (AST.NOT(AST.ATOM(s))::xs)    = (AST.NOT(AST.ATOM(s)) :: (extractLiterals xs))
|   extractLiterals (x::xs)                       = (extractLiterals xs) 

fun cleanUpBranches [] = []
|   cleanUpBranches (x::xs) =   let 
                                    val branch = (extractLiterals x) 
                                in 
                                    if (List.length branch)>0
                                    then (branch :: (cleanUpBranches xs))
                                    else (cleanUpBranches xs)
                                end

fun checkNegationInBranch str []                           = true
|   checkNegationInBranch str (AST.NOT(AST.ATOM(s))::xs)   = if (String.compare(s, str) = EQUAL) then false else (checkNegationInBranch str xs)
|   checkNegationInBranch str (x::xs)                      = (checkNegationInBranch str xs)

fun checkEntireBranch checkAgainst []                =  true
|   checkEntireBranch checkAgainst (AST.ATOM(s)::xs) =  if (checkNegationInBranch s checkAgainst) 
                                                        then (checkEntireBranch checkAgainst xs) 
                                                        else false
|   checkEntireBranch checkAgainst (x::xs)           =  (checkEntireBranch checkAgainst xs)

fun checkTableau []         =   []
|   checkTableau (x::xs)    =   if (checkEntireBranch x x)
                                then x::(checkTableau xs)
                                else (checkTableau xs)

fun ast2Tableau(ast_input) = formTableau [([AST.convertITE(AST.ast2PropNegated(ast_input))], 0)]

fun ast2FalsifyingBranches(ast_input) = checkTableau(cleanUpBranches(ast2Tableau(ast_input)))

fun literal2Str (AST.ATOM(s))             = ("\"" ^ s ^ "\"")
|   literal2Str (AST.NOT(AST.ATOM(s)))    = "NOT(" ^ ("\"" ^ s ^ "\"") ^ ")"

fun cleanedBranch2Str (x::[])   =   (literal2Str x) ^ ".\n"
|   cleanedBranch2Str (x::xs)   =   (literal2Str x) ^ " , " ^ (cleanedBranch2Str xs)

fun falsifyingBranches2Str [] n         =   ""
|   falsifyingBranches2Str (x::xs) n    =   (Int.toString (n+1)) ^ ". " ^ (cleanedBranch2Str x) ^ (falsifyingBranches2Str xs (n+1))

fun checkASTvalidity(ast_input) =   let
                                        val falsifyingBranches = ast2FalsifyingBranches(ast_input)
                                    in
                                        if (List.length falsifyingBranches) > 0
                                        then (cleanedBranch2Str (List.hd falsifyingBranches))
                                        else "The Argument is valid. \n"
                                    end

val inp_file = (List.hd argv);
val ast = Driver.parser inp_file;
val out_file = substring(inp_file, 0, ((size inp_file)) -5) ^ "out";
val _ = printToFile(checkASTvalidity(ast), out_file );

OS.Process.exit(OS.Process.success);