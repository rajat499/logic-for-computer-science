exception invalid;

structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,col:int) =
		    	(TextIO.output(TextIO.stdOut, "Syntax ERROR:" ^ (Int.toString (pos+1)) ^ ":" ^ (Int.toString (col+1)) ^": " ^ s ^ "\n"); raise invalid)
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
		lexer
    end	

fun read_file infile =
    let 
    	val instream = TextIO.openIn infile
	fun loop instream =
		case TextIO.inputLine instream of
	            SOME line => line ^ loop instream
    	    	   | NONE      => ""
    in
	 loop instream before TextIO.closeIn instream
    end;
	


fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = (CalcParser.Stream.get lexer)
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer
val parseFile = parse o stringToLexer o read_file


fun printResults(filename:string) =
	let
	 	val res = parseFile filename;
	in
		(print("["^res^"] \n"); ()) 
	end
