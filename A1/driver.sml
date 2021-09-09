(*Modified excerpts from 
https://www.smlnj.org/doc/ML-Yacc/mlyacc005.html
and https://cs.wellesley.edu/~cs235/fall08/lectures/35_YACC_revised.pdf*)
structure Driver =
struct
    structure flaslLrVals = flaslLrValsFun(structure Token = LrParser.Token)
    structure flaslLex = FlaslLexFun(structure Tokens = flaslLrVals.Tokens)
    structure flaslParser = Join(
        structure LrParser = LrParser
        structure ParserData = flaslLrVals.ParserData
        structure Lex = flaslLex
    )

    val invokeLexer = fn lexstream =>
                let val error_print = fn (token, line, chr) =>
                                    (TextIO.output(TextIO.stdOut, "ParserError: Line:"^(Int.toString line)^", character:"^(Int.toString chr^". String: "^token^"\n")); raise ParseError)
                in flaslParser.parse(0, lexstream, error_print, ())
                end

    fun initLexer lexFun = 
                let val lexer = flaslParser.makeLexer lexFun
                    val _     = flaslLex.UserDeclarations.init()
                in lexer
                end

    fun stringToLexer line = 
                let val evaluated = ref false
                in
                    initLexer (fn x => if (!evaluated) then "" else (evaluated := true; line))
                end

    fun lexerToParser (lexer) = 
		let val dummyEOF = flaslLrVals.Tokens.EOF(0,0)
			val (result, lexer) = invokeLexer lexer
			val (nextToken, lexer) = flaslParser.Stream.get lexer
		in 
			if flaslParser.sameToken(nextToken, dummyEOF) then
				result
			else (TextIO.output(TextIO.stdOut,"Warning: Unconsumed Input\n");
				result)
		end

	fun readFile (filename) =
		let val ins = TextIO.openIn filename
			fun readLine ins = case TextIO.inputLine ins of
						SOME line 	=> line^(readLine ins)
					  | NONE		=> ""
		in readLine ins before TextIO.closeIn ins
		end
    fun parser (filename) = lexerToParser(stringToLexer(readFile filename))
    val parseString = lexerToParser o stringToLexer
end