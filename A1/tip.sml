use "flasl.lex.sml";
val instream = TextIO.openIn "input";
val lexer = FlaslLex.makeLexer ( fn _ => TextIO.input instream);
