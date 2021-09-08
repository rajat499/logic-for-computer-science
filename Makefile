all:
	mllex calc.lex
	mlyacc calc.yacc
	mlton -output a2 bundler.mlb
