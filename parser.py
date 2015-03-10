import ply.yacc as yacc
















	lexer = lex.lex()
	parser = yacc.yacc()
	file=(open(sys.argv[1],'r')).read()
	# file = [
	#         '<T extends Foo & Bar>'
	#     ]

    for expr in file:
        print('lexing expression {}'.format(expr))
        lexer.input(expr)
        for token in lexer:
            print(token)

        print('parsing expression {}'.format(expr))
        t = parser.parse(expr, lexer=lexer, debug=1)
        print('result: {}'.format(t))
        print('--------------------------------')
