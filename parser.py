#!/usr/bin/env python

import ply.lex as lex
from ply.lex import TOKEN
import sys
import ply.yacc as yacc

class MyLexer(object):
	# 	
	# 
	# 
	# 
	# 
	# Our lexer code is here
	# 
	# 
	# 
	# 
	# 

class Expression_Parser(object):




class StatementParser(object):





class NameParser(object):




class LiteralParser(object):




class TypeParser(object):




class ClassParser(object):





class CompilationUnitParser(object):





class MyParser(Expression_Parser, StatementParser, NameParser, LiteralParser, TypeParser, ClassParser, CompilationUnitParser):

    tokens = MyLexer.tokens

    def p_goal_compilation_unit(self, p):
        '''goal : PLUSPLUS compilation_unit'''
        p[0] = p[2]

    def p_goal_expression(self, p):
        '''goal : MINUSMINUS expression'''
        p[0] = p[2]

    def p_goal_statement(self, p):
        '''goal : '*' block_statement'''
        p[0] = p[2]

    def p_error(self, p):
        print('error: {}'.format(p))

    def p_empty(self, p):
        '''empty :'''

# class Parser(object):

#     def __init__(self):
#         self.lexer = lex.lex(module=MyLexer(), optimize=1)
#         self.parser = yacc.yacc(module=MyParser(), start='goal', optimize=1)

#     def tokenize_string(self, code):
#         self.lexer.input(code)
#         for token in self.lexer:
#             print(token)

#     def tokenize_file(self, _file):
#         if type(_file) == str:
#             _file = open(_file)
#         content = ''
#         for line in _file:
#             content += line
#         return self.tokenize_string(content)

#     def parse_expression(self, code, debug=0, lineno=1):
#         return self.parse_string(code, debug, lineno, prefix='--')

#     def parse_statement(self, code, debug=0, lineno=1):
#         return self.parse_string(code, debug, lineno, prefix='* ')

#     def parse_string(self, code, debug=0, lineno=1, prefix='++'):
#         self.lexer.lineno = lineno
#         return self.parser.parse(prefix + code, lexer=self.lexer, debug=debug)

#     def parse_file(self, _file, debug=0):
#         if type(_file) == str:
#             _file = open(_file)
#         content = _file.read()
#         return self.parse_string(content, debug=debug)

lexer = lex.lex(module=MyLexer())
parser = yacc.yacc(module=MyParser(), write_tables=0, start='type_parameters')
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
    t = parser.parse(expr, lexer=lexer)
    # t = parser.parse(expr, lexer=lexer, debug=1)
    print('result: {}'.format(t))
    print('--------------------------------')
