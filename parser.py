#!/usr/bin/env python

import ply.yacc as yacc

from lexer import tokens

def p_Identifier(p):
    'Identifier : IDENTIFIER'

def p_QualifiedIdentifier(p):
    '''QualifiedIdentifier : Identifier | Identifier . QualifiedIdentifier'''

def p_QualifiedIdentifierList(p): 
    ''' QualifiedIdentifierList : QualifiedIdentifier | QualifiedIdentifier , QualifiedIdentifierList  '''

def p_CompilationUnit(p): 
	''' CompilationUnit : Sq '''

def p_Square_Annotations(p):
	''' Square_Annotations : Annotations |  '''

def p_Curly_ImportDeclaration(p):
	'''  Curly_ImportDeclaration : ImportDeclaration | ImportDeclaration Curly_ImportDeclaration '''

def p_Curly_TypeDeclaration(p):
	'''  Curly_TypeDeclaration : TypeDeclaration | TypeDeclaration Curly_TypeDeclaration '''



    [[Annotations] package QualifiedIdentifier ;]
                                {ImportDeclaration} {TypeDeclaration}

ImportDeclaration: 
    import [static] Identifier { . Identifier } [. *] ;

TypeDeclaration: 
    ClassOrInterfaceDeclaration
    ;

ClassOrInterfaceDeclaration: 
    {Modifier} (ClassDeclaration | InterfaceDeclaration)

ClassDeclaration: 
    NormalClassDeclaration
    EnumDeclaration

InterfaceDeclaration: 
    NormalInterfaceDeclaration
    AnnotationTypeDeclaration



NormalClassDeclaration: 
    class Identifier [TypeParameters]
                                [extends Type] [implements TypeList] ClassBody

EnumDeclaration:
    enum Identifier [implements TypeList] EnumBody

NormalInterfaceDeclaration: 
    interface Identifier [TypeParameters] [extends TypeList] InterfaceBody

AnnotationTypeDeclaration:
    @ interface Identifier AnnotationTypeBody

Type:
    BasicType {[]}
    ReferenceType  {[]}

BasicType: 
    byte
    short
    char
    int
    long
    float
    double
    boolean

ReferenceType:
    Identifier [TypeArguments] { . Identifier [TypeArguments] }

TypeArguments: 
    < TypeArgument { , TypeArgument } >

TypeArgument:  
    ReferenceType
    ? [ (extends | super) ReferenceType ]



























parser = yacc.yacc()

while True:
   try:
       s = raw_input('Input:')
   except EOFError:
       break
   if not s: continue
   result = parser.parse(s)
   print result
