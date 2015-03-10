#!/usr/bin/env python

import ply.yacc as yacc

from lexer import tokens



# ----------------------------------------------------------------------------------------------------
# Identifier:
#     IDENTIFIER

# QualifiedIdentifier:
#     Identifier { . Identifier }

# QualifiedIdentifierList: 
#     QualifiedIdentifier { , QualifiedIdentifier }

def p_Identifier(p):
    'Identifier : IDENTIFIER'

def p_QualifiedIdentifier(p):
    '''QualifiedIdentifier : Identifier | Identifier '.' QualifiedIdentifier'''

def p_QualifiedIdentifierList(p): 
    ''' QualifiedIdentifierList : QualifiedIdentifier | QualifiedIdentifier ',' QualifiedIdentifierList  '''

#------------------------------------------------------------------------------------------------------
#CompilationUnit: 
#     [[Annotations] package QualifiedIdentifier ;]
#                                 {ImportDeclaration} {TypeDeclaration}

# ImportDeclaration: 
#     import [static] Identifier { . Identifier } [. *] ;

# TypeDeclaration: 
#     ClassOrInterfaceDeclaration
#     ;

# ClassOrInterfaceDeclaration: 
#     {Modifier} (ClassDeclaration | InterfaceDeclaration)

# ClassDeclaration: 
#     NormalClassDeclaration
#     EnumDeclaration

# InterfaceDeclaration: 
#     NormalInterfaceDeclaration
#     AnnotationTypeDeclaration



# NormalClassDeclaration: 
#     class Identifier [TypeParameters]
#                                 [extends Type] [implements TypeList] ClassBody

# EnumDeclaration:
#     enum Identifier [implements TypeList] EnumBody

# NormalInterfaceDeclaration: 
#     interface Identifier [TypeParameters] [extends TypeList] InterfaceBody

# AnnotationTypeDeclaration:
#     @ interface Identifier AnnotationTypeBody
def p_CompilationUnit(p): 
	''' CompilationUnit : Square_Annotations PACKAGE QualifiedIdentifier SEMICOLON Curly_ImportDeclaration Curly_TypeDeclaration
						| Curly_ImportDeclaration Curly_TypeDeclaration '''

def p_Square_Annotations(p):
	''' Square_Annotations : Annotations |  '''

def p_Curly_ImportDeclaration(p):
	'''  Curly_ImportDeclaration : ImportDeclaration Curly_ImportDeclaration |  '''

def p_Curly_TypeDeclaration(p):
	'''  Curly_TypeDeclaration : TypeDeclaration Curly_TypeDeclaration | '''

def p_ImportDeclaration(p): 
	''' ImportDeclaration : IMPORT Square_static Identifier Curly_dot_Identifier Square_dot_asterisk '''

def p_Square_static(p):
	''' Square_static : STATIC |  ''' 

def p_Curly_dot_Identifier(p):
	''' Curly_dot_Identifier : '.' Identifier Curly_dot_Identifier |  '''

def p_Square_dot_asterisk(p):
	''' Square_dot_asterisk : '.' '*' | ''' 

def p_TypeDeclaration(p):
	''' TypeDeclaration : ClassOrInterfaceDeclaration ';' '''

def p_ClassOrInterfaceDeclaration(p):
	' ClassOrInterfaceDeclaration : Curly_Modifier ClassDeclaration | Curly_Modifier InterfaceDeclaration' 

def p_Curly_Modifier(p):
	' Curly_Modifier : Modifier Curly_Modifier | '

def p_ClassDeclaration(p):
	' ClassDeclaration : NormalClassDeclaration | EnumDeclaration' 

def p_InterfaceDeclaration(p):
	' InterfaceDeclaration : NormalInterfaceDeclaration | AnnotationTypeDeclaration'

def p_NormalClassDeclaration(p):
	''' NormalClassDeclaration : CLASS Identifier Square_TypeParameters Square_extends_Type Square_implements_TypeList ClassBody '''

def p_Square_TypeParameters(p):
	' Square_TypeParameters : TypeParameters |  '

def p_Square_extends_Type(p):
	' Square_extends_Type : EXTENDS Type |  '

def p_Square_implements_TypeList(p):
	' Square_implements_TypeList : IMPLEMENTS TypeList  ' 

def p_EnumDeclaration(p):
	'EnumDeclaration : ENUM Identifier Square_implements_TypeList EnumBody'

def p_Square_extends_TypeList(p):
	'Square_extends_TypeList : EXTENDS TypeList | '

def p_NormalInterfaceDeclaration(p):
	'NormalInterfaceDeclaration : INTERFACE Identifier Square_TypeParameters Square_extends_TypeList InterfaceBody' 

def p_AnnotationTypeDeclaration(p):
	'''AnnotationTypeDeclaration : '@' INTERFACE Identifier AnnotationTypeBody '''

#--------------------------------------------------------------------------------------------------------------------------
#Type:
#     BasicType {[]}
#     ReferenceType  {[]}

# BasicType: 
#     byte
#     short
#     char
#     int
#     long
#     float
#     double
#     boolean

# ReferenceType:
#     Identifier [TypeArguments] { . Identifier [TypeArguments] }

# TypeArguments: 
#     < TypeArgument { , TypeArgument } >

# TypeArgument:  
#     ReferenceType
#     ? [ (extends | super) ReferenceType ]

def p_Type(p):
	''' Type : BasicType Curly_Square_Brackets | ReferenceType Curly_Square_Brackets  '''

def p_Curly_Square_Brackets(p):
	''' Curly_Square_Brackets : '[' ']' Curly_Square_Brackets |   '''

def p_BasicType(p):
	''' BasicType : BYTE | SHORT | CHAR | INT | LONG | FLOAT | DOUBLE | BOOLEAN''' 

def p_ReferenceType(p):
	''' ReferenceType : Identifier Square_TypeArguments Curly_dot_Identifier_Square_TypeArguments  '''

def p_Square_TypeArguments(p):
	'''  Square_TypeArguments : TypeArguments |  '''

def p_Curly_dot_Identifier_Square_TypeArguments(p):
	''' Curly_dot_Identifier_Square_TypeArguments : '.' Identifier Square_TypeArguments Curly_dot_Identifier_Square_TypeArguments |  '''

def p_TypeArguments(p):
	''' TypeArguments : '<' TypeArgument Curly_comma_typeargument '>'  ''' 

def p_Curly_comma_typeargument(p):
	''' Curly_comma_typeargument : ',' TypeArgument Curly_comma_typeargument | '''

def p_TypeArgument(p):
	''' TypeArgument : ReferenceType '?' | ReferenceType '?' EXTENDS ReferenceType | ReferenceType '?' SUPER ReferenceType '''  


#----------------------------------------------------------------------------------------
#NonWildcardTypeArguments:
#     < TypeList >

# TypeList:  
#     ReferenceType { , ReferenceType }



# TypeArgumentsOrDiamond:
#     < > 
#     TypeArguments

# NonWildcardTypeArgumentsOrDiamond:
#     < >
#     NonWildcardTypeArguments



# TypeParameters:
#     < TypeParameter { , TypeParameter } >

# TypeParameter:
#     Identifier [extends Bound]

# Bound:  
#     ReferenceType { & ReferenceType }









































#-------------------------------------------------------------------------------------------------
# Modifier: 
#     Annotation
#     public
#     protected
#     private
#     static 
#     abstract
#     final
#     native
#     synchronized
#     transient
#     volatile
#     strictfp

# Annotations:
#     Annotation {Annotation}

# Annotation:
#     @ QualifiedIdentifier [ ( [AnnotationElement] ) ]

# AnnotationElement:
#     ElementValuePairs
#     ElementValue

# ElementValuePairs:
#     ElementValuePair { , ElementValuePair }

# ElementValuePair:
#     Identifier = ElementValue
    
# ElementValue:
#     Annotation
#     Expression1 
#     ElementValueArrayInitializer

# ElementValueArrayInitializer:
#     { [ElementValues] [,] }

# ElementValues:
#     ElementValue { , ElementValue }























parser = yacc.yacc()

while True:
   try:
       s = raw_input('Input:')
   except EOFError:
       break
   if not s: continue
   result = parser.parse(s)
   print result
