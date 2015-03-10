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

# def p_QualifiedIdentifier(p):
#     '''QualifiedIdentifier : Identifier '{' '.' Identifier '}' '''

# def p_QualifiedIdentifierList(p): 
#     ''' QualifiedIdentifierList : QualifiedIdentifier '{' ',' QualifiedIdentifierList '}'  '''

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

def p_NonWildcardTypeArguments(p):
	''' NonWildcardTypeArguments : '<' TypeList '>'  '''

def p_TypeList(p):
	'''  TypeList : ReferenceType | ReferenceType ',' TypeList '''  

def p_TypeArgumentsOrDiamond(p):
	'''  TypeArgumentsOrDiamond : '<' '>' | TypeArguments '''

def p_NonWildcardTypeArgumentsOrDiamond(p):
	''' NonWildcardTypeArgumentsOrDiamond :  '<' '>' | NonWildcardTypeArguments '''

def p_TypeParameters(p):
	''' '<' TypeParameter Curly_comma_TypeParameter '>' '''

def p_Curly_comma_TypeParameter(p):
	''' Curly_comma_TypeParameter : ',' TypeParameter Curly_comma_TypeParameter |  '''

def p_TypeParameter(p):
	''' TypeParameter : Identifier Square_extends_Bound '''

def p_Square_extends_Bound(p):
	' Square_extends_Bound : EXTENDS Bound |  '

def p_Bound(p):
	' Bound : ReferenceType Curly_And_Reference_Type'

def p_Curly_And_Reference_Type(p):
	''' Curly_And_Reference_Type : '&' ReferenceType Curly_And_Reference_Type |  '''  


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

def p_Modifier(p):
	'''Modifier : Annotation | PUBLIC | PROTECTED | PROTECTED | PRIVATE | STATIC 
				|ABSTRACT | FINAL | NATIVE | SYNCHRONIZED | TRANSIENT | VOLATILE | STRICTFP'''

def p_Annotations(p):
	'Annotations : Annotation | Annotation Annotations'

def p_Annotation(p):
    ''' Annotation : '@' QualifiedIdentifier | '@' QualifiedIdentifier '(' ')' | '@' QualifiedIdentifier '(' AnnotationElement ')' '''

def p_AnnotationElement(p):
	'AnnotationElement : ElementValue | ElementValuePairs'

def p_ElementValuePairs(p):
	''' ElementValuePairs : ElementValuePair | ElementValuePair ',' ElementValuePairs '''

def p_ElementValuePair(p):
	'''  ElementValuePair : Identifier '=' ElementValue ''' 

def p_ElementValue(p):
	'ElementValue : Annotation | Expression1 | ElementValueArrayInitializer'

def p_ElemetValueArrayInitializer(p):
	'ElementValueArrayInitializer : Square_ElementValues_And_comma ElementValueArrayInitializer |  '

def p_Square_ElementValues_And_comma(p):
	'''Square_ElementValues_And_comma : ElementValues | ElementValues ',' | ',' | '''

def p_ElementValues(p):
	'''ElementValues : ElementValue | ElementValue ',' ElementValues '''

# -----------------------------------------------------------------------------------------------------
# ClassBody: 
#     { { ClassBodyDeclaration } }

# ClassBodyDeclaration:
#     ; 
#     {Modifier} MemberDecl
#     [static] Block

# MemberDecl:
#     MethodOrFieldDecl
#     void Identifier VoidMethodDeclaratorRest
#     Identifier ConstructorDeclaratorRest
#     GenericMethodOrConstructorDecl
#     ClassDeclaration
#     InterfaceDeclaration

# MethodOrFieldDecl:
#     Type Identifier MethodOrFieldRest

# MethodOrFieldRest:  
#     FieldDeclaratorsRest ;
#     MethodDeclaratorRest

# FieldDeclaratorsRest:  
#     VariableDeclaratorRest { , VariableDeclarator }

# MethodDeclaratorRest:
#     FormalParameters {[]} [throws QualifiedIdentifierList] (Block | ;)

# VoidMethodDeclaratorRest:
#     FormalParameters [throws QualifiedIdentifierList] (Block | ;)

# ConstructorDeclaratorRest:
#     FormalParameters [throws QualifiedIdentifierList] Block

# GenericMethodOrConstructorDecl:
#     TypeParameters GenericMethodOrConstructorRest

# GenericMethodOrConstructorRest:
#     (Type | void) Identifier MethodDeclaratorRest
#     Identifier ConstructorDeclaratorRest

























# ------------------------------------------------------------------------------------------------------
# InterfaceBody: 
#     { { InterfaceBodyDeclaration } }

# InterfaceBodyDeclaration:
#     ; 
#     {Modifier} InterfaceMemberDecl

# InterfaceMemberDecl:
#     InterfaceMethodOrFieldDecl
#     void Identifier VoidInterfaceMethodDeclaratorRest
#     InterfaceGenericMethodDecl
#     ClassDeclaration
#     InterfaceDeclaration

# InterfaceMethodOrFieldDecl:
#     Type Identifier InterfaceMethodOrFieldRest

# InterfaceMethodOrFieldRest:
#     ConstantDeclaratorsRest ;
#     InterfaceMethodDeclaratorRest

# ConstantDeclaratorsRest: 
#     ConstantDeclaratorRest { , ConstantDeclarator }

# ConstantDeclaratorRest: 
#     {[]} = VariableInitializer

# ConstantDeclarator: 
#     Identifier ConstantDeclaratorRest

# InterfaceMethodDeclaratorRest:
#     FormalParameters {[]} [throws QualifiedIdentifierList] ; 

# VoidInterfaceMethodDeclaratorRest:
#     FormalParameters [throws QualifiedIdentifierList] ;  

# InterfaceGenericMethodDecl:
#     TypeParameters (Type | void) Identifier InterfaceMethodDeclaratorRest


















# ------------------------------------------------------------------------------------------
# FormalParameters: 
#     ( [FormalParameterDecls] )

# FormalParameterDecls: 
#     {VariableModifier}  Type FormalParameterDeclsRest

# VariableModifier:
#     final
#     Annotation

# FormalParameterDeclsRest: 
#     VariableDeclaratorId [, FormalParameterDecls]
#     ... VariableDeclaratorId

# VariableDeclaratorId:
#     Identifier {[]}

# VariableDeclarators:
#     VariableDeclarator { , VariableDeclarator }

# VariableDeclarator:
#     Identifier VariableDeclaratorRest

# VariableDeclaratorRest:
#     {[]} [ = VariableInitializer ]

# VariableInitializer:
#     ArrayInitializer
#     Expression

# ArrayInitializer:
#     { [ VariableInitializer { , VariableInitializer } [,] ] }














# ------------------------------------------------------------------------------------------------
# Block: 
#     { BlockStatements }

# BlockStatements: 
#     { BlockStatement }

# BlockStatement:
#     LocalVariableDeclarationStatement
#     ClassOrInterfaceDeclaration
#     [Identifier :] Statement

# LocalVariableDeclarationStatement:
#     { VariableModifier }  Type VariableDeclarators ;

# Statement:
#     Block
#     ;
#     Identifier : Statement
#     StatementExpression ;
#     if ParExpression Statement [else Statement] 
#     assert Expression [: Expression] ;
#     switch ParExpression { SwitchBlockStatementGroups } 
#     while ParExpression Statement
#     do Statement while ParExpression ;
#     for ( ForControl ) Statement
#     break [Identifier] ;
#     continue [Identifier] ;
#     return [Expression] ;
#     throw Expression ;
#     synchronized ParExpression Block
#     try Block (Catches | [Catches] Finally)
#     try ResourceSpecification Block [Catches] [Finally]

# StatementExpression: 
#     Expression










# ----------------------------------------------------------------------
# Catches:
#     CatchClause { CatchClause }

# CatchClause:  
#     catch ( {VariableModifier} CatchType Identifier ) Block

# CatchType:
#     QualifiedIdentifier { | QualifiedIdentifier }

# Finally:
#     finally Block

# ResourceSpecification:
#     ( Resources [;] )

# Resources:
#     Resource { ; Resource }

# Resource:
#     {VariableModifier} ReferenceType VariableDeclaratorId = Expression 






























# ------------------------------------------------------------------------------------
# SwitchBlockStatementGroups: 
#     { SwitchBlockStatementGroup }

# SwitchBlockStatementGroup: 
#     SwitchLabels BlockStatements

# SwitchLabels:
#     SwitchLabel { SwitchLabel }

# SwitchLabel: 
#     case Expression :
#     case EnumConstantName :
#     default :

# EnumConstantName:
#     Identifier



# ForControl:
#     ForVarControl
#     ForInit ; [Expression] ; [ForUpdate]

# ForVarControl:
#     {VariableModifier} Type VariableDeclaratorId  ForVarControlRest

# ForVarControlRest:
#     ForVariableDeclaratorsRest ; [Expression] ; [ForUpdate]
#     : Expression

# ForVariableDeclaratorsRest:
#     [= VariableInitializer] { , VariableDeclarator }

# ForInit: 
# ForUpdate:
#     StatementExpression { , StatementExpression }  















# ----------------------------------------------------------------------------------------
# Expression: 
#     Expression1 [AssignmentOperator Expression1]

# AssignmentOperator: 
#     = 
#     +=
#     -= 
#     *=
#     /=
#     &=
#     |=
#     ^=
#     %=
#     <<=
#     >>=
#     >>>=

# Expression1: 
#     Expression2 [Expression1Rest]

# Expression1Rest: 
#     ? Expression : Expression1

# Expression2:
#     Expression3 [Expression2Rest]

# Expression2Rest:
#     { InfixOp Expression3 }
#     instanceof Type





























# -------------------------------------------------------------------------------------
# InfixOp: 
#     || 
#     &&
#     |
#     ^
#     &
#     ==
#     !=
#     <
#     >
#     <=
#     >=
#     <<
#     >>
#     >>>
#     +
#     -
#     *
#     /
#     %

# Expression3: 
#     PrefixOp Expression3
#     ( (Expression | Type) ) Expression3
#     Primary { Selector } { PostfixOp }

# PrefixOp: 
#     ++
#     --
#     !
#     ~
#     +
#     -

# PostfixOp: 
#     ++
#     --

















# -----------------------------------------------------------------------------------------
# Primary: 
#     Literal
#     ParExpression
#     this [Arguments]
#     super SuperSuffix
#     new Creator
#     NonWildcardTypeArguments (ExplicitGenericInvocationSuffix | this Arguments)
#     Identifier { . Identifier } [IdentifierSuffix]
#     BasicType {[]} . class
#     void . class



# Literal:
#     IntegerLiteral
#     FloatingPointLiteral
#     CharacterLiteral 	
#     StringLiteral 	
#     BooleanLiteral
#     NullLiteral

# ParExpression: 
#     ( Expression )

# Arguments:
#     ( [ Expression { , Expression } ] )

# SuperSuffix: 
#     Arguments 
#     . Identifier [Arguments]

# ExplicitGenericInvocationSuffix: 
#     super SuperSuffix
#     Identifier Arguments








# ---------------------------------------------------------------------------------------------
# Creator:  
#     NonWildcardTypeArguments CreatedName ClassCreatorRest
#     CreatedName (ClassCreatorRest | ArrayCreatorRest)

# CreatedName:   
#     Identifier [TypeArgumentsOrDiamond] { . Identifier [TypeArgumentsOrDiamond] }

# ClassCreatorRest: 
#     Arguments [ClassBody]

# ArrayCreatorRest:
#     [ (] {[]} ArrayInitializer  |  Expression ] {[ Expression ]} {[]})



# IdentifierSuffix:
#     [ ({[]} . class | Expression) ]
#     Arguments 
#     . (class | ExplicitGenericInvocation | this | super Arguments |
#                                 new [NonWildcardTypeArguments] InnerCreator)

# ExplicitGenericInvocation:
#     NonWildcardTypeArguments ExplicitGenericInvocationSuffix

# InnerCreator:  
#     Identifier [NonWildcardTypeArgumentsOrDiamond] ClassCreatorRest



# Selector:
#     . Identifier [Arguments]
#     . ExplicitGenericInvocation
#     . this
#     . super SuperSuffix
#     . new [NonWildcardTypeArguments] InnerCreator
#     [ Expression ]







# ------------------------------------------------------------------------------------------------
# EnumBody:
#     { [EnumConstants] [,] [EnumBodyDeclarations] }

# EnumConstants:
#     EnumConstant
#     EnumConstants , EnumConstant

# EnumConstant:
#     [Annotations] Identifier [Arguments] [ClassBody]

# EnumBodyDeclarations:
#     ; {ClassBodyDeclaration}



# AnnotationTypeBody:
#     { [AnnotationTypeElementDeclarations] }

# AnnotationTypeElementDeclarations:
#     AnnotationTypeElementDeclaration
#     AnnotationTypeElementDeclarations AnnotationTypeElementDeclaration

# AnnotationTypeElementDeclaration:
#     {Modifier} AnnotationTypeElementRest

# AnnotationTypeElementRest:
#     Type Identifier AnnotationMethodOrConstantRest ;
#     ClassDeclaration
#     InterfaceDeclaration
#     EnumDeclaration  
#     AnnotationTypeDeclaration

# AnnotationMethodOrConstantRest:
#     AnnotationMethodRest
#     ConstantDeclaratorsRest  

# AnnotationMethodRest:
#     ( ) [[]] [default ElementValue]

def p_AnnotationTypeBody(p):
	''' AnnotationTypeBody : '{' AnnotationTypeElementDeclarations '}' | '{' '}' '''

def p_AnnotationTypeElementDeclarations(p):
	' AnnotationTypeElementDeclarations : AnnotationTypeElementDeclaration | AnnotationTypeElementDeclarations AnnotationTypeElementDeclaration '

def p_AnnotationTypeElementDeclaration(p) :
	'AnnotationTypeElementDeclaration : Curly_Modifier AnnotationTypeElementRest'

def p_AnnotationTypeElementRest(p):
	'''AnnotationTypeElementRest : Type Identifier AnnotationMethodOrConstantRest
									| ClassDeclaration
									| InterfaceDeclaration
									| EnumDeclaration
									| AnnotationTypeDeclaration '''

def p_AnnotationMethodOrConstantRest(p):
	''' AnnotationMethodOrConstantRest : AnnotationMethodRest | ConstantDeclaratorsRest '''

def p_AnnotationMethodRest(p):
	''' AnnotationMethodRest : '(' ')' '[' ']' Square_default_ElementValue | '(' ')' Square_default_ElementValue   '''

def p_Square_default_ElementValue(p):
	''' Square_default_ElementValue : DEFAULT ElementValue |   '''

#---------------------------------------------------------------------------------------------------------
#Parser
parser = yacc.yacc()

while True:
   try:
       s = raw_input('Input:')
   except EOFError:
       break
   if not s: continue
   result = parser.parse(s)
   print result
