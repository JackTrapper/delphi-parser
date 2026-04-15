parser grammar DelphiFromYaml;

// Auto-transpiled (best-effort) from Grammar.yaml DGrok-style docs.
// Parser rules only; provide a lexer for keywords/tokens.

addOp
	: '+'
	| '-'
	| OR
	| XOR
	;

arrayType
	: ARRAY ('(' (type (',')?)+ ')?')? OF type
	;

assemblerStatement
	: ASM ASSEMBLYLANGUAGE END
	;

assemblyAttribute
	: '(' ASSEMBLY ':' expression ')?'
	;

atom
	: particle ( '.' extendedIdent | '(' expressionList ')?' | '^' | '(' (parameterExpression (',')?)* ')' )*
	;

bareInherited
	: INHERITED
	;

block
	: BEGIN (statementList)? END
	| assemblerStatement
	;

caseSelector
	: (expressionOrRange (',')?)+ ':' (statement)? (';')?
	;

caseStatement
	: CASE expression OF (caseSelector)+ (ELSE (statementList)?)? END
	;

classHelperType
	: CLASS HELPER ('(' qualifiedIdent ')')? FOR qualifiedIdent (visibilitySection)* END
	;

classOfType
	: CLASS OF qualifiedIdent
	;

classType
	: CLASS (ABSTRACT | SEALED)? ('(' (qualifiedIdent (',')?)+ ')')? (visibilitySection)* END
	;

constantDecl
	: ident (':' type)? '=' typedConstant (portabilityDirective)* ';'
	;

constSection
	: (CONST|RESOURCESTRING) (constantDecl)+
	;

directive
	: (';')? ABSTRACT
	| (';')? ASSEMBLER
	| (';')? CDECL
	| (';')? DISPID expression
	| (';')? DYNAMIC
	| (';')? EXPORT
	| (';')? EXTERNAL (expression (exportsSpecifier)*)?
	| (';')? FAR
	| (';')? FINAL
	| (';')? FORWARD
	| (';')? INLINE
	| (';')? LOCAL
	| (';')? MESSAGE expression
	| (';')? NEAR
	| (';')? OVERLOAD
	| (';')? OVERRIDE
	| (';')? PASCAL
	| (';')? REGISTER
	| (';')? REINTRODUCE
	| (';')? SAFECALL
	| (';')? STATIC
	| (';')? STDCALL
	| (';')? VARARGS
	| (';')? VIRTUAL
	| (';')? portabilityDirective
	;

enumeratedType
	: '(' (enumeratedTypeElement (',')?)+ ')'
	;

enumeratedTypeElement
	: ident '[' '=' expression ']'
	;

exceptionItem
	: ON (ident ':')? qualifiedIdent DO (statement)? (';')?
	;

exportsItem
	: ident (exportsSpecifier)*
	;

exportsSpecifier
	: (INDEX | NAME) expression
	;

exportsStatement
	: EXPORTS (exportsItem (',')?)+ ';'
	;

expression
	: simpleExpression (relOp simpleExpression)*
	;

expressionList
	: (expression (',')?)+
	;

expressionOrAssignment
	: expression
	| expression ':=' expression
	;

expressionOrRange
	: simpleExpression ('..' simpleExpression)?
	;

expressionOrRangeList
	: (expressionOrRange (',')?)+
	;

extendedIdent
	: ident
	| KEYWORD
	;

factor
	: atom
	| unaryOperator factor
	;

fancyBlock
	: (implementationDecl)* block
	;

fieldDecl
	: identList ':' type (portabilityDirective)* (';')?
	;

fieldSection
	: ((CLASS)? VAR)? (fieldDecl)*
	;

fileType
	: FILE
	| FILE OF qualifiedIdent
	;

forStatement
	: FOR ident ':=' expression (TO | DOWNTO) expression DO (statement)?
	| FOR ident IN expression DO (statement)?
	;

goal
	: program
	| package
	| unit
	;

gotoStatement
	: GOTO labelId
	;

ident
	: IDENTIFIER
	| SEMIKEYWORD
	| '&' IDENTIFIER
	| '&' SEMIKEYWORD
	| '&' KEYWORD
	;

identList
	: (ident (',')?)+
	;

ifStatement
	: IF expression THEN (statement)? (ELSE (statement)?)?
	;

implementationDecl
	: labelDeclSection
	| constSection
	| typeSection
	| varSection
	| methodImplementation
	| exportsStatement
	| assemblyAttribute
	;

implementationSection
	: IMPLEMENTATION (usesClause)? (implementationDecl)*
	;

initSection
	: END
	| block
	| INITIALIZATION (statementList)? (FINALIZATION (statementList)?)? END
	;

interfaceDecl
	: constSection
	| typeSection
	| varSection
	| methodHeading
	;

interfaceSection
	: INTERFACE (usesClause)? (interfaceDecl)*
	;

interfaceType
	: (INTERFACE | DISPINTERFACE) ('(' qualifiedIdent ')')? ('(' expression ')?')? (methodOrProperty)* END
	;

labelDeclSection
	: LABEL (labelId (',')?)+ ';'
	;

labelId
	: NUMBER
	| ident
	;

methodHeading
	: (CLASS)? (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR) qualifiedIdent ( ('(' (parameter (';')?)* ')')? (':' methodReturnType)? (directive)* | '=' ident ) (';')?
	;

methodImplementation
	: methodHeading fancyBlock ';'
	| methodHeading
	;

methodOrProperty
	: methodHeading
	| property
	;

methodReturnType
	: qualifiedIdent
	| STRING
	;

mulOp
	: '*'
	| '/'
	| DIV
	| MOD
	| AND
	| SHL
	| SHR
	;

openArray
	: ARRAY OF qualifiedIdent
	| ARRAY OF STRING
	| ARRAY OF FILE
	| ARRAY OF CONST
	;

package
	: PACKAGE qualifiedIdent ';' (requiresClause)? (usesClause)? (assemblyAttribute)* END '.'
	;

packedType
	: PACKED type
	;

parameter
	: (VAR | CONST | OUT)? identList (':' parameterType)? ('=' expression)?
	;

parameterExpression
	: expression (':' expression (':' expression)?)?
	;

parameterType
	: qualifiedIdent
	| STRING
	| FILE
	| openArray
	;

parenthesizedExpression
	: '(' expression ')'
	;

particle
	: NUMBER
	| STRINGLITERAL
	| ident
	| NIL
	| parenthesizedExpression
	| setLiteral
	| STRING
	| FILE
	;

pointerType
	: '^' type
	;

portabilityDirective
	: platform
	| deprecated
	| library
	| experimental
	;

procedureType
	: (PROCEDURE | FUNCTION) ('(' (parameter (';')?)* ')')? (':' methodReturnType)? (directive)* (OF OBJECT)? (directive)*
	;

program
	: (PROGRAM | LIBRARY) ident ('(' identList ')')? ';'
	| (usesClause)? (implementationDecl)* initSection '.'
	;

property
	: (CLASS)? PROPERTY ident ('(' (parameter (';')?)+ ')?')? (':' methodReturnType)? (propertyDirective)* ';'
	;

propertyDirective
	: ';' DEFAULT
	| DEFAULT expression
	| DISPID expression
	| IMPLEMENTS (qualifiedIdent (',')?)+
	| INDEX expression
	| NODEFAULT
	| READ expression
	| READONLY
	| STORED expression
	| WRITE expression
	| WRITEONLY
	;

qualifiedIdent
	: ident ('.' extendedIdent)*
	;

raiseStatement
	: RAISE (expression (AT expression)?)?
	;

recordHelperType
	: RECORD HELPER FOR qualifiedIdent (visibilitySection)* END
	;

recordType
	: RECORD (visibilitySection)* (variantSection)? END
	;

relOp
	: '='
	| '>'
	| '<'
	| '<='
	| '>='
	| '<>'
	| IN
	| IS
	| AS
	;

repeatStatement
	: REPEAT (statementList)? UNTIL expression
	;

requiresClause
	: REQUIRES (qualifiedIdent (',')?)+ ';'
	;

setLiteral
	: '(' (expressionOrRangeList)? ')?'
	;

setType
	: SET OF type
	;

simpleExpression
	: term (addOp term)*
	;

simpleStatement
	: bareInherited
	| expressionOrAssignment
	| gotoStatement
	| block
	| ifStatement
	| caseStatement
	| repeatStatement
	| whileStatement
	| forStatement
	| withStatement
	| tryStatement
	| raiseStatement
	;

statement
	: labelId ':' (simpleStatement)?
	| simpleStatement
	;

statementList
	: ((statement)? (';')?)+
	;

stringType
	: STRING
	| STRING '(' expression ')?'
	;

term
	: factor (mulOp factor)*
	;

tryStatement
	: TRY (statementList)? ( FINALLY (statementList)? | EXCEPT ( (statementList)? | (exceptionItem)* (ELSE (statementList)?)? ) END
	;

type
	: enumeratedType
	| expressionOrRange
	| arrayType
	| setType
	| fileType
	| recordHelperType
	| recordType
	| pointerType
	| stringType
	| procedureType
	| classHelperType
	| classOfType
	| classType
	| interfaceType
	| packedType
	;

typedConstant
	: expression
	| '(' (qualifiedIdent ':' typedConstant (';')?)+ ')'
	| '(' (typedConstant (',')?)+ ')'
	| '(' ')'
	;

typeDecl
	: ident '=' (TYPE)? type (portabilityDirective)* ';'
	| ident '=' CLASS ';'
	| ident '=' DISPINTERFACE ';'
	| ident '=' INTERFACE ';'
	;

typeSection
	: TYPE (typeDecl)+
	;

unaryOperator
	: NOT
	| '+'
	| '-'
	| '@'
	| INHERITED
	;

unit
	: UNIT ident (portabilityDirective)* ';' interfaceSection implementationSection initSection '.'
	;

usesClause
	: (USES | CONTAINS) (usedUnit (',')?)+ ';'
	;

usedUnit
	: ident
	| ident IN STRINGLITERAL
	;

varDecl
	: identList ':' type (portabilityDirective)* (ABSOLUTE expression | '=' typedConstant)? (portabilityDirective)* ';'
	;

variantGroup
	: expressionList ':' '('
	| (fieldDecl)* (variantSection)? ')' (';')?
	;

variantSection
	: CASE (ident ':')? qualifiedIdent OF (variantGroup)+
	;

varSection
	: (VAR | THREADVAR) (varDecl)+
	;

visibility
	: STRICT PRIVATE
	| STRICT PROTECTED
	| PRIVATE
	| PROTECTED
	| PUBLIC
	| PUBLISHED
	;

visibilitySection
	: (visibility)? (visibilitySectionContent)*
	;

visibilitySectionContent
	: fieldSection
	| methodOrProperty
	| constSection
	| typeSection
	;

whileStatement
	: WHILE expression DO (statement)?
	;

withStatement
	: WITH expressionList DO (statement)?
	;
