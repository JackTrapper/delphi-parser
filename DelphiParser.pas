unit DelphiParser;

{
Takes a set of tokens from a tokenizer, and turns them into a Syntax Tree.



	var
		root: TSyntaxNode2;

		root := TDelphiParser.ParseText(moCode.Text, '');


Note: It is not an Abstract Syntax Tree (AST), as it preserves trivia, whitespace, and tokens.
		This is useful for linters that want to read comments

Example
=======

DelphiTokenzier.pas implments the tokenizer.

	unit System.Generics.Collections;

Tokens (from lexer):
	unit				ptKeyword token (LeadingTrivia = [], TrailingTrivia = [Whitespace])
	System			ptIdentifier token
	.					ptDot token
	Generics			ptIdentifier token
	.					ptDot token
	Collections		ptIdentifier token
	;					ptSemiColon token (TrailingTrivia = [EOL])

Which then forms the tree:

ntCompilationUnit
└─ ntUnitDeclaration
	├─ ptIdentifier('unit')
	├─ ntIdentifier('System.Generics.Collections')
	│   ├─ ptIdentifier('System')
	│   ├─ ptDot('.')
	│   ├─ ptIdentifier('Generics')
	│   ├─ ptDot('.')
	│   └─ ptIdentifier('Collections')
	└─ ptSemicolon(';')

NOTE: the children are a mix of syntax nodes and token references.
		Tokens carry their trivia inside, so the tree doesn’t need explicit trivia nodes.


1. The basic contract
=====================

Every `TSyntaxNode2` is one of:

	- a **nonterminal** node (e.g. ntIdentifier, ntIfStmt, ntParamList)
	- a **token** node (`ntToken`),
	- or a **trivia** node (`ntTrivia`).

	- **Tokens are never duplicated.** Each token from the lexer appears *once* in the tree,
		in the place the grammar expects it.

	- **Tokens own their trivia.** Leading/trailing whitespace/comments are children of the token node.

2. Example: `unit System.Generics.Collections;`
===============================================

This source has **6 tokens**:

	`unit`			(keyword),
	`System`			(identifier)
	`.`				(dot)
	`Generics`		(identifier)
	`.`				(dot)
	`Collections`	(identifier)
	`;`				(semicolon)

Tree:

ntUnitDeclaration
 ├─ ntToken('unit')          [slot='UnitKeyword']
 │   └─ ntTrivia(' ')        (leading space after 'unit')
 ├─ ntIdentifier             [slot='Name']
 │   ├─ ntToken('System')    [slot='Identifier']
 │   ├─ ntToken('.')         [slot='Dot']
 │   ├─ ntToken('Generics')  [slot='Identifier']
 │   ├─ ntToken('.')         [slot='Dot']
 │   └─ ntToken('Collections')[slot='Identifier']
 └─ ntToken(';')             [slot='Semicolon']

3. Rules of thumb
=================


**Nonterminal = grammar production.**

	- *If statement*   → `ntIfStmt`    node with children for `IfKeyword`, `Condition`, `ThenKeyword`, `ThenStmt`, optional `ElseKeyword`, `ElseStmt`.
	- *Parameter list* → `ntParamList` node with children for `(`, parameters, commas, `)`.

**Every keyword/punctuation/identifier is its own token node.**

	- Never stored as an attribute — always as a child token node.
	- Attributes are for *semantic info* (e.g. `anName` = assembled string).

- Trivia attaches to tokens, never to nonterminals.

	- Comments/whitespace live under the nearest token.
	- This makes the tree fully round-trippable.
	- No "flat list of tokens" at the root.
	- To get “all tokens”, traverse the tree and collect leaf tokens.
	- `FullSpan` of a node = span from the first token under it to the last.






	Roslyn Lexer: https://github.com/dotnet/roslyn/blob/main/src/Compilers/CSharp/Portable/Parser/Lexer.cs#L2461


Lexer
├── Lex() [public]
│   ├── LexTrivia() [private]
│   │   ├── ScanWhitespace()
│   │   ├── ScanSingleLineComment()
│   │   ├── ScanMultiLineComment()
│   │   ├── ScanDocumentationComment()
│   │   └── ScanPreprocessorDirective()
│   ├── ScanSyntaxToken() [private]
│   │   ├── ScanIdentifierOrKeyword()
│   │   ├── ScanNumericLiteral()
│   │   ├── ScanStringLiteral()
│   │   ├── ScanCharLiteral()
│   │   ├── ScanInterpolatedStringLiteral()
│   │   └── ScanOperatorOrPunctuation()
│   ├── LexDirective() [private]
│   │   └── ScanPreprocessorDirective()
│   └── LexXmlToken() [private]
│       ├── LexXmlElementTagToken()
│       ├── LexXmlAttributeTextToken()
│       ├── LexXmlCDataSectionTextToken()
│       ├── LexXmlCommentTextToken()
│       ├── LexXmlProcessingInstructionTextToken()
│       └── LexXmlCrefOrNameToken()
└── LexConflictMarker() [public]

}

interface

uses
	SysUtils,
	ActiveX,
	ComObj,
	Contnrs,
	System.Types,
	System.Generics.Collections,
	Classes,
	DelphiTokenizer;

type
	TSyntaxToken = DelphiTokenizer.TSyntaxToken; // import for helpfullness

type
{
	Every syntax node has a .NodeType
}
	TSyntaxNodeType = (
		ntUnknown,

		ntCompilationUnit,		// the root of any parsing, used to hold directives and other file-level settings
		ntUnitDeclaration,		// Unit Declaration. e.g. "unit Contoso;"				http://dgrok.excastle.com/Grammar.html#Unit
		ntProgram,					// Program Declaration. e.g. "program TestApp;"
		ntLibrary,					// Library Declaration. e.g. "library Contoso;"


		ntUsedUnit,					// The name of a unit in a uses clause

		ntQualifiedIdentifier,	// A possibly qualified identifier. e.g. "Winapi.msxml"
		ntIdentifier,				// An identifier. e.g. "msxml"

		ntPortabilityDirective,	// PLATFORM | DEPRECATED <String> | LIBRARY | EXPERIMENTAL

		ntInterfaceSection,

		ntTypeSection,				// type TSpecial =
		ntTypeDecl,					// TSpecial = ...

		ntAncestorList,			// TTriangle = class(TShape, IShape, ILogger);

		ntAbsolute,
		ntAddr,
		ntAlignmentParam,
		ntAnd,
		ntAnonymousMethod,
		ntArguments,
		ntAs,
		ntAssign,
		ntAt,
		ntAttribute,
		ntAttributes,
		ntBounds,							// an array bounds
		ntDimension,
		ntCall,
		ntCase,
		ntCaseElse,
		ntCaseLabel,
		ntCaseLabels,
		ntCaseSelector,

		ntConstants,						// const keyword
		ntConstant,							// individual ident = value pair
		ntResourceStrings,				// the resourecestring keyword
		ntResourceString,					// individual ident = 'string' pair


		// Generic type constraints (e.g. U: class, object
		ntConstraints,
		ntClassConstraint,
		ntConstructorConstraint,
		ntRecordConstraint,



		ntContains,
		ntDefault,
		ntDeref,
		ntDiv,
		ntDot,
		ntDownTo,
		ntElement,
		ntElse,
		ntEmptyStatement,
		ntEnum,
		ntEqual,
		ntExcept,
		ntExceptionHandler,
		ntExports,
		ntExpression,
		ntRelationalExpression,			// [Left] [Operator] [Right}
		ntExpressions,
		ntExternal,
		ntFDiv,
		ntField,
		ntFields,
		ntFinalization,
		ntFinally,
		ntFor,
		ntFrom,
		ntGeneric,
		ntGoto,
		ntGreater,
		ntGreaterEqual,
		ntGuid,
		ntHelper,
		ntIf,
		ntImplementation,
		ntImplements,
		ntIn,
		ntIndex,
		ntIndexed,
		ntInherited,
		ntInitialization,
		ntIs,
		ntLabel,
		ntLHS,
		ntLiteral,
		ntLower,
		ntLowerEqual,
		ntMessage,
		ntMethod,
		ntMod,
		ntMul,
		ntName,
		ntNamedArgument,
		ntNotEqual,
		ntNot,
		ntPackage,
		ntParameter,
		ntParameters,
		ntPath,
		ntPositionalArgument,
		ntPropertySpecifiers,
		ntProtected,
		ntPrivate,
		ntProperty,
		ntPublic,
		ntPublished,
		ntRaise,
		ntRead,
		ntRepeat,
		ntRequires,  				// requires package
		ntResolutionClause,
		ntReturnType,
		ntRHS,
		ntRoundClose,
		ntRoundOpen,
		ntSet,
		ntShl,
		ntShr,
		ntStatement,
		ntStatements,
		ntStrictPrivate,
		ntStrictProtected,

		// AddOp: + - OR XOR
		ntAdd,			// +
		ntSub,			// -
		ntOr,				// OR
		ntXor,			// XOR


		ntSubrange,
		ntThen,
		ntTo,
		ntToken,
		ntTrivia,		// Leaf representing a single trivia run (whitespace/comment/disabled)
		ntTry,
		ntType,
		ntTypeArgs,

		ntTypeParam,
		ntTypeParams,
		ntValue,
		ntVariable,
		ntVariables,
		ntUnaryMinus,
		ntHintDirectives,
		ntHintDirective,
		ntUses,
		ntWhile,
		ntWith,
		ntWrite,

		ntAnsiComment,
		ntBorComment,
		ntSlashesComment
	);

// Convert TSyntaxNodeType enumer to string (e.g. ntInterfaceSection --> 'ntInterfaceSection')
function SyntaxNodeTypeToStr(NodeType: TSyntaxNodeType): string;

type
{
	A TSyntaxNode has a property bag called Attributes. E.g.:

		syntaxNode.Attributes[TAttributeName] := 'string';
}
	TAttributeName = (
		anName,
		anMissing,				// tells consumers this token was synthesized
		anType,					// for ntType nodes, indicates which Type (TAttributeValue) it is. e.g. anClass: class
		anClass,
		anForwarded,
		anKind,					// TAttributeValue
		anVisibility,
		anCallingConvention,
		anPath,
		anMethodBinding,
		anReintroduce,
		anOverload,
		anAbstract,				// class abstract
		anSealed,				// class sealed
		anInline,
		anAlign,					// the '8' associated with $ALIGN 8 or $A8
		anTriviaKind,
		anText,
		anSlot,
		anTokenKind,
		anDeprecated,			// marked as deprecated, with an @anText='Use  comment
		anValueText,
		anLibrary,				// the unit is marked as unit Contoso; library;
		anPlatform,				// the unit is marked as unit Contoso; platform;
		anExperimental,		// the unit is marked as unit Contoso; experimental
		anDistinct				// type TCustomerID = type Integer;
	);

function AttributeNameToStr(AttributeName: TAttributeName): string;

type
	TAttributeValue = (
			avAsm,
			avTrue,
			avFunction,
			avProcedure,
			avClassOf,
			avClass,
			avConst,
			avConstructor,
			avDestructor,
			avEnum,
			avInterface,
			avNil,
			avNumeric,
			avOut,
			avPointer,
			avName,
			avString,
			avSubRange,
			avVar,
			avDispInterface);

function AttributeValueToStr(const AttributeValue: TAttributeValue): string;


type
	// The kind of trivia captured under tokens
	TTriviaKind = (tkWhitespace, tkEOL, tkLineComment, tkBlockComment, tkDisabled);

	// A trivia item as produced by the lexer
	TTrivia = record
		Kind: TTriviaKind; // classification
		Text: string;      // exact bytes from source
	end;

const
	 // List of directive tokens that apply to methods.
	ClassMethodDirectiveEnum = [
		ptAbstract,
		ptCdecl,
		ptDynamic,
		ptMessage,
		ptOverride,
		ptOverload,
		ptPascal,
		ptRegister,
		ptReintroduce,
		ptSafeCall,
		ptStdCall,
		ptVirtual,
		ptDeprecated,		// PortabilityDirective
		ptLibrary,			// PortabilityDirective
		ptPlatform,			// PortabilityDirective
		ptExperimental,	// PortabilityDirective
		ptStatic,
		ptInline,
		ptFinal,
		ptDispId
	];

type
	ESyntaxError = class(Exception)
	private
		FPosXY: TPoint;
	public
		constructor Create(const Msg: string);
		constructor CreateFmt(const Msg: string; const Args: array of const);
		constructor CreatePos(const Msg: string; aPosXY: TPoint);
		property PosXY: TPoint read FPosXY write FPosXY;
	end;


	EParserException = class(ESyntaxError)
	private
		FPosXY: TPoint;
		FFilename: string;
	public
		constructor Create(X, Y: Integer; Filename: string; Msg: string);
	end;


	TMessageEvent = procedure(Sender: TObject; const Msg: string; X, Y: Integer) of object;

	TSyntaxNode2 = class; //forward

	// Helper class that makes it easy to get AsNode AsToken
	TSyntaxNodeOrToken = class
	private
		FObject: TObject;

		function get_IsNode: Boolean;
		function get_IsToken: Boolean;
		function get_AsNode: TSyntaxNode2;
		function get_AsToken: TSyntaxToken;
	public
		constructor Create(SyntaxNode: TSyntaxNode2); overload;
		constructor Create(SyntaxToken: TSyntaxToken); overload;
		destructor Destroy; override;

		property IsNode: Boolean			read get_IsNode;
		property IsToken: Boolean			read get_IsToken;
		property AsNode: TSyntaxNode2		read get_AsNode;
		property AsToken: TSyntaxToken	read get_AsToken;
	end;


	// It's called TSyntaxNode2 because TSyntaxNode is already taken by DelphiAST
	TSyntaxNode2 = class
	private
		FNodeType: TSyntaxNodeType;
		FChildNodes: TObjectList<TSyntaxNodeOrToken>;
		FAttributes: TDictionary<TAttributeName, string>;
		FCurrentColumn: Integer;
		FCurrentLine: Integer;
		FFilename: string;

		procedure AddChild(ChildNode: TSyntaxNode2); overload;
		procedure AddChild(ChildNode: TSyntaxToken); overload;

		function get_Attributes(Attribute: TAttributeName): string;
		procedure set_Attributes(Attribute: TAttributeName; const Value: string);
		function get_HasChildren: Boolean;
		function get_Value: string;	// @anName
		function get_DisplayName: string;
	public
		constructor Create(SyntaxNodeType: TSyntaxNodeType);
		destructor Destroy; override;

		function ToString: string; override;

		class function DumpTree(Node: TSyntaxNode2): string;

		property NodeType: TSyntaxNodeType read FNodeType;
		property Value: string read get_Value;    // alias of anName attribute

		property Attributes[Attribute: TAttributeName]: string read get_Attributes write set_Attributes; default;

		property ChildNodes: TObjectList<TSyntaxNodeOrToken> read FChildNodes;

		property DisplayName: string read get_DisplayName;
		property HasChildren: Boolean read get_HasChildren; // if ChildNodes contains items
		property Line: Integer read FCurrentLine;
		property Column: Integer read FCurrentColumn;
	end;

{
	The root class that does the parsing.
}
	TDelphiParser = class
	private
		// Infrastructure
		FTokens: TObjectList<TSyntaxToken>; // non-owning references; syntax tree owns token lifetime
		FCurrent: Integer; //current token index; starts at -1, and you have to NextToken to get to the first

		FCompilerDirectives: TStrings; // the current compiler directives. Defaults to your directives (see GetDefaultConditionalDirectives function)
		FInterfaceOnly: Boolean;
		FScopedEnums: Boolean;

		FOnMessage: TMessageEvent;

		function ParseVariableTail: TSyntaxNode2;
		procedure set_IncludeHandler(IncludeHandler: IUnknown{IIncludeHandler});

		procedure DoMessage(const Msg: string; X, Y: Integer);


		procedure MoveMembersToVisibilityNodes(TypeNode: TSyntaxNode2);

		procedure GetDefaultConditionalDirectives(TargetList: TStrings);

		function PoisonNode(NodeType: TSyntaxNodeType=ntUnknown): TSyntaxNode2; //special NotImplemented node

		// Property getters
		function get_CurrentToken: TSyntaxToken;
		function get_CurrentTokenKind: TptTokenKind; virtual;	//deprecated 'Use FCurrentToken.TokenKind instead';
		function get_CurrentTokenExID: TptTokenKind; virtual;		// the possible identifier type of the token
		function get_CurrentTokenGenID: TptTokenKind; virtual;
		function get_NextFewTokens: string;
	protected
		// Not used
		function NodeListToString(NamesNode: TSyntaxNode2): string;
		procedure RearrangeVarSection(const VarSect: TSyntaxNode2);
		property NextFewTokens: string read get_NextFewTokens;

		function ParseClassProperty: TSyntaxNode2;


	private
		procedure Log(const s: string);

		// Token navigation

		// The next token info
		function PeekToken: TSyntaxToken; overload;
		function PeekToken(n: Integer): TSyntaxToken; overload;
		function PeekTokenKind:  TptTokenKind; // helper for --> PeekToken(1).TokenKind
		function PeekTokenExID:  TptTokenKind; // helper for --> PeekToken(1).ExID
		function PeekTokenGenID: TptTokenKind; // helper for --> PeekToken(1).GenID


		// Convenience: like Expect but without slot
		procedure NextToken; // advances to the next token (which is also how FCurrentToken is set)
		function EatToken: TSyntaxToken; overload; //
		function EatToken(ExpectedTokenKind: TptTokenKind): TSyntaxToken; overload; // emits an error message if the CurrentToken kind is not Sym.

		// Assertion utility routines. Moves to next token as long as current token is specified type
		function EatTokenEx(    ExpectedTokenKind: TptTokenKind): TSyntaxToken;	// emits an error message if the CurrentToken ExID is not Sym.

		// Output an error message
		function SynError(Error: string): TSyntaxNode2;
		function SynErrorFmt(const Error: string; const Args: array of const): TSyntaxNode2;


		// ***********************************************************************
		// Start of the production methods
		// ***********************************************************************

		// Main parsing function.
		function ParseCore: TSyntaxNode2; virtual; //as a nice way to split plumbing from grammer




		// ParseCore then turns around and calls one of:
		function ParseUnit: TSyntaxNode2;						// e.g. unit Unit1;
		function ParseProgramFile: TSyntaxNode2;				// e.g. program Program1;
		function ParseLibraryFile: TSyntaxNode2;				// e.g. library Library1;
		function ParsePackage: TSyntaxNode2;					// e.g. package Package1;
		function ParseScriptFile: TSyntaxNode2; 				// If the text is not one of file types, we can parse some contents


		function ParseUsesClause: TSyntaxNode2;				// uses a,b,c;
			function IsPossibleUsesClause: Boolean;

		function ParseUsedUnit: TSyntaxNode2;					// a

		function ParseAssemblyAttribute: TSyntaxNode2;		// ptSquareOpen

		function ParseQualifiedIdent: TSyntaxNode2;	// Contoso.Grunion;
		function ParseInterfaceSection: TSyntaxNode2;
		function ParseInterfaceDeclaration: TSyntaxNode2;

		function ParsePortabilityDirective: TSyntaxNode2;	// ntPortabilityDirective
			function IsPossiblePortabilityDirective: Boolean;

			function ParseDirectiveDeprecated: TSyntaxNode2;
			function ParseDirectiveLibrary: TSyntaxNode2;
			function ParseDirectivePlatform: TSyntaxNode2;



		function ParseStringType: TSyntaxNode2;				// ntType

		function ParseTypeSection: TSyntaxNode2;				// ntTypeSection
			function IsPossibleTypeSection: Boolean;

		function ParseTypeDeclaration: TSyntaxNode2;			// ntTypeDecl					http://dgrok.excastle.com/Grammar.html#TypeDecl
			function IsPossibleTypeDeclaration: Boolean; 	// peeks ahead to see if the next production is a TypeDeclaration, meaning you can call ParseTypeDeclaration()

		function ParseType: TSyntaxNode2;						// ntType						http://dgrok.excastle.com/Grammar.html#Type

		// ntType, TFoo = <Type>
		function ParseEnumeratedType: TSyntaxNode2;
		function ParseExpressionOrRange: TSyntaxNode2;
		//ParseArrayType
		//ParseSetType
		//ParseFileType
		//ParseRecordHelperType
		function ParseClassType: TSyntaxNode2;					// ntType(@anType=atClass)
		function ParseAncestorList: TSyntaxNode2;				// TWidget = class(TShape, IShape, IWidget)
		function ParseClassMemberList: TSyntaxNode2;			// the ancestor of a class TCustomer=class(TShape, ILog, IFoo)
		function ParseVisibilitySection: TSyntaxNode2;
			function IsPossibleVisibilitySection: Boolean;

		function ParseVisibilitySectionContent: TSyntaxNode2;
			function IsPossibleVisibilitySectionContent: Boolean;

		function ParseConstSection: TSyntaxNode2;				// ntConstants
			function IsPossibleConstSection: Boolean;

		function ParseConstantDecl: TSyntaxNode2;				// ntConstant
			function IsPossibleConstantDecl: Boolean;


		function ParseResStringSection: TSyntaxNode2;		// ntResourceStrings
			function IsPossibleResStringSection: Boolean;
		function ParseResourceStringDecl: TSyntaxNode2;
			function IsPossibleResourceStringDecl: Boolean;

		function ParseMethodOrProperty: TSyntaxNode2;
		function ParseFieldDecl: TSyntaxNode2;


		function ParseClassHelperType: TSyntaxNode2;				// ntHelper
		function ParseSimpleType: TSyntaxNode2;
		function ParseSubrangeType: TSyntaxNode2;
		function ParseStructuredType: TSyntaxNode2;
		function ParseProceduralType: TSyntaxNode2;
		function ParseTypeId: TSyntaxNode2;
		function ParsePointerType: TSyntaxNode2;

		function ParseVarSection: TSyntaxNode2;
			function IsPossibleVarSection: Boolean;

		function ParseVarDecl: TSyntaxNode2;


		function ParseImplementationSection: TSyntaxNode2;

		function ParseInitializationSection: TSyntaxNode2;
		function ParseFinalizationSection: TSyntaxNode2;

		function ParseAnonymousMethodType: TSyntaxNode2;
		function ParseArrayType: TSyntaxNode2;
		function ParseBlock: TSyntaxNode2;
		function ParseCharString: TSyntaxNode2;


		function ParseClassMethodOrProperty: TSyntaxNode2;
		function ParseClassTypeEnd: TSyntaxNode2;
		function ParseClassVisibility: TSyntaxNode2;
		function ParseConstantColon: TSyntaxNode2;
		function ParseConstantEqual: TSyntaxNode2;

		function ParseConstantExpression: TSyntaxNode2;
		function ParseConstantType: TSyntaxNode2;

		function ParseImplementationDecl: TSyntaxNode2;
			function IsPossibleImplementationDecl: Boolean;

		function ParseDesignator: TSyntaxNode2;

		// parse directive
		function ParseDirective: TSyntaxNode2;
			function IsPossibleDirective: Boolean;

		function ParseDirective16Bit: TSyntaxNode2;
		function ParseDirectiveBindingMessage: TSyntaxNode2;
		function ParseDirectiveLocal: TSyntaxNode2;
		function ParseDirectiveVarargs: TSyntaxNode2;

		// Involved in tree building
		function ParseAccessSpecifier: TSyntaxNode2;
		function ParseAddOp: TSyntaxNode2;
		function ParseAddressOp: TSyntaxNode2;
		function ParseAnonymousMethod: TSyntaxNode2;
		function ParseArrayConstant: TSyntaxNode2;
		function ParseAssemblerStatement: TSyntaxNode2;
		function ParseAsOp: TSyntaxNode2;
		function ParseAtExpression: TSyntaxNode2; // raise at
		function ParseCaseLabel: TSyntaxNode2;
		function ParseCaseLabelList: TSyntaxNode2;
		function ParseCaseSelector: TSyntaxNode2;
		function ParseCaseStatement: TSyntaxNode2;
		function ParseClassClass: TSyntaxNode2;
		function ParseClassConstraint: TSyntaxNode2;
		function ParseClassField: TSyntaxNode2;
		function ParseClassMethodResolution: TSyntaxNode2;
		function ParseClassMethodHeading: TSyntaxNode2;

		function ParseMethodHeading: TSyntaxNode2;
			function IsPossibleMethodHeading: Boolean;

			// Called by ParseMethodHeading to specially handle each:
			function ParseProcedureMethodHeading: TSyntaxNode2;		// ptProcedure
			function ParseFunctionMethodHeading: TSyntaxNode2;			// ptFunction
			function ParseConstructorMethodHeading: TSyntaxNode2;		// ptConstructor
			function ParseDestructorMethodHeading: TSyntaxNode2;		// ptDestructor
			function ParseOperatorMethodHeading: TSyntaxNode2;			// ptOperator

		procedure ClassProcedureHeading(ParentNode: TSyntaxNode2);
		procedure ConstructorHeading(ParentNode: TSyntaxNode2);
		procedure ClassFunctionHeading(ParentNode: TSyntaxNode2);
		procedure ClassMethod(ParentNode: TSyntaxNode2);
		procedure ConstructorName(ParentNode: TSyntaxNode2);
		procedure DestructorName(ParentNode: TSyntaxNode2);
		procedure DirectiveBinding(ParentNode: TSyntaxNode2);
		procedure DirectiveCalling(ParentNode: TSyntaxNode2);
		procedure DirectiveInline(ParentNode: TSyntaxNode2);
		procedure ExternalDirectiveTwo(ParentNode: TSyntaxNode2);
		procedure FunctionProcedureBlock(ParentNode: TSyntaxNode2);
		procedure InterfaceMemberList(ParentNode: TSyntaxNode2);
		procedure ObjectConstructorHeading(ParentNode: TSyntaxNode2);
		procedure ObjectDestructorHeading(ParentNode: TSyntaxNode2);
		procedure ObjectFunctionHeading(ParentNode: TSyntaxNode2);
		procedure ObjectMemberList(ParentNode: TSyntaxNode2);
		procedure ObjectMethodDirective(ParentNode: TSyntaxNode2);
		procedure ObjectMethodHeading(ParentNode: TSyntaxNode2);
		procedure ObjectProcedureHeading(ParentNode: TSyntaxNode2);
		procedure ObjectType(ParentNode: TSyntaxNode2);
		procedure ProceduralDirective(ParentNode: TSyntaxNode2);
		procedure PropertyName(ParentNode: TSyntaxNode2);
		procedure RecordAlignValue(ParentNode: TSyntaxNode2);
		procedure RecordType(ParentNode: TSyntaxNode2);

		procedure ClassMethodDirective(ParentNode: TSyntaxNode2);

		procedure ClassOperatorHeading(ParentNode: TSyntaxNode2);
		procedure DestructorHeading(ParentNode: TSyntaxNode2);

		function ParseProperty: TSyntaxNode2;
			function IsPossibleProperty: Boolean;
//			property : Integer read FCurrent;

		function ParseClassOfType: TSyntaxNode2;
		function ParseConstParameter: TSyntaxNode2;			// ntParameter
		function ParseVarParameter: TSyntaxNode2;

		function ParseConstantName: TSyntaxNode2;

		function ParseConstantValue: TSyntaxNode2;
		function ParseConstantValueTyped: TSyntaxNode2;
		function ParseConstructorConstraint: TSyntaxNode2;

		function ParseDotOp: TSyntaxNode2;
		function ParseElseStatement: TSyntaxNode2;
		function ParseEmptyStatement: TSyntaxNode2;
		function ParseExceptBlock: TSyntaxNode2;

		function ParseExceptionItem: TSyntaxNode2;
		function ParseExceptionBlockElseBranch: TSyntaxNode2;
		function ParseExceptionHandler: TSyntaxNode2;
		function ParseExceptionVariable: TSyntaxNode2;
		function ParseExceptionClassTypeIdentifier: TSyntaxNode2;
		function ParseExceptionHandlerList: TSyntaxNode2;
		function ParseExceptionIdentifier: TSyntaxNode2;


		function ParseExportStatement: TSyntaxNode2;
		function ParseExportsItem: TSyntaxNode2;
		function ParseExportsName: TSyntaxNode2;

		function ParseExpression: TSyntaxNode2;

		function ParseSimpleExpression: TSyntaxNode2;


		function ParseExpressionList: TSyntaxNode2;

		function ParseExternalDirective: TSyntaxNode2; // ptExternal
		function ParseExternalDirectiveThree: TSyntaxNode2;

		function ParseFieldName: TSyntaxNode2;
		function ParseFinallyBlock: TSyntaxNode2;
		function ParseFormalParameterList: TSyntaxNode2;
		function ParseForStatement: TSyntaxNode2;
		function ParseForStatementDownTo: TSyntaxNode2;
		function ParseForStatementFrom: TSyntaxNode2;
		function ParseForStatementIn: TSyntaxNode2;
		function ParseForStatementTo: TSyntaxNode2;

		function ParseFunctionMethodName: TSyntaxNode2;
		function ParseForwardDeclaration: TSyntaxNode2;
		function ParseFunctionMethodDeclaration: TSyntaxNode2;
		function ParseIdentifierList: TSyntaxNode2;
		function ParseInlineConstSection: TSyntaxNode2;
		function ParseInlineStatement: TSyntaxNode2;
		function ParseInParameter: TSyntaxNode2;
		function ParseInterfaceHeritage: TSyntaxNode2;
		function ParseLabelDeclarationSection: TSyntaxNode2;
		function ParseLabeledStatement: TSyntaxNode2;

		function ParseFormalParameterType: TSyntaxNode2;
		function ParseObjectField: TSyntaxNode2;
		function ParseObjectHeritage: TSyntaxNode2;

		function ParsePropertyDirective: TSyntaxNode2;
			function IsPossiblePropertyDirective: Boolean;

		function ParseObjectTypeEnd: TSyntaxNode2;
		function ParseObjectVisibility: TSyntaxNode2;
		function ParseOrdinalIdentifier: TSyntaxNode2;
		function ParseOrdinalType: TSyntaxNode2;
		function ParseParameterNameList: TSyntaxNode2;
		function ParseProceduralDirectiveOf: TSyntaxNode2;
		function ParseProcedureMethodName: TSyntaxNode2;
		function ParsePropertyDefault: TSyntaxNode2;
		function ParsePropertyInterface: TSyntaxNode2;
		function ParsePropertySpecifiers: TSyntaxNode2;

		function ParseReadAccessIdentifier: TSyntaxNode2;
		function ParseRealIdentifier: TSyntaxNode2;
		function ParseRealType: TSyntaxNode2;
		function ParseRecordAlign: TSyntaxNode2;
		function ParseRecordVariant: TSyntaxNode2;
		function ParseSetType: TSyntaxNode2;

		function ParseStatement: TSyntaxNode2;
//			function IsPossibleStatement: Boolean;

		function ParseStatementOrExpression: TSyntaxNode2;
		function ParseStatements: TSyntaxNode2;
		function ParseStorageExpression: TSyntaxNode2;
		function ParseStorageIdentifier: TSyntaxNode2;
		function ParseStorageNoDefault: TSyntaxNode2;
		function ParseStorageSpecifier: TSyntaxNode2;
		function ParseStorageStored: TSyntaxNode2;

		function ParseDispIDSpecifier: TSyntaxNode2;
		function ParseEnumeratedTypeElement: TSyntaxNode2;
		function ParseFactor: TSyntaxNode2;
		function ParseFieldDeclaration: TSyntaxNode2;
		function ParseFieldList: TSyntaxNode2;
		function ParseFieldNameList: TSyntaxNode2;
		function ParseFileType: TSyntaxNode2;
		function ParseFormalParameterSection: TSyntaxNode2;
		function ParseFunctionProcedureName: TSyntaxNode2;
		function ParseGotoStatement: TSyntaxNode2;
		function ParseIfStatement: TSyntaxNode2;

		function ParseIdent: TSyntaxNode2;				// ntIdentifier
			function IsPossibleIdentifier: Boolean;

		function ParseImplementsSpecifier: TSyntaxNode2;
		function ParseIndexSpecifier: TSyntaxNode2;
		function ParseIndexOp: TSyntaxNode2;
		function ParseInheritedStatement: TSyntaxNode2;
		function ParseInheritedVariableReference: TSyntaxNode2;
		function ParseInlineVarDeclaration: TSyntaxNode2;
		function ParseInlineVarSection: TSyntaxNode2;
		function ParseInterfaceGUID: TSyntaxNode2;
		function ParseInterfaceType: TSyntaxNode2;

		function ParseLabelId: TSyntaxNode2;
			function IsPossibleLabelID: Boolean;

		function ParseMainUsesClause: TSyntaxNode2;
		function ParseMainUsedUnitStatement: TSyntaxNode2;
		function ParseMethodKind: TSyntaxNode2;
		function ParseMultiplicativeOperator: TSyntaxNode2;
		function ParseNotOp: TSyntaxNode2;
		function ParseNilToken: TSyntaxNode2;
		function ParseNumber: TSyntaxNode2;
		function ParseObjectNameOfMethod: TSyntaxNode2;
		function ParseOutParameter: TSyntaxNode2;
		function ParseParameterFormal: TSyntaxNode2;
		function ParseParameterName: TSyntaxNode2;
		function ParsePointerSymbol: TSyntaxNode2;
		function ParseProcedureDeclarationSection: TSyntaxNode2;
		function ParseProcedureProcedureName: TSyntaxNode2;
		function ParsePropertyParameterList: TSyntaxNode2; // not done
		function ParseRaiseStatement: TSyntaxNode2;
		function ParseRecordConstraint: TSyntaxNode2;
		function ParseRecordFieldConstant: TSyntaxNode2;

		function ParseRelativeOperator: TSyntaxNode2;
		function ParseRepeatStatement: TSyntaxNode2;
		function ParseResourceDeclaration: TSyntaxNode2;
		function ParseResourceValue: TSyntaxNode2;

		function ParseRequiresClause: TSyntaxNode2;
			function IsPossibleRequiresClause: Boolean;

		function ParseRequiresIdentifier: TSyntaxNode2;
		function ParseRequiresIdentifierId: TSyntaxNode2;
		function ParseReturnType: TSyntaxNode2;
		function ParseRoundClose: TSyntaxNode2;
		function ParseRoundOpen: TSyntaxNode2;
		function ParseSetConstructor: TSyntaxNode2;
		function ParseSetElement: TSyntaxNode2;

		function ParseSimpleStatement: TSyntaxNode2;
			//function IsPossibleSimpleStatement: Boolean;

		function ParseStatementList: TSyntaxNode2;
		function ParseStorageDefault: TSyntaxNode2;
		function ParseStringConst: TSyntaxNode2;
		function ParseStringConstSimple: TSyntaxNode2;
		function ParseStringStatement: TSyntaxNode2;
		function ParseThenStatement: TSyntaxNode2;
		function ParseTryStatement: TSyntaxNode2;

		function ParseStringIdentifier: TSyntaxNode2;
		function ParseStringTypeInternal: TSyntaxNode2;
		function ParseTagField: TSyntaxNode2;
		function ParseTagFieldName: TSyntaxNode2;
		function ParseTagFieldTypeName: TSyntaxNode2;
		function ParseTerm: TSyntaxNode2;
		function ParseTypedConstant: TSyntaxNode2;
		function ParseTypeSimple: TSyntaxNode2;

		function ParseUnaryMinus: TSyntaxNode2;

		function ParseVarAbsolute: TSyntaxNode2;
		function ParseVarEqual: TSyntaxNode2;

		function ParseVariable: TSyntaxNode2;
		function ParseVariableReference: TSyntaxNode2;
		function ParseVariantIdentifier: TSyntaxNode2;
		function ParseVariantSection: TSyntaxNode2;
		function ParseIdentList: TSyntaxNode2;

		function ParseVisibilityAutomated: TSyntaxNode2;
		function ParseVisibilityPrivate: TSyntaxNode2;
		function ParseVisibilityProtected: TSyntaxNode2;
		function ParseVisibilityPublic: TSyntaxNode2;
		function ParseVisibilityPublished: TSyntaxNode2;
		function ParseVisibilityStrictPrivate: TSyntaxNode2;
		function ParseVisibilityStrictProtected: TSyntaxNode2;
		function ParseWhileStatement: TSyntaxNode2;
		function ParseWithExpressionList: TSyntaxNode2;
		function ParseWithStatement: TSyntaxNode2;
		function ParseWriteAccessIdentifier: TSyntaxNode2;

		// Generics
		function ParseTypeArgs: TSyntaxNode2;
		function ParseTypeParams: TSyntaxNode2;
		function ParseTypeParam: TSyntaxNode2;						// T, U: class, constructor; V: IComparable<T>>= class
			function ParseConstraintTypeRef: TSyntaxNode2;		// IComparable<T>>= class
			function ParseTypeParamConstraint: TSyntaxNode2;

		function ParseConstraintList: TSyntaxNode2;
		function ParseConstraint: TSyntaxNode2;
		//end generics


		// Attributes
		// ==========
		// This is the syntax for custom attributes, based quite strictly on the
		// ECMA syntax specifications for C#, but with a Delphi expression being
		// used at the bottom as opposed to a C# expression. -JThurman 2004-03-21
		function ParseGlobalAttributeTargetSpecifier: TSyntaxNode2;
		function ParseGlobalAttributeTarget: TSyntaxNode2;
		function ParseAttributeSections: TSyntaxNode2;
		function ParseAttributeSection: TSyntaxNode2;
		function ParseAttributeTargetSpecifier: TSyntaxNode2;
		function ParseAttributeTarget: TSyntaxNode2;
		function ParseAttributeList: TSyntaxNode2;
		function ParseAttribute: TSyntaxNode2;
		function ParseAttributeName: TSyntaxNode2;
		function ParseAttributeArguments: TSyntaxNode2;
		function ParsePositionalArgumentList: TSyntaxNode2;
		function ParsePositionalArgument: TSyntaxNode2;
		function ParseNamedArgumentList: TSyntaxNode2;
		function ParseNamedArgument: TSyntaxNode2;
		function ParseAttributeArgumentName: TSyntaxNode2;
		function ParseAttributeArgumentExpression: TSyntaxNode2;

		property CurrentToken:      TSyntaxToken read get_CurrentToken;		// read-only
		property CurrentTokenKind:  TptTokenKind read get_CurrentTokenKind;	// the TokenKind of the current token
		property CurrentTokenExID:  TptTokenKind read get_CurrentTokenExID;	// for an identifier token, contains the directive's TokenKind
		property CurrentTokenGenID: TptTokenKind read get_CurrentTokenGenID;	// ExID, or TokenID if ExID is empty

		// Token consumption helpers for whitespace-preserving parser
		function ConsumeKeyword(ExpectedKind: TptTokenKind): TSyntaxNode2;
	public
		constructor Create;
		destructor Destroy; override;

		class function ParseText(const Text: UnicodeString; FilePath: string): TSyntaxNode2;

		function ParseFile(FilePath: string): TSyntaxNode2;
		function ParseStream(SourceStream: ISequentialStream; FilePath: string; CodePage: Word): TSyntaxNode2;

		// Parse using the list of tokens. Note: the parser takes ownership of the tokens
		function Parse(const Tokens: TList): TSyntaxNode2;

		procedure ResetComplierDirectives;

		property CompilerDirectives: TStrings read FCompilerDirectives;

		// Whether parsing should skip the implementation, only parsing the interface section
		property InterfaceOnly: Boolean read FInterfaceOnly write FInterfaceOnly;

		//
		property IncludeHandler: IUnknown{IIncludeHandler} write set_IncludeHandler;
	public
{
		Not yet implemented; probably because they exist inside something else
}
//		function ParseAtom: TSyntaxNode2;					// currently part of ParseFactor, will extract to own method
//		function ParseBareInherited: TSyntaxNode2;		// is so simple it's just in SimpleStatement
{		function ParseClassOfType: TSyntaxNode2;
		function ParseExportsItem: TSyntaxNode2;
		function ParseExportsSpecifier: TSyntaxNode2;
		function ParseExportsStatement: TSyntaxNode2;
		function ParseExpressionOrAssignment: TSyntaxNode2;
		function ParseExpressionOrRangeList: TSyntaxNode2;
		function ParseExtendedIdent: TSyntaxNode2;
		function ParseFancyBlock: TSyntaxNode2;
		function ParseFieldSection: TSyntaxNode2;
		function ParseIdent: TSyntaxNode2;
		function ParseIdentList: TSyntaxNode2;

		function ParseInitSection: TSyntaxNode2;
		function ParseInterfaceDecl: TSyntaxNode2;
		function ParseLabelDeclSection: TSyntaxNode2;
		function ParseMethodImplementation: TSyntaxNode2;
		function ParseMethodReturnType: TSyntaxNode2;
		function ParseMulOp: TSyntaxNode2;
		function ParseOpenArray: TSyntaxNode2;
		function ParsePackedType: TSyntaxNode2;
		function ParseParameter: TSyntaxNode2;
		function ParseParameterExpression: TSyntaxNode2;
		function ParseParameterType: TSyntaxNode2;
		function ParseParenthesizedExpression: TSyntaxNode2;
		function ParseParticle: TSyntaxNode2;
		function ParseProcedureType: TSyntaxNode2;
		function ParseProgram: TSyntaxNode2;
		function ParseQualifiedIdent: TSyntaxNode2;
		function ParseRecordHelperType: TSyntaxNode2;
		function ParseRecordType: TSyntaxNode2;
		function ParseRelOp: TSyntaxNode2;
		function ParseSetLiteral: TSyntaxNode2;
		function ParseTypeDecl: TSyntaxNode2;
		function ParseUnaryOperator: TSyntaxNode2;
		function ParseVarDecl: TSyntaxNode2;
		function ParseVariantGroup: TSyntaxNode2;
		function ParseVisibility: TSyntaxNode2;}
	end;

implementation

uses
	System.IOUtils,
	TypInfo,
	Windows,
{$IFDEF UnitTests}DelphiParserTests,{$ENDIF}
	Avatar.Exceptions;

resourcestring
	SExpected	= '''%s'' expected found ''%s''';
	SEndOfFile	= 'end of file';

	SE2029 = 'E2029 %s expected but %s found';
	SE2123 = 'E2123 PROCEDURE, FUNCTION, PROPERTY, or VAR expected';
	SE2128 = 'E2128 %s clause expected, but %s found';

{ ESyntaxError }

constructor ESyntaxError.Create(const Msg: string);
begin
  FPosXY.X := -1;
  FPosXY.Y := -1;
  inherited Create(Msg);
end;

constructor ESyntaxError.CreateFmt(const Msg: string; const Args: array of const);
begin
  FPosXY.X := -1;
  FPosXY.Y := -1;
  inherited CreateFmt(Msg, Args);
end;

constructor ESyntaxError.CreatePos(const Msg: string; aPosXY: TPoint);
begin
  FPosXY := aPosXY;
  inherited Create(Msg);
end;

{ TDelphiParser }

function TDelphiParser.ParseForwardDeclaration: TSyntaxNode2;
var
	forwardToken: TSyntaxNode2;
begin
{
	Handles the forward keyword token.

   All the original code did was advance to the next token,
   and check it was a semi-colon;

   type
      TCustomer = class; forward;

	The semicolon makes it a forward declaration.
   So it's not a syntax, but an assertion.

	But we don't want to lose the foreward token at the very least.

	But that assertion advances the token.
	
	New approach: Create a container for the forward declaration that preserves
	both the 'forward' keyword and semicolon as child nodes for whitespace preservation,
	while also setting semantic attributes for analysis tools.
}
	// Create a container for the forward declaration
	Result := TSyntaxNode2.Create(ntIdentifier); // You might want to add ntForwardDeclaration to your enum

	// Preserve the 'forward' keyword as a child node
	forwardToken := ConsumeKeyword(ptForward);
	if Assigned(forwardToken) then
		Result.AddChild(forwardToken);

	// Preserve the semicolon as a child node
	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken(ptSemicolon));

	// Set semantic attribute for analysis tools
	Result.Attributes[anForwarded] := AttributeValueToStr(avTrue);
end;

function TDelphiParser.ParseProperty: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Property

Property
	-> [CLASS]
			PROPERTY Ident
			['[' (Parameter [';'])+ ']']
			[':' MethodReturnType]
			(PropertyDirective)*
			';'

property : Integer read FCurrent write FCurrent;		// Delphi 12: E2029 Identifier expected but ':' found

}
	Result := TSyntaxNode2.Create(ntProperty);

//	[CLASS]
	if CurrentTokenKind = ptClass then
	begin
		Result[anClass] := AttributeValueToStr(avTrue);
		Result.AddChild(EatToken(ptClass));
	end;

//	PROPERTY Ident
	Result.AddChild(EatToken(ptProperty));
	Result.AddChild(ParseIdent);

	// TODO: Implement the rest of this
//	['[' (Parameter [';'])+ ']']
	if CurrentTokenKind = ptOpenBracket then
	begin
		Result.AddChild(EatToken(ptOpenBracket));
//		Result.AddChild(ParseParameter);		//todo implement this
		while CurrentTokenKind = ptSemicolon do
		begin
			Result.AddChild(EatToken(ptSemicolon));
//			Result.AddChild(ParseParameter); //todo implement this
		end;
	end;

//	[':' MethodReturnType]
   if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseTypeId);
	end;

//	(PropertyDirective)*
	while IsPossiblePropertyDirective do
	begin
		Result.AddChild(ParsePropertyDirective);
	end;

//	';'
	Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParsePropertyDirective: TSyntaxNode2;

	function IsDirectiveWord(const AWord: string): Boolean;
	var
		tokText: string;
	begin
		tokText := CurrentToken.ValueText;
		if tokText = '' then
			tokText := CurrentToken.Text;
		Result := SameText(tokText, AWord);
	end;

	function PeekIsDirectiveWord(const AWord: string): Boolean;
	var
		tokText: string;
	begin
		tokText := PeekToken.ValueText;
		if tokText = '' then
			tokText := PeekToken.Text;
		Result := SameText(tokText, AWord);
	end;
begin
{
http://dgrok.excastle.com/Grammar.html#PropertyDirective

PropertyDirective
	-> ';' DEFAULT
	-> DEFAULT Expression
	-> DISPID Expression
	-> IMPLEMENTS (QualifiedIdent [','])+
	-> INDEX Expression
	-> NODEFAULT
	-> READ Expression
	-> READONLY
	-> STORED Expression
	-> WRITE Expression
	-> WRITEONLY
}
	Result := TSyntaxNode2.Create(ntPropertySpecifiers);

	if (CurrentTokenKind = ptSemicolon) and ((PeekTokenExID = ptDefault) or PeekIsDirectiveWord('default')) then
	begin
//		-> ';' DEFAULT
		Result.AddChild(EatToken(ptSemicolon));
		if CurrentTokenExID = ptDefault then
			Result.AddChild(EatTokenEx(ptDefault))
		else
			Result.AddChild(EatToken);
	end
	else if (CurrentTokenExID = ptDefault) or IsDirectiveWord('default') then
	begin
//		-> DEFAULT Expression
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenExID = ptDispId) or IsDirectiveWord('dispid') then
	begin
//		-> DISPID Expression
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenExID = ptIndex) or IsDirectiveWord('index') then
	begin
//		-> INDEX Expression
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenExID = ptNoDefault) or IsDirectiveWord('nodefault') then
	begin
//		-> NODEFAULT
		Result.AddChild(EatToken);
	end
	else if (CurrentTokenExID = ptRead) or IsDirectiveWord('read') then
	begin
//		-> READ Expression
		Result.AddChild(EatToken);
		// Keep accessor parsing bounded so the next property directive token is not consumed.
		Result.AddChild(ParseReadAccessIdentifier);
	end
	else if (CurrentTokenExID = ptReadOnly) or IsDirectiveWord('readonly') then
	begin
//		-> READONLY
		Result.AddChild(EatToken);
	end
	else if (CurrentTokenExID = ptStored) or IsDirectiveWord('stored') then
	begin
//		-> STORED Expression
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenExID = ptWrite) or IsDirectiveWord('write') then
	begin
//		-> WRITE Expression
		Result.AddChild(EatToken);
		// Keep accessor parsing bounded so the terminating semicolon is preserved.
		Result.AddChild(ParseWriteAccessIdentifier);
	end
	else if (CurrentTokenExID = ptWriteOnly) or IsDirectiveWord('writeonly') then
	begin
//		-> WRITEONLY
		Result.AddChild(EatToken);
	end
	else if (CurrentTokenExID = ptImplements) or IsDirectiveWord('implements') then
	begin
//		-> IMPLEMENTS (QualifiedIdent [','])+
		Result.AddChild(EatToken);
		Result.AddChild(ParseQualifiedIdent);
		while CurrentTokenKind = ptComma do
		begin
			Result.AddChild(EatToken(ptComma));
			Result.AddChild(ParseQualifiedIdent);
		end;
	end
	else
	begin
		// E2029 %s expected but %s found
		// E2029 Property directive expected but string constant found
		Result.AddChild(SynErrorFmt(SE2029, ['Property directive', CurrentToken.ToString]));
	end;
end;

function TDelphiParser.ParseStream(SourceStream: ISequentialStream; FilePath: string; CodePage: Word): TSyntaxNode2;
var
	tokenizer: TDelphiTokenizer;
	currentToken: TSyntaxToken;
	tokens: TObjectList;
begin
{
	Assigns the source stream to the lexer, and calls the protected ParseFile method.
}
	// Tokenize the input and put the tokens into our list
	tokens := TObjectList.Create(False); //don't own them, they go into the tree
	try
		tokenizer := TDelphiTokenizer.Create(SourceStream, CodePage);
		try
			// Set the compiler defined (e.g. IFDEF $UNICODE)
			tokenizer.CompilerDirectives.Assign(Self.FCompilerDirectives);

			while tokenizer.NextToken({out}currentToken) do
				tokens.Add(currentToken);
		finally
			tokenizer.Free;
		end;

		// Parse the tokens into a tree.
		Result := Self.Parse(tokens);
	finally
		tokens.Free;
	end;
end;

constructor TDelphiParser.Create;
begin
	inherited Create;

	FTokens := TObjectList<TSyntaxToken>.Create(False); // non-owning; tokens are owned by TSyntaxNodeOrToken wrappers
	FCurrent := -1; // so the first time they call NextToken it increments to 0

	FCompilerDirectives := TStringList.Create;
	GetDefaultConditionalDirectives(FCompilerDirectives);
	FInterfaceOnly := False;
end;

destructor TDelphiParser.Destroy;
begin
	FreeAndNil(FCompilerDirectives);
//	FreeAndNil(FStack);
	FreeAndNil(FTokens);

	FCurrent := 0; // technically off the end of an empty list

	inherited Destroy;
end;

{next two helpers validate expected tokens and recover with missing tokens at EOF}

function TDelphiParser.EatTokenEx(ExpectedTokenKind: TptTokenKind): TSyntaxToken;
var
	s: string;
begin
	// Expect the CurrentToken's ExID to be ExpectedTokenKind
	if CurrentTokenExID <> ExpectedTokenKind then
	begin
		s := Format(SExpected, ['EX:' + TokenName(ExpectedTokenKind), TokenName(CurrentTokenExID)]);
		DoMessage(s, CurrentToken.Line, CurrentToken.Column);

		// Roslyn-style recovery: synthesize the expected token and keep EOF in place.
		Result := TSyntaxToken.Create(ExpectedTokenKind, CurrentToken.Line, CurrentToken.Column, '');
		Result.IsMissing := True;

		if CurrentToken.TokenKind <> ptEof then
			NextToken;
		Exit;
	end;

	Result := CurrentToken;
	NextToken;
 end;

procedure TDelphiParser.NextToken;
var
	prevTokenText: string;
	currTokenText: string;
begin
	if FTokens = nil then
		raise Exception.Create('[TDelphiParser.NextToken] FTokens has been freed');

	// Record the current token text so we can write the "old" vs "new" token text.
//	prevTokenText := CurrentToken.Text;
	prevTokenText := TokenKindToStr(CurrentToken.TokenKind)+'('+QuotedStr(CurrentToken.Text)+')';

	// Once we are over the end, no point in going even further off the end.
	if FCurrent < FTokens.Count then
		Inc(FCurrent);

	currTokenText := TokenKindToStr(CurrentToken.TokenKind)+'('+QuotedStr(CurrentToken.Text)+')';

//	Log(''''+prevTokenText+''' --> '''+CurrentToken.Text+'''');
	Log(currTokenText+' <== '+prevTokenText);
end;

function TDelphiParser.PeekTokenExID: TptTokenKind;
begin
	Result := PeekToken(1).DirectiveID;
end;

function TDelphiParser.PeekTokenGenID: TptTokenKind;
begin
	Result := PeekToken(1).GenID;
end;

function TDelphiParser.PeekTokenKind: TptTokenKind;
begin
	Result := PeekToken(1).TokenKind;
end;

function TDelphiParser.ParseNilToken: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntLiteral);
	Result.Attributes[anType] := AttributeValueToStr(avNil);
	Result.AddChild(EatToken(ptNil));
end;

function TDelphiParser.NodeListToString(NamesNode: TSyntaxNode2): string;
var
	nodeOrToken: TSyntaxNodeOrToken;
begin
	Result := '';
	for nodeOrToken in NamesNode.ChildNodes do
	begin
		if not nodeOrToken.IsNode then
			Continue;

		if Result <> '' then
			Result := Result + '.';
		Result := Result + nodeOrToken.AsNode.Attributes[anName];
	end;
end;

function TDelphiParser.ParseNotOp: TSyntaxNode2;
begin
{
	Expects: ptNot

	Example

		if not Visible then

	Returns: ntNot
}
	Result := TSyntaxNode2.Create(ntNot);
	Result.AddChild(EatToken(ptNot));
end;

function TDelphiParser.ParseThenStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntThen);
	Result.AddChild(EatToken(ptThen));
	Result.AddChild(ParseStatement);
end;

//function TDelphiParser.Semicolon: TSyntaxToken;
//begin
//	case CurrentToken.TokenKind of
//	ptElse, ptEnd, ptExcept, ptfinally, ptFinalization, ptCloseParen, ptUntil: Result := nil; // NOP
//	else
//		Result := EatToken(ptSemiColon); // move to the next token, throwing a warning if CurrentToken is not a ptSemiColon
//	end;
//end;

procedure TDelphiParser.GetDefaultConditionalDirectives(TargetList: TStrings);

	procedure Add(const s: string);
	begin
		TargetList.Add(s);
	end;
begin
{
	Conditional compilation (Delphi)
	https://docwiki.embarcadero.com/RADStudio/Athens/en/Conditional_compilation_(Delphi)

	Conditional compilation is based on the existence and evaluation of constants,
	the status of compiler switches, and the definition of conditional symbols.

	Pre-defined Conditionals
	https://docwiki.embarcadero.com/RADStudio/Athens/en/Conditional_compilation_(Delphi)#Predefined_Conditionals

}
//	Compiler

{
	Compiler Versions
	https://docwiki.embarcadero.com/RADStudio/Athens/en/Compiler_Versions
}
{$IFDEF VER360}
	Add('VER360'); //	Delphi 12.0 Athens / C++Builder 12.0 Athens	23.0	290	36.0
{$ENDIF}

{$IFDEF VER350}
	Add('VER350'); //	Delphi 11.0 Alexandria / C++Builder 11.0 Alexandria	22.0	280	35.0
{$ENDIF}

{$IFDEF VER340}
	Add('VER340'); //	Delphi 10.4 Sydney / C++Builder 10.4 Sydney	21.0	270	34.0
{$ENDIF}

{$IFDEF VER330}
	Add('VER330'); //	Delphi 10.3 Rio / C++Builder 10.3 Rio	20.0	260	33.0
{$ENDIF}

{$IFDEF VER320}
	Add('VER320'); //	Delphi 10.2 Tokyo / C++Builder 10.2 Tokyo	19.0	250	32.0
{$ENDIF}

{$IFDEF VER310'}
	Add('VER310'); //	Delphi 10.1 Berlin / C++Builder 10.1 Berlin	18.0	240	31.0
{$ENDIF}

{$IFDEF VER300}
	Add('VER300'); //	Delphi 10 Seattle / C++Builder 10 Seattle	17.0	230	30.0
{$ENDIF}

{$IFDEF VER290}
	Add('VER290'); //	Delphi XE8 / C++Builder XE8	16.0	220	29.0
{$ENDIF}

{$IFDEF VER280}
	Add('VER280'); //	Delphi XE7 / C++Builder XE7	15.0	210	28.0
{$ENDIF}

{$IFDEF VER270}
	Add('VER270'); //	Delphi XE6 / C++Builder XE6	14.0	200	27.0
{$ENDIF}

{$IFDEF VER260}
	Add('VER260'); //	Delphi XE5 / C++Builder XE5	12.0	190	26.0
{$ENDIF}

{$IFDEF VER250}
	Add('VER250'); //	Delphi XE4 / C++Builder XE4	11.0	180	25.0
{$ENDIF}

{$IFDEF VER240}
	Add('VER240'); //	Delphi XE3 / C++Builder XE3	10.0	170	24.0
{$ENDIF}

{$IFDEF VER230}
	Add('VER230'); //	Delphi XE2 / C++Builder XE2	9.0	160[1]	23.0
{$ENDIF}

{$IFDEF VER220}
	Add('VER220'); //	Delphi XE / C++Builder XE	8.0	150	22.0
{$ENDIF}

{$IFDEF VER210}
	Add('VER210'); //	Delphi 2010 / C++Builder 2010	7	140	21.0
{$ENDIF}

{$IFDEF VER200}
	Add('VER200'); //	Delphi 2009 / C++Builder 2009	6	120	20.0
{$ENDIF}

{$IFDEF VER190}
	Add('VER190'); //	Delphi 2007 for .Net [2]	5	110	19.0
{$ENDIF}

{$IFDEF VER185}
	Add('VER185'); //	Delphi 2007 / C++Builder 2007 for Win32 [2]	5	110	18.5
{$ENDIF}

{$IFDEF VER180}
	Add('VER180'); //	Delphi 2007 / C++Builder 2007 for Win32 [2]	5	110	18.5
{$ENDIF}

{$IFDEF VER180}
	Add('VER180'); //	Delphi 2006 / C++Builder 2006	4	100	18.0
{$ENDIF}

{$IFDEF VER170}
	Add('VER170'); //	Delphi 2005	3	90	17.0
{$ENDIF}

{$IFDEF VER160}
	Add('VER160'); //	Delphi 8 for .Net	2	80	16.0
{$ENDIF}

{$IFDEF VER150}
	Add('VER150'); //	Delphi 7 (and 7.1)	NA	70	15.0
{$ENDIF}

{$IFDEF VER140}
	Add('VER140'); //	Delphi 6 / C++Builder 6	NA	60	14.0
{$ENDIF}

{$IFDEF VER130'}
	Add('VER130'); //	Delphi 5 / C++Builder 5	NA	50	NA
{$ENDIF}

{$IFDEF VER125'}
	Add('VER125'); //	C++Builder 4	NA	40	NA
{$ENDIF}

{$IFDEF VER120'}
	Add('VER120'); //	Delphi 4	NA	40	NA
{$ENDIF}

{$IFDEF VER110'}
	Add('VER110'); //	C++Builder 3	NA	30	NA
{$ENDIF}

{$IFDEF VER100'}
	Add('VER100'); //	Delphi 3	NA	30	NA
{$ENDIF}

{$IFDEF VER93'}
	Add('VER93'); //	C++Builder 1	NA	NA	NA
{$ENDIF}

{$IFDEF VER90'}
	Add('VER90'); //	Delphi 2	NA	20	NA
{$ENDIF}

{$IFDEF VER80'}
	Add('VER80'); //	Delphi 1	NA	10	NA
{$ENDIF}

{$IFDEF VER70'}
	Add('VER70'); //	Borland Pascal 7.0	NA	70	NA
{$ENDIF}

{$IFDEF VER15'}
	Add('VER15'); //	Turbo Pascal for Windows 1.5	NA	15	NA
{$ENDIF}

{$IFDEF VER10'}
	Add('VER10'); //	Turbo Pascal for Windows 1.0	NA	10	NA
{$ENDIF}

{$IFDEF VER60'}
	Add('VER60'); //	Turbo Pascal 6.0	NA	60	NA
{$ENDIF}

{$IFDEF VER55'}
	Add('VER55'); //	Turbo Pascal 5.5	NA	55	NA
{$ENDIF}

{$IFDEF VER50'}
	Add('VER50'); //	Turbo Pascal 5.0	NA	50	NA
{$ENDIF}

{$IFDEF VER40'}
	Add('VER40'); //	Turbo Pascal 4.0	NA	40	NA
{$ENDIF}


//	Platform
{$IFDEF CONSOLE}
	Add('CONSOLE');		//	Defined if an application is being compiled as a console application.
{$ENDIF}

{$IFDEF IOS}
	Add('IOS'); 			//	Defined if the target platform is iOS. *New* in XE4/iOS.
{$ENDIF}

{$IFDEF IOS64'}
	Add('IOS64');			//	Defined if the target platform is iOS64. Since XE8/iOSarm64.
{$ENDIF}

{$IFDEF OSX'}
	Add('OSX');				//	Defined if the target platform is macOS. *New* in XE2/macOS.
{$ENDIF}

{$IFDEF OSX64'}
	Add('OSX64');			//	Defined if the target platform is macOS. *New* in 10.3.2 .
{$ENDIF}

{$IFDEF NATIVECODE'}
	Add('NATIVECODE');	//	Since Delphi.Net
{$ENDIF}

{$IFDEF MSWINDOWS'}
	Add('MSWINDOWS');		//	Indicates that the operating environment is Windows. Use MSWINDOWS to test for any flavor of the Windows platform instead of WIN32.
{$ENDIF}

{$IFDEF WIN32'}
	Add('WIN32');			//	Target platform is the native 32-bit Windows platform.
{$ENDIF}

{$IFDEF WIN64'}
	Add('WIN64');			//	Target platform is 64-bit Windows. *New* in XE2/x64.
{$ENDIF}

{$IFDEF MACOS'}
	Add('MACOS');			//	Target platform is an Apple Darwin OS (macOS or iOS). *New* in XE2/macOS.
{$ENDIF}
//		Note: This symbol existed before Apple changed the name of OS X to macOS.

{$IFDEF MACOS32'}
	Add('MACOS32');		//	Target platform is 32-bit Apple Darwin OS (32-bit macOS or 32-bit iOS). *New* in XE2/macOS.
{$ENDIF}
//		Note: This symbol existed before Apple changed the name of OS X to macOS.

{$IFDEF MACOS64'}
	Add('MACOS64');		//	Target platform is 64-bit Apple Darwin OS (64-bit macOS or 64-bit iOS). *New* in XE8/macOS.
{$ENDIF}
//		Note: This symbol existed before Apple changed the name of OS X to macOS.

{$IFDEF LINUX'}
	Add('LINUX');			//	Since Kylix.
{$ENDIF}

{$IFDEF LINUX32'}
	Add('LINUX32');		//	Since Kylix.
{$ENDIF}

{$IFDEF LINUX64'}
	Add('LINUX64');		//	New in 10.2
{$ENDIF}

{$IFDEF POSIX'}
	Add('POSIX');			//	Since Kylix.
{$ENDIF}

{$IFDEF POSIX32'}
	Add('POSIX32'); //	Since Kylix.
{$ENDIF}

{$IFDEF POSIX64'}
	Add('POSIX64'); //	Since Kylix.
{$ENDIF}

{$IFDEF ANDROID'}
	Add('ANDROID'); //	Defined if the target platform is Android. *New* in XE5.
{$ENDIF}

{$IFDEF ANDROID32'}
	Add('ANDROID32'); //	Since XE8/iOSarm64.
{$ENDIF}

{$IFDEF ANDROID64'}
	Add('ANDROID64'); //	Delphi compiler for Android 64-bit platform. *New* in 10.3.3.
{$ENDIF}


//	CPU
{$IFDEF CPU386'}
	Add('CPU386'); //	Indicates that the CPU is an Intel 386 or later.
{$ENDIF}

{$IFDEF CPUX86'}
	Add('CPUX86'); //	CPU is an Intel 386 or later on any platform. *New* in XE2/x64.
{$ENDIF}

{$IFDEF CPUX64'}
	Add('CPUX64'); //	The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64.
{$ENDIF}

{$IFDEF CPU32BITS'}
	Add('CPU32BITS'); //	The CPU is in a 32-bit environment, such as DCC32.EXE. *New* in XE8.
{$ENDIF}

{$IFDEF CPU64BITS'}
	Add('CPU64BITS'); //	The CPU is in a 64-bit environment, such as DCC64.EXE. *New* in XE8.
{$ENDIF}

{$IFDEF CPUARM'}
	Add('CPUARM'); //	Defined if the CPU is based on the ARM architecture. *New* in XE4/iOS.
{$ENDIF}

{$IFDEF CPUARM32'}
	Add('CPUARM32'); //	The CPU is in a 32-bit ARM environment. *New* in XE8.
{$ENDIF}

{$IFDEF CPUARM64'}
	Add('CPUARM64'); //	The CPU is in a 64-bit ARM environment, such as DCCIOSARM64.EXE. *New* in XE8.
{$ENDIF}


//	Availability
{$IFDEF ALIGN_STACK'}
	Add('ALIGN_STACK'); //	Defined in code that may be shared with the macOS compiler and another compiler on another platform such as Linux that does not have a rigid stack alignment requirement. *New* in XE2/macOS.
{$ENDIF}

{$IFDEF ASSEMBLER'}
	Add('ASSEMBLER'); //	Assembler syntax is accepted.
{$ENDIF}

// Removed in Delphi 10.4
{$IFDEF AUTOREFCOUNT'}
	//*New* in XE4/iOS. Removed in 10.4 Sydney.
	Add('AUTOREFCOUNT'); // Defined for compilers that use automatic refe rence counting, such as the Delphi mobile compilers.
{$ENDIF}

{$IFDEF EXTERNALLINKER'}
	Add('EXTERNALLINKER'); //	Defined for compilers that have an external linker and the LLVM code generator; the Delphi mobile compilers have the external ld linker and use LLVM as code generator. *New* in XE4/iOS.
{$ENDIF}

{$IFDEF UNICODE'}
	Add('UNICODE');						// UNICODE is defined as the default string type.
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS'}
	Add('CONDITIONALEXPRESSIONS');	// Tests for the use of the $IF directive.
{$ENDIF}

{$IFDEF ELF'}
	Add('ELF');								// Defined when targeting Executable and Linkable Format (ELF) files.
{$ENDIF}

// Removed in Delphi 10.4
{$IFDEF NEXTGEN'}
	//Defined for compilers (such as the Delphi mobile compilers) that use
	// "next-generation" language features, such as 0-based strings.

	// *New* in XE4/iOS. Removed in 10.4 Sydney.
	// Were defined for Delphi mobile compilers in the past,
	//but they have been removed in RAD Studio 10.4 Sydney as part of the
	//removal of the ARC memory model.
	Add('NEXTGEN'); //* Defined for compilers (such as the Delphi mobile compilers) that use "next-generation" language features, such as 0-based strings. *New* in XE4/iOS. Removed in 10.4 Sydney.
{$ENDIF}

{$IFDEF PC_MAPPED_EXCEPTIONS'}
	Add('PC_MAPPED_EXCEPTIONS'); //	Defined when compiling on a platform or for a target platform that uses address maps instead of stack frames to unwind exceptions (such as macOS). *New* in XE2.
{$ENDIF}

{$IFDEF PIC'}
	Add('PIC'); //	Defined for platforms that require Position-Independent Code (PIC), such as macOS.
{$ENDIF}

{$IFDEF UNDERSCOREIMPORTNAME'}
	Add('UNDERSCOREIMPORTNAME'); //	Defined for compilers that add a leading underscore (for example, in names of dynamic libraries imported from Mac OS). *New* in XE4/iOS.
{$ENDIF}

{$IFDEF WEAKREF'}
	Add('WEAKREF'); //	Defined for compilers that can use weak references (the [weak] attribute). *New* in XE4/iOS.
{$ENDIF}

{$IFDEF WEAKINSTREF'}
	//	Defined when weak references are defined for instances.

	// *New* in XE4/iOS. Removed in 10.4 Sydney.
	// Were defined for Delphi mobile compilers in the past,
	//but they have been removed in RAD Studio 10.4 Sydney as part of the
	//removal of the ARC memory model.
	Add('WEAKINSTREF');
{$ENDIF}

{$IFDEF WEAKINTFREF'}
	Add('WEAKINTFREF'); //	Defined when weak references are defined for interfaces. *New* in XE4/iOS.
{$ENDIF}

{$IFDEF FRAMEWORK_VCL}
	Add('FRAMEWORK_VCL'); // Defined if the project uses VCL framework.
{$ENDIF}

{$IFDEF FRAMEWORK_FMX}
	Add('FRAMEWORK_FMX'); // Defined if the project uses FMX framework.
{$ENDIF}

end;

procedure TDelphiParser.ResetComplierDirectives;
begin
{
	We need to know what the compiler conditional defines are,
	so we can tokenize the text correctly.

	This is the default behavior, which uses the defines of your compiler
	that compiled this code.
}
	FCompilerDirectives.Clear;
	GetDefaultConditionalDirectives(FCompilerDirectives);
end;

function TDelphiParser.get_CurrentTokenExID: TptTokenKind;
begin
	// For an identifier token, contains the directive's TokenKind
	// e.g. {$IFDEF}, {$ENDIF}, {$DEFINE}, etc.
	// If it's the EOF token, return ptEOF.
	if CurrentToken.TokenKind = ptEof then
		Exit(ptEof);

	Result := CurrentToken.DirectiveID;
end;

function TDelphiParser.get_CurrentTokenKind: TptTokenKind;
begin
	Result := CurrentToken.TokenKind;
end;

function TDelphiParser.ParseGotoStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#GotoStatement

GotoStatement
	-> GOTO LabelId
}
	Result := TSyntaxNode2.Create(ntGoto);
	Result.AddChild(EatToken(ptGoto));
	Result.AddChild(ParseLabelId);
end;

function TDelphiParser.get_CurrentTokenGenID: TptTokenKind;
begin
	Result := CurrentToken.GenID;
end;

function TDelphiParser.get_NextFewTokens: string;
var
	i: Integer;
	tok: TSyntaxToken;
begin
{
	Returns that thing that i wish i could just see: where we are in the source.

	So lets return this token, and the next few tokens.
}
	Result := '';

	for i := 0 to 6 do
	begin
		tok := PeekToken(i); // 0:currentToken;
		if tok.TokenKind = ptEOF then
		begin
			Result := Result+'[==EOF==]';
			Break;
		end;

		Result := Result+' ['+tok.ValueText+']';
	end;

end;

function TDelphiParser.SynError(Error: string): TSyntaxNode2;
begin
	Result := SynErrorFmt(Error, []);
end;

function TDelphiParser.SynErrorFmt(const Error: string; const Args: array of const): TSyntaxNode2;
var
	errToken: TSyntaxToken;
begin
	// DONE: This should probably call the other new methods to synthesize the missing token.
	// Investigate closer how roslyn does with this, including Token.IsMissing, and it's use
	// as a compile feedback mechanism
//	DoMessage(1, Error + ' found ' + TokenName(CurrentToken.TokenKind), CurrentToken.Line, CurrentToken.Column);

	// Create a zero-width (synthesized) token for error recovery, preserving tree shape
	Result := TSyntaxNode2.Create(ntUnknown);
	Result.Attributes[anName] := Format(Error, Args);
	Result.Attributes[anMissing] := 'true';
	errToken := CurrentToken;
	Result.AddChild(EatToken);

	DoMessage(Error, errToken.Line, errToken.Column);
end;

function TDelphiParser.Parse(const Tokens: TList): TSyntaxNode2;
var
	i: Integer;
	token: TSyntaxToken;
begin
{
	Parse the supplied tokens from the tokenzier into a Syntax Tree,
	rooted at Result (TSyntaxNode2).
}
	// Put the tokens into our local token list so we can indexing functions
	FTokens.Clear;
	FCurrent := -1;

	for i := 0 to Tokens.Count-1 do
	begin
		token := TObject(Tokens[i]) as TSyntaxToken;
		FTokens.Add(token);
	end;

	Result := ParseCore;
end;

function TDelphiParser.ParseMethodOrProperty: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodOrProperty

MethodOrProperty
	-> MethodHeading
	-> Property


MethodHeading
	-> [CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR)
			QualifiedIdent
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]		// FUNCTION required, OPERATOR optional
			[';']
			(Directive [';'])*

Property
	-> [CLASS]
			PROPERTY Ident
			['[' (Parameter [';'])+ ']']
			[':' MethodReturnType]
			(PropertyDirective)*
			';'
}
	if IsPossibleMethodHeading then
	begin
		Result := TSyntaxNode2.Create(ntMethod);
		Result.AddChild(ParseMethodHeading);
	end
	else if IsPossibleProperty then
	begin
		Result := TSyntaxNode2.Create(ntProperty);
		Result.AddChild(ParseProperty);
	end
	else
	begin
		Result := SynError('xxxxx Expected method or property');
	end;
end;

function TDelphiParser.ParseFieldDecl: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#FieldDecl

FieldDecl
	-> IdentList ':' Type
			(PortabilityDirective)*
			[';']
}
	Result := TSyntaxNode2.Create(ntField);

	// Parse one or more field names (e.g. A, B: Integer) as ntName nodes.
	Result.AddChild(ParseFieldName);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseFieldName);
	end;

	// Normal form is "IdentList ':' Type".
	// Recovery: in some error paths the ':' may have been consumed while we're still
	// positioned at the type identifier; don't require a second ':' in that case.
	if CurrentTokenKind = ptColon then
		Result.AddChild(EatToken(ptColon))
	else if (FCurrent > 0) and (CurrentTokenKind = ptIdentifier) and (FTokens[FCurrent-1].TokenKind = ptColon) then
	begin
		// ':' already consumed; continue with type parsing.
	end
	else
		Result.AddChild(EatToken(ptColon));

	Result.AddChild(ParseType);

//	(PortabilityDirective)*
	while IsPossiblePortabilityDirective do
		Result.AddChild(ParsePortabilityDirective);

// [;]
	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken(ptSemiColon));
end;

function TDelphiParser.ParseCore: TSyntaxNode2;
var
	node: TSyntaxNode2;
	trailing: TSyntaxNode2;
begin
{
	Parse the tokens contained in FTokens and returns it as a syntax Tree

	- CompilationUnit
		- unit

	This part is oriented at the official grammar of Delphi 4
	and parialy based on Robert Zierers Delphi grammar.
	For more information about Delphi grammars take a look at:
		http://www.stud.mw.tu-muenchen.de/~rz1/Grammar.html
		https://archive.ph/2ytar
}

	// The tree is rooted on a "CompilationUnit" node.
	Result := TSyntaxNode2.Create(ntCompilationUnit);

	NextToken; // advance to the first token

{
ntCompilationUnit is the root container for whatever the parser produced.

This has a few features:

- Uniform root: Every syntax tree, whether it's a full unit with interface/implementation,
		or just a single expression—has one root node type.
		That means tools (traversals, visitors, rewriters) don’t need special cases
		like "sometimes the root is a UnitDeclaration, sometimes it's just an Expression."
- Global trivia: Roslyn sticks file-level comments, #pragmas, #if/#endif, extern alias,
		and using directives at the root.
		Delphi equivalents could be compiler directives, attributes,
		or even stray comments before the unit keyword.
- EndOfFile token: the EOF sentinel always lives under the compilation root.
- Extensibility: When the language adds new top-level constructs
		(e.g. top-level statements in C# 9, Delphi's program vs. unit headers),
		you don’t have to change the concept of the root,
		it just holds whatever is allowed at top level.
}

	// Check the file type directive
	case CurrentToken.GenID of
	ptUnit:		node := ParseUnit;		// e.g. unit SimpleParser;
	ptProgram:	node := ParseProgramFile;			// e.g. program SimpleParser;
	ptPackage:	node := ParsePackage;			// e.g. package SimpleParser;
	ptLibrary:	node := ParseLibraryFile;			// e.g. library SimpleParser;
	else
		node := ParseScriptFile; // for arbitrary expressions
	end;

	Result.AddChild(node);

	// If parsing ended early, preserve ownership of all remaining real tokens.
	if CurrentTokenKind <> ptEof then
	begin
		trailing := TSyntaxNode2.Create(ntUnknown);
		trailing.Attributes[anName] := 'Unexpected trailing tokens';
		while CurrentTokenKind <> ptEof do
			trailing.AddChild(EatToken); // consume real token, attach to tree
		Result.AddChild(trailing);
	end;

	Result.AddChild(EatToken(ptEOF)); // consume/attach real EOF token
end;

function TDelphiParser.ParseFile(FilePath: string): TSyntaxNode2;

	function DetectEncodingFromStream(AStream: TStream): TEncoding;
	var
		LBuffer: TBytes;
		LPosition: Int64;
		LEncoding: TEncoding;
	begin
		// Remember the current stream position so we can restore it later
		LPosition := AStream.Position;

		// Read up to 4 bytes (max BOM length in Delphi’s detection logic)
		SetLength(LBuffer, 4);
		FillChar(LBuffer[0], 4, 0);
		AStream.ReadBuffer(LBuffer[0], 4);

		// Rewind the stream to its original position
		AStream.Position := LPosition;

		// Let Delphi’s TEncoding check if these bytes match a known BOM
		TEncoding.GetBufferEncoding(LBuffer, {var}LEncoding);
		LEncoding := TEncoding.Default;

		Result := LEncoding;
	end;

var
	fs: TFileStream;
	stm: IStream;
	encoding: TEncoding;
begin
{
}
	fs := TFile.OpenRead(FilePath);
	encoding := DetectEncodingFromStream(fs); // Check for BOM

	stm := TStreamAdapter.Create(fs, soOwned);

	Result := Self.ParseStream(stm, FilePath, encoding.CodePage);
end;

class function TDelphiParser.ParseText(const Text: UnicodeString; FilePath: string): TSyntaxNode2;
var
	s: TStream;
	stm: ISequentialStream;
	parser: TDelphiParser;
begin
{
	Convert the Text to an ISequentialStream and pass it to the .Parse method.
}
	s := TStringStream.Create(Text, CP_UTF16); // expose the string as UTF-16

	// Wrap in a TStreamAdapter in order to get an ISequentialStream (inherited by IStream)
	stm := TStreamAdapter.Create(s, soOwned) as IStream;

	parser := TDelphiParser.Create;
	try
		Result := parser.ParseStream(stm, FilePath, CP_UTF16); // unicode strings are UTF-16
	finally
		parser.Free;
	end;

	stm := nil;
end;

function TDelphiParser.ParseLibraryFile: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Program

Program
	-> (PROGRAM | LIBRARY) Ident
			['(' IdentList ')'] ';'
			[UsesClause]
			(ImplementationDecl)*
			InitSection '.'
}
	Result := TSyntaxNode2.Create(ntLibrary);
	Result.AddChild(EatToken(ptLibrary));
	Result.AddChild(ParseIdent);

//	['(' IdentList ')'] ';'
	if CurrentTokenKind = ptOpenParen then
	begin
		// program MyProgram1 (Foo, Bar);  //legacy turbo pascal
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseIdentifierList);
		Result.AddChild(EatToken(ptCloseParen));
	end;

	Result.AddChild(EatToken(ptSemicolon));

	if IsPossibleUsesClause then
		Result.AddChild(ParseUsesClause);

	// If the user only asked us to parse the interface section
	if InterfaceOnly then
		Exit;

	// (ImplementationDecl)*
	while IsPossibleImplementationDecl do
		Result.AddChild(ParseImplementationDecl);

	// InitSection '.'
	if CurrentTokenKind = ptBegin then
		Result.AddChild(ParseStatements)
	else
		Result.AddChild(ParseInitializationSection);

	Result.AddChild(EatToken(ptDot));
end;

procedure TDelphiParser.DoMessage(const Msg: string; X, Y: Integer);
begin
	if Assigned(FOnMessage) then
		FOnMessage(Self, Msg, X, Y);

	if IsDebuggerPresent then
		OutputDebugString(PWideChar(Format('%s (%d,%d)', [Msg, X, Y])));
end;

procedure TDelphiParser.Log(const s: string);
begin
	Windows.OutputDebugString(PChar(s));
end;

function TDelphiParser.ParsePackage: TSyntaxNode2;
begin
{
Returns:
	ntPackage

http://dgrok.excastle.com/Grammar.html#Package

Package
	-> PACKAGE QualifiedIdent ';'
			[RequiresClause]
			[UsesClause]
			(AssemblyAttribute)*
			END '.'
}
	Result := TSyntaxNode2.Create(ntPackage);

//	PACKAGE QualifiedIdent ';'
	Result.AddChild(EatTokenEx(ptPackage));
	Result.AddChild(ParseQualifiedIdent);   // ntQualifiedIdentifier
	Result.AddChild(EatToken(ptSemicolon));

//	[RequiresClause]
	if IsPossibleRequiresClause then        //todo: implement IsPossibleRequireClause
		Result.AddChild(ParseRequiresClause);

//	[UsesClause]
	if IsPossibleUsesClause then
		Result.AddChild(ParseUsesClause);

	// TODO: Find the parse methods that handle this, and rename them.
//	(AssemblyAttribute)*
//	while IsPossibleAssemblyAttribute do
//		Result.AddChild(ParseAssemblyAttribute);

	Result.AddChild(EatToken(ptEnd));
	Result.AddChild(EatToken(ptDot));
end;

function TDelphiParser.ParseProgramFile: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Program

Program
	-> (PROGRAM | LIBRARY) Ident
			['(' IdentList ')'] ';'
			[UsesClause]
			(ImplementationDecl)*
			InitSection '.'
}
	Result := TSyntaxNode2.Create(ntProgram);
	Result.AddChild(EatToken(ptProgram));
	Result.AddChild(ParseIdent);

	// program MyProgram1 (Foo, Bar);  //legacy turbo pascal
	if CurrentTokenKind = ptOpenParen then
	begin
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseIdentifierList);
		Result.AddChild(EatToken(ptCloseParen));
	end;

	Result.AddChild(EatToken(ptSemicolon));

	if IsPossibleUsesClause then
		Result.AddChild(ParseUsesClause);

	// If the user only asked us to parse the interface section
	if InterfaceOnly then
		Exit;

	// (ImplementationDecl)*
	while IsPossibleImplementationDecl do
		Result.AddChild(ParseImplementationDecl);

	// InitSection '.'
	if CurrentTokenKind = ptBegin then
		Result.AddChild(ParseStatements)
	else
		Result.AddChild(ParseInitializationSection);

	Result.AddChild(EatToken(ptDot));
end;

function TDelphiParser.ParseUnaryMinus: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntUnaryMinus);
	Result.AddChild(EatToken(ptMinus));
end;

function TDelphiParser.ParseUnit: TSyntaxNode2;
var
	unitName: TSyntaxNode2;
	portabilityDirective: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Unit

Unit
	-> UNIT QualifiedIdentifier (PortabilityDirective)* ';'
			InterfaceSection
			ImplementationSection
			InitSection '.'

Example
-------

	unit System.Classes platform deprecated 'use System.Classes2' library experimental;

Returns
-------

	TSyntaxNode2(NodeType=ntUnitDeclaration, Ln=1, Col=1, Children=2
			@anName         = "System.Classes"
			@anDeprecated   = "use System.Classes2"
			@anLibrary      = "true"
			@anPlatform     = "true"
			@anExperimental = "true")


ntCompilationUnit
	ntUnitDeclaration @anName="Contoso.SpecialCharactersDemo" @anLibrary="true" @anPlatform="true" @deprecated="use Fabrikam.SpecialCharactersDemo" @anExperimental="true"
		ptUnit("unit")
		ntIdentifier @anName="Contoso.SpecialCharactersDemo"
			ptIdentifier("Contoso")
			ptDot["."]
			ptIdentifier("SpecialCharactersDemo")
		ntPortabilityDirective
			ptLibrary("library")
			ptPlatform("platform")
			ptDeprecated("deprecated")
			ptStringConst("use Fabrikam.SpecialCharactersDemo")
			ptExperimental("experimental")
		ptSemicolon(';')
		ntInterfaceSection
	ptEof
}
	Result := TSyntaxNode2.Create(ntUnitDeclaration);

	// Add the "unit" token to the child list
	Result.AddChild(EatToken(ptUnit));	// EatToken moves next,

	// Get the unit's full name (e.g. "System.Classes")
	unitName := ParseQualifiedIdent;
	Result.Attributes[anName] := unitName.Attributes[anName];	// get the unit name
	Result.AddChild(unitName);

	// Portability directives
	if (CurrentTokenGenID in [ptPlatform, ptLibrary, ptDeprecated, ptExperimental]) then
	begin
		portabilityDirective := ParsePortabilityDirective;
		Result.AddChild(portabilityDirective);
		Result.Attributes[anPlatform]     := portabilityDirective.Attributes[anPlatform];
		Result.Attributes[anLibrary]      := portabilityDirective.Attributes[anLibrary];
		Result.Attributes[anDeprecated]   := portabilityDirective.Attributes[anDeprecated];
		Result.Attributes[anExperimental] := portabilityDirective.Attributes[anPlatform];
	end;

	Result.AddChild(EatToken(ptSemicolon)); // Expect a semicolon

	// interface
	Result.AddChild(ParseInterfaceSection);		// read the interface section

	// If they only want the interfaction section then we are now done
	if InterfaceOnly then
		Exit;

	// implementation
	Result.AddChild(ParseImplementationSection);

	case CurrentTokenKind of
	ptInitialization:
		begin
			Result.AddChild(ParseInitializationSection);
			if CurrentTokenKind = ptFinalization then
				Result.AddChild(ParseFinalizationSection);
			Result.AddChild(EatToken(ptEnd));
		end;
	ptBegin:	Result.AddChild(ParseStatements);
	ptEnd:	Result.AddChild(EatToken(ptEnd));
	end;

	Result.AddChild(EatToken(ptDot));
end;

function TDelphiParser.ParseMainUsesClause: TSyntaxNode2;
//var
//	nameNode, pathNode, pathLiteralNode, temp: TSyntaxNode2;
begin
	Result := PoisonNode;


(*
	TODO: Convert this to the node and token are first class citizens, nodes are returned model.


	FStack.Push(ntUnit);
	try
		FStack.PushCompoundSyntaxNode(ntUses);
		try
			Result.AddChild(EatToken(ptUses));
			Result.AddChild(ParseMainUsedUnitStatement);
			while CurrentTokenKind = ptComma do
			begin
				NextToken;
				Result.AddChild(ParseMainUsedUnitStatement);
			end;
			Semicolon;
		finally
			FStack.Pop;
		end;

		nameNode := FStack.Peek.FindNode(ntUnit);

		if Assigned(nameNode) then
		begin
			temp := FStack.Peek;
			temp.Attributes[anName] := nameNode.Attributes[anName];
			temp.DeleteChild(nameNode);
		end;

		pathNode := FStack.Peek.FindNode(ntExpression);
		if Assigned(pathNode) then
		begin
			FStack.Peek.ExtractChild(pathNode);
			try
				pathLiteralNode := pathNode.FindNode(ntLiteral);

//				if PathLiteralNode is TValuedSyntaxNode then
					FStack.Peek.Attributes[anPath] := pathLiteralNode.Value; //TValuedSyntaxNode(PathLiteralNode).Value);
			finally
				pathNode.Free;
			end;
		end;
	finally
		FStack.Pop;
	end;
*)
end;

function TDelphiParser.ParseMethodKind: TSyntaxNode2;
begin
	Result := PoisonNode;


	case CurrentTokenKind of
	ptConstructor:	Result.AddChild(EatToken(ptConstructor));
	ptDestructor:	Result.AddChild(EatToken(ptDestructor));
	ptProcedure:	Result.AddChild(EatToken(ptProcedure));
	ptFunction:		Result.AddChild(EatToken(ptFunction));
	else
{
		public
			class Fetch: Boolean; // E2123 PROCEDURE, FUNCTION, PROPERTY, or VAR expected
		end;

		E2123 PROCEDURE, FUNCTION, PROPERTY, or VAR expected (Delphi)
}
		Result.AddChild(SynError(SE2123)); // E2123 PROCEDURE, FUNCTION, PROPERTY, or VAR expected
	end;
end;

procedure TDelphiParser.MoveMembersToVisibilityNodes(TypeNode: TSyntaxNode2);
{var
  child, vis: TSyntaxNode2;
  i: Integer;
  extracted: Boolean;}
begin
{
  vis := nil;
  i := 0;
  while i < Length(TypeNode.ChildNodes) do
  begin
    child := TypeNode.ChildNodes[i];
    extracted := false;
    if child.HasAttribute(anVisibility) then
      vis := child
    else if Assigned(vis) then
    begin
      TypeNode.ExtractChild(child);
      vis.AddChild(child);
      extracted := true;
    end;
    if not extracted then
      inc(i);
  end;
}
end;


function TDelphiParser.ParseMainUsedUnitStatement: TSyntaxNode2;
begin
	Result := PoisonNode;

{
	TODO: Convert this to the node and token are first class citizens, nodes are returned model.


	FStack.Push(ntUnit);
	try
		Result.AddChild(ParseMainUsedUnitName);
		if CurrentToken.TokenKind = ptIn then
		begin
			NextToken;
			Result.AddChild(ParseMainUsedUnitExpression);
		end;
		nameNode := FStack.Peek.FindNode(ntUnit);

		if Assigned(nameNode) then
		begin
			temp := FStack.Peek;
			temp.Attributes[anName] := nameNode.Attributes[anName];
			temp.DeleteChild(nameNode);
		end;

		pathNode := FStack.Peek.FindNode(ntExpression);
		if Assigned(pathNode) then
		begin
			FStack.Peek.ExtractChild(pathNode);
			try
				pathLiteralNode := pathNode.FindNode(ntLiteral);

//				if PathLiteralNode is TValuedSyntaxNode then
					FStack.Peek.Attributes[anPath] := pathLiteralNode.Value; //, TValuedSyntaxNode(PathLiteralNode).Value);
			finally
				pathNode.Free;
			end;
		end;
	finally
		FStack.Pop;
	end;
}
end;

function TDelphiParser.ParseUsesClause: TSyntaxNode2;
var
	usedUnitName: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#UsesClause

Grammer
=======

UsesClause
	-> (USES | CONTAINS)
			(UsedUnit [','])+ ';'


uses
	winapi.msxml, Toolkit, Avatar.Exceptions;

ntUses
	ptUses('uses')
	ntUsedUnit('winapi.msxml')
		ptIdentifier('winapi')
		ptDot('.')
		ptIdentifier('msxml')
	ptComma(',')
	ntUsedUnit('Tookit')
		ptIdentifier('Toolkit')
	ptComma(',')
	ntUsedUnit('Avatar.Exceptions')
		ptIdentifier('Avatar')
		ptDot('.')
		ptIdentifier('Exceptions')
	ptComma(',')
	ptSemicolon(";")
}
	if CurrentTokenKind = ptUses then
	begin
		Result := TSyntaxNode2.Create(ntUses);
		Result.AddChild(EatToken(ptUses));
	end
	else if CurrentTokenKind = ptContains then
	begin
		Result := TSyntaxNode2.Create(ntContains);
		Result.AddChild(EatToken(ptContains));
	end
	else
	begin
		// Check the error returned by Delphi if there's no `uses` or `contains`
		Result := SynError('ParseUsesClause');
		Exit;
	end;

{
	(UsedUnit [','])+ ';'
		one or more unit names, separated with commas; terminated with a semicolon.
}
	usedUnitName := ParseUsedUnit; // ptUsedUnit
	Result.AddChild(usedUnitName);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));

		usedUnitName := ParseUsedUnit;
		Result.AddChild(usedUnitName);
	end;

	Result.AddChild(EatToken(ptSemiColon));
end;

function TDelphiParser.ParseUsedUnit: TSyntaxNode2;
var
	unitName: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#UsedUnit

UsedUnit
	-> Ident
	-> Ident IN <stringliteral>
}
	Result := TSyntaxNode2.Create(ntUsedUnit);

	unitName := ParseQualifiedIdent;
	Result.AddChild(unitName);
	Result.Attributes[anName] := unitName.Attributes[anName];

	//  IN <stringliteral>
	if CurrentTokenKind = ptIn then
	begin
		Result.AddChild(EatToken(ptIn));
		Result.AddChild(EatToken(ptStringLiteral));
	end;
end;

function TDelphiParser.ParseBlock: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Block

Block
	-> BEGIN [StatementList] END
	-> AssemblerStatement

AssemblerStatement
	-> ASM
			<assemblylanguage>
			END
}
	if CurrentTokenKind = ptBegin then
	begin
		Result := TSyntaxNode2.Create(ntStatements);
		Result.AddChild(EatToken(ptBegin));
		Result.AddChild(ParseStatements);
		Result.AddChild(EatToken(ptEnd));
	end
	else if CurrentTokenKind = ptAsm then
	begin
		Result := ParseAssemblerStatement;
	end
	else
		Result := SynError('xxxxx Expected block');
end;

function TDelphiParser.ParseImplementationDecl: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ImplementationDecl

ImplementationDecl
	-> LabelDeclSection
	-> ConstSection
	-> ResourceString				; added this over DGrok because the rules are slightly different
	-> TypeSection
	-> VarSection  (and ThreadVar)

	-> MethodImplementation

	-> ExportsStatement
	-> AssemblyAttribute
}
	case CurrentTokenKind of
	ptLabel:				Result := ParseLabelDeclarationSection;				// ntLabel
	ptConst:				Result := ParseConstSection;                       // ntConstants
	ptResourceString:	Result := ParseResStringSection;
	ptType:				Result := ParseTypeSection;

   // Var is both Var and ThreadVar
	ptVar:				Result := ParseVarSection;
	ptThreadVar:		Result := ParseVarSection;

	// [class] procedure, function, constructor, destructor, operator
	ptClass:				Result := ParseProcedureDeclarationSection;
	ptProcedure:		Result := ParseProcedureDeclarationSection;
	ptFunction:			Result := ParseProcedureDeclarationSection;
	ptConstructor:		Result := ParseProcedureDeclarationSection;
	ptDestructor:		Result := ParseProcedureDeclarationSection;
	ptOperator:			Result := ParseProcedureDeclarationSection;

	ptExports:			Result := ParseExportStatement;
	ptOpenbracket:		Result := ParseAssemblyAttribute; // [assembly: Expression ]
	else
{
		implementation
		'test';  // E2029 Declaration expected but string constant found
		3.1415;  // E2029 Declaration expected but real constant found

		'E2029 %s expected but %s found'
}
		Result := SynErrorFmt(SE2029, ['Declaration', CurrentToken.Text]);
	end;
end;

function TDelphiParser.ParseQualifiedIdent: TSyntaxNode2;
var
	identTok, dotTok: TSyntaxToken;
	fullName: string;
begin
{
http://dgrok.excastle.com/Grammar.html#QualifiedIdent

QualifiedIdent
	-> Ident ('.' ExtendedIdent)*

Example

unit System.Generics.Collections;
     ^^^^^^ ^^^^^^^^ ^^^^^^^^^^^
      part    part       part

ntQualifiedIdentifier anName="System.Generics.Collections"
	ptIdentifier('System')
	ptDot('.')
	ptIdentifier('Generics')
	ptDot('.')
	ptIdentifier('Collections')


Design notes
------------

- We *preserve* every token (identifiers and dots) as child nodes so a
		round-trip printer can emit exact source (tokens + trivia).
- We also compute a convenient logical name into anName for tooling.
- Delphi's reference-counted strings make concatenation efficient here;
		a separate string list adds no real benefit unless we want the parts.
}
	Result := TSyntaxNode2.Create(ntQualifiedIdentifier);

	// First required identifier (synthesized if missing).
	// Assumption: Eat returns an ntToken with Value set to the token’s ValueText.
	identTok := EatToken(ptIdentifier); // Identifier
	Result.AddChild(identTok);
	fullName := identTok.ValueText; // initialize fullName with the first identifier

	// Zero or more repetitions of: '.' Identifier
	while CurrentToken.TokenKind = ptDot do
	begin
		// The '.' token (kept in tree for round-tripping).
		dotTok := EatToken(ptDot);
		Result.AddChild(dotTok);

		// The next identifier token
		identTok := EatToken(ptIdentifier);
		Result.AddChild(identTok);

		// Append ".<identifier>" to the assembled name.
		fullName := fullName + '.' + identTok.ValueText;
	end;

	// Store the logical/assembled unit name for easy consumption by tools.
	Result.Attributes[anName] := fullName;
end;

function TDelphiParser.ParseInterfaceHeritage: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptOpenParen));
	Result.AddChild(ParseAncestorList);
	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParseInterfaceGUID: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntGuid);
//FStack.Push(ntGuid);

	Result.AddChild(EatToken(ptOpenBracket));
	Result.AddChild(ParseCharString);
	Result.AddChild(EatToken(ptCloseBracket));
end;

function TDelphiParser.ParseAccessSpecifier: TSyntaxNode2;
begin
{
PropertyDirective
	Backlinks: Property
	-> ';' DEFAULT
	-> DEFAULT Expression
	-> DISPID Expression
	-> IMPLEMENTS (QualifiedIdent [','])+
	-> INDEX Expression
	-> NODEFAULT
	-> READ Expression
	-> READONLY
	-> STORED Expression
	-> WRITE Expression
	-> WRITEONLY
}
	case CurrentTokenExID of
	ptRead:	Result := TSyntaxNode2.Create(ntRead);
	ptWrite:	Result := TSyntaxNode2.Create(ntWrite);
	else
		Result := TSyntaxNode2.Create(ntUnknown);
	end;

	case CurrentTokenExID of
	ptRead:
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseReadAccessIdentifier);
		end;
	ptWrite:
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseWriteAccessIdentifier);
		end;
	ptReadOnly:
		begin
			Result.AddChild(EatToken);
		end;
	ptWriteOnly:
		begin
			Result.AddChild(EatToken);
		end;
//	ptAdd:
//		begin
//			Result.AddChild(EatToken);
//			QualifiedIdentifier; //TODO: AddAccessIdentifier
//		end;
//	ptRemove:
//		begin
//			Result.AddChild(EatToken);
//			QualifiedIdentifier; //TODO: RemoveAccessIdentifier
//	end;
	else
{
		TCustomer = class
			property DisplayName: string except // E2128 INDEX, READ or WRITE clause expected, but 'EXCEPT' found
		end;

		E2128 %s clause expected, but %s found
}
		Result.AddChild(SynErrorFmt(SE2128, ['INDEX, READ or WRITE', CurrentToken.Text]));
	end;
end;

function TDelphiParser.ParseReadAccessIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(ParseVariable);
end;

function TDelphiParser.ParseWriteAccessIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(ParseVariable);
end;

function TDelphiParser.ParseStorageSpecifier: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenExID of
	ptStored:		Result.AddChild(ParseStorageStored);
	ptDefault:		Result.AddChild(ParseStorageDefault);
	ptNoDefault:	Result.AddChild(ParseStorageNoDefault);
	else
		Result.AddChild(SynError('InvalidStorageSpecifier'));
	end;
end;

function TDelphiParser.ParseStorageDefault: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntDefault);
//FStack.Push(ntDefault);

	Result.AddChild(EatTokenEx(ptDefault));
	Result.AddChild(ParseStorageExpression);
end;

function TDelphiParser.ParseStorageNoDefault: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatTokenEx(ptNoDefault));
end;

function TDelphiParser.ParseStorageStored: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatTokenEx(ptStored));
	case CurrentTokenKind of
	ptIdentifier:			Result.AddChild(ParseStorageIdentifier);
	else
		if CurrentTokenKind <> ptSemiColon then
			Result.AddChild(ParseStorageExpression);
	end;
end;

function TDelphiParser.ParseStorageExpression: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(ParseConstantExpression);
end;

function TDelphiParser.ParseStorageIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;
	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParsePropertyParameterList: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptOpenBracket));
	Result.AddChild(ParseFormalParameterSection);
	while CurrentTokenKind = ptSemiColon do
	begin
		Result.AddChild(EatToken(ptSemicolon));
		Result.AddChild(ParseFormalParameterSection);
	end;
	Result.AddChild(EatToken(ptCloseBracket));
end;

function TDelphiParser.ParsePropertySpecifiers: TSyntaxNode2;
begin
	Result := PoisonNode;

	if CurrentTokenExID = ptIndex then
		Result.AddChild(ParseIndexSpecifier);

	while CurrentTokenExID in [ptRead, ptReadOnly, ptWrite, ptWriteOnly{, ptAdd, ptRemove}] do
	begin
		Result.AddChild(ParseAccessSpecifier);
		if CurrentTokenKind = ptSemicolon then
			Result.AddChild(EatToken);
	end;

	if CurrentTokenExID = ptDispId then
		Result.AddChild(ParseDispIDSpecifier);

	while CurrentTokenExID in [ptDefault, ptNoDefault, ptStored] do
	begin
		Result.AddChild(ParseStorageSpecifier);
		if CurrentTokenKind = ptSemicolon then
			Result.AddChild(EatToken);
	end;

	if CurrentTokenExID = ptImplements then
		Result.AddChild(ParseImplementsSpecifier);

	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken);
end;

function TDelphiParser.ParsePropertyInterface: TSyntaxNode2;
begin
	Result := PoisonNode;

	if CurrentTokenKind = ptOpenBracket then
		Result.AddChild(ParsePropertyParameterList);

	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseTypeId);
end;

function TDelphiParser.ParseClassMethodHeading: TSyntaxNode2;
var
	aheadTokenKind: TptTokenKind;
begin
	Result := TSyntaxNode2.Create(ntMethod);

	if CurrentTokenKind = ptClass then
		Result.AddChild(ParseClassClass);

//	InitAhead;
	Result.AddChild(EatToken);
	Result.AddChild(ParseFunctionProcedureName);
	aheadTokenKind := CurrentTokenKind;
//	RestoreAhead;


	if aheadTokenKind = ptEquals then
		Result.AddChild(ParseClassMethodResolution)
	else
	begin
		case CurrentTokenKind of
		ptConstructor: ConstructorHeading(Result);
		ptDestructor:  DestructorHeading(Result);
		ptFunction:    ClassFunctionHeading(Result);
		ptProcedure:   ClassProcedureHeading(Result);
		ptIdentifier:
			begin
				if CurrentTokenExID = ptOperator then
					ClassOperatorHeading(Result)
				else
					Result.AddChild(SynError('InvalidProcedureMethodDeclaration'));
			end;
		else
			Result.AddChild(SynError('InvalidClassMethodHeading'));
		end;
	end;
end;

procedure TDelphiParser.ClassFunctionHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anKind] := AttributeValueToStr(avFunction);

	ParentNode.AddChild(EatToken(ptFunction));
	ParentNode.AddChild(ParseFunctionProcedureName);

	if CurrentTokenKind = ptOpenParen then
		ParentNode.AddChild(ParseFormalParameterList);

	ParentNode.AddChild(EatToken(ptColon));
	ParentNode.AddChild(ParseReturnType);

	if CurrentTokenKind = ptSemicolon then
		ParentNode.AddChild(EatToken(ptSemicolon));

	if CurrentTokenExID in ClassMethodDirectiveEnum then
		ClassMethodDirective(ParentNode);
end;

function TDelphiParser.ParseFunctionMethodName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

	Result.AddChild(EatToken(ptIdentifier));
end;

procedure TDelphiParser.ClassProcedureHeading(ParentNode: TSyntaxNode2);
begin
  ParentNode.Attributes[anKind] := AttributeValueToStr(avProcedure);

	ParentNode.AddChild(EatToken(ptProcedure));
	ParentNode.AddChild(ParseFunctionProcedureName);
	if CurrentTokenKind = ptOpenParen then
	begin
		ParentNode.AddChild(ParseFormalParameterList);
	end;
	if CurrentTokenKind = ptSemicolon then
		ParentNode.AddChild(EatToken(ptSemicolon));

	if CurrentTokenExID = ptDispId then
	begin
		ParentNode.AddChild(ParseDispIDSpecifier);
		if CurrentTokenKind = ptSemicolon then
			ParentNode.AddChild(EatToken(ptSemicolon));
	end;
	if CurrentTokenExID in ClassMethodDirectiveEnum then
		ClassMethodDirective(ParentNode);
end;

function TDelphiParser.ParseProcedureMethodName: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseClassMethodResolution: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntResolutionClause);
//	FStack.Push(ntResolutionClause);

	case CurrentTokenKind of
	ptFunction: Result.AddChild(EatToken);
	ptProcedure: Result.AddChild(EatToken);
	ptIdentifier:
		begin
			if CurrentTokenExID = ptOperator then
				Result.AddChild(EatToken);
		end;
	end;

	Result.AddChild(ParseFunctionProcedureName);
	Result.AddChild(EatToken(ptEquals));
	Result.AddChild(ParseFunctionMethodName);
	Result.AddChild(EatToken(ptSemicolon));
end;

procedure TDelphiParser.ClassOperatorHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatTokenEx(ptOperator));
	ParentNode.AddChild(ParseFunctionProcedureName);
	if CurrentTokenKind = ptOpenParen then
	begin
		ParentNode.AddChild(ParseFormalParameterList);
	end;

	if CurrentTokenKind = ptColon then
	begin
		ParentNode.AddChild(EatToken(ptColon));
		ParentNode.AddChild(ParseReturnType);
	end;

	if CurrentTokenKind = ptSemicolon then
		ParentNode.AddChild(EatToken(ptSemicolon));

	if CurrentTokenExID in ClassMethodDirectiveEnum then
		ClassMethodDirective(ParentNode);
end;

function TDelphiParser.ParseConstraint: TSyntaxNode2;
begin
	Result := PoisonNode;

	while CurrentTokenKind in [ptConstructor, ptRecord, ptClass, ptIdentifier] do
	begin
		case CurrentTokenKind of
		ptConstructor: Result.AddChild(ParseConstructorConstraint);
		ptRecord: Result.AddChild(ParseRecordConstraint);
		ptClass: Result.AddChild(ParseClassConstraint);
		ptIdentifier: Result.AddChild(ParseTypeId);
		end;

		if CurrentTokenKind = ptComma then
			Result.AddChild(EatToken);
	end;
end;

function TDelphiParser.ParseConstraintList: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntConstraints);
//FStack.Push(ntConstraints);
	Result.AddChild(ParseConstraint);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(ParseConstraint);
	end;
end;

function TDelphiParser.ParseConstructorConstraint: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntConstructorConstraint);
	Result.AddChild(EatToken(ptConstructor));
end;

procedure TDelphiParser.ConstructorHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptConstructor));
	ConstructorName(ParentNode);

	if CurrentTokenKind = ptOpenParen then
	begin
		ParentNode.AddChild(ParseFormalParameterList);
	end;
	if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(EatToken(ptSemicolon));

	ClassMethodDirective(ParentNode);
end;

procedure TDelphiParser.ConstructorName(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anKind] := AttributeValueToStr(avConstructor);
	ParentNode.Attributes[anName] := CurrentToken.ValueText;

	ParentNode.AddChild(EatToken(ptIdentifier));
end;

procedure TDelphiParser.DestructorHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptDestructor));
	DestructorName(ParentNode);
	if CurrentTokenKind = ptOpenParen then
		ParentNode.AddChild(ParseFormalParameterList);

	if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(EatToken(ptSemicolon));
	ClassMethodDirective(ParentNode);
end;

procedure TDelphiParser.DestructorName(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anKind] := AttributeValueToStr(avDestructor);
	ParentNode.Attributes[anName] := CurrentToken.ValueText;

	ParentNode.AddChild(EatToken(ptIdentifier));
end;

procedure TDelphiParser.ClassMethod(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anClass] := AttributeValueToStr(avTrue);
	ParentNode.AddChild(EatToken(ptClass));
end;

procedure TDelphiParser.ClassMethodDirective(ParentNode: TSyntaxNode2);
begin
	while CurrentTokenExID in ClassMethodDirectiveEnum do
	begin
		if CurrentTokenExID = ptDispId then
			ParentNode.AddChild(ParseDispIDSpecifier)
		else
			ProceduralDirective(ParentNode);

		if CurrentTokenKind = ptSemicolon then
			ParentNode.AddChild(EatToken(ptSemicolon));
	end;
end;

procedure TDelphiParser.ObjectMethodHeading(ParentNode: TSyntaxNode2);
begin
	case CurrentTokenKind of
	ptConstructor:		ObjectConstructorHeading(ParentNode);
	ptDestructor:		ObjectDestructorHeading(ParentNode);
	ptFunction:			ObjectFunctionHeading(ParentNode);
	ptProcedure:		ObjectProcedureHeading(ParentNode);
	else
		ParentNode.AddChild(SynError('InvalidMethodHeading'));
	end;
end;

procedure TDelphiParser.ObjectFunctionHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptFunction));
	ParentNode.AddChild(ParseFunctionMethodName);
	if CurrentTokenKind = ptOpenParen then
		ParentNode.AddChild(ParseFormalParameterList);

	ParentNode.AddChild(EatToken(ptColon));
	ParentNode.AddChild(ParseReturnType);
	if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(EatToken(ptSemicolon));

	ObjectMethodDirective(ParentNode);
end;

procedure TDelphiParser.ObjectProcedureHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptProcedure));
	ParentNode.AddChild(ParseProcedureMethodName);
	if CurrentTokenKind = ptOpenParen then
		ParentNode.AddChild(ParseFormalParameterList);

	if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(EatToken(ptSemicolon));

	ObjectMethodDirective(ParentNode);
end;

procedure TDelphiParser.ObjectConstructorHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptConstructor));
	ConstructorName(ParentNode);

	if CurrentTokenKind = ptOpenParen then
		ParentNode.AddChild(ParseFormalParameterList);

	if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(EatToken(ptSemicolon));

	ObjectMethodDirective(ParentNode);
end;

procedure TDelphiParser.ObjectDestructorHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptDestructor));
	DestructorName(ParentNode);

	if CurrentTokenKind = ptOpenParen then
	begin
		ParentNode.AddChild(ParseFormalParameterList);
	end;

	if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(EatToken(ptSemicolon));

	ObjectMethodDirective(ParentNode);
end;

procedure TDelphiParser.ObjectMethodDirective(ParentNode: TSyntaxNode2);
begin
	while CurrentTokenExID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
		ptMessage, ptNear, ptOverload, ptPascal, ptRegister, ptSafeCall, ptStdCall,
		ptVirtual, ptDeprecated, ptLibrary, ptPlatform, ptStatic, ptInline] do
	begin
		ProceduralDirective(ParentNode);
		if CurrentTokenKind = ptSemiColon then
			ParentNode.AddChild(EatToken(ptSemicolon));
	end;
end;

function TDelphiParser.ParseDirective: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Directive

Directive
			Backlinks: MethodHeading, ProcedureType
	-> [';'] ABSTRACT
	-> [';'] ASSEMBLER
	-> [';'] CDECL
	-> [';'] DISPID Expression
	-> [';'] DYNAMIC
	-> [';'] EXPORT
	-> [';'] EXTERNAL [Expression (ExportsSpecifier)*]
	-> [';'] FAR
	-> [';'] FINAL
	-> [';'] FORWARD
	-> [';'] INLINE
	-> [';'] LOCAL
	-> [';'] MESSAGE Expression
	-> [';'] NEAR
	-> [';'] OVERLOAD
	-> [';'] OVERRIDE
	-> [';'] PASCAL
	-> [';'] REGISTER
	-> [';'] REINTRODUCE
	-> [';'] SAFECALL
	-> [';'] STATIC
	-> [';'] STDCALL
	-> [';'] VARARGS
	-> [';'] VIRTUAL
	-> [';'] PortabilityDirective
}
	Result := TSyntaxNode2.Create(ntHintDirective);

	// the leading semicolon is optional
	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken(ptSemicolon));

	case CurrentTokenExID of
	ptAbstract,			//		-> [';'] ABSTRACT
	ptAssembler,		//		-> [';'] ASSEMBLER
	ptCDecl,				//		-> [';'] CDECL
	ptDispid,			//		-> [';'] DISPID Expression
	ptDynamic,			//		-> [';'] DYNAMIC
	ptExport, 			//		-> [';'] EXPORT
	ptExternal, 		//		-> [';'] EXTERNAL [Expression (ExportsSpecifier)*]
	ptFar,				//		-> [';'] FAR
	ptFinal,				//		-> [';'] FINAL
	ptForward,			//		-> [';'] FORWARD
	ptInline,			//		-> [';'] INLINE
	ptLocal,				//		-> [';'] LOCAL
	ptMessage,			//		-> [';'] MESSAGE Expression
	ptNear,				//		-> [';'] NEAR
	ptOverload,			//		-> [';'] OVERLOAD
	ptOverride,			//		-> [';'] OVERRIDE
	ptPascal,			//		-> [';'] PASCAL
	ptRegister,			//		-> [';'] REGISTER
	ptReintroduce,		//		-> [';'] REINTRODUCE
	ptSafecall,			//		-> [';'] SAFECALL
	ptStatic,			//		-> [';'] STATIC
	ptStdcall,			//		-> [';'] STDCALL
	ptVarargs,			//		-> [';'] VARARGS
	ptVirtual:			//		-> [';'] VIRTUAL
		Result.AddChild(EatToken(CurrentTokenExID));
	else
//		-> [';'] PortabilityDirective
		Result.AddChild(ParsePortabilityDirective);
	end;
end;

function TDelphiParser.ParseDirective16Bit: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenExID of
	ptNear:		Result.AddChild(EatToken);
	ptFar:		Result.AddChild(EatToken);
	ptExport:	Result.AddChild(EatToken);
	else
		Result.AddChild(SynError('InvalidDirective16Bit'));
	end;
end;

procedure TDelphiParser.DirectiveBinding(ParentNode: TSyntaxNode2);
var
	token: string;
begin
	token := CurrentToken.ValueText;

	// Method bindings:
	if SameText(token, 'override') or SameText(token, 'virtual') or SameText(token, 'dynamic') then
		ParentNode.Attributes[anMethodBinding] := token

	// Other directives
	else if SameText(token, 'reintroduce') then
		ParentNode.Attributes[anReintroduce] := AttributeValueToStr(avTrue)
	else if SameText(token, 'overload') then
		ParentNode.Attributes[anOverload] := AttributeValueToStr(avTrue)
	else if SameText(token, 'abstract') then
		ParentNode.Attributes[anAbstract] := AttributeValueToStr(avTrue);

	case CurrentTokenExID of
	ptAbstract:		ParentNode.AddChild(EatToken);
	ptVirtual:		ParentNode.AddChild(EatToken);
	ptDynamic:		ParentNode.AddChild(EatToken);
	ptMessage:		ParentNode.AddChild(ParseDirectiveBindingMessage);
	ptOverride:		ParentNode.AddChild(EatToken);
	ptOverload:		ParentNode.AddChild(EatToken);
	ptReintroduce:	ParentNode.AddChild(EatToken);
	else
		ParentNode.AddChild(SynError('InvalidDirectiveBinding'));
	end;
end;

function TDelphiParser.ParseDirectiveBindingMessage: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntMessage);
	Result.AddChild(EatToken);
	Result.AddChild(ParseConstantExpression);
end;

function TDelphiParser.ParseReturnType: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntReturnType);
//	FStack.Push(ntReturnType);

//	while CurrentTokenKind = ptOpenBracket do
//		Result.AddChild(ParseCustomAttribute);

	Result.AddChild(ParseTypeId);
end;

function TDelphiParser.ParseRoundClose: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntRoundClose);
	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParseRoundOpen: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntRoundOpen);

	Result.AddChild(EatToken(ptOpenParen));
end;

function TDelphiParser.ParseFormalParameterList: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptOpenParen));
	Result.AddChild(ParseFormalParameterSection);

	while CurrentTokenKind = ptSemiColon do
	begin
		Result.AddChild(EatToken(ptSemicolon));
		Result.AddChild(ParseFormalParameterSection);
	end;

	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParseFormalParameterSection: TSyntaxNode2;
begin
	Result := PoisonNode;


//	while CurrentTokenKind = ptOpenBracket do
//		Result.AddChild(ParseCustomAttribute);

	case CurrentTokenKind of
	ptConst: Result.AddChild(ParseConstParameter);
	ptIdentifier:
		case CurrentTokenExID of
		ptOut: Result.AddChild(ParseOutParameter);
		else
			Result.AddChild(ParseParameterFormal);
		end;
	ptIn: Result.AddChild(ParseInParameter);
	ptVar: Result.AddChild(ParseVarParameter);
	end;
end;

function TDelphiParser.ParseConstParameter: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntParameter);
	Result.Attributes[anKind] := AttributeValueToStr(avConst);

	Result.AddChild(EatToken(ptConst));
	Result.AddChild(ParseParameterNameList);
	case CurrentTokenKind of
	ptColon:
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseFormalParameterType);
			if CurrentTokenKind = ptEquals then
			begin
				Result.AddChild(EatToken);
				Result.AddChild(ParseTypedConstant);
			end;
		end
	end;
end;

function TDelphiParser.ParseVarParameter: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntParameters);
	Result.Attributes[anKind] := AttributeValueToStr(avVar);

	Result.AddChild(EatToken(ptVar));
	Result.AddChild(ParseParameterNameList);
	case CurrentTokenKind of
	ptColon:
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseFormalParameterType);
		end
	end;
end;

function TDelphiParser.ParseOutParameter: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntParameters);
	Result.Attributes[anKind] := AttributeValueToStr(avOut);
	Result.AddChild(EatTokenEx(ptOut));
	Result.AddChild(ParseParameterNameList);
	case CurrentTokenKind of
	ptColon:
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseFormalParameterType);
		end
	end;
end;

function TDelphiParser.ParseParameterFormal: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntParameters);
	case CurrentTokenKind of
	ptIdentifier:
		begin
			Result.AddChild(ParseParameterNameList);
			Result.AddChild(EatToken(ptColon));
			Result.AddChild(ParseFormalParameterType);
			if CurrentTokenKind = ptEquals then
			begin
				Result.AddChild(EatToken(ptEquals));
				Result.AddChild(ParseTypedConstant);
			end;
		end;
	else
		Result.AddChild(SynError('InvalidParameter'));
	end;
end;

function TDelphiParser.ParseParameterNameList: TSyntaxNode2;
begin
	Result := PoisonNode;


//	while CurrentTokenKind = ptOpenBracket do
//		Result.AddChild(ParseCustomAttribute);

	Result.AddChild(ParseParameterName);

	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken);

//		while CurrentTokenKind = ptOpenBracket do
//			Result.AddChild(ParseCustomAttribute);
		Result.AddChild(ParseParameterName);
	end;
end;

function TDelphiParser.ParseParameterName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseFormalParameterType: TSyntaxNode2;
begin
	Result := PoisonNode;


  if CurrentTokenKind = ptArray then
    ParseStructuredType
  else
    Result.AddChild(ParseTypeId);
end;

function TDelphiParser.ParseFunctionMethodDeclaration: TSyntaxNode2;
var
	methodName: string;
begin
	Result := PoisonNode;


	if (CurrentTokenKind = ptIdentifier) and (CurrentTokenExID = ptOperator) then
	begin
		Result.AddChild(EatToken);
		Exit;
	end;

	methodName := LowerCase(CurrentToken.ValueText);
	Result.Attributes[anKind] := methodName;

	Result.AddChild(ParseMethodKind);
	Result.AddChild(ParseFunctionProcedureName);

	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseFormalParameterList);

	case CurrentTokenKind of
	ptSemiColon: FunctionProcedureBlock(Result);
	else
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseReturnType);
		FunctionProcedureBlock(Result);
	end;
end;

function TDelphiParser.ParseProcedureProcedureName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result.Attributes[anName] := CurrentToken.ValueText;

	Result.AddChild(ParseMethodKind);
	Result.AddChild(ParseFunctionProcedureName);

	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseFormalParameterList);

	FunctionProcedureBlock(Result);
end;

function TDelphiParser.ParseFunctionProcedureName: TSyntaxNode2;
//var
//	ChildNode, NameNode, TypeParam, TypeNode, Temp: TSyntaxNode2;
//	FullName, TypeParams: string;
begin
	Result := TSyntaxNode2.Create(ntName);
(*
  NameNode := FStack.Peek;
	  Result.AddChild(ParseObjectNameOfMethod);
    for ChildNode in NameNode.ChildNodes do
    begin
      if ChildNode.NodeType = ntTypeParams then
      begin
        TypeParams := '';

        for TypeParam in ChildNode.ChildNodes do
        begin
          TypeNode := TypeParam.FindNode(ntType);
          if Assigned(TypeNode) then
          begin
            if TypeParams <> '' then
              TypeParams := TypeParams + ',';
            TypeParams := TypeParams + TypeNode.Attributes[anName];
          end;
        end;

        FullName := FullName + '<' + TypeParams + '>';
        Continue;
      end;

      if FullName <> '' then
        FullName := FullName + '.';
      FullName := FullName + ChildNode.Value;
    end;
  finally
    FStack.Pop;
    Temp := FStack.Peek;
//  DoHandleString(FullName);
    Temp.Attributes[anName] := FullName;
    Temp.DeleteChild(NameNode);
  end;
*)
end;

function TDelphiParser.ParseObjectNameOfMethod: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

  if CurrentTokenKind = ptIn then
    Result.AddChild(EatToken(ptIn))
  else
    Result.AddChild(EatToken(ptIdentifier));

  if CurrentTokenKind = ptLessThan then
    Result.AddChild(ParseTypeParams);
  if CurrentTokenKind = ptDot then
  begin
    Result.AddChild(EatToken(ptDot));
    Result.AddChild(ParseObjectNameOfMethod);
  end;
end;

procedure TDelphiParser.FunctionProcedureBlock(ParentNode: TSyntaxNode2);
var
  HasBlock: Boolean;
begin
  HasBlock := True;
  if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(EatToken(ptSemicolon));

  while CurrentTokenExID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptDelayed, ptFar,
    ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
    ptReintroduce, ptSafeCall, ptStdCall, ptVirtual, ptLibrary,
    ptPlatform, ptLocal, ptVarargs, ptAssembler, ptStatic, ptInline, ptForward,
    ptExperimental, ptDeprecated] do
  begin
    case CurrentTokenExID of
      ptExternal:
        begin
          ProceduralDirective(ParentNode);
          HasBlock := False;
        end;
      ptForward:
        begin
//          FStack.AddChild(ParseForwardDeclaration);		todo: figure this out
          HasBlock := False;
        end
    else
      begin
        ProceduralDirective(ParentNode);
      end;
    end;
    if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(EatToken(ptSemicolon));
  end;

  if HasBlock then
  begin
    case CurrentTokenKind of
      ptAsm:
        begin
          ParentNode.AddChild(ParseAssemblerStatement);
        end;
    else
      begin
        ParentNode.AddChild(ParseBlock);
      end;
    end;
    ParentNode.AddChild(EatToken(ptSemicolon));
  end;
end;

function TDelphiParser.ParseExternalDirective: TSyntaxNode2;
begin
{
	Returns an ntExternal node.

	// simplest form: function comes from DLL, entry point name is same as Delphi name
	function Beep: BOOL; stdcall; external 'kernel32.dll';

	// external with explicit entry point name
	function MessageBoxW(hWnd: HWND; lpText, lpCaption: PWideChar; uType: UINT): Integer; stdcall; external 'user32.dll' name 'MessageBoxW';

	// external with index instead of name
	procedure InitProc; stdcall; external 'mylib.dll' index 17;

	// external with library and delayed loading
	function SHCreateItemFromParsingName(pszPath: LPCWSTR; pbc: IBindCtx;
			const riid: TGUID; out ppv): HRESULT; stdcall;
			external 'shell32.dll' delayed;

	// external with alias name and ordinal
	function SomeFunc: Integer; stdcall; external 'legacy.dll' name 'LegacyEntry' index 5;
}
	Result := TSyntaxNode2.Create(ntExternal);
	Result.AddChild(EatTokenEx(ptExternal)); // expect ptExternal, and move to next token

	case CurrentTokenKind of
	ptSemiColon: Result.AddChild(EatToken(ptSemicolon));
	else
		begin
			if CurrentTokenExID <> ptName then
				Result.AddChild(ParseSimpleExpression);

			if CurrentTokenExID = ptDelayed then
				Result.AddChild(EatToken);

			ExternalDirectiveTwo(Result);
		end;
	end;

end;

procedure TDelphiParser.ExternalDirectiveTwo(ParentNode: TSyntaxNode2);
begin
	case CurrentTokenExID of
	ptIndex: ParentNode.AddChild(ParseIndexSpecifier);
	ptName:
		begin
			ParentNode.AddChild(EatToken);
			ParentNode.AddChild(ParseSimpleExpression);
		end;
	ptSemiColon:
		begin
			ParentNode.AddChild(EatToken(ptSemicolon));
			ParentNode.AddChild(ParseExternalDirectiveThree);
		end;
	end
end;

function TDelphiParser.ParseExternalDirectiveThree: TSyntaxNode2;
begin
   Result := PoisonNode;

	case CurrentTokenKind of
	ptMinus: Result.AddChild(EatToken);
	end;

	case CurrentTokenKind of
	ptIdentifier, ptIntegerConst: Result.AddChild(EatToken);
	end;
end;

function TDelphiParser.ParseForStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ForStatement

ForStatement
	-> FOR Ident ':=' Expression (TO | DOWNTO) Expression DO [Statement]
	-> FOR Ident IN Expression DO [Statement]
}
	Result := TSyntaxNode2.Create(ntFor);
	Result.AddChild(EatToken(ptFor));
	if CurrentTokenKind = ptVar then
	begin
		Result.AddChild(EatToken);
		Result.AddChild(ParseInlineVarDeclaration);
	end
	else
		Result.AddChild(ParseQualifiedIdent);

	if CurrentToken.TokenKind = ptAssign then
	begin
		Result.AddChild(EatToken(ptAssign));
		Result.AddChild(ParseForStatementFrom);
		case CurrentTokenKind of
		ptTo: Result.AddChild(ParseForStatementTo);
		ptDownTo: Result.AddChild(ParseForStatementDownTo);
		else
			Result.AddChild(SynError('InvalidForStatement'));
		end;
	end
	else if CurrentToken.TokenKind = ptIn then
		Result.AddChild(ParseForStatementIn);

	Result.AddChild(EatToken(ptDo));
	Result.AddChild(ParseStatement);
end;

function TDelphiParser.ParseForStatementDownTo: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntDownTo);
	Result.AddChild(EatToken(ptDownTo));
	Result.AddChild(ParseExpression);
end;

function TDelphiParser.ParseForStatementFrom: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntFrom);
	Result.AddChild(ParseExpression);
end;

function TDelphiParser.ParseForStatementIn: TSyntaxNode2;
begin
   Result := TSyntaxNode2.Create(ntIn);
//  FStack.Push(ntIn);
	  Result.AddChild(EatToken(ptIn));
	  Result.AddChild(ParseExpression);
end;

function TDelphiParser.ParseForStatementTo: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntTo);
//  FStack.Push(ntTo);
	  Result.AddChild(EatToken(ptTo));
	  Result.AddChild(ParseExpression);
end;

function TDelphiParser.ParseWhileStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#WhileStatement

WhileStatement
	-> WHILE Expression DO [Statement]
}
	Result := TSyntaxNode2.Create(ntWhile);
//  FStack.Push(ntWhile);
	  Result.AddChild(EatToken(ptWhile));
	  Result.AddChild(ParseExpression);
	  Result.AddChild(EatToken(ptDo));
	  Result.AddChild(ParseStatement);
end;

function TDelphiParser.ParseRepeatStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#RepeatStatement

RepeatStatement
	-> REPEAT [StatementList] UNTIL Expression
}
	Result := TSyntaxNode2.Create(ntRepeat);
//  FStack.Push(ntRepeat);
	  Result.AddChild(EatToken(ptRepeat));
	  Result.AddChild(ParseStatementList);
	  Result.AddChild(EatToken(ptUntil));
	  Result.AddChild(ParseExpression);
end;

function TDelphiParser.ParseCaseStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#CaseStatement

CaseStatement
	-> CASE Expression OF
			CaseSelector (';' CaseSelector)* [';']
			[ELSE StatementList]
			END

CaseSelector
	-> (ExpressionOrRange (',' ExpressionOrRange)*) ':' [Statement]
}
	Result := TSyntaxNode2.Create(ntCase);

//	CASE Expression OF
	Result.AddChild(EatToken(ptCase));
	Result.AddChild(ParseExpression);
	Result.AddChild(EatToken(ptOf));

//	CaseSelector (';' CaseSelector)* [';']

	// First the mandatory selector
	Result.AddChild(ParseCaseSelector);

	// Now: zero or more (';' CaseSelector) and an optional trailing ';'
	while CurrentTokenKind = ptSemicolon do
	begin
		Result.AddChild(EatToken(ptSemicolon));

		// If the semicolon is trailing, stop (it's the [';'] before ELSE/END)
		if (CurrentTokenKind = ptElse) or (CurrentTokenKind = ptEnd) then
			Break;

		// If we see another semicolon, it's an empty CaseSelector: recover.
		while CurrentTokenKind = ptSemicolon do
		begin
			// Error: Missing case selector before semicolon
			Result.AddChild(SynErrorFmt('Missing case selelctor before semicolon', []));
			Result.AddChild(EatToken(ptSemicolon));

			if (CurrentTokenKind = ptElse) or (CurrentTokenKind = ptEnd) then
				Break;
		end;

		Result.AddChild(ParseCaseSelector);
	end;


//	[ELSE StatementList]
	if (CurrentTokenKind = ptElse) and (PeekTokenKind <> ptEnd) then
	begin
		Result.AddChild(EatToken(ptElse));
		Result.AddChild(ParseStatementList);
	end;

//	END
	Result.AddChild(EatToken(ptEnd));
end;

function TDelphiParser.ParseCaseSelector: TSyntaxNode2;
begin
(*
http://dgrok.excastle.com/Grammar.html#CaseSelector

CaseSelector
	-> (ExpressionOrRange [','])+
			':' [Statement] [';']

CaseStatement
	-> CASE Expression OF
			(CaseSelector)+
			[ELSE [StatementList]]
			END

Or combined:

CaseSelector
	-> (ExpressionOrRange [','])+ ':' [Statement] [';']

CaseStatement
	-> CASE Expression OF
			( (ExpressionOrRange [','])+ ':' [Statement] [';'] )+
			[ELSE [StatementList]]
			END

Is there any situation where the ; is allowed at the end of case?

	case foo of
   vrOne: {nop};
	vrTwo: {nop}
	vrThree: {nop};
	end;

You break out of the CaseSelector by being an ptElse or a ptEnd

*)
	Result := TSyntaxNode2.Create(ntCaseSelector);

//	(ExpressionOrRange [','])+
	Result.AddChild(ParseCaseLabelList);
	while (CurrentTokenKind = ptComma) do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseExpressionOrRange);
	end;

//	':' [Statement] [';']
	Result.AddChild(EatToken(ptColon));

//	if IsPossibleStatement then  lets use a different way to do this one
		Result.AddChild(ParseStatement);

	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParseExpressionOrRange: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExpressionOrRange

ExpressionOrRange
	-> SimpleExpression ['..' SimpleExpression]
}
	Result := TSyntaxNode2.Create(ntExpression);

//	SimpleExpression
	Result.AddChild(ParseSimpleExpression);	// ntExpression

//	['..' SimpleExpression]
	if CurrentTokenKind = ptDotDot then
	begin
		Result.AddChild(EatToken(ptDotDot));
		Result.AddChild(ParseSimpleExpression);
	end;
end;

function TDelphiParser.ParseCaseLabel: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntCaseLabel);
//	FStack.Push(ntCaseLabel);
		Result.AddChild(ParseConstantExpression);
		if CurrentTokenKind = ptDotDot then
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseConstantExpression);
		end;
end;

function TDelphiParser.ParseIfStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#IfStatement

IfStatement
	-> IF Expression THEN [Statement]
			[ELSE [Statement]]
}
	Result := TSyntaxNode2.Create(ntIf);
//  FStack.Push(ntIf);
	  Result.AddChild(EatToken(ptIf));
	  Result.AddChild(ParseExpression);
	  Result.AddChild(ParseThenStatement);
	  if CurrentTokenKind = ptElse then
	    Result.AddChild(ParseElseStatement);
end;

function TDelphiParser.ParseExceptionItem: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExceptionItem

ExceptionItem
	-> ON
			[Ident ':']
			QualifiedIdent DO
			[Statement]
			[';']

Backlinks: TryStatement

	try except on   exception do; end;
	try except on E:exception do  end;
	try except on E:exception do; end;
	           \________________/

}
	Result := TSyntaxNode2.Create(ntExceptionHandler);
	Result.AddChild(EatToken(ptOn));

{
	TODO: This needs to change. since a qualified identifier is just a bunch of identifiers separated by dots.

   There's 3 syntaxes above:

      on       QualifiedIdent do
      on Ident:QualifiedIdent do

   But what is a QualifiedIdent but
         Ident
         Ident.Ident
			Ident.Ident.Ident
			...

   So in order to peek we are really looking for:

		- ident:ident.ident do
		- ident:ident do
		- ident.ident do
		- ident do
}
	if (CurrentTokenKind = ptIdentifier) and (PeekTokenKind = ptColon) then
	begin
		Result.AddChild(ParseIdent);
		Result.AddChild(EatToken(ptColon));
	end;

//	QualifiedIdent DO
	Result.AddChild(ParseQualifiedIdent);
	Result.AddChild(EatToken(ptDo));

//	[Statement]
	if CurrentTokenKind in [ptSemicolon, ptEnd] then
		Result.AddChild(ParseStatement);

//	[';']
	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParseExceptBlock: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExcept);
//  FStack.Push(ntExcept);
     if CurrentTokenExID = ptOn then
     begin
       Result.AddChild(ParseExceptionHandlerList);
       if CurrentTokenKind = ptElse then
         Result.AddChild(ParseExceptionBlockElseBranch);
     end else
       if CurrentTokenKind = ptElse then
         Result.AddChild(ParseExceptionBlockElseBranch)
       else
         Result.AddChild(ParseStatementList);
end;

function TDelphiParser.ParseExceptionHandlerList: TSyntaxNode2;
begin
   Result := PoisonNode;

  while CurrentTokenExID = ptOn do
  begin
    Result.AddChild(ParseExceptionHandler);
    Result.AddChild(EatToken(ptSemicolon));
  end;
end;

function TDelphiParser.ParseExceptionHandler: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExceptionHandler);
//  FStack.Push(ntExceptionHandler);
	  Result.AddChild(EatTokenEx(ptOn));
	  Result.AddChild(ParseExceptionIdentifier);
	  Result.AddChild(EatToken(ptDo));
	  Result.AddChild(ParseStatement);
end;

function TDelphiParser.ParseExceptionBlockElseBranch: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntElse);
	Result.AddChild(EatToken);
	Result.AddChild(ParseStatementList);
end;

function TDelphiParser.ParseExceptionIdentifier: TSyntaxNode2;
begin
   Result := PoisonNode;

//	InitAhead;
  case PeekTokenKind of
    ptDot:
      begin
        Result.AddChild(ParseExceptionClassTypeIdentifier);
      end;
    ptColon:
      begin
        Result.AddChild(ParseExceptionVariable);
      end
  else
    begin
      Result.AddChild(ParseExceptionClassTypeIdentifier);
    end;
  end;
end;

function TDelphiParser.ParseExceptionClassTypeIdentifier: TSyntaxNode2;
begin
   Result := PoisonNode;
  Result.AddChild(ParseType);
end;

function TDelphiParser.ParseExceptionVariable: TSyntaxNode2;
var
	child: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntVariable);

	child := TSyntaxNode2.Create(ntName);
	child[anName] := CurrentToken.ValueText;

	child.AddChild(EatToken(ptIdentifier));
	child.AddChild(EatToken(ptColon));
	child.AddChild(ParseExceptionClassTypeIdentifier);

	Result.AddChild(child);
end;

function TDelphiParser.ParseInlineConstSection: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenKind of
	ptConst:
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseConstantDecl);
		end;
	else
		Result.AddChild(SynError('InvalidConstSection'));
	end;
end;

function TDelphiParser.ParseInlineStatement: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptInline));
	Result.AddChild(EatToken(ptOpenParen));
	Result.AddChild(EatToken(ptIntegerConst));
	while (CurrentTokenKind = ptSlash) do
	begin
		Result.AddChild(EatToken);
		Result.AddChild(EatToken(ptIntegerConst));
	end;

	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParseInlineVarSection: TSyntaxNode2;
//var
//	VarSect, Variables, nodeExpression: TSyntaxNode2;
//	assignNode: TSyntaxNode2;
begin
	Result := PoisonNode;


{
  VarSect := TSyntaxNode2.Create(ntUnknown);
  try
    Variables := FStack.Push(ntVariables);

    FStack.Push(VarSect);
    try
        FStack.Push(ntVariables);
        try
           Result.AddChild(EatToken(ptVar));
           while CurrentTokenKind = ptIdentifier do
             Result.AddChild(ParseInlineVarDeclaration);

         if CurrentTokenKind = ptAssign then
           begin
             Result.AddChild(EatToken);
             Expression;
           end;
        finally
          FStack.Pop;
        end;
    finally
      FStack.Pop;
    end;
    RearrangeVarSection(VarSect);
    nodeExpression := VarSect.FindNode(ntExpression);
    if Assigned(nodeExpression) then
      begin
			assignNode := TSyntaxNode2.Create(ntAssign);
			assignNode.AddChild(nodeExpression.Clone);
			Variables.AddChild(assignNode);
		end;

    FStack.Pop;
  finally
    VarSect.Free;
  end;
}
end;

function TDelphiParser.ParseInlineVarDeclaration: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntVariables);

	Result.AddChild(ParseIdentList);
	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken);
		Result.AddChild(ParseType);
	end;
end;

function TDelphiParser.ParseInParameter: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptIn));
	Result.AddChild(ParseParameterNameList);

	case CurrentTokenKind of
	ptColon:
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseFormalParameterType);
			if CurrentTokenKind = ptEquals then
			begin
				Result.AddChild(EatToken);
				Result.AddChild(ParseTypedConstant);
			end;
		end
	end;
end;

function TDelphiParser.ParseAssemblerStatement: TSyntaxNode2;
begin
{
AssemblerStatement
	-> ASM
			<assemblylanguage>
			END
}
	Result := TSyntaxNode2.Create(ntStatements);
	Result[anType] := AttributeValueToStr(avAsm);
	Result.AddChild(EatToken(ptAsm));

	// should be replaced with a Assembler lexer
	while (CurrentTokenKind <> ptEnd) and (CurrentTokenKind <> ptEOF) do
	begin
		Result.AddChild(EatToken);
	end;

// END
	Result.AddChild(EatToken(ptEnd));
end;

function TDelphiParser.ParseAsOp: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAs);
	Result.AddChild(EatToken(ptAs));
end;

function TDelphiParser.ParseAtExpression: TSyntaxNode2;
begin
{
	raise Exception.Create('Hello') at @SaveChanges;

	Read more at https://stackoverflow.com/a/8951057/12597
}
	Result := TSyntaxNode2.Create(ntAt);
		Result.AddChild(EatTokenEx(ptAt));
		Result.AddChild(ParseExpression);
end;

function TDelphiParser.ParseRaiseStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#RaiseStatement

RaiseStatement
	-> RAISE [Expression [AT Expression]]
}
	Result := TSyntaxNode2.Create(ntRaise);
     Result.AddChild(EatToken(ptRaise));
     case CurrentTokenKind of
       ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptOpenParen:
         begin
           Result.AddChild(ParseExpression);
         end;
     end;
     if CurrentTokenExID = ptAt then
       Result.AddChild(ParseAtExpression);
end;

function TDelphiParser.ParseTryStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#TryStatement

TryStatement
	-> TRY
			[StatementList]
			( FINALLY [StatementList]
			| EXCEPT (
			[StatementList] |
			(ExceptionItem)* [ELSE [StatementList]]
			)
			END
}
	Result := TSyntaxNode2.Create(ntTry);

	Result.AddChild(EatToken(ptTry));
	Result.AddChild(ParseStatementList);

	case CurrentTokenKind of
	ptExcept:
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseExceptBlock);
			Result.AddChild(EatToken(ptEnd));
		end;
	ptFinally:
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ParseFinallyBlock);
			Result.AddChild(EatToken(ptEnd));
		end;
	else
		Result.AddChild(SynError('InvalidTryStatement'));
	end;
end;

function TDelphiParser.ParseWithStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#WithStatement

WithStatement
	-> WITH ExpressionList DO [Statement]
}
	Result := TSyntaxNode2.Create(ntWith);
	  Result.AddChild(EatToken(ptWith));
	  Result.AddChild(ParseWithExpressionList);
	  Result.AddChild(EatToken(ptDo));
	  Result.AddChild(ParseStatement);
end;

function TDelphiParser.ParseWithExpressionList: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExpressions);
	Result.AddChild(ParseExpression);

	while CurrentToken.TokenKind = ptComma do
	begin
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end;
end;

function TDelphiParser.ParseStatementList: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#StatementList

StatementList
	-> ([Statement] [';'])+
}
	Result := TSyntaxNode2.Create(ntStatements);
	Result.AddChild(ParseStatements);
end;

function TDelphiParser.ParseStatementOrExpression: TSyntaxNode2;
begin
	Result := PoisonNode;


  if CurrentTokenKind = ptGoto then
    Result.AddChild(ParseSimpleStatement)
  else
  begin
//    InitAhead;
    Result.AddChild(ParseDesignator);

    if CurrentTokenKind in [ptAssign, ptSemicolon, ptElse] then
      Result.AddChild(ParseSimpleStatement)
    else
      Result.AddChild(ParseExpression);
  end;
end;

function TDelphiParser.ParseStatements: TSyntaxNode2;
begin {removed ptIntegerConst jdj-Put back in for labels}
	Result := PoisonNode;

	while CurrentTokenKind in [ptAddressOp, ptAsm, ptBegin, ptCase, ptConst, ptDoubleAddressOp,
			ptFor, ptGoTo, ptIdentifier, ptIf, ptInherited, ptInline, ptIntegerConst,
			ptPointerSymbol, ptRaise, ptOpenParen, ptRepeat, ptSemiColon, ptString,
			ptTry, ptVar, ptWhile, ptWith] do
	begin
		Result.AddChild(ParseStatement);
		Result.AddChild(EatToken(ptSemicolon));
	end;
end;

function TDelphiParser.ParseSimpleStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#SimpleStatement

SimpleStatement
	-> BareInherited								-> INHERITED
	-> ExpressionOrAssignment					-> Expression
														-> Expression ':=' Expression
	-> GotoStatement								-> GOTO LabelId
	-> Block											-> BEGIN [StatementList] END
														-> AssemblerStatement
	-> IfStatement									-> IF Expression THEN [Statement] (Completed)    [ELSE [Statement]]
	-> CaseStatement								-> CASE Expression OF (CaseSelector)+ [ELSE [StatementList]] END
	-> RepeatStatement							-> REPEAT [StatementList] UNTIL Expression
	-> WhileStatement								-> WHILE Expression DO [Statement]
	-> ForStatement								-> FOR Ident ':=' Expression (TO | DOWNTO) Expression DO [Statement]
														-> FOR Ident IN Expression DO [Statement]
	-> WithStatement								-> WITH ExpressionList DO [Statement]
	-> TryStatement								-> TRY (Completed) [StatementList] ...
	-> RaiseStatement								-> RAISE [Expression [AT Expression]]
}
	Result := TSyntaxNode2.Create(ntStatement);

	case CurrentTokenKind of
//	-> BareInherited								-> INHERITED
	ptInherited: Result.AddChild(EatToken(ptInherited));

// ExpressionOrAssignment


//	-> GotoStatement								-> GOTO LabelId
	ptGoto,

//	-> Block											-> BEGIN [StatementList] END
//														-> AssemblerStatement
	ptBegin, ptAsm, {Block}

//	-> IfStatement									-> IF Expression THEN [Statement] (Completed)    [ELSE [Statement]]
	ptIf,

//	-> CaseStatement								-> CASE Expression OF (CaseSelector)+ [ELSE [StatementList]] END
	ptCase,


//	-> RepeatStatement							-> REPEAT [StatementList] UNTIL Expression
	ptRepeat,

//	-> WhileStatement								-> WHILE Expression DO [Statement]
	ptWhile,

//	-> ForStatement								-> FOR Ident ':=' Expression (TO | DOWNTO) Expression DO [Statement]
//														-> FOR Ident IN Expression DO [Statement]
	ptFor,

//	-> WithStatement								-> WITH ExpressionList DO [Statement]
	ptWith,


//	-> TryStatement								-> TRY (Completed) [StatementList] ...
	ptTry,

//	-> RaiseStatement								-> RAISE [Expression [AT Expression]]
	ptRaise: Result := TSyntaxNode2.Create(ntStatement);
	else
		Result := SynError('Expected Simple Statement');
	end;

	case CurrentTokenKind of
	ptInherited,
// ExpressionOrAssignment
	ptGoto,
	ptBegin, ptAsm, {Block}
	ptIf,
	ptCase,
	ptRepeat,
	ptWhile,
	ptFor,
	ptWith,
	ptTry,
	ptRaise: Result := TSyntaxNode2.Create(ntGoto);
	else
		Result := SynError('Expected Simple Statement');
	end;
end;

(*
function TDelphiParser.IsPossibleSimpleStatement: Boolean;
begin
{
SimpleStatement
	-> BareInherited
	-> ExpressionOrAssignment
	-> GotoStatement
	-> Block
	-> IfStatement
	-> CaseStatement
	-> RepeatStatement
	-> WhileStatement
	-> ForStatement
	-> WithStatement
	-> TryStatement
	-> RaiseStatement
}

//	BareInherited
//		-> INHERITED
	if CurrentTokenKind = ptInherited then
		Result := True
//	ExpressionOrAssignment
//		-> Expression
//		-> Expression ':=' Expression
	else if IsPossibleExpression then
		Result := True
//	GotoStatement
//		-> GOTO LabelId
	else if CurrentTokenKind = ptGoto then
		Result := True
//	Block
//		-> BEGIN [StatementList] END
//		-> AssemblerStatement
	else if (CurrentTokenKind = ptBegin) or (CurrentTokenKind = ptAsm) then
		Result := True
//	IfStatement
//		-> IF Expression THEN [Statement]
//				[ELSE [Statement]]
	else if CurrentTokenKind = ptIf then
		Result := True
//	CaseStatement [^]
//		-> CASE Expression OF
//				(CaseSelector)+
//				[ELSE [StatementList]]
//				END
	else if CurrentTokenKind = ptCase then
		Result := True
//	RepeatStatement
//		-> REPEAT [StatementList] UNTIL Expression
	else if CurrentTokenKind = ptRepeat then
		Result := True
//	WhileStatement [^]
//		-> WHILE Expression DO [Statement]
	else if CurrentTokenKind = ptWhile then
		Result := True
//	TryStatement
//		-> TRY
//				[StatementList]
//				( FINALLY [StatementList]
//				| EXCEPT (
//				[StatementList] |
//				(ExceptionItem)* [ELSE [StatementList]]
//				)
//				END
	else if CurrentTokenKind = ptTry then
		Result := True
//	RaiseStatement
//		-> RAISE [Expression [AT Expression]]
	else if CurrentTokenKind = ptRaise then
		Result := True
	else
		Result := False;
end; *)

function TDelphiParser.ParseStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Statement

Statement
	-> LabelId ':' [SimpleStatement]
	-> SimpleStatement

LabelId
	-> <number>
	-> Ident


}
	Result := PoisonNode;

	case CurrentTokenKind of
	ptAsm:	Result := ParseAssemblerStatement;
	ptBegin:	Result := ParseStatements;
	ptCase:	Result := ParseCaseStatement;
	ptConst:	Result := ParseInlineConstSection;
	ptFor:	Result := ParseForStatement;
	ptIf:		Result := ParseIfStatement;
	ptIdentifier:
		begin
			case PeekTokenKind of
			ptColon:	Result := ParseLabeledStatement;
			else
				Result := ParseStatementOrExpression;
			end;
		end;
	ptInherited:	Result := ParseInheritedStatement;
	ptInLine:		Result := ParseInlineStatement;
	ptIntegerConst:
		begin
			case PeekTokenKind of
			ptColon: Result := ParseLabeledStatement;
			else
				begin
					Result.AddChild(SynError('InvalidLabeledStatement'));
				end;
			end;
		end;
	ptRepeat:		Result := ParseRepeatStatement;
	ptRaise:			Result := ParseRaiseStatement;
	ptSemiColon:	Result := ParseEmptyStatement;
	ptTry:			Result := ParseTryStatement;
	ptVar:			Result := ParseInlineVarSection;
	ptWhile:			Result := ParseWhileStatement;
	ptWith:			Result := ParseWithStatement;
	else
		Result := ParseStatementOrExpression;
	end;
end;

(*function TDelphiParser.IsPossibleStatement: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#Statement

Statement
	-> LabelId ':' [SimpleStatement]
	-> SimpleStatement
}
	Result := IsPossibleLabelID
			or IsPossibleSimpleStatement;
end; *)

function TDelphiParser.EatToken(ExpectedTokenKind: TptTokenKind): TSyntaxToken;
var
	t: TSyntaxToken;
	s: string;
begin
	// Consume the expected token and return it as an ntToken node with trivia.
	// If not present, return a synthesized missing-token node.
	t := CurrentToken;
	if t.TokenKind = ExpectedTokenKind then
	begin
		Result := t;
		NextToken; // advance lexer AFTER capturing token+trivia
	end
	else
	begin
		// Create a zero-width (synthesized) token for error recovery, preserving tree shape
		Result := TSyntaxToken.Create(ExpectedTokenKind, CurrentToken.Line, CurrentToken.Column, '');
		Result.IsMissing := True;
		s := Format(SExpected, [TokenName(ExpectedTokenKind), TokenName(CurrentToken.TokenKind)]);
		DoMessage(s, CurrentToken.Line, CurrentToken.Column);
		Log(s);

		// Do not consume unexpected real tokens here.
		// Keeping CurrentToken allows surrounding productions to recover without dropping source tokens.

		if IsDebuggerPresent then
		begin
			OutputDebugString(PChar(s+'   '+NextFewTokens));
//			Windows.DebugBreak;
		end;
	end;
end;

function TDelphiParser.EatToken: TSyntaxToken;
begin
	Result := CurrentToken;
	NextToken;
end;

{======================== Parser helpers ========================}


function TDelphiParser.ParseElseStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntElse);
	Result.AddChild(EatToken(ptElse));
	Result.AddChild(ParseStatement);
end;

function TDelphiParser.ParseEmptyStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntEmptyStatement);
	{	Nothing to do here.
		The semicolon will be removed in ParseStatementList
	}
end;

function TDelphiParser.ParseInheritedStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntInherited);
		Result.AddChild(EatToken(ptInherited));
		if CurrentTokenKind = ptIdentifier then
			Result.AddChild(ParseStatement);
end;

function TDelphiParser.ParseLabeledStatement: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenKind of
	ptIdentifier:
		begin
			Result.AddChild(EatToken(ptIdentifier));
			Result.AddChild(EatToken(ptColon));
			Result.AddChild(ParseStatement);
		end;
	ptIntegerConst:
		begin
			Result.AddChild(EatToken(ptIntegerConst));
			Result.AddChild(EatToken(ptColon));
			Result.AddChild(ParseStatement);
		end;
	else
		Result.AddChild(SynError('InvalidLabeledStatement'));
	end;
end;

function TDelphiParser.ParseStringStatement: TSyntaxNode2;
var
	str: TSyntaxToken;
begin
	Result := TSyntaxNode2.Create(ntType);

	str := EatToken(ptStringLiteral);
	Result.Attributes[anName] := str.ValueText;
	Result.Attributes[anType ] := AttributeValueToStr(avString);

	Result.AddChild(EatToken(ptString));
end;

function TDelphiParser.ParseSetElement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntElement);

		Result.AddChild(ParseExpression);
		if CurrentTokenKind = ptDotDot then
		begin
			Result.AddChild(EatToken(ptDotDot));
			Result.AddChild(ParseExpression);
		end;
end;

procedure TDelphiParser.set_IncludeHandler(IncludeHandler: IUnknown{IIncludeHandler});
begin
//	FLexer.IncludeHandler := IncludeHandler;
end;

function TDelphiParser.ParseSetConstructor: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntSet);
		Result.AddChild(EatToken(ptOpenBracket));
		if CurrentTokenKind <> ptCloseBracket then
		begin
			Result.AddChild(ParseSetElement);
			while CurrentTokenKind = ptComma do
			begin
				Result.AddChild(EatToken(ptComma));
				Result.AddChild(ParseSetElement);
			end;
		end;
		Result.AddChild(EatToken(ptCloseBracket));
end;

function TDelphiParser.ParseNumber: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntLiteral);
	Result.Attributes[anName] := CurrentToken.ValueText;
	Result.Attributes[anType] := AttributeValueToStr(avNumeric);

	case CurrentTokenKind of
	ptFloat: Result.AddChild(EatToken(ptFloat));
	ptIntegerConst: Result.AddChild(EatToken(ptIntegerConst));
	ptIdentifier: Result.AddChild(EatToken(ptIdentifier));
	else
		Result.AddChild(SynError('InvalidNumber'));
	end;
end;

function TDelphiParser.ParseExpressionList: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExpressionList

ExpressionList
	-> (Expression [','])+
}
	Result := TSyntaxNode2.Create(ntExpressions);

		Result.AddChild(ParseExpression);
		if CurrentTokenKind = ptAssign then
		begin
			Result.AddChild(EatToken(ptAssign));
			Result.AddChild(ParseExpression);
		end;
		while CurrentTokenKind = ptComma do
		begin
			Result.AddChild(EatToken(ptComma));
			Result.AddChild(ParseExpression);
			if CurrentTokenKind = ptAssign then
			begin
				Result.AddChild(EatToken(ptAssign));
				Result.AddChild(ParseExpression);
			end;
		end;
end;

function TDelphiParser.ParseDesignator: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(ParseVariableReference);
end;

function TDelphiParser.ParseMultiplicativeOperator: TSyntaxNode2;
begin
	case CurrentTokenKind of
	ptAnd: Result := TSyntaxNode2.Create(ntAnd);
	ptDiv: Result := TSyntaxNode2.Create(ntDiv);
	ptMod: Result := TSyntaxNode2.Create(ntMod);
	ptShl: Result := TSyntaxNode2.Create(ntShl);
	ptShr: Result := TSyntaxNode2.Create(ntShr);
	ptSlash: Result := TSyntaxNode2.Create(ntFDiv);
	ptAsterisk: Result := TSyntaxNode2.Create(ntMul);
	else
		Result := TSyntaxNode2.Create(ntUnknown);
	end;

	case CurrentTokenKind of
	ptAnd:	Result.AddChild(EatToken);
	ptDiv:	Result.AddChild(EatToken);
	ptMod:	Result.AddChild(EatToken);
	ptShl:	Result.AddChild(EatToken);
	ptShr:	Result.AddChild(EatToken);
	ptSlash:	Result.AddChild(EatToken);
	ptAsterisk:	Result.AddChild(EatToken);
	else
		Result.AddChild(SynError('InvalidMultiplicativeOperator'));
	end;
end;

function TDelphiParser.ParseFactor: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Factor

Factor
	-> Atom
	-> UnaryOperator Factor

Atom
	-> Particle
			( '.' ExtendedIdent
			| '[' ParseExpressionList ']'
			| '^'
			| '(' (ParameterExpression [','])* ')'
			)*

Particle
	-> <number>
	-> <stringliteral>
	-> Ident
	-> NIL
	-> ParenthesizedExpression
	-> SetLiteral
	-> STRING
	-> FILE


UnaryOperator
	-> NOT						ptNot: ParseNotOp
	-> '+'						ptPlus: ntAdd
	-> '-'						ptMinus: ParseUnaryMinus
	-> '@'						ptAt: ParseAtExpression
	-> INHERITED

}
	//TODO: Implement ParseFactor correctly.
	Result := PoisonNode;

	case CurrentTokenKind of
	ptAsciiChar, ptStringLiteral: Result.AddChild(ParseCharString);
	ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptInherited, ptPointerSymbol: Result.AddChild(ParseDesignator);
	ptOpenParen:
		begin
			Result.AddChild(ParseRoundOpen);
			Result.AddChild(ParseExpressionList);
			Result.AddChild(ParseRoundClose);
		end;

// Atom -> Particle -> <number>
	ptIntegerConst, ptFloat:	Result.AddChild(ParseNumber);					// ntLiteral anType=atNumeric
	ptString:						Result.AddChild(ParseStringStatement);
	ptNil:							Result.AddChild(ParseNilToken);
	ptMinus:
		begin
			Result.AddChild(ParseUnaryMinus);
			Result.AddChild(ParseFactor);
		end;
	ptNot:
		begin
			Result.AddChild(ParseNotOp);
			Result.AddChild(ParseFactor);
		end;
	ptPlus:
		begin
			Result.AddChild(EatToken(ptPlus));
			Result.AddChild(ParseFactor);
		end;
	ptOpenBracket: Result.AddChild(ParseSetConstructor);
	ptFunction, ptProcedure: Result.AddChild(ParseAnonymousMethod);
	end;

	while CurrentTokenKind = ptOpenBracket do
		Result.AddChild(ParseIndexOp);

	while CurrentTokenKind = ptPointerSymbol do
		Result.AddChild(ParsePointerSymbol);

	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseFactor);

	while CurrentTokenKind = ptDot do
	begin
		Result.AddChild(ParseDotOp);
		Result.AddChild(ParseFactor);
	end;
end;

function TDelphiParser.ParseAddOp: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#AddOp

AddOp
	-> '+'
	-> '-'
	-> OR
	-> XOR
}
	case CurrentTokenKind of
	ptPlus:	Result := TSyntaxNode2.Create(ntAdd);
	ptMinus:	Result := TSyntaxNode2.Create(ntSub);
	ptOr:		Result := TSyntaxNode2.Create(ntOr);
	ptXor:	Result := TSyntaxNode2.Create(ntXor);
	else
		Result := SynError('Unknown ParseAdditiveOperator token kind');
		Exit;
	end;

	Result.AddChild(EatToken);
end;

function TDelphiParser.ParseAddressOp: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAddr);
	Result.AddChild(EatToken(ptAddressOp));
end;

function TDelphiParser.ParseTerm: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Term

Term
	-> Factor (MulOp Factor)*
}
	Result := PoisonNode;

  Result.AddChild(ParseFactor);
  while CurrentTokenKind in [ptAnd, ptDiv, ptMod, ptShl, ptShr, ptSlash, ptAsterisk] do
  begin
    Result.AddChild(ParseMultiplicativeOperator);
    Result.AddChild(ParseFactor);
  end;
end;

function TDelphiParser.ParseRelativeOperator: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#RelOp

RelOp
	Backlinks: Expression
	-> '='
	-> '>'
	-> '<'
	-> '<='
	-> '>='
	-> '<>'
	-> IN
	-> IS
	-> AS
}
	case CurrentTokenKind of
	ptAs:							Result := TSyntaxNode2.Create(ntAs);
	ptEquals:					Result := TSyntaxNode2.Create(ntEqual);
	ptGreaterThan:				Result := TSyntaxNode2.Create(ntGreater);
	ptGreaterThanEquals:		Result := TSyntaxNode2.Create(ntGreaterEqual);
	ptIn:							Result := TSyntaxNode2.Create(ntIn);
	ptIs:							Result := TSyntaxNode2.Create(ntIs);
	ptLessThan:					Result := TSyntaxNode2.Create(ntLower);
	ptLessThanEquals:			Result := TSyntaxNode2.Create(ntLowerEqual);
	ptNotEqual:					Result := TSyntaxNode2.Create(ntNotEqual);
	else
		Result := SynError('InvalidRelativeOperator');
	end;

	Result.AddChild(EatToken);
end;

function TDelphiParser.ParseSimpleExpression: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#SimpleExpression

SimpleExpression
	-> Term (AddOp Term)*

AddOp
	-> '+'
	-> '-'
	-> OR
	-> XOR
}
	Result := TSyntaxNode2.Create(ntExpression);
	Result.AddChild(ParseTerm);

	while CurrentTokenKind in [ptPlus, ptMinus, ptOr, ptXor] do
	begin
		Result.AddChild(ParseAddOp);
		Result.AddChild(ParseTerm);
	end;
end;

function TDelphiParser.ParseExpression: TSyntaxNode2;

const
	RelOps: set of TptTokenKind = [ptEquals, ptGreaterThan, ptLessThan, ptGreaterThanEquals, ptLessThanEquals, ptNotEqual, ptIn, ptIs, ptAs];
var
	expr, right: TSyntaxNode2;
	opTok: TSyntaxToken;

	function MakeRel(L: TSyntaxNode2; Op: TSyntaxToken; R: TSyntaxNode2): TSyntaxNode2;
	var
		n: TSyntaxNode2;
	begin
		n := TSyntaxNode2.Create(ntRelationalExpression);
		n.AddChild(L);
		n.AddChild(Op);  // keep token to preserve trivia
		n.AddChild(R);
		Result := n;
	end;

begin
{
http://dgrok.excastle.com/Grammar.html#Expression

Expression
	-> SimpleExpression (RelOp SimpleExpression)*

Returns: ntExpression

Examples
--------

a
	ntExpression
	└─ ntSimpleExpression  // SE(a)
	   └─ ...              // (whatever SE parses for 'a')

a or b
	ntExpression
	└─ ntRelationalExpression
	   ├─ Left:  ntSimpleExpression  // SE(a)
	   ├─ Op:    OpTok('op')         // the actual operator token (e.g., '=', '<', 'in')
	   └─ Right: ntSimpleExpression  // SE(b)

	a or b or c or d
	ntExpression
	└─ ntRelationalExpression
	   ├─ Left:  ntRelationalExpression
	   │  ├─ Left:  ntRelationalExpression
	   │  │  ├─ Left:  ntSimpleExpression  // SE(a)
	   │  │  ├─ Op:    OpTok('op')
	   │  │  └─ Right: ntSimpleExpression  // SE(b)
	   │  ├─ Op:    OpTok('op')
	   │  └─ Right: ntSimpleExpression     // SE(c)
	   ├─ Op:    OpTok('op')
	   └─ Right: ntSimpleExpression        // SE(d)

}
	Result := TSyntaxNode2.Create(ntExpression);

	// Parse first term at this precedence
	expr := ParseSimpleExpression;

	// Left-associative fold: Simple (op Simple)*
	while CurrentTokenKind in RelOps do
	begin
		opTok := EatToken;                 // take '=', '<', 'IN', 'IS', 'AS', etc.
		right := ParseSimpleExpression;    // parse RHS
		expr  := MakeRel(expr, opTok, right); // ntRelationalExpression(Child: left, op, right)
	end;

	// Always attach the parsed expression subtree so consumed tokens are preserved.
	Result.AddChild(expr);
end;

function TDelphiParser.ParseVarDecl: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VarDecl

VarDecl
	-> IdentList ':' Type
			(PortabilityDirective)*
			[ABSOLUTE Expression | '=' TypedConstant]
			(PortabilityDirective)*
			';'
}

	Result := TSyntaxNode2.Create(ntVariables);

	// IdentList ':' Type
	Result.AddChild(ParseIdentList);
	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseType);

	// (PortabilityDirective)*
	Result.AddChild(ParsePortabilityDirective);		// ntHintDirectives

	case CurrentToken.GenID of
	ptAbsolute: Result.AddChild(ParseVarAbsolute);			// ABSOLUTE t1
	ptEquals:	Result.AddChild(ParseVarEqual);				// = TypedConstant
	end;

	// (PortabilityDirective)*
	Result.AddChild(ParsePortabilityDirective);		// ntHintDiretives
end;

function TDelphiParser.ParseVarAbsolute: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VarDecl

VarDecl
	-> IdentList ':' Type
			(PortabilityDirective)*
			[ABSOLUTE Expression | '=' TypedConstant]
			(PortabilityDirective)*
			';'
}
	Result := TSyntaxNode2.Create(ntAbsolute);
	Result.AddChild(EatTokenEx(ptAbsolute));	//Expect the token's ExID to be ptAbsolute
	Result.AddChild(ParseExpression);
end;

function TDelphiParser.ParseVarEqual: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VarDecl

VarDecl
	-> IdentList ':' Type
			(PortabilityDirective)*
			[ABSOLUTE Expression | '=' TypedConstant]
			(PortabilityDirective)*
			';'
}
	Result := TSyntaxNode2.Create(ntConstant);
	Result.AddChild(EatToken(ptEquals));
   Result.AddChild(ParseConstantValueTyped); //todo: rename to ParseTypedConstant
end;

function TDelphiParser.ParseIdentList: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#IdentList

IdentList
	-> ( Ident [','] )+
}
	Result := TSyntaxNode2.Create(ntIdentifier);

	Result.AddChild(ParseIdent);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseIdent);
	end;
end;

procedure TDelphiParser.DirectiveCalling(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anCallingConvention] := CurrentToken.ValueText;

	case CurrentTokenExID of
	ptCdecl: ParentNode.AddChild(EatToken(ptCdecl));
	ptPascal: ParentNode.AddChild(EatToken(ptPascal));
	ptRegister: ParentNode.AddChild(EatToken(ptRegister));
	ptSafeCall: ParentNode.AddChild(EatToken(ptSafeCall));
	ptStdCall: ParentNode.AddChild(EatToken(ptStdCall));
	else
		ParentNode.AddChild(SynError('InvalidDirectiveCalling'));
	end;
end;

function TDelphiParser.ParseRecordVariant: TSyntaxNode2;
begin
	Result := PoisonNode;


  Result.AddChild(ParseConstantExpression);
  while (CurrentTokenKind = ptComma) do
  begin
	Result.AddChild(EatToken(ptComma));
    Result.AddChild(ParseConstantExpression);
  end;
  Result.AddChild(EatToken(ptColon));
  Result.AddChild(EatToken(ptOpenParen));
  if CurrentTokenKind <> ptCloseParen then
  begin
    Result.AddChild(ParseFieldList);
  end;
  Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParseVariantSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VariantSection

VariantSection
	-> CASE [Ident ':'] QualifiedIdent OF
			(VariantGroup)+
}
	Result := PoisonNode;

  Result.AddChild(EatToken(ptCase));
  Result.AddChild(ParseTagField);
  Result.AddChild(EatToken(ptOf));
  Result.AddChild(ParseRecordVariant);
  while CurrentTokenKind = ptSemiColon do
  begin
    Result.AddChild(EatToken(ptSemicolon));
    case CurrentTokenKind of
      ptEnd, ptCloseParen: Break;
    else
      Result.AddChild(ParseRecordVariant);
    end;
  end;
end;

function TDelphiParser.ParseTagField: TSyntaxNode2;
begin
	Result := PoisonNode;

  Result.AddChild(ParseTagFieldName);
  case CurrentToken.TokenKind of
    ptColon:
      begin
		Result.AddChild(EatToken(ptColon));
        Result.AddChild(ParseTagFieldTypeName);
      end;
  end;
end;

function TDelphiParser.ParseTagFieldName: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseTagFieldTypeName: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(ParseOrdinalType);
end;

function TDelphiParser.ParseFieldDeclaration: TSyntaxNode2;
begin
	Result := PoisonNode;

//	if CurrentTokenKind = ptOpenBracket then
//		Result.AddChild(ParseCustomAttribute);
	Result.AddChild(ParseFieldNameList);
	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseType);
	Result.AddChild(ParsePortabilityDirective);
end;

function TDelphiParser.ParseFieldList: TSyntaxNode2;
begin
	Result := PoisonNode;

	while CurrentTokenKind in [ptIdentifier, ptOpenBracket] do
	begin
		Result.AddChild(ParseFieldDeclaration);
		Result.AddChild(EatToken(ptSemicolon));
	end;
	if CurrentTokenKind = ptCase then
		Result.AddChild(ParseVariantSection);
end;

function TDelphiParser.ParseFieldName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseFieldNameList: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(ParseFieldName);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseFieldName);
	end;
end;

procedure TDelphiParser.RecordType(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptRecord));
	if CurrentTokenKind = ptSemicolon then
		Exit;

	if CurrentTokenExID = ptHelper then
		ParentNode.AddChild(ParseClassHelperType);

	if CurrentTokenKind = ptOpenParen then
	begin
		ParentNode.AddChild(EatToken(ptOpenParen));
		ParentNode.AddChild(ParseAncestorList); // ntAncestorList
		ParentNode.AddChild(EatToken(ptCloseParen));
		if CurrentTokenKind = ptSemicolon then
			Exit;
	end;
	ParentNode.AddChild(ParseClassMemberList);
	ParentNode.AddChild(EatToken(ptEnd));

	ParentNode.AddChild(ParseClassTypeEnd);
	ParentNode.AddChild(ParseRecordAlign);

	MoveMembersToVisibilityNodes(ParentNode);
end;

function TDelphiParser.ParseFileType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#FileType

FileType
	-> FILE
	-> FILE OF QualifiedIdent
}
	Result := PoisonNode;


	Result.AddChild(EatToken(ptFile));
	if CurrentTokenKind = ptOf then
	begin
		Result.AddChild(EatToken(ptOf));
		Result.AddChild(ParseTypeId);
	end;
end;

function TDelphiParser.ParseFinalizationSection: TSyntaxNode2;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntFinalization);
	Result.AddChild(EatToken(ptFinalization));
	Result.AddChild(ParseStatementList);
end;

function TDelphiParser.ParseFinallyBlock: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntFinally);
	Result.AddChild(ParseStatementList);
end;

function TDelphiParser.ParseSetType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#SetType

SetType
	-> SET OF Type
}
	Result := PoisonNode;

	Result.AddChild(EatToken(ptSet));
	Result.AddChild(EatToken(ptOf));
	Result.AddChild(ParseOrdinalType);
end;

function TDelphiParser.ParseArrayType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ArrayType

ArrayType
	-> ARRAY ['[' (ExpressionOrRange [','])+ ']'] OF Type

	Where ExpressionOrRange already exists in DGrok and matches Delphi's bounds shape:
		1..10
		Low(TEnum)..High(TEnum)
		(and other const-expression-ish forms)
}
	Result := TSyntaxNode2.Create(ntType);
	Result[anType] := 'array';

	// ARRAY
	Result.AddChild(EatToken(ptArray));

	// Optional: '[' (ExpressionOrRange [','])+ ']'
	if CurrentTokenKind = ptOpenBracket then
	begin
		Result.AddChild(EatToken(ptOpenBracket));
		Result.AddChild(ParseExpressionOrRange);
		while CurrentTokenKind = ptComma do
		begin
			Result.AddChild(EatToken(ptComma));
			Result.AddChild(ParseExpressionOrRange);
		end;

		Result.AddChild(EatToken(ptCloseBracket));
	end;

	// OF Type
	Result.AddChild(EatToken(ptOf));
	Result.AddChild(ParseType);
end;

function TDelphiParser.ParseEnumeratedType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#EnumeratedType

EnumeratedType
			Backlinks: Type
	-> '(' (EnumeratedTypeElement [','])+ ')'

Returns
-------

ntType anName="atEnum"
}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := AttributeValueToStr(avEnum);

	if FScopedEnums then
		Result.Attributes[anVisibility] := 'scoped';

	Result.AddChild(EatToken(ptOpenParen));
	Result.AddChild(ParseEnumeratedTypeElement);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseEnumeratedTypeElement);
	end;
	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParseSubrangeType: TSyntaxNode2;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := AttributeValueToStr(avSubRange);

	Result.AddChild(ParseConstantExpression);
	Result.AddChild(EatToken(ptDotDot));
	Result.AddChild(ParseConstantExpression);
end;

function TDelphiParser.ParseRealIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;


	case CurrentTokenExID of
	ptReal48:	Result.AddChild(EatToken(ptReal48));
	ptReal:		Result.AddChild(EatToken(ptReal));
	ptSingle:	Result.AddChild(EatToken(ptSingle));
	ptDouble:	Result.AddChild(EatToken(ptDouble));
	ptExtended:	Result.AddChild(EatToken(ptExtended));
	ptCurrency:	Result.AddChild(EatToken(ptCurrency));
	ptComp:		Result.AddChild(EatToken(ptComp));
	else
		Result.AddChild(SynError('InvalidRealIdentifier'));
	end;
end;

function TDelphiParser.ParseRealType: TSyntaxNode2;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntType);


	case CurrentTokenKind of
	ptMinus: Result.AddChild(EatToken(ptMinus));
	ptPlus: Result.AddChild(EatToken(ptPlus));
	end;

	case CurrentTokenKind of
	ptFloat: Result.AddChild(EatToken(ptFloat));
	else
		Result.AddChild(ParseVariableReference);
	end;
end;

procedure TDelphiParser.RearrangeVarSection(const VarSect: TSyntaxNode2);
//var
//	Temp: TSyntaxNode2;
//	VarList, ParseVariable, TypeInfo, ValueInfo: TSyntaxNode2;
begin
(*
	TODO: Figure out what this does and rewrite it.

	for VarList in VarSect.ChildNodes do
	begin
    TypeInfo := VarList.FindNode(ntType);
    ValueInfo := VarList.FindNode(ntValue);
    for ParseVariable in VarList.ChildNodes do
    begin
      if ParseVariable.NodeType <> ntName then
        Continue;
      Temp := FStack.Push(ntVariable);
      try
        Temp.AssignPositionFrom(ParseVariable);
        FStack.AddChild(ParseVariable.Clone);
        if Assigned(TypeInfo) then
          FStack.AddChild(TypeInfo.Clone);
        if Assigned(ValueInfo) then
          FStack.AddChild(ValueInfo.Clone)
        else
        begin
          Temp := VarList.FindNode([ntAbsolute, ntValue, ntExpression, ntIdentifier]);
          if Assigned(Temp) then
            FStack.AddChild(ntAbsolute).AddChild(Temp.Clone);
        end;
      finally
        FStack.Pop;
      end;
    end;
  end;
*)
end;

function TDelphiParser.ParseOrdinalIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;


	case CurrentTokenExID of
	ptBoolean:		Result.AddChild(EatToken(ptBoolean));
	ptByte:			Result.AddChild(EatToken(ptByte));
	ptBytebool:		Result.AddChild(EatToken(ptBytebool));
	ptCardinal:		Result.AddChild(EatToken(ptCardinal));
	ptChar:			Result.AddChild(EatToken(ptChar));
//	ptDWord:			NextToken;
	ptInt64:			Result.AddChild(EatToken(ptInt64));
	ptInteger:		Result.AddChild(EatToken(ptInteger));
	ptLongBool:		Result.AddChild(EatToken(ptLongBool));
	ptLongInt:		Result.AddChild(EatToken(ptLongInt));
	ptLongWord:		Result.AddChild(EatToken(ptLongWord));
	ptPChar:			Result.AddChild(EatToken(ptPChar));
	ptShortInt:		Result.AddChild(EatToken(ptShortInt));
	ptSmallInt:		Result.AddChild(EatToken(ptSmallInt));
	ptWideChar:		Result.AddChild(EatToken(ptWideChar));
	ptWord:			Result.AddChild(EatToken(ptWord));
	ptWordbool:		Result.AddChild(EatToken(ptWordbool));
	else
		Result.AddChild(SynError('InvalidOrdinalIdentifier'));
	end;
end;

function TDelphiParser.ParseOrdinalType: TSyntaxNode2;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntType);


	case CurrentTokenKind of
	ptIdentifier:
		begin
			case PeekTokenKind of
			ptDot:						Result.AddChild(ParseTypeId);
			ptOpenParen, ptDotDot:	Result.AddChild(ParseConstantExpression);
			else
				Result.AddChild(ParseTypeId);
			end;
		end;
	ptOpenParen: Result.AddChild(ParseEnumeratedType);
	ptOpenBracket:
		begin
			Result.AddChild(EatToken(ptOpenBracket));
			Result.AddChild(ParseSubrangeType);
			Result.AddChild(EatToken(ptCloseBracket));
		end;
	else
		Result.AddChild(ParseConstantExpression);
	end;

	if CurrentTokenKind = ptDotDot then
	begin
		Result.AddChild(EatToken(ptDotDot));
		Result.AddChild(ParseConstantExpression);
	end;
end;

function TDelphiParser.ParseVariableReference: TSyntaxNode2;
begin
	{
	}
	Result := PoisonNode;


	case CurrentTokenKind of
	ptOpenParen:
		begin
			Result.AddChild(ParseRoundOpen);
			Result.AddChild(ParseExpression);
			Result.AddChild(ParseRoundClose);
			Result.AddChild(ParseVariableTail);
		end;
	ptOpenBracket:
		begin
			Result.AddChild(ParseSetConstructor);
		end;
	ptAddressOp:
		begin
			Result.AddChild(ParseAddressOp);
			Result.AddChild(ParseVariableReference);
		end;
	ptDoubleAddressOp:
		begin
			Result.AddChild(EatToken(ptDoubleAddressOp));
			Result.AddChild(ParseVariableReference);
		end;
	ptInherited:
		begin
			Result.AddChild(ParseInheritedVariableReference);
		end;
	else
		Result.AddChild(ParseVariable);
	end;
end;

function TDelphiParser.ParseVisibilitySection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VisibilitySection

VisibilitySection
	-> [Visibility]
			(VisibilitySectionContent)*

Returns:
	ntVisibilitySection


Visibility
	-> STRICT PRIVATE
	-> STRICT PROTECTED
	-> PRIVATE
	-> PROTECTED
	-> PUBLIC
	-> PUBLISHED

VisibilitySectionContent
	-> FieldSection
	-> MethodOrProperty
	-> ConstSection
	-> TypeSection

FieldSection
	-> [[CLASS] VAR] (FieldDecl)*

MethodOrProperty
	-> MethodHeading
	-> Property

MethodHeading
	-> [CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR)
			QualifiedIdent
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]		// FUNCTION required, OPERATOR optional
			[';']
			(Directive [';'])*

Property
	-> [CLASS]
			PROPERTY Ident
			['[' (Parameter [';'])+ ']']
			[':' MethodReturnType]
			(PropertyDirective)*
			';'

ConstSection
	-> (CONST|RESOURCESTRING) (ConstantDecl)+

TypeSection
	-> TYPE (TypeDecl)+

Example
=======

	private
		FValue: T;
		FId: TGUID;

	ntPrivate(@anVisibilty="true")
		ptPrivate('ptPrivate')
		ntField
			ntName(@anName="FValue")
				ptIdentifier("FValue")
			ntType(@anName="T")
				ptIdentifier("T")
		ntField
			ntName(@anName="FId")
				ptIdentifier("FId")
			ntType(@anName="TGUID")
				ptIdentifier("TGUID")
}
		// Parse optional visibility and allocate Result exactly once.
		if CurrentTokenExID = ptPrivate then
		begin
			Result := TSyntaxNode2.Create(ntPrivate);
			Result.AddChild(EatTokenEx(ptPrivate));
		end
		else if CurrentTokenExID = ptProtected then
		begin
			Result := TSyntaxNode2.Create(ntProtected);
			Result.AddChild(EatTokenEx(ptProtected));
		end
		else if CurrentTokenExID = ptPublic then
		begin
			Result := TSyntaxNode2.Create(ntPublic);
			Result.AddChild(EatTokenEx(ptPublic));
		end
		else if CurrentTokenExID = ptPublished then
		begin
			Result := TSyntaxNode2.Create(ntPublished);
			Result.AddChild(EatTokenEx(ptPublished));
		end
		else if (CurrenttokenExID = ptStrict) and (PeekTokenExID = ptPrivate) then
		begin
			Result := TSyntaxNode2.Create(ntStrictPrivate);
			Result.AddChild(EatTokenEx(ptStrict));
			Result.AddChild(EatTokenEx(ptPrivate));
		end
		else if (CurrenttokenExID = ptStrict) and (PeekTokenExID = ptProtected) then
		begin
			Result := TSyntaxNode2.Create(ntStrictProtected);
			Result.AddChild(EatTokenEx(ptStrict));
			Result.AddChild(EatTokenEx(ptProtected));
		end
		else
		begin
			Result := TSyntaxNode2.Create(ntUnknown);
			Result.Attributes[anName] := 'ntVisibilitySection';
			// Visibility is optional.
		end;

{
	Read optional VisibilitySectionContents
	--> FieldSection
	--> MethodOrProperty
	--> ConstSection
	--> TypeSection
}
	while IsPossibleVisibilitySectionContent do
	begin
		Result.AddChild(ParseVisibilitySectionContent);
	end;
end;

function TDelphiParser.ParseVisibilitySectionContent: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VisibilitySectionContent

VisibilitySectionContent
	-> FieldSection
	-> MethodOrProperty
	-> ConstSection
	-> TypeSection

ConstSection
	-> (CONST|RESOURCESTRING) (ConstantDecl)+

TypeSection
	-> TYPE (TypeDecl)+

MethodOrProperty
	-> MethodHeading
	-> Property

Property
	-> [CLASS]
			PROPERTY Ident
			['[' (Parameter [';'])+ ']']
			[':' MethodReturnType]
			(PropertyDirective)*
			';'

MethodHeading
	-> [CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR)
			QualifiedIdent
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]		// FUNCTION required, OPERATOR optional
			[';']
			(Directive [';'])*

FieldSection
	-> [[CLASS] VAR] (FieldDecl)*
}
	case CurrentTokenKind of
	ptConst: Result := ParseConstSection;
	ptResourceString: Result := ParseResStringSection;
	ptType: Result := ParseTypeSection;
	ptProperty, ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator: Result := ParseMethodOrProperty;
	ptIdentifier:
		begin
			// Keywords like "private/public/strict" can arrive as ptIdentifier with ExID set.
			// Only plain identifiers start field declarations.
			if CurrentTokenExID = ptUnknown then
				Result := ParseFieldDecl
			else
				Result := PoisonNode;
		end;
	ptClass:
		begin
			{
				class property
				class procedure
				class function
				class constructor
				class destructor
				class operator
				class var (FieldDecl)*
				class (FieldDecl)*
			}
			case PeekTokenKind of
			ptProperty, ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator: Result := ParseMethodOrProperty;
			ptVar: Result := ParseFieldDecl;
			else
				// class procedure|function|constructor|destructor|operator
				Result := ParseMethodOrProperty;
			end;
		end;
	else
		begin
			Result := PoisonNode;
			Result.AddChild(SynError('Expected VisibilitySectionContent'));
		end;
	end;
end;

function TDelphiParser.ParseVariable: TSyntaxNode2;
begin
//	Attention: could also came from proc_call !!
	Result := PoisonNode;

	Result.AddChild(ParseQualifiedIdent);
	Result.AddChild(ParseVariableTail);
end;

function TDelphiParser.ParseVariableTail: TSyntaxNode2;
begin
	Result := PoisonNode;


	case CurrentTokenKind of
	ptOpenParen:
		begin
			Result.AddChild(ParseRoundOpen);
			Result.AddChild(ParseExpressionList);
			Result.AddChild(ParseRoundClose);
		end;
	ptOpenBracket:		Result.AddChild(ParseIndexOp);
	ptPointerSymbol:	Result.AddChild(ParsePointerSymbol);
	ptLessThan:
		begin
			Result.AddChild(EatToken(ptLessThan));
			Result.AddChild(ParseTypeArgs);

			if CurrentTokenKind = ptGreaterThan then
			begin
				Result.AddChild(EatToken(ptGreaterThan));
				case CurrentTokenKind of
				ptAddressOp, ptDoubleAddressOp, ptIdentifier:
					begin
						Result.AddChild(ParseVariableReference);
					end;
				ptDot, ptPointerSymbol, ptOpenParen, ptOpenBracket:
					begin
						Result.AddChild(ParseVariableTail);
					end;
				end;
			end;
		end;
	end;

	case CurrentTokenKind of
	ptOpenParen, ptOpenBracket, ptPointerSymbol:	Result.AddChild(ParseVariableTail);
	ptDot:
		begin
			Result.AddChild(ParseDotOp);
			Result.AddChild(ParseVariable);
		end;
	ptAs:
		begin
			Result.AddChild(ParseAsOp);
			Result.AddChild(ParseSimpleExpression);
		end;
	end;
end;

function TDelphiParser.ParseInterfaceType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#InterfaceType

InterfaceType
	-> (INTERFACE | DISPINTERFACE)
			['(' QualifiedIdent ')']
			['[' Expression ']']
			(MethodOrProperty)*
			END
}
	Result := PoisonNode;

	case CurrentTokenKind of
	ptInterface:
		begin
			Result := TSyntaxNode2.Create(ntType);
			Result.Attributes[anType] := AttributeValueToStr(avInterface);
		end;
	ptDispInterface:
		begin
			Result := TSyntaxNode2.Create(ntType);
			Result.Attributes[anType] := AttributeValueToStr(avDispInterface);
		end;
	end;

   case CurrentTokenKind of
	ptInterface:		Result.AddChild(EatToken(ptInterface));
	ptDispInterface:	Result.AddChild(EatToken(ptDispInterface));
   else
	  Result.AddChild(SynError('InvalidInterfaceType'));
   end;

   case CurrentTokenKind of
	ptEnd: Result.AddChild(EatToken(ptEnd)); { Direct descendant without new members }
   ptOpenParen:
      begin
         Result.AddChild(ParseInterfaceHeritage);
         case CurrentTokenKind of
         ptEnd:
            begin
			   Result.AddChild(EatToken(ptEnd)); { No new members }
            end;
         ptSemiColon: ; { No new members }
         else
            begin
               if CurrentTokenKind = ptOpenBracket then
               begin
                  Result.AddChild(ParseInterfaceGUID);
               end;
               InterfaceMemberList(Result);
               Result.AddChild(EatToken(ptEnd));
            end;
         end;
      end;
   else
      if CurrentTokenKind = ptOpenBracket then
      begin
         Result.AddChild(ParseInterfaceGUID);
      end;
      InterfaceMemberList(Result); { Direct descendant }
      Result.AddChild(EatToken(ptEnd));
   end;
end;

procedure TDelphiParser.InterfaceMemberList(ParentNode: TSyntaxNode2);
begin
	while CurrentTokenKind in [ptOpenBracket, ptFunction, ptProcedure, ptProperty] do
	begin
//		while CurrentTokenKind = ptOpenBracket do
//			ParentNode.AddChild(ParseCustomAttribute);

		ParentNode.AddChild(ParseClassMethodOrProperty);
	end;
end;

function TDelphiParser.ParseClassType: TSyntaxNode2;
var
	startIndex: Integer;
begin
{
http://dgrok.excastle.com/Grammar.html#ClassType

ClassType
	-> CLASS
			[ABSTRACT | SEALED]
			[ '(' (QualifiedIdent [','])+ ')' ]

		The remainder is optional, but only if the base class is specified and lookahead shows that the next token is a semicolon

		(VisibilitySection)*


		END

VisibilitySection
	-> [Visibility]
			(VisibilitySectionContent)*

Visibility
	-> STRICT PRIVATE
	-> STRICT PROTECTED
	-> PRIVATE
	-> PROTECTED
	-> PUBLIC
	-> PUBLISHED


Returns
	ntType(@anType = atClass)

Example
=======

	           /-----from here
	TSpecial = class
		FTime: Integer
	end;

	ntType anType="atClass"
		ntField
			ntName anName="FTime"
			ntType anName="Integer"

}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := AttributeValueToStr(avClass);
	Result.AddChild(EatToken(ptClass));

	// Read optional modifieers
	case CurrentTokenGenID of
	ptAbstract:
		begin
			Result.AddChild(EatToken(ptIdentifier));
			Result.Attributes[anAbstract] := 'true';
		end;
	ptSealed:
		begin
			Result.AddChild(EatToken(ptSealed));
			Result.Attributes[anSealed] := 'true';
		end;
	end;

	// Optional ancestor list i.e. TWidget = class(TShape, IShape, IWidget)
	if CurrentTokenKind = ptOpenParen then
	begin
		//		[ '(' (QualifiedIdent [','])+ ')' ]
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseAncestorList); // ntAncestorList
		Result.AddChild(EatToken(ptCloseParen));
	end;

	// The remainder is optional, but only if the base class is specified
	// and lookahead shows that the next token is a semicolon
	if CurrentTokenKind = ptSemiColon then
		Exit;

	while (CurrentTokenKind <> ptEnd) and (CurrentTokenKind <> ptEof) do
	begin
		// Parse members defensively: malformed class bodies can leave the token index unchanged.
		// If no progress is made, advance one token to avoid infinite loops and keep recovery moving.
		startIndex := FCurrent;

		if IsPossibleVisibilitySection then
			Result.AddChild(ParseVisibilitySection)
		else if IsPossibleVisibilitySectionContent then
			Result.AddChild(ParseVisibilitySectionContent)
		else
			Result.AddChild(SynErrorFmt('Skipping token to recover inside class body: %s', [CurrentToken.Text]));

		if FCurrent = startIndex then
		begin
			Result.AddChild(SynErrorFmt('Skipping token to recover inside class body: %s', [CurrentToken.Text]));
		end;
	end;

	if CurrentTokenKind = ptEnd then
		Result.AddChild(EatToken(ptEnd));

	MoveMembersToVisibilityNodes(Result);
end;

function TDelphiParser.ParseClassHelperType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ClassHelperType

ClassHelperType									Backlinks: Type
	-> CLASS HELPER
			['(' QualifiedIdent ')']
			FOR QualifiedIdent
			(VisibilitySection)*
			END

Visibility												Backlinks: VisibilitySection
	-> STRICT PRIVATE
	-> STRICT PROTECTED
	-> PRIVATE
	-> PROTECTED
	-> PUBLIC
	-> PUBLISHED
}
	Result := TSyntaxNode2.Create(ntHelper);
	Result.AddChild(EatToken(ptHelper));

//	['(' QualifiedIdent ')']
	if CurrentTokenKind = ptOpenParen then
	begin
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseAncestorList); // ntAncestorList
		Result.AddChild(EatToken(ptCloseParen));
	end;

//	FOR QualifiedIdent
	Result.AddChild(EatToken(ptFor));
	Result.AddChild(ParseTypeId);

//	(VisibilitySection)*
	while IsPossibleVisibilitySection do
		Result.AddChild(ParseVisibilitySection);

	Result.AddChild(EatToken(ptEnd));
end;

function TDelphiParser.ParseClassVisibility: TSyntaxNode2;
var
	isStrict: boolean;
begin
	Result := PoisonNode;


	isStrict := (CurrentTokenExID = ptStrict);
	if isStrict then
		Result.AddChild(EatTokenEx(ptStrict));

	while CurrentTokenExID in [ptAutomated, ptPrivate, ptProtected, ptPublic, ptPublished] do
	begin
		// Recovery: if someone writes "private:" or "public," we must still consume tokens,
		// otherwise this loop never advances and the parser hangs.
		if PeekToken(1).DirectiveID in [ptColon, ptComma] then
		begin
			Result.AddChild(EatTokenEx(CurrentTokenExID));
			if CurrentTokenKind in [ptColon, ptComma] then
				Result.AddChild(EatToken);
			Exit;
		end;

		case CurrentTokenExID of
			ptAutomated:
				begin
					Result.AddChild(ParseVisibilityAutomated);
				end;
			ptPrivate:
				begin
					if IsStrict then
						Result.AddChild(ParseVisibilityStrictPrivate)
					else
						Result.AddChild(ParseVisibilityPrivate);
				end;
			ptProtected:
				begin
					if IsStrict then
						Result.AddChild(ParseVisibilityStrictProtected)
					else
						Result.AddChild(ParseVisibilityProtected);
				end;
			ptPublic:
				begin
					Result.AddChild(ParseVisibilityPublic);
				end;
			ptPublished:
				begin
					Result.AddChild(ParseVisibilityPublished);
				end;
		end;
	end;
end;

function TDelphiParser.ParseVisibilityAutomated: TSyntaxNode2;
begin
	Result := PoisonNode;


	Result.AddChild(EatTokenEx(ptAutomated));
end;

function TDelphiParser.ParseVisibilityStrictPrivate: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntStrictPrivate);
	Result.Attributes[anVisibility] := AttributeValueToStr(avTrue);

	Result.AddChild(EatTokenEx(ptPrivate));
end;

function TDelphiParser.ParseVisibilityPrivate: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntPrivate);
	Result.Attributes[anVisibility] :=  AttributeValueToStr(avTrue);
	Result.AddChild(EatTokenEx(ptPrivate));
end;

function TDelphiParser.ParseVisibilityStrictProtected: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntStrictProtected);
	Result.Attributes[anVisibility] :=  AttributeValueToStr(avTrue);

	Result.AddChild(EatTokenEx(ptProtected));
end;

function TDelphiParser.ParseVisibilityProtected: TSyntaxNode2;
//var
//	Temp: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntProtected);
	Result.Attributes[anVisibility] := AttributeValueToStr(avTrue);
	Result.AddChild(EatTokenEx(ptProtected));
end;

function TDelphiParser.ParseVisibilityPublic: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntPublic);
	Result.Attributes[anVisibility] := AttributeValueToStr(avTrue);

	Result.AddChild(EatTokenEx(ptPublic));
end;

function TDelphiParser.ParseVisibilityPublished: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntPublished);
	Result.Attributes[anVisibility] := AttributeValueToStr(avTrue);

	Result.AddChild(EatTokenEx(ptPublished));
end;

function TDelphiParser.ParseClassMemberList: TSyntaxNode2;
begin
{
VisibiltySection
	-> [Visibility] (VisibilitySectionContent)*

Visibility
	-> STRICT PRIVATE
	-> STRICT PROTECTED
	-> PRIVATE
	-> PROTECTED
	-> PUBLIC
	-> PUBLISHED


}
	Result := PoisonNode; // ntClassMemberList

	while (CurrentTokenKind in [ptClass, ptConstructor, ptDestructor, ptFunction,
		ptIdentifier, ptProcedure, ptProperty, ptType, ptOpenBracket, ptVar, ptConst, ptCase]) or (CurrentTokenExID = ptStrict) do
	begin
		Result.AddChild(ParseClassVisibility);

//		if CurrentTokenKind = ptOpenBracket then
//			Result.AddChild(ParseCustomAttribute);

		if (CurrentTokenKind = ptIdentifier) and
				not (CurrentTokenExID in [ptPrivate, ptProtected, ptPublished, ptPublic, ptStrict]) then
		begin
			if PeekTokenKind = ptEquals then
				Result.AddChild(ParseConstantDecl)
			else
			begin
				Result.AddChild(ParseClassField);
				if CurrentTokenKind = ptEquals then
				begin
					Result.AddChild(EatToken(ptEquals));
					Result.AddChild(ParseTypedConstant);
				end;
			end;

			Result.AddChild(EatToken(ptSemicolon));
		end
		else if CurrentTokenKind in [ptClass, ptConstructor, ptDestructor, ptFunction,
				ptProcedure, ptProperty, ptVar, ptConst] then
		begin
			Result.AddChild(ParseClassMethodOrProperty);
		end;
		if CurrentTokenKind = ptType then
			Result.AddChild(ParseTypeSection);
		if CurrentTokenKind = ptCase then
		begin
			Result.AddChild(ParseVariantSection);
		end;
	end;
end;

function TDelphiParser.ParseClassMethodOrProperty: TSyntaxNode2;
var
  CurToken: TptTokenKind;
begin
	Result := PoisonNode;


  if CurrentTokenKind = ptClass then
  begin
    CurToken := PeekTokenKind;
  end
	else
    CurToken := CurrentTokenKind;

  case CurToken of
    ptProperty: Result.AddChild(ParseProperty);
    ptVar, ptThreadVar:
      begin
        if CurrentTokenKind = ptClass then
          Result.AddChild(ParseClassClass);

		Result.AddChild(EatToken);
        while (CurrentTokenKind = ptIdentifier) and (CurrentTokenExID = ptUnknown) do
        begin
          Result.AddChild(ParseClassField);
          Result.AddChild(EatToken(ptSemicolon));
        end;
      end;
    ptConst:
      begin
        if CurrentTokenKind = ptClass then
          Result.AddChild(ParseClassClass);

		Result.AddChild(EatToken);
        while (CurrentTokenKind = ptIdentifier) and (CurrentTokenExID = ptUnknown) do
        begin
          Result.AddChild(ParseConstantDecl);
          Result.AddChild(EatToken(ptSemicolon));
        end;
      end;
  else
    begin
      Result.AddChild(ParseClassMethodHeading);
    end;
  end;
end;

function TDelphiParser.ParseClassProperty: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntProperty);

     if CurrentTokenKind = ptClass then
       Result.AddChild(ParseClassClass);

     Result.AddChild(EatToken(ptProperty));
     PropertyName(Result);
     case CurrentTokenKind of
       ptColon, ptOpenBracket:
         begin
           Result.AddChild(ParsePropertyInterface);
         end;
     end;
     Result.AddChild(ParsePropertySpecifiers);
     case CurrentTokenExID of
       ptDefault:
         begin
           Result.AddChild(ParsePropertyDefault);
           Result.AddChild(EatToken(ptSemicolon));
         end;
     end;
end;

procedure TDelphiParser.PropertyName(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anName] := CurrentToken.ValueText;

	ParentNode.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseClassField: TSyntaxNode2;
//var
//	Fields, Temp: TSyntaxNode2;
//	TypeInfo, ParseTypeArgs: TSyntaxNode2;
//	Field: TSyntaxNodeOrToken;
begin
	Result := PoisonNode;


{
	Fields := TSyntaxNode2.Create(ntFields);
	try
		FStack.Push(Fields);
		try
			if CurrentTokenKind = ptOpenBracket then
				Result.AddChild(ParseCustomAttribute);
			Result.AddChild(ParseFieldNameList);
			Result.AddChild(EatToken(ptColon));
			TypeKind;
			Result.AddChild(ParseHintDirectives);

		finally
			FStack.Pop;
		end;

		TypeInfo := Fields.FindNode(ntType);
		ParseTypeArgs := Fields.FindNode(ntTypeArgs);
		for Field in Fields.ChildNodes do
		begin
			if Field.AsNode.NodeType <> ntName then
				Continue;

			Temp := FStack.Push(ntField);
			try
				Temp.AssignPositionFrom(Field.AsNode);

				FStack.AddChild(Field.AsNode.Clone);
				TypeInfo := TypeInfo.Clone;
				if Assigned(ParseTypeArgs) then
					TypeInfo.AddChild(ParseTypeArgs.Clone);
				FStack.AddChild(TypeInfo);
			finally
				FStack.Pop;
			end;
		end;
	finally
		Fields.Free;
	end;
}
end;

procedure TDelphiParser.ObjectType(ParentNode: TSyntaxNode2);
begin
  ParentNode.AddChild(EatToken(ptObject));
  case CurrentTokenKind of
    ptEnd:
      begin
        ParentNode.AddChild(ParseObjectTypeEnd);
		  ParentNode.AddChild(EatToken(ptEnd)); { Direct descendant without new members }
      end;
    ptOpenParen:
      begin
        ParentNode.AddChild(ParseObjectHeritage);
        case CurrentTokenKind of
          ptEnd:
            begin
              ParentNode.AddChild(EatToken(ptEnd));
              ParentNode.AddChild(ParseObjectTypeEnd);
            end;
          ptSemiColon: ParentNode.AddChild(ParseObjectTypeEnd);
        else
          begin
            ObjectMemberList(ParentNode); { Direct descendant }
            ParentNode.AddChild(EatToken(ptEnd));
            ParentNode.AddChild(ParseObjectTypeEnd);
          end;
        end;
      end;
  else
    begin
      ObjectMemberList(ParentNode); { Direct descendant }
      ParentNode.AddChild(EatToken(ptEnd));
      ParentNode.AddChild(ParseObjectTypeEnd);
    end;
  end;
end;

function TDelphiParser.ParseObjectHeritage: TSyntaxNode2;
begin
	Result := PoisonNode;


  Result.AddChild(EatToken(ptOpenParen));
  Result.AddChild(ParseAncestorList);
  Result.AddChild(EatToken(ptCloseParen));
end;

procedure TDelphiParser.ObjectMemberList(ParentNode: TSyntaxNode2);
begin
	{jdj added ptProperty-call to ObjectProperty 02/07/2001}
  ParentNode.AddChild(ParseObjectVisibility);
  while CurrentTokenKind in [ptConstructor, ptDestructor, ptFunction, ptIdentifier,
    ptProcedure, ptProperty] do
  begin
    while CurrentTokenKind = ptIdentifier do
    begin
      ParentNode.AddChild(ParseObjectField);
      ParentNode.AddChild(EatToken(ptSemicolon));
      ParentNode.AddChild(ParseObjectVisibility);
    end;
    while CurrentTokenKind in [ptConstructor, ptDestructor, ptFunction, ptProcedure, ptProperty] do
    begin
      case CurrentTokenKind of
        ptConstructor, ptDestructor, ptFunction, ptProcedure: ObjectMethodHeading(ParentNode);
        ptProperty: ParentNode.AddChild(ParseProperty);
      end;
    end;
    ParentNode.AddChild(ParseObjectVisibility);
  end;
end;

function TDelphiParser.ParseObjectVisibility: TSyntaxNode2;
begin
	Result := PoisonNode;


  while CurrentTokenExID in [ptPrivate, ptProtected, ptPublic] do
  begin
    case PeekTokenKind of
      ptColon, ptComma: ;
    else
      case CurrentTokenExID of
        ptPrivate:
          begin
            Result.AddChild(ParseVisibilityPrivate);
          end;
        ptProtected:
          begin
            Result.AddChild(ParseVisibilityProtected);
          end;
        ptPublic:
          begin
            Result.AddChild(ParseVisibilityPublic);
          end;
      end;
    end;
  end;
end;

function TDelphiParser.ParseObjectField: TSyntaxNode2;
begin
	Result := PoisonNode;


  Result.AddChild(ParseIdentifierList);
  Result.AddChild(EatToken(ptColon));
  Result.AddChild(ParseType);
  Result.AddChild(ParsePortabilityDirective);
end;

function TDelphiParser.ParseClassOfType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ClassOfType

ClassOfType
	-> CLASS OF QualifiedIdent
}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := AttributeValueToStr(avClassOf);

	Result.AddChild(EatToken(ptClass));
	Result.AddChild(EatToken(ptOf));
	Result.AddChild(ParseQualifiedIdent);
end;

function TDelphiParser.ParseVariantIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;


  case CurrentTokenExID of
    ptOleVariant:
      begin
		  Result.AddChild(EatToken(ptOleVariant));
      end;
    ptVariant:
      begin
		  Result.AddChild(EatToken(ptVariant));
      end;
  else
    begin
		  Result.AddChild(SynError('InvalidVariantIdentifier'));
    end;
  end;
end;

function TDelphiParser.ParseProceduralType: TSyntaxNode2;
var
  TheTokenID: TptTokenKind;
begin
	{
	}
   Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := CurrentToken.ValueText;
	case CurrentTokenKind of
	ptFunction:
		begin
			Result.AddChild(EatToken(ptFunction));
			if CurrentTokenKind = ptOpenParen then
				Result.AddChild(ParseFormalParameterList);

			Result.AddChild(EatToken(ptColon));
			Result.AddChild(ParseReturnType);
		end;
	ptProcedure:
		begin
			Result.AddChild(EatToken(ptProcedure));
			if CurrentTokenKind = ptOpenParen then
				Result.AddChild(ParseFormalParameterList);
		end;
	else
		   Result.AddChild(SynError('InvalidProceduralType'));
	end;

	if CurrentTokenKind = ptOf then
		Result.AddChild(ParseProceduralDirectiveOf);

	case CurrentTokenKind of
	ptSemiColon: TheTokenID := PeekToken(1).DirectiveID;
	else
		TheTokenID := CurrentTokenExID;
	end;

	// DR 2001-11-14 no checking for deprecated etc. since it's captured by the typedecl
	while TheTokenID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
			ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
			ptReintroduce, ptSafeCall, ptStdCall, ptVirtual, ptStatic, ptInline, ptVarargs] do
	begin
		if CurrentTokenKind = ptSemiColon then
			Result.AddChild(EatToken(ptSemicolon));
		ProceduralDirective(Result);
		case CurrentTokenKind of
		ptSemiColon: TheTokenID := PeekToken(1).DirectiveID;
		else
			TheTokenID := CurrentTokenExID;
		end;
	end;

	if CurrentTokenKind = ptOf then
		Result.AddChild(ParseProceduralDirectiveOf);
end;

function TDelphiParser.ParseStringConst: TSyntaxNode2;
//var
//	StrConst: TSyntaxNode2;
//	Literal: TSyntaxNodeOrToken;
//	Node: TSyntaxNode2;
//	Str: string;
begin
	Result := PoisonNode;


{
	StrConst := TSyntaxNode2.Create(ntUnknown);
	try
		FStack.Push(StrConst);
		try
			Result.AddChild(ParseStringConstSimple);
			while CurrentTokenKind in [ptStringConst, ptAsciiChar] do
				Result.AddChild(ParseStringConstSimple);
		finally
			FStack.Pop;
		end;

		Str := '';
		for Literal in StrConst.ChildNodes do
			Str := Str + Literal.AsNode.Value;
	finally
		StrConst.Free;
	end;

//	DoHandleString(Str);
	Node := TSyntaxNode2.Create(ntLiteral);
	Node.FValue := Str;
	Node.Attributes[anType] := AttributeValue[atString];
	Result.AddChild(Node);
}
end;

function TDelphiParser.ParseStringConstSimple: TSyntaxNode2;
begin
  //TODO support ptAsciiChar
	Result := TSyntaxNode2.Create(ntLiteral);
	Result[anName] := CurrentToken.ValueText;

	Result.AddChild(EatToken);
end;

function TDelphiParser.ParseStringIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;

  case CurrentTokenExID of
    ptAnsiString:
      begin
		  Result.AddChild(EatToken(ptAnsiString));
      end;
    ptShortString:
      begin
		  Result.AddChild(EatToken(ptShortString));
      end;
    ptWideString:
      begin
		  Result.AddChild(EatToken(ptWideString));
      end;
  else
    begin
		  Result.AddChild(SynError('InvalidStringIdentifier'));
    end;
  end;
end;

function TDelphiParser.ParseStringTypeInternal: TSyntaxNode2;
begin
	Result := PoisonNode;

  case CurrentTokenKind of
    ptString:
      begin
		  Result.AddChild(EatToken(ptString));
        if CurrentTokenKind = ptOpenBracket then
        begin
			Result.AddChild(EatToken(ptOpenBracket));
          Result.AddChild(ParseConstantExpression);
          Result.AddChild(EatToken(ptCloseBracket));
        end;
      end;
  else
    begin
      Result.AddChild(ParseVariableReference);
    end;
  end;
end;

function TDelphiParser.ParsePointerSymbol: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntDeref);
		Result.AddChild(EatToken(ptPointerSymbol));
end;

function TDelphiParser.ParsePointerType: TSyntaxNode2;
begin
{
Returns
	ntType anType=atPointer

http://dgrok.excastle.com/Grammar.html#PointerType

PointerType
	-> '^' Type
}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := AttributeValueToStr(avPointer);
	Result.AddChild(EatToken(ptPointerSymbol));
	Result.AddChild(ParseType); // was ParseTypeId in the old code
end;

function TDelphiParser.ParseStringType: TSyntaxNode2;
begin
{
Returns:
	ntType anType="string"

http://dgrok.excastle.com/Grammar.html#StringType

StringType
	-> STRING
	-> STRING '[' Expression ']'
}
	Result := TSyntaxNode2.Create(ntType);
//	Result.Attributes[anType] :=
	Result.AddChild(EatToken(ptString));


	if (PeekTokenKind = ptOpenBracket) then
	begin
		Result.AddChild(ParseExpression);
		Result.AddChild(EatToken(ptCloseBracket));
	end;
end;

function TDelphiParser.PoisonNode(NodeType: TSyntaxNodeType=ntUnknown): TSyntaxNode2;
const
	SyncTokens: set of TptTokenKind = [ptSemicolon];
begin
{
We are creating a poison node; which means the node isn't implmented yet.

But can we think of it instead as a syntax error, and try to recover?
Can we just eat tokens until we get to an `end`? Nearly every grammer uses an end.

For example
-----------

Lets say we are parsing a property:

Property
	-> [CLASS]
			PROPERTY Ident
			['[' (Parameter [';'])+ ']']
			[':' MethodReturnType]
			(PropertyDirective)*
			';'

with the example:

	property : string getName write setName;		//E2029 Identifier expected but ':' found

Lets say we've read the ptProperty reserved word token.
Next we are expecting an Ident. But : is not a valid identifier.

So we know we're broken. But lets eat up unil the semicolon.
And for many productions, they do end with semi-colon.

So keep adding trivia until we've consumed a semi-colon (or gotten and eof)

We are being called in a loop looking for Methods or Properties. If we say
we're a property, and even though we're jumbled inside, we got to our end; the semicolon.

This where you can now call PoisonNode with a NodeType and a set of sync points [ptSemicolon, ptEnd]

}
	Result := TSyntaxNode2.Create(NodeType);
	Result.Attributes[anMissing] := AttributeValueToStr(avTrue);
	Result[anName] := 'Not implemented yet';

	// keep eating until we get to our semicolon (or eof)
	while (not (CurrentTokenKind in SyncTokens)) do
	begin
		// Eat the trailing semicolon. Don't eat the Eof. Let that bubble up
		if CurrentTokenKind = ptEOF then
			Break;

		Result.AddChild(EatToken);				// CurrentTokenKind
	end;
end;

function TDelphiParser.ParseStructuredType: TSyntaxNode2;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := CurrentToken.ValueText;

	case CurrentTokenKind of
	ptArray: 	Result.AddChild(ParseArrayType);
	ptSet:		Result.AddChild(ParseSetType);
	ptFile:		Result.AddChild(ParseFileType);
	ptRecord:	RecordType(Result);
	ptObject:	ObjectType(Result);
	else
		Result.AddChild(SynError('InvalidStructuredType'));
	end;
end;

function TDelphiParser.ParseSimpleType: TSyntaxNode2;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := CurrentToken.ValueText;

	case CurrentTokenKind of
	ptMinus: Result.AddChild(EatToken(ptMinus));
	ptPlus: Result.AddChild(EatToken(ptPlus));
	end;

	case CurrentToken.TokenKind of
	ptAsciiChar, ptIntegerConst: Result.AddChild(ParseOrdinalType);
	ptFloat: Result.AddChild(ParseRealType);
	ptIdentifier:
		begin
			if PeekTokenKind = ptDotDot then
				Result.AddChild(ParseSubrangeType)
			else
				Result.AddChild(ParseTypeId);
		end;
	else
		Result.AddChild(ParseVariableReference);
	end;
end;

function TDelphiParser.ParseRecordAlign: TSyntaxNode2;
begin
{
	If the current token is for the ALIGN directive, then return that token
}
	Result := PoisonNode;

  if CurrentTokenExID = ptAlign then
  begin
	  Result.AddChild(EatToken(ptAlign));
    RecordAlignValue(Result);
  end;
end;

procedure TDelphiParser.RecordAlignValue(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anAlign] := CurrentToken.ValueText;
	ParentNode.AddChild(EatToken(ptIntegerConst));
end;

function TDelphiParser.ParseRecordFieldConstant: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntField);
	Result[anName] := CurrentToken.ValueText;
	Result.Attributes[anType] := AttributeValueToStr(avName);

	Result.AddChild(EatToken(ptIdentifier));
	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseTypedConstant);
end;

function TDelphiParser.ParseRecordConstraint: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntRecordConstraint);
	Result.AddChild(EatToken(ptRecord));
end;

function TDelphiParser.ParseArrayConstant: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExpressions);

	Result.AddChild(EatToken(ptOpenParen));

	Result.AddChild(ParseTypedConstant);
	if CurrentTokenKind = ptDotDot then
	begin
		Result.AddChild(EatToken(ptDotDot));
		Result.AddChild(ParseTypedConstant);
	end;

	while (CurrentTokenKind = ptComma) do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseTypedConstant);
		if CurrentTokenKind = ptDotDot then
		begin
			Result.AddChild(EatToken(ptDotDot));
			Result.AddChild(ParseTypedConstant);
		end;
	end;

	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParseDotOp: TSyntaxNode2;
begin
   Result := TSyntaxNode2.Create(ntDot);
   Result.AddChild(EatToken(ptDot));
end;

function TDelphiParser.ParseTypeDeclaration: TSyntaxNode2;

var
	typeParams: TSyntaxNode2;
	missingEquals: TSyntaxToken;

	function IsPortabilityDirectiveToken(k: TptTokenKind): Boolean;
	begin
		Result := k in [ptPlatform, ptDeprecated, ptLibrary, ptExperimental];
	end;
begin
{
http://dgrok.excastle.com/Grammar.html#TypeDecl

<TypeDecl>
	→ <Ident> '=' [TYPE] <Type> (PortabilityDirective)* ';'
	→ <Ident> '=' CLASS ';'
	→ <Ident> '=' DISPINTERFACE ';'
	→ <Ident> '=' INTERFACE ';'

Example
=======

TSpecial<T, U: class, constructor; V: IComparable<T>>= class(TShape, ILogger)
private
	FValue: Integer;
public
	constructor Create(Value: Integer);
	procedure DemoMethod;
	property Value: Integer read FValue write FValue;
end;

ntTypeDecl anName="TSpecial"
	ntType
	ntType anType="atClass"
		ntAncestorList
			ntQualifiedIdentifier anName="TShape"
			ntQualifiedIdentifier anName="ILogger"
		ntUnknown('Not implemented yet')
		ntUnknown('Not implemented yet')

Example 2
=========

	TSpecial<T, U: class, constructor; V: IComparable<T>>= class
	^----currentToken

ntTypeDecl(@anName="TSpecial")
	ntType()
	ntType(@anType="atClass"
		ntAncestorList()
			ntQualifiedIdentifier(@anName="TShape")
			ntQualifiedIdentifier(@anName="ILogger")
		ntUnknown('Not implemented yet')
		ntUnknown('Not implemented yet')

}
	Result := TSyntaxNode2.Create(ntTypeDecl);					// TSpecial
	Result.Attributes[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));

	// Parse <T> as ntTypeParams
	if CurrentTokenKind = ptLessThan then
	begin
		typeParams := ParseTypeParams;
		Result.AddChild(typeParams);
	end;

	// ParseTypeParam might have eaten a ">=" for us, rather than the usual ">".
	// Which means we require an "=" here, but no "=" token to eat.
	// We need some way to know...that the previous token was a ptGreaterThanEquals
	if PeekToken(-1).TokenKind = ptGreaterThanEquals then
	begin
		// TODO: Add a missing equals token
		missingEquals := TSyntaxToken.Create(ptEquals, -1, -1, '=');
		missingEquals.IsMissing := True;
		Result.AddChild(missingEquals);
	end
	else
		Result.AddChild(EatToken(ptEquals));

	// Restricted forward forms: "= class ;" | "= interface ;" | "= dispinterface ;"
	case CurrentTokenKind of
	ptClass:
		if PeekTokenKind = ptSemicolon then
		begin
			Result.AddChild(EatToken);         // 'class'
			Result.AddChild(EatToken(ptSemicolon));
			Exit;
		end;
	ptInterface:
		if PeekTokenKind = ptSemicolon then
		begin
			Result.AddChild(EatToken);            // 'interface'
			Result.AddChild(EatToken(ptSemicolon));
			Exit;
		end;
	ptDispInterface:
		if PeekTokenKind = ptSemicolon then
		begin
			Result.AddChild(EatToken);            // 'dispinterface'
			Result.AddChild(EatToken(ptSemicolon));
			Exit;
		end;
	end;


{
	[TYPE] <Type> (PortabilityDirective)* ';'
}
	// [TYPE]
	if CurrentTokenKind = ptType then
	begin
		Result.AddChild(EatToken(ptType));
		Result.Attributes[anDistinct] := 'true'	// todo: here to store the type tag?
	end;

	// Full <Type> branch (covers pointer/array/set/record/class/interface/etc.)
	Result.AddChild(ParseType);

	// PortabilityDirective* — only after the full <Type> branch
	while IsPortabilityDirectiveToken(CurrentTokenKind) do
		Result.AddChild(ParsePortabilityDirective);

	Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParseType: TSyntaxNode2;
var
	packedToken: TSyntaxToken;
begin
{
http://dgrok.excastle.com/Grammar.html#Type

Type
	-> EnumeratedType					'(' (EnumeratedTypeElement [','])+ ')'
	-> ExpressionOrRange				SimpleExpression ['..' SimpleExpression]
	-> ParseArrayType					ARRAY ['[' (Type [','])+ ']'] OF Type
	-> ParseSetType					Term (AddOp Term)*
	-> ParseFileType					FILE
	-> RecordHelperType				RECORD HELPER FOR QualifiedIdent
	-> RecordType						RECORD
	-> PointerType						'^' Type
	-> ParseStringType				STRING
	-> ProcedureType					(PROCEDURE | FUNCTION)
	-> ClassHelperType				CLASS HELPER
	-> ClassOfType						CLASS OF QualifiedIdent
	-> ClassType						CLASS
	-> InterfaceType					(INTERFACE | DISPINTERFACE)
	-> PackedType						PACKED Type

Note: Delphi assumes that a Type starting with '(' is an enum, not an expression.
}
	case CurrentTokenKind of
	ptOpenParen: Result := ParseEnumeratedType;		// ntType
	ptAsciiChar, ptFloat, ptIntegerConst, ptMinus, ptNil, ptPlus, ptStringLiteral, ptConst: Result := ParseSimpleType;
	ptOpenBracket: Result := ParseSubrangeType;
	ptArray, ptFile, ptPacked, ptRecord, ptSet:
		begin
			packedToken := nil;
			if (CurrentTokenKind = ptPacked) then
				packedToken := EatToken(ptPacked);

			Result := ParseStructuredType;
			if packedToken <> nil then
				Result.AddChild(packedToken);
		end;
	ptFunction, ptProcedure: Result := ParseProceduralType;
	ptIdentifier:
		begin
			if PeekTokenKind = ptDotDot then
				Result := ParseSubrangeType
			else
				Result := ParseTypeId;
		end;
	ptPointerSymbol: Result := ParsePointerType;          // ^Type
	ptString: Result := ParseStringType;
	ptClass:
		if PeekTokenKind = ptOf then
			Result := ParseClassOfType
		else
			Result := ParseClassType;
	else
		// E2029 %s expected but %s found
		Result := SynErrorFmt(SE2029, ['Type', CurrentToken.Text]);
	end;
end;

function TDelphiParser.ParseTypeArgs: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntTypeArgs);

	  Result.AddChild(ParseType);
	  while CurrentTokenKind = ptComma do
	  begin
	    Result.AddChild(EatToken(ptComma));
	    Result.AddChild(ParseType);
	  end;
end;

function TDelphiParser.ParseTypedConstant: TSyntaxNode2;
var
	s: string;
	tok: TSyntaxToken;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntExpression);
	s := '';

	// Keep this fast and robust for declaration parsing: consume constant value
	// tokens until a declaration boundary (directive/semicolon/EOF).
	while not (CurrentTokenKind in [ptSemicolon, ptSemiColon, ptPlatform, ptLibrary, ptDeprecated, ptExperimental, ptEof]) do
	begin
		tok := EatToken;
		Result.AddChild(tok);
		s := s + tok.ValueText;
	end;

	Result[anValueText] := s;
end;

function TDelphiParser.ParseTypeId: TSyntaxNode2;
var
	typeName: string;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntType);

	if CurrentTokenKind <> ptIdentifier then
	begin
		Result.AddChild(SynErrorFmt(SE2029, ['Identifier', CurrentToken.Text]));
		Exit;
	end;

	typeName := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));

	while CurrentTokenKind = ptDot do
	begin
		Result.AddChild(EatToken(ptDot));
		if CurrentTokenKind = ptIdentifier then
		begin
			typeName := typeName + '.' + CurrentToken.ValueText;
			Result.AddChild(EatToken(ptIdentifier));
		end
		else
		begin
			Result.AddChild(SynErrorFmt(SE2029, ['Identifier', CurrentToken.Text]));
			Break;
		end;
	end;

	Result.Attributes[anName] := typeName;


{
	TODO: Convert this to the node and token are first class citizens, nodes are returned model.


	TypeNode := FStack.Push(ntType);
	try
		Result.AddChild(ParseTypeSimple);

		while CurrentTokenKind = ptDot do
		begin
			Result.AddChild(EatToken(ptDot));
			Result.AddChild(ParseTypeSimple);
		end;

		if CurrentTokenKind = ptOpenParen then
		begin
			Result.AddChild(EatToken(ptOpenParen));
			SimpleExpression;
			Result.AddChild(EatToken(ptCloseParen));
		end;
		InnerTypeName := '';
		InnerTypeNode := TypeNode.FindNode(ntType);
		if Assigned(InnerTypeNode) then
		begin
			InnerTypeName := InnerTypeNode.Attributes[anName];
			for SubNode in InnerTypeNode.ChildNodes do
				TypeNode.AddChild(SubNode.AsNode.Clone);

			TypeNode.DeleteChild(InnerTypeNode);
		end;

		TypeName := '';
		for i := TypeNode.ChildNodes.Count-1 downto 0 do
		begin
			SubNode := TypeNode.ChildNodes[i];
			if SubNode.AsNode.NodeType = ntType then
			begin
				if TypeName <> '' then
					TypeName := '.' + TypeName;

				TypeName := SubNode.AsNode.Attributes[anName] + TypeName;
				TypeNode.DeleteChild(SubNode.AsNode);
			end;
		end;

		if TypeName <> '' then
			TypeName := '.' + TypeName;
		TypeName := InnerTypeName + TypeName;

//		DoHandleString(TypeName);
		TypeNode.Attributes[anName] := TypeName;
	finally
		FStack.Pop;
	end;
}
end;

function TDelphiParser.ParseConstantExpression: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExpression);
	Result.AddChild(ParseSimpleExpression);
end;

function TDelphiParser.ParseResourceDeclaration: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntResourceString);

	Result.AddChild(ParseConstantName);
	Result.AddChild(EatToken(ptEquals));

	Result.AddChild(ParseResourceValue);

	Result.AddChild(ParsePortabilityDirective);
end;

function TDelphiParser.ParseResourceValue: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntValue);
	  Result.AddChild(ParseCharString);
	  while CurrentTokenKind = ptPlus do
	  begin
	    Result.AddChild(EatToken(ptPlus));
	    Result.AddChild(ParseCharString);
	  end;
end;

function TDelphiParser.ParseConstantDecl: TSyntaxNode2;
var
	identNode: TSyntaxNode2;
	valueNode: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ConstantDecl

ConstantDecl
	-> Ident
			[':' Type]
			'=' TypedConstant
			(PortabilityDirective)*
			';'

Ident
	-> <identifier>
	-> <semikeyword>
	-> '&' <identifier>
	-> '&' <semikeyword>
	-> '&' <keyword>

Example
-------

	const
		LEN_FirstName = 50;
		MAX_BUFFER = 1024*16; //16 KB
		clMainInstruction: TColor = $00993300;

Parsed:

;	const
	ntConstants
	├─ #ptConstKeyword("const")

;	LEN_FirstName = 50;
	├─ ntConstant(@anName="LEN_FirstName", @anValueText="50")
	│	├─ #ptIdentifier("LEN_FirstName")
	│	├─ #ptEquals("=")
	│	├─ ntExpression(@anValueText="50")
	│	│	└─ #ptIntegerLiteral("50")
	│	└─ #ptSemicolon(";")

;	MAX_BUFFER = 1024*16; //16 KB
	├─ ntConstant(@anName="MAX_BUFFER", @anValueText="1024*16")
	│	├─ #ptIdentifier("MAX_BUFFER")
	│	├─ #ptEquals("=")
	│	├─ ntExpression(@anValueText="1024*16")
	│	│	├─ #ptIntegerLiteral("1024")
	│	│	├─ #ptAsterisk("*")
	│	│	└─ #ptIntegerLiteral("16")
	│	├─ #ptSemicolon(";")
	│	└─ #ptLineComment("//16 KB")

;	clMainInstruction: TColor = $00993300;
	└─ ntConstant(@anName="clMainInstruction", @anType="TColor", @anValueText="$00993300")
		├─ #ptIdentifier("clMainInstruction")
		├─ #ptColon(":")
		├─ ntType(@anType="TColor")
		│  └─ #ptIdentifier("TColor")
		├─ #ptEquals("=")
		├─ ntExpression(@anValueText="$00993300")
		│  └─ #ptHexIntegerLiteral("$00993300")
		└─ #ptSemicolon(";")
}
	Result := TSyntaxNode2.Create(ntConstant);

//	-> Ident
	identNode := ParseIdent;
	Result.AddChild(identNode);
	Result.Attributes[anName] := identNode.Value;

//	-> [':' Type]
	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseType);
	end;

//	'=' TypedConstant
	Result.AddChild(EatToken(ptEquals));
	valueNode := ParseTypedConstant;
	Result.AddChild(valueNode);
	Result.Attributes[anValueText] := valueNode.Attributes[anValueText];

//	(PortabilityDirective)*
	// A constant declaration can optionally end with things like `deprecated;`
	while IsPossiblePortabilityDirective do
	begin
		Result.AddChild(ParsePortabilityDirective);
	end;

	Result.AddChild(EatToken(ptSemiColon));
end;

function TDelphiParser.ParseConstantColon: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseConstantType);
	Result.AddChild(EatToken(ptEquals));
	Result.AddChild(ParseConstantValueTyped);
end;

function TDelphiParser.ParseConstantEqual: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptEquals));
	Result.AddChild(ParseConstantValue);
end;

function TDelphiParser.ParseConstantValue: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntValue);
	Result.AddChild(ParseExpression);
end;

function TDelphiParser.ParseConstantValueTyped: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntValue);
	Result.AddChild(ParseTypedConstant);
end;

function TDelphiParser.ParseConstantName: TSyntaxNode2;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseConstantType: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(ParseType);
end;

function TDelphiParser.ParseLabelId: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#LabelId

LabelId
	-> <number>
	-> Ident
}
	Result := TSyntaxNode2.Create(ntLabel);
	Result[anName] := CurrentToken.ValueText;

	case CurrentTokenKind of
	ptIntegerConst:	Result.AddChild(EatToken);
	ptIdentifier:		Result.AddChild(EatToken);
	else
		Result.AddChild(SynError('InvalidLabelId'));
	end;
end;

function TDelphiParser.ParseProcedureDeclarationSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodHeading

MethodHeading
	-> [CLASS]
			(PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR)
			QualifiedIdent
			(
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]
			(Directive)*
			| '=' Ident

				[';']
}
	Result := TSyntaxNode2.Create(ntMethod);

   if CurrentTokenKind = ptClass then
   begin
      ClassMethod(Result);
   end;

   case CurrentTokenKind of
   ptConstructor:	Result.AddChild(ParseProcedureProcedureName);
   ptDestructor:	Result.AddChild(ParseProcedureProcedureName);
   ptProcedure:	Result.AddChild(ParseProcedureProcedureName);
   ptFunction:		Result.AddChild(ParseFunctionMethodDeclaration);
   ptIdentifier:
      begin
         if CurrentTokenExID = ptOperator then
         begin
            Result.AddChild(ParseFunctionMethodDeclaration);
         end
         else
			   Result.AddChild(SynError('InvalidProcedureDeclarationSection'));
      end;
   else
	  Result.AddChild(SynError('InvalidProcedureDeclarationSection'));
   end;
end;

function TDelphiParser.ParseLabelDeclarationSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#LabelDeclSection

LabelDeclSection [^]
	-> LABEL (ParseLabelId [','])+ ';'
}
	Result := TSyntaxNode2.Create(ntLabel);
	Result.AddChild(EatToken(ptLabel));

	// (ParseLabelId [','])+
	while IsPossibleLabelID do
	begin
		Result.AddChild(ParseLabelId);
		if CurrentTokenKind = ptComma then
			Result.AddChild(EatToken(ptComma));
	end;

	Result.AddChild(EatToken(ptSemicolon));
end;

procedure TDelphiParser.ProceduralDirective(ParentNode: TSyntaxNode2);
begin

	case CurrentToken.GenID of
	ptAbstract:						DirectiveBinding(ParentNode);
	ptCdecl, ptPascal, ptRegister, ptSafeCall, ptStdCall: DirectiveCalling(ParentNode);
	ptExport, ptFar, ptNear:	ParentNode.AddChild(ParseDirective16Bit);
	ptExternal:						ParentNode.AddChild(ParseExternalDirective);
	ptDynamic, ptMessage, ptOverload, ptOverride, ptReintroduce, ptVirtual: DirectiveBinding(ParentNode);
	ptAssembler:					ParentNode.AddChild(EatToken(ptAssembler));
	ptStatic:						ParentNode.AddChild(EatToken(ptStatic));
	ptInline:						DirectiveInline(ParentNode);
	ptDeprecated:					ParentNode.AddChild(ParseDirectiveDeprecated);
	ptLibrary:						ParentNode.AddChild(ParseDirectiveLibrary);
	ptPlatform:						ParentNode.AddChild(ParseDirectivePlatform);
	ptLocal:							ParentNode.AddChild(ParseDirectiveLocal);
	ptVarargs:						ParentNode.AddChild(ParseDirectiveVarargs);
	ptFinal:							ParentNode.AddChild(EatToken(ptFinal));
	ptExperimental:				ParentNode.AddChild(EatToken(ptExperimental));
	ptDelayed:						ParentNode.AddChild(EatToken(ptDelayed));
	else
		ParentNode.AddChild(SynError('InvalidProceduralDirective'));
	end;
end;

function TDelphiParser.ParseMethodHeading: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodHeading

MethodHeading
	-> [CLASS]
			(PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR)
			QualifiedIdent
			(
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]
			(Directive)*
			| '=' Ident
			)
			[';']
}
	case CurrentTokenKind of
	ptProcedure:		Result := ParseProcedureMethodHeading;
	ptFunction:			Result := ParseFunctionMethodHeading;
	ptConstructor:		Result := ParseConstructorMethodHeading;
	ptDestructor:		Result := ParseDestructorMethodHeading;
	ptOperator:			Result := ParseOperatorMethodHeading;
	ptIdentifier:
		begin
			if CurrentTokenExID = ptOperator then
				Result := ParseOperatorMethodHeading
			else
				Result := SynError('Expected method type');
		end;
	ptClass:
		begin
			case PeekTokenKind of
			ptProcedure: 		Result := ParseProcedureMethodHeading;
			ptFunction: 		Result := ParseFunctionMethodHeading;
			ptConstructor: 	Result := ParseConstructorMethodHeading;
			ptDestructor:		Result := ParseDestructorMethodHeading;
			ptOperator:			Result := ParseOperatorMethodHeading;
			ptIdentifier:
				begin
					if PeekTokenExID = ptOperator then
						Result := ParseOperatorMethodHeading
					else
						Result := SynError('Expected method type');
				end;
			else
				Result := SynError('Expected method type');
			end;
		end;
	else
		Result := SynError('Expected method type');
	end;

	if CurrentTokenKind = ptSemiColon then
		Result.AddChild(EatToken(ptSemicolon));

	//TODO: Add FINAL
	while IsPossibleDirective do
	begin
		Result.AddChild(ParseDirective);
	end;
end;

function TDelphiParser.ParseFunctionMethodHeading: TSyntaxNode2;
begin
{
FunctionMethodHeading
	-> [CLASS] FUNCTION QualifiedIdent
			['(' (Parameter [';'])* ')']
			':' MethodReturnType
}
	Result := TSyntaxNode2.Create(ntMethod);
	Result.Attributes[anKind] := AttributeValueToStr(avFunction);

	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.Attributes[anClass] := AttributeValueToStr(avTrue);
	end;

	Result.AddChild(EatToken(ptFunction));
	Result.AddChild(ParseQualifiedIdent);
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseFormalParameterList);
	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseReturnType);
end;

function TDelphiParser.ParseProcedureMethodHeading: TSyntaxNode2;
begin
{
ProcedureMethodHeading
	-> [CLASS] PROCEDURE QualifiedIdent
				['(' (Parameter [';'])* ')']
}
	Result := TSyntaxNode2.Create(ntMethod);
	Result.Attributes[anKind] := AttributeValueToStr(avProcedure);

// [CLASS] PROCEDURE QualifiedIdent
	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.Attributes[anClass] := AttributeValueToStr(avTrue);
	end;

	Result.AddChild(EatToken(ptProcedure));
	Result.AddChild(ParseQualifiedIdent);

//	['(' (Parameter [';'])* ')']
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseFormalParameterList);
end;

function TDelphiParser.ParseConstructorMethodHeading: TSyntaxNode2;
begin
{
ConstructorMethodHeading
	-> [CLASS] CONSTRUCTOR QualifiedIdent
			['(' (Parameter [';'])* ')']
}
	Result := TSyntaxNode2.Create(ntMethod);
	Result.Attributes[anKind] := AttributeValueToStr(avConstructor);

	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.Attributes[anClass] := AttributeValueToStr(avTrue);
	end;

	Result.AddChild(EatToken(ptConstructor));
	Result.AddChild(ParseQualifiedIdent);
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseFormalParameterList);
end;

function TDelphiParser.ParseDestructorMethodHeading: TSyntaxNode2;
begin
{
DestructorMethodHeading
	-> [CLASS] DESTRUCTOR QualifiedIdent
			['(' (Parameter [';'])* ')']
}
	Result := TSyntaxNode2.Create(ntMethod);
	Result.Attributes[anKind] := AttributeValueToStr(avDestructor);

	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.Attributes[anClass] := AttributeValueToStr(avTrue);
	end;

	Result.AddChild(EatToken(ptDestructor));
	Result.AddChild(ParseQualifiedIdent);
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseFormalParameterList);
end;

function TDelphiParser.ParseOperatorMethodHeading: TSyntaxNode2;
begin
{
OperatorMethodHeading
	-> [CLASS] OPERATOR QualifiedIdent
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]
}
	Result := TSyntaxNode2.Create(ntMethod);
	Result.Attributes[anKind] := 'operator';

	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.Attributes[anClass] := AttributeValueToStr(avTrue);
	end;

	if CurrentTokenKind = ptOperator then
		Result.AddChild(EatToken(ptOperator))
	else if (CurrentTokenKind = ptIdentifier) and (CurrentTokenExID = ptOperator) then
		Result.AddChild(EatToken(ptIdentifier))
	else
		Result.AddChild(SynError('Expected operator'));

	Result.AddChild(ParseQualifiedIdent);
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseFormalParameterList);

	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseReturnType);
	end;
end;

function TDelphiParser.ParseVarSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VarSection

VarSection
	-> (VAR | THREADVAR) (VarDecl)+
}
	case CurrentTokenKind of
	ptVar:			Result := TSyntaxNode2.Create(ntVariables);
	ptThreadVar:	Result := TSyntaxNode2.Create(ntVariables);
	else
		Result := SynError('Expected var section');
	end;

//	(VarDecl)+
	Result.AddChild(ParseVarDecl);

	// and keep looping until the next token isn't an identifier
	while IsPossibleIdentifier do
		Result.AddChild(ParseVarDecl);
end;

function TDelphiParser.IsPossibleConstSection: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#ConstSection

ConstSection
	-> CONST (ConstantDecl)+

ConstantDecl
	-> Ident
			[':' Type]
			'=' TypedConstant
			(PortabilityDirective)*
			';'

Ident
	-> <identifier>
	-> <semikeyword>
	-> '&' <identifier>
	-> '&' <semikeyword>
	-> '&' <keyword>

Example
-------

	const
		LEN_FirstName = 50;
		MAX_BUFFER = 1024*16; //16 KB
		clMainInstruction: TColor = $00993300;

Parsed:

; const
	ntConstants
	├─ #ptConstKeyword("const")

;	LEN_FirstName = 50;
	├─ ntConstant(@anName="LEN_FirstName", @anValueText="50")
	│	├─ #ptIdentifier("LEN_FirstName")
	│	├─ #ptEquals("=")
	│	├─ ntExpression(@anValueText="50")
	│	│	└─ #ptIntegerLiteral("50")
	│	└─ #ptSemicolon(";")

;	MAX_BUFFER = 1024*16; //16 KB
	├─ ntConstant(@anName="MAX_BUFFER", @anValueText="1024*16")
	│	├─ #ptIdentifier("MAX_BUFFER")
	│	├─ #ptEquals("=")
	│	├─ ntExpression(@anValueText="1024*16")
	│	│	├─ #ptIntegerLiteral("1024")
	│	│	├─ #ptAsterisk("*")
	│	│	└─ #ptIntegerLiteral("16")
	│	├─ #ptSemicolon(";")
	│	└─ #ptLineComment("//16 KB")

;	clMainInstruction: TColor = $00993300;
	└─ ntConstant(@anName="clMainInstruction", @anType="TColor", @anValueText="$00993300")
		├─ #ptIdentifier("clMainInstruction")
		├─ #ptColon(":")
		├─ ntType(@anType="TColor")
		│  └─ #ptIdentifier("TColor")
		├─ #ptEquals("=")
		├─ ntExpression(@anValueText="$00993300")
		│  └─ #ptHexIntegerLiteral("$00993300")
		└─ #ptSemicolon(";")
}
	Result := (CurrentTokenKind = ptConst);
end;

function TDelphiParser.IsPossibleDirective: Boolean;
var
	directiveType: TptTokenKind;
begin
{
http://dgrok.excastle.com/Grammar.html#Directive

Directive
			Backlinks: MethodHeading, ProcedureType
	-> [';'] ABSTRACT
	-> [';'] ASSEMBLER
	-> [';'] CDECL
	-> [';'] DISPID Expression
	-> [';'] DYNAMIC
	-> [';'] EXPORT
	-> [';'] EXTERNAL [Expression (ExportsSpecifier)*]
	-> [';'] FAR
	-> [';'] FINAL
	-> [';'] FORWARD
	-> [';'] INLINE
	-> [';'] LOCAL
	-> [';'] MESSAGE Expression
	-> [';'] NEAR
	-> [';'] OVERLOAD
	-> [';'] OVERRIDE
	-> [';'] PASCAL
	-> [';'] REGISTER
	-> [';'] REINTRODUCE
	-> [';'] SAFECALL
	-> [';'] STATIC
	-> [';'] STDCALL
	-> [';'] VARARGS
	-> [';'] VIRTUAL
	-> [';'] PortabilityDirective
}
	Result := False;

	directiveType := CurrentTokenKind;

	// the leading semicolon is optional
	if CurrentTokenKind = ptSemicolon then
		directiveType := PeekTokenKind;

	case directiveType of
	ptAbstract,			//		-> [';'] ABSTRACT
	ptAssembler,		//		-> [';'] ASSEMBLER
	ptCDecl,				//		-> [';'] CDECL
	ptDispid,			//		-> [';'] DISPID Expression
	ptDynamic,			//		-> [';'] DYNAMIC
	ptExport, 			//		-> [';'] EXPORT
	ptExternal, 		//		-> [';'] EXTERNAL [Expression (ExportsSpecifier)*]
	ptFar,				//		-> [';'] FAR
	ptFinal,				//		-> [';'] FINAL
	ptForward,			//		-> [';'] FORWARD
	ptInline,			//		-> [';'] INLINE
	ptLocal,				//		-> [';'] LOCAL
	ptMessage,			//		-> [';'] MESSAGE Expression
	ptNear,				//		-> [';'] NEAR
	ptOverload,			//		-> [';'] OVERLOAD
	ptOverride,			//		-> [';'] OVERRIDE
	ptPascal,			//		-> [';'] PASCAL
	ptRegister,			//		-> [';'] REGISTER
	ptReintroduce,		//		-> [';'] REINTRODUCE
	ptSafecall,			//		-> [';'] SAFECALL
	ptStatic,			//		-> [';'] STATIC
	ptStdcall,			//		-> [';'] STDCALL
	ptVarargs,			//		-> [';'] VARARGS
	ptVirtual:			//		-> [';'] VIRTUAL
		Result := True;
	else
//		-> [';'] PortabilityDirective
		Result := IsPossiblePortabilityDirective;
	end;
end;

function TDelphiParser.IsPossibleMethodHeading: Boolean;
begin
{
MethodHeading
	-> [CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR) QualifiedIdent
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]		// FUNCTION required, OPERATOR optional
			[';']
			(Directive [';'])*
}
	// Check for optional `class` prefix
	if CurrentTokenKind = ptClass then
		Result := (PeekTokenKind in [ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator]) or
			((PeekTokenKind = ptIdentifier) and (PeekTokenExID = ptOperator))
	else
		Result := (CurrentTokenKind in [ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator]) or
			((CurrentTokenKind = ptIdentifier) and (CurrentTokenExID = ptOperator));
end;

function TDelphiParser.IsPossibleResStringSection: Boolean;
begin
{
ResStringSection
	-> RESOURCESTRING
			(ConstantDecl)+
}
	case CurrentTokenKind of
	ptConst, ptResourceString: Result := True;
	else
		Result := False;
	end;

end;

function TDelphiParser.IsPossibleTypeDeclaration: Boolean;
var
	i, depth: Integer;
	k: TptTokenKind;

	function PeekKind(Offset: Integer): TptTokenKind;
	begin
		if Offset = 0 then
			Result := CurrentToken.TokenKind
		else
			Result := PeekToken(Offset).TokenKind;
	end;

	procedure SkipAttributeList(var I: Integer);
	begin
		// [Attr, Attr2(...)]  -- attributes aren't nested; we just scan to ']'
		if PeekKind(I) <> ptOpenBracket then Exit;
		Inc(I); // '['
		while True do
		begin
			k := PeekKind(I);
			if (k = ptCloseBracket) then
			begin
				Inc(I); // ']'
				Break;
			end;
			if (k = ptEOF) then Exit;
			Inc(I);
		end;
	end;

	function TrySkipDefiningIdent(var I: Integer): Boolean;
	var
		k0, k1: TptTokenKind;
	begin
		k0 := PeekKind(I);
		case k0 of
		ptIdentifier:
			begin
				// identifier or identifier-with-ExID (semi-keyword/directive) — both OK as a name
				Inc(I);
				Exit(True);
			end;
		ptAmpersand:
			begin
				// & <identifier-or-semi>  OR  & <reserved-word>
				k1 := PeekKind(I+1);
				if (k1 = ptIdentifier) or IsReservedWord(k1) then
				begin
					Inc(I, 2);
					Exit(True);
				end;
				Exit(False);
			end;
		else
			Exit(False);
		end;
	end;

	procedure SkipGenericTypeParams(var I: Integer);
	begin
		// LHS only: TName<T, U: class>
		if PeekKind(I) <> ptLessThan then Exit;
		depth := 0;
		repeat
			k := PeekKind(I);
			if k = ptEOF then
				Exit;

			if k = ptLessThan then
				Inc(depth)
			else if k = ptGreaterThan then
				Dec(depth);

			Inc(I);
		until depth = 0;
	end;

	function IsSectionStart(K: TptTokenKind): Boolean;
	begin
		Result := K in [
			ptVar, ptThreadVar, ptConst, ptType, ptLabel,
			ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator,
			ptBegin, ptEnd, ptInterface, ptImplementation, ptUses, ptContains
		];
	end;

begin
{
	Peeks ahead to see if the next production is a TypeDeclaration,
	meaning you can call ParseTypeDeclaration()


	<TypeSection> ::= 'TYPE' ( <TypeDecl> ';' )+

	<TypeDecl> starts with: [attributes] Ident [<T...>] '='
}
	i := 0;

	// Allow multiple leading attribute lists: [A] [B]
	while PeekKind(i) = ptOpenBracket do
		SkipAttributeList({var}i);

	// Must start with a defining (unqualified) identifier (escaped or not)
	if not TrySkipDefiningIdent({var}i) then
		Exit(False);

	// Optional generic type parameters after the name
	if PeekKind(i) = ptLessThan then
		SkipGenericTypeParams({var}i);

	// Require '=' next; otherwise, it's not a type declaration
	while True do
	begin
		k := PeekKind(i);
		case k of
		ptEquals: Exit(True);					// definitely a TypeDecl
		ptSemicolon, ptEOF: Exit(False);		// no '=' before ';'/EOF
		else
			if IsSectionStart(k) then Exit(False);
			// Anything unexpected between name[/generics] and '=' disqualifies it
			Exit(False);
		end;
	end;
end;


function TDelphiParser.IsPossibleTypeSection: Boolean;
begin
{
TypeSection
	-> TYPE (TypeDecl)+

TypeDecl
	-> Ident '=' [TYPE] Type (PortabilityDirective)* ';'
	-> Ident '=' CLASS ';'
	-> Ident '=' DISPINTERFACE ';'
	-> Ident '=' INTERFACE ';'
}
case CurrentTokenKind of
	ptType: Result := True;
	else
		Result := False;
	end;
end;

function TDelphiParser.IsPossibleUsesClause: Boolean;
begin
{
UsesClause
	-> (USES | CONTAINS)
			(UsedUnit [','])+ ';'
}
	Result := (CurrentTokenKind in [ptUses, ptContains]);

end;

function TDelphiParser.IsPossibleVarSection: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#VarSection

VarSection
	-> (VAR | THREADVAR) (VarDecl)+
}
	Result := (CurrentTokenKind in [ptVar, ptThreadVar]);
end;

function TDelphiParser.IsPossibleVisibilitySection: Boolean;
begin
	Result :=
			(CurrentTokenExID in [ptPrivate, ptProtected, ptPublic, ptPublished])
			or (
				(CurrentTokenExID = ptStrict)
				and
				(PeekTokenExID in [ptPrivate, ptProtected])
			);
end;

function TDelphiParser.IsPossibleVisibilitySectionContent: Boolean;
begin
{
	VisibilitySectionContent
	--> FieldSection				[[CLASS] VAR] (FieldDecl)*
	--> MethodOrProperty			[CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR)
										[CLASS] PROPERTY
	--> ConstSection				(CONST|RESOURCESTRING) (ConstantDecl)+
	--> TypeSection				TYPE (TypeDecl)+

		FieldSection
		--> [[CLASS] VAR] (FieldDecl)*

		MethodOrProperty
		--> MethodHeading
		--> Property

			MethodHeading
			--> [CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR)
					QualifiedIdent
					['(' (Parameter [';'])* ')']
					[':' MethodReturnType]		// FUNCTION required, OPERATOR optional
					[';']
					(Directive [';'])*

			Property
			--> [CLASS]
					PROPERTY Ident
					['[' (Parameter [';'])+ ']']
					[':' MethodReturnType]
					(PropertyDirective)*
					';'

		ConstSection
		--> (CONST|RESOURCESTRING) (ConstantDecl)+

		TypeSection
		--> TYPE (TypeDecl)+
}
	// This is about member *content* (fields/methods/const/type/etc), not the visibility keywords.
	// Keeping this accurate prevents "no progress" loops when class members appear without an
	// explicit visibility section (common Delphi style).
	case CurrentTokenKind of
	ptConst, ptResourceString,		// ConstSection
	ptType,								// TypeSection
	ptProperty, ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator,	// MethodOrProperty
	ptVar, 								// FieldSection
	ptThreadVar,						// FieldSection
	ptOpenBracket,						// FieldSection (custom attribute before field declaration)
	ptCase,								// FieldSection (variant record field)
	ptClass:
		Result := True;
	ptIdentifier:
		begin
			// "private/public/strict/..." are often lexed as ptIdentifier with a non-unknown ExID.
			Result := (CurrentTokenExID = ptUnknown);
		end;
	else
		Result := False;
	end;
end;

function TDelphiParser.ParseTypeSection: TSyntaxNode2;
var
	typeDecl: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#TypeSection

Grammer
=======

TypeSection
	-> TYPE (TypeDecl)+

TypeDecl
	-> Ident '=' [TYPE] Type (PortabilityDirective)* ';'
	-> Ident '=' CLASS ';'
	-> Ident '=' DISPINTERFACE ';'
	-> Ident '=' INTERFACE ';'


Returns: ntTypeSection

Examples
--------

	type
		TSpecial
}
	Result := TSyntaxNode2.Create(ntTypeSection);
	Result.AddChild(EatToken(ptType));	// always produce a token (missing on error)

// Grammar says (TypeDecl)+, so parse one unconditionally, then loop.
	typeDecl := ParseTypeDeclaration; // ntTypeDecl
	Result.AddChild(typeDecl);

	while IsPossibleTypeDeclaration do
		Result.AddChild(ParseTypeDeclaration); // ntTypeDecl
end;

function TDelphiParser.ParseTypeSimple: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := CurrentToken.ValueText;

	case CurrentToken.GenID of
	ptBoolean, ptByte, ptChar,
	//ptDWord, DWORD is not a built-in type; it is in Windows.pas
	ptInt64, ptInteger, ptLongInt,
	ptLongWord, ptPChar, ptShortInt, ptSmallInt, ptWideChar, ptWord: Result.AddChild(ParseOrdinalIdentifier);
	ptComp, ptCurrency, ptDouble, ptExtended, ptReal, ptReal48, ptSingle: Result.AddChild(ParseRealIdentifier);
	ptAnsiString, ptShortString, ptWideString: Result.AddChild(ParseStringIdentifier);
	ptOleVariant, ptVariant: Result.AddChild(ParseVariantIdentifier);
	ptString: Result.AddChild(ParseStringTypeInternal);
	ptFile: Result.AddChild(ParseFileType);
	ptArray:
		begin
			Result.AddChild(EatToken(ptArray));
			Result.AddChild(EatToken(ptOf));
			case CurrentTokenKind of
			ptConst: Result.AddChild(EatToken(ptConst)); // new in ObjectPascal80
			else
				Result.AddChild(ParseTypeId);
			end;
		end;
	else
		Result.AddChild(EatToken(ptIdentifier));
	end;

	if CurrentTokenKind = ptLessThan then
	begin
		Result.AddChild(EatToken(ptLessThan));
		Result.AddChild(ParseTypeArgs);
		Result.AddChild(EatToken(ptGreaterThan));
	end;
end;

function TDelphiParser.ParseTypeParam: TSyntaxNode2;
var
	typeNameNode, constraints: TSyntaxNode2;
begin
(*

Returns
=======

	ntTypeParam

Examples
=========

	TSpecial<T, U: class, constructor; V: IComparable<T>>= class
            ^
            +current token

	ntTypeParam()
		ntType(@anName="T")
		ntType(@anName="U")
		ntConstraints
			ntClassConstraint
			ntConstructorConstraint
		ntType(@anName="V")
		ntConstraints
			ntType(@anNam="IComparable">
				ntTypeArgs
					ntType(@anName="T")


Grammer
=======
TypeParam                  ::= identifier (',' identifier)* [ ':' TypeParamConstraint (',' TypeParamConstraint)* ]
TypeParamConstraint        ::= 'class' | 'record' | 'constructor' | ConstraintTypeRef
ConstraintTypeRef          ::= QualifiedIdent [ '<' TypeArgList '>' ]
QualifiedIdent             ::= identifier ('.' identifier)*
TypeArgList                ::= ConstraintTypeRef (',' ConstraintTypeRef)*

*)
	Result := TSyntaxNode2.Create(ntTypeParam);

	// identifier { ',' identifier }
	// First identifier
	typeNameNode := TSyntaxNode2.Create(ntType);
	typeNameNode.Attributes[anName] := CurrentToken.ValueText;
	typeNameNode.AddChild(EatToken(ptIdentifier));
	Result.AddChild(typeNameNode);

	// Additional identifiers separated by commas
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		typeNameNode := TSyntaxNode2.Create(ntType);
		typeNameNode.Attributes[anName] := CurrentToken.ValueText;
		typeNameNode.AddChild(EatToken(ptIdentifier));
		Result.AddChild(typeNameNode);
	end;

	// Optional constraints after ':'
	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		constraints := TSyntaxNode2.Create(ntConstraints);
		Result.AddChild(constraints);

		constraints.AddChild(ParseTypeParamConstraint);
		while CurrentTokenKind = ptComma do
		begin
			constraints.AddChild(EatToken(ptComma));
			constraints.AddChild(ParseTypeParamConstraint);
		end;
	end;

	// This function leaves expecting to be sitting on a > ptGreaterThan
	// It's also possible it could be sitting on a >= in a special Delphi quirk
end;

// --- Helpers ---------------------------------------------------------------

function TDelphiParser.ParseTypeParamConstraint: TSyntaxNode2;
//var
//	N: TSyntaxNode2;
begin
{
	TSpecial<T, U: class, constructor; V: IComparable<T>>= class
   ---------------^currentToken


	TSpecial<T, U: class, constructor; V: IComparable<T>>= class
   ----------------------^currentToken

	TSpecial<T, U: class, constructor; V: IComparable<T>>= class
   --------------------------------------^currentToken

}
	case CurrentTokenKind of
	ptClass:
		begin
			Result := TSyntaxNode2.Create(ntClassConstraint);
			Result.AddChild(EatToken(ptClass));
		end;
	ptRecord:
		begin
			Result := TSyntaxNode2.Create(ntRecordConstraint);
			Result.AddChild(EatToken(ptRecord));
		end;
	ptConstructor:
		begin
			Result := TSyntaxNode2.Create(ntConstructorConstraint);
			Result.AddChild(EatToken(ptConstructor));
		end;
	else
		// Interface/class type constraint, possibly generic (e.g., IComparable<T>)
		Result := ParseConstraintTypeRef;
	end;
end;

function TDelphiParser.ParseConstraintTypeRef: TSyntaxNode2;
//var
//	args: TSyntaxNode2;
begin
{
Example

	TSpecial<T, U: class, constructor; V: IComparable<T>>= class
	--------------------------------------^currentToken

	ntType(@anName="IComparable")
		ntTypeArgs
			ntType(@naName="T")
		ptGreaterThan('>')
}
	// Base type name (qualified identifier)
	Result := TSyntaxNode2.Create(ntType);
	Result.AddChild(ParseQualifiedIdent); // ntQualifiedIdentifier("IComparable")

	// Optional generic type arguments: '<' Type {',' Type} '>'
	if CurrentTokenKind = ptLessThan then
	begin
		Result.AddChild(ParseTypeParams); //!!RECURSIVE!!
	end;
end;

function TDelphiParser.ParseTypeParams: TSyntaxNode2;
begin
{
Returns
	ntTypeParams

Grammer
=======

	< ntTypeParam `;` ntTypeParam, ...  >

Example
=======

	TSpecial<T>= class
	--------^current token

	TSpecial<T> = class
	--------^current token

	TSpecial<T, U: class, constructor; V: IComparable<T>>= class
	--------^current token


	ntTypeParams
		ptLessThan('<')
		ntTypeParam()
			ntType(@anName="T")
			ptComma(',')
			ntType(@anName="U")
			ptColon(':')
			ntContraints
				ntClassConstraint
				ntConstructorConstraint
		ntTypeParam
			ntType(@anName="V")
			ntContraints
				ntType(@anName="IComparable")
					ntTypeArgs
						ntType(@naName="T")
		ptGreaterThan('>')
}
	Result := TSyntaxNode2.Create(ntTypeParams);
	Result.AddChild(EatToken(ptLessThan));

	// first param
	Result.AddChild(ParseTypeParam); // ntTypeParam

	// additional params
	while CurrentTokenKind = ptSemiColon do
	begin
		Result.AddChild(EatToken(ptSemiColon));
		Result.AddChild(ParseTypeParam);
	end;

{
	We expect to be sitting on a > sign right now, the end of the <T> section.

	But due to the weird interaction with our tokenizer, both of the following work.

		<T> = class
		<T>= class

	There is an issue where this following are both valid:

		TFoo<T: record> = class				// ptGreaterThan ptEquals
		TFoo<T: record>= class				// ptGreaterThanEquals

	So you have to handle the optional space.

		ParseTypeParams reads up until the final > or >=
}
	// Handle the >= quirk
	if CurrentTokenKind = ptGreaterThanEquals then
		Result.AddChild(EatToken(ptGreaterThanEquals))
	else
	begin
		// The normal syntax
		Result.AddChild(EatToken(ptGreaterThan));
	end;

end;

function TDelphiParser.ParseConstSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ConstSection

ConstSection
	-> CONST (ConstantDecl)+

ConstantDecl
	-> Ident
			[':' Type]
			'=' TypedConstant
			(PortabilityDirective)*
			';'

Ident
	-> <identifier>
	-> <semikeyword>
	-> '&' <identifier>
	-> '&' <semikeyword>
	-> '&' <keyword>

Example
-------

	const
		LEN_FirstName = 50;
		MAX_BUFFER = 1024*16; //16 KB
		clMainInstruction: TColor = $00993300;

Parsed:

; const
	ntConstants
	├─ #ptConstKeyword("const")

;	LEN_FirstName = 50;
	├─ ntConstant(@anName="LEN_FirstName", @anValueText="50")
	│	├─ #ptIdentifier("LEN_FirstName")
	│	├─ #ptEquals("=")
	│	├─ ntExpression(@anValueText="50")
	│	│	└─ #ptIntegerLiteral("50")
	│	└─ #ptSemicolon(";")

;	MAX_BUFFER = 1024*16; //16 KB
	├─ ntConstant(@anName="MAX_BUFFER", @anValueText="1024*16")
	│	├─ #ptIdentifier("MAX_BUFFER")
	│	├─ #ptEquals("=")
	│	├─ ntExpression(@anValueText="1024*16")
	│	│	├─ #ptIntegerLiteral("1024")
	│	│	├─ #ptAsterisk("*")
	│	│	└─ #ptIntegerLiteral("16")
	│	├─ #ptSemicolon(";")
	│	└─ #ptLineComment("//16 KB")

;	clMainInstruction: TColor = $00993300;
	└─ ntConstant(@anName="clMainInstruction", @anType="TColor", @anValueText="$00993300")
		├─ #ptIdentifier("clMainInstruction")
		├─ #ptColon(":")
		├─ ntType(@anType="TColor")
		│  └─ #ptIdentifier("TColor")
		├─ #ptEquals("=")
		├─ ntExpression(@anValueText="$00993300")
		│  └─ #ptHexIntegerLiteral("$00993300")
		└─ #ptSemicolon(";")
}
	Result := TSyntaxNode2.Create(ntConstants);
	Result.AddChild(EatToken(ptConst));

{
ConstSection
	-> CONST (ConstantDecl)+
}
	Result.AddChild(ParseConstantDecl);
	while IsPossibleConstantDecl do
	begin
		Result.AddChild(ParseConstantDecl);
	end;
end;

function TDelphiParser.IsPossibleConstantDecl: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#ConstSection

ConstantDecl
	-> Ident
			[':' Type]
			'=' TypedConstant
			(PortabilityDirective)*
			';'

Ident
	-> <identifier>
	-> <semikeyword>
	-> '&' <identifier>
	-> '&' <semikeyword>
	-> '&' <keyword>

Examples
--------

;	LEN_FirstName = 50;
	├─ ntConstant(@anName="LEN_FirstName", @anValueText="50")
	│	├─ #ptIdentifier("LEN_FirstName")
	│	├─ #ptEquals("=")
	│	├─ ntExpression(@anValueText="50")
	│	│	└─ #ptIntegerLiteral("50")
	│	└─ #ptSemicolon(";")

;	MAX_BUFFER = 1024*16; //16 KB
	├─ ntConstant(@anName="MAX_BUFFER", @anValueText="1024*16")
	│	├─ #ptIdentifier("MAX_BUFFER")
	│	├─ #ptEquals("=")
	│	├─ ntExpression(@anValueText="1024*16")
	│	│	├─ #ptIntegerLiteral("1024")
	│	│	├─ #ptAsterisk("*")
	│	│	└─ #ptIntegerLiteral("16")
	│	├─ #ptSemicolon(";")
	│	└─ #ptLineComment("//16 KB")

;	clMainInstruction: TColor = $00993300;
	└─ ntConstant(@anName="clMainInstruction", @anType="TColor", @anValueText="$00993300")
		├─ #ptIdentifier("clMainInstruction")
		├─ #ptColon(":")
		├─ ntType(@anType="TColor")
		│  └─ #ptIdentifier("TColor")
		├─ #ptEquals("=")
		├─ ntExpression(@anValueText="$00993300")
		│  └─ #ptHexIntegerLiteral("$00993300")
		└─ #ptSemicolon(";")
}
	Result := IsPossibleIdentifier;
	//TODO or CurrentTokenKind in Directives[]
	//TODO or currentTokekKind=ptAmpersand and NextTokenKind=ptIdentifier
	//TODO or currentTokekKind=ptAmpersand and NextTokenKind in Directives
	//TODO or currentTokekKind=ptAmpersand and NextTokenKind in ReservedWords
end;

function TDelphiParser.ParseResStringSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#InterfaceDecl

ResStringSection
	-> RESOURCESTRING ( ResourceStringDecl )*

ResourceStringDecl
	-> Ident '=' ConstExpr ';'

Example
-------

resourcestring
	SOK = 'OK';

ntResourceStrings

}
	Result := TSyntaxNode2.Create(ntResourceStrings);
	Result.AddChild(EatToken(ptResourceString));

	// You are allowed to have zero string delcarations after `resourcestring`. I have spoken.
	while IsPossibleResourceStringDecl do
	begin
		Result.AddChild(ParseResourceStringDecl);
	end;

end;

function TDelphiParser.IsPossibleResourceStringDecl: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#InterfaceDecl

ResourceStringDecl
	-> Ident '=' ConstExpr
}
	Result := IsPossibleIdentifier;
end;

function TDelphiParser.ParseResourceStringDecl: TSyntaxNode2;
var
	name: TSyntaxToken; 	// ntIdentifier
	expr: TSyntaxNode2; // ntExpression
	s: string;
	strTok: TSyntaxToken;
begin
{
ResourceStringDecl
	-> Ident '=' ConstExpr ';'

ConstExpr
	-> ?An expression which evaluates to a constant at compilation time?

Examples
========

	SFirstName = 'Steve';

	├─ ntResourceString(@anName:'SFirstName', @anValueText:'Steve')
	│	├─ #ptIdentifier('SFirstName')
	│	├─ #ptEquals("=")
	│	├─ ntExpression(@anValueText="50")
	│	│	└─ #ptStringLiteral("Steve")
	│	└─ #ptSemicolon(";")
}
	Result := TSyntaxNode2.Create(ntResourceString);

	// SFirstName
	name := EatToken(ptIdentifier);
	Result.AddChild(name);
	Result[anName] := name.Text;

	// =
	Result.AddChild(EatToken(ptEquals));

{
	Read the strings that make up the resourcestring

	#ptStringLiteral ( #ptPlus #ptStringLiteral )*
}
	expr := TSyntaxNode2.Create(ntExpression);

	strTok := EatToken(ptStringLiteral);
	s := strTok.ValueText;
	expr.AddChild(strTok);

	while (CurrentTokenKind = ptPlus) do
	begin
		expr.AddChild(EatToken(ptPlus));

		strTok := EatToken(ptStringLiteral);
		s := s+strTok.ValueText;
		expr.AddChild(strTok);
	end;

	expr.Attributes[anValueText] := s;

	Result.AddChild(expr);
	Result.Attributes[anValueText] := s;
	Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParseInterfaceDeclaration: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#InterfaceDecl

InterfaceDecl
	-> ConstSection			==> ntConstants
	-> ResStringSection		==> ntResourceString
	-> TypeSection
	-> VarSection
	-> MethodHeading
}

	if IsPossibleConstSection then
		Result := ParseConstSection
	else if IsPossibleResStringSection then
		Result := ParseResStringSection
	else if IsPossibleTypeSection then
		Result := ParseTypeSection
	else if IsPossibleVarSection then
		Result := ParseVarSection
	else if IsPossibleMethodHeading then
		Result := ParseMethodHeading
	else
		Result := SynError('InvalidInterfaceDeclaration');
end;

function TDelphiParser.ParseExportsItem: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExportsItem

ExportsItem
	-> Ident (ExportsSpecifier)*

Backlinks: ExportsStatement

ExportsSpecifier [^]
	-> (INDEX | NAME) Expression
}
	Result := TSyntaxNode2.Create(ntElement);
	Result.AddChild(ParseIdent);

//	(ExportsSpecifier)*
	while CurrentTokenKind in [ptIndex, ptName] do
	begin
		Result.AddChild(EatToken());
		Result.AddChild(ParseExpression);
	end;
end;

function TDelphiParser.ParseExportStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExportsStatement

ExportsStatement
			Backlinks: ImplementationDecl
	-> EXPORTS (ExportsItem [','])+ ';'
}
	Result := TSyntaxNode2.Create(ntExports);
	Result.AddChild(EatToken(ptExports));

   // Test: with and without any export items; just "exports;"  Is that valid?

	Result.AddChild(ParseExportsItem);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseExportsItem);
	end;
	Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.IsPossibleRequiresClause: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#RequiresClause

RequiresClause
	-> REQUIRES (QualifiedIdent [','])+ ';'


ntRequires
	ntQualifiedIdentifier
}
	Result := (CurrentTokenKind = ptRequires);
end;

function TDelphiParser.ParseRequiresClause: TSyntaxNode2;
begin
{
Returns:
	ntRequires
		ntQualifiedIdentifier

http://dgrok.excastle.com/Grammar.html#RequiresClause

RequiresClause
	-> REQUIRES (QualifiedIdent [','])+ ';'
}
	Result := TSyntaxNode2.Create(ntRequires);

//	REQUIRES
	Result.AddChild(EatTokenEx(ptRequires));

//	(QualifiedIdent [','])+ ';'
	Result.AddChild(ParseQualifiedIdent);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseQualifiedIdent);
	end;
	Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParseRequiresIdentifier: TSyntaxNode2;
//var
//	NamesNode: TSyntaxNode2;
begin
	Result := PoisonNode;

(*
   todo: figure this out
	Result := TSyntaxNode2.Create(ntUnknown);
	try
		FStack.Push(NamesNode);
		try
			Result.AddChild(ParseRequiresIdentifierId);
			while CurrentToken.TokenKind = ptDot do
			begin
				NextToken;
				Result.AddChild(ParseRequiresIdentifierId);
			end;
		finally
			FStack.Pop;
		end;

		FStack.AddChild(ntPackage).Attributes[anName] := NodeListToString(NamesNode);
	finally
		NamesNode.Free;
	end;
*)
end;

function TDelphiParser.ParseRequiresIdentifierId: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntUnknown);
	Result.Attributes[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseInitializationSection: TSyntaxNode2;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntInitialization);
	Result.AddChild(EatToken(ptInitialization));
	Result.AddChild(ParseStatementList);
end;

function TDelphiParser.ParseImplementationSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ImplementationSection

ImplementationSection
	-> IMPLEMENTATION
			[UsesClause]
			(ImplementationDecl)*
}
	Result := TSyntaxNode2.Create(ntImplementation);

	// IMPLEMENTATION
	Result.AddChild(EatToken(ptImplementation));

	// [UsesClause]
	if IsPossibleUsesClause then
		Result.AddChild(ParseUsesClause);

	// (ImplementationDecl)*
	while IsPossibleImplementationDecl do
		Result.AddChild(ParseImplementationDecl);

end;

function TDelphiParser.ParseInterfaceSection: TSyntaxNode2;
begin
{
Returns:
	ntInterface

http://dgrok.excastle.com/Grammar.html#InterfaceSection

InterfaceSection
	-> INTERFACE
			[UsesClause]
			(InterfaceDecl)*


Example
=======

interface

uses
	winapi.msxml, Toolkit, Contoso.Exceptions;

Parsed
======

ntInterface
	ptInterface('interface')
	ntUses
		ptUses('uses')
		ntUsedUnit('winapi.msxml')
			ptIdentifier('winapi')
			ptDot('.')
			ptIdentifier('msxml')
			ptComma(',')
		ntUsedUnit('Tookit')
			ptIdentifier('Toolkit')
			ptComma(',')
		ntUsedUnit('Avatar.Exceptions')
			ptIdentifier('Avatar')
			ptDot('.')
			ptIdentifier('Exceptions')
			ptComma(',')
		ptSemicolon(";")

}
	Result := TSyntaxNode2.Create(ntInterfaceSection);
	Result.AddChild(EatToken(ptInterface));

	// Peek if it is `uses` (or `contains`)
	if CurrentToken.TokenKind in [ptUses, ptContains] then
		Result.AddChild(ParseUsesClause);

{
	(InterfaceDecl)*
		zero or more InterfaceDecl
}
	while CurrentToken.TokenKind in [ptConst, ptFunction, ptResourceString, ptProcedure,
		ptThreadVar, ptType, ptVar, ptExports, ptOpenBracket] do
	begin
		Result.AddChild(ParseInterfaceDeclaration);
	end;

end;

function TDelphiParser.ParsePortabilityDirective: TSyntaxNode2;
var
	s: string;
begin
{
http://dgrok.excastle.com/Grammar.html#PortabilityDirective

PortabilityDirective
	-> platform
	-> deprecated [ <stringliteral> ]
	-> library
	-> experimental

Returns
	ntPortabilityDirective

DGrok Bug: Added <stringliteral> after deprecated


Declarations and ParseStatements (Delphi)
https://docwiki.embarcadero.com/RADStudio/Sydney/en/Declarations_and_Statements_(Delphi)#Hinting_Directives
=========================================

Hinting Directives
------------------

The 'hint' directives `platform`, `deprecated`, and `library` may be appended to
any declaration. These directives will produce warnings at compile time.

Hint directives can be applied to type declarations, variable declarations, class,
interface, and structure declarations, field declarations within classes or
records, procedure, function, and method declarations, and unit declarations.

When a hint directive appears in a unit declaration, it means that the hint
applies to everything in the unit. For example, the Windows 3.1 style
OleAuto.pas unit on Windows is completely deprecated. Any reference to that unit
or any symbol in that unit produces a deprecation message.

The platform hinting directive on a symbol or unit indicates that it may not
exist or that the implementation may vary considerably on different platforms.
The library hinting directive on a symbol or unit indicates that the code may
not exist or the implementation may vary considerably on different library
architectures.

The platform and library directives do not specify which platform or library.
If your goal is writing platform-independent code, you do not need to know which
platform a symbol is specific to; it is sufficient that the symbol be marked as
specific to some platform to let you know it may cause problems for your goal of
portability.

In the case of a procedure or function declaration, the hint directive should be
separated from the rest of the declaration with a semicolon.

}
	if not (CurrentToken.GenID in [ptPlatform, ptDeprecated, ptLibrary, ptExperimental]) then
	begin
		Result := SynErrorFmt('Expected %s but was %s', ['PortabilityDirective', CurrentToken.ValueText]);
		Exit;
	end;

	Result := TSyntaxNode2.Create(ntPortabilityDirective);

	case CurrentToken.GenID of
	ptPlatform:			Result.Attributes[anPlatform]			:= AttributeValueToStr(avTrue);
	ptLibrary:			Result.Attributes[anLibrary]			:= AttributeValueToStr(avTrue);
	ptExperimental:	Result.Attributes[anExperimental]	:= AttributeValueToStr(avTrue);
	ptDeprecated:
		begin
			Result.Attributes[anDeprecated]		:= AttributeValueToStr(avTrue);

			// Check for deprecated 'use this other unit'
			// [ptDeprecated] [ptStringLiteral]
			if PeekTokenKind = ptStringLiteral then
			begin
				s := CurrentToken.ValueText;
				Result.AddChild(EatToken(ptStringLiteral)); // the reason for the deprecation
				Result.Attributes[anDeprecated] := s;
			end;
		end;
	end;
end;

function TDelphiParser.IsPossiblePortabilityDirective: Boolean;
begin
{
PortabilityDirective
	-> platform
	-> deprecated
	-> library
	-> experimental
}
	Result := (CurrentTokenGenID in [ptPlatform, ptDeprecated, ptLibrary, ptExperimental]);
end;

function TDelphiParser.IsPossibleProperty: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#Property

Property [^]
	-> [CLASS]
			PROPERTY Ident
			['[' (Parameter [';'])+ ']']
			[':' MethodReturnType]
			(PropertyDirective)*
			';'
}
	Result := (CurrentTokenKind = ptProperty)
			or ((CurrentTokenKind = ptClass) and (PeekTokenKind = ptProperty));
end;

function TDelphiParser.IsPossiblePropertyDirective: Boolean;
	function IsDirectiveWord(const AWord: string): Boolean;
	var
		tokText: string;
	begin
		tokText := CurrentToken.ValueText;
		if tokText = '' then
			tokText := CurrentToken.Text;
		Result := SameText(tokText, AWord);
	end;

	function PeekIsDirectiveWord(const AWord: string): Boolean;
	var
		tokText: string;
	begin
		tokText := PeekToken.ValueText;
		if tokText = '' then
			tokText := PeekToken.Text;
		Result := SameText(tokText, AWord);
	end;
begin
{
http://dgrok.excastle.com/Grammar.html#PropertyDirective

PropertyDirective
	-> ';' DEFAULT
	-> DEFAULT Expression
	-> DISPID Expression
	-> IMPLEMENTS (QualifiedIdent [','])+
	-> INDEX Expression
	-> NODEFAULT
	-> READ Expression
	-> READONLY
	-> STORED Expression
	-> WRITE Expression
	-> WRITEONLY
}
	if CurrentTokenKind = ptSemicolon then
	begin
		Result := (PeekTokenExID = ptDefault) or PeekIsDirectiveWord('default');
		Exit;
	end;

	case CurrentTokenExID of
	ptDefault,
	ptDispid,
	ptImplements,
	ptIndex,
	ptNodefault,
	ptRead,
	ptReadonly,
	ptStored,
	ptWrite,
	ptWriteonly: Result := True
	else
		Result := IsDirectiveWord('default')
			or IsDirectiveWord('dispid')
			or IsDirectiveWord('implements')
			or IsDirectiveWord('index')
			or IsDirectiveWord('nodefault')
			or IsDirectiveWord('read')
			or IsDirectiveWord('readonly')
			or IsDirectiveWord('stored')
			or IsDirectiveWord('write')
			or IsDirectiveWord('writeonly');
	end;
end;

function TDelphiParser.ParseIdentifierList: TSyntaxNode2;
begin
	{
	}
	Result := PoisonNode;

	Result.AddChild(ParseIdent);

	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseIdent);
	end;
end;

function TDelphiParser.ParseCharString: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentToken.GenID of
	ptAsciiChar, ptIdentifier, ptOpenParen, ptStringLiteral:
		while CurrentToken.GenID in
			[ptAsciiChar, ptIdentifier, ptOpenParen, ptStringLiteral, ptString] do
		begin
			case CurrentTokenKind of
			ptIdentifier, ptOpenParen:
				begin
					if CurrentTokenExID in [ptIndex] then
						Break;
					Result.AddChild(ParseVariableReference);
				end;
			ptString:
				begin
					Result.AddChild(ParseStringStatement);
					Result.AddChild(ParseStatement);
				end;
			else
				Result.AddChild(ParseStringConst);
			end;
//			if FCurrentToken.TokenKind = ptPoint then
//			begin
//				NextToken;
//          VariableReference;
//        end;
		end;
  else
	 begin
		Result.AddChild(SynError('InvalidCharString'));
	 end;
  end;
end;

function TDelphiParser.ParseScriptFile: TSyntaxNode2;
begin
	{
	}
	Result := PoisonNode;

{
	Read the allowed set of child tokens
}
	while CurrentTokenKind <> ptEof do
	begin
		case CurrentTokenKind of
		ptClass:				Result.AddChild(ParseProcedureDeclarationSection);
		ptConst:				Result.AddChild(ParseConstSection);
		ptConstructor:		Result.AddChild(ParseProcedureDeclarationSection);
		ptDestructor:		Result.AddChild(ParseProcedureDeclarationSection);
		ptExports:			Result.AddChild(ParseExportStatement);
		ptFunction:			Result.AddChild(ParseProcedureDeclarationSection);
		ptIdentifier:		Result.AddChild(ParseStatements);
		ptLabel:				Result.AddChild(ParseLabelDeclarationSection);
		ptProcedure:		Result.AddChild(ParseProcedureDeclarationSection);
		ptResourceString:	Result.AddChild(ParseConstSection);
		ptType:				Result.AddChild(ParseTypeSection);
		ptThreadVar:		Result.AddChild(ParseVarSection);
		ptVar:				Result.AddChild(ParseVarSection);
		else
			Result.AddChild(SynErrorFmt(SE2029, ['Top-level declaration', CurrentToken.Text]));
		end;
	end;
end;

function TDelphiParser.ParseClassClass: TSyntaxNode2;
begin
	Result := PoisonNode;

	// TODO: fix this
//	FStack.Peek.SetAttribute(anClass, AttributeValues[atTrue]);

	Result.AddChild(EatToken(ptClass));
end;

function TDelphiParser.ParseClassConstraint: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntClassConstraint);
		Result.AddChild(EatToken(ptClass));
end;

function TDelphiParser.ParsePropertyDefault: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatTokenEx(ptDefault));
end;

function TDelphiParser.ParseDispIDSpecifier: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatTokenEx(ptDispid));
  Result.AddChild(ParseConstantExpression);
end;

function TDelphiParser.ParseIndexOp: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntIndexed);

		Result.AddChild(EatToken(ptOpenBracket));
		Result.AddChild(ParseExpression);
		while CurrentTokenKind = ptComma do
		begin
			Result.AddChild(EatToken(ptComma));
			Result.AddChild(ParseExpression);
		end;
		Result.AddChild(EatToken(ptCloseBracket));
end;

function TDelphiParser.ParseIndexSpecifier: TSyntaxNode2;
begin
	{
	}
	Result := TSyntaxNode2.Create(ntIndex);
	Result.AddChild(EatTokenEx(ptIndex));
	Result.AddChild(ParseConstantExpression);
end;

function TDelphiParser.ParseClassTypeEnd: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenExID of
	ptExperimental: Result.AddChild(EatToken(ptExperimental));
	ptDeprecated: Result.AddChild(ParseDirectiveDeprecated);
	end;
end;

function TDelphiParser.ParseObjectTypeEnd: TSyntaxNode2;
begin
	Result := PoisonNode;
end;

function TDelphiParser.ParseDirectiveDeprecated: TSyntaxNode2;
begin
{
	DEPRICATED [ string ]

ntHintDiretive
}
	Result := TSyntaxNode2.Create(ntHintDirective);
	Result.AddChild(EatTokenEx(ptDeprecated));
	Result.Attributes[anPlatform] := AttributeValueToStr(avTrue);

	if CurrentTokenKind = ptStringLiteral then
		Result.AddChild(EatToken);
end;

procedure TDelphiParser.DirectiveInline(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anInline] := AttributeValueToStr(avTrue);
	ParentNode.AddChild(EatToken(ptInline));
end;

function TDelphiParser.ParseDirectiveLibrary: TSyntaxNode2;
begin
	Result := PoisonNode;
  Result.AddChild(EatToken(ptLibrary));
end;

function TDelphiParser.ParseDirectivePlatform: TSyntaxNode2;
begin
	Result := PoisonNode;
	Result.AddChild(EatTokenEx(ptPlatform));
end;

function TDelphiParser.ParseEnumeratedTypeElement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#EnumeratedTypeElement

EnumeratedTypeElement
			Backlinks: EnumeratedType
	-> Ident [ '=' Expression ]
}
	Result := ParseIdent;

	if CurrentTokenKind = ptEquals then
	begin
		Result.AddChild(EatToken(ptEquals));
		Result.AddChild(ParseConstantExpression);
	end;
end;

function TDelphiParser.ParseIdent: TSyntaxNode2;
var
	iden: TSyntaxToken;
begin
{
http://dgrok.excastle.com/Grammar.html#Ident

Ident
	-> <identifier>
	-> <semikeyword>
	-> '&' <identifier>
	-> '&' <semikeyword>
	-> '&' <keyword>

ntIdentifier anName:"firstName"
}
	Result := TSyntaxNode2.Create(ntIdentifier);

	if CurrentTokenKind = ptAmpersand then
	begin
		// When prefixed (&) you can use keywords as an identifier.
		// Please never do this.
		Result.AddChild(EatToken(ptAmpersand));

		if IsReservedWord(CurrentTokenKind) then
			iden := EatToken
		else
			iden := EatToken(ptIdentifier);
	end
	else
		iden := EatToken(ptIdentifier);

	Result.AddChild(iden);
	Result.Attributes[anName] := iden.ValueText;
end;

function TDelphiParser.IsPossibleIdentifier: Boolean;
begin
{
	http://dgrok.excastle.com/Grammar.html#Ident

Ident
	-> <identifier>
	-> <semikeyword>
	-> '&' <identifier>
	-> '&' <semikeyword>
	-> '&' <keyword>

	ntIdentifier(@anName:"firstName")

	TODO: handle semikeyword, and & prefixes
}
	Result := (CurrentTokenKind = ptIdentifier);
end;

function TDelphiParser.IsPossibleImplementationDecl: Boolean;
begin
{
ImplementationDecl
	-> LabelDeclSection 			// LABEL
	-> ConstSection				// CONST|RESOURCESTRING
	-> TypeSection					// TYPE
	-> VarSection					// VAR | THREADVAR
	-> MethodImplementation		// [CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR)
	-> ExportsStatement			// EXPORTS
	-> AssemblyAttribute			// '[' ASSEMBLY ':' Expression ']'
}
	if CurrentTokenKind in [ptLabel, ptConst, ptResourceString, ptVar, ptThreadVar,
			ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator, ptExports] then
		Result := True
	else if CurrentTokenKind = ptClass then
		Result := (PeekTokenKind in [ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator])
		// TODO: Decide if asembly is an identifier with an extended type of assembly
//	else if (CurrentTokenKind = ptOpenBracket) and (PeekTokenKind = ptAssembly) then
//		Result := True
	else
		Result := False;
end;

function TDelphiParser.IsPossibleLabelID: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#ParseLabelId

ParseLabelId
	-> <number>
	-> Ident
}
	Result := CurrentTokenKind in [ptIntegerConst, ptIdentifier];
end;

function TDelphiParser.ParseDirectiveLocal: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatTokenEx(ptLocal));
end;

function TDelphiParser.ParseDirectiveVarargs: TSyntaxNode2;
begin
	Result := PoisonNode;
	Result.AddChild(EatTokenEx(ptVarargs));
end;

function TDelphiParser.ParseAncestorList: TSyntaxNode2;
//var
//	s: string;
begin
{
The part of ClassType:

ClassType
	-> CLASS
			[ABSTRACT | SEALED]
			['(' (QualifiedIdent [','])+ ')']

The remainder is optional, but only if the base class is specified and lookahead shows that the next token is a semicolon

			(VisibilitySection)*
			END

But specificlaly this list:

	(QualifiedIdent [','])+


type
	TTriangle = class(TShape, IShape, ILogger)
	                  \_____________________/

	ntAncestorList
		ntQualifiedIdentifier anName="TShape"
		ntQualifiedIdentifier anName="IShape"
		ntQualifiedIdentifier anName="ILogger"

}
	Result := TSyntaxNode2.Create(ntAncestorList);

	Result.AddChild(ParseQualifiedIdent);

	while (CurrentTokenKind = ptComma) do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseQualifiedIdent);
	end;
end;

function TDelphiParser.ParseAnonymousMethod: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAnonymousMethod);
		case CurrentTokenKind of
		ptFunction:
			begin
				Result.AddChild(EatToken(ptFunction));
				if CurrentTokenKind = ptOpenParen then
					Result.AddChild(ParseFormalParameterList);
				Result.AddChild(EatToken(ptColon));
				Result.AddChild(ParseReturnType);
			end;
		ptProcedure:
			begin
				Result.AddChild(EatToken(ptProcedure));
				if CurrentTokenKind = ptOpenParen then
					Result.AddChild(ParseFormalParameterList);
			end;
		end;
		Result.AddChild(ParseBlock);
end;

function TDelphiParser.ParseAnonymousMethodType: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatTokenEx(ptReference));
	Result.AddChild(EatToken(ptTo));
	case CurrentTokenKind of
	ptProcedure:
		begin
			Result.AddChild(EatToken(ptProcedure));
			if CurrentTokenKind = ptOpenParen then
				Result.AddChild(ParseFormalParameterList);
		end;
	ptFunction:
		begin
			Result.AddChild(EatToken(ptFunction));
			if CurrentTokenKind = ptOpenParen then
				Result.AddChild(ParseFormalParameterList);
			Result.AddChild(EatToken(ptColon));
			Result.AddChild(ParseReturnType);
		end;
	end;
end;

function TDelphiParser.ParseExportsName: TSyntaxNode2;
//var
//	NamesNode: TSyntaxNode2;
begin
	Result := PoisonNode;


{
  NamesNode := TSyntaxNode2.Create(ntUnknown);
  try
    FStack.Push(NamesNode);
    try
        Result.AddChild(ParseExportsNameId);
        while CurrentToken.TokenKind = ptDot do
        begin
          NextToken;
          Result.AddChild(ParseExportsNameId);
        end;
    finally
      FStack.Pop;
    end;

    FStack.Peek.Attributes[anName] := NodeListToString(NamesNode);
  finally
    NamesNode.Free;
  end;
}
end;

function TDelphiParser.ParseImplementsSpecifier: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntImplements);
	Result.AddChild(EatTokenEx(ptImplements));

     Result.AddChild(ParseTypeId);
     while (CurrentTokenKind = ptComma) do
     begin
	       Result.AddChild(EatToken(ptComma));
       Result.AddChild(ParseTypeId);
     end;
end;

function TDelphiParser.ParseAttributeArgumentName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseCaseLabelList: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntCaseLabels);

	Result.AddChild(ParseCaseLabel);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken);
		Result.AddChild(ParseCaseLabel);
	end;
end;

function TDelphiParser.ParseProceduralDirectiveOf: TSyntaxNode2;
begin
	Result := PoisonNode;

	Result.AddChild(EatToken(ptOf));
	Result.AddChild(EatToken(ptObject));
end;

function TDelphiParser.ParseInheritedVariableReference: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntInherited);

	Result.AddChild(EatToken(ptInherited));
	if CurrentToken.TokenKind = ptIdentifier then
		Result.AddChild(ParseVariableReference);
end;

function TDelphiParser.ParseGlobalAttributeTargetSpecifier: TSyntaxNode2;
begin
	Result := ParseGlobalAttributeTarget;
	Result.AddChild(EatToken(ptColon));
end;

function TDelphiParser.ParseGlobalAttributeTarget: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAttribute); // todo: ntAttribute?
	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseAttributeSections: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAttributes);

	while CurrentTokenKind = ptOpenBracket do
		Result.AddChild(ParseAttributeSection);
end;

function TDelphiParser.ParseAttributeSection: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAttribute);

	Result.AddChild(EatToken(ptOpenBracket));
	if PeekTokenKind = ptColon then
		Result.AddChild(ParseAttributeTargetSpecifier);
	Result.AddChild(ParseAttributeList);
	while CurrentToken.TokenKind = ptComma do
	begin
		if PeekTokenKind = ptColon then
			Result.AddChild(ParseAttributeTargetSpecifier);
		Result.AddChild(ParseAttributeList);
	end;
	Result.AddChild(EatToken(ptCloseBracket));
end;

function TDelphiParser.ParseAttributeTargetSpecifier: TSyntaxNode2;
begin
	Result := ParseAttributeTarget;
	Result.AddChild(EatToken(ptColon));
end;

function TDelphiParser.ParseAttributeTarget: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntUnknown); // todo: what is an attribute target?

	case CurrentToken.TokenKind of
	ptProperty: Result.AddChild(EatToken(ptProperty));
	ptType: Result.AddChild(EatToken(ptType));
	else
		Result.AddChild(EatToken(ptIdentifier));
	end;
end;

function TDelphiParser.ParseAttributeList: TSyntaxNode2;
begin
	Result := ParseAttribute;

	while CurrentToken.TokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseAttributeList);
	end;
end;

function TDelphiParser.ParseAttribute: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAttribute);

	Result.AddChild(ParseAttributeName);
	if CurrentToken.TokenKind = ptOpenParen then
		Result.AddChild(ParseAttributeArguments);
end;

function TDelphiParser.ParseAttributeName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

	case CurrentToken.TokenKind of
	ptIn, ptOut, ptConst, ptVar, ptUnsafe: Result.AddChild(EatToken);
	else
		begin
			Result.AddChild(EatToken(ptIdentifier));
			while CurrentToken.TokenKind = ptDot do
			begin
				Result.AddChild(EatToken);
				Result.AddChild(EatToken(ptIdentifier));
			end;
		end;
	end;
end;

function TDelphiParser.ParseAttributeArguments: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntArguments);

	Result.AddChild(EatToken(ptOpenParen));
	if CurrentTokenKind <> ptCloseParen then
	begin
		if PeekTokenKind = ptEquals then
			Result.AddChild(ParseNamedArgumentList)
		else
			Result.AddChild(ParsePositionalArgumentList);

		if CurrentToken.TokenKind = ptEquals then
			Result.AddChild(ParseNamedArgumentList)
	end;
	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParsePositionalArgumentList: TSyntaxNode2;
begin
	Result := ParsePositionalArgument;
	while CurrentToken.TokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParsePositionalArgument);
	end;
end;

function TDelphiParser.ParsePositionalArgument: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntPositionalArgument);
	Result.AddChild(ParseAttributeArgumentExpression);
end;

function TDelphiParser.ParseNamedArgumentList: TSyntaxNode2;
begin
	Result := ParseNamedArgument;
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseNamedArgument);
	end;
end;

function TDelphiParser.ParseNamedArgument: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntNamedArgument);
	Result.AddChild(ParseAttributeArgumentName);
	Result.AddChild(EatToken(ptEquals));
	Result.AddChild(ParseAttributeArgumentExpression);
end;

function TDelphiParser.ParseAttributeArgumentExpression: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntValue);
	Result.AddChild(ParseExpression);
end;

function TDelphiParser.get_CurrentToken: TSyntaxToken;
begin
	if FCurrent < 0 then
		Result := TSyntaxToken.Eof
	else if FCurrent >= FTokens.Count then
		Result := TSyntaxToken.Eof
	else
		Result := FTokens[FCurrent] as TSyntaxToken;
end;

function TDelphiParser.ParseAssemblyAttribute: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#AssemblyAttribute

AssemblyAttribute
	-> '[' ASSEMBLY ':' Expression ']'
}
	Result := TSyntaxNode2.Create(ntAttribute);

//	'[' ASSEMBLY ':' Expression ']'
	Result.AddChild(EatToken(ptOpenBracket));
	Result.AddChild(EatTokenEx(ptAssembly));
	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseExpression);
	Result.AddChild(EatToken(ptCloseBracket));
end;

{ TSyntaxNode2 }

procedure TSyntaxNode2.AddChild(ChildNode: TSyntaxNode2);
var
	wrapper: TSyntaxNodeOrToken;
begin
	TConstraints.NotNull(ChildNode, '[TSyntaxNode2.AddChild] ChildNode cannot be nil');

	wrapper := TSyntaxNodeOrToken.Create(ChildNode);
	FChildNodes.Add(wrapper);
end;

procedure TSyntaxNode2.AddChild(ChildNode: TSyntaxToken);
var
	node: TSyntaxNodeOrToken;
begin
	TConstraints.NotNull(ChildNode, 'ChildNode');

	node := TSyntaxNodeOrToken.Create(ChildNode);
	FChildNodes.Add(node);
end;

constructor TSyntaxNode2.Create(SyntaxNodeType: TSyntaxNodeType);
begin
	inherited Create;

	FNodeType := SyntaxNodeType;
	FAttributes := TDictionary<TAttributeName, string>.Create;
	FChildNodes := TObjectList<TSyntaxNodeOrToken>.Create(True);
end;

destructor TSyntaxNode2.Destroy;
begin
	FNodeType := ntUnknown;
	FreeAndNil(FAttributes);
	FreeAndNil(FChildNodes);
	FCurrentColumn := -1;
	FCurrentLine := -1;
	FFilename := '';

	inherited;
end;

class function TSyntaxNode2.DumpTree(Node: TSyntaxNode2): string;

const
	INCLUDE_TOKENS: Boolean = False;

	function DumpNode(const Node: TSyntaxNode2; Prefix: UnicodeString): UnicodeString;
	var
		sChild: UnicodeString;
		i: Integer;
		child: TSyntaxNodeOrToken;
		isLast: Boolean;
	const
		// Ansi
//		middlePrefix: string = #$251C#$2500#$2500+' '; // '|--- '
//		finalPrefix:  string = #$2570#$2500#$2500+' '; // '\--- '
//		nestedPrefix: string = #$2502'   ';            // '|    '

		// Unicode
		middlePrefix: string = #$251C#$2500' '; // '|- '
		finalPrefix:  string = #$2570#$2500' '; // '\- '
		nestedPrefix: string = #$2502'  ';      // '|  '
	begin
		if (Node = nil) then
		begin
			Result := '';
			Exit;
		end;

		Result := Node.DisplayName;

		if Length(Prefix) > 100 then
			Exit; // Stackoverflow prevention


		for i := 0 to Node.ChildNodes.Count-1 do
		begin
			isLast := (i = Node.ChildNodes.Count-1);

			child := Node.ChildNodes[i];

			if child.IsToken and (not INCLUDE_TOKENS) then
				Continue;

			Result := Result+#13#10+
					prefix;
			if not isLast then
			begin
				Result := Result + middlePrefix;
				if child.IsNode then
					sChild := DumpNode(child.AsNode, prefix+nestedPrefix)
				else
					sChild := child.AsToken.ToString;
			end
			else
			begin
				Result := Result + finalPrefix;
				if child.IsNode then
					sChild := DumpNode(child.AsNode, prefix+'   ')
				else
					sChild := child.AsToken.ToString;
			end;

			Result := Result+sChild;
		end;
	end;

begin
	Result := DumpNode(Node, '');
end;

function TSyntaxNode2.get_HasChildren: Boolean;
begin
	Result := (FChildNodes.Count > 0);
end;

function TSyntaxNode2.get_Value: string;
begin
	Result := Self.Attributes[anName];
end;

function TSyntaxNode2.get_DisplayName: string;

	function SyntaxNodeAttributesToStr(Node: TSyntaxNode2): string;
	var
		attr: DelphiParser.TAttributeName;
		s: string;
	begin
	{
		Return all attributes of a node
	}
		Result := '';

		for attr := Low(DelphiParser.TAttributeName) to High(DelphiParser.TAttributeName) do
		begin
			s := Node.Attributes[attr];
			if s = '' then
				Continue;

			if Result <> '' then
				Result := Result+' ';

			Result := Result + AttributeNameToStr(attr)+'="'+s+'"';
		end;
	end;

begin
	Result := SyntaxNodeTypeToStr(Self.NodeType)+'('+SyntaxNodeAttributesToStr(Self)+')';
end;

function TSyntaxNode2.get_Attributes(Attribute: TAttributeName): string;
begin
	TConstraints.NotNull(FAttributes, 'FAttributes');

	if not FAttributes.TryGetValue(Attribute, {var}Result) then
		Result := '';
end;

procedure TSyntaxNode2.set_Attributes(Attribute: TAttributeName; const Value: string);
begin
	FAttributes.AddOrSetValue(Attribute, Value);
end;

function TSyntaxNode2.ToString: string;
begin
	Result := Format('%s %s, "%s"', [
         Self.ClassName,
         SyntaxNodeTypeToStr(Self.NodeType),
         Self.Value]);
end;

{ EParserException }

constructor EParserException.Create(X, Y: Integer; Filename, Msg: string);
begin
	inherited Create(Msg);
	FPosXY.X := X;
	FPosXY.Y := Y;
	FFilename := Filename;
end;

{ TDelphiParser - Fluent parsing helpers }

function TDelphiParser.PeekToken: TSyntaxToken;
begin
	// So much nicer to have "PeekToken" without the parans everywhere. I know, i know.
	Result := Self.PeekToken(1);
end;

function TDelphiParser.PeekToken(n: Integer): TSyntaxToken;
var
	index: Integer;
begin
	index := FCurrent+n;
	if index < 0 then
		index := 0;

	if index >= FTokens.Count then
		index := FTokens.Count-1; //keep returning final eof token

	Result := FTokens[index];
end;

function TDelphiParser.ConsumeKeyword(ExpectedKind: TptTokenKind): TSyntaxNode2;
var
	consumedToken: TSyntaxToken;
begin
//	Result := ConsumeToken(keyword, ntIdentifier); // or ntKeyword if you prefer

	if CurrentToken.TokenKind = expectedKind then
		Result := TSyntaxNode2.Create(ntIdentifier)
	else
		Result := SynErrorFmt(SExpected, [TokenName(expectedKind), TokenName(CurrentToken.TokenKind)]);

	consumedToken := EatToken(expectedKind);
	Result.AddChild(consumedToken);
	Result[anName] := consumedToken.ValueText;
	Result.FCurrentLine := consumedToken.Line;
	Result.FCurrentColumn := consumedToken.Column;
//	Result.FFilename := CurrentToken.Filename;   wishful thinking
end;

{---------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwSimplePasPar.pas, released November 14, 1999.

The Initial Developer of the Original Code is Martin Waldenburg
(Martin.Waldenburg@T-Online.de).
Portions created by Martin Waldenburg are Copyright (C) 1998, 1999 Martin
Waldenburg.
All Rights Reserved.
Portions CopyRight by Robert Zierer.

Contributor(s):  Vladimir Churbanov, Dean Hill, James Jacobson, LaKraven Studios Ltd, Roman Yankovsky
(This list is ALPHABETICAL)

Last Modified: 2014/09/14
Current Version: 1.10

Notes: This program is an early beginning of a Pascal parser.
I'd like to invite the Delphi community to develop it further and to create
a fully featured Object Pascal parser.

Modification history:

LaKraven Studios Ltd, January 2015:

- Cleaned up version-specifics up to XE8
- Fixed all warnings & hints

Jacob Thurman between 20040301 and 20020401

Made ready for Delphi 8:

Added new directives and keywords: static, sealed, final, operator, unsafe.

Added parsing for custom attributes (based on ECMA C# specification).

Added support for nested types in class declarations.

Jeff Rafter between 20020116 and 20020302

Added AncestorId and AncestorIdList back in, but now treat them as Qualified
Identifiers per Daniel Rolf's fix. The separation from QualifiedIdentifierList
is need for descendent classes.

Added ParseVarName and ParseVarNameList back in for descendent classes, fixed to correctly
use Identifiers as in Daniel's verison

Removed fInJunk flags (they were never used, only set)

Pruned uses clause to remove windows dependency. This required changing
"TPoint" to "TTokenPoint". TTokenPoint was declared in mwPasLexTypes

Daniel Rolf between 20010723 and 20020116

Made ready for Delphi 6

ciClassClass for "class function" etc.
ciClassTypeEnd marks end of a class declaration (I needed that for the delphi-objectif-connector)
ciEnumeratedTypeItem for items of enumerations
ciDirectiveXXX for the platform, deprecated, varargs, local
ciForwardDeclaration for "forward" (until now it has been read but no event)
ciIndexSpecifier for properties
ciObjectTypeEnd marks end of an object declaration
ciObjectProperty property for objects
ciObjectPropertySpecifiers property for objects
ciPropertyDefault marking default of property
ciDispIDSpecifier for dispid

patched some functions for implementing the above things and patching the following bugs/improv.:

ObjectProperty handling overriden properties
ProgramFile, UnitFile getting Identifier instead of dropping it
ParseInterfaceHeritage: Qualified identifiers
bugs in variant records
typedconstant failed with complex set constants. simple patch using ParseConstantExpression

German localization for the two string constants. Define GERMAN for german string constants.

Greg Chapman on 20010522
Better handling of defaut array property
Separate handling of X and Y in property Pixels[X, Y: Integer through identifier "event"
corrected spelling of "ParseForwardDeclaration"

James Jacobson on 20010223
semi colon before finalization fix

James Jacobson on 20010223
ParseRecordConstant Fix

Martin waldenburg on 2000107
Even Faster lexer implementation !!!!

James Jacobson on 20010107
  Improper handling of the construct
      property TheName: Integer read FTheRecord.One.Two; (stop at second point)
      where one and two are "qualifiable" structures.

James Jacobson on 20001221
   Stops at the second const.
   property Anchor[const Section: string; const Ident:string]: string read
   changed TDelphiParser.PropertyParameterList

On behalf of  Martin Waldenburg and James Jacobson
 Correction in array property Handling (Matin and James) 07/12/2000
 Use of ExId instead of TokenId in ExportsElements (James) 07/12/2000
 Reverting to old behavior in Statementlist [PtintegerConst put back in] (James) 07/12/2000

Xavier Masson InnerCircleProject : XM : 08/11/2000
  Integration of the new version delivered by Martin Waldenburg with the modification I made described just below

Xavier Masson InnerCircleProject : XM : 07/15/2000
  Added "states/events " for      spaces( SkipSpace;) CRLFco (SkipCRLFco) and
    CRLF (SkipCRLF) this way the parser can give a complete view on code allowing
    "perfect" code reconstruction.
    (I fully now that this is not what a standard parser will do but I think it is more usefull this way ;) )
    go to www.innercircleproject.com for more explanations or express your critisism ;)

previous modifications not logged sorry ;)

Known Issues:
-----------------------------------------------------------------------------}
{----------------------------------------------------------------------------
 Last Modified: 05/22/2001
 Current Version: 1.1
 official version
   Maintained by InnerCircle

   http://www.innercircleproject.org

 02/07/2001
   added property handling in Object types
   changed handling of forward declarations in ParseMethodHeading method
-----------------------------------------------------------------------------}


{ TSyntaxNodeOrToken }

function TSyntaxNodeOrToken.get_AsNode: TSyntaxNode2;
begin
	Result := FObject as TSyntaxNode2;
end;

function TSyntaxNodeOrToken.get_AsToken: TSyntaxToken;
begin
	Result := FObject as TSyntaxToken;
end;

constructor TSyntaxNodeOrToken.Create(SyntaxNode: TSyntaxNode2);
begin
	inherited Create;

	if not Assigned(SyntaxNode) then
		raise EArgumentNullException.Create('SyntaxNode');

	FObject := SyntaxNode;
end;

constructor TSyntaxNodeOrToken.Create(SyntaxToken: TSyntaxToken);
begin
	inherited Create;

	if not Assigned(SyntaxToken) then
		raise EArgumentNullException.Create('SyntaxToken');

	FObject := SyntaxToken;
end;

destructor TSyntaxNodeOrToken.Destroy;
begin
	FreeAndNil(FObject);

	inherited;
end;

function TSyntaxNodeOrToken.get_IsNode: Boolean;
begin
	Result := (FObject is TSyntaxNode2);
end;

function TSyntaxNodeOrToken.get_IsToken: Boolean;
begin
	Result := (FObject is TSyntaxToken);
end;

function SyntaxNodeTypeToStr(NodeType: TSyntaxNodeType): string;
begin
	Result := TypInfo.GetEnumName(TypeInfo(TSyntaxNodeType), Ord(NodeType));
end;

function AttributeNameToStr(AttributeName: TAttributeName): string;
begin
	Result := TypInfo.GetEnumName(TypeInfo(TAttributeName), Ord(AttributeName));
end;

function AttributeValueToStr(const AttributeValue: TAttributeValue): string;
{const
	SAttributeValue: array[TAttributeValue] of string = (
			'atAsm',
			'atTrue',
			'atFunction',
			'atProcedure',
			'atClassOf',
			'atClass',
			'atConst',
			'atConstructor',
			'atDestructor',
			'atEnum',
			'atInterface',
			'atNil',
			'atNumeric',
			'atOut',
			'atPointer',
			'atName',
			'atString',
			'atSubRange',
			'atVar',
			'atDispInterface');}
begin
	Result := TypInfo.GetEnumName(TypeInfo(TAttributeValue), Ord(AttributeValue));
end;


end.
