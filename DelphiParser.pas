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
		ntAdd,
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
		ntBounds,
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
		ntDimension,
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
		ntOr,
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
		ntSub,
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
		ntXor,
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
			atAsm,
			atTrue,
			atFunction,
			atProcedure,
			atClassOf,
			atClass,
			atConst,
			atConstructor,
			atDestructor,
			atEnum,
			atInterface,
			atNil,
			atNumeric,
			atOut,
			atPointer,
			atName,
			atString,
			atSubRange,
			atVar,
			atDispInterface);

function AttributeValueToString(const AttributeValue: TAttributeValue): string;


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

		property IsNode: Boolean			read get_IsNode;
		property IsToken: Boolean			read get_IsToken;
		property AsNode: TSyntaxNode2		read get_AsNode;
		property AsToken: TSyntaxToken	read get_AsToken;
	end;


	// It's called TSyntaxNode2 because TSyntaxNode is already taken by DelphiAST
	TSyntaxNode2 = class
	private
		FNodeType: TSyntaxNodeType;
//		FValue: string;
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
	protected
		// Not used
		/// find a child node with the specified node type
		function FindNode(NodeType: TSyntaxNodeType): TSyntaxNode2; overload; // Warning: can return nil
		/// find a child node based on matching a patch of node types
		function FindNode(const TypesPath: array of TSyntaxNodeType): TSyntaxNode2; overload;	// Warning: can return nil

		function Clone: TSyntaxNode2; virtual;

		procedure AssignPositionFrom(const Node: TSyntaxNode2);

		procedure DeleteChild( const NodeOrToken: TSyntaxNodeOrToken);
		procedure ExtractChild(const NodeOrToken: TSyntaxNodeOrToken);
	public
		constructor Create(SyntaxNodeType: TSyntaxNodeType);
		destructor Destroy; override;

		function ToString: string; override;

		class function DumpTree(Node: TSyntaxNode2): string;

		property NodeType: TSyntaxNodeType read FNodeType;
		property Value: string read get_Value;

		property Attributes[Attribute: TAttributeName]: string read get_Attributes write set_Attributes; default;

		property ChildNodes: TObjectList<TSyntaxNodeOrToken> read FChildNodes;

		property DisplayName: string read get_DisplayName;
		property HasChildren: Boolean read get_HasChildren; // if ChildNodes contains items
		property Line: Integer read FCurrentLine;
		property Column: Integer read FCurrentColumn;
	end;

{
	The root class that does the parsing.

	TObject
		- SimpleParser.TDelphiParser
			- DelphiAST.SimpleParserEx.TmwSimplePasParEx		(derived)
				- DelphiAST.TPasSyntaxTreeBuilder				(derived)
}
	TDelphiParser = class
	private type
		TTreeBuilderMethod = function: TSyntaxNode2 of object;
	private
		// Infrastructure
		FTokens: TObjectList<TSyntaxToken>; // owns list of tokens
		FCurrent: Integer; //current token index
//		FAheadOriginalCurrentIndex: Integer; // the Current index we were at before we started parsing ahead

//		FCurrentToken: TSyntaxToken;

		FCompilerDirectives: TStrings;
		FLastProgressIndex: Integer;

		FOnMessage: TMessageEvent;


		FInterfaceOnly: Boolean;
//		AheadParse: TDelphiParser;
		FInRound: Integer; // incremented by RoundOpen, decremented by RoundClose
		function VariableTail: TSyntaxNode2;
		function get_InRound: Boolean;
		function get_UseDefines: Boolean;  // get_UseDefines
		function get_ScopedEnums: Boolean;
		procedure set_UseDefines(const Value: Boolean);
		procedure set_IncludeHandler(IncludeHandler: IUnknown{IIncludeHandler});

		procedure DoMessage(const Msg: string; X, Y: Integer);


		procedure MoveMembersToVisibilityNodes(TypeNode: TSyntaxNode2);
		procedure BuildExpressionTree(ExpressionMethod: TTreeBuilderMethod);

		procedure GetDefaultConditionalDirectives(TargetList: TStrings);


		//procedure InitAhead;			// Save current position in tokens array (so we can restore it later after parsing ahead)
		//procedure RestoreAhead;		// Restore the current position in the tokens array (after we're done parsing ahead)

		function PoisonNode: TSyntaxNode2; //special NotImplemented node

		function ClassProperty: TSyntaxNode2;

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

	protected
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
		procedure ExpectedFatal(Sym: TptTokenKind); 			// throws an exception if the CurrentToken kind is not Sym.
		function ExpectedEx(    ExpectedTokenKind: TptTokenKind): TSyntaxToken;	// emits an error message if the CurrentToken ExID is not Sym.

		// Output an error message
		function SynError(Error: string): TSyntaxNode2;
		function SynErrorFmt(const Error: string; const Args: array of const): TSyntaxNode2;

		procedure EnsureProgress;

		// ***********************************************************************
		// Start of the production methods
		// ***********************************************************************

		// Main parsing function.
		function ParseCore: TSyntaxNode2; virtual; //as a nice way to split plumbing from grammer

		// ParseCore then turns around and calls one of:
		function ParseUnitDeclaration: TSyntaxNode2;	// e.g. unit Unit1;
		function ParseProgramFile: TSyntaxNode2;			// e.g. program Program1;
		function ParseLibraryFile: TSyntaxNode2;			// e.g. library Library1;
		function ParsePackageFile: TSyntaxNode2;			// e.g. package Package1;
		function ParseScriptFile: TSyntaxNode2; 			// If the text is not one of file types, we can parse some contents


		function ParseUsesClause: TSyntaxNode2;			// uses a,b,c;
			function IsPossibleUsesClause: Boolean;

		function UsedUnitsList: TSyntaxNode2;		deprecated 'Moved to ParseUsesClause'; // a,b,c
		function ParseUsedUnit: TSyntaxNode2;			// a

		function CustomAttribute: TSyntaxNode2;				// ptSquareOpen
		function Semicolon: TSyntaxToken;						// consumes a semicolon token, but only in places where it's valid

		function ParseQualifiedIdentifier: TSyntaxNode2;   // Contoso.Grunion;
		function ParseInterfaceSection: TSyntaxNode2;
		function ParseInterfaceDeclaration: TSyntaxNode2;

		function ParsePortabilityDirective: TSyntaxNode2;  // ntPortabilityDirective
			function IsPossiblePortabilityDirective: Boolean;

		function ParseStringType: TSyntaxNode2;				// ntType

		function ParseTypeSection: TSyntaxNode2;				// ntTypeSection
			function IsPossibleTypeSection: Boolean;

		function ParseTypeDeclaration: TSyntaxNode2;			// ntTypeDecl
			function IsPossibleTypeDeclaration: Boolean; 	// peeks ahead to see if the next production is a TypeDeclaration, meaning you can call ParseTypeDeclaration()

		function ParseType: TSyntaxNode2;		// ntType

		// ntType, TFoo = <Type>
		function ParseEnumeratedType: TSyntaxNode2;
		//ParseExpressionOrRange
		//ParseArrayType
		//ParseSetType
		//ParseFileType
		//ParseRecordHelperType
		function ParseClassType: TSyntaxNode2;			// ntType(@anType=atClass)
		function ParseAncestorList: TSyntaxNode2;		// TWidget = class(TShape, IShape, IWidget)
		function ParseClassMemberList: TSyntaxNode2;	// the ancestor of a class TCustomer=class(TShape, ILog, IFoo)
		function ParseVisibilitySection: TSyntaxNode2;
		function ParseVisibilitySectionContent: TSyntaxNode2;
			function IsPossibleVisibilitySectionContent: Boolean;

		function ParseConstSection: TSyntaxNode2;					// ntConstants
			function IsPossibleConstSection: Boolean;
		function ParseConstantDecl: TSyntaxNode2;					// ntConstant
			function IsPossibleConstantDecl: Boolean;


		function ParseResStringSection: TSyntaxNode2;			// ntResourceStrings
			function IsPossibleResStringSection: Boolean;
		function ParseResourceStringDecl: TSyntaxNode2;
			function IsPossibleResourceStringDecl: Boolean;

		function ParseMethodOrProperty: TSyntaxNode2;
		function ParseFieldDecl: TSyntaxNode2;


		function ParseClassHelper: TSyntaxNode2;			// ntHelper
		function ParseSimpleType: TSyntaxNode2;
		function ParseSubrangeType: TSyntaxNode2;
		function ParseStructuredType: TSyntaxNode2;
		function ParseProceduralType: TSyntaxNode2;
		function ParseSimpleExpression: TSyntaxNode2;
		function ParseTypeId: TSyntaxNode2;
		function ParsePointerType: TSyntaxNode2;

		function ParseVarSection: TSyntaxNode2;
			function IsPossibleVarSection: Boolean;

		function ParseImplementationSection: TSyntaxNode2;

		function ParseInitializationSection: TSyntaxNode2;
		function ParseFinalizationSection: TSyntaxNode2;

		function AnonymousMethodType: TSyntaxNode2;
		function ArrayType: TSyntaxNode2;
		function Block: TSyntaxNode2;
		function CharString: TSyntaxNode2;
		procedure ClassMethodDirective(ParentNode: TSyntaxNode2);
		function ClassMethodOrProperty: TSyntaxNode2;
		procedure ClassOperatorHeading(ParentNode: TSyntaxNode2);
		function ClassTypeEnd: TSyntaxNode2;
		function ClassVisibility: TSyntaxNode2;
		function ConstantColon: TSyntaxNode2;
		function ConstantEqual: TSyntaxNode2;

		function ConstantExpression: TSyntaxNode2;
		function ConstantType: TSyntaxNode2;
		procedure ConstructorHeading(ParentNode: TSyntaxNode2);

		function ParseImplementationDecl: TSyntaxNode2;
			function IsPossibleImplementationDecl: Boolean;

		function Designator: TSyntaxNode2;
		procedure DestructorHeading(ParentNode: TSyntaxNode2);
		function Directive16Bit: TSyntaxNode2;

		// Involved in tree building
		function AccessSpecifier: TSyntaxNode2;
		function AdditiveOperator: TSyntaxNode2;
		function AddressOp: TSyntaxNode2;
		function AlignmentParameter: TSyntaxNode2;
		function AnonymousMethod: TSyntaxNode2;
		function ArrayBounds: TSyntaxNode2;
		function ArrayConstant: TSyntaxNode2;
		function ArrayDimension: TSyntaxNode2;
		function AsmStatement: TSyntaxNode2;
		function AsOp: TSyntaxNode2;
		function AssignOp: TSyntaxNode2;
		function AtExpression: TSyntaxNode2; // raise at
		function CaseElseStatement: TSyntaxNode2;
		function CaseLabel: TSyntaxNode2;
		function CaseLabelList: TSyntaxNode2;
		function CaseSelector: TSyntaxNode2;
		function CaseStatement: TSyntaxNode2;
		function ClassClass: TSyntaxNode2;
		function ClassConstraint: TSyntaxNode2;
		function ClassField: TSyntaxNode2;
		procedure ClassForward(ParentNode: TSyntaxNode2);
		procedure ClassFunctionHeading(ParentNode: TSyntaxNode2);
		procedure ClassMethod(ParentNode: TSyntaxNode2);
		function ClassMethodResolution: TSyntaxNode2;
		function ClassMethodHeading: TSyntaxNode2;
		procedure ClassProcedureHeading(ParentNode: TSyntaxNode2);

		function ParseProperty: TSyntaxNode2;
			function IsPossibleProperty: Boolean;


		function ClassReferenceType: TSyntaxNode2;
		function ConstParameter: TSyntaxNode2;
		function ConstantDeclaration: TSyntaxNode2;
		function ParseConstantName: TSyntaxNode2;

		function ConstantValue: TSyntaxNode2;
		function ConstantValueTyped: TSyntaxNode2;
		function ConstructorConstraint: TSyntaxNode2;
		procedure ConstructorName(ParentNode: TSyntaxNode2);
		function ContainsClause: TSyntaxNode2;
		procedure DestructorName(ParentNode: TSyntaxNode2);
		procedure DirectiveBinding(ParentNode: TSyntaxNode2);
		function DirectiveBindingMessage: TSyntaxNode2;
		procedure DirectiveCalling(ParentNode: TSyntaxNode2);
		procedure DirectiveInline(ParentNode: TSyntaxNode2);
		procedure DispInterfaceForward(ParentNode: TSyntaxNode2);
		function DotOp: TSyntaxNode2;
		function ElseStatement: TSyntaxNode2;
		function EmptyStatement: TSyntaxNode2;
		function ExceptBlock: TSyntaxNode2;
		function ExceptionBlockElseBranch: TSyntaxNode2;
		function ExceptionHandler: TSyntaxNode2;
		function ExceptionVariable: TSyntaxNode2;
		function ParseMethodHeading: TSyntaxNode2;
			function IsPossibleMethodHeading: Boolean;

		function ExportsClause: TSyntaxNode2;
		function ExportsElement: TSyntaxNode2;
		function ExportsName: TSyntaxNode2;
		function ExportsNameId: TSyntaxNode2;
		function ParseExpression: TSyntaxNode2;
		function ExpressionList: TSyntaxNode2;

		function ExternalDirective: TSyntaxNode2; // ptExternal
		procedure ExternalDirectiveTwo(ParentNode: TSyntaxNode2);
		function ExternalDirectiveThree: TSyntaxNode2;

		function FieldName: TSyntaxNode2;
		function FinallyBlock: TSyntaxNode2;
		function FormalParameterList: TSyntaxNode2;
		function ForStatement: TSyntaxNode2;
		function ForStatementDownTo: TSyntaxNode2;
		function ForStatementFrom: TSyntaxNode2;
		function ForStatementIn: TSyntaxNode2;
		function ForStatementTo: TSyntaxNode2;
		procedure FunctionHeading(ParentNode: TSyntaxNode2);
		function FunctionMethodName: TSyntaxNode2;
		function ForwardDeclaration: TSyntaxNode2;
		function FunctionMethodDeclaration: TSyntaxNode2;
		procedure FunctionProcedureBlock(ParentNode: TSyntaxNode2);
		function ParseIdentifierList: TSyntaxNode2;
		function InlineConstSection: TSyntaxNode2;
		function InlineStatement: TSyntaxNode2;
		function InParameter: TSyntaxNode2;
		function InterfaceHeritage: TSyntaxNode2;
		procedure InterfaceMemberList(ParentNode: TSyntaxNode2);
		function LabelDeclarationSection: TSyntaxNode2;
		function LabeledStatement: TSyntaxNode2;

		function MainUsedUnitExpression: TSyntaxNode2;
		function MainUsedUnitName: TSyntaxNode2;
		function FormalParameterType: TSyntaxNode2;
		procedure ObjectConstructorHeading(ParentNode: TSyntaxNode2);
		procedure ObjectDestructorHeading(ParentNode: TSyntaxNode2);
		function ObjectField: TSyntaxNode2;
		function ObjectForward: TSyntaxNode2;
		procedure ObjectFunctionHeading(ParentNode: TSyntaxNode2);
		function ObjectHeritage: TSyntaxNode2;
		procedure ObjectMemberList(ParentNode: TSyntaxNode2);
		procedure ObjectMethodDirective(ParentNode: TSyntaxNode2);
		procedure ObjectMethodHeading(ParentNode: TSyntaxNode2);
		function ParsePropertyDirective: TSyntaxNode2;
		procedure ObjectProcedureHeading(ParentNode: TSyntaxNode2);
		procedure ObjectType(ParentNode: TSyntaxNode2);
		function ObjectTypeEnd: TSyntaxNode2;
		function ObjectVisibility: TSyntaxNode2;
		function OrdinalIdentifier: TSyntaxNode2;
		function ParseOrdinalType: TSyntaxNode2;
		function ParameterNameList: TSyntaxNode2;
		procedure ProceduralDirective(ParentNode: TSyntaxNode2);
		function ProceduralDirectiveOf: TSyntaxNode2;
		function ProcedureMethodName: TSyntaxNode2;
		function ProgramBlock: TSyntaxNode2;
		function PropertyDefault: TSyntaxNode2;
		function PropertyInterface: TSyntaxNode2;
		function PropertySpecifiers: TSyntaxNode2;

		function ReadAccessIdentifier: TSyntaxNode2;
		function RealIdentifier: TSyntaxNode2;
		function ParseRealType: TSyntaxNode2;
		function RecordAlign: TSyntaxNode2;
		function RecordConstant: TSyntaxNode2;
		function RecordVariant: TSyntaxNode2;
		function ResolutionInterfaceName: TSyntaxNode2;
		function SetType: TSyntaxNode2;
		function Statement: TSyntaxNode2;
		function StatementOrExpression: TSyntaxNode2;
		function Statements: TSyntaxNode2;
		function StorageExpression: TSyntaxNode2;
		function StorageIdentifier: TSyntaxNode2;
		function StorageNoDefault: TSyntaxNode2;
		function StorageSpecifier: TSyntaxNode2;
		function StorageStored: TSyntaxNode2;

		function DirectiveDeprecated: TSyntaxNode2;
		function DirectiveLibrary: TSyntaxNode2;
		function DirectiveLocal: TSyntaxNode2;
		function DirectivePlatform: TSyntaxNode2;
		function DirectiveVarargs: TSyntaxNode2;
		function DispIDSpecifier: TSyntaxNode2;
		function EnumeratedTypeItem: TSyntaxNode2;
		function ExceptionClassTypeIdentifier: TSyntaxNode2;
		function ExceptionHandlerList: TSyntaxNode2;
		function ExceptionIdentifier: TSyntaxNode2;
		function ExplicitType: TSyntaxNode2;
		function Factor: TSyntaxNode2;
		function FieldDeclaration: TSyntaxNode2;
		function FieldList: TSyntaxNode2;
		function FieldNameList: TSyntaxNode2;
		function FileType: TSyntaxNode2;
		function FormalParameterSection: TSyntaxNode2;
		function FunctionProcedureName: TSyntaxNode2;
		function GotoStatement: TSyntaxNode2;
		function IfStatement: TSyntaxNode2;

		function ParseIdentifier: TSyntaxNode2;				// ntIdentifier
			function IsPossibleIdentifier: Boolean;

		function ImplementsSpecifier: TSyntaxNode2;
		function ParseIndexSpecifier: TSyntaxNode2;
		function IndexOp: TSyntaxNode2;
		function InheritedStatement: TSyntaxNode2;
		function InheritedVariableReference: TSyntaxNode2;
		function InlineVarDeclaration: TSyntaxNode2;
		function InlineVarSection: TSyntaxNode2;
		procedure InterfaceForward(ParentNode: TSyntaxNode2);
		function InterfaceGUID: TSyntaxNode2;
		function ParseInterfaceType: TSyntaxNode2;

		function LabelId: TSyntaxNode2;
			function IsPossibleLabelID: Boolean;

		function MainUsesClause: TSyntaxNode2;
		function MainUsedUnitStatement: TSyntaxNode2;
		function MethodKind: TSyntaxNode2;
		function MultiplicativeOperator: TSyntaxNode2;
		function NotOp: TSyntaxNode2;
		function NilToken: TSyntaxNode2;
		function Number: TSyntaxNode2;
		function ObjectNameOfMethod: TSyntaxNode2;
		function OutParameter: TSyntaxNode2;
		function ParameterFormal: TSyntaxNode2;
		function ParameterName: TSyntaxNode2;
		function PointerSymbol: TSyntaxNode2;
		procedure ProcedureHeading(ParentNode: TSyntaxNode2);
		function ProcedureDeclarationSection: TSyntaxNode2;
		function ProcedureProcedureName: TSyntaxNode2;
		procedure PropertyName(ParentNode: TSyntaxNode2);
		function PropertyParameterList: TSyntaxNode2; // not done
		function RaiseStatement: TSyntaxNode2;
		procedure RecordAlignValue(ParentNode: TSyntaxNode2);
		function RecordConstraint: TSyntaxNode2;
		function RecordFieldConstant: TSyntaxNode2;
		procedure RecordType(ParentNode: TSyntaxNode2);

		function RelativeOperator: TSyntaxNode2;
		function RepeatStatement: TSyntaxNode2;
		function ResourceDeclaration: TSyntaxNode2;
		function ResourceValue: TSyntaxNode2;
		function RequiresClause: TSyntaxNode2;
		function RequiresIdentifier: TSyntaxNode2;
		function RequiresIdentifierId: TSyntaxNode2;
		function ReturnType: TSyntaxNode2;
		function RoundClose: TSyntaxNode2;
		function RoundOpen: TSyntaxNode2;
		function SetConstructor: TSyntaxNode2;
		function SetElement: TSyntaxNode2;
		function SimpleStatement: TSyntaxNode2;
		function StatementList: TSyntaxNode2;
		function StorageDefault: TSyntaxNode2;
		function StringConst: TSyntaxNode2;
		function StringConstSimple: TSyntaxNode2;
		function StringStatement: TSyntaxNode2;
		function ThenStatement: TSyntaxNode2;
		function TryStatement: TSyntaxNode2;

		function StringIdentifier: TSyntaxNode2;
		function StringType: TSyntaxNode2;
		function TagField: TSyntaxNode2;
		function TagFieldName: TSyntaxNode2;
		function TagFieldTypeName: TSyntaxNode2;
		function ParseTerm: TSyntaxNode2;
		function ParseTypedConstant: TSyntaxNode2;
		function TypeReferenceType: TSyntaxNode2;
		function TypeSimple: TSyntaxNode2;

		function UnaryMinus: TSyntaxNode2;

		function UnitId: TSyntaxNode2;

		function VarAbsolute: TSyntaxNode2;
		function VarEqual: TSyntaxNode2;
		function VarDeclaration: TSyntaxNode2;
		function Variable: TSyntaxNode2;
		function ParseVariableReference: TSyntaxNode2;
		function VariantIdentifier: TSyntaxNode2;
		function VariantSection: TSyntaxNode2;
		function VarParameter: TSyntaxNode2;
		function VarName: TSyntaxNode2;
		function VarNameList: TSyntaxNode2;

		function VisibilityAutomated: TSyntaxNode2;
		function VisibilityPrivate: TSyntaxNode2;
		function VisibilityProtected: TSyntaxNode2;
		function VisibilityPublic: TSyntaxNode2;
		function VisibilityPublished: TSyntaxNode2;
		function VisibilityStrictPrivate: TSyntaxNode2;
		function VisibilityStrictProtected: TSyntaxNode2;
		function VisibilityUnknown: TSyntaxNode2;
		function WhileStatement: TSyntaxNode2;
		function WithExpressionList: TSyntaxNode2;
		function WithStatement: TSyntaxNode2;
		function WriteAccessIdentifier: TSyntaxNode2;

		// Generics
		function TypeArgs: TSyntaxNode2;
		function HintDirectives: TSyntaxNode2;
		function ParseTypeParams: TSyntaxNode2;
		function ParseTypeParam: TSyntaxNode2;						// T, U: class, constructor; V: IComparable<T>>= class
			function ParseConstraintTypeRef: TSyntaxNode2;		// IComparable<T>>= class
			function ParseTypeParamConstraint: TSyntaxNode2;

		function TypeParamList: TSyntaxNode2;

		function ConstraintList: TSyntaxNode2;
		function Constraint: TSyntaxNode2;
		//end generics


		// Attributes
		// ==========
		// This is the syntax for custom attributes, based quite strictly on the
		// ECMA syntax specifications for C#, but with a Delphi expression being
		// used at the bottom as opposed to a C# expression. -JThurman 2004-03-21
		function GlobalAttributes: TSyntaxNode2;
		function GlobalAttributeSections: TSyntaxNode2;
		function GlobalAttributeSection: TSyntaxNode2;
		function GlobalAttributeTargetSpecifier: TSyntaxNode2;
		function GlobalAttributeTarget: TSyntaxNode2;
		function Attributes: TSyntaxNode2;
		function AttributeSections: TSyntaxNode2;
		function AttributeSection: TSyntaxNode2;
		function AttributeTargetSpecifier: TSyntaxNode2;
		function AttributeTarget: TSyntaxNode2;
		function AttributeList: TSyntaxNode2;
		function Attribute: TSyntaxNode2;
		function AttributeName: TSyntaxNode2;
		function AttributeArguments: TSyntaxNode2;
		function PositionalArgumentList: TSyntaxNode2;
		function PositionalArgument: TSyntaxNode2;
		function NamedArgumentList: TSyntaxNode2;
		function NamedArgument: TSyntaxNode2;
		function AttributeArgumentName: TSyntaxNode2;
		function AttributeArgumentExpression: TSyntaxNode2;

		property CurrentToken:      TSyntaxToken read get_CurrentToken;		// read-only
		property CurrentTokenKind:  TptTokenKind read get_CurrentTokenKind;	// the TokenKind of the current token
		property CurrentTokenExID:  TptTokenKind read get_CurrentTokenExID;	// for an identifier token, contains the directive's TokenKind
		property CurrentTokenGenID: TptTokenKind read get_CurrentTokenGenID;	// ExID, or TokenID if ExID is empty

		property InRound: Boolean read get_InRound;

		// Token consumption helpers for whitespace-preserving parser
		function ConsumeToken(expectedKind: TptTokenKind; nodeType: TSyntaxNodeType=ntIdentifier): TSyntaxNode2;
		function ConsumeIdentifier: TSyntaxNode2;
		function ConsumeKeyword(keyword: TptTokenKind): TSyntaxNode2;
		function ConsumeSemicolon: TSyntaxNode2;
		
		// Fluent parsing helpers
		procedure AddToken(parent: TSyntaxNode2; token: TptTokenKind); //Consumes and preserves a token as a child node
		function Add(parent: TSyntaxNode2; child: TSyntaxNode2): TSyntaxNode2; //Adds a child node to parent and returns the child
	public
		constructor Create;
		destructor Destroy; override;

		function Parse(Tokens: TList): TSyntaxNode2;

		function ParseFile(FilePath: string): TSyntaxNode2;
		function ParseStream(SourceStream: ISequentialStream; FilePath: string; CodePage: Word): TSyntaxNode2;

		class function ParseText(Text: UnicodeString; FilePath: string): TSyntaxNode2;

		procedure InitDefinesDefinedByCompiler;

		property CompilerDirectives: TStrings read FCompilerDirectives;

		property InterfaceOnly: Boolean read FInterfaceOnly write FInterfaceOnly;
		property UseDefines: Boolean read get_UseDefines write set_UseDefines;
		property ScopedEnums: Boolean read get_ScopedEnums;
		property IncludeHandler: IUnknown{IIncludeHandler} write set_IncludeHandler;
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

function TDelphiParser.ForwardDeclaration: TSyntaxNode2;
var
  forwardToken, semicolonToken: TSyntaxNode2;
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
  semicolonToken := ConsumeSemicolon;
  if Assigned(semicolonToken) then
    Result.AddChild(semicolonToken);

  // Set semantic attribute for analysis tools
  Result.Attributes[anForwarded] := AttributeValueToString(atTrue);
end;

function TDelphiParser.ParseProperty: TSyntaxNode2;
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
	Tested by: basic_cases.dat/ParseProperty
}
	Result := TSyntaxNode2.Create(ntProperty);

	if CurrentTokenKind = ptClass then
	begin
		Result[anClass] := 'true';
		Result.AddChild(EatToken(ptClass));
	end;

	Result.AddChild(EatToken(ptProperty));

	// TODO: Implement the rest of this

end;

function TDelphiParser.ParsePropertyDirective: TSyntaxNode2;
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


	Tested by: basic_cases.dat/ParsePropertyDirective
}
	Result := TSyntaxNode2.Create(ntPropertySpecifiers);

	if (CurrentTokenKind = ptSemicolon) and (PeekTokenExID = ptDefault) then
	begin
//		-> ';' DEFAULT
		Result.AddChild(EatToken(ptSemiColon));
		Result.AddChild(EatToken(ptDefault));
	end
	else if (CurrentTokenKind = ptDefault) then
	begin
//		-> DEFAULT Expression
		Result.AddChild(EatToken(ptDefault));
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenKind = ptDispId) then
	begin
//		-> DISPID Expression
		Result.AddChild(EatToken(ptDispId));
		Result.AddChild(ParseExpression);
	end
	else if CurrentTokenExID = ptIndex then
	begin
//		-> INDEX Expression
		Result.AddChild(EatToken(ptIndex));
		Result.AddChild(ParseExpression);
	end
	else if CurrentTokenExID = ptNoDefault then
	begin
//		-> NODEFAULT
		Result.AddChild(EatToken(ptNoDefault));
	end
	else if CurrentTokenExID = ptRead then
	begin
//		-> READ Expression
		Result.AddChild(EatToken(ptRead));
		Result.AddChild(ParseExpression);
	end
	else if CurrentTokenExID = ptReadOnly then
	begin
//		-> READONLY
		Result.AddChild(EatToken(ptReadonly));
	end
	else if CurrentTokenExID = ptStored then
	begin
//		-> STORED Expression
		Result.AddChild(EatToken(ptStored));
		Result.AddChild(ParseExpression);
	end
	else if CurrentTokenExID = ptWrite then
	begin
//		-> WRITE Expression
		Result.AddChild(EatToken(ptWrite));
	end
	else if CurrentTokenExID = ptWriteOnly then
	begin
//		-> WRITEONLY
		Result.AddChild(EatToken(ptWriteOnly));
	end
	else
	begin
		// E2029 %s expected but %s found
		// E2029 Property directive expected but string constant found
		SynErrorFmt(SE2029, ['Property directive', CurrentToken.ToString]);
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
	Tested by: basic_cases.dat/ParseStream
}
	// Tokenize the input and put the tokens into our list
	tokens := TObjectList.Create(False);
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

	FTokens := TObjectList<TSyntaxToken>.Create(True); //owns the list of tokens
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

{next two check for ptNull and ExpectedFatal for an EOF Error}

function TDelphiParser.ExpectedEx(ExpectedTokenKind: TptTokenKind): TSyntaxToken;
begin
	// Expect the CurrentToken's ExID to be SymID
	if ExpectedTokenKind <> CurrentTokenExID then
	begin
		if CurrentToken.TokenKind = ptEof then
			ExpectedFatal(ExpectedTokenKind) //jdj 7/22/1999
		else if Assigned(FOnMessage) then
			DoMessage(Format(SExpected, ['EX:' + TokenName(ExpectedTokenKind), TokenName(CurrentTokenExID)]), CurrentToken.Line, CurrentToken.Column);
	end;

	Result := CurrentToken;
	NextToken;
end;

{Replace Token with cnEndOfFile if TokenId = ptnull}

procedure TDelphiParser.ExpectedFatal(Sym: TptTokenKind);
var
	s: string;
begin
	if Sym <> CurrentToken.TokenKind then
	begin
		// Handle EOF special --jdj 7/22/1999
		if CurrentTokenKind = ptEof then
			s := SEndOfFile // '%s' expected found '%s'
		else
			s := CurrentToken.ToString;

		raise ESyntaxError.CreatePos(Format(SExpected, [TokenKindToStr(Sym), s]), Point(CurrentToken.Line, CurrentToken.Column));
	end;

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

function TDelphiParser.NilToken: TSyntaxNode2;
begin
//	FStack.AddChild(ntLiteral).Attributes[anType] := AttributeValue[atNil];
//	EatToken(ptNil);

	Result := TSyntaxNode2.Create(ntLiteral);
	Result.Attributes[anType] := AttributeValueToString(atNil);

	AddToken(Result, ptNil);  // Preserves the actual 'nil' token as child
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

function TDelphiParser.NotOp: TSyntaxNode2;
begin
{
	Expects: ptNot

	Example

		if not Visible then

	Returns: ntNot
}
	EatToken(ptNot);

	Result := TSyntaxNode2.Create(ntNot);
end;

function TDelphiParser.ThenStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntThen);
	EatToken(ptThen);
	Result.AddChild(Statement);
end;

function TDelphiParser.Semicolon: TSyntaxToken;
begin
	case CurrentToken.TokenKind of
	ptElse, ptEnd, ptExcept, ptfinally, ptFinalization, ptCloseParen, ptUntil: Result := nil; // NOP
	else
		Result := EatToken(ptSemiColon); // move to the next token, throwing a warning if CurrentToken is not a ptSemiColon
	end;
end;

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

function TDelphiParser.get_UseDefines: Boolean;
begin
//	Result := FTokenizer.UseDefines;
	Result := True;
end;

function TDelphiParser.get_ScopedEnums: Boolean;
begin
//	Result := FTokenizer.ScopedEnums;
	Result := True;
end;

function TDelphiParser.GotoStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntGoto);
//	FStack.Push(ntGoto);

	EatToken(ptGoto);
	LabelId;
end;

function TDelphiParser.get_CurrentTokenGenID: TptTokenKind;
begin
	Result := CurrentToken.GenID;
end;

function TDelphiParser.get_InRound: Boolean;
begin
	Result := FInRound > 0;
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
begin
	// DONE: This should probably call the other new methods to synthesize the missing token.
	// Investigate closer how roslyn does with this, including Token.IsMissing, and it's use
	// as a compile feedback mechanism
//	DoMessage(1, Error + ' found ' + TokenName(CurrentToken.TokenKind), CurrentToken.Line, CurrentToken.Column);

	// Create a zero-width (synthesized) token for error recovery, preserving tree shape
	Result := TSyntaxNode2.Create(ntUnknown);
	Result.Attributes[anName] := Format(Error, Args);
	Result.Attributes[anMissing] := 'true';
	Result.AddChild(CurrentToken);
	NextToken; // We advance tokens just to make progress always.

	DoMessage(Error, CurrentToken.Line, CurrentToken.Column);
end;

function TDelphiParser.Parse(Tokens: TList): TSyntaxNode2;
var
	i: Integer;
	token: TSyntaxToken;
begin
{
	Parse the supplied tokens from the tokenzier into a Syntax Tree,
	rooted at Result (TSyntaxNode2).
}
	// Put the tokens into our local token list so we can indexing functions
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
	// ParseMethodOrProperty handles optional leading [CLASS]
	// This method currently implements procedure/function headings.
	-> (PROCEDURE | FUNCTION)
			QualifiedIdent
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]		// FUNCTION only
			[';']
			(Directive [';'])*

Property
	-> [CLASS]
			PROPERTY Ident
			['[' (Parameter [';'])+ ']']
			[':' MethodReturnType]
			(PropertyDirective)*
			';'
	Tested by: basic_cases.dat/ParseMethodOrProperty
}
	if IsPossibleMethodHeading then
	begin
		Result := TSyntaxNode2.Create(ntMethod);
		if CurrentTokenKind = ptClass then
			Result[anClass] := 'true';
		Result.AddChild(ParseMethodHeading);
	end
	else if IsPossibleProperty then
	begin
		Result := TSyntaxNode2.Create(ntProperty);
		if CurrentTokenKind = ptClass then
			Result[anClass] := 'true';
		Result.AddChild(ParseProperty);
	end
	else
	begin
		Result := SynError('xxxxx Expected method or property');
		Result.AddChild(EatToken);
	end;
end;

function TDelphiParser.ParseFieldDecl: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#FieldDecl

FieldDecl
		-> IdentList ':' Type (PortabilityDirective)* [';']
	Tested by: basic_cases.dat/ParseFieldDecl
}
	Result := TSyntaxNode2.Create(ntField);
	Result.AddChild(ParseIdentifierList);

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

	while IsPossiblePortabilityDirective do
		Result.AddChild(ParsePortabilityDirective);

	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken(ptSemiColon));
end;

function TDelphiParser.ParseCore: TSyntaxNode2;
var
	node: TSyntaxNode2;
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
	Tested by: basic_cases.dat/ParseCore
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
	ptUnit:		node := ParseUnitDeclaration;		// e.g. unit SimpleParser;
	ptProgram:	node := ParseProgramFile;			// e.g. program SimpleParser;
	ptPackage:	node := ParsePackageFile;				// e.g. package SimpleParser;
	ptLibrary:	node := ParseLibraryFile;				// e.g. library SimpleParser;
	else
		node := ParseScriptFile; // for arbitrary expressions
	end;

	Result.AddChild(node);
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
	Tested by: basic_cases.dat/ParseFile
	}
	fs := TFile.OpenRead(FilePath);
	encoding := DetectEncodingFromStream(fs); // Check for BOM

	stm := TStreamAdapter.Create(fs, soOwned);

	Result := Self.ParseStream(stm, FilePath, encoding.CodePage);
end;

class function TDelphiParser.ParseText(Text: UnicodeString; FilePath: string): TSyntaxNode2;
var
	s: TStream;
	stm: ISequentialStream;
	parser: TDelphiParser;
begin
{
	Convert the Text to an ISequentialStream and pass it to the .Parse method.
	Tested by: basic_cases.dat/ParseText
}
	s := TStringStream.Create(Text, CP_UTF16); // expose the string as UTF-16

	// Wrap in a TStreamAdapter in order to get an ISequentialStream (inherited by IStream)
	stm := TStreamAdapter.Create(s, soOwned) as IStream;

	parser := TDelphiParser.Create;
	try
		Result := parser.ParseStream(stm, FilePath, CP_UTF16); // unicode strings are UTF-16
	finally
//		parser.Free;
	end;
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
	Tested by: basic_cases.dat/ParseLibraryFile
}
	Result := TSyntaxNode2.Create(ntLibrary);
	Result.AddChild(EatToken(ptLibrary));
	Result.AddChild(ParseIdentifier);

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

function TDelphiParser.ParsePackageFile: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParsePackageFile
	}
	Result := PoisonNode;

	ExpectedEx(ptPackage);
	ParseQualifiedIdentifier;
	Semicolon;

	case CurrentTokenExID of
	ptRequires: RequiresClause;
	end;

	case CurrentTokenExID of
	ptContains: ContainsClause;
	end;

	while CurrentToken.TokenKind = ptOpenBracket do
	begin
		CustomAttribute;
	end;

	EatToken(ptEnd);
	EatToken(ptDot);
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
	Tested by: basic_cases.dat/ParseProgramFile
}
	Result := TSyntaxNode2.Create(ntProgram);
	Result.AddChild(EatToken(ptProgram));
	Result.AddChild(ParseIdentifier);

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
	Result.AddChild(ParseInitializationSection);
	Result.AddChild(EatToken(ptDot));
end;

function TDelphiParser.UnaryMinus: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntUnaryMinus);
	Result.AddChild(EatToken(ptMinus));
end;

function TDelphiParser.ParseUnitDeclaration: TSyntaxNode2;
var
	unitName: TSyntaxNode2;
	portabilityDirective: TSyntaxNode2;
begin
{
DGrok: http://dgrok.excastle.com/Grammar.html#Unit

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
	Tested by: basic_cases.dat/ParseUnitDeclaration
}
	Result := TSyntaxNode2.Create(ntUnitDeclaration);

	// Add the "unit" token to the child list
	Result.AddChild(EatToken(ptUnit));	// EatToken moves next,

	// Get the unit's full name (e.g. "System.Classes")
	unitName := ParseQualifiedIdentifier;
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
	ptBegin: Result.AddChild(Statements);
	ptEnd: Result.AddChild(EatToken(ptEnd));
	end;

	Result.AddChild(EatToken(ptDot));
end;

function TDelphiParser.ProgramBlock: TSyntaxNode2;
begin
	Result := PoisonNode;


	if CurrentTokenKind = ptUses then
	begin
		MainUsesClause;
	end;

	Block;
end;

function TDelphiParser.MainUsesClause: TSyntaxNode2;
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
			EatToken(ptUses);
			MainUsedUnitStatement;
			while CurrentTokenKind = ptComma do
			begin
				NextToken;
				MainUsedUnitStatement;
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

function TDelphiParser.MethodKind: TSyntaxNode2;
begin
	Result := PoisonNode;


	case CurrentTokenKind of
	ptConstructor:	NextToken;
	ptDestructor:	NextToken;
	ptProcedure:	NextToken;
	ptFunction:		NextToken;
	else
{
		public
			class Fetch: Boolean; // E2123 PROCEDURE, FUNCTION, PROPERTY, or VAR expected
		end;

		E2123 PROCEDURE, FUNCTION, PROPERTY, or VAR expected (Delphi)
}
		SynError(SE2123); // E2123 PROCEDURE, FUNCTION, PROPERTY, or VAR expected
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


function TDelphiParser.MainUsedUnitStatement: TSyntaxNode2;
//var
//	nameNode, pathNode, pathLiteralNode, temp: TSyntaxNode2;
begin
	Result := PoisonNode;


{
	TODO: Convert this to the node and token are first class citizens, nodes are returned model.


	FStack.Push(ntUnit);
	try
		MainUsedUnitName;
		if CurrentToken.TokenKind = ptIn then
		begin
			NextToken;
			MainUsedUnitExpression;
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

function TDelphiParser.MainUsedUnitName: TSyntaxNode2;
begin
	Result := PoisonNode;


  ParseUsedUnit;
end;

function TDelphiParser.MainUsedUnitExpression: TSyntaxNode2;
begin
	Result := PoisonNode;
	ConstantExpression;
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
	Tested by: basic_cases.dat/ParseUsesClause
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

function TDelphiParser.UsedUnitsList: TSyntaxNode2;
begin
{
	uses
		Toolkit, winapi.msxml, Avatar.Exceptions;


}

	Result := ParseUsedUnit;
	while CurrentTokenKind = ptComma do
	begin
		NextToken;
		Result.AddChild(ParseUsedUnit);
	end;
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
	Tested by: basic_cases.dat/ParseUsedUnit
}
	Result := TSyntaxNode2.Create(ntUsedUnit);
	unitName := ParseQualifiedIdentifier;
	Result.AddChild(unitName);
	Result.Attributes[anName] := unitName.Attributes[anName];

	//  IN <stringliteral>
	if CurrentTokenKind = ptIn then
	begin
		Result.AddChild(EatToken(ptIn));
		Result.AddChild(EatToken(ptStringLiteral));
	end;
end;

function TDelphiParser.Block: TSyntaxNode2;
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
		Result.AddChild(Statements);
		Result.AddChild(EatToken(ptEnd));
	end
	else if CurrentTokenKind = ptAsm then
	begin
		Result := AsmStatement;
	end
	else
		Result := SynError('xxxxx Expected block');
end;

procedure TDelphiParser.BuildExpressionTree(ExpressionMethod: TTreeBuilderMethod);
var
	rawExprNode: TSyntaxNode2;
	exprNode: TSyntaxNode2;

	nodeList: TList<TSyntaxNode2>;
	node: TSyntaxNodeOrToken;
	col, line: Integer;
	fileName: string;
begin
	line := CurrentToken.Line;
	col := CurrentToken.Column;
	fileName := 'FCurrentToken.FileName'; // TODO: Fix this

	rawExprNode := TSyntaxNode2.Create(ntExpression);
	try
	//	FStack.Push(rawExprNode);  todo: figure out what this is trying to do
		try
			ExpressionMethod;
		finally
//			FStack.Pop;
		end;

		if rawExprNode.HasChildren then
		begin
			exprNode := TSyntaxNode2.Create(ntExpression);
			exprNode.FCurrentLine := line;
			exprNode.FCurrentColumn := col;
			exprNode.FFileName := fileName;

         nodeList := TList<TSyntaxNode2>.Create;
         try
            for node in rawExprNode.ChildNodes do
               nodeList.Add(node.AsNode);
//					TExpressionTools.RawNodeListToTree(RawExprNode, NodeList, ExprNode); TODO: Fix this
         finally
            nodeList.Free;
         end;
         rawExprNode.AddChild(exprNode);
		end;
	finally
		rawExprNode.Free;
	end;
end;

function TDelphiParser.ParseImplementationDecl: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ImplementationDecl

ImplementationDecl
	-> LabelDeclSection
	-> ConstSection
	-> TypeSection
	-> VarSection
	-> MethodImplementation
	-> ExportsStatement
	-> AssemblyAttribute
	Tested by: basic_cases.dat/ParseImplementationDecl
}
	case CurrentTokenKind of
	ptLabel:				Result := LabelDeclarationSection;				//LabelDeclSection
	ptConst:				Result := ParseConstSection;
	ptResourceString:	Result := ParseResStringSection;
	ptType:				Result := ParseTypeSection;
	ptVar:				Result := ParseVarSection;
	ptThreadVar:		Result := ParseVarSection;

	ptClass:				Result := ProcedureDeclarationSection;
	ptProcedure:		Result := ProcedureDeclarationSection;
	ptFunction:			Result := ProcedureDeclarationSection;
	ptConstructor:		Result := ProcedureDeclarationSection;
	ptDestructor:		Result := ProcedureDeclarationSection;
	ptOperator:			Result := ProcedureDeclarationSection;

	ptExports:			Result := ExportsClause;
	ptOpenbracket:		Result := CustomAttribute; // [assembly: Expression ]
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

function TDelphiParser.UnitId: TSyntaxNode2;
begin
{

}
	Result := TSyntaxNode2.Create(ntUnknown);
	Result.Attributes[anName] := CurrentToken.ValueText;

	EatToken(ptIdentifier);
end;

function TDelphiParser.ParseQualifiedIdentifier: TSyntaxNode2;
var
//	node: TSyntaxNode2;
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
	Tested by: basic_cases.dat/ParseQualifiedIdentifier
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

function TDelphiParser.InterfaceHeritage: TSyntaxNode2;
begin
	Result := PoisonNode;

	EatToken(ptOpenParen);
	ParseAncestorList;
	EatToken(ptCloseParen);
end;

function TDelphiParser.InterfaceGUID: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntGuid);
//FStack.Push(ntGuid);

	EatToken(ptOpenBracket);
	CharString;
	EatToken(ptCloseBracket);
end;

function TDelphiParser.AccessSpecifier: TSyntaxNode2;
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
			ReadAccessIdentifier;
		end;
	ptWrite:
		begin
			Result.AddChild(EatToken);
			WriteAccessIdentifier;
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

function TDelphiParser.ReadAccessIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;

	Variable;
end;

function TDelphiParser.WriteAccessIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;

	Variable;
end;

function TDelphiParser.StorageSpecifier: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenExID of
	ptStored:		StorageStored;
	ptDefault:		StorageDefault;
	ptNoDefault:	StorageNoDefault;
	else
		SynError('InvalidStorageSpecifier');
	end;
end;

function TDelphiParser.StorageDefault: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntDefault);
//FStack.Push(ntDefault);

	ExpectedEx(ptDefault);
	StorageExpression;
end;

function TDelphiParser.StorageNoDefault: TSyntaxNode2;
begin
	Result := PoisonNode;

	ExpectedEx(ptNoDefault);
end;

function TDelphiParser.StorageStored: TSyntaxNode2;
begin
	Result := PoisonNode;

	ExpectedEx(ptStored);
	case CurrentTokenKind of
	ptIdentifier:			StorageIdentifier;
	else
		if CurrentTokenKind <> ptSemiColon then
			StorageExpression;
	end;
end;

function TDelphiParser.StorageExpression: TSyntaxNode2;
begin
	Result := PoisonNode;

	ConstantExpression;
end;

function TDelphiParser.StorageIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;

	EatToken(ptIdentifier);
end;

function TDelphiParser.PropertyParameterList: TSyntaxNode2;
begin
	Result := PoisonNode;

	EatToken(ptOpenBracket);
	FormalParameterSection;
	while CurrentTokenKind = ptSemiColon do
	begin
		Semicolon;
		FormalParameterSection;
	end;
	EatToken(ptCloseBracket);
end;

function TDelphiParser.PropertySpecifiers: TSyntaxNode2;
begin
	Result := PoisonNode;

	if CurrentTokenExID = ptIndex then
		ParseIndexSpecifier;

	while CurrentTokenExID in [ptRead, ptReadOnly, ptWrite, ptWriteOnly{, ptAdd, ptRemove}] do
	begin
		AccessSpecifier;
		if CurrentTokenKind = ptSemicolon then
			NextToken;
	end;

	if CurrentTokenExID = ptDispId then
		DispIDSpecifier;

	while CurrentTokenExID in [ptDefault, ptNoDefault, ptStored] do
	begin
		StorageSpecifier;
		if CurrentTokenKind = ptSemicolon then
			NextToken;
	end;

	if CurrentTokenExID = ptImplements then
		ImplementsSpecifier;

	if CurrentTokenKind = ptSemicolon then
		NextToken;
end;

function TDelphiParser.PropertyInterface: TSyntaxNode2;
begin
	Result := PoisonNode;

	if CurrentTokenKind = ptOpenBracket then
		PropertyParameterList;

	EatToken(ptColon);
	ParseTypeId;
end;

function TDelphiParser.ClassMethodHeading: TSyntaxNode2;
var
	aheadTokenKind: TptTokenKind;
begin
	Result := TSyntaxNode2.Create(ntMethod);

	if CurrentTokenKind = ptClass then
		ClassClass;

//	InitAhead;
	NextToken;
	FunctionProcedureName;
	aheadTokenKind := CurrentTokenKind;
//	RestoreAhead;


	if aheadTokenKind = ptEquals then
		ClassMethodResolution
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
					SynError('InvalidProcedureMethodDeclaration');
			end;
		else
			SynError('InvalidClassMethodHeading');
		end;
	end;
end;

procedure TDelphiParser.ClassFunctionHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anKind] := AttributeValueToString(atFunction);

	ParentNode.AddChild(EatToken(ptFunction));
	ParentNode.AddChild(FunctionProcedureName);

	if CurrentTokenKind = ptOpenParen then
		ParentNode.AddChild(FormalParameterList);

	ParentNode.AddChild(EatToken(ptColon));
	ParentNode.AddChild(ReturnType);

	if CurrentTokenKind = ptSemicolon then
		ParentNode.AddChild(Semicolon);

	if CurrentTokenExID in ClassMethodDirectiveEnum then
		ClassMethodDirective(ParentNode);
end;

function TDelphiParser.FunctionMethodName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

	Result.AddChild(EatToken(ptIdentifier));
end;

procedure TDelphiParser.ClassProcedureHeading(ParentNode: TSyntaxNode2);
begin
  ParentNode.Attributes[anKind] := AttributeValueToString(atProcedure);

	ParentNode.AddChild(EatToken(ptProcedure));
	ParentNode.AddChild(FunctionProcedureName);
	if CurrentTokenKind = ptOpenParen then
	begin
		ParentNode.AddChild(FormalParameterList);
	end;
	if CurrentTokenKind = ptSemicolon then
		ParentNode.AddChild(Semicolon);

	if CurrentTokenExID = ptDispId then
	begin
		ParentNode.AddChild(DispIDSpecifier);
		if CurrentTokenKind = ptSemicolon then
			ParentNode.AddChild(Semicolon);
	end;
	if CurrentTokenExID in ClassMethodDirectiveEnum then
		ClassMethodDirective(ParentNode);
end;

function TDelphiParser.ProcedureMethodName: TSyntaxNode2;
begin
	Result := PoisonNode;

	EatToken(ptIdentifier);
end;

function TDelphiParser.ClassMethodResolution: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntResolutionClause);
//  FStack.Push(ntResolutionClause);

     case CurrentTokenKind of
    ptFunction:
      begin
        NextToken;
      end;
    ptProcedure:
      begin
        NextToken;
      end;
    ptIdentifier:
      begin
        if CurrentTokenExID = ptOperator then
          NextToken;
      end;
     end;
     FunctionProcedureName;
     EatToken(ptEquals);
     FunctionMethodName;
     Semicolon;
end;

procedure TDelphiParser.ClassOperatorHeading(ParentNode: TSyntaxNode2);
begin
  ExpectedEx(ptOperator);
  FunctionProcedureName;
  if CurrentTokenKind = ptOpenParen then
  begin
    FormalParameterList;
  end;

  if CurrentTokenKind = ptColon then
  begin
    EatToken(ptColon);
    ReturnType;
  end;

  if CurrentTokenKind = ptSemicolon then
    Semicolon;
  if CurrentTokenExID in ClassMethodDirectiveEnum then
    ClassMethodDirective(ParentNode);
end;

function TDelphiParser.ResolutionInterfaceName: TSyntaxNode2;
begin
	Result := PoisonNode;


	EatToken(ptIdentifier);
end;

function TDelphiParser.Constraint: TSyntaxNode2;
begin
	Result := PoisonNode;
  while CurrentTokenKind in [ptConstructor, ptRecord, ptClass, ptIdentifier] do
  begin
    case CurrentTokenKind of
      ptConstructor: ConstructorConstraint;
      ptRecord: RecordConstraint;
      ptClass: ClassConstraint;
      ptIdentifier: ParseTypeId;
    end;
    if CurrentTokenKind = ptComma then
      NextToken;
  end;
end;

function TDelphiParser.ConstraintList: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntConstraints);
//FStack.Push(ntConstraints);
	Constraint;
	while CurrentTokenKind = ptComma do
	begin
		Constraint;
	end;
end;

function TDelphiParser.ConstructorConstraint: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntConstructorConstraint);
//FStack.Push(ntConstructorConstraint);

		EatToken(ptConstructor);
end;

procedure TDelphiParser.ConstructorHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptConstructor));
	ConstructorName(ParentNode);

	if CurrentTokenKind = ptOpenParen then
	begin
		ParentNode.AddChild(FormalParameterList);
	end;
	if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(Semicolon);

	ClassMethodDirective(ParentNode);
end;

procedure TDelphiParser.ConstructorName(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anKind] := AttributeValueToString(atConstructor);
	ParentNode.Attributes[anName] := CurrentToken.ValueText;

	ParentNode.AddChild(EatToken(ptIdentifier));
end;

procedure TDelphiParser.DestructorHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptDestructor));
	DestructorName(ParentNode);
	if CurrentTokenKind = ptOpenParen then
		FormalParameterList;

	if CurrentTokenKind = ptSemiColon then
		Semicolon;
	ClassMethodDirective(ParentNode);
end;

procedure TDelphiParser.DestructorName(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anKind] := AttributeValueToString(atDestructor);
	ParentNode.Attributes[anName] := CurrentToken.ValueText;

	ParentNode.AddChild(EatToken(ptIdentifier));
end;

procedure TDelphiParser.ClassMethod(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anClass] := AttributeValueToString(atTrue);
	ParentNode.AddChild(EatToken(ptClass));
end;

procedure TDelphiParser.ClassMethodDirective(ParentNode: TSyntaxNode2);
begin
	while CurrentTokenExID in ClassMethodDirectiveEnum do
	begin
		if CurrentTokenExID = ptDispId then
			ParentNode.AddChild(DispIDSpecifier)
		else
			ProceduralDirective(ParentNode);

		if CurrentTokenKind = ptSemicolon then
			ParentNode.AddChild(Semicolon);
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
		SynError('InvalidMethodHeading');
	end;
end;

procedure TDelphiParser.ObjectFunctionHeading(ParentNode: TSyntaxNode2);
begin
	EatToken(ptFunction);
	FunctionMethodName;
	if CurrentTokenKind = ptOpenParen then
		FormalParameterList;

	EatToken(ptColon);
	ReturnType;
	if CurrentTokenKind = ptSemiColon then
		Semicolon;

	ObjectMethodDirective(ParentNode);
end;

procedure TDelphiParser.ObjectProcedureHeading(ParentNode: TSyntaxNode2);
begin
	EatToken(ptProcedure);
	ProcedureMethodName;
	if CurrentTokenKind = ptOpenParen then
		FormalParameterList;

	if CurrentTokenKind = ptSemiColon then
		Semicolon;

	ObjectMethodDirective(ParentNode);
end;

procedure TDelphiParser.ObjectConstructorHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptConstructor));
	ConstructorName(ParentNode);

	if CurrentTokenKind = ptOpenParen then
		ParentNode.AddChild(FormalParameterList);

	if CurrentTokenKind = ptSemiColon then
		ParentNode.AddChild(Semicolon);

	ObjectMethodDirective(ParentNode);
end;

procedure TDelphiParser.ObjectDestructorHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptDestructor));
	DestructorName(ParentNode);

	if CurrentTokenKind = ptOpenParen then
	begin
		ParentNode.AddChild(FormalParameterList);
	end;

	if CurrentTokenKind = ptSemiColon then Semicolon;
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
			ParentNode.AddChild(Semicolon);
	end;
end;

function TDelphiParser.Directive16Bit: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenExID of
	ptNear:		NextToken;
	ptFar:		NextToken;
	ptExport:	NextToken;
	else
		SynError('InvalidDirective16Bit');
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
		ParentNode.Attributes[anReintroduce] := AttributeValueToString(atTrue)
	else if SameText(token, 'overload') then
		ParentNode.Attributes[anOverload] := AttributeValueToString(atTrue)
	else if SameText(token, 'abstract') then
		ParentNode.Attributes[anAbstract] := AttributeValueToString(atTrue);

	case CurrentTokenExID of
	ptAbstract:		NextToken;
	ptVirtual:		NextToken;
	ptDynamic:		NextToken;
	ptMessage:		DirectiveBindingMessage;
	ptOverride:		NextToken;
	ptOverload:		NextToken;
	ptReintroduce:	NextToken;
	else
		SynError('InvalidDirectiveBinding');
	end;
end;

function TDelphiParser.DirectiveBindingMessage: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntMessage);
//	FStack.Push(ntMessage);

	NextToken;
	ConstantExpression;
end;

function TDelphiParser.ReturnType: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntReturnType);
//	FStack.Push(ntReturnType);

	while CurrentTokenKind = ptOpenBracket do
		CustomAttribute;

	ParseTypeId;
end;

function TDelphiParser.RoundClose: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntRoundClose);
	Result.AddChild(EatToken(ptCloseParen));
	Dec(FInRound);
end;

function TDelphiParser.RoundOpen: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntRoundOpen);

	Result.AddChild(EatToken(ptOpenParen));
	Inc(FInRound);
end;

function TDelphiParser.FormalParameterList: TSyntaxNode2;
begin
	Result := PoisonNode;

  EatToken(ptOpenParen);
  FormalParameterSection;
  while CurrentTokenKind = ptSemiColon do
  begin
    Semicolon;
    FormalParameterSection;
  end;
  EatToken(ptCloseParen);
end;

function TDelphiParser.FormalParameterSection: TSyntaxNode2;
begin
	Result := PoisonNode;


  while CurrentTokenKind = ptOpenBracket do
    CustomAttribute;
  case CurrentTokenKind of
    ptConst:
      begin
        ConstParameter;
      end;
    ptIdentifier:
      case CurrentTokenExID of
        ptOut: OutParameter;
      else
        ParameterFormal;
      end;
    ptIn:
      begin
        InParameter;
      end;
    ptVar:
      begin
        VarParameter;
      end;
  end;
end;

function TDelphiParser.ConstParameter: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntParameter);
	Result.Attributes[anKind] := AttributeValueToString(atConst);

   EatToken(ptConst);
   ParameterNameList;
   case CurrentTokenKind of
    ptColon:
      begin
        NextToken;
        FormalParameterType;
        if CurrentTokenKind = ptEquals then
        begin
          NextToken;
          ParseTypedConstant;
        end;
      end
   end;
end;

function TDelphiParser.VarParameter: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntParameters);
	Result.Attributes[anKind] := AttributeValueToString(atVar);

     EatToken(ptVar);
     ParameterNameList;
     case CurrentTokenKind of
       ptColon:
         begin
           NextToken;
           FormalParameterType;
         end
     end;
end;

function TDelphiParser.OutParameter: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntParameters);
	Result.Attributes[anKind] := AttributeValueToString(atOut);
	ExpectedEx(ptOut);
	ParameterNameList;
	case CurrentTokenKind of
	ptColon:
		begin
			NextToken;
			FormalParameterType;
		end
	end;
end;

function TDelphiParser.ParameterFormal: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntParameters);
	case CurrentTokenKind of
	ptIdentifier:
		begin
			Result.AddChild(ParameterNameList);
			Result.AddChild(EatToken(ptColon));
			Result.AddChild(FormalParameterType);
			if CurrentTokenKind = ptEquals then
			begin
				Result.AddChild(EatToken(ptEquals));
				Result.AddChild(ParseTypedConstant);
			end;
		end;
	else
		SynError('InvalidParameter');
	end;
end;

function TDelphiParser.ParameterNameList: TSyntaxNode2;
begin
	Result := PoisonNode;


  while CurrentTokenKind = ptOpenBracket do
    CustomAttribute;
  ParameterName;

  while CurrentTokenKind = ptComma do
  begin
    NextToken;

    while CurrentTokenKind = ptOpenBracket do
      CustomAttribute;
    ParameterName;
  end;
end;

function TDelphiParser.ParameterName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.FormalParameterType: TSyntaxNode2;
begin
	Result := PoisonNode;


  if CurrentTokenKind = ptArray then
    ParseStructuredType
  else
    ParseTypeId;
end;

function TDelphiParser.FunctionMethodDeclaration: TSyntaxNode2;
var
	methodName: string;
begin
	Result := PoisonNode;


	if (CurrentTokenKind = ptIdentifier) and (CurrentTokenExID = ptOperator) then
	begin
		NextToken;
		Exit;
	end;

	methodName := LowerCase(CurrentToken.ValueText);
	Result.Attributes[anKind] := methodName;

	MethodKind;
	FunctionProcedureName;

	if CurrentTokenKind = ptOpenParen then
		FormalParameterList;

	case CurrentTokenKind of
	ptSemiColon: FunctionProcedureBlock(Result);
	else
		EatToken(ptColon);
		ReturnType;
		FunctionProcedureBlock(Result);
	end;
end;

function TDelphiParser.ProcedureProcedureName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result.Attributes[anName] := CurrentToken.ValueText;

	MethodKind;
	FunctionProcedureName;

	if CurrentTokenKind = ptOpenParen then
	begin
		FormalParameterList;
	end;

	FunctionProcedureBlock(Result);
end;

function TDelphiParser.FunctionProcedureName: TSyntaxNode2;
//var
//	ChildNode, NameNode, TypeParam, TypeNode, Temp: TSyntaxNode2;
//	FullName, TypeParams: string;
begin
	Result := TSyntaxNode2.Create(ntName);
(*
	TODO: figure out what this does, and update it
  NameNode := FStack.Peek;
	  ObjectNameOfMethod;
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

function TDelphiParser.ObjectNameOfMethod: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

  if CurrentTokenKind = ptIn then
    EatToken(ptIn)
  else
    EatToken(ptIdentifier);

  if CurrentTokenKind = ptLessThan then
    ParseTypeParams;
  if CurrentTokenKind = ptDot then
  begin
    EatToken(ptDot);
    ObjectNameOfMethod;
  end;
end;

procedure TDelphiParser.FunctionProcedureBlock(ParentNode: TSyntaxNode2);
var
  HasBlock: Boolean;
begin
  HasBlock := True;
  if CurrentTokenKind = ptSemiColon then
		Semicolon;

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
//          FStack.AddChild(ForwardDeclaration);		todo: figure this out
          HasBlock := False;
        end
    else
      begin
        ProceduralDirective(ParentNode);
      end;
    end;
    if CurrentTokenKind = ptSemiColon then Semicolon;
  end;

  if HasBlock then
  begin
    case CurrentTokenKind of
      ptAsm:
        begin
          AsmStatement;
        end;
    else
      begin
        Block;
      end;
    end;
    Semicolon;
  end;
end;

function TDelphiParser.ExternalDirective: TSyntaxNode2;
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
	ExpectedEx(ptExternal); // expect ptExternal, and move to next token

	Result := TSyntaxNode2.Create(ntExternal);

	case CurrentTokenKind of
	ptSemiColon: Semicolon;
	else
		begin
			if CurrentTokenExID <> ptName then
				ParseSimpleExpression;

			if CurrentTokenExID = ptDelayed then
				NextToken;

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
			Semicolon;
			ExternalDirectiveThree;
		end;
	end
end;

function TDelphiParser.ExternalDirectiveThree: TSyntaxNode2;
begin
   Result := PoisonNode;

	case CurrentTokenKind of
	ptMinus: NextToken;
	end;

	case CurrentTokenKind of
	ptIdentifier, ptIntegerConst: NextToken;
	end;
end;

function TDelphiParser.ForStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntFor);
	EatToken(ptFor);
	if CurrentTokenKind = ptVar then
	begin
		NextToken;
		InlineVarDeclaration;
	end
	else
		ParseQualifiedIdentifier;

	if CurrentToken.TokenKind = ptAssign then
	begin
		EatToken(ptAssign);
		ForStatementFrom;
		case CurrentTokenKind of
		ptTo: ForStatementTo;
		ptDownTo: ForStatementDownTo;
		else
			SynError('InvalidForStatement');
		end;
	end
	else if CurrentToken.TokenKind = ptIn then
		ForStatementIn;

	EatToken(ptDo);
	Statement;
end;

function TDelphiParser.ForStatementDownTo: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntDownTo);
	Result.AddChild(EatToken(ptDownTo));
	Result.AddChild(ParseExpression);
end;

function TDelphiParser.ForStatementFrom: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntFrom);
	Result.AddChild(ParseExpression);
end;

function TDelphiParser.ForStatementIn: TSyntaxNode2;
begin
   Result := TSyntaxNode2.Create(ntIn);
//  FStack.Push(ntIn);
	  EatToken(ptIn);
	  ParseExpression;
end;

function TDelphiParser.ForStatementTo: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntTo);
//  FStack.Push(ntTo);
	  EatToken(ptTo);
	  ParseExpression;
end;

function TDelphiParser.WhileStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntWhile);
//  FStack.Push(ntWhile);
	  EatToken(ptWhile);
	  ParseExpression;
	  EatToken(ptDo);
	  Statement;
end;

function TDelphiParser.RepeatStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntRepeat);
//  FStack.Push(ntRepeat);
	  EatToken(ptRepeat);
	  StatementList;
	  EatToken(ptUntil);
	  ParseExpression;
end;

function TDelphiParser.CaseStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntCase);
//	FStack.Push(ntCase);
		EatToken(ptCase);
		ParseExpression;
		EatToken(ptOf);
		CaseSelector;
		while CurrentTokenKind = ptSemiColon do
		begin
			Semicolon;
			case CurrentTokenKind of
			ptElse, ptEnd: ;
			else
				CaseSelector;
			end;
		end;
		if CurrentTokenKind = ptElse then
			CaseElseStatement;
		EatToken(ptEnd);
end;

function TDelphiParser.CaseSelector: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntCaseSelector);
//	FStack.Push(ntCaseSelector);
		CaseLabelList;
		EatToken(ptColon);
		case CurrentTokenKind of
		ptSemiColon: EmptyStatement;
		else
			Statement;
		end;
end;

function TDelphiParser.CaseElseStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntCaseElse);
//	FStack.Push(ntCaseElse);
		EatToken(ptElse);
		StatementList;
		Semicolon;
end;

function TDelphiParser.CaseLabel: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntCaseLabel);
//	FStack.Push(ntCaseLabel);
		ConstantExpression;
		if CurrentTokenKind = ptDotDot then
		begin
			NextToken;
			ConstantExpression;
		end;
end;

function TDelphiParser.IfStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntIf);
//  FStack.Push(ntIf);
	  EatToken(ptIf);
	  ParseExpression;
	  ThenStatement;
	  if CurrentTokenKind = ptElse then
	    ElseStatement;
end;

function TDelphiParser.ExceptBlock: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExcept);
//  FStack.Push(ntExcept);
     if CurrentTokenExID = ptOn then
     begin
       ExceptionHandlerList;
       if CurrentTokenKind = ptElse then
         ExceptionBlockElseBranch;
     end else
       if CurrentTokenKind = ptElse then
         ExceptionBlockElseBranch
       else
         StatementList;
end;

function TDelphiParser.ExceptionHandlerList: TSyntaxNode2;
begin
   Result := PoisonNode;

  while CurrentTokenExID = ptOn do
  begin
    ExceptionHandler;
    Semicolon;
  end;
end;

function TDelphiParser.ExceptionHandler: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExceptionHandler);
//  FStack.Push(ntExceptionHandler);
	  ExpectedEx(ptOn);
	  ExceptionIdentifier;
	  EatToken(ptDo);
	  Statement;
end;

function TDelphiParser.ExceptionBlockElseBranch: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntElse);
//  FStack.Push(ntElse);
	  NextToken;
	  StatementList;
end;

function TDelphiParser.ExceptionIdentifier: TSyntaxNode2;
begin
   Result := PoisonNode;

//	InitAhead;
  case PeekTokenKind of
    ptDot:
      begin
        ExceptionClassTypeIdentifier;
      end;
    ptColon:
      begin
        ExceptionVariable;
      end
  else
    begin
      ExceptionClassTypeIdentifier;
    end;
  end;
end;

function TDelphiParser.ExceptionClassTypeIdentifier: TSyntaxNode2;
begin
   Result := PoisonNode;
  ParseType;
end;

function TDelphiParser.ExceptionVariable: TSyntaxNode2;
var
	child: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntVariable);

	child := TSyntaxNode2.Create(ntName);
	child[anName] := CurrentToken.ValueText;

	child.AddChild(EatToken(ptIdentifier));
	child.AddChild(EatToken(ptColon));
	child.AddChild(ExceptionClassTypeIdentifier);

	Result.AddChild(child);
end;

function TDelphiParser.InlineConstSection: TSyntaxNode2;
begin
	Result := PoisonNode;


  case CurrentTokenKind of
    ptConst:
      begin
        NextToken;
        ConstantDeclaration;
      end;
  else
    begin
      SynError('InvalidConstSection');
    end;
  end;
end;

function TDelphiParser.InlineStatement: TSyntaxNode2;
begin
	Result := PoisonNode;


  EatToken(ptInline);
  EatToken(ptOpenParen);
  EatToken(ptIntegerConst);
  while (CurrentTokenKind = ptSlash) do
  begin
    NextToken;
    EatToken(ptIntegerConst);
  end;
  EatToken(ptCloseParen);
end;

function TDelphiParser.InlineVarSection: TSyntaxNode2;
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
           EatToken(ptVar);
           while CurrentTokenKind = ptIdentifier do
             InlineVarDeclaration;

         if CurrentTokenKind = ptAssign then
           begin
             NextToken;
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

function TDelphiParser.InlineVarDeclaration: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntVariables);
	  VarNameList;
	  if CurrentTokenKind = ptColon then
	  begin
	    NextToken;
	    ParseType;
	  end;
end;

function TDelphiParser.InParameter: TSyntaxNode2;
begin
	Result := PoisonNode;


  EatToken(ptIn);
  ParameterNameList;
  case CurrentTokenKind of
    ptColon:
      begin
        NextToken;
        FormalParameterType;
        if CurrentTokenKind = ptEquals then
        begin
          NextToken;
          ParseTypedConstant;
        end;
      end
  end;
end;

function TDelphiParser.AsmStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntStatements);
	Result[anType] := AttributeValueToString(atAsm);
{
		Lexer.AsmCode  := True;
		EatToken(ptAsm);
		// should be replaced with a Assembler lexer
		while TokenID <> ptEnd do
			case FCurrentToken.TokenKind of
			ptAddressOp:
				begin
					NextToken;
					NextToken;
				end;
			ptDoubleAddressOp:
				begin
					NextToken;
					NextToken;
				end;
			ptNull:
				begin
					EatToken(ptEnd);
					Exit;
				end;
			else
				NextToken;
		end;
		Lexer.AsmCode := False;
}
		EatToken(ptEnd);
end;

function TDelphiParser.AsOp: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAs);
	EatToken(ptAs);
end;

function TDelphiParser.AssignOp: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAssign);
	Result.AddChild(EatToken(ptAssign));
end;

function TDelphiParser.AtExpression: TSyntaxNode2;
begin
{
	raise Exception.Create('Hello') at @SaveChanges;

	Read more at https://stackoverflow.com/a/8951057/12597
}
	Result := TSyntaxNode2.Create(ntAt);
		ExpectedEx(ptAt);
		ParseExpression;
end;

function TDelphiParser.RaiseStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntRaise);
     EatToken(ptRaise);
     case CurrentTokenKind of
       ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptOpenParen:
         begin
           ParseExpression;
         end;
     end;
     if CurrentTokenExID = ptAt then
       AtExpression;
end;

function TDelphiParser.TryStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntTry);
     EatToken(ptTry);
     StatementList;
     case CurrentTokenKind of
       ptExcept:
         begin
           NextToken;
           ExceptBlock;
           EatToken(ptEnd);
         end;
       ptFinally:
         begin
           NextToken;
           FinallyBlock;
           EatToken(ptEnd);
         end;
     else
       begin
         SynError('InvalidTryStatement');
       end;
     end;
end;

function TDelphiParser.WithStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntWith);
	  EatToken(ptWith);
	  WithExpressionList;
	  EatToken(ptDo);
	  Statement;
end;

function TDelphiParser.WithExpressionList: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExpressions);
	  ParseExpression;
	  while CurrentToken.TokenKind = ptComma do
	  begin
	    NextToken;
	    ParseExpression;
	  end;
end;

function TDelphiParser.StatementList: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntStatements);
	Result.AddChild(Statements);
end;

function TDelphiParser.StatementOrExpression: TSyntaxNode2;
begin
	Result := PoisonNode;


  if CurrentTokenKind = ptGoto then
    SimpleStatement
  else
  begin
//    InitAhead;
    Designator;

    if CurrentTokenKind in [ptAssign, ptSemicolon, ptElse] then
      SimpleStatement
    else
      ParseExpression;
  end;
end;

function TDelphiParser.Statements: TSyntaxNode2;
begin {removed ptIntegerConst jdj-Put back in for labels}
	Result := PoisonNode;

	while CurrentTokenKind in [ptAddressOp, ptAsm, ptBegin, ptCase, ptConst, ptDoubleAddressOp,
			ptFor, ptGoTo, ptIdentifier, ptIf, ptInherited, ptInline, ptIntegerConst,
			ptPointerSymbol, ptRaise, ptOpenParen, ptRepeat, ptSemiColon, ptString,
			ptTry, ptVar, ptWhile, ptWith] do
	begin
		Statement;
		Semicolon;
	end;
end;

function TDelphiParser.SimpleStatement: TSyntaxNode2;
//var
//	Temp: TSyntaxNode2;
//	Node: TSyntaxNodeOrToken;
//	LHS, RHS: TSyntaxNode2;
//	NodeList: TList<TSyntaxNode2>;
//	I, AssignIdx: Integer;
//	Position: TPoint;
//	FileName: string;
begin
//	TODO: Fix this

//	Position := Lexer.PosXY;
//	FileName := Lexer.FileName;

	Result := TSyntaxNode2.Create(ntStatement);
   case CurrentTokenKind of
   ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptOpenParen, ptString:
      begin
         Designator;
         if CurrentTokenKind = ptAssign then
         begin
            AssignOp;
            ParseExpression;
         end;
      end;
   ptGoTo: GotoStatement;
   end;

   if not Result.HasChildren then
      Exit;

{
		if RawStatement.FindNode(ntAssign) <> nil then
		begin
			Temp := FStack.Push(ntAssign);
			try
				Temp.FCurrentColumn := Position.X;
				Temp.FCurrentLine := Position.Y;
				Temp.FFileName := FileName;

				NodeList := TList<TSyntaxNode2>.Create;
				try
					AssignIdx := -1;
					for I := 0 to RawStatement.ChildNodes.Count-1 do
					begin
						if RawStatement.ChildNodes[I].AsNode.NodeType = ntAssign then
						begin
							AssignIdx := I;
							Break;
						end;
						NodeList.Add(RawStatement.ChildNodes[I].AsNode);
					end;

					if NodeList.Count = 0 then
						raise EParserException.Create(Position.Y, Position.X, 'FTokenizer.FileName', 'Illegal expression');

					LHS := FStack.AddChild(ntLHS);
					LHS.AssignPositionFrom(NodeList[0]);

//					TExpressionTools.RawNodeListToTree(RawStatement, NodeList, LHS); TODO: Fix this

					NodeList.Clear;

					for I := AssignIdx + 1 to RawStatement.ChildNodes.Count-1 do
						NodeList.Add(RawStatement.ChildNodes[I].AsNode);

					if NodeList.Count = 0 then
						raise EParserException.Create(Position.Y, Position.X, 'FTokenizer.FileName', 'Illegal expression');

					RHS := FStack.AddChild(ntRHS);
					RHS.AssignPositionFrom(NodeList[0]);

//					TExpressionTools.RawNodeListToTree(RawStatement, NodeList, RHS); TODO: Fix this
				finally
					NodeList.Free;
				end;
			finally
				FStack.Pop;
			end;
		end
		else
		begin
			Temp := TSyntaxNode2.Create(ntCall);
				// TODO: fix this
				Temp.FCurrentColumn := Position.X;
				Temp.FCurrentLine := Position.Y;

				NodeList := TList<TSyntaxNode2>.Create;
				try
					for Node in RawStatement.ChildNodes do
						NodeList.Add(Node.AsNode);
//					TExpressionTools.RawNodeListToTree(RawStatement, NodeList, FStack.Peek); TODO: fix this
				finally
					NodeList.Free;
				end;
		end;
}
end;

function TDelphiParser.Statement: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenKind of
	ptAsm:	Result := AsmStatement;
	ptBegin:	Result := Statements;
	ptCase:	Result := CaseStatement;
	ptConst:	Result := InlineConstSection;
	ptFor:	Result := ForStatement;
	ptIf:		Result := IfStatement;
	ptIdentifier:
		begin
			case PeekTokenKind of
			ptColon:	Result := LabeledStatement;
			else
				Result := StatementOrExpression;
			end;
		end;
	ptInherited:	Result := InheritedStatement;
	ptInLine:		Result := InlineStatement;
	ptIntegerConst:
		begin
			case PeekTokenKind of
			ptColon: Result := LabeledStatement;
			else
				begin
					SynError('InvalidLabeledStatement');
					NextToken;
				end;
			end;
		end;
	ptRepeat:		Result := RepeatStatement;
	ptRaise:			Result := RaiseStatement;
	ptSemiColon:	Result := EmptyStatement;
	ptTry:			Result := TryStatement;
	ptVar:			Result := InlineVarSection;
	ptWhile:			Result := WhileStatement;
	ptWith:			Result := WithStatement;
	else
		Result := StatementOrExpression;
	end;
end;

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
		Result := TSyntaxToken.Create(ExpectedTokenKind, -1, -1, '');
		Result.IsMissing := True;
		if CurrentToken.TokenKind = ptEof then
			ExpectedFatal(ExpectedTokenKind)
		else
		begin
			s := Format(SExpected, [TokenName(ExpectedTokenKind), TokenName(CurrentToken.TokenKind)]);
			DoMessage(s, CurrentToken.Line, CurrentToken.Column);
			Log(s);
		end;

		// We advance tokens just to make progress always.
		NextToken;

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


function TDelphiParser.ElseStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntElse);
	EatToken(ptElse);
	Statement;
end;

function TDelphiParser.EmptyStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntEmptyStatement);
	{	Nothing to do here.
		The semicolon will be removed in StatementList
	}
end;

function TDelphiParser.InheritedStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntInherited);
		EatToken(ptInherited);
		if CurrentTokenKind = ptIdentifier then
			Statement;
end;

function TDelphiParser.LabeledStatement: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenKind of
	ptIdentifier:
		begin
			NextToken;
			EatToken(ptColon);
			Statement;
		end;
	ptIntegerConst:
		begin
			NextToken;
			EatToken(ptColon);
			Statement;
		end;
	else
		SynError('InvalidLabeledStatement');
	end;
end;

function TDelphiParser.StringStatement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := CurrentToken.ValueText;

	Result.AddChild(EatToken(ptString));
end;

function TDelphiParser.SetElement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntElement);

		ParseExpression;
		if CurrentTokenKind = ptDotDot then
		begin
			NextToken;
			ParseExpression;
		end;
end;

procedure TDelphiParser.set_IncludeHandler(IncludeHandler: IUnknown{IIncludeHandler});
begin
//	FLexer.IncludeHandler := IncludeHandler;
end;

function TDelphiParser.SetConstructor: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntSet);
		EatToken(ptOpenBracket);
		if CurrentTokenKind <> ptCloseBracket then
		begin
			SetElement;
			while CurrentTokenKind = ptComma do
			begin
				NextToken;
				SetElement;
			end;
		end;
		EatToken(ptCloseBracket);
end;

function TDelphiParser.Number: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntLiteral);
	Result.Attributes[anName] := CurrentToken.ValueText;
	Result.Attributes[anType] := AttributeValueToString(atNumeric);

	case CurrentTokenKind of
	ptFloat: NextToken;
	ptIntegerConst: NextToken;
	ptIdentifier: NextToken;
	else
		SynError('InvalidNumber');
	end;
end;

function TDelphiParser.ExpressionList: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExpressions);

		ParseExpression;
		if CurrentTokenKind = ptAssign then
		begin
			EatToken(ptAssign);
			ParseExpression;
		end;
		while CurrentTokenKind = ptComma do
		begin
			NextToken;
			ParseExpression;
			if CurrentTokenKind = ptAssign then
			begin
				EatToken(ptAssign);
				ParseExpression;
			end;
		end;
end;

function TDelphiParser.Designator: TSyntaxNode2;
begin
	Result := PoisonNode;

	ParseVariableReference;
end;

function TDelphiParser.MultiplicativeOperator: TSyntaxNode2;
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
		SynError('InvalidMultiplicativeOperator');
	end;
end;

function TDelphiParser.Factor: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Factor

Factor
	-> Atom
	-> UnaryOperator Factor

Atom
	-> Particle
			( '.' ExtendedIdent
			| '[' ExpressionList ']'
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
	-> NOT						ptNot: NotOp
	-> '+'						ptPlus: ntAdd
	-> '-'						ptMinus: UnaryMinus
	-> '@'						ptAt: AtExpression
	-> INHERITED

}
	Result := PoisonNode;

	case CurrentTokenKind of
	ptAsciiChar, ptStringLiteral: CharString;
	ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptInherited, ptPointerSymbol: Designator;
	ptOpenParen:
		begin
			RoundOpen;
			ExpressionList;
			RoundClose;
		end;
	ptIntegerConst, ptFloat: Number;
	ptNil: NilToken;
	ptMinus:
		begin
			UnaryMinus;
			Factor;
		end;
	ptNot:
		begin
			NotOp;
			Factor;
		end;
	ptPlus:
		begin
			NextToken;
			Factor;
		end;
	ptOpenBracket: SetConstructor;
	ptString: StringStatement;
	ptFunction, ptProcedure: AnonymousMethod;
	end;

	while CurrentTokenKind = ptOpenBracket do
		IndexOp;

	while CurrentTokenKind = ptPointerSymbol do
		PointerSymbol;

	if CurrentTokenKind = ptOpenParen then
		Factor;

	while CurrentTokenKind = ptDot do
	begin
		DotOp;
		Factor;
	end;
end;

function TDelphiParser.AdditiveOperator: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#AddOp

AddOp
	-> '+'
	-> '-'
	-> OR
	-> XOR

todo: rename to ParseAddOp
}
	Result := PoisonNode;

	case CurrentTokenKind of
	ptMinus: Result := TSyntaxNode2.Create(ntSub);
	ptOr: Result := TSyntaxNode2.Create(ntOr);
	ptPlus: Result := TSyntaxNode2.Create(ntAdd);
	ptXor: Result := TSyntaxNode2.Create(ntXor);
	else
		SynError('Unknown AdditiveOperator token kind');
		Exit;
	end;


	if CurrentTokenKind in [ptMinus, ptOr, ptPlus, ptXor] then
	begin
		Result.AddChild(EatToken);
	end
	else
	begin
		SynError('InvalidAdditiveOperator');
	end;
end;

function TDelphiParser.AddressOp: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAddr);
	EatToken(ptAddressOp);
end;

function TDelphiParser.AlignmentParameter: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAlignmentParam);
	ParseSimpleExpression;
end;

function TDelphiParser.ParseTerm: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Term

Term
	-> Factor (MulOp Factor)*
	Tested by: basic_cases.dat/ParseTerm
}
	Result := PoisonNode;

  Factor;
  while CurrentTokenKind in [ptAnd, ptDiv, ptMod, ptShl, ptShr, ptSlash, ptAsterisk] do
  begin
    MultiplicativeOperator;
    Factor;
  end;
end;

function TDelphiParser.RelativeOperator: TSyntaxNode2;
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

SimpleExpression - Backlinks: Expression, ExpressionOrRange
	-> Term (AddOp Term)*

Term - Backlinks: SimpleExpression
	-> Factor (MulOp Factor)*


	Tested by: basic_cases.dat/ParseSimpleExpression
}
	Result := TSyntaxNode2.Create(ntExpression);
	Result.AddChild(ParseTerm);

	while CurrentTokenKind in [ptMinus, ptOr, ptPlus, ptXor] do
	begin
		Result.AddChild(AdditiveOperator);
		Result.AddChild(ParseTerm);
	end;

	case CurrentTokenKind of
	ptAs:
		begin
			AsOp;
			ParseTypeId;
		end;
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

	Tested by: basic_cases.dat/ParseExpression
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

	Result.AddChild(expr);
end;

function TDelphiParser.VarDeclaration: TSyntaxNode2;
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
	Result.AddChild(VarNameList);
	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseType);

	// (PortabilityDirective)*
	Result.AddChild(HintDirectives);		// ntHintDirectives

	case CurrentToken.GenID of
	ptAbsolute: Result.AddChild(VarAbsolute);			// ABSOLUTE t1
	ptEquals:	Result.AddChild(VarEqual);				// = TypedConstant
	end;

	// (PortabilityDirective)*
	Result.AddChild(HintDirectives);		// ntHintDiretives
end;

function TDelphiParser.VarAbsolute: TSyntaxNode2;
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
	ExpectedEx(ptAbsolute);	//Expect the token's ExID to be ptAbsolute
	Result.AddChild(ParseExpression);
end;

function TDelphiParser.VarEqual: TSyntaxNode2;
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
   Result.AddChild(ConstantValueTyped); //todo: rename to ParseTypedConstant
end;

function TDelphiParser.VarNameList: TSyntaxNode2;
begin
	Result := PoisonNode;

  VarName;
  while CurrentTokenKind = ptComma do
    begin
      NextToken;
      VarName;
    end;
end;

function TDelphiParser.VarName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

	Result.AddChild(EatToken(ptIdentifier));
end;

procedure TDelphiParser.DirectiveCalling(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anCallingConvention] := CurrentToken.ValueText;

	case CurrentTokenExID of
	ptCdecl: NextToken;
	ptPascal: NextToken;
	ptRegister: NextToken;
	ptSafeCall: NextToken;
	ptStdCall: NextToken;
	else
		SynError('InvalidDirectiveCalling');
	end;
end;

function TDelphiParser.RecordVariant: TSyntaxNode2;
begin
	Result := PoisonNode;


  ConstantExpression;
  while (CurrentTokenKind = ptComma) do
  begin
    NextToken;
    ConstantExpression;
  end;
  EatToken(ptColon);
  EatToken(ptOpenParen);
  if CurrentTokenKind <> ptCloseParen then
  begin
    FieldList;
  end;
  EatToken(ptCloseParen);
end;

function TDelphiParser.VariantSection: TSyntaxNode2;
begin
	Result := PoisonNode;

  EatToken(ptCase);
  TagField;
  EatToken(ptOf);
  RecordVariant;
  while CurrentTokenKind = ptSemiColon do
  begin
    Semicolon;
    case CurrentTokenKind of
      ptEnd, ptCloseParen: Break;
    else
      RecordVariant;
    end;
  end;
end;

function TDelphiParser.TagField: TSyntaxNode2;
begin
	Result := PoisonNode;

  TagFieldName;
  case CurrentToken.TokenKind of
    ptColon:
      begin
        NextToken;
        TagFieldTypeName;
      end;
  end;
end;

function TDelphiParser.TagFieldName: TSyntaxNode2;
begin
	Result := PoisonNode;

	EatToken(ptIdentifier);
end;

function TDelphiParser.TagFieldTypeName: TSyntaxNode2;
begin
	Result := PoisonNode;

	ParseOrdinalType;
end;

function TDelphiParser.FieldDeclaration: TSyntaxNode2;
begin
	Result := PoisonNode;

	if CurrentTokenKind = ptOpenBracket then
		CustomAttribute;
	FieldNameList;
	EatToken(ptColon);
	ParseType;
	HintDirectives;
end;

function TDelphiParser.FieldList: TSyntaxNode2;
begin
	Result := PoisonNode;

	while CurrentTokenKind in [ptIdentifier, ptOpenBracket] do
	begin
		FieldDeclaration;
		Semicolon;
	end;
	if CurrentTokenKind = ptCase then
		VariantSection;
end;

function TDelphiParser.FieldName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;
	EatToken(ptIdentifier);
end;

function TDelphiParser.FieldNameList: TSyntaxNode2;
begin
	Result := PoisonNode;

	FieldName;
	while CurrentTokenKind = ptComma do
	begin
		NextToken;
		FieldName;
	end;
end;

procedure TDelphiParser.RecordType(ParentNode: TSyntaxNode2);
begin
	ParentNode.AddChild(EatToken(ptRecord));
	if CurrentTokenKind = ptSemicolon then
		Exit;

	if CurrentTokenExID = ptHelper then
		ParentNode.AddChild(ParseClassHelper);

	if CurrentTokenKind = ptOpenParen then
	begin
		ParentNode.AddChild(EatToken(ptOpenParen));
		ParentNode.AddChild(ParseAncestorList); // ntAncestorList
		ParentNode.AddChild(EatToken(ptCloseParen));
		if CurrentTokenKind = ptSemicolon then
			Exit;
	end;
	ParseClassMemberList;
	EatToken(ptEnd);

	ClassTypeEnd;
	RecordAlign;

	MoveMembersToVisibilityNodes(ParentNode);
end;

function TDelphiParser.FileType: TSyntaxNode2;
begin
	Result := PoisonNode;


	EatToken(ptFile);
	if CurrentTokenKind = ptOf then
	begin
		NextToken;
		ParseTypeId;
	end;
end;

function TDelphiParser.ParseFinalizationSection: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseFinalizationSection
	}
	Result := TSyntaxNode2.Create(ntFinalization);
	EatToken(ptFinalization);
	StatementList;
end;

function TDelphiParser.FinallyBlock: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntFinally);
	StatementList;
end;

function TDelphiParser.SetType: TSyntaxNode2;
begin
	Result := PoisonNode;

	EatToken(ptSet);
	EatToken(ptOf);
	ParseOrdinalType;
end;

procedure TDelphiParser.set_UseDefines(const Value: Boolean);
begin
//  FLexer.UseDefines := Value;
end;

function TDelphiParser.ArrayType: TSyntaxNode2;
begin
	Result := PoisonNode;


	EatToken(ptArray);
	ArrayBounds;
	EatToken(ptOf);
	ParseType;
end;

function TDelphiParser.ParseEnumeratedType: TSyntaxNode2;
var
	typeNode: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseEnumeratedType
	}
	typeNode := TSyntaxNode2.Create(ntType);

   typeNode.Attributes[anName] := AttributeValueToString(atEnum);
   if ScopedEnums then
      typeNode.Attributes[anVisibility] := 'scoped';
   EatToken(ptOpenParen);
   EnumeratedTypeItem;
   while CurrentTokenKind = ptComma do
   begin
      NextToken;
      EnumeratedTypeItem;
   end;
   EatToken(ptCloseParen);

	Result := typeNode;
end;

function TDelphiParser.ParseSubrangeType: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseSubrangeType
	}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := AttributeValueToString(atSubRange);

	ConstantExpression;
	if CurrentTokenKind = ptDotDot then
	begin
		NextToken;
		ConstantExpression;
	end;
end;

function TDelphiParser.RealIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;


	case CurrentTokenExID of
	ptReal48:	NextToken;
	ptReal:		NextToken;
	ptSingle:	NextToken;
	ptDouble:	NextToken;
	ptExtended:	NextToken;
	ptCurrency:	NextToken;
	ptComp:		NextToken;
	else
		SynError('InvalidRealIdentifier');
	end;
end;

function TDelphiParser.ParseRealType: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseRealType
	}
	Result := PoisonNode;


	case CurrentTokenKind of
	ptMinus: NextToken;
	ptPlus: NextToken;
	end;

	case CurrentTokenKind of
	ptFloat: NextToken;
	else
		ParseVariableReference;
	end;
end;

procedure TDelphiParser.RearrangeVarSection(const VarSect: TSyntaxNode2);
//var
//	Temp: TSyntaxNode2;
//	VarList, Variable, TypeInfo, ValueInfo: TSyntaxNode2;
begin
(*
	TODO: Figure out what this does and rewrite it.

	for VarList in VarSect.ChildNodes do
	begin
    TypeInfo := VarList.FindNode(ntType);
    ValueInfo := VarList.FindNode(ntValue);
    for Variable in VarList.ChildNodes do
    begin
      if Variable.NodeType <> ntName then
        Continue;
      Temp := FStack.Push(ntVariable);
      try
        Temp.AssignPositionFrom(Variable);
        FStack.AddChild(Variable.Clone);
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

function TDelphiParser.OrdinalIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;


	case CurrentTokenExID of
	ptBoolean:		NextToken;
	ptByte:			NextToken;
	ptBytebool:		NextToken;
	ptCardinal:		NextToken;
	ptChar:			NextToken;
//	ptDWord:			NextToken;
	ptInt64:			NextToken;
	ptInteger:		NextToken;
	ptLongBool:		NextToken;
	ptLongInt:		NextToken;
	ptLongWord:		NextToken;
	ptPChar:			NextToken;
	ptShortInt:		NextToken;
	ptSmallInt:		NextToken;
	ptWideChar:		NextToken;
	ptWord:			NextToken;
	ptWordbool:		NextToken;
	else
		SynError('InvalidOrdinalIdentifier');
	end;
end;

function TDelphiParser.ParseOrdinalType: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseOrdinalType
	}
	Result := PoisonNode;


	case CurrentTokenKind of
	ptIdentifier:
		begin
			case PeekTokenKind of
			ptDot:						ParseTypeId;
			ptOpenParen, ptDotDot:	ConstantExpression;
			else
				ParseTypeId;
			end;
		end;
	ptOpenParen: ParseEnumeratedType;
	ptOpenBracket:
		begin
			NextToken;
			ParseSubrangeType;
			EatToken(ptCloseBracket);
		end;
	else
		ConstantExpression;
	end;

	if CurrentTokenKind = ptDotDot then
	begin
		NextToken;
		ConstantExpression;
	end;
end;

function TDelphiParser.ParseVariableReference: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseVariableReference
	}
	Result := PoisonNode;


	case CurrentTokenKind of
	ptOpenParen:
		begin
			RoundOpen;
			ParseExpression;
			RoundClose;
			VariableTail;
		end;
	ptOpenBracket:
		begin
			SetConstructor;
		end;
	ptAddressOp:
		begin
			AddressOp;
			ParseVariableReference;
		end;
	ptDoubleAddressOp:
		begin
			NextToken;
			ParseVariableReference;
		end;
	ptInherited:
		begin
			InheritedVariableReference;
		end;
	else
		Variable;
	end;
end;

function TDelphiParser.ParseVisibilitySection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VisibilitySection

Returns: ntVisibilitySection


	VisibilitySection
	--> [Visibility]
			(VisibilitySectionContent)*

	Visibility
	--> STRICT PRIVATE
	--> STRICT PROTECTED
	--> PRIVATE
	--> PROTECTED
	--> PUBLIC
	--> PUBLISHED

	VisibilitySectionContent
	--> FieldSection
	--> MethodOrProperty
	--> ConstSection
	--> TypeSection

	FieldSection
	--> [[CLASS] VAR] (FieldDecl)*

	MethodOrProperty
	--> MethodHeading
	--> Property

	MethodHeading
	--> (PROCEDURE | FUNCTION)
			QualifiedIdent
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]		// FUNCTION only
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
	Tested by: basic_cases.dat/ParseVisibilitySection
}
	Result := TSyntaxNode2.Create(ntUnknown);
	Result.Attributes[anName] := 'ntVisibilitySection';

	// Parse optional Visibility
	if CurrentTokenExID = ptPrivate then
	begin
		Result := TSyntaxNode2.Create(ntPrivate);
		Result.AddChild(ExpectedEx(ptPrivate));
	end
	else if CurrentTokenExID = ptProtected then
	begin
		Result := TSyntaxNode2.Create(ntProtected);
		Result.AddChild(ExpectedEx(ptProtected));
	end
	else if CurrentTokenExID = ptPublic then
	begin
		Result := TSyntaxNode2.Create(ntPublic);
		Result.AddChild(ExpectedEx(ptPublic));
	end
	else if CurrentTokenExID = ptPublished then
	begin
		Result := TSyntaxNode2.Create(ntPublished);
		Result.AddChild(ExpectedEx(ptPublished));
	end
	else if (CurrenttokenExID = ptStrict) and (PeekTokenExID = ptPrivate) then
	begin
		Result := TSyntaxNode2.Create(ntStrictPrivate);
		Result.AddChild(ExpectedEx(ptStrict));
		Result.AddChild(ExpectedEx(ptPrivate));
	end
	else if (CurrenttokenExID = ptStrict) and (PeekTokenExID = ptProtected) then
	begin
		Result := TSyntaxNode2.Create(ntStrictProtected);
		Result.AddChild(ExpectedEx(ptStrict));
		Result.AddChild(ExpectedEx(ptProtected));
	end
	else
	begin
		// Visibilty is optional
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
	VisibilitySectionContent
	--> FieldSection
	--> MethodOrProperty
	--> ConstSection
	--> TypeSection

	ConstSection
	--> (CONST|RESOURCESTRING) (ConstantDecl)+

	TypeSection
	--> TYPE (TypeDecl)+

	MethodOrProperty
	--> MethodHeading
	--> Property

	Property
	--> [CLASS]
			PROPERTY Ident
			['[' (Parameter [';'])+ ']']
			[':' MethodReturnType]
			(PropertyDirective)*
			';'

	MethodHeading
	--> (PROCEDURE | FUNCTION)
			QualifiedIdent
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]		// FUNCTION only
			[';']
			(Directive [';'])*

	FieldSection
	--> [[CLASS] VAR] (FieldDecl)*
	Tested by: basic_cases.dat/ParseVisibilitySectionContent
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

function TDelphiParser.Variable: TSyntaxNode2;
begin
//	Attention: could also came from proc_call !!
	Result := PoisonNode;

	ParseQualifiedIdentifier;
	VariableTail;
end;

function TDelphiParser.VariableTail: TSyntaxNode2;
begin
	Result := PoisonNode;


	case CurrentTokenKind of
	ptOpenParen:
		begin
			RoundOpen;
			ExpressionList;
			RoundClose;
		end;
	ptOpenBracket:		IndexOp;
	ptPointerSymbol:	PointerSymbol;
	ptLessThan:
		begin
			NextToken;
			TypeArgs;

			if CurrentTokenKind = ptGreaterThan then
			begin
				NextToken;
				TypeArgs;
				EatToken(ptGreaterThan);
				case CurrentTokenKind of
				ptAddressOp, ptDoubleAddressOp, ptIdentifier:
					begin
						ParseVariableReference;
					end;
				ptDot, ptPointerSymbol, ptOpenParen, ptOpenBracket:
					begin
						VariableTail;
					end;
				end;
			end;
		end;
	end;

	case CurrentTokenKind of
	ptOpenParen, ptOpenBracket, ptPointerSymbol:	VariableTail;
	ptDot:
		begin
			DotOp;
			Variable;
		end;
	ptAs:
		begin
			AsOp;
			ParseSimpleExpression;
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
	Tested by: basic_cases.dat/ParseInterfaceType
}
	Result := PoisonNode;

	case CurrentTokenKind of
	ptInterface:
		begin
			Result := TSyntaxNode2.Create(ntType);
			Result.Attributes[anType] := AttributeValueToString(atInterface);
		end;
	ptDispInterface:
		begin
			Result := TSyntaxNode2.Create(ntType);
			Result.Attributes[anType] := AttributeValueToString(atDispInterface);
		end;
	end;

   case CurrentTokenKind of
   ptInterface:		NextToken;
   ptDispInterface:	NextToken;
   else
      SynError('InvalidInterfaceType');
   end;

   case CurrentTokenKind of
   ptEnd: NextToken; { Direct descendant without new members }
   ptOpenParen:
      begin
         InterfaceHeritage;
         case CurrentTokenKind of
         ptEnd:
            begin
               NextToken; { No new members }
            end;
         ptSemiColon: ; { No new members }
         else
            begin
               if CurrentTokenKind = ptOpenBracket then
               begin
                  InterfaceGUID;
               end;
               InterfaceMemberList(Result);
               EatToken(ptEnd);
            end;
         end;
      end;
   else
      if CurrentTokenKind = ptOpenBracket then
      begin
         InterfaceGUID;
      end;
      InterfaceMemberList(Result); { Direct descendant }
      EatToken(ptEnd);
   end;
end;

procedure TDelphiParser.InterfaceMemberList(ParentNode: TSyntaxNode2);
begin
	while CurrentTokenKind in [ptOpenBracket, ptFunction, ptProcedure, ptProperty] do
	begin
		while CurrentTokenKind = ptOpenBracket do
			CustomAttribute;

		ClassMethodOrProperty;
	end;
end;

function TDelphiParser.ParseClassType: TSyntaxNode2;
var
	startIndex: Integer;
begin
{
Returns
	ntType(@anType = atClass)

Grammer
========

ClassType
	-> CLASS
			[ABSTRACT | SEALED]
			[ '(' (QualifiedIdent [','])+ ')' ]

		The remainder is optional, but only if the base class is specified and lookahead shows that the next token is a semicolon

		(VisibilitySection)*
		END

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

	Tested by: basic_cases.dat/ParseClassType
}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := AttributeValueToString(atClass);
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

	while (CurrentTokenExID <> ptEnd) and (CurrentTokenKind <> ptEof) do
	begin
		// Parse members defensively: malformed class bodies can leave the token index unchanged.
		// If no progress is made, advance one token to avoid infinite loops and keep recovery moving.
		startIndex := FCurrent;

      // what happens if we have no visiblity section?
		Result.AddChild(ParseVisibilitySection);
		if FCurrent = startIndex then
		begin
			DoMessage('Skipping token to recover inside class body', CurrentToken.Line, CurrentToken.Column);
			NextToken;
		end;
	end;

	MoveMembersToVisibilityNodes(Result);
end;

function TDelphiParser.ParseClassHelper: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseClassHelper
	}
	Result := TSyntaxNode2.Create(ntHelper);
	Result.AddChild(EatToken(ptHelper));

	if CurrentTokenKind = ptOpenParen then
	begin
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseAncestorList); // ntAncestorList
		Result.AddChild(EatToken(ptCloseParen));
	end;
	Result.AddChild(EatToken(ptFor));
	Result.AddChild(ParseTypeId);
end;

function TDelphiParser.ClassVisibility: TSyntaxNode2;
var
	isStrict: boolean;
begin
	Result := PoisonNode;


	isStrict := (CurrentTokenExID = ptStrict);
	if isStrict then
		ExpectedEx(ptStrict);

	while CurrentTokenExID in [ptAutomated, ptPrivate, ptProtected, ptPublic, ptPublished] do
	begin
		// Recovery: if someone writes "private:" or "public," we must still consume tokens,
		// otherwise this loop never advances and the parser hangs.
		if PeekToken(1).DirectiveID in [ptColon, ptComma] then
		begin
			ExpectedEx(CurrentTokenExID);
			if CurrentTokenKind in [ptColon, ptComma] then
				NextToken;
			Exit;
		end;

		case CurrentTokenExID of
			ptAutomated:
				begin
					VisibilityAutomated;
				end;
			ptPrivate:
				begin
					if IsStrict then
						VisibilityStrictPrivate
					else
						VisibilityPrivate;
				end;
			ptProtected:
				begin
					if IsStrict then
						VisibilityStrictProtected
					else
						VisibilityProtected;
				end;
			ptPublic:
				begin
					VisibilityPublic;
				end;
			ptPublished:
				begin
					VisibilityPublished;
				end;
		end;
	end;
end;

function TDelphiParser.VisibilityAutomated: TSyntaxNode2;
begin
	Result := PoisonNode;


	ExpectedEx(ptAutomated);
end;

function TDelphiParser.VisibilityStrictPrivate: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntStrictPrivate);
	Result.Attributes[anVisibility] := AttributeValueToString(atTrue);

	Result.AddChild(ExpectedEx(ptPrivate));
end;

function TDelphiParser.VisibilityPrivate: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntPrivate);
	Result.Attributes[anVisibility] :=  AttributeValueToString(atTrue);
	ExpectedEx(ptPrivate);
end;

function TDelphiParser.VisibilityStrictProtected: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntStrictProtected);
	Result.Attributes[anVisibility] :=  AttributeValueToString(atTrue);

	Result.AddChild(ExpectedEx(ptProtected));
end;

function TDelphiParser.VisibilityProtected: TSyntaxNode2;
//var
//	Temp: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntProtected);
	Result.Attributes[anVisibility] := AttributeValueToString(atTrue);
	Result.AddChild(ExpectedEx(ptProtected));
end;

function TDelphiParser.VisibilityPublic: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntPublic);
	Result.Attributes[anVisibility] := AttributeValueToString(atTrue);

	Result.AddChild(ExpectedEx(ptPublic));
end;

function TDelphiParser.VisibilityPublished: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntPublished);
	Result.Attributes[anVisibility] := AttributeValueToString(atTrue);

	Result.AddChild(ExpectedEx(ptPublished));
end;

function TDelphiParser.VisibilityUnknown: TSyntaxNode2;
begin
	Result := PoisonNode;


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


	Tested by: basic_cases.dat/ParseClassMemberList
}
	Result := PoisonNode; // ntClassMemberList

	while (CurrentTokenKind in [ptClass, ptConstructor, ptDestructor, ptFunction,
		ptIdentifier, ptProcedure, ptProperty, ptType, ptOpenBracket, ptVar, ptConst, ptCase]) or (CurrentTokenExID = ptStrict) do
	begin
		ClassVisibility;

		if CurrentTokenKind = ptOpenBracket then
			CustomAttribute;

		if (CurrentTokenKind = ptIdentifier) and
				not (CurrentTokenExID in [ptPrivate, ptProtected, ptPublished, ptPublic, ptStrict]) then
		begin
			if PeekTokenKind = ptEquals then
				ConstantDeclaration
			else
			begin
				ClassField;
				if CurrentTokenKind = ptEquals then
				begin
					NextToken;
					ParseTypedConstant;
				end;
			end;

			Semicolon;
		end
		else if CurrentTokenKind in [ptClass, ptConstructor, ptDestructor, ptFunction,
				ptProcedure, ptProperty, ptVar, ptConst] then
		begin
			ClassMethodOrProperty;
		end;
		if CurrentTokenKind = ptType then
			ParseTypeSection;
		if CurrentTokenKind = ptCase then
		begin
			VariantSection;
		end;
	end;
end;

function TDelphiParser.ClassMethodOrProperty: TSyntaxNode2;
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
    ptProperty: ParseProperty;
    ptVar, ptThreadVar:
      begin
        if CurrentTokenKind = ptClass then
          ClassClass;

        NextToken;
        while (CurrentTokenKind = ptIdentifier) and (CurrentTokenExID = ptUnknown) do
        begin
          ClassField;
          Semicolon;
        end;
      end;
    ptConst:
      begin
        if CurrentTokenKind = ptClass then
          ClassClass;

        NextToken;
        while (CurrentTokenKind = ptIdentifier) and (CurrentTokenExID = ptUnknown) do
        begin
          ConstantDeclaration;
          Semicolon;
        end;
      end;
  else
    begin
      ClassMethodHeading;
    end;
  end;
end;

function TDelphiParser.ClassProperty: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntProperty);

     if CurrentTokenKind = ptClass then
       ClassClass;

     EatToken(ptProperty);
     PropertyName(Result);
     case CurrentTokenKind of
       ptColon, ptOpenBracket:
         begin
           PropertyInterface;
         end;
     end;
     PropertySpecifiers;
     case CurrentTokenExID of
       ptDefault:
         begin
           PropertyDefault;
           Semicolon;
         end;
     end;
end;

procedure TDelphiParser.PropertyName(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anName] := CurrentToken.ValueText;

	EatToken(ptIdentifier);
end;

function TDelphiParser.ClassField: TSyntaxNode2;
//var
//	Fields, Temp: TSyntaxNode2;
//	TypeInfo, TypeArgs: TSyntaxNode2;
//	Field: TSyntaxNodeOrToken;
begin
	Result := PoisonNode;


{
	Fields := TSyntaxNode2.Create(ntFields);
	try
		FStack.Push(Fields);
		try
			if CurrentTokenKind = ptOpenBracket then
				CustomAttribute;
			FieldNameList;
			EatToken(ptColon);
			TypeKind;
			HintDirectives;

		finally
			FStack.Pop;
		end;

		TypeInfo := Fields.FindNode(ntType);
		TypeArgs := Fields.FindNode(ntTypeArgs);
		for Field in Fields.ChildNodes do
		begin
			if Field.AsNode.NodeType <> ntName then
				Continue;

			Temp := FStack.Push(ntField);
			try
				Temp.AssignPositionFrom(Field.AsNode);

				FStack.AddChild(Field.AsNode.Clone);
				TypeInfo := TypeInfo.Clone;
				if Assigned(TypeArgs) then
					TypeInfo.AddChild(TypeArgs.Clone);
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
  EatToken(ptObject);
  case CurrentTokenKind of
    ptEnd:
      begin
        ObjectTypeEnd;
        NextToken; { Direct descendant without new members }
      end;
    ptOpenParen:
      begin
        ObjectHeritage;
        case CurrentTokenKind of
          ptEnd:
            begin
              EatToken(ptEnd);
              ObjectTypeEnd;
            end;
          ptSemiColon: ObjectTypeEnd;
        else
          begin
            ObjectMemberList(ParentNode); { Direct descendant }
            EatToken(ptEnd);
            ObjectTypeEnd;
          end;
        end;
      end;
  else
    begin
      ObjectMemberList(ParentNode); { Direct descendant }
      EatToken(ptEnd);
      ObjectTypeEnd;
    end;
  end;
end;

function TDelphiParser.ObjectHeritage: TSyntaxNode2;
begin
	Result := PoisonNode;


  EatToken(ptOpenParen);
  ParseAncestorList;
  EatToken(ptCloseParen);
end;

procedure TDelphiParser.ObjectMemberList(ParentNode: TSyntaxNode2);
begin
	{jdj added ptProperty-call to ObjectProperty 02/07/2001}
  ObjectVisibility;
  while CurrentTokenKind in [ptConstructor, ptDestructor, ptFunction, ptIdentifier,
    ptProcedure, ptProperty] do
  begin
    while CurrentTokenKind = ptIdentifier do
    begin
      ObjectField;
      Semicolon;
      ObjectVisibility;
    end;
    while CurrentTokenKind in [ptConstructor, ptDestructor, ptFunction, ptProcedure, ptProperty] do
    begin
      case CurrentTokenKind of
        ptConstructor, ptDestructor, ptFunction, ptProcedure: ObjectMethodHeading(ParentNode);
        ptProperty: ParseProperty;
      end;
    end;
    ObjectVisibility;
  end;
end;

function TDelphiParser.ObjectVisibility: TSyntaxNode2;
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
            VisibilityPrivate;
          end;
        ptProtected:
          begin
            VisibilityProtected;
          end;
        ptPublic:
          begin
            VisibilityPublic;
          end;
      end;
    end;
  end;
end;

function TDelphiParser.ObjectField: TSyntaxNode2;
begin
	Result := PoisonNode;


  ParseIdentifierList;
  EatToken(ptColon);
  ParseType;
  HintDirectives;
end;

function TDelphiParser.ClassReferenceType: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := AttributeValueToString(atClassof);
	  EatToken(ptClass);
	  EatToken(ptOf);
	  ParseTypeId;
end;

function TDelphiParser.VariantIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;


  case CurrentTokenExID of
    ptOleVariant:
      begin
        NextToken;
      end;
    ptVariant:
      begin
        NextToken;
      end;
  else
    begin
      SynError('InvalidVariantIdentifier');
    end;
  end;
end;

function TDelphiParser.ParseProceduralType: TSyntaxNode2;
var
  TheTokenID: TptTokenKind;
begin
	{
	Tested by: basic_cases.dat/ParseProceduralType
	}
   Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := CurrentToken.ValueText;
     case CurrentTokenKind of
       ptFunction:
         begin
           NextToken;
           if CurrentTokenKind = ptOpenParen then
           begin
             FormalParameterList;
           end;
           EatToken(ptColon);
           ReturnType;
         end;
       ptProcedure:
         begin
           NextToken;
           if CurrentTokenKind = ptOpenParen then
           begin
             FormalParameterList;
           end;
         end;
     else
       begin
         SynError('InvalidProceduralType');
       end;
     end;
     if CurrentTokenKind = ptOf then
       ProceduralDirectiveOf;

     case CurrentTokenKind of
       ptSemiColon: TheTokenID := PeekToken(1).DirectiveID;
     else
       TheTokenID := CurrentTokenExID;
     end;
     while TheTokenID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
       ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
       ptReintroduce, ptSafeCall, ptStdCall, ptVirtual, ptStatic, ptInline, ptVarargs] do
     // DR 2001-11-14 no checking for deprecated etc. since it's captured by the typedecl
     begin
       if CurrentTokenKind = ptSemiColon then Semicolon;
       ProceduralDirective(Result);
       case CurrentTokenKind of
         ptSemiColon: TheTokenID := PeekToken(1).DirectiveID;
       else
         TheTokenID := CurrentTokenExID;
       end;
     end;

     if CurrentTokenKind = ptOf then
       ProceduralDirectiveOf;
end;

function TDelphiParser.StringConst: TSyntaxNode2;
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
			StringConstSimple;
			while CurrentTokenKind in [ptStringConst, ptAsciiChar] do
				StringConstSimple;
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

function TDelphiParser.StringConstSimple: TSyntaxNode2;
begin
  //TODO support ptAsciiChar
	Result := TSyntaxNode2.Create(ntLiteral);
	Result[anName] := CurrentToken.ValueText;

  NextToken;
end;

function TDelphiParser.StringIdentifier: TSyntaxNode2;
begin
	Result := PoisonNode;

  case CurrentTokenExID of
    ptAnsiString:
      begin
        NextToken;
      end;
    ptShortString:
      begin
        NextToken;
      end;
    ptWideString:
      begin
        NextToken;
      end;
  else
    begin
      SynError('InvalidStringIdentifier');
    end;
  end;
end;

function TDelphiParser.StringType: TSyntaxNode2;
begin
	Result := PoisonNode;

  case CurrentTokenKind of
    ptString:
      begin
        NextToken;
        if CurrentTokenKind = ptOpenBracket then
        begin
          NextToken;
          ConstantExpression;
          EatToken(ptCloseBracket);
        end;
      end;
  else
    begin
      ParseVariableReference;
    end;
  end;
end;

function TDelphiParser.PointerSymbol: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntDeref);
		EatToken(ptPointerSymbol);
end;

function TDelphiParser.ParsePointerType: TSyntaxNode2;
begin
{
	PointerType
	-> '^' Type

	Backlinks: Type

	Returns
		ntType(@anType=atPointer)
	Tested by: basic_cases.dat/ParsePointerType
}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := AttributeValueToString(atPointer);
	Result.AddChild(EatToken(ptPointerSymbol));
	Result.AddChild(ParseType); // was ParseTypeId in the old code
end;

function TDelphiParser.ParseStringType: TSyntaxNode2;
begin
{
	StringType
		-> STRING								string
		-> STRING '[' Expression ']'		string[67]
	Tested by: basic_cases.dat/ParseStringType
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

function TDelphiParser.PoisonNode: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntUnknown);
	Result.AddChild(EatToken);
	Result[anName] := 'Not implemented yet';
end;

function TDelphiParser.ParseStructuredType: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseStructuredType
	}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := CurrentToken.ValueText;

	case CurrentTokenKind of
	ptArray: 	Result.AddChild(ArrayType);
	ptSet:		Result.AddChild(SetType);
	ptFile:		Result.AddChild(FileType);
	ptRecord:	RecordType(Result);
	ptObject:	ObjectType(Result);
	else
		SynError('InvalidStructuredType');
	end;
end;

function TDelphiParser.ParseSimpleType: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseSimpleType
	}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := CurrentToken.ValueText;

	case CurrentTokenKind of
	ptMinus: NextToken;
	ptPlus: NextToken;
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

function TDelphiParser.RecordAlign: TSyntaxNode2;
begin
{
	If the current token is for the ALIGN directive, then return that token
}
	Result := PoisonNode;

  if CurrentTokenExID = ptAlign then
  begin
    NextToken;
    RecordAlignValue(Result);
  end;
end;

procedure TDelphiParser.RecordAlignValue(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anAlign] := CurrentToken.ValueText;
	ParentNode.AddChild(EatToken(ptIntegerConst));
end;

function TDelphiParser.RecordFieldConstant: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntField);
	Result[anName] := CurrentToken.ValueText;
	Result.Attributes[anType] := AttributeValueToString(atName);

	Result.AddChild(EatToken(ptIdentifier));
	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseTypedConstant);
end;

function TDelphiParser.RecordConstant: TSyntaxNode2;
begin
	Result := PoisonNode;

  EatToken(ptOpenParen);
  RecordFieldConstant;
  while (CurrentTokenKind = ptSemiColon) do
  begin
    Semicolon;
    if CurrentTokenKind <> ptCloseParen then
      RecordFieldConstant;
  end;
  EatToken(ptCloseParen);
end;

function TDelphiParser.RecordConstraint: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntRecordConstraint);
	Result.AddChild(EatToken(ptRecord));
end;

function TDelphiParser.ArrayConstant: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExpressions);

		EatToken(ptOpenParen);

		ParseTypedConstant;
		if CurrentTokenKind = ptDotDot then
		begin
			NextToken;
			ParseTypedConstant;
		end;

		while (CurrentTokenKind = ptComma) do
		begin
			NextToken;
			ParseTypedConstant;
			if CurrentTokenKind = ptDotDot then
			begin
				NextToken;
				ParseTypedConstant;
			end;
		end;
		EatToken(ptCloseParen);
end;

function TDelphiParser.ArrayDimension: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntDimension);
	Result.AddChild(ParseOrdinalType);
end;

procedure TDelphiParser.ClassForward(ParentNode: TSyntaxNode2);
begin
  ParentNode.Attributes[anForwarded] := AttributeValueToString(atTrue);
  EatToken(ptClass);
end;

procedure TDelphiParser.DispInterfaceForward(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anForwarded] := AttributeValueToString(atTrue);
	ParentNode.AddChild(EatToken(ptDispInterface));
end;

function TDelphiParser.DotOp: TSyntaxNode2;
begin
   Result := TSyntaxNode2.Create(ntDot);
   Result.AddChild(EatToken(ptDot));
end;

procedure TDelphiParser.InterfaceForward(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anForwarded] := AttributeValueToString(atTrue);
	ParentNode.AddChild(EatToken(ptInterface));
end;

function TDelphiParser.ObjectForward: TSyntaxNode2;
begin
	Result := PoisonNode;

  EatToken(ptObject);
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

	Tested by: basic_cases.dat/ParseTypeDeclaration
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

function TDelphiParser.ExplicitType: TSyntaxNode2;
begin
	Result := PoisonNode;

	EatToken(ptType);
end;

function TDelphiParser.ParseType: TSyntaxNode2;
var
	aheadTokenKind: TptTokenKind;
	packedToken: TSyntaxToken;
begin
{
	<Type>
		-> EnumeratedType					ParseEnumeratedType
		-> ExpressionOrRange
		-> ArrayType						ParseStructuredType
		-> SetType							ParseStructuredType
		-> FileType							ParseStructuredType
		-> RecordHelperType
		-> RecordType						ParseStructuredType
		-> PointerType						ParsePointerType
		-> StringType
		-> ProcedureType
		-> ClassHelperType
		-> ClassOfType
		-> ClassType
		-> InterfaceType
		-> PackedType

	Backlinks: ArrayType, ConstantDecl, FieldDecl, PackedType, PointerType, SetType, TypeDecl, VarDecl
	Note: Delphi assumes that a Type starting with '(' is an enum, not an expression.
	Tested by: basic_cases.dat/ParseType
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
			Result.AddChild(packedToken);
		end;
	ptFunction, ptProcedure: Result := ParseProceduralType;
	ptIdentifier:
		begin
//			InitAhead;
			NextToken;
			Result := ParseSimpleExpression;
			aheadTokenKind := CurrentTokenKind;
//			RestoreAhead;
			if aheadTokenKind = ptDotDot then
				ParseSubrangeType
			else
				ParseTypeId;
		end;
	ptPointerSymbol: Result := ParsePointerType;          // ^Type
	ptString: Result := ParseStringType;
	ptClass: Result := ParseClassType;
	else
		// E2029 %s expected but %s found
		Result := SynErrorFmt(SE2029, ['Type', CurrentToken.Text]);
	end;
end;

function TDelphiParser.TypeArgs: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntTypeArgs);

	  ParseType;
	  while CurrentTokenKind = ptComma do
	  begin
	    NextToken;
	    ParseType;
	  end;
end;

function TDelphiParser.ParseTypedConstant: TSyntaxNode2;
var
	s: string;
	tok: TSyntaxToken;
begin
	{
	Tested by: basic_cases.dat/ParseTypedConstant
	}
	Result := TSyntaxNode2.Create(ntExpression);
	s := '';

	// Keep this fast and robust for declaration parsing: consume constant value
	// tokens until a declaration boundary (directive/semicolon/EOF).
	while not (CurrentTokenKind in [ptSemicolon, ptSemiColon, ptPlatform, ptLibrary, ptDeprecated, ptExperimental, ptEof]) do
	begin
		tok := EatToken;
		s := s + tok.ValueText;
	end;

	Result[anValueText] := s;
end;

function TDelphiParser.ParseTypeId: TSyntaxNode2;
//var
//	TypeNode, InnerTypeNode: TSyntaxNode2;
//	SubNode: TSyntaxNodeOrToken;
//	TypeName, InnerTypeName: string;
//	i: integer;
begin
	{
	Tested by: basic_cases.dat/ParseTypeId
	}
	Result := PoisonNode;


{
	TODO: Convert this to the node and token are first class citizens, nodes are returned model.


	TypeNode := FStack.Push(ntType);
	try
		TypeSimple;

		while CurrentTokenKind = ptDot do
		begin
			EatToken(ptDot);
			TypeSimple;
		end;

		if CurrentTokenKind = ptOpenParen then
		begin
			EatToken(ptOpenParen);
			SimpleExpression;
			EatToken(ptCloseParen);
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

function TDelphiParser.ConstantExpression: TSyntaxNode2;
var
	expressionMethod: TTreeBuilderMethod;
begin
	Result := PoisonNode;

	expressionMethod := ParseSimpleExpression;
	BuildExpressionTree(expressionMethod);
end;

function TDelphiParser.ResourceDeclaration: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntResourceString);

	ParseConstantName;
	EatToken(ptEquals);

	ResourceValue;

	HintDirectives;
end;

function TDelphiParser.ResourceValue: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntValue);
	  CharString;
	  while CurrentTokenKind = ptPlus do
	  begin
	    NextToken;
	    CharString;
	  end;
end;

function TDelphiParser.ConstantDeclaration: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntConstant);

	ParseConstantName;

	case CurrentTokenKind of
	ptEquals: ConstantEqual;
	ptColon:  ConstantColon;
	else
		SynError('InvalidConstantDeclaration');
	end;

	HintDirectives;
end;

function TDelphiParser.ConstantColon: TSyntaxNode2;
begin
	Result := PoisonNode;

	EatToken(ptColon);
	ConstantType;
	EatToken(ptEquals);
	ConstantValueTyped;
end;

function TDelphiParser.ConstantEqual: TSyntaxNode2;
begin
	Result := PoisonNode;

	EatToken(ptEquals);
	ConstantValue;
end;

function TDelphiParser.ConstantValue: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntValue);
	ParseExpression;
end;

function TDelphiParser.ConstantValueTyped: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntValue);
	ParseTypedConstant;
end;

function TDelphiParser.ParseConstantName: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseConstantName
	}
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ConstantType: TSyntaxNode2;
begin
	Result := PoisonNode;

	ParseType;
end;

function TDelphiParser.LabelId: TSyntaxNode2;
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
		SynError('InvalidLabelId');
	end;
end;

function TDelphiParser.ProcedureDeclarationSection: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntMethod);

   if CurrentTokenKind = ptClass then
   begin
      ClassMethod(Result);
   end;

   case CurrentTokenKind of
   ptConstructor:	Result.AddChild(ProcedureProcedureName);
   ptDestructor:	ProcedureProcedureName;
   ptProcedure:	ProcedureProcedureName;
   ptFunction:		FunctionMethodDeclaration;
   ptIdentifier:
      begin
         if CurrentTokenExID = ptOperator then
         begin
            FunctionMethodDeclaration;
         end
         else
            SynError('InvalidProcedureDeclarationSection');
      end;
   else
      SynError('InvalidProcedureDeclarationSection');
   end;
end;

function TDelphiParser.LabelDeclarationSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#LabelDeclSection

LabelDeclSection [^]
	-> LABEL (LabelId [','])+ ';'
}
	Result := TSyntaxNode2.Create(ntLabel);
	Result.AddChild(EatToken(ptLabel));

	// (LabelId [','])+
	while IsPossibleLabelID do
	begin
		Result.AddChild(LabelId);
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
	ptExport, ptFar, ptNear:	Directive16Bit;
	ptExternal:						ExternalDirective;
	ptDynamic, ptMessage, ptOverload, ptOverride, ptReintroduce, ptVirtual: DirectiveBinding(ParentNode);
	ptAssembler:					NextToken;
	ptStatic:						NextToken;
	ptInline:						DirectiveInline(ParentNode);
	ptDeprecated:					DirectiveDeprecated;
	ptLibrary:						DirectiveLibrary;
	ptPlatform:						DirectivePlatform;
	ptLocal:							DirectiveLocal;
	ptVarargs:						DirectiveVarargs;
	ptFinal, ptExperimental, ptDelayed:	NextToken;
	else
		SynError('InvalidProceduralDirective');
	end;
end;

function TDelphiParser.ParseMethodHeading: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodHeading

MethodHeading
		// Recognizer accepts [CLASS] and constructor/destructor/operator starts,
		// but ParseMethodHeading currently implements procedure/function headings.
		-> [CLASS] (PROCEDURE | FUNCTION) QualifiedIdent
					['(' (Parameter [';'])* ')']
					[':' MethodReturnType]		// FUNCTION only
					[';']
					(Directive [';'])*


	Tested by: basic_cases.dat/ParseMethodHeading
}
	Result := TSyntaxNode2.Create(ntMethod);

	case CurrentTokenKind of
	ptFunction: FunctionHeading(Result);
	ptProcedure: ProcedureHeading(Result);
	else
		SynError('InvalidExportedHeading');
	end;

	if CurrentTokenKind = ptSemiColon then
		Semicolon;

     //TODO: Add FINAL
     while CurrentTokenExID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
       ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
       ptReintroduce, ptSafeCall, ptStdCall, ptVirtual,
       ptDeprecated, ptLibrary, ptPlatform, ptLocal, ptVarargs,
       ptStatic, ptInline, ptAssembler, ptForward, ptDelayed] do
     begin
       case CurrentTokenExID of
         ptAssembler: NextToken;
         ptForward: Result.AddChild(ForwardDeclaration);
       else
         ProceduralDirective(Result);
       end;
       if CurrentTokenKind = ptSemiColon then Semicolon;
     end;
end;

procedure TDelphiParser.FunctionHeading(ParentNode: TSyntaxNode2);
begin
   ParentNode.Attributes[anKind] := AttributeValueToString(atFunction);

  EatToken(ptFunction);
  FunctionProcedureName;
  if CurrentTokenKind = ptOpenParen then
  begin
    FormalParameterList;
  end;
  if CurrentTokenKind = ptColon then
  begin
    EatToken(ptColon);
    ReturnType;
  end;
end;

procedure TDelphiParser.ProcedureHeading(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anKind] := AttributeValueToString(atProcedure);

	EatToken(ptProcedure);
	FunctionProcedureName;
	if CurrentTokenKind = ptOpenParen then
	begin
		FormalParameterList;
	end;
end;

function TDelphiParser.ParseVarSection: TSyntaxNode2;
//var
//	varSect: TSyntaxNode2;
begin
{
VarSection [^]
	-> (VAR | THREADVAR) (VarDecl)+
	Tested by: basic_cases.dat/ParseVarSection
}
	case CurrentTokenKind of
	ptVar:
		begin
			Result := TSyntaxNode2.Create(ntVariables);
		end;
	ptThreadVar:
		begin
			Result := TSyntaxNode2.Create(ntVariables);

		end;
	else
		SynError('ParseVarSection');
		Exit(nil);
	end;


{
	varSect := TSyntaxNode2.Create(ntUnknown);
	try
		FStack.Push(ntVariables);

		FStack.Push(varSect);
		try
			case CurrentTokenKind of
			ptThreadVar: NextToken;
			ptVar:       NextToken;
			else
				SynError('InvalidVarSection');
			end;

			while CurrentTokenKind in [ptIdentifier, ptOpenBracket] do
			begin
				if CurrentTokenKind = ptOpenBracket then
					CustomAttribute
				else
				begin
					VarDeclaration;
					Semicolon;
				end;
			end;
		finally
			FStack.Pop;
		end;

		RearrangeVarSection(varSect);
		FStack.Pop;
	finally
		varSect.Free;
	end;
}
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

function TDelphiParser.IsPossibleMethodHeading: Boolean;
begin
{
MethodHeading
		// Recognizer accepts [CLASS] and constructor/destructor/operator starts,
		// but ParseMethodHeading currently implements procedure/function headings.
		-> [CLASS] (PROCEDURE | FUNCTION) QualifiedIdent
					['(' (Parameter [';'])* ')']
					[':' MethodReturnType]		// FUNCTION only
					[';']
					(Directive [';'])*
}
	// Check for optional `class` prefix
	if CurrentTokenKind = ptClass then
		Result := PeekTokenKind in [ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator]
	else
		Result := CurrentTokenKind in [ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator];
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
   raise ENotImplementedException.Create('TDelphiParser.IsPossibleVarSection');
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
			--> (PROCEDURE | FUNCTION)
					QualifiedIdent
					['(' (Parameter [';'])* ')']
					[':' MethodReturnType]		// FUNCTION only
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
	Tested by: basic_cases.dat/ParseTypeSection
}
	Result := TSyntaxNode2.Create(ntTypeSection);
	Result.AddChild(EatToken(ptType));	// always produce a token (missing on error)

// Grammar says (TypeDecl)+, so parse one unconditionally, then loop.
	typeDecl := ParseTypeDeclaration; // ntTypeDecl
	Result.AddChild(typeDecl);

	while IsPossibleTypeDeclaration do
		Result.AddChild(ParseTypeDeclaration); // ntTypeDecl
end;

function TDelphiParser.TypeSimple: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anName] := CurrentToken.ValueText;

	case CurrentToken.GenID of
	ptBoolean, ptByte, ptChar,
	//ptDWord, DWORD is not a built-in type; it is in Windows.pas
	ptInt64, ptInteger, ptLongInt,
	ptLongWord, ptPChar, ptShortInt, ptSmallInt, ptWideChar, ptWord: OrdinalIdentifier;
	ptComp, ptCurrency, ptDouble, ptExtended, ptReal, ptReal48, ptSingle: RealIdentifier;
	ptAnsiString, ptShortString, ptWideString: StringIdentifier;
	ptOleVariant, ptVariant: VariantIdentifier;
	ptString: StringType;
	ptFile: FileType;
	ptArray:
		begin
			NextToken;
			EatToken(ptOf);
			case CurrentTokenKind of
			ptConst:NextToken; // new in ObjectPascal80
			else
				ParseTypeId;
			end;
		end;
	else
		EatToken(ptIdentifier);
	end;

	if CurrentTokenKind = ptLessThan then
	begin
		EatToken(ptLessThan);
		TypeArgs;
		EatToken(ptGreaterThan);
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

Tested by: basic_cases.dat/ParseTypeParam
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

	Tested by: basic_cases.dat/ParseTypeParamConstraint
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
	Tested by: basic_cases.dat/ParseConstraintTypeRef
}
	// Base type name (qualified identifier)
	Result := TSyntaxNode2.Create(ntType);
	Result.AddChild(ParseQualifiedIdentifier); // ntQualifiedIdentifier("IComparable")

	// Optional generic type arguments: '<' Type {',' Type} '>'
	if CurrentTokenKind = ptLessThan then
	begin
		Result.AddChild(ParseTypeParams); //!!RECURSIVE!!
	end;
end;

function TDelphiParser.TypeParamList: TSyntaxNode2;
begin
	Result := PoisonNode;

  if CurrentTokenKind = ptOpenBracket then
    AttributeSection;
  TypeSimple;
  while CurrentTokenKind = ptComma do
  begin
    NextToken;
    if CurrentTokenKind = ptOpenBracket then
      AttributeSection;
    TypeSimple;
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
	Tested by: basic_cases.dat/ParseTypeParams
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


function TDelphiParser.TypeReferenceType: TSyntaxNode2;
begin
	Result := PoisonNode;


	EatToken(ptType);
	EatToken(ptOf);
	ParseTypeId;
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
	Tested by: basic_cases.dat/ParseConstSection
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

function TDelphiParser.ParseConstantDecl: TSyntaxNode2;
var
	tok: TSyntaxToken;
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
	Tested by: basic_cases.dat/ParseConstantDecl
}
	Result := TSyntaxNode2.Create(ntConstant);

	// Get the constant name
	// @anName = "clMainInstruction"
	tok := EatToken(ptIdentifier);
	Result.AddChild(tok);
	Result[anName] := tok.ValueText;

	// Read optional type [':' Type]
	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseType);
	end;

	// '=' TypedConstant

	Result.AddChild(EatToken(ptEquals));
	valueNode := ParseTypedConstant;
	Result.AddChild(valueNode);
	Result[anValueText] := valueNode[anValueText];

	// (PortabilityDirective)*
	while IsPossiblePortabilityDirective do
	begin
		Result.AddChild(ParsePortabilityDirective);
	end;

	Result.AddChild(EatToken(ptSemiColon));
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
	-> RESOURCESTRING ( ResourceStringDecl ';' )*

ResourceStringDecl
	-> Ident '=' ConstExpr

Example
-------

resourcestring
	SOK = 'OK';

ntResourceStrings

	Tested by: basic_cases.dat/ParseResStringSection
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
	-> Ident '=' ConstExpr

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
	Tested by: basic_cases.dat/ParseResourceStringDecl
}
	Result := TSyntaxNode2.Create(ntResourceString);

	// SFirstName
	name := EatToken(ptIdentifier);
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
	Tested by: basic_cases.dat/ParseInterfaceDeclaration
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

function TDelphiParser.ExportsElement: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntElement);
     ExportsName;
     if CurrentTokenKind = ptOpenParen then
     begin
       FormalParameterList;
     end;

     if CurrentTokenExID = ptIndex then
     begin
       NextToken;
       EatToken(ptIntegerConst);
     end;
     if CurrentTokenExID = ptName then
     begin
       NextToken;
       ParseSimpleExpression;
     end;
     if CurrentTokenExID = ptResident then
     begin
       NextToken;
     end;
end;

function TDelphiParser.ExportsClause: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntExports);
		EatToken(ptExports);
		ExportsElement;
		while CurrentTokenKind = ptComma do
		begin
			NextToken;
			ExportsElement;
		end;
		Semicolon;
end;

function TDelphiParser.ContainsClause: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntContains);
     ExpectedEx(ptContains);
     MainUsedUnitStatement;
     while CurrentTokenKind = ptComma do
     begin
       NextToken;
       MainUsedUnitStatement;
     end;
     Semicolon;
end;

function TDelphiParser.RequiresClause: TSyntaxNode2;
begin
{
   ntRequires]
		[ptRequires]
      RequiresIdentifier
      if comma
			RequiresIdentifier
      [ptSemicolon]
}
	Result := TSyntaxNode2.Create(ntRequires);
	  ExpectedEx(ptRequires);
	  RequiresIdentifier;
	  while CurrentTokenKind = ptComma do
	  begin
	    NextToken;
	    RequiresIdentifier;
	  end;
	  Semicolon;
end;

function TDelphiParser.RequiresIdentifier: TSyntaxNode2;
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
			RequiresIdentifierId;
			while CurrentToken.TokenKind = ptDot do
			begin
				NextToken;
				RequiresIdentifierId;
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

function TDelphiParser.RequiresIdentifierId: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntUnknown);
	Result.Attributes[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseInitializationSection: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseInitializationSection
	}
	Result := TSyntaxNode2.Create(ntInitialization);
	EatToken(ptInitialization);
	StatementList;
end;

function TDelphiParser.ParseImplementationSection: TSyntaxNode2;
begin
{
ImplementationSection
	-> IMPLEMENTATION
			[UsesClause]
			(ImplementationDecl)*
	Tested by: basic_cases.dat/ParseImplementationSection
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
http://dgrok.excastle.com/Grammar.html#InterfaceSection

InterfaceSection
	-> INTERFACE
			[UsesClause]
			(InterfaceDecl)*

Returns: ntInterface

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

	Tested by: basic_cases.dat/ParseInterfaceSection
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
PortabilityDirective
		-> platform
		-> deprecated
		-> library
		-> experimental
	Tested by: basic_cases.dat/ParsePortabilityDirective
}
	Result := TSyntaxNode2.Create(ntPortabilityDirective);

	while CurrentToken.GenID in [ptPlatform, ptLibrary, ptDeprecated, ptExperimental] do
	begin
		case CurrentToken.GenID of
		ptPlatform:			Result.Attributes[anPlatform]     := 'true';
		ptLibrary:			Result.Attributes[anLibrary]      := 'true';
		ptDeprecated:
			begin
				Result.Attributes[anDeprecated]   := 'true';

				// Check for deprecated 'use this other unit'
				// [ptDeprecated] [ptStringConst]
				if PeekTokenKind = ptStringLiteral then
				begin
					Result.AddChild(EatToken);
					s := CurrentToken.ValueText;
					if s <> '' then
						Result.Attributes[anDeprecated] := s;
					Result.AddChild(EatToken(ptStringLiteral)); // add the reason for the deprecation
				end;
			end;
		ptExperimental: Result.Attributes[anExperimental] := 'true';
		else
			SynError('Invalid identifier '+CurrentToken.ValueText);
		end;
		Result.AddChild(EatToken);
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

function TDelphiParser.ParseIdentifierList: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseIdentifierList
	}
	Result := PoisonNode;

	ParseIdentifier;

	while CurrentTokenKind = ptComma do
	begin
		NextToken;
		ParseIdentifier;
	end;
end;

function TDelphiParser.CharString: TSyntaxNode2;
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
					ParseVariableReference;
				end;
			ptString:
				begin
					StringStatement;
					Statement;
				end;
			else
				StringConst;
			end;
//			if FCurrentToken.TokenKind = ptPoint then
//			begin
//				NextToken;
//          VariableReference;
//        end;
		end;
  else
	 begin
		SynError('InvalidCharString');
	 end;
  end;
end;

function TDelphiParser.ParseScriptFile: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseScriptFile
	}
	Result := PoisonNode;

{
	Read the allowed set of child tokens
}
	while CurrentTokenKind <> ptEof do
	begin
		case CurrentTokenKind of
		ptClass:				ProcedureDeclarationSection;
		ptConst:				ParseConstSection;
		ptConstructor:		ProcedureDeclarationSection;
		ptDestructor:		ProcedureDeclarationSection;
		ptExports:			ExportsClause;
		ptFunction:			ProcedureDeclarationSection;
		ptIdentifier:		Statements;
		ptLabel:				LabelDeclarationSection;
		ptProcedure:		ProcedureDeclarationSection;
		ptResourceString:	ParseConstSection;
		ptType:				ParseTypeSection;
		ptThreadVar:		ParseVarSection;
		ptVar:				ParseVarSection;
		else
			NextToken;
		end;
	end;
end;

function TDelphiParser.ClassClass: TSyntaxNode2;
begin
	Result := PoisonNode;

	// TODO: fix this
//	FStack.Peek.SetAttribute(anClass, AttributeValues[atTrue]);

	EatToken(ptClass);
end;

function TDelphiParser.ClassConstraint: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntClassConstraint);
		EatToken(ptClass);
end;

function TDelphiParser.PropertyDefault: TSyntaxNode2;
begin
	Result := PoisonNode;

  ExpectedEx(ptDefault);
end;

function TDelphiParser.DispIDSpecifier: TSyntaxNode2;
begin
	Result := PoisonNode;

  ExpectedEx(ptDispid);
  ConstantExpression;
end;

function TDelphiParser.IndexOp: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntIndexed);

		EatToken(ptOpenBracket);
		ParseExpression;
		while CurrentTokenKind = ptComma do
		begin
			NextToken;
			ParseExpression;
		end;
		EatToken(ptCloseBracket);
end;

function TDelphiParser.ParseIndexSpecifier: TSyntaxNode2;
begin
	{
	Tested by: basic_cases.dat/ParseIndexSpecifier
	}
	Result := TSyntaxNode2.Create(ntIndex);
	ExpectedEx(ptIndex);
	ConstantExpression;
end;

function TDelphiParser.ClassTypeEnd: TSyntaxNode2;
begin
	Result := PoisonNode;

	case CurrentTokenExID of
	ptExperimental: NextToken;
	ptDeprecated: DirectiveDeprecated;
	end;
end;

function TDelphiParser.ObjectTypeEnd: TSyntaxNode2;
begin
	Result := PoisonNode;
end;

function TDelphiParser.DirectiveDeprecated: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntHintDirective);

	Result.AddChild(ExpectedEx(ptDeprecated));
	if CurrentTokenKind = ptStringLiteral then
		Result.AddChild(EatToken);
end;

procedure TDelphiParser.DirectiveInline(ParentNode: TSyntaxNode2);
begin
	ParentNode.Attributes[anInline] := AttributeValueToString(atTrue);
	ParentNode.AddChild(EatToken(ptInline));
end;

function TDelphiParser.DirectiveLibrary: TSyntaxNode2;
begin
	Result := PoisonNode;
  EatToken(ptLibrary);
end;

function TDelphiParser.DirectivePlatform: TSyntaxNode2;
begin
	Result := PoisonNode;
  ExpectedEx(ptPlatform);
end;

procedure TDelphiParser.EnsureProgress;
begin
	if FLastProgressIndex = FCurrent then
		raise EParserException.Create(-1, -1, '', 'No progress');

	FLastProgressIndex := FCurrent;
end;

function TDelphiParser.EnumeratedTypeItem: TSyntaxNode2;
begin
	Result := ParseQualifiedIdentifier;

	if CurrentTokenKind = ptEquals then
	begin
		Result.AddChild(EatToken(ptEquals));
		Result.AddChild(ConstantExpression);
	end;
end;

function TDelphiParser.ParseIdentifier: TSyntaxNode2;
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
	Tested by: basic_cases.dat/ParseIdentifier
}
	Result := TSyntaxNode2.Create(ntIdentifier);
	Result.Attributes[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));
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
http://dgrok.excastle.com/Grammar.html#LabelId

LabelId
	-> <number>
	-> Ident
}
	Result := CurrentTokenKind in [ptIntegerConst, ptIdentifier];
end;

function TDelphiParser.DirectiveLocal: TSyntaxNode2;
begin
	Result := PoisonNode;

	ExpectedEx(ptLocal);
end;

function TDelphiParser.DirectiveVarargs: TSyntaxNode2;
begin
	ExpectedEx(ptVarargs);
	Result := PoisonNode;
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

	Tested by: basic_cases.dat/ParseAncestorList
}
	Result := TSyntaxNode2.Create(ntAncestorList);

	Result.AddChild(ParseQualifiedIdentifier);

	while (CurrentTokenKind = ptComma) do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseQualifiedIdentifier);
	end;
end;

function TDelphiParser.AnonymousMethod: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAnonymousMethod);
		case CurrentTokenKind of
		ptFunction:
			begin
				NextToken;
				if CurrentTokenKind = ptOpenParen then
					FormalParameterList;
				EatToken(ptColon);
				ReturnType;
			end;
		ptProcedure:
			begin
				NextToken;
				if CurrentTokenKind = ptOpenParen then
					FormalParameterList;
			end;
		end;
		Block;
end;

function TDelphiParser.AnonymousMethodType: TSyntaxNode2;
begin
	Result := PoisonNode;

	ExpectedEx(ptReference);
	EatToken(ptTo);
	case CurrentTokenKind of
	ptProcedure:
		begin
			NextToken;
			if CurrentTokenKind = ptOpenParen then
				FormalParameterList;
		end;
	ptFunction:
		begin
			NextToken;
			if CurrentTokenKind = ptOpenParen then
				FormalParameterList;
			EatToken(ptColon);
			ReturnType;
		end;
	end;
end;

function TDelphiParser.ExportsNameId: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntUnknown);
	Result.Attributes[anName] :=CurrentToken.ValueText;

	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ExportsName: TSyntaxNode2;
//var
//	NamesNode: TSyntaxNode2;
begin
	Result := PoisonNode;


{
  NamesNode := TSyntaxNode2.Create(ntUnknown);
  try
    FStack.Push(NamesNode);
    try
        ExportsNameId;
        while CurrentToken.TokenKind = ptDot do
        begin
          NextToken;
          ExportsNameId;
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

function TDelphiParser.ImplementsSpecifier: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntImplements);
     ExpectedEx(ptImplements);

     ParseTypeId;
     while (CurrentTokenKind = ptComma) do
     begin
       NextToken;
       ParseTypeId;
     end;
end;

function TDelphiParser.AttributeArgumentName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.CaseLabelList: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntCaseLabels);

	Result.AddChild(CaseLabel);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken);
		Result.AddChild(CaseLabel);
	end;
end;

function TDelphiParser.ArrayBounds: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntBounds);

	if CurrentTokenKind = ptOpenBracket then
	begin
		Result.AddChild(EatToken);
		Result.AddChild(ArrayDimension);
		while CurrentTokenKind = ptComma do
		begin
			Result.AddChild(EatToken);
			Result.AddChild(ArrayDimension);
		end;
		Result.AddChild(EatToken(ptCloseBracket));
	end;
end;

function TDelphiParser.ProceduralDirectiveOf: TSyntaxNode2;
begin
	Result := PoisonNode;

  NextToken;
  EatToken(ptObject);
end;

function TDelphiParser.HintDirectives: TSyntaxNode2;
begin
{

Declarations and Statements (Delphi)
https://docwiki.embarcadero.com/RADStudio/Sydney/en/Declarations_and_Statements_(Delphi)#Hinting_Directives


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
	Result := TSyntaxNode2.Create(ntHintDirectives);

	while CurrentToken.GenID in [ptDeprecated, ptLibrary, ptPlatform, ptExperimental] do
	begin
		case CurrentToken.GenID of
		ptDeprecated:   Result.AddChild(DirectiveDeprecated);
		ptLibrary:      Result.AddChild(DirectiveLibrary);
		ptPlatform:     Result.AddChild(DirectivePlatform);
		ptExperimental: Result.AddChild(EatToken);
		end;
	end;
end;

function TDelphiParser.InheritedVariableReference: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntInherited);

	Result.AddChild(EatToken(ptInherited));
	if CurrentToken.TokenKind = ptIdentifier then
		Result.AddChild(ParseVariableReference);
end;

{procedure TDelphiParser.InitAhead;
begin
	// InitAhead means to memorize our current position in the token array,
	// as we are about to parse ahead, and we want to return where we were.
//	FAheadOriginalCurrentIndex := FCurrent;
end;}

{procedure TDelphiParser.RestoreAhead;
begin
//	FCurrent := FAheadOriginalCurrentIndex;
end;}

procedure TDelphiParser.InitDefinesDefinedByCompiler;
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

function TDelphiParser.GlobalAttributes: TSyntaxNode2;
begin
	Result := GlobalAttributeSections;
end;

function TDelphiParser.GlobalAttributeSections: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAttributes);

	while CurrentToken.TokenKind = ptOpenBracket do
		Result.AddChild(GlobalAttributeSection);
end;

function TDelphiParser.GlobalAttributeSection: TSyntaxNode2;
begin
   Result := TSyntaxNode2.Create(ntAttributes);

	Result.AddChild(EatToken(ptOpenBracket));
	Result.AddChild(GlobalAttributeTargetSpecifier);
	Result.AddChild(AttributeList);
	while CurrentToken.TokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(GlobalAttributeTargetSpecifier);
		Result.AddChild(AttributeList);
	end;
	Result.AddChild(EatToken(ptCloseBracket));
end;

function TDelphiParser.GlobalAttributeTargetSpecifier: TSyntaxNode2;
begin
	Result := GlobalAttributeTarget;
	Result.AddChild(EatToken(ptColon));
end;

function TDelphiParser.GlobalAttributeTarget: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAttribute); // todo: ntAttribute?
	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.Attributes: TSyntaxNode2;
begin
	Result := AttributeSections;
end;

function TDelphiParser.AttributeSections: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAttributes);

	while CurrentTokenKind = ptOpenBracket do
		Result.AddChild(AttributeSection);
end;

function TDelphiParser.AttributeSection: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAttribute);

	Result.AddChild(EatToken(ptOpenBracket));
	if PeekTokenKind = ptColon then
		Result.AddChild(AttributeTargetSpecifier);
	Result.AddChild(AttributeList);
	while CurrentToken.TokenKind = ptComma do
	begin
		if PeekTokenKind = ptColon then
			Result.AddChild(AttributeTargetSpecifier);
		Result.AddChild(AttributeList);
	end;
	Result.AddChild(EatToken(ptCloseBracket));
end;

function TDelphiParser.AttributeTargetSpecifier: TSyntaxNode2;
begin
	Result := AttributeTarget;
	Result.AddChild(EatToken(ptColon));
end;

function TDelphiParser.AttributeTarget: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntUnknown); // todo: what is an attribute target?

	case CurrentToken.TokenKind of
	ptProperty: Result.AddChild(EatToken(ptProperty));
	ptType: Result.AddChild(EatToken(ptType));
	else
		Result.AddChild(EatToken(ptIdentifier));
	end;
end;

function TDelphiParser.AttributeList: TSyntaxNode2;
begin
	Result := Attribute;

	while CurrentToken.TokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(AttributeList);
	end;
end;

function TDelphiParser.Attribute: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntAttribute);

	Result.AddChild(AttributeName);
	if CurrentToken.TokenKind = ptOpenParen then
		Result.AddChild(AttributeArguments);
end;

function TDelphiParser.AttributeName: TSyntaxNode2;
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

function TDelphiParser.AttributeArguments: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntArguments);

	Result.AddChild(EatToken(ptOpenParen));
	if CurrentTokenKind <> ptCloseParen then
	begin
		if PeekTokenKind = ptEquals then
			Result.AddChild(NamedArgumentList)
		else
			Result.AddChild(PositionalArgumentList);

		if CurrentToken.TokenKind = ptEquals then
			Result.AddChild(NamedArgumentList)
	end;
	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.PositionalArgumentList: TSyntaxNode2;
begin
	Result := PositionalArgument;
	while CurrentToken.TokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(PositionalArgument);
	end;
end;

function TDelphiParser.PositionalArgument: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntPositionalArgument);
	Result.AddChild(AttributeArgumentExpression);
end;

function TDelphiParser.NamedArgumentList: TSyntaxNode2;
begin
	Result := NamedArgument;
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(NamedArgument);
	end;
end;

function TDelphiParser.NamedArgument: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntNamedArgument);
	Result.AddChild(AttributeArgumentName);
	Result.AddChild(EatToken(ptEquals));
	Result.AddChild(AttributeArgumentExpression);
end;

function TDelphiParser.AttributeArgumentExpression: TSyntaxNode2;
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

function TDelphiParser.CustomAttribute: TSyntaxNode2;
begin
	//TODO: Global vs. Local attributes
	Result := AttributeSections;
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
	node := TSyntaxNodeOrToken.Create(ChildNode);
	FChildNodes.Add(node);
end;

procedure TSyntaxNode2.AssignPositionFrom(const Node: TSyntaxNode2);
begin
	raise ENotImplemented.Create('TSyntaxNode2.AssignPositionFrom');
end;

function TSyntaxNode2.Clone: TSyntaxNode2;
//var
//  i: Integer;
begin
	raise ENotImplemented.Create('TSyntaxNode.Clone');
{  Result := TSyntaxNodeClass(Self.ClassType).Create(FTyp);

  SetLength(Result.FChildNodes, Length(FChildNodes));
  for i := 0 to High(FChildNodes) do
  begin
    Result.FChildNodes[i] := FChildNodes[i].Clone;
    Result.FChildNodes[i].FParentNode := Result;
  end;

  Result.FAttributes := Copy(FAttributes);
  Result.AssignPositionFrom(Self);
}
end;

constructor TSyntaxNode2.Create(SyntaxNodeType: TSyntaxNodeType);
begin
	inherited Create;

	FNodeType := SyntaxNodeType;
	FAttributes := TDictionary<TAttributeName, string>.Create;
	FChildNodes := TObjectList<TSyntaxNodeOrToken>.Create(True);
end;

procedure TSyntaxNode2.DeleteChild(const NodeOrToken: TSyntaxNodeOrToken);
begin
	FChildNodes.Extract(NodeOrToken);
end;

destructor TSyntaxNode2.Destroy;
begin
	FNodeType := TSyntaxNodeType(-1);
	FreeAndNil(FAttributes);
	FreeAndNil(FChildNodes);

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

procedure TSyntaxNode2.ExtractChild(const NodeOrToken: TSyntaxNodeOrToken);
begin
	FChildNodes.Extract(NodeOrToken);
end;

function TSyntaxNode2.FindNode(const TypesPath: array of TSyntaxNodeType): TSyntaxNode2;
begin
	raise ENotImplemented.Create('TSyntaxNode.FindNode');
end;

function TSyntaxNode2.get_Attributes(Attribute: TAttributeName): string;
begin
	if not FAttributes.TryGetValue(Attribute, {var}Result) then
		Result := '';
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

			Result := Result + '@'+AttributeNameToStr(attr)+'="'+s+'"';
		end;
	end;

begin
	Result := SyntaxNodeTypeToStr(Self.NodeType)+'('+SyntaxNodeAttributesToStr(Self)+')';
end;

procedure TSyntaxNode2.set_Attributes(Attribute: TAttributeName; const Value: string);
begin
	FAttributes.AddOrSetValue(Attribute, Value);
end;

function TSyntaxNode2.ToString: string;
begin
	Result := Format('%s(%s, "%s")', [
         Self.ClassName,
         SyntaxNodeTypeToStr(Self.NodeType),
         Self.Value]);
end;

function TSyntaxNode2.FindNode(NodeType: TSyntaxNodeType): TSyntaxNode2;
begin
	raise ENotImplemented.Create('TSyntaxNode.FindNode');
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

procedure TDelphiParser.AddToken(parent: TSyntaxNode2; token: TptTokenKind);
var 
  node: TSyntaxNode2;
begin
  node := ConsumeToken(token, ntIdentifier);
  if Assigned(node) then 
    parent.AddChild(node);
end;

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

	if index >= FTokens.Count then
		index := FTokens.Count-1; //keep returning final eof token

	Result := FTokens[index];
end;

function TDelphiParser.Add(parent: TSyntaxNode2; child: TSyntaxNode2): TSyntaxNode2;
begin
  Result := child;
  if Assigned(parent) and Assigned(child) then
    parent.AddChild(child);
end;

{ TDelphiParser - Token consumption helpers }

function TDelphiParser.ConsumeToken(expectedKind: TptTokenKind; nodeType: TSyntaxNodeType): TSyntaxNode2;
begin
  if CurrentToken.TokenKind <> expectedKind then
    DoMessage(Format(SExpected, [TokenName(expectedKind), TokenName(CurrentToken.TokenKind)]), CurrentToken.Line, CurrentToken.Column);
    
  Result := TSyntaxNode2.Create(nodeType);
  Result[anName] := CurrentToken.ValueText;
  Result.FCurrentLine := CurrentToken.Line;
  Result.FCurrentColumn := CurrentToken.Column;
//	Result.FFilename := CurrentToken.Filename;   wishful thinking
  
  NextToken;
end;

function TDelphiParser.ConsumeIdentifier: TSyntaxNode2;
begin
  Result := ConsumeToken(ptIdentifier, ntIdentifier);
  Result[anName] := CurrentToken.Text;
end;

function TDelphiParser.ConsumeKeyword(keyword: TptTokenKind): TSyntaxNode2;
begin
  Result := ConsumeToken(keyword, ntIdentifier); // or ntKeyword if you prefer
end;

function TDelphiParser.ConsumeSemicolon: TSyntaxNode2;
begin
  // Handle the special semicolon logic from your original Semicolon procedure
  case CurrentToken.TokenKind of
    ptElse, ptEnd, ptExcept, ptfinally, ptFinalization, ptCloseParen, ptUntil: 
      Result := nil; // No semicolon to consume
  else
    Result := ConsumeToken(ptSemicolon, ntIdentifier); // preserve the semicolon token
  end;
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

Added VarName and VarNameList back in for descendent classes, fixed to correctly
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
InterfaceHeritage: Qualified identifiers
bugs in variant records
typedconstant failed with complex set constants. simple patch using ConstantExpression

German localization for the two string constants. Define GERMAN for german string constants.

Greg Chapman on 20010522
Better handling of defaut array property
Separate handling of X and Y in property Pixels[X, Y: Integer through identifier "event"
corrected spelling of "ForwardDeclaration"

James Jacobson on 20010223
semi colon before finalization fix

James Jacobson on 20010223
RecordConstant Fix

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

function AttributeValueToString(const AttributeValue: TAttributeValue): string;
const
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
			'atDispInterface');
begin
	Result := TypInfo.GetEnumName(TypeInfo(TAttributeValue), Ord(AttributeValue));
end;


end.
