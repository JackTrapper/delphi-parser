unit DelphiParser;

{
Takes a set of tokens from a tokenizer, and turns them into a Syntax Tree.

Sample Usage
============

	var
		syntaxTree: TSyntaxTree;
		tree: string;

		syntaxTree := TDelphiParser.ParseText(moCode.Text, '');

		tree := TSyntaxNode2.DumpTree(syntaxTree.Root);


Note: It is not an Abstract Syntax Tree (AST). 
		It is a Syntax Tree; as it preserves trivia, whitespace, and tokens.
		This is useful for linters that want to read comments

This is what the parser returns to you. It contains the tree (.Root),
and a memory area for all the nodes and tokens.

   Compilation
   │
   ├─ SyntaxTree (one per source file)
   │  ├─ Node Arena
   │  ├─ Token Arena
   │  └─ Root SyntaxNode
   │
   ├─ SemanticModel (per SyntaxTree)
   │  ├─ BindingTable
   │  ├─ TypeTable
   │  ├─ ConstantTable
   │  └─ ConversionTable
   │
   └─ FlowAnalysis (per method or region)
        └─ FlowTable



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
	ntUnitDeclaration
		ptIdentifier anName="unit"
		ntIdentifier('System.Generics.Collections')
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

		ntIdentifierList,			// a list of identifiers. e.g. "t1, t2: Int64", the list is [t1], [t2]
		ntIdentifier,				// An identifier. e.g. "msxml"

		ntPortabilityDirective,	// PLATFORM | DEPRECATED <String> | LIBRARY | EXPERIMENTAL

		ntInterfaceSection,
		ntImplementation,


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

		ntCaseStatement,					// case FLogLevel of
		ntCaseSelector,					// 1..2, 3..5
		ntCaseLabels,						// 1..2
		ntCaseLabel,						// 1, 2
		ntCaseElse,							// else LogError;

		ntConstants,						// const keyword
		ntConstant,							// individual ident = value pair
		ntResourceStrings,				// the resourecestring keyword
		ntResourceString,					// individual ident = 'string' pair


		// Generic type constraints (e.g. U: class, object
		ntConstraints,
		ntClassConstraint,
		ntConstructorConstraint,
		ntRecordConstraint,


		ntTerm,							// a term is a <factor> #op <factor>
		ntFactor,
		ntAtom,

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
		ntExceptionHandler,				// one of the "on" blocks
		ntExports,
		ntExpressionOrRangeList,
		ntExpressionOrRange,
		ntRelationalExpression,			// [Left] [Operator] [Right}
		ntExpressionList,
		ntExpression,
		ntSimpleExpression,
		ntBinaryExpression,           // a xor b (
		ntExternal,							// [Directive] ntExternal is one of the kind of Directives
		ntFDiv,
		ntFieldSection,      // FieldSection
		ntField,             // FieldDecl

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
		ntImplements,
		ntIn,
		ntIndex,
		ntIndexed,
		ntInherited,
		ntInitialization, // --todo--> ntInitSection
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
		ntParameterList,
		ntParameterType,

		ntPath,
		ntPositionalArgument,

		// Visibility
		ntVisibilitySection,		//anVisibility="private|protected|public|published|strict private|strict protected"
//		ntStrictPrivate,			ntVisibilitySection anVisibility="strict private"
//		ntStrictProtected,		ntVisibilitySection anVisibility="strict protected"
//		ntPrivate,					ntVisibilitySection anVisibility="private"
//		ntProtected,				ntVisibilitySection anVisibility="protected"
//		ntPublic,					ntVisibilitySection anVisibility="public"
//		ntPublished,				ntVisibilitySection anVisibility="published"	

		ntProperty,
		ntPropertyDirective,

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
		ntStatementList,

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

		ntVarSection, 				// var
		ntVariable,					// 	t1: Int64
		ntUnaryOperator,
		ntParameterExpression,	// call argument with optional format specifiers: Expression [':' Expression [':' Expression]]
		ntParticle,
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
		anForward,				// the type or method is delcared as forward
		anExternal,				// method is delcared as external
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
		anTriviaKind,
		anText,
		anSlot,
		anTokenKind,
		anValueText,
		anDistinct,				// type TCustomerID = type Integer;

		anLibrary,				// the unit is marked as unit Contoso; library;
		anPlatform,				// the unit is marked as unit Contoso; platform;
		anExperimental,		// the unit is marked as unit Contoso; experimental
		anDeprecated			// marked as deprecated, with an @anText='Use  comment
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
			avOperator,
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
		FChildNodes: TObjectList<TSyntaxNodeOrToken>;		// owns the child objects
		FAttributes: TDictionary<TAttributeName, string>;  // e.g. akKind="isdefault"
		FFilename: string;

		FWidth: Integer;
		FFullWidth: Integer;

		procedure AddChild(ChildNode: TSyntaxNode2); overload;
		procedure AddChild(ChildNode: TSyntaxToken); overload;

		function get_Attributes(Attribute: TAttributeName): string;
		procedure set_Attributes(Attribute: TAttributeName; const Value: string);
		function get_HasChildren: Boolean;
		function get_Value: string;	// @anName
		function get_DisplayName: string;
		function get_IsMissing: Boolean;
	public
		constructor Create(SyntaxNodeType: TSyntaxNodeType);
		destructor Destroy; override;

		function ToString: string; override;

		class function DumpTree(Node: TSyntaxNode2): string;
		class function DumpPascal(Node: TSyntaxNode2; ShowParserTags: Boolean=False): string;

		property NodeType: TSyntaxNodeType read FNodeType;
		property Value: string read get_Value;    // alias of anName attribute

		property Attributes[Attribute: TAttributeName]: string read get_Attributes write set_Attributes; default;

		property ChildNodes: TObjectList<TSyntaxNodeOrToken> read FChildNodes;

		property DisplayName: string read get_DisplayName;
		property HasChildren: Boolean read get_HasChildren; // if ChildNodes contains items
		property IsMissing: Boolean read get_IsMissing; // alias of anMissing attrbute


		property Width: Integer read FWidth;			// Full width minus trivia (just the token/node text)
		property FullWidth: Integer read FFullWidth;	// Includes leading + trailing trivia
	end;

	TSyntaxTree = class
	private
		FRoot: TSyntaxNode2;
		FTokenArena: TObjectList; // owns the list of TSyntaxToken objects
		FNodeArena:  TObjectList; // owns the list of TSyntaxNode2 objects
	public
		constructor Create(const ARoot: TSyntaxNode2);
		destructor Destroy; override;

		property Root: TSyntaxNode2 read FRoot;
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

		procedure GetDefaultConditionalDirectives(TargetList: TStrings);

		// Advances to the next token (which is also how FCurrentToken is set)
		// Only used internally by EatToken, and EatTokenEx
		procedure NextToken;

		{
			Eventually this will be deprecated to catch all the legal code paths, or maybe it's because i just missed something.
			...it's a lot of conflicting grammer to sort through.
		}
		function PoisonNode(NodeType: TSyntaxNodeType=ntUnknown): TSyntaxNode2; //deprecated 'special NotImplemented node';

		// Property getters
		function get_CurrentToken: TSyntaxToken;
		function get_CurrentTokenKind: TptTokenKind; virtual;		//deprecated 'Use FCurrentToken.TokenKind instead';
		function get_CurrentContentualKind: TptTokenKind; virtual;
		function get_NextFewTokens: string;
	protected
		// Not used
		function NodeListToString(NamesNode: TSyntaxNode2): string;
		property NextFewTokens: string read get_NextFewTokens;

		procedure DoMessage(const Msg: string; X, Y: Integer); deprecated 'Delcared but never used';


		function ParseClassProperty: TSyntaxNode2;

		function NextTokensAre(const CheckTokens: array of TptTokenKind): Boolean;		// if NextTokensAre([ptArray, ptOf]) then
	private
		procedure Log(const s: string);

		// Token navigation

		// The next token info
		function PeekToken: TSyntaxToken; overload;
		function PeekToken(n: Integer): TSyntaxToken; overload;	// 0:CurrentToken, 1:PeekToken, 2:...
		function PeekTokenKind:  TptTokenKind; 						// helper for --> PeekToken.TokenKind
		function PeekTokenExID:  TptTokenKind;							// helper for --> PeekToken.ExID

		// Convenience: like Expect but without slot
		function EatToken: TSyntaxToken; overload;
		function EatToken(const ExpectedTokenKind: TptTokenKind): TSyntaxToken; overload;   // eat, create an IsMissing token if it's not
		function EatTokenEx(const ExpectedTokenContentualKind: TptTokenKind): TSyntaxToken;	// eat, create an IsMissing token if it's not

		// Output an error message
		function SynError(Error: string): TSyntaxNode2;
		function SynErrorFmt(const Error: string; const Args: array of const): TSyntaxNode2;

		property CurrentToken:               TSyntaxToken read get_CurrentToken;				// read-only
		property CurrentTokenKind:           TptTokenKind read get_CurrentTokenKind;			// the CurrentToken.Kind
		property CurrentTokenContentualKind: TptTokenKind read get_CurrentContentualKind;	// the CurrentToken.ContextualKind
	protected
		// ***********************************************************************
		// Start of the production methods
		// ***********************************************************************

		// Main parsing function.
		function ParseCore: TSyntaxTree; virtual; //as a nice way to split plumbing from grammer

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


		function ParseStringType: TSyntaxNode2;				// ntType

		function ParseTypeSection: TSyntaxNode2;				// ntTypeSection
			function IsPossibleTypeSection: Boolean;

		function ParseTypeDeclaration: TSyntaxNode2;			// ntTypeDecl					http://dgrok.excastle.com/Grammar.html#TypeDecl
			function IsPossibleTypeDeclaration: Boolean; 	// peeks ahead to see if the next production is a TypeDeclaration, meaning you can call ParseTypeDeclaration()

		function ParseType: TSyntaxNode2;						// ntType						http://dgrok.excastle.com/Grammar.html#Type

		function ParsePointerType: TSyntaxNode2;

		function ParseProcedureType: TSyntaxNode2;

		function ParseParameterList: TSyntaxNode2;			// ntParameterList			['(' (Parameter [';'])* ')']
		function ParseParameter: TSyntaxNode2;					// ntParameter
		function ParseParameterType: TSyntaxNode2;			// ntParameterType

		// ntType, TFoo = <Type>
		function ParseEnumeratedType: TSyntaxNode2;
		function ParseExpressionOrRange: TSyntaxNode2;
		function ParseExpressionOrRangeList: TSyntaxNode2;
		//ParseArrayType
		//ParseSetType
		//ParseFileType
		function ParseClassType: TSyntaxNode2;					// ntType(@anType=atClass)
		function ParseObjectType: TSyntaxNode2;					// ntType(@anType=object)
		function ParseAncestorList: TSyntaxNode2;				// TWidget = class(TShape, IShape, IWidget)

		// strict private, private, strict protected, protected, public, published
		function ParseVisibilitySection: TSyntaxNode2;
			function IsPossibleVisibilitySection: Boolean;

		function ParseVisibilitySectionContent: TSyntaxNode2;
			function IsPossibleVisibilitySectionContent: Boolean;

		function ParseFieldSection: TSyntaxNode2;
		function ParseFieldDecl: TSyntaxNode2;


		function ParseConstSection: TSyntaxNode2;				// ntConstants
			function IsPossibleConstSection: Boolean;

		function ParseConstantDecl: TSyntaxNode2;				// ntConstant
			function IsPossibleConstantDecl: Boolean;


		function ParseResStringSection: TSyntaxNode2;		// ntResourceStrings
			function IsPossibleResStringSection: Boolean;
		function ParseResourceStringDecl: TSyntaxNode2;
			function IsPossibleResourceStringDecl: Boolean;

		function ParseMethodOrProperty: TSyntaxNode2;



		function ParseClassHelperType: TSyntaxNode2;				// ntHelper
		function ParseSubrangeType: TSyntaxNode2;
		function ParseTypeId: TSyntaxNode2;

		function ParseVarSection: TSyntaxNode2;
			function IsPossibleVarSection: Boolean;

		function ParseVarDecl: TSyntaxNode2;


		function ParseImplementationSection: TSyntaxNode2;

		function ParseInitSection: TSyntaxNode2;
		function ParseFinalizationSection: TSyntaxNode2;

		function ParseArrayType: TSyntaxNode2;
		function ParseBlock: TSyntaxNode2;

		function ParseConstantExpression: TSyntaxNode2;

		function ParseImplementationDecl: TSyntaxNode2;
			function IsPossibleImplementationDecl: Boolean;

		// parse directive
		function ParseDirective: TSyntaxNode2;
			function IsPossibleDirective: Boolean;
		function ParseDirectiveExternal: TSyntaxNode2; // ptExternal

		// Involved in tree building
		function ParseAddOp: TSyntaxNode2;
		function ParseAddressOp: TSyntaxNode2;
		function ParseAssemblerStatement: TSyntaxNode2;
		function ParseAsOp: TSyntaxNode2;
		function ParseAtExpression: TSyntaxNode2; // raise at

		function ParseCaseStatement: TSyntaxNode2;				// [ntCaseStatement]	case ErrorLevel of
		function ParseCaseSelector: TSyntaxNode2;					// [ntCaseSelector] 1..39, 99, 101: ;
		function ParseCaseLabel: TSyntaxNode2;


		function ParseCaseLabelList: TSyntaxNode2;



		function ParseMethodImplementation: TSyntaxNode2;

		function ParseMethodHeading: TSyntaxNode2;
			function IsPossibleMethodHeading: Boolean;

			// Called by ParseMethodHeading to specially handle each:
			function ParseProcedureMethodHeading: TSyntaxNode2;		// ptProcedure
			function ParseFunctionMethodHeading: TSyntaxNode2;			// ptFunction
			function ParseConstructorMethodHeading: TSyntaxNode2;		// ptConstructor
			function ParseDestructorMethodHeading: TSyntaxNode2;		// ptDestructor
			function ParseOperatorMethodHeading: TSyntaxNode2;			// ptOperator
		function ParseAnonymousMethod: TSyntaxNode2;
		function ParseAnonymousMethodType: TSyntaxNode2;

		procedure ExternalDirectiveTwo(ParentNode: TSyntaxNode2);
		function ParseRecordType: TSyntaxNode2;

		function ParseProperty: TSyntaxNode2;
			function IsPossibleProperty: Boolean;

		function ParsePropertyDirective: TSyntaxNode2;
			function IsPossiblePropertyDirective: Boolean;



		function ParseClassOfType: TSyntaxNode2;

		function ParseConstantValueTyped: TSyntaxNode2;

		function ParseDotOp: TSyntaxNode2;
		function ParseElseStatement: TSyntaxNode2;

		function ParseExceptBlock: TSyntaxNode2;							// except
		function ParseExceptionItem: TSyntaxNode2;						// on E:Exception do
		function ParseExceptionBlockElseBranch: TSyntaxNode2;

		function ParseExportsStatement: TSyntaxNode2;
		function ParseExportsItem: TSyntaxNode2;

{
		Expression [RelOp ]
}
		function ParseExpression: TSyntaxNode2;				// [SimpleExpression] [RelOp] [SimpleExpression]e.g. a < b*c + d*e
			function ParseSimpleExpression: TSyntaxNode2;	// [Term] [AddOp] [Term] e.g. b+c
			function ParseTerm: TSyntaxNode2;					// [Factor] [MulOp] [Factor]  d*e
			function ParseFactor: TSyntaxNode2;					//	[Atom]
																			// [UnaryOperator]
				function ParseAtom: TSyntaxNode2;					// [Particle]
				function ParseUnaryOperator: TSyntaxNode2;			// not d
					function ParseParticle: TSyntaxNode2;					// (x+y)



//			function IsPossibleSimpleExpression: Boolean;   do not write this. Find the implementation comments to find out why. Stop re-implementing it.


		function ParseExpressionList: TSyntaxNode2;

		function ParseFieldName: TSyntaxNode2;
		function ParseFinallyBlock: TSyntaxNode2;
		function ParseForStatement: TSyntaxNode2;
		function ParseForStatementDownTo: TSyntaxNode2;
		function ParseForStatementFrom: TSyntaxNode2;
		function ParseForStatementIn: TSyntaxNode2;
		function ParseForStatementTo: TSyntaxNode2;

		function ParseLabelDeclSection: TSyntaxNode2;
		function ParseExpressionOrAssignment: TSyntaxNode2;
		function ParseExtendedIdent: TSyntaxNode2;
		function ParseParameterExpression: TSyntaxNode2;


		function ParseRecordVariant: TSyntaxNode2;
		function ParseRecordAlign: TSyntaxNode2;
		function ParseSetType: TSyntaxNode2;

		function ParseStatement: TSyntaxNode2;
//			function IsPossibleStatement: Boolean;

		function ParseEnumeratedTypeElement: TSyntaxNode2;


		function ParseFieldDeclaration: TSyntaxNode2;
		function ParseFieldList: TSyntaxNode2;
		function ParseFileType: TSyntaxNode2;
		function ParseRecordHelperType: TSyntaxNode2;
		function ParseGotoStatement: TSyntaxNode2;
		function ParseIfStatement: TSyntaxNode2;

		function ParseIdentList: TSyntaxNode2;			//
		function ParseIdent: TSyntaxNode2;				// ntIdentifier
			function IsPossibleIdent: Boolean;

		function ParseIndexSpecifier: TSyntaxNode2;
		function ParseIndexOp: TSyntaxNode2;
		function ParseInheritedVariableReference: TSyntaxNode2;
		function ParseInlineVarDeclaration: TSyntaxNode2;
		function ParseInlineVarSection: TSyntaxNode2;
		function ParseInlineConstSection: TSyntaxNode2;
		function ParseInterfaceType: TSyntaxNode2;

		function ParseLabelId: TSyntaxNode2;
			function IsPossibleLabelID: Boolean;  // <number>, Ident

		function ParseMultiplicativeOperator: TSyntaxNode2;
		function ParseObjectNameOfMethod: TSyntaxNode2;
		function ParsePointerSymbol: TSyntaxNode2;
		function ParseRaiseStatement: TSyntaxNode2;

		function ParseRelOp: TSyntaxNode2;
		function ParseRepeatStatement: TSyntaxNode2;

		function ParseRequiresClause: TSyntaxNode2;
			function IsPossibleRequiresClause: Boolean;

		function ParseMethodReturnType: TSyntaxNode2;
		function ParseRoundClose: TSyntaxNode2;
		function ParseRoundOpen: TSyntaxNode2;
		function ParseSetConstructor: TSyntaxNode2;
		function ParseSetElement: TSyntaxNode2;

		function ParseSimpleStatement: TSyntaxNode2;
			//function IsPossibleSimpleStatement: Boolean;

		function ParseStatementList: TSyntaxNode2;
		function ParseThenStatement: TSyntaxNode2;
		function ParseTryStatement: TSyntaxNode2;

		function ParseTagField: TSyntaxNode2;
		function ParseTagFieldName: TSyntaxNode2;
		function ParseTagFieldTypeName: TSyntaxNode2;
		function ParseTypedConstant: TSyntaxNode2;

		function ParseVarAbsolute: TSyntaxNode2;
		function ParseVarEqual: TSyntaxNode2;

		function ParseVariable: TSyntaxNode2;
		function ParseVariableReference: TSyntaxNode2;
		function ParseVariantSection: TSyntaxNode2;

		function ParseWhileStatement: TSyntaxNode2;
		function ParseWithExpressionList: TSyntaxNode2;
		function ParseWithStatement: TSyntaxNode2;

		// Generics
		function ParseTypeArgs: TSyntaxNode2;
		function ParseTypeParams: TSyntaxNode2;
		function ParseTypeParam: TSyntaxNode2;						// T, U: class, constructor; V: IComparable<T>>= class
			function ParseConstraintTypeRef: TSyntaxNode2;		// IComparable<T>>= class
			function ParseTypeParamConstraint: TSyntaxNode2;
		//end generics


		// Attributes
		// ==========
		// This is the syntax for custom attributes, based quite strictly on the
		// ECMA syntax specifications for C#, but with a Delphi expression being
		// used at the bottom as opposed to a C# expression. -JThurman 2004-03-21
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
	protected
		function ParseProcedureDeclarationSection: TSyntaxNode2; deprecated 'Not used';
	public
		constructor Create;
		destructor Destroy; override;

		// The main way it will be used
		class function ParseText(const Text: UnicodeString; FilePath: string=''): TSyntaxTree;

		// Parse using the list of tokens. Note: the parser takes ownership of the tokens
		function Parse(const Tokens: TList): TSyntaxTree;
		function ParseFile(FilePath: string): TSyntaxTree;
		function ParseStream(SourceStream: ISequentialStream; FilePath: string; CodePage: Word): TSyntaxTree;

		procedure ResetComplierDirectives; //to that of the current compiler


		property CompilerDirectives: TStrings read FCompilerDirectives;

		// Whether parsing should skip the implementation, only parsing the interface section
		property InterfaceOnly: Boolean read FInterfaceOnly write FInterfaceOnly;

		//
		property IncludeHandler: IUnknown{IIncludeHandler} write set_IncludeHandler;
	public
{
		Not yet implemented; probably because they exist inside something else
}
//		function ParseBareInherited: TSyntaxNode2;		// is so simple it's just in SimpleStatement
//		function ParseExportsSpecifier: TSyntaxNode2;

//		function ParseInterfaceDecl: TSyntaxNode2;
//		function ParseMulOp: TSyntaxNode2;
//		function ParsePackedType: TSyntaxNode2;
//		function ParseParenthesizedExpression: TSyntaxNode2;
//		function ParseProgram: TSyntaxNode2;
//		function ParseSetLiteral: TSyntaxNode2;
//		function ParseTypeDecl: TSyntaxNode2;
//		function ParseVariantGroup: TSyntaxNode2;
//		function ParseVisibility: TSyntaxNode2;}
	end;

implementation

uses
	System.IOUtils,
	TypInfo,
	Windows,
{$IFDEF UnitTests}DelphiParserTests,{$ENDIF}
	Avatar.Exceptions,
	DelphiPreprocessor;

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

//	['[' (Parameter [';'])+ ']']
	if CurrentTokenKind = ptOpenBracket then
	begin
		Result.AddChild(EatToken(ptOpenBracket));
		Result.AddChild(ParseParameter);
		while CurrentTokenKind = ptSemicolon do
		begin
			Result.AddChild(EatToken(ptSemicolon));
			// Allow an optional trailing semicolon before ']'.
			if CurrentTokenKind = ptCloseBracket then
				Break;
			Result.AddChild(ParseParameter);
		end;
		Result.AddChild(EatToken(ptCloseBracket));
	end;

//	[':' MethodReturnType]
   if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseType);
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

	function IsDirectiveWord(const AWord: string): Boolean; //deprecated 'Should be using ContextKind';
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


Sample
------

	property FirstName: string read FFirstName write FFirstName;

ntPropertyDirective anKind=

}
	Result := TSyntaxNode2.Create(ntPropertyDirective);

	if (CurrentTokenKind = ptSemicolon) and ((PeekTokenExID = ptDefault) or PeekIsDirectiveWord('default')) then
	begin
//		-> ';' DEFAULT
		Result.Attributes[anKind] := 'isdefault';
		Result.AddChild(EatToken(ptSemicolon));
		if CurrentTokenContentualKind = ptDefault then
			Result.AddChild(EatTokenEx(ptDefault))
		else
			Result.AddChild(EatToken);
	end
	else if (CurrentTokenContentualKind = ptDefault) or IsDirectiveWord('default') then
	begin
//		-> DEFAULT Expression
		Result.Attributes[anKind] := 'default';
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenContentualKind = ptDispId) or IsDirectiveWord('dispid') then
	begin
//		-> DISPID Expression
		Result.Attributes[anKind] := 'dispid';
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenContentualKind = ptIndex) or IsDirectiveWord('index') then
	begin
//		-> INDEX Expression
		Result.Attributes[anKind] := 'index';
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenContentualKind = ptNoDefault) or IsDirectiveWord('nodefault') then
	begin
//		-> NODEFAULT
		Result.Attributes[anKind] := 'nodefault';
		Result.AddChild(EatToken);
	end
	else if (CurrentTokenContentualKind = ptRead) or IsDirectiveWord('read') then
	begin
//		-> READ Expression
		Result.Attributes[anKind] := 'read';
		Result.AddChild(EatTokenEx(ptRead));
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenContentualKind = ptReadOnly) or IsDirectiveWord('readonly') then
	begin
//		-> READONLY
		Result.Attributes[anKind] := 'readonly';
		Result.AddChild(EatToken);
	end
	else if (CurrentTokenContentualKind = ptStored) or IsDirectiveWord('stored') then
	begin
//		-> STORED Expression
		Result.Attributes[anKind] := 'stored';
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenContentualKind = ptWrite) or IsDirectiveWord('write') then
	begin
//		-> WRITE Expression
		Result.Attributes[anKind] := 'write';
		Result.AddChild(EatTokenEx(ptWrite));
		// Keep accessor parsing bounded so the terminating semicolon is preserved.
		Result.AddChild(ParseExpression);
	end
	else if (CurrentTokenContentualKind = ptWriteOnly) or IsDirectiveWord('writeonly') then
	begin
//		-> WRITEONLY
		Result.Attributes[anKind] := 'writeonly';
		Result.AddChild(EatToken);
	end
	else if (CurrentTokenContentualKind = ptImplements) or IsDirectiveWord('implements') then
	begin
//		-> IMPLEMENTS (QualifiedIdent [','])+
		Result.Attributes[anKind] := 'implements';
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
		Result.AddChild(SynErrorFmt(SE2029, ['Property directive', CurrentToken.Text]));
	end;
end;

function TDelphiParser.ParseStream(SourceStream: ISequentialStream; FilePath: string; CodePage: Word): TSyntaxTree;
var
	tokenizer: TDelphiTokenizer;
	preprocessor: TDelphiPreprocessor;
	currentToken: TSyntaxToken;
	tokens: TObjectList;
begin
{
	Pipeline:  parser  <--  preprocessor  <--  tokenizer  <--  stream

	The tokenizer reads characters from the stream and emits tokens.
	The preprocessor reads tokens from the tokenizer and handles conditional compilation
	($IFDEF, $ELSE, $ENDIF, etc.), demoting directives and disabled
	content to trivia before the parser sees them.
	The parser collects the preprocessed tokens into FTokens and builds the tree.
}
	tokens := TObjectList.Create(False); //don't own them, they go into the tree
	try
		tokenizer := TDelphiTokenizer.Create(SourceStream, CodePage);
		try
			preprocessor := TDelphiPreprocessor.Create(tokenizer, Self.FCompilerDirectives);
			try
				while preprocessor.NextToken({out}currentToken) do
					tokens.Add(currentToken);
			finally
				preprocessor.Free;
			end;
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

function TDelphiParser.EatTokenEx(const ExpectedTokenContentualKind: TptTokenKind): TSyntaxToken;
//var
//	s: string;
begin
	// Expect the CurrentToken's ContextualKind to be ExpectedTokenContentualKind
	if CurrentTokenContentualKind <> ExpectedTokenContentualKind then
	begin
		//s := Format(SExpected, ['EX:' + TokenName(ExpectedTokenKind), TokenName(CurrentTokenGenID)]);
		//DoMessage(s, CurrentToken.Width, CurrentToken.FullWidth);

		// Roslyn-style recovery: synthesize the expected token and keep EOF in place.
		Result := TSyntaxToken.Create(ExpectedTokenContentualKind, CurrentToken.Width, CurrentToken.FullWidth, '');
		Result.IsMissing := True;

		if CurrentToken.Kind <> ptEof then
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
	prevTokenText := TokenKindToStr(CurrentToken.Kind)+'('+QuotedStr(CurrentToken.Text)+')';

	// Once we are over the end, no point in going even further off the end.
	if FCurrent < FTokens.Count then
		Inc(FCurrent);

	currTokenText := TokenKindToStr(CurrentToken.Kind)+'('+QuotedStr(CurrentToken.Text)+')';

	// Logging the token changes less useful now that we log the tokens in the unit tests
//	Log(currTokenText+' <== '+prevTokenText);
end;

function TDelphiParser.NextTokensAre(const CheckTokens: array of TptTokenKind): Boolean;
var
	i: Integer;
begin
{
This is a little bit of syntatic sugar to help checking a set of nodes ahead.

	if NextTokensAre([ptClass, ptHelper]) then
		...

The issue arises because we can't do a case statememnt CurrentTokenKind and NextTokenKind.
And if we try to nest the Peek, we can't fall through to another case:

	case CurrentTokenKind of
	ptClass:
		begin
			case PeekTokenKind of
			ptHelper: ParseClassHelper;
			ptOf: ParseClassOf;
			else
				...we can't fall-through...
			end;
		end;
	end;

So a single line syntax that allows:

	if NextTokensAre([ptClass, ptHelper]) then
		ParseClassHelper
	else if NextTokensAre([ptClass, ptOf]) then
		ParseClassOf
	...
}
	if Length(CheckTokens) <= 0 then
	begin
		Result := True;
		Exit;
	end;

	Result := False; // if we bail out it's false

	for i := Low(CheckTokens) to High(CheckTokens) do
	begin
		if PeekToken(i).ContextualKind <> CheckTokens[i] then
			Exit;
	end;

	Result := True;
end;

function TDelphiParser.PeekTokenExID: TptTokenKind;
begin
	// todo: rename this to PeekContextualKind (and review the logic around every break; which is why we renamed it)
	Result := PeekToken(1).ContextualKind;
end;

function TDelphiParser.PeekTokenKind: TptTokenKind;
begin
	Result := PeekToken(1).Kind;
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

function TDelphiParser.get_CurrentTokenKind: TptTokenKind;
begin
	Result := CurrentToken.Kind;
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

function TDelphiParser.get_CurrentContentualKind: TptTokenKind;
begin
	if CurrentTokenKind = ptEOF then
	begin
		Result := ptEOF;
		Exit;
	end;

	Result := CurrentToken.ContextualKind;
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
		if tok.Kind = ptEOF then
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
//var
//	errToken: TSyntaxToken;
begin
	// Create a zero-width (synthesized) token for error recovery, preserving tree shape
	Result := TSyntaxNode2.Create(ntUnknown);
	Result.Attributes[anName] := Format(Error, Args);
	Result.Attributes[anMissing] := 'true';
//	errToken := CurrentToken;

	// Do not consume at EOF.  Eating the EOF sentinel here would steal it from
	// ParseCore's final EatToken(ptEOF) call and could lead to double-ownership
	// of the same token object (or the global singleton), causing use-after-free
	// on the next parser run.
	if CurrentTokenKind <> ptEof then
		Result.AddChild(EatToken);

//	DoMessage(Error, errToken.Width, errToken.FullWidth);
end;

function TDelphiParser.Parse(const Tokens: TList): TSyntaxTree;
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

	Result := Self.ParseCore;
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
		Result := ParseMethodHeading
	else if IsPossibleProperty then
		Result := ParseProperty
	else
		Result := SynErrorFmt(SE2029, ['method or property', CurrentToken.Text]);
end;

function TDelphiParser.ParseFieldDecl: TSyntaxNode2;
var
	valueNode: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#FieldDecl

FieldDecl
	-> IdentList ':' Type
			(PortabilityDirective)*
			['=' TypedConstant]		; extended: initialized fields in var blocks (e.g. O: Integer = 12)
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
	else if (FCurrent > 0) and (CurrentTokenKind = ptIdentifier) and (FTokens[FCurrent-1].Kind = ptColon) then
	begin
		// ':' already consumed; continue with type parsing.
	end
	else
		Result.AddChild(EatToken(ptColon));

	Result.AddChild(ParseType);

//	(PortabilityDirective)*
	while IsPossiblePortabilityDirective do
		Result.AddChild(ParsePortabilityDirective);

//	['=' TypedConstant]		; optional initializer for field declarations in var blocks
	if CurrentTokenKind = ptEquals then
	begin
		Result.AddChild(EatToken(ptEquals));
		valueNode := ParseTypedConstant;
		Result.AddChild(valueNode);
		Result.Attributes[anValueText] := valueNode.Attributes[anValueText];
	end;

//	(PortabilityDirective)*
	while IsPossiblePortabilityDirective do
		Result.AddChild(ParsePortabilityDirective);

// [;]
	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken(ptSemiColon));
end;

function TDelphiParser.ParseCore: TSyntaxTree;
var
	root: TSyntaxNode2;
	node: TSyntaxNode2;
	trailing: TSyntaxNode2;

	procedure CollectNodes(run: TSyntaxNode2);
	var
		nodeOrToken: TSyntaxNodeOrToken;
	begin
		{
			Walk the tree:
				put tokens in FTokenArena
				put nodes  in FNodeArena
		}

		// We start by being passed a node
		Result.FNodeArena.Add(run);

		// Put children into the arena
		for nodeOrToken in run.ChildNodes do
		begin
			if nodeOrToken.IsToken then
				Result.FTokenArena.Add(nodeOrToken.AsToken)
			else if nodeOrToken.IsNode then
				CollectNodes(nodeOrToken.AsNode)
			else
   			raise Exception.Create('Neither node nor token');
		end;
	end;
begin
{
Parse the tokens contained in FTokens and returns it as a syntax Tree (TSyntaxTree)

SyntaxTree has to "arena" objects that hold, and own, the tokens and nodes in the tree.
This way nodes can be removed from the tree, but still remain valid for any outstanding references.
And it lets us graft trees later and not worry about memory management.
And plus the tokens and nodes are cached and shared; so we can't let anyone else
claim ownership of them, because we own them in the arena.

Result:

	tree: TSyntaxTree
		- private FTokenArena: TObjectList(True)
		- private FNodeArena:  TObjectList(True)
		- property RootNode --> TSyntaxNode(ntCompilationUnit)
			- ntUnitDeclaration

RootNode returns you the root node of the tree. The root node is of type ntComplationUnit.

ntCompilationUnit is the root container for whatever the parser produced.

It is the unit of work you asked it to perform.

This has a few features:

- Uniform root: Every syntax tree, whether it's a full unit with interface/implementation,
		or just a single expression with, both have just one root node type.
		That means tools (traversals, visitors, rewriters) don’t need special cases
		like "sometimes the root is a UnitDeclaration, sometimes it's just an Expression."
- Global trivia: Roslyn sticks file-level comments, #pragmas, #if/#endif, extern alias,
		and using directives at the root.
		Delphi equivalents could be compiler directives, attributes,
		or even stray comments before the unit keyword.
- EndOfFile token: the EOF sentinel always lives under the compilation root
- Extensibility: When the language adds new top-level constructs
		(e.g. top-level statements in C# 9, Delphi's program vs. unit headers),
		you don’t have to change the concept of the root,
		it just holds whatever is allowed at top level.

This part is oriented at the official grammar of Delphi 4
and parialy based on Robert Zierers Delphi grammar.
For more information about Delphi grammars take a look at:
		http://www.stud.mw.tu-muenchen.de/~rz1/Grammar.html
		https://archive.ph/2ytar
}


	// The tree is rooted on a "CompilationUnit" node.
	root := TSyntaxNode2.Create(ntCompilationUnit);

	NextToken; // advance to the first token

	// Check the file type directive
	case CurrentTokenContentualKind of
	ptUnit:		node := ParseUnit;			// e.g. unit SimpleParser;
	ptProgram:	node := ParseProgramFile;	// e.g. program SimpleParser;
	ptPackage:	node := ParsePackage;		// e.g. package SimpleParser;
	ptLibrary:	node := ParseLibraryFile;	// e.g. library SimpleParser;
	else
		node := ParseScriptFile; // for arbitrary expressions
	end;

	root.AddChild(node);

	// If parsing ended early, preserve ownership of all remaining real tokens.
	if CurrentTokenKind <> ptEof then
	begin
		trailing := TSyntaxNode2.Create(ntUnknown);
		trailing.Attributes[anName] := 'Unexpected trailing tokens';
		while CurrentTokenKind <> ptEof do
			trailing.AddChild(EatToken); // consume real token, attach to tree
		root.AddChild(trailing);
	end;

	root.AddChild(EatToken(ptEOF)); // consume/attach real EOF token

	// Prepare the SyntaxTree return (it holds the arenas)
	Result := TSyntaxTree.Create(root);

	// Collect all tokens and nodes into the arena.
	CollectNodes(root);
end;

function TDelphiParser.ParseFile(FilePath: string): TSyntaxTree;

	function DetectEncodingFromStream(AStream: TStream): TEncoding;
	var
		LBuffer: TBytes;
		LPosition: Int64;
		LEncoding: TEncoding;
	begin
		// Remember the current stream position so we can restore it later
		LPosition := AStream.Position;

		// Read up to 4 bytes (max BOM length in Delphi's detection logic)
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

class function TDelphiParser.ParseText(const Text: UnicodeString; FilePath: string=''): TSyntaxTree;
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

Library
	-> LIBRARY Ident ';'
			[UsesClause]
			(ImplementationDecl)*
			[BEGIN StatementList]
			END '.'

Note: The '(' IdentList ')' form from DGrok is not valid in Delphi 7+.
Note: Unlike units, libraries use BEGIN..END for their main block, not INITIALIZATION.

TODO: Check both with BEGIN StatemntList and without (going straight to END.)
}
	Result := TSyntaxNode2.Create(ntLibrary);
	Result.AddChild(EatToken(ptLibrary));
	Result.AddChild(ParseIdent);

//	';'
	Result.AddChild(EatToken(ptSemicolon));

//	[UsesClause]
	if IsPossibleUsesClause then
		Result.AddChild(ParseUsesClause);

	// If the user only asked us to parse the interface section
	if InterfaceOnly then
		Exit;

//	(ImplementationDecl)*
	while IsPossibleImplementationDecl do
		Result.AddChild(ParseImplementationDecl);

//	[BEGIN StatementList]
	if CurrentTokenKind = ptBegin then
	begin
		Result.AddChild(EatToken(ptBegin));
		if not (CurrentTokenKind in [ptEnd, ptEof]) then
			Result.AddChild(ParseStatementList);
	end;

//	END '.'
	Result.AddChild(EatToken(ptEnd));
	Result.AddChild(EatToken(ptDot));
end;

function TDelphiParser.ParseInitSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#InitSection

InitSection (initialization only; finalization is a sibling handled by ParseUnit)
	-> INITIALIZATION  [StatementList]

Finalization is parsed separately by ParseFinalizationSection and attached
as a sibling node on ntUnitDeclaration, not nested inside ntInitialization.
}
	Result := TSyntaxNode2.Create(ntInitialization);
	Result.AddChild(EatToken(ptInitialization));

	if not (CurrentTokenKind in [ptFinalization, ptEnd, ptEof]) then
		Result.AddChild(ParseStatementList);
end;

function TDelphiParser.ParseFinalizationSection: TSyntaxNode2;
begin
{
FinalizationSection
	-> FINALIZATION  [StatementList]

Attached as a sibling to ntInitialization on the ntUnitDeclaration node.
}
	Result := TSyntaxNode2.Create(ntFinalization);
	Result.AddChild(EatToken(ptFinalization));

	if not (CurrentTokenKind in [ptEnd, ptEof]) then
		Result.AddChild(ParseStatementList);
end;

procedure TDelphiParser.DoMessage(const Msg: string; X, Y: Integer);
begin
	if Assigned(FOnMessage) then
		FOnMessage(Self, Msg, X, Y);

	Log(Format('%s (%d,%d)', [Msg, X, Y]));
end;

procedure TDelphiParser.Log(const s: string);
begin
{$IFDEF DEBUG}
	Windows.OutputDebugString(PChar(s));
{$ENDIF}
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
	-> PROGRAM Ident ';'
			[UsesClause]
			(ImplementationDecl)*
			[BEGIN StatementList]
			END '.'

Note: The '(' IdentList ')' form from DGrok is not valid in Delphi 7+.
Note: Like libraries, programs use BEGIN..END for their main block, not INITIALIZATION.
}
	Result := TSyntaxNode2.Create(ntProgram);
	Result.AddChild(EatToken(ptProgram));
	Result.AddChild(ParseIdent);

//	';'
	Result.AddChild(EatToken(ptSemicolon));

//	[UsesClause]
	if IsPossibleUsesClause then
		Result.AddChild(ParseUsesClause);

	// If the user only asked us to parse the interface section
	if InterfaceOnly then
		Exit;

//	(ImplementationDecl)*
	while IsPossibleImplementationDecl do
		Result.AddChild(ParseImplementationDecl);

//	[BEGIN StatementList]
	if CurrentTokenKind = ptBegin then
	begin
		Result.AddChild(EatToken(ptBegin));
		if not (CurrentTokenKind in [ptEnd, ptEof]) then
			Result.AddChild(ParseStatementList);
	end;

//	END '.'
	Result.AddChild(EatToken(ptEnd));
	Result.AddChild(EatToken(ptDot));
end;

function TDelphiParser.ParseUnit: TSyntaxNode2;
var
	unitName: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Unit

Unit
	-> UNIT QualifiedIdentifier (PortabilityDirective)* ';'
			InterfaceSection
			ImplementationSection
			[ InitSection ]
			END '.'

TODO: update grammer.yaml since i've changed it

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
	ntUnitDeclaration anName="Contoso.SpecialCharactersDemo" anLibrary="true" anPlatform="true" deprecated="use Fabrikam.SpecialCharactersDemo" anExperimental="true"
		ptUnit
		ntIdentifier anName="Contoso.SpecialCharactersDemo"
			ptIdentifier("Contoso")
			ptDot
			ptIdentifier("SpecialCharactersDemo")
		ntPortabilityDirective
			ptLibrary
			ptPlatform
			ptDeprecated
			ptStringConst("use Fabrikam.SpecialCharactersDemo")
			ptExperimental
		ptSemicolon
		ntInterfaceSection
	ptEof
}
	Result := TSyntaxNode2.Create(ntUnitDeclaration);

//	UNIT
	// Add the "unit" token to the child list
	Result.AddChild(EatToken(ptUnit));	// EatToken moves next,

//	QualifiedIdentifier
	// Get the unit's full name (e.g. "System.Classes")
	unitName := ParseQualifiedIdent;
	Result.Attributes[anName] := unitName.Attributes[anName];	// get the unit name
	Result.AddChild(unitName);

//	(PortabilityDirective)* ';'
	// Portability directives
	while (CurrentTokenContentualKind in [ptPlatform, ptLibrary, ptDeprecated, ptExperimental]) do
	begin
		Result.AddChild(ParsePortabilityDirective);
	end;

	Result.AddChild(EatToken(ptSemicolon)); // Expect a semicolon

//	InterfaceSection
	Result.AddChild(ParseInterfaceSection);		// read the interface section

	// If they only want the interfaction section then we are now done
	if InterfaceOnly then
		Exit;

//	ImplementationSection
	Result.AddChild(ParseImplementationSection);

//	[ InitSection ]
	if CurrentTokenKind = ptInitialization then
	begin
		Result.AddChild(ParseInitSection);

//		[ FinalizationSection ]
		if CurrentTokenKind = ptFinalization then
			Result.AddChild(ParseFinalizationSection);
	end;

// END '.'
	Result.AddChild(EatToken(ptEnd));
	Result.AddChild(EatToken(ptDot));
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
var
	LBeginToken: TSyntaxToken;
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
		LBeginToken := EatToken(ptBegin);
		Result := ParseStatementList;
		Result.FChildNodes.Insert(0, TSyntaxNodeOrToken.Create(LBeginToken));
		Result.AddChild(EatToken(ptEnd));
	end
	else if CurrentTokenKind = ptAsm then
	begin
		Result := ParseAssemblerStatement;
	end
	else
		Result := SynErrorFmt('Expected %s but found %s', ['BEGIN, ASM', CurrentToken.Text]);
end;

function TDelphiParser.ParseImplementationDecl: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ImplementationDecl

Called by: FancyBlock, ImplementationSection, Program

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
	ptLabel:				Result := ParseLabelDeclSection;				// [ntLabel] 				-> LABEL (LabelId [','])+ ';'
	ptConst:				Result := ParseConstSection;					// [ntConstants]			-> CONST (ConstantDecl)+
	ptResourceString:	Result := ParseResStringSection;				// [ntResourceStrings]	-> RESOURCESTRING (ResourceStringDecl)+
	ptType:				Result := ParseTypeSection;					// [ntTypeSection]		-> TYPE (TypeDecl)+

	// Var is both Var and ThreadVar
	ptVar,
	ptThreadVar:		Result := ParseVarSection;						// [ntVariables]			-> (VAR | THREADVAR) (VarDecl)+

	// [class] procedure, function, constructor, destructor, operator
	ptClass,
	ptProcedure,
	ptFunction,
	ptConstructor,
	ptDestructor,
	ptOperator:       Result := ParseMethodImplementation;

	ptExports:			Result := ParseExportsStatement;					// [ntExports]			-> EXPORTS (ExportsItem [','])+ ';'
	ptOpenbracket:		Result := ParseAssemblyAttribute;				// [ntAttribute]		-> '[' ASSEMBLY ':' Expression ']'
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
	while CurrentToken.Kind = ptDot do
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

	case CurrentTokenContentualKind of
	ptExternal:
		begin
	 		//		-> [';'] EXTERNAL [Expression (ExportsSpecifier)*]
			Result.AddChild(ParseDirectiveExternal);
			Result.Attributes[anExternal] := AttributeValueToStr(avTrue);
		end;
	ptDispid:
		begin
			//		-> [';'] DISPID Expression
			Result.AddChild(EatTokenEx(ptDispID));
			Result.AddChild(ParseExpression);
		end;
	ptMessage:
		begin
			//		-> [';'] MESSAGE Expression
			Result.AddChild(EatTokenEx(ptMessage));
			Result.AddChild(ParseExpression);
		end;
	ptAbstract,			//		-> [';'] ABSTRACT
	ptAssembler,		//		-> [';'] ASSEMBLER
	ptCDecl,				//		-> [';'] CDECL
	ptDynamic,			//		-> [';'] DYNAMIC
	ptExport, 			//		-> [';'] EXPORT
	ptFar,				//		-> [';'] FAR
	ptFinal,				//		-> [';'] FINAL
	ptInline,			//		-> [';'] INLINE
	ptLocal,				//		-> [';'] LOCAL
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
		Result.AddChild(EatTokenEx(CurrentTokenContentualKind));
	ptForward:			//		-> [';'] FORWARD
		begin
			Result.AddChild(EatTokenEx(ptForward));
			Result.Attributes[anForward] := AttributeValueToStr(avTrue);
		end;
	else
//		-> [';'] PortabilityDirective
		Result.AddChild(ParsePortabilityDirective);
	end;
end;

function TDelphiParser.ParseDirectiveExternal: TSyntaxNode2;
begin
{
Directive
	Backlinks: MethodHeading, ProcedureType

-> ...
-> [';'] EXTERNAL [Expression (ExportsSpecifier)*]

Returns an ntExternal node.

	// external with alias name and ordinal
	function SomeFunc: Integer; stdcall; external 'legacy.dll' name 'LegacyEntry' index 5;

TODO: right now we have:

		Token.TokenKind      Token.DirectieID
		'far'						ptFar

This is because where are some keywords that are RESERVED (e.g. 'do', ptDo), that you cannot use anywhere out the allowed context,
there are also some keywords that are "directives" (e.g. 'for', ptFor). Directies are keywords that can also be identifiers.

And right now:

		- token.TokenKind (ptIdentifier)
		- token.DirectiveID (ptFor)

I've not yet done any syntax tree navigation work, but based on the Parse methods,
we care a lot more about having to check for:

> tokenKind, if tokenKind is not identifier, and if it is identifir, use directiveID

But now that i remember it: if my goal here is to have one single enumeration value
i can check for all normal work, i can just use token.GenID.

But i still wonder if there might be something to having a:

	token.IsIdentifier
	token.IsDirective
}
	Result := TSyntaxNode2.Create(ntExternal);

//	[';'] EXTERNAL
	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken(ptSemicolon));					// optional semicolon
	Result.AddChild(EatTokenEx(ptExternal));						// expect ptExternal

//	[Expression (ExportsSpecifier)*]
	case CurrentTokenKind of
	ptSemiColon: Result.AddChild(EatToken(ptSemicolon));
	else
		begin
			if CurrentTokenContentualKind <> ptName then
				Result.AddChild(ParseSimpleExpression);

			if CurrentTokenContentualKind = ptDelayed then
				Result.AddChild(EatToken);

			ExternalDirectiveTwo(Result);
		end;
	end;

end;

function TDelphiParser.ParseMethodReturnType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodReturnType

MethodReturnType					Backlinks: MethodHeading, ProcedureType, Property
	-> QualifiedIdent
	-> STRING
}
	Result := TSyntaxNode2.Create(ntReturnType);

	if CurrentTokenKind = ptString then
		Result.AddChild(EatToken(ptString))
	else
		Result.AddChild(ParseQualifiedIdent);
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

procedure TDelphiParser.ExternalDirectiveTwo(ParentNode: TSyntaxNode2);
begin
{
	TODO: Since this does not return a node, it should not exist. I under it is a helper
			method, but i would prefer that code be in the TDelphiParser.ParseDirective method who calls this.
			
	Almost certainly the loop can sit there; which is where the grammer is documented.
}
	while True do
	begin
		if CurrentTokenContentualKind = ptIndex then
			ParentNode.AddChild(ParseIndexSpecifier)
		else if CurrentTokenContentualKind = ptName then
		begin
			ParentNode.AddChild(EatToken);
			ParentNode.AddChild(ParseSimpleExpression);
		end
		else if CurrentTokenContentualKind = ptDelayed then
			ParentNode.AddChild(EatToken)
		else
			Break;
	end;

	if CurrentTokenKind = ptSemicolon then
		ParentNode.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParseForStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ForStatement

ForStatement
	-> FOR (QualifiedIdent | VAR InlineVarDeclaration)
			( ':=' Expression (TO | DOWNTO) Expression
			| IN Expression )
			DO [Statement]

InlineVarDeclaration
	-> IdentList [':' Type]
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

	if CurrentToken.Kind = ptAssign then
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
	else if CurrentToken.Kind = ptIn then
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
var
	caseElse: TSyntaxNode2;
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

Example
-------

case FErrorLevel of
1: LogError(s);
else
	LogWarning(s);
end;

Tree
----

ntCaseStatement
	ntParticle
		ntIdentifier anName="FErrorLevel"
	ntCaseSelector
		ntCaseLabels
			ntCaseLabel
				ntExpression
					ntParticle
		ntStatement
			ntParticle
				ntIdentifier anName="LogError"
				ntParticle
					ntIdentifier anName="s"
	ntCaseElse												; formerly ntStatementList
		ntStatement
			ntParticle
				ntIdentifier anName="LogWarning"
				ntParticle
					ntIdentifier anName="s"

}
	Result := TSyntaxNode2.Create(ntCaseStatement);

//	CASE Expression OF
	Result.AddChild(EatToken(ptCase));
	Result.AddChild(ParseExpression);
	Result.AddChild(EatToken(ptOf));

//	CaseSelector (';' CaseSelector)* [';']

	// First the mandatory selector
	Result.AddChild(ParseCaseSelector);					// [ntCaseSelector]

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
	// i want an empty else statement to still have the node; the developer took the time to write it.
	if (CurrentTokenKind = ptElse) { and (PeekTokenKind <> ptEnd)} then
	begin
		// ParseCaseElse --> ntCaseElse
		caseElse := TSyntaxNode2.Create(ntCaseElse);
		caseElse.AddChild(EatToken(ptElse));

		if (CurrentTokenKind <> ptEnd) and (CurrentTokenKind <> ptEof) then
				caseElse.AddChild(ParseStatementList);

		Result.AddChild(caseElse);
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

//	':' [Statement]
	Result.AddChild(EatToken(ptColon));

//	if IsPossibleStatement then  lets use a different way to do this one
		Result.AddChild(ParseStatement);

	// Note: Trailing ';' is NOT consumed here.
	// ParseCaseStatement's while-loop consumes the ';' between selectors,
	// which also handles the optional trailing ';' before ELSE/END.
end;

function TDelphiParser.ParseExpressionOrRange: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExpressionOrRange

ExpressionOrRange
	-> SimpleExpression ['..' SimpleExpression]
}
	Result := TSyntaxNode2.Create(ntExpressionOrRange);

//	SimpleExpression
	Result.AddChild(ParseSimpleExpression);	// ntExpression

//	['..' SimpleExpression]
	if CurrentTokenKind = ptDotDot then
	begin
		Result.AddChild(EatToken(ptDotDot));
		Result.AddChild(ParseSimpleExpression);
	end;
end;

function TDelphiParser.ParseExpressionOrRangeList: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExpressionOrRangeList

ExpressionOrRangeList								Backlinks: SetLiteral
	-> (ExpressionOrRange [','])+
}

	Result := TSyntaxNode2.Create(ntExpressionOrRange);
	Result.AddChild(ParseExpressionOrRange);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseExpressionOrRange);
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

function TDelphiParser.ParseExceptBlock: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExceptionItem

ExceptionItem
	-> ON
			[Ident ':'] QualifiedIdent DO
			[Statement]
			[';']

Backlinks: TryStatement

	try
	except
		on
			EDbException do;
	end;

	try
	except
		on E:EDbException do
	end;

	try
	except
		on E:EDbException do;
	end;

	try
	except
		on EDbException do
			begin
			end;
	end;

	try
	except
		on E:EDbException do
	end;

	try
	except
		on E:EDbException do;
	end;


Example
--------

except											;ntExcept
	on E:EArgumentNullException do		;ntExceptionHandler
		begin
		end;
	else
		begin

		end;
end;

Tree
----

ntExcept
	ntExceptionItem
		ntIdentifier anName='E'
		ntQualifiedIdentifier anName='EArgumentNullException'

	ntElse
		ntStatementList


}
	Result := TSyntaxNode2.Create(ntExcept);

	if CurrentTokenContentualKind = ptOn then
	begin
		// ExceptionItemList: one or more "on [Ident:] QualifiedIdent do Statement;"
		while CurrentTokenContentualKind = ptOn do
			Result.AddChild(ParseExceptionItem);

		if CurrentTokenKind = ptElse then
			Result.AddChild(ParseExceptionBlockElseBranch);
	end
	else if CurrentTokenKind = ptElse then
		Result.AddChild(ParseExceptionBlockElseBranch)
	else
		Result.AddChild(ParseStatementList);
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
	Result.AddChild(EatTokenEx(ptOn));

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
	Result.AddChild(EatTokenEx(ptDo));

//	[Statement]
	if not (CurrentTokenKind in [ptSemicolon, ptEnd]) then
		Result.AddChild(ParseStatement);

//	[';']
	if CurrentTokenKind = ptSemicolon then
		Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParseExceptionBlockElseBranch: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntElse);
	Result.AddChild(EatToken);
	Result.AddChild(ParseStatementList);
end;

function TDelphiParser.ParseInlineVarDeclaration: TSyntaxNode2;
begin
	// called from ParseForStatement
	Result := TSyntaxNode2.Create(ntIdentifierList);

	Result.AddChild(ParseIdentList);
	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken);
		Result.AddChild(ParseType);
	end;
end;

function TDelphiParser.ParseInlineVarSection: TSyntaxNode2;
var
	LVarDecl: TSyntaxNode2;
	LAssign: TSyntaxNode2;
begin
{
InlineVarSection
	-> VAR InlineVarDecl

InlineVarDecl
	-> IdentList [':' Type] [':=' Expression]

Important: this production is used inside ParseStatementList, so it must NOT
consume the trailing semicolon. ParseStatementList owns statement separators.
}
	Result := TSyntaxNode2.Create(ntVarSection);
	Result.AddChild(EatToken(ptVar));

	LVarDecl := TSyntaxNode2.Create(ntVariable);
	Result.AddChild(LVarDecl);

	LVarDecl.AddChild(ParseIdentList);

	if CurrentTokenKind = ptColon then
	begin
		LVarDecl.AddChild(EatToken(ptColon));
		LVarDecl.AddChild(ParseType);
	end
	else if CurrentTokenKind <> ptAssign then
		LVarDecl.AddChild(SynErrorFmt('Expected %s but found %s', [': or :=', CurrentToken.Text]));

	if CurrentTokenKind = ptAssign then
	begin
		LAssign := TSyntaxNode2.Create(ntAssign);
		LVarDecl.AddChild(LAssign);
		LAssign.AddChild(EatToken(ptAssign));
		LAssign.AddChild(ParseExpression);
	end;
end;

function TDelphiParser.ParseInlineConstSection: TSyntaxNode2;
var
	LConstDecl: TSyntaxNode2;
	LIdentNode: TSyntaxNode2;
	LValueNode: TSyntaxNode2;
begin
{
InlineConstSection
	-> CONST InlineConstDecl

InlineConstDecl
	-> Ident [':' Type] '=' TypedConstant

Important: this production is used inside ParseStatementList, so it must NOT
consume the trailing semicolon. ParseStatementList owns statement separators.
}
	Result := TSyntaxNode2.Create(ntConstants);
	Result.AddChild(EatToken(ptConst));

	LConstDecl := TSyntaxNode2.Create(ntConstant);
	Result.AddChild(LConstDecl);

	LIdentNode := ParseIdent;
	LConstDecl.AddChild(LIdentNode);
	LConstDecl[anName] := LIdentNode.Value;

	if CurrentTokenKind = ptColon then
	begin
		LConstDecl.AddChild(EatToken(ptColon));
		LConstDecl.AddChild(ParseType);
	end;

	LConstDecl.AddChild(EatToken(ptEquals));
	LValueNode := ParseTypedConstant;
	LConstDecl.AddChild(LValueNode);
	LConstDecl[anValueText] := LValueNode[anValueText];

	while IsPossiblePortabilityDirective do
		LConstDecl.AddChild(ParsePortabilityDirective);
end;

function TDelphiParser.ParseAssemblerStatement: TSyntaxNode2;
begin
{
AssemblerStatement
	-> ASM
			<assemblylanguage>
			END
}
	Result := TSyntaxNode2.Create(ntStatementList);
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
     if CurrentTokenContentualKind = ptAt then
       Result.AddChild(ParseAtExpression);
end;

function TDelphiParser.ParseTryStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#TryStatement

TryStatement				Backlinks: SimpleStatement

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
{
http://dgrok.excastle.com/Grammar.html#ExpressionList

ExpressionList																Backlinks: Atom, VariantGroup, WithStatement
	-> (Expression [','])+
}
	Result := TSyntaxNode2.Create(ntExpressionList);
	Result.AddChild(ParseExpression);		//ntExpression

	while CurrentToken.Kind = ptComma do
	begin
		Result.AddChild(EatToken);
		Result.AddChild(ParseExpression);
	end;
end;

function TDelphiParser.ParseStatementList: TSyntaxNode2;
var
	LStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#StatementList

StatementList
	-> ([Statement] [';'])+

Empty statements (ntStatement with no children) are discarded to keep the tree flat.
}
	Result := TSyntaxNode2.Create(ntStatementList);

	LStatement := ParseStatement;
	if LStatement.HasChildren then
		Result.AddChild(LStatement)
	else
		LStatement.Free;

	while CurrentTokenKind in [ptSemicolon, ptSemiColon] do
	begin
		Result.AddChild(EatToken);
		LStatement := ParseStatement;
		if LStatement.HasChildren then
			Result.AddChild(LStatement)
		else
			LStatement.Free;
	end;
end;

function TDelphiParser.ParseSimpleStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#SimpleStatement

SimpleStatement		Backlinks: Statement
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
	-> ForStatement								-> FOR (QualifiedIdent | VAR InlineVarDeclaration)
														( ':=' Expression (TO | DOWNTO) Expression
														| IN Expression )
														DO [Statement]
	-> WithStatement								-> WITH ExpressionList DO [Statement]
	-> TryStatement								-> TRY (Completed) [StatementList] ...
	-> RaiseStatement								-> RAISE [Expression [AT Expression]]
	-> InlineVarSection							-> VAR IdentList [':' Type] [':=' Expression]
	-> InlineConstSection						-> CONST Ident [':' Type] '=' TypedConstant

ParseSimpleStatement is transparent: it dispatches to the correct sub-parser
and returns the child node directly, without creating an ntStatement wrapper.
The ntStatement wrapper is created by the caller (ParseStatement).
}
	case CurrentTokenKind of
	ptInherited:
		begin
			//	-> BareInherited or InheritedVariableReference
			Result := ParseInheritedVariableReference;
		end;
	ptVar:
		begin
			//	-> InlineVarSection
			Result := ParseInlineVarSection;
		end;
	ptConst:
		begin
			//	-> InlineConstSection
			Result := ParseInlineConstSection;
		end;
	// ExpressionOrAssignment

	ptGoto:
		begin
			//	-> GotoStatement								-> GOTO LabelId
			Result := ParseGotoStatement;
		end;
	ptBegin, ptAsm:
		begin
			//	-> Block											-> BEGIN [StatementList] END
			//														-> AssemblerStatement
			Result := ParseBlock;
		end;
	ptIf:
		begin
			//	-> IfStatement		0> IF Expression THEN [Statement] (Completed)    [ELSE [Statement]]
			Result := ParseIfStatement;
		end;
	ptCase:
		begin
			//	-> CaseStatement		-> CASE Expression OF (CaseSelector)+ [ELSE [StatementList]] END
			Result := ParseCaseStatement;
		end;
	ptRepeat:
		begin
			//	-> RepeatStatement		-> REPEAT [StatementList] UNTIL Expression
			Result := ParseRepeatStatement;
		end;
	ptWhile:
		begin
			//	-> WhileStatement		-> WHILE Expression DO [Statement]
			Result := ParseWhileStatement;
		end;
	ptFor:
		begin
			//	-> ForStatement		-> FOR Ident ':=' Expression (TO | DOWNTO) Expression DO [Statement]
			//								-> FOR Ident IN Expression DO [Statement]
			Result := ParseForStatement;
		end;
	ptWith:
		begin
			//	-> WithStatement								-> WITH ExpressionList DO [Statement]
			Result := ParseWithStatement;
		end;
	ptTry:
		begin
			//	-> TryStatement								-> TRY (Completed) [StatementList] ...
			Result := ParseTryStatement;
		end;
	ptRaise:
		begin
			//	-> RaiseStatement								-> RAISE [Expression [AT Expression]]
			Result := ParseRaiseStatement;
		end;
	else
		//	-> ExpressionOrAssignment					-> Expression [':=' Expression]
		Result := ParseExpressionOrAssignment;
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

LabelId (ParseLabelID, IsPossibleLabelID)
	-> <number>
	-> Ident

Ident (ParseIdent, IsPossibleIdent)
	-> <identifier>
	-> <semikeyword>
	-> '&' <identifier>
	-> '&' <semikeyword>
	-> '&' <keyword>

It's a simple statement with an optional label before it.
Treat the label as a separate piece.

}
	Result := TSyntaxNode2.Create(ntStatement);

	// Label disambiguation: only parse as label if identifier/number is followed by ':'
	// (not ':=' which is tokenized as ptAssign, so PeekTokenKind = ptColon is unambiguous)
	if IsPossibleLabelID and (PeekTokenKind = ptColon) then
	begin
		Result.AddChild(ParseLabelId);
		Result.AddChild(EatToken(ptColon));
	end;

	// SimpleStatement is optional (empty statements are valid in Delphi)
	// Skip when current token is a known statement terminator
	if not (CurrentTokenKind in [ptSemicolon, ptEnd, ptUntil, ptExcept, ptFinally, ptElse, ptEof]) then
		Result.AddChild(ParseSimpleStatement);
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

function TDelphiParser.EatToken(const ExpectedTokenKind: TptTokenKind): TSyntaxToken;
var
	t: TSyntaxToken;
	s: string;
begin
{
Consumes the current token:

	- returns the current token
	- and advances the parser to the next token

But only if the current token's Kind is 'ExpectedTokenKind'.

See also:

- EatToken:												eats the current token no matter the kind
- EatTken(ExpectedTokenKind):						eat the current token if it is of the expected kind
- EatTokenEx(ExpectedTokenContextualKind):	eat the current token only if it's ContextualKind is the expected value
}
	t := CurrentToken;
	if t.Kind = ExpectedTokenKind then
	begin
		Result := t;
		NextToken; // advance lexer AFTER capturing token+trivia
	end
	else
	begin
		// Create a zero-width (synthesized) token for error recovery, preserving tree shape
		Result := TSyntaxToken.Create(ExpectedTokenKind, CurrentToken.Width, CurrentToken.FullWidth, '');
		Result.IsMissing := True;
		s := Format(SExpected, [TokenName(ExpectedTokenKind), TokenName(CurrentToken.Kind)]);
//		DoMessage(s, CurrentToken.Width, CurrentToken.FullWidth);
//		Log(s);

		// Do not consume unexpected real tokens here.
		// Keeping CurrentToken allows surrounding productions to recover without dropping source tokens.

		if IsDebuggerPresent then
		begin
//			OutputDebugString(PChar(s+'   '+NextFewTokens));
			Log(s+'   '+NextFewTokens);
//			Windows.DebugBreak;
		end;
	end;
end;

function TDelphiParser.EatToken: TSyntaxToken;
begin
{
Consumes the current token:

	- returns the current token
	- and advances the parser to the next token
}
	if CurrentTokenKind = ptEof then
	begin
		//	Guard: never consume the real EOF token unconditionally.
		//	The EOF sentinel must be consumed only by ParseCore's explicit EatToken(ptEOF) call.
		//  If error recovery or other callers try to eat past end-of-stream we
		//	return a synthetic missing-EOF so the real EOF stays in FTokens for ParseCore.
		Result := TSyntaxToken.Create(ptEof, CurrentToken.Width, CurrentToken.FullWidth, '');
		Result.IsMissing := True;
		Exit;
	end;

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

function TDelphiParser.ParseExpressionList: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExpressionList

ExpressionList
	-> (Expression [','])+
}
	Result := TSyntaxNode2.Create(ntExpressionList);

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
	-> Atom -> Particle -> ...
	-> UnaryOperator Factor

UnaryOperator
	-> NOT						ptNot: ParseNotOp
	-> '+'						ptPlus: ntAdd
	-> '-'						ptMinus: ParseUnaryMinus
	-> '@'						ptAt: ParseAtExpression
	-> INHERITED
}
{
	Factor is recusrive:

		Factor --> Atom
		       --> UnaryOperator Factor

	So we check for a unary operator, and if we find one, eat that, and then recurse
}

	if CurrentTokenKind in [ptNot, ptPlus, ptMinus, ptAddressOp, ptInherited] then
	begin
		Result := ParseUnaryOperator;
		Result.AddChild(ParseFactor);
	end
	else
		Result := ParseAtom;
end;

function TDelphiParser.ParseAtom: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Atom

Atom											Backlinks: Factor
	-> Particle
			( '.' ExtendedIdent
			| '[' ExpressionList ']'
			| '^'
			| '(' (ParameterExpression [','])* ')'
			)*
}

	Result := ParseParticle; // ntParticle

	// Atom suffixes: member access, indexing, dereference, call
	while CurrentTokenKind in [ptDot, ptOpenBracket, ptCaret, ptOpenParen] do
	begin
		case CurrentTokenKind of
		ptDot:
			begin
				// '.' ExtendedIdent  (member access)
				Result.AddChild(EatToken(ptDot));
				Result.AddChild(ParseExtendedIdent);
			end;
		ptOpenBracket:
			begin
				// '[' ExpressionList ']'  (indexing)
				Result.AddChild(EatToken(ptOpenBracket));
				Result.AddChild(ParseExpression);
				while CurrentTokenKind = ptComma do
				begin
					Result.AddChild(EatToken(ptComma));
					Result.AddChild(ParseExpression);
				end;
				Result.AddChild(EatToken(ptCloseBracket));
			end;
		ptCaret:
			begin
				// '^'  (pointer dereference)
				Result.AddChild(EatToken(ptCaret));
			end;
		ptOpenParen:
			begin
				// '(' (ParameterExpression [','])* ')'  (function call)
				Result.AddChild(EatToken(ptOpenParen));
				if CurrentTokenKind <> ptCloseParen then
				begin
					Result.AddChild(ParseParameterExpression);
					while CurrentTokenKind = ptComma do
					begin
						Result.AddChild(EatToken(ptComma));
						Result.AddChild(ParseParameterExpression);
					end;
				end;
				Result.AddChild(EatToken(ptCloseParen));
			end;
		end;
	end;
end;

function TDelphiParser.ParseUnaryOperator: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#UnaryOperator

UnaryOperator				Backlinks: Factor
	-> NOT
	-> '+'
	-> '-'
	-> '@'
	-> INHERITED
}
	Result := TSyntaxNode2.Create(ntUnaryOperator);
	case CurrentTokenKind of
	ptNot:				Result.AddChild(EatToken(ptNot));
	ptPlus:				Result.AddChild(EatToken(ptPlus));
	ptMinus:				Result.AddChild(EatToken(ptMinus));
	ptAddressOp:		Result.AddChild(EatToken(ptAddressOp));
	ptInherited:		Result.AddChild(EatToken(ptInherited));
	else
		Result.AddChild(SynErrorFmt('Expected %s but found %s', ['UnaryOperator', CurrentToken.Text]))
	end;
end;

function TDelphiParser.ParseParticle: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Particle

Particle											Backlinks: Atom
	-> <number>
	-> <stringliteral>
	-> Ident
	-> NIL
	-> ParenthesizedExpression		-> '(' Expression ')'
	-> SetLiteral						-> '[' [ExpressionOrRangeList] ']'
	-> STRING
	-> FILE
}

{
	TODO: Do not return ntParticle, but instead create a result based on the type
}
	Result := TSyntaxNode2.Create(ntParticle);

	if CurrentTokenKind in [ptFloat, ptIntegerConst] then
		Result.AddChild(EatToken(CurrentTokenKind))
	else if CurrentTokenKind in [ptStringLiteral] then
		Result.AddChild(EatToken(ptStringLiteral))
	else if IsPossibleIdent then
		Result.AddChild(ParseIdent)
	else if CurrentTokenKind = ptNil then
		Result.AddChild(EatToken(ptNil))
	else if CurrentTokenKind = ptOpenParen then
	begin
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseExpression);
		Result.AddChild(EatToken(ptCloseParen));
	end
	else if CurrentTokenKind = ptOpenBracket then
	begin
		Result.AddChild(EatToken(ptOpenBracket));
		Result.AddChild(ParseExpressionOrRangeList);
		Result.AddChild(EatToken(ptCloseBracket));
	end
	else if CurrentTokenKind = ptString then
		Result.AddChild(EatToken(ptString))
	else if CurrentTokenKind = ptFile then
		Result.AddChild(EatToken(ptFile))
	else if CurrentTokenContentualKind in [ptProcedure, ptFunction] then
		Result.AddChild(ParseAnonymousMethod)
	else
		Result.AddChild(SynErrorFmt('Expected %s but found %s', ['Particle', CurrentToken.Text]));
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
var
	factor: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Term

Term														Backlinks: SimpleExpression
	-> Factor (MulOp Factor)*

ntTerm


TODO: Earlier research indicated that roslyn doesn't, or can avoid, child lists, but instead have a recursive chain.
		Figure out what that looks like from an API consumer standing, and see if i want to use that.
		Term sounds like a perfect place for this. It's:

			- factor

		And if the next token is an operation, then eat the token and parse the factor.

		And that means there is a bunch of them attached under this node.

			- how does Roslyn represent it
			- or is it because every syntax node is roslyn is a custom class,
			- while our guy wanted the type to be held in a field, rather than the Delphi Type System
			- No, but seriously: one class for every node is a lot of classes.
			- right now it's a lot to have a lot of ParseXxx functions and getting them right
         - but if i change to the SyntaxNodeFactor that Roslyn uses, i can easily just pass the same nodeType
         - and certain classes can be broken out with helper properties where reading the attribute system is just no fun
			- and but if Roslyn has a generic .Child property, what is it?

Example
--------

	a * b

Returns

	ntFactor anName='a'
		ntMul
			ntFactor anName='b'

}
//	Factor
	factor := ParseFactor;

	if not (CurrentTokenKind in [ptAnd, ptDiv, ptMod, ptShl, ptShr, ptSlash, ptAsterisk]) then
	begin
		Result := factor;
		Exit;
	end;

// (MulOp Factor)*
	Result := TSyntaxNode2.Create(ntTerm);
	Result.AddChild(factor);

	while CurrentTokenKind in [ptAnd, ptDiv, ptMod, ptShl, ptShr, ptSlash, ptAsterisk] do
	begin
		Result.AddChild(ParseMultiplicativeOperator);
		Result.AddChild(ParseFactor);
	end;
end;

function TDelphiParser.ParseRelOp: TSyntaxNode2;
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
var
	left, right, binaryNode: TSyntaxNode2;
	opNode: TSyntaxNode2;
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

Sample
------

a + b - c

Returns:

ntSimpleExpression
	ntBinaryExpression
		ntIdentifier		anName='a'
		ntAddOp				anName='+'
		ntBinaryExpression
			ntAddOp			anName='-'
			ntIdentifier	anName='c'
}

	// Term (AddOp Term)*
	// Parse the first Term
	left := ParseTerm;

	// Parse zero or more (AddOp Term) pairs, folding left
	while CurrentTokenKind in [ptPlus, ptMinus, ptOr, ptXor] do
	begin
		opNode := ParseAddOp;
		right := ParseTerm;

		binaryNode := TSyntaxNode2.Create(ntBinaryExpression);
		binaryNode.Attributes[anKind] := opNode.Value;
		binaryNode.AddChild(left);
		binaryNode.AddChild(opNode);
		binaryNode.AddChild(right);

		left := binaryNode;
	end;

	Result := left;
end;

function TDelphiParser.ParseExpressionOrAssignment: TSyntaxNode2;
var
	lhs: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExpressionOrAssignment

ExpressionOrAssignment                    Backlinks: SimpleStatement
	-> Expression [':=' Expression]

Example
-------

	X := 1;

Tree
----

ntAssign
   [expression]            X

}
	lhs := ParseExpression;
	if CurrentTokenKind = ptAssign then
	begin
		Result := TSyntaxNode2.Create(ntAssign);
		Result.AddChild(lhs);
		Result.AddChild(EatToken(ptAssign));
		Result.AddChild(ParseExpression);
	end
	else
		Result := lhs;
end;

function TDelphiParser.ParseParameterExpression: TSyntaxNode2;
var
	expr: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ParameterExpression

ParameterExpression                        Backlinks: Atom (function call)
	-> Expression [':' Expression [':' Expression]]


A write parameter has the form:

	OutExpr [: MinWidth [: DecPlaces ] ]

where OutExpr is an output expression.

MinWidth and DecPlaces are type integer expressions:

- MinWidth specifies the minimum field width, which must be greater than 0.
	Exactly MinWidth characters are written (using leading blanks if necessary)
	except when OutExpr has a value that must be represented in more than MinWidth characters.
	In that case, enough characters are written to represent the value of OutExpr.
	Likewise, if MinWidth is omitted, then the necessary number of characters is
	written to represent the value of OutExpr.
- DecPlaces specifies the number of decimal places in a fixed-point representation
		of one of the Real types. It can be specified only if OutExpr is one of the Real types,
		and if MinWidth is also specified. When MinWidth is specified, it must be greater than or equal to 0.



A write parameter has the form:

	OutExpr [: MinWidth [: DecPlaces ] ]

If there's no colon, we just return the bare expression (no wrapper node).
If there IS a colon, we wrap in ntParameterExpression so that consumers
can distinguish formatted arguments from plain ones.
}
	Result := ParseExpression;
	if CurrentTokenKind = ptColon then
	begin
		// Wrap in ntParameterExpression: value ':' width [':' decimals]
		expr := Result;
		Result := TSyntaxNode2.Create(ntParameterExpression);
		Result.AddChild(expr);
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseExpression);
		if CurrentTokenKind = ptColon then
		begin
			Result.AddChild(EatToken(ptColon));
			Result.AddChild(ParseExpression);
		end;
	end;
end;

function TDelphiParser.ParseExpression: TSyntaxNode2;
var
	expr: TSyntaxNode2;
const
	RelOps: set of TptTokenKind = [ptEquals, ptGreaterThan, ptLessThan, ptGreaterThanEquals, ptLessThanEquals, ptNotEqual, ptIn, ptIs, ptAs];

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
//	-> SimpleExpression (RelOp SimpleExpression)*
	expr := ParseSimpleExpression;
	if not (CurrentTokenKind in RelOps) then
	begin
		Result := expr;
		Exit;
	end;

	Result := TSyntaxNode2.Create(ntExpression);
	Result.AddChild(expr);
	Result.AddChild(parseRelOp);

	// -> SimpleExpression (RelOp SimpleExpression)*
	Result.AddChild(ParseSimpleExpression);

	// Left-associative fold: Simple (op Simple)*
	while CurrentTokenKind in RelOps do
		Result.AddChild(ParseRelOp);
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

Example
--------

	t1, t2: Int64;

Tree
----

ntVariable
	ntIdentifierList
	#ptColon
	ntType
}

	Result := TSyntaxNode2.Create(ntVariable);

	// IdentList ':' Type
	Result.AddChild(ParseIdentList);								// ntIdentifierList
	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseType);									// ntType

	// (PortabilityDirective)*
	while IsPossiblePortabilityDirective do
		Result.AddChild(ParsePortabilityDirective);

	case CurrentTokenContentualKind of
	ptAbsolute: Result.AddChild(ParseVarAbsolute);			// ABSOLUTE t1
	ptEquals:	Result.AddChild(ParseVarEqual);				// = TypedConstant
	end;

	// (PortabilityDirective)*
	while IsPossiblePortabilityDirective do
		Result.AddChild(ParsePortabilityDirective);

	Result.AddChild(EatToken(ptSemicolon));
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
	-> Ident (',' Ident )*

Example
-------

var
	t1, t2: Int64;

expected

ntVariable     									; a variable declaration
	ntIdentifierList								; a list of identifiers (i.e. t1, t2)
		ntIdentifier anName='t1'				; identifier t1
		ntIdentifier anName='t2'				; identifeir t2
	ntType anName="Int64"
}

	Result := TSyntaxNode2.Create(ntIdentifierList);
	Result.AddChild(ParseIdent);

//	Rest of the comma separated list
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseIdent);
	end;
end;

function TDelphiParser.ParseRecordVariant: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntCaseSelector);

	// (ExpressionOrRange [','])+
	Result.AddChild(ParseCaseLabelList);

	// ':'
	Result.AddChild(EatToken(ptColon));

	// '(' [FieldList] ')'
	Result.AddChild(EatToken(ptOpenParen));
	if CurrentTokenKind <> ptCloseParen then
		Result.AddChild(ParseFieldList);
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
	Result := TSyntaxNode2.Create(ntCaseStatement);

	Result.AddChild(EatToken(ptCase));
	Result.AddChild(ParseTagField);
	Result.AddChild(EatToken(ptOf));

	Result.AddChild(ParseRecordVariant);
	while CurrentTokenKind = ptSemicolon do
	begin
		Result.AddChild(EatToken(ptSemicolon));

		// Optional trailing ';' before END or ')' is valid.
		if CurrentTokenKind in [ptEnd, ptCloseParen] then
			Break;

		Result.AddChild(ParseRecordVariant);
	end;
end;

function TDelphiParser.ParseTagField: TSyntaxNode2;
begin
	// CASE [Ident ':'] QualifiedIdent OF
	// If a tag name exists, keep it as ntName under an ntField node.
	Result := TSyntaxNode2.Create(ntField);

	if (CurrentTokenKind = ptIdentifier) and (PeekTokenKind = ptColon) then
	begin
		Result.AddChild(ParseTagFieldName);
		Result.AddChild(EatToken(ptColon));
	end;

	Result.AddChild(ParseTagFieldTypeName);
end;

function TDelphiParser.ParseTagFieldName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));
end;

function TDelphiParser.ParseTagFieldTypeName: TSyntaxNode2;
begin
	// DGrok specifies QualifiedIdent here.
	Result := ParseTypeId;
end;

function TDelphiParser.ParseFieldDeclaration: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntField);

	// Field names: X, Y, Z
	Result.AddChild(ParseFieldName);
	while CurrentTokenKind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseFieldName);
	end;

	Result.AddChild(EatToken(ptColon));
	Result.AddChild(ParseType);

	while IsPossiblePortabilityDirective do
		Result.AddChild(ParsePortabilityDirective);
end;

function TDelphiParser.ParseFieldList: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntFieldSection);

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

function TDelphiParser.ParseFieldSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#FieldSection

FieldSection			Backlinks: VisibilitySectionContent
	-> [CLASS] VAR (FieldVarDecl)+

FieldVarDecl
	-> FieldDecl					; IdentList ':' Type ['=' TypedConstant] ';'
	-> ConstantDecl				; Ident '=' TypedConstant ';'  (permissive: constant-syntax under var)

The dispatch heuristic peeks one token ahead from the current identifier:
	- Peek '=' (no colon) --> ConstantDecl  (e.g. N = 11;)
	- Peek ':' or ','     --> FieldDecl     (e.g. E: Integer;  or  O: Integer = 12;)

NOTE: Inside a class, visibility keywords and other section-starters
(const, type, procedure, etc.) terminate the FieldVarDecl loop because
their TokenKind is not ptIdentifier.

See also: https://docwiki.embarcadero.com/RADStudio/Athens/en/Fields_%28Delphi%29
}
	Result := TSyntaxNode2.Create(ntFieldSection);

//	[CLASS] VAR
	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.AddChild(EatToken(ptVar));
	end
	else
		Result.AddChild(EatToken(ptVar));

//	(FieldVarDecl)+
//	Loop while the current token looks like the start of a field or constant declaration.
//	Visibility keywords (private/protected/public/published/strict) have TokenKind=ptIdentifier
//	but are excluded via GenID so they correctly terminate the loop.
	while (CurrentTokenKind = ptIdentifier)
			and not (CurrentTokenContentualKind in [ptPrivate, ptProtected, ptPublic, ptPublished])
			and not ((CurrentTokenContentualKind = ptStrict) and (PeekTokenExID in [ptPrivate, ptProtected])) do
	begin
		if PeekTokenKind = ptEquals then
			Result.AddChild(ParseConstantDecl)		// permissive: N = 11;
		else
			Result.AddChild(ParseFieldDecl);		// normal: E: Integer;  or  O: Integer = 12;
	end;
end;

function TDelphiParser.ParseRecordType: TSyntaxNode2;
begin
{
	http://dgrok.excastle.com/Grammar.html#RecordType

RecordType														Backlinks: Type
	-> RECORD
			(VisibilitySection)*     ; see ParseVisibilitySection for refined grammar
			[VariantSection]
			END

VariantSection													Backlinks: RecordType, VariantGroup
	-> CASE [Ident ':'] QualifiedIdent OF
			(VariantGroup)+
}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := 'record';

//	-> RECORD
	Result.AddChild(EatToken(ptRecord));

//	(VisibilitySection | VisibilitySectionContent)*
	while not (CurrentTokenKind in [ptEnd, ptCase, ptEof]) do
	begin
		if IsPossibleVisibilitySection then
			Result.AddChild(ParseVisibilitySection)
		else if IsPossibleVisibilitySectionContent then
			Result.AddChild(ParseVisibilitySectionContent)
		else
			Result.AddChild(SynErrorFmt('Skipping token to recover inside record body: %s', [CurrentToken.Text]));
	end;

//	[VariantSection]
	if CurrentTokenKind = ptCase then
		Result.AddChild(ParseVariantSection);

//	END
	Result.AddChild(EatToken(ptEnd));
end;

function TDelphiParser.ParseRecordAlign: TSyntaxNode2;
begin
{
RecordAlign
	-> ALIGN ConstantExpression
}
	Result := TSyntaxNode2.Create(ntAlignmentParam);
	Result.AddChild(EatTokenEx(ptAlign));
	Result.AddChild(ParseConstantExpression);
end;

function TDelphiParser.ParseFileType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#FileType

FileType
	-> FILE
	-> FILE OF QualifiedIdent
}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := 'file';

	Result.AddChild(EatToken(ptFile));
	if CurrentTokenKind = ptOf then
	begin
		Result.AddChild(EatToken(ptOf));
		Result.AddChild(ParseType);
	end;
end;

function TDelphiParser.ParseRecordHelperType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#RecordHelperType

RecordHelperType											Backlinks: Type
	-> RECORD HELPER FOR QualifiedIdent
			(VisibilitySection)*     ; see ParseVisibilitySection for refined grammar
			END

Returns:
	ntType
}
	Result := TSyntaxNode2.Create(ntType);

//	-> RECORD HELPER FOR QualifiedIdent
	Result.AddChild(EatToken(ptRecord));
	Result.AddChild(EatTokenEx(ptHelper));
	Result.AddChild(EatToken(ptFor));

//	(VisibilitySection)*
	while IsPossibleVisibilitySection do
		Result.AddChild(ParseVisibilitySection);

//	END
	Result.AddChild(EatToken(ptEnd));
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
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := 'set';

	Result.AddChild(EatToken(ptSet));
	Result.AddChild(EatToken(ptOf));
	Result.AddChild(ParseType);
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
var
	blockStarterSeen: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#VisibilitySection

VisibilitySection
	-> [Visibility] [BlockContent] (NonBlockContent)*

A visibility section begins with an optional visibility keyword, then
optionally one block-starter (var / const / type / resourcestring / class var),
followed by zero or more non-block items (bare fields, methods, properties).
A second block-starter terminates this section so it becomes a sibling
(either inside a new VisibilitySection or as a top-level item).

Returns:
	ntVisibilitySection


Visibility
	-> STRICT PRIVATE
	-> STRICT PROTECTED
	-> PRIVATE
	-> PROTECTED
	-> PUBLIC
	-> PUBLISHED
	-> AUTOMATED

BlockContent
	-> FieldSection				-> [CLASS] VAR (FieldVarDecl)+
	-> ConstSection				-> (CONST|RESOURCESTRING) (ConstantDecl)+
	-> TypeSection					-> TYPE (TypeDecl)+

NonBlockContent
	-> FieldDecl					; bare field (no var prefix), exactly one at a time
	-> MethodOrProperty			-> MethodHeading		-> [CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR) ...
											-> Property				-> [CLASS] PROPERTY Ident ...
FieldVarDecl
	-> FieldDecl					; A: Integer;  or  O: Integer = 12;
	-> ConstantDecl				; N = 11;  (permissive: constant-syntax under var)

Example
=======

	private
		FValue: T;
		FId: TGUID;

	ntVisibilitySection anVisibility="private"
		ptPrivate
		ntField
			ntName anName="FValue"
				ptIdentifier("FValue")
			ntType anName="T"
				ptIdentifier("T")
		ntField
			ntName anName="FId"
				ptIdentifier("FId")
			ntType anName="TGUID"
				ptIdentifier("TGUID")
}
	Result := TSyntaxNode2.Create(ntVisibilitySection);

//	-> [Visibility]
	if CurrentTokenContentualKind = ptPrivate then
	begin
		Result.AddChild(EatTokenEx(ptPrivate));
		Result.Attributes[anVisibility] := 'private';
	end
	else if CurrentTokenContentualKind = ptProtected then
	begin
		Result.AddChild(EatTokenEx(ptProtected));
		Result.Attributes[anVisibility] := 'protected';
	end
	else if CurrentTokenContentualKind = ptPublic then
	begin
		Result.AddChild(EatTokenEx(ptPublic));
		Result.Attributes[anVisibility] := 'public';
	end
	else if CurrentTokenContentualKind = ptPublished then
	begin
		Result.AddChild(EatTokenEx(ptPublished));
		Result.Attributes[anVisibility] := 'published';
	end
	else if CurrentTokenContentualKind = ptAutomated then
	begin
		Result.AddChild(EatTokenEx(ptAutomated));
		Result.Attributes[anVisibility] := 'automated';
	end
	else if (CurrentTokenContentualKind = ptStrict) and (PeekTokenExID = ptPrivate) then
	begin
		Result.AddChild(EatTokenEx(ptStrict));
		Result.AddChild(EatTokenEx(ptPrivate));
		Result.Attributes[anVisibility] := 'strict private';
	end
	else if (CurrentTokenContentualKind = ptStrict) and (PeekTokenExID = ptProtected) then
	begin
		Result.AddChild(EatTokenEx(ptStrict));
		Result.AddChild(EatTokenEx(ptProtected));
		Result.Attributes[anVisibility] := 'strict protected';
	end
	else
	begin
		Result.Attributes[anVisibility] := ''; // it's the default; but i want this to be contractual
	end;

{
	(VisibilitySectionContent)*

	Block-starters (var / const / type / resourcestring / class var) are each
	consumed at most once per visibility section.  A subsequent block-starter
	terminates this section so it can be picked up as a new top-level item
	by the ParseClassType loop.
}
	blockStarterSeen := False;
	while IsPossibleVisibilitySectionContent do
	begin
		if (CurrentTokenKind in [ptVar, ptConst, ptType, ptResourceString])
				or ((CurrentTokenKind = ptClass) and (PeekTokenKind = ptVar)) then
		begin
			if blockStarterSeen then
				Break;
			blockStarterSeen := True;
		end;
		Result.AddChild(ParseVisibilitySectionContent);
	end;
end;

function TDelphiParser.ParseVisibilitySectionContent: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VisibilitySectionContent

VisibilitySectionContent
	-> FieldSection				-> [CLASS] VAR (FieldVarDecl)+
	-> FieldDecl					; bare field (no var prefix), exactly one at a time
	-> MethodOrProperty			-> MethodHeading		-> [CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR) ...
											-> Property				-> [CLASS] PROPERTY ...
	-> ConstSection				-> (CONST|RESOURCESTRING) (ConstantDecl)+
	-> TypeSection					-> TYPE (TypeDecl)+

FieldVarDecl
	-> FieldDecl					; A: Integer;  or  O: Integer = 12;
	-> ConstantDecl				; N = 11;  (permissive: no colon, treated as constant)
}

	case CurrentTokenKind of
	ptConst:				Result := ParseConstSection;					// ntConstants
	ptResourceString:	Result := ParseResStringSection;				// ntResourceStrings
	ptType:				Result := ParseTypeSection;					// ntTypeSection
	ptVar:				Result := ParseFieldSection;					// ntFieldSection (standalone var block)
	ptProperty, ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator: Result := ParseMethodOrProperty;
	ptIdentifier:		Result := ParseFieldDecl;						// ntField (bare field, no var prefix)
	ptClass:
		begin
			{
				class property
				class procedure
				class function
				class constructor
				class destructor
				class operator
				class var (FieldVarDecl)+
			}
			case PeekTokenKind of
			ptProperty, ptProcedure, ptFunction, ptConstructor, ptDestructor, ptOperator: Result := ParseMethodOrProperty;
			ptVar: Result := ParseFieldSection;						// ntFieldSection (class var block)
			else
				// class procedure|function|constructor|destructor|operator (error recovery)
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
	ptCaret:			Result.AddChild(ParsePointerSymbol);
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
				ptDot, ptCaret, ptOpenParen, ptOpenBracket:
					begin
						Result.AddChild(ParseVariableTail);
					end;
				end;
			end;
		end;
	end;

	case CurrentTokenKind of
	ptOpenParen, ptOpenBracket, ptCaret:	Result.AddChild(ParseVariableTail);
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
	Result := TSyntaxNode2.Create(ntType);

//	-> (INTERFACE | DISPINTERFACE)
	case CurrentTokenKind of
	ptInterface:
		begin
			Result.AddChild(EatToken(ptInterface));
			Result.Attributes[anType] := AttributeValueToStr(avInterface);
		end;
	ptDispInterface:
		begin
			Result.AddChild(EatToken(ptDispInterface));
			Result.Attributes[anType] := AttributeValueToStr(avDispInterface);
		end;
	else
		Result := SynErrorFmt('Expected InterfaceType but was %s', ['InterfaceType', CurrentToken.Text]);
	end;

//	['(' QualifiedIdent ')']
	if CurrentTokenKind = ptOpenParen then
	begin
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseQualifiedIdent);
		while CurrentTokenKind = ptComma do
		begin
			Result.AddChild(EatToken(ptComma));
			Result.AddChild(ParseQualifiedIdent);
		end;
		Result.AddChild(EatToken(ptCloseParen));
	end;

//	['[' Expression ']']
	if CurrentTokenKind = ptOpenBracket then
		Result.AddChild(ParseExpression);

//	(MethodOrProperty)*
//	END
	while not (CurrentTokenKind in [ptEnd, ptEof]) do
		Result.AddChild(ParseMethodOrProperty);

	Result.AddChild(EatToken(ptEnd));
end;

function TDelphiParser.ParseClassType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ClassType

ClassType					Backlinks: Type
	-> CLASS
			[ABSTRACT | SEALED]
			[ '(' (QualifiedIdent [','])+ ')' ]
			(VisibilitySection | VisibilitySectionContent)*
			END

VisibilitySection
	-> [Visibility] [BlockContent] (NonBlockContent)*

A visibility section begins with an optional visibility keyword, then
optionally one block-starter (var / const / type / resourcestring / class var),
followed by zero or more non-block items (bare fields, methods, properties).
A second block-starter terminates this section so it becomes a sibling.

Visibility
	-> STRICT PRIVATE
	-> STRICT PROTECTED
	-> PRIVATE
	-> PROTECTED
	-> PUBLIC
	-> PUBLISHED

BlockContent
	-> FieldSection              // [CLASS] VAR (FieldVarDecl)+
	-> ConstSection              // (CONST|RESOURCESTRING) (ConstantDecl)+
	-> TypeSection               // TYPE (TypeDecl)+

NonBlockContent
	-> FieldDecl                 // bare field: A: Integer;  (no var prefix)
	-> MethodOrProperty          // [CLASS] (PROCEDURE|FUNCTION|...) or [CLASS] PROPERTY

FieldSection
	-> [CLASS] VAR (FieldVarDecl)+

FieldVarDecl
	-> FieldDecl                 // A: Integer;  or  O: Integer = 12;
	-> ConstantDecl              // N = 11;  (permissive: constant-syntax under var)

ConstSection
	-> (CONST|RESOURCESTRING) (ConstantDecl)+

NOTE: Content preceded by a visibility keyword is wrapped in ntVisibilitySection.
Content at class level without a visibility keyword (e.g. bare var, bare field)
becomes a direct child of ntType(avClass).

Returns
	ntType anType="atClass"

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

//	CLASS
	Result.AddChild(EatToken(ptClass));

//	[ABSTRACT | SEALED]				; Read optional modifieers
	case CurrentTokenContentualKind of
	ptAbstract:
		begin
			Result.AddChild(EatToken(ptAbstract));
			Result.Attributes[anAbstract] := AttributeValueToStr(avTrue);
		end;
	ptSealed:
		begin
			Result.AddChild(EatToken(ptSealed));
			Result.Attributes[anSealed] := AttributeValueToStr(avTrue);
		end;
	end;

//	[ '(' (QualifiedIdent [','])+ ')' ]					; Optional ancestor list i.e. TWidget = class(TShape, IShape, IWidget)
	if CurrentTokenKind = ptOpenParen then
	begin
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseQualifiedIdent);

		while CurrentTokenKind = ptComma do
		begin
			Result.AddChild(EatToken(ptComma));
			Result.AddChild(ParseQualifiedIdent);
		end;

		Result.AddChild(EatToken(ptCloseParen));
	end;


//	(VisibilitySection | VisibilitySectionContent)*
	while not (CurrentTokenKind in [ptEnd, ptEof]) do
	begin
		if IsPossibleVisibilitySection then
			Result.AddChild(ParseVisibilitySection)
		else if IsPossibleVisibilitySectionContent then
			Result.AddChild(ParseVisibilitySectionContent)
		else
			Result.AddChild(SynErrorFmt('Expected VisibilitySection or VisibilitySectionContent but found %s', [CurrentToken.Text]));
	end;

	Result.AddChild(EatToken(ptEnd));
end;

function TDelphiParser.ParseObjectType: TSyntaxNode2;
begin
{
ObjectType
	-> OBJECT
			['(' QualifiedIdent ')']
			(VisibilitySection | VisibilitySectionContent)*
			END
}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := 'object';

	Result.AddChild(EatToken(ptObject));

	if CurrentTokenKind = ptOpenParen then
	begin
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseQualifiedIdent);

		while CurrentTokenKind = ptComma do
		begin
			Result.AddChild(EatToken(ptComma));
			Result.AddChild(ParseQualifiedIdent);
		end;

		Result.AddChild(EatToken(ptCloseParen));
	end;

	while not (CurrentTokenKind in [ptEnd, ptEof]) do
	begin
		if IsPossibleVisibilitySection then
			Result.AddChild(ParseVisibilitySection)
		else if IsPossibleVisibilitySectionContent then
			Result.AddChild(ParseVisibilitySectionContent)
		else
			Result.AddChild(SynErrorFmt('Expected VisibilitySection or VisibilitySectionContent but found %s', [CurrentToken.Text]));
	end;

	Result.AddChild(EatToken(ptEnd));
end;

function TDelphiParser.ParseClassHelperType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ClassHelperType

ClassHelperType									Backlinks: Type
	-> CLASS HELPER
			['(' QualifiedIdent ')']
			FOR QualifiedIdent
			(VisibilitySection|VisibilitySectionContent)*
			END

VisibilitySection  ; see ParseVisibilitySection for refined grammar
	-> [Visibility] [BlockContent] (NonBlockContent)*

Visibility												Backlinks: VisibilitySection
	-> STRICT PRIVATE
	-> STRICT PROTECTED
	-> PRIVATE
	-> PROTECTED
	-> PUBLIC
	-> PUBLISHED
}
	Result := TSyntaxNode2.Create(ntHelper);
	Result.AddChild(EatToken(ptClass));
	Result.AddChild(EatTokenEx(ptHelper));

//	['(' QualifiedIdent ')']
	if CurrentTokenKind = ptOpenParen then
	begin
		Result.AddChild(EatToken(ptOpenParen));
		Result.AddChild(ParseAncestorList); // ntAncestorList
		Result.AddChild(EatToken(ptCloseParen));
	end;

//	FOR QualifiedIdent
	Result.AddChild(EatToken(ptFor));
	Result.AddChild(ParseType);

//	(VisibilitySection)*
	while IsPossibleVisibilitySection do
		Result.AddChild(ParseVisibilitySection);

	Result.AddChild(EatToken(ptEnd));
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

function TDelphiParser.ParseClassProperty: TSyntaxNode2;
begin
	Result := PoisonNode;
end;

function TDelphiParser.ParseParameterList: TSyntaxNode2;
begin
{
	['(' (Parameter [';'])* ')']

Returns
	ntParameterList

	TODO: does the grammer allow an optional semicolon before the closing param, e.g.:
		FrobTheTrobber(a: string; b: string;);
	Because that's what the grammer allows. When i think it's trying to say:
		If you see another semicolon, then read another parameter)
}
	Result := TSyntaxNode2.Create(ntParameterList);
	Result.AddChild(EatToken(ptOpenParen));

	Result.AddChild(ParseParameter);			// ntParameter
	while CurrentTokenKind = ptSemicolon do
	begin
		Result.AddChild(EatToken(ptSemicolon));
		Result.AddChild(ParseParameter);
	end;

	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParseParameter: TSyntaxNode2;
var
	attributeNode: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Parameter

Parameter													Backlinks: MethodHeading, ProcedureType, Property
	-> [AttributeSection]*
			[VAR | CONST | OUT]
			IdentList
			[':' ParameterType]
			['=' Expression]

Returns:
	ntParameter
}
	Result := TSyntaxNode2.Create(ntParameter);

//	-> [AttributeSection]*
//	   Handles Delphi/FPC-style parameter attributes like: [ref] const A: Integer
	while CurrentTokenKind = ptOpenBracket do
	begin
		attributeNode := TSyntaxNode2.Create(ntAttributes);
		attributeNode.AddChild(EatToken(ptOpenBracket));
		if CurrentTokenKind <> ptCloseBracket then
			attributeNode.AddChild(ParseAttributeList);
		attributeNode.AddChild(EatToken(ptCloseBracket));
		Result.AddChild(attributeNode);
	end;

//	-> [VAR | CONST | OUT]
	case CurrentTokenContentualKind of
	ptVar: 	Result.AddChild(EatTokenEx(ptVar));
	ptConst:	Result.AddChild(EatTokenEx(ptConst));
	ptOut:	Result.AddChild(EatTokenEx(ptOut));
	end;

//	IdentList
	Result.AddChild(ParseIdentList);

//	[':' ParameterType]
	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseParameterType);	// ntParameterType
	end;

//	['=' Expression]
	if CurrentTokenKind = ptEquals then
	begin
		Result.AddChild(EatToken(ptEquals));
		Result.AddChild(ParseExpression);
	end;
end;

function TDelphiParser.ParseParameterType: TSyntaxNode2;
{
http://dgrok.excastle.com/Grammar.html#ParameterType

ParameterType													Backlinks: Parameter
	-> QualifiedIdent
	-> STRING
	-> FILE
	-> OpenArray				-> ARRAY OF ...

Returns
	ntParameterType

TODO: note that if only look at the current token for ARRAY, and then call ParseOpenArray,
   there is the possiblity that array is not a reserved word, and i can actually have a quialifiedIdent of 'array',
   in which case i should have taken the QualifiedIdent branch.

   This all depends on of ARRAY is a reserved word or not. But then all reserved words can be used if i say

		&array: string

	Right?

	Ok, i just checked: array *is* a reserved word.

	Which mean it's ok to simply check for 'array' and take the array of branch

	But this brings me to the larger question, and the huge project goal:
		- what is the correct parse tree in ever one of these edge cases.

	We need another test cases file that just exercises each of these parsing flow edge cases.
}
begin
	Result := TSyntaxNode2.Create(ntParameterType);

	if CurrentTokenKind = ptString then
		Result.AddChild(EatToken(ptString))
	else if CurrentTokenKind = ptFile then
		Result.AddChild(EatToken(ptFile))
	else if (CurrentTokenKind = ptArray) then
	begin
{
		OpenArray											Backlinks: ParameterType
			-> ARRAY OF QualifiedIdent
			-> ARRAY OF STRING
			-> ARRAY OF FILE
			-> ARRAY OF CONST
}
		Result.AddChild(EatToken(ptArray));
		Result.AddChild(EatToken(ptOf));
		case CurrentTokenKind of
		ptString:	Result.AddChild(EatToken(ptString));
		ptFile:		Result.AddChild(EatToken(ptFile));
		ptConst:		Result.AddChild(EatToken(ptConst));
		else
			Result.AddChild(ParseQualifiedIdent);
		end;
	end
	else
		Result.AddChild(ParseQualifiedIdent);
end;

function TDelphiParser.ParseProcedureType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ProcedureType

ProcedureType												Backlinks: Type
	-> (PROCEDURE | FUNCTION)
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]
			(Directive)*
			[OF OBJECT]
			(Directive)*

Returns:
	ntType anName='FrobTheGrobber' anType='procedure'
		ntParameterList

}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := 'procedure';
	Result.Attributes[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptProcedure));

//	['(' (Parameter [';'])* ')']
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseParameterList);

//	[':' MethodReturnType]
//	Only functions have a return type; procedures do not.
	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseMethodReturnType);
	end;

//	(Directive)*
	while IsPossibleDirective do
		Result.AddChild(ParseDirective);

//	[OF OBJECT]
	if (CurrentTokenKind = ptOf) then
	begin
		Result.AddChild(EatToken(ptOf));
		Result.AddChild(EatToken(ptObject));
	end;

//	(Directive)*
	while IsPossibleDirective do
		Result.AddChild(ParseDirective);
end;

function TDelphiParser.ParseAnonymousMethod: TSyntaxNode2;
begin
{
AnonymousMethod
	-> PROCEDURE ['(' (Parameter [';'])* ')'] Block
	-> FUNCTION  ['(' (Parameter [';'])* ')'] ':' TypeId Block
}
	Result := TSyntaxNode2.Create(ntAnonymousMethod);

	case CurrentTokenContentualKind of
	ptProcedure:
		begin
			Result.Attributes[anKind] := AttributeValueToStr(avProcedure);
			Result.AddChild(EatTokenEx(ptProcedure));
		end;
	ptFunction:
		begin
			Result.Attributes[anKind] := AttributeValueToStr(avFunction);
			Result.AddChild(EatTokenEx(ptFunction));
		end;
	else
		begin
			Result.AddChild(SynErrorFmt('Expected %s but found %s', ['anonymous method', CurrentToken.Text]));
			Exit;
		end;
	end;

	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseParameterList);

	if CurrentTokenContentualKind = ptFunction then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseTypeId);
	end;

	Result.AddChild(ParseBlock);
end;

function TDelphiParser.ParseAnonymousMethodType: TSyntaxNode2;
begin
{
AnonymousMethodType
	-> REFERENCE TO PROCEDURE ['(' (Parameter [';'])* ')']
	-> REFERENCE TO FUNCTION  ['(' (Parameter [';'])* ')'] ':' TypeId
}
	Result := TSyntaxNode2.Create(ntType);
	Result.Attributes[anType] := 'reference';

	Result.AddChild(EatTokenEx(ptReference));
	Result.AddChild(EatToken(ptTo));

	case CurrentTokenContentualKind of
	ptProcedure:
		begin
			Result.Attributes[anKind] := AttributeValueToStr(avProcedure);
			Result.AddChild(EatTokenEx(ptProcedure));
		end;
	ptFunction:
		begin
			Result.Attributes[anKind] := AttributeValueToStr(avFunction);
			Result.AddChild(EatTokenEx(ptFunction));
		end;
	else
		begin
			Result.AddChild(SynErrorFmt('Expected %s but found %s', ['PROCEDURE or FUNCTION', CurrentToken.Text]));
			Exit;
		end;
	end;

	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseParameterList);

	if CurrentTokenContentualKind = ptFunction then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseTypeId);
	end;
end;

function TDelphiParser.ParsePointerSymbol: TSyntaxNode2;
begin
	Result := PoisonNode;
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
	Result.AddChild(EatToken(ptCaret));
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

	if (CurrentTokenKind = ptOpenBracket) then
	begin
		Result.AddChild(EatToken(ptOpenBracket));
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

function TDelphiParser.ParseDotOp: TSyntaxNode2;
begin
   Result := TSyntaxNode2.Create(ntDot);
   Result.AddChild(EatToken(ptDot));
end;

function TDelphiParser.ParseTypeDeclaration: TSyntaxNode2;

var
	typeParams: TSyntaxNode2;
	attributeNode: TSyntaxNode2;

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

	// Leading type attributes: [Attr] [Attr2(...)] TypeName = ...
	while CurrentTokenKind = ptOpenBracket do
	begin
		attributeNode := TSyntaxNode2.Create(ntAttributes);
		attributeNode.AddChild(EatToken(ptOpenBracket));
		if CurrentTokenKind <> ptCloseBracket then
			attributeNode.AddChild(ParseAttributeList);
		attributeNode.AddChild(EatToken(ptCloseBracket));
		Result.AddChild(attributeNode);
	end;

	Result.Attributes[anName] := CurrentToken.ValueText;
	Result.AddChild(EatToken(ptIdentifier));

	// Parse <T> as ntTypeParams
	if CurrentTokenKind = ptLessThan then
	begin
		typeParams := ParseTypeParams;
		Result.AddChild(typeParams);
	end;

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

	// Optional Lazarus/FPC record alignment suffix: "end align <const-expr>;"
	if (CurrentTokenContentualKind = ptAlign) then
		Result.AddChild(ParseRecordAlign);

	// PortabilityDirective* — only after the full <Type> branch
	while IsPortabilityDirectiveToken(CurrentTokenKind) do
		Result.AddChild(ParsePortabilityDirective);

	Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParseType: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#Type

Type
	-> EnumeratedType					'(' (EnumeratedTypeElement [','])+ ')'
	-> ArrayType						ARRAY ['[' (Type [','])+ ']'] OF Type
	-> FileType							FILE
	-> RecordHelperType				RECORD HELPER FOR QualifiedIdent
	-> RecordType						RECORD
	-> PointerType						'^' Type
	-> StringType						STRING
	-> ProcedureType					(PROCEDURE | FUNCTION)
	-> ClassHelperType				CLASS HELPER
	-> ClassOfType						CLASS OF QualifiedIdent
	-> ClassType						CLASS
	-> ObjectType						OBJECT
	-> InterfaceType					(INTERFACE | DISPINTERFACE)
	-> PackedType						PACKED Type
	-> TypeId							<identifier> ['.' <identifier>]*     ; named type reference
	-> ExpressionOrRange				SimpleExpression ['..' SimpleExpression]  ; subrange or literal expression

Note: Delphi assumes that a Type starting with '(' is an enum, not an expression.
Note: An identifier always denotes a named type (TypeId); subranges/expressions
      start with non-identifier tokens (literals, parentheses, unary operators).

Returns:
	ntType
}

//	EnumeratedType		-> '(' (EnumeratedTypeElement [','])+ ')'
	if NextTokensAre([ptOpenParen]) then
		Result := ParseEnumeratedType

	else if CurrentTokenKind = ptOpenBracket then
		Result := ParseSubrangeType

//	ArrayType			-> ARRAY ['[' (Type [','])+ ']'] OF Type
	else if CurrentTokenKind = ptArray then
		Result := ParseArrayType

//	SetType				-> SET OF Type
	else if NextTokensAre([ptSet, ptOf]) then
		Result := ParseSetType

//	FileType				-> FILE
//	FielType				-> FILE OF QualifiedIdent
	else if NextTokensAre([ptFile]) then
		Result := ParseFileType

//	-> RECORD HELPER FOR QualifiedIdent
	else if NextTokensAre([ptRecord, ptHelper, ptFor]) then
		Result := ParseRecordHelperType

//	-> RECORD
	else if CurrentTokenKind = ptRecord then
		Result := ParseRecordType

//	^Type
	else if CurrentTokenKind = ptCaret then
		Result := ParsePointerType

//	-> STRING
//	-> STRING '[' Expression ']'
	else if CurrentTokenKind = ptString then
		Result := ParseStringType

//	-> PROCEDURE
	else if (CurrentTokenContentualKind = ptReference) and (PeekTokenKind = ptTo) then
		Result := ParseAnonymousMethodType

//	-> PROCEDURE
	else if CurrentTokenKind = ptProcedure then
		Result := ParseProcedureType					// -> (PROCEDURE | FUNCTION)

//	-> FUNCTION
	else if CurrentTokenKind = ptFunction then
		Result := ParseProcedureType					// -> (PROCEDURE | FUNCTION)

//	-> CLASS HELPER
	else if NextTokensAre([ptClass, ptHelper]) then
		Result := ParseClassHelperType

//	-> CLASS OF QualifiedIdent
	else if NextTokensAre([ptClass, ptOf]) then
		Result := ParseClassOfType

//	-> CLASS
	else if CurrentTokenKind = ptClass then
		Result := ParseClassType

//	-> OBJECT
	else if CurrentTokenKind = ptObject then
		Result := ParseObjectType

//	-> (INTERFACE | DISPINTERFACE)
	else if CurrentTokenKind in [ptInterface, ptDispInterface] then
		Result := ParseInterfaceType

//	-> PACKED Type
	else if CurrentTokenKind = ptPacked then
	begin
		{
			PackedType							Backlinks: Type
				-> PACKED Type
		}
		Result := TSyntaxNode2.Create(ntType);
		Result.AddChild(EatToken(ptPacked));
		Result.AddChild(ParseType);
	end
	else if CurrentTokenKind = ptIdentifier then
	begin
//		Named type (simple or qualified): Integer, System.SysUtils.TStringList, etc.
//		ParseTypeId produces ntType anName="..." which is the correct node for type references.
		Result := ParseTypeId;
	end
	else
	begin
//		ExpressionOrRange -> SimpleExpression ['..' SimpleExpression]
//		Handles subranges (0..255), negative ranges (-1..1), parenthesized expressions, etc.
//		These always start with a non-identifier token (literal, '(', '-', etc.).
		Result := TSyntaxNode2.Create(ntExpressionOrRange);
		Result.AddChild(ParseSimpleExpression);

		if CurrentTokenKind = ptDotDot then
		begin
			Result.AddChild(EatToken(ptDotDot));
			Result.AddChild(ParseSimpleExpression);
		end;
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

	// Optional generic type arguments on the first identifier segment.
	if CurrentTokenKind = ptLessThan then
	begin
		Result.AddChild(EatToken(ptLessThan));
		Result.AddChild(ParseTypeArgs);
		Result.AddChild(EatToken(ptGreaterThan));
	end;

	while CurrentTokenKind = ptDot do
	begin
		Result.AddChild(EatToken(ptDot));
		if CurrentTokenKind = ptIdentifier then
		begin
			typeName := typeName + '.' + CurrentToken.ValueText;
			Result.AddChild(EatToken(ptIdentifier));

			// Optional generic type arguments on qualified segments.
			if CurrentTokenKind = ptLessThan then
			begin
				Result.AddChild(EatToken(ptLessThan));
				Result.AddChild(ParseTypeArgs);
				Result.AddChild(EatToken(ptGreaterThan));
			end;
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

function TDelphiParser.ParseConstantValueTyped: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntValue);
	Result.AddChild(ParseTypedConstant);
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

function TDelphiParser.ParseLabelDeclSection: TSyntaxNode2;
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

function TDelphiParser.ParseMethodHeading: TSyntaxNode2;
var
	directiveNode: TSyntaxNode2;
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
			if CurrentTokenContentualKind = ptOperator then
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

	// Method resolution clause, e.g.:
	//   procedure IFoo.Bar = MyBar;
	if CurrentTokenKind = ptEquals then
	begin
		Result.AddChild(EatToken(ptEquals));
		Result.AddChild(ParseQualifiedIdent);
	end;

	if CurrentTokenKind = ptSemiColon then
		Result.AddChild(EatToken(ptSemicolon));

	//TODO: Add FINAL
	while IsPossibleDirective do
	begin
		directiveNode := ParseDirective;
		Result.AddChild(directiveNode);

		if directiveNode.Attributes[anForward] <> '' then
			Result.Attributes[anForward] := directiveNode.Attributes[anForward];
		if directiveNode.Attributes[anExternal] <> '' then
			Result.Attributes[anExternal] := directiveNode.Attributes[anExternal];
	end;

	// Allow a trailing semicolon after the final directive, e.g.:
	//   destructor Destroy; override;
	if CurrentTokenKind = ptSemiColon then
		Result.AddChild(EatToken(ptSemicolon));
end;

function TDelphiParser.ParseMethodImplementation: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodImplementation

MethodImplementation							Backlinks: ImplementationDecl

	; If the MethodHeading does not include 'external' or 'forward':
	-> MethodHeading
			FancyBlock ';'

	; If the MethodHeading does include 'external' or 'forward':
	-> MethodHeading
}
	Result := ParseMethodHeading;

	if (Result.Attributes[anExternal] = '') and (Result.Attributes[anForward] = '') then
	begin
		{
			FancyBlock ';'
			---------------

			FancyBlock							Backlinks: MethodImplementation
				-> (ImplementationDecl)*
						Block
		}
		while IsPossibleImplementationDecl do
			Result.AddChild(ParseImplementationDecl);
		Result.AddChild(ParseBlock);

		Result.AddChild(EatToken(ptSemicolon));
	end;
end;

function TDelphiParser.ParseFunctionMethodHeading: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodHeading

MethodHeading													Backlinks: InterfaceDecl, MethodImplementation, MethodOrProperty
	-> [CLASS]
			FUNCTION
			QualifiedIdent
			(
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]
			(Directive)*
			| '=' Ident
			)
			[';']
}
	Result := TSyntaxNode2.Create(ntMethod);
	Result.Attributes[anKind] := AttributeValueToStr(avFunction);

// [CLASS] FUNCTION QualifiedIdent
	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.Attributes[anClass] := AttributeValueToStr(avTrue);
	end;

	Result.AddChild(EatToken(ptFunction));
	Result.AddChild(ParseQualifiedIdent);

//	['(' (Parameter [';'])* ')']
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseParameterList);

//	[':' MethodReturnType]
	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseTypeId);
	end;
end;

function TDelphiParser.ParseProcedureDeclarationSection: TSyntaxNode2;
begin
	Result := PoisonNode;
end;

function TDelphiParser.ParseProcedureMethodHeading: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodHeading

MethodHeading
	-> [CLASS]
			PROCEDURE
			QualifiedIdent
			(
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]
			(Directive)*
			| '=' Ident
			)
			[';']
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
		Result.AddChild(ParseParameterList);
end;

function TDelphiParser.ParseConstructorMethodHeading: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodHeading

MethodHeading													Backlinks: InterfaceDecl, MethodImplementation, MethodOrProperty
	-> [CLASS]
			CONSTRUCTOR
			QualifiedIdent
			(
			['(' (Parameter [';'])* ')']
			(Directive)*
			| '=' Ident
			)
			[';']
}
	Result := TSyntaxNode2.Create(ntMethod);
	Result.Attributes[anKind] := AttributeValueToStr(avConstructor);

// [CLASS] CONSTRUCTOR QualifiedIdent
	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.Attributes[anClass] := AttributeValueToStr(avTrue);
	end;

	Result.AddChild(EatToken(ptConstructor));
	Result.AddChild(ParseQualifiedIdent);

//	['(' (Parameter [';'])* ')']
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseParameterList);
end;

function TDelphiParser.ParseDestructorMethodHeading: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodHeading

MethodHeading													Backlinks: InterfaceDecl, MethodImplementation, MethodOrProperty
	-> [CLASS]
			DESTRUCTOR
			QualifiedIdent
			(
			['(' (Parameter [';'])* ')']
			(Directive)*
			| '=' Ident
			)
			[';']
}
	Result := TSyntaxNode2.Create(ntMethod);
	Result.Attributes[anKind] := AttributeValueToStr(avDestructor);

// [CLASS] DESTRUCTOR QualifiedIdent
	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.Attributes[anClass] := AttributeValueToStr(avTrue);
	end;

	Result.AddChild(EatToken(ptDestructor));
	Result.AddChild(ParseQualifiedIdent);

//	['(' (Parameter [';'])* ')']
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseParameterList);
end;

function TDelphiParser.ParseOperatorMethodHeading: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#MethodHeading

MethodHeading													Backlinks: InterfaceDecl, MethodImplementation, MethodOrProperty
	-> [CLASS]
			OPERATOR
			QualifiedIdent
			(
			['(' (Parameter [';'])* ')']
			[':' MethodReturnType]
			(Directive)*
			| '=' Ident
			)
			[';']
}
	Result := TSyntaxNode2.Create(ntMethod);
	Result.Attributes[anKind] := AttributeValueToStr(avOperator);

// [CLASS] OPERATOR QualifiedIdent
	if CurrentTokenKind = ptClass then
	begin
		Result.AddChild(EatToken(ptClass));
		Result.Attributes[anClass] := AttributeValueToStr(avTrue);
	end;

	Result.AddChild(EatTokenEx(ptOperator));
	Result.AddChild(ParseQualifiedIdent);

//	['(' (Parameter [';'])* ')']
	if CurrentTokenKind = ptOpenParen then
		Result.AddChild(ParseParameterList);

//	[':' MethodReturnType]
	if CurrentTokenKind = ptColon then
	begin
		Result.AddChild(EatToken(ptColon));
		Result.AddChild(ParseTypeId);
	end;
end;

function TDelphiParser.ParseVarSection: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#VarSection

VarSection
	-> (VAR | THREADVAR) (VarDecl)+

Returns:
	ntVarSection

Example
---------

var
	t1, t2: Int64;

Tree
------

ntVarSection                        ; var
	ntVariable
		ntIdentifierList
			ntIdentifier anName="t1"
			ntIdentifier anName="t2"
		ntType anName="Int64"
}
	case CurrentTokenKind of
	ptVar:
	begin
		Result := TSyntaxNode2.Create(ntVarSection);
		Result.AddChild(EatToken(ptVar));
	end;
	ptThreadVar:
	begin
		Result := TSyntaxNode2.Create(ntVarSection);
		Result.AddChild(EatToken(ptThreadVar));
	end;
	else
		Result := SynError('Expected var section');
	end;

//	(VarDecl)+
	// Inside the var section, we need to parse at least 1 variable declaration.
	// TODO: i know a class can have a var section, and then no vars. I think that is a different chain that doesn't call this one.
	Result.AddChild(ParseVarDecl);		// ntVariable

	// and keep looping until the next token isn't an identifier
	while IsPossibleIdent do
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
	directiveType := CurrentTokenContentualKind;

	// the leading semicolon is optional
	if CurrentTokenKind = ptSemicolon then
		directiveType := PeekTokenExID;

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
			((CurrentTokenKind = ptIdentifier) and (CurrentTokenContentualKind = ptOperator));
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

//function TDelphiParser.IsPossibleSimpleExpression: Boolean;
//begin
{
http://dgrok.excastle.com/Grammar.html#SimpleExpression

SimpleExpression		-> Term (AddOp Term)* -> Term -> Factor (MulOp Factor)*

   Do not write it. It's recurisive in the parent. Dont' peek ahead, cycle if it's the special ..
}
//end;

function TDelphiParser.IsPossibleTypeDeclaration: Boolean;
var
	i, depth: Integer;
	k: TptTokenKind;

	function PeekKind(Offset: Integer): TptTokenKind;
	begin
		if Offset = 0 then
			Result := CurrentToken.Kind
		else
			Result := PeekToken(Offset).Kind;
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
{
Visibility
	-> STRICT PRIVATE
	-> STRICT PROTECTED
	-> PRIVATE
	-> PROTECTED
	-> PUBLIC
	-> PUBLISHED
	-> AUTOMATED
}
	Result :=
			(CurrentTokenContentualKind in [ptPrivate, ptProtected, ptPublic, ptPublished, ptAutomated])
			or (
				(CurrentTokenContentualKind = ptStrict)
				and
				(PeekTokenExID in [ptPrivate, ptProtected])
			);
end;

function TDelphiParser.IsPossibleVisibilitySectionContent: Boolean;
begin
{
http://dgrok.excastle.com/Grammar.html#VisibilitySectionContent

VisibilitySectionContent
	-> FieldSection				-> [CLASS] VAR (FieldVarDecl)+
	-> FieldDecl					; bare field (no var prefix), exactly one at a time
	-> MethodOrProperty			-> MethodHeading		-> [CLASS] (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR) ...
											-> Property				-> [CLASS] PROPERTY ...
	-> ConstSection				-> (CONST|RESOURCESTRING) (ConstantDecl)+
	-> TypeSection					-> TYPE (TypeDecl)+

FieldVarDecl
	-> FieldDecl					; A: Integer;  or  O: Integer = 12;
	-> ConstantDecl				; N = 11;  (permissive: no colon, treated as constant)

FieldDecl
	-> IdentList ':' Type (PortabilityDirective)* ['=' TypedConstant] (PortabilityDirective)* [';']
IdentList
	-> (Ident [','])+
Ident
	-> <identifier>
	-> <semikeyword>
	-> '&' <identifier>
	-> '&' <semikeyword>
	-> '&' <keyword>

NOTE: Visibility keywords (private, protected, public, published, strict) have
TokenKind=ptIdentifier but are EXCLUDED here so they are handled by
IsPossibleVisibilitySection instead, preventing the inner content loop
from consuming the next visibility section's keyword as a field name.
}

	if ((CurrentTokenKind = ptClass) and (PeekTokenKind = ptVar)) or (CurrentTokenKind = ptVar) then
	begin
		//	-> FieldSection				-> [CLASS] VAR (FieldVarDecl)+
		Result := True;
	end
	else if IsPossibleMethodHeading or IsPossibleProperty then
	begin
		// -> MethodOrProperty (covers class operator where operator may be contextual)
		Result := True;
	end
	else if CurrentTokenKind in [ptConst, ptResourceString] then
	begin
		// -> ConstSection				-> (CONST|RESOURCESTRING) (ConstantDecl)+
		Result := True;
	end
	else if CurrentTokenKind = ptType then
	begin
		// -> TypeSection					-> TYPE (TypeDecl)+
		Result := True;
	end
	else if (CurrentTokenKind = ptAmpersand) and ((PeekTokenKind = ptIdentifier) or IsReservedWord(PeekTokenKind)) then
	begin
		// Escaped identifier (e.g. &private) - always starts a FieldDecl
		Result := True;
	end
	else if (CurrentTokenKind = ptIdentifier) then
	begin
		// Exclude visibility keywords - they start a new VisibilitySection, not content.
		// These directives have TokenKind=ptIdentifier but GenID distinguishes them.
		if CurrentTokenContentualKind in [ptPrivate, ptProtected, ptPublic, ptPublished, ptAutomated] then
			Result := False
		else if (CurrentTokenContentualKind = ptStrict) and (PeekTokenExID in [ptPrivate, ptProtected]) then
			Result := False
		else
			Result := True;
	end
	else
		Result := False;
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
var
	geToken: TSyntaxToken;
	equalsToken: TSyntaxToken;
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
	// Handle the >= quirk: the tokenizer's maximal-munch produces a single
	// ptGreaterThanEquals when ">" is immediately followed by "=". In a generic
	// declaration the ">" closes the type params and the "=" begins the type
	// definition, so we split the token and inject the "=" back into the stream.
	if CurrentTokenKind = ptGreaterThanEquals then
	begin
		geToken := EatToken(ptGreaterThanEquals);
		TSyntaxToken.SplitGreaterThanEquals(geToken, {out}equalsToken);
		Result.AddChild(geToken); // now ptGreaterThan
		FTokens.Insert(FCurrent, equalsToken); // caller will see ptEquals
	end
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

//	-> CONST (ConstantDecl)+
	Result := TSyntaxNode2.Create(ntConstants);
	Result.AddChild(EatToken(ptConst));

	Result.AddChild(ParseConstantDecl);
	while IsPossibleConstantDecl do
		Result.AddChild(ParseConstantDecl);
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
	Result := IsPossibleIdent;

	// Inside class bodies, visibility keywords (private/protected/public/published)
	// have TokenKind = ptIdentifier and would be consumed as constant names.
	// Exclude them so the const loop terminates at the next visibility section.
	if Result then
	begin
		if CurrentTokenContentualKind in [ptPrivate, ptProtected, ptPublic, ptPublished] then
			Result := False
		else if (CurrentTokenContentualKind = ptStrict) and (PeekTokenExID in [ptPrivate, ptProtected]) then
			Result := False;
	end;

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
	Result := IsPossibleIdent;
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
	-> AssemblyAttribute		==> ntAttribute
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
	else if (CurrentTokenKind = ptOpenBracket) and (PeekTokenExID = ptAssembly) then
		Result := ParseAssemblyAttribute
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
	while CurrentTokenContentualKind in [ptIndex, ptName] do
	begin
		Result.AddChild(EatToken());
		Result.AddChild(ParseExpression);
	end;
end;

function TDelphiParser.ParseExportsStatement: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExportsStatement

ExportsStatement
			Backlinks: ImplementationDecl
	-> EXPORTS (ExportsItem [','])+ ';'

Returns
	ntExports
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
	Result := (CurrentTokenContentualKind = ptRequires);
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
	if CurrentToken.Kind in [ptUses, ptContains] then
		Result.AddChild(ParseUsesClause);

{
	(InterfaceDecl)*
		zero or more InterfaceDecl
}
	while (CurrentToken.Kind in [ptConst, ptFunction, ptResourceString, ptProcedure,
		ptThreadVar, ptType, ptVar, ptExports]) or
		((CurrentToken.Kind = ptOpenBracket) and (PeekTokenExID = ptAssembly)) do
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
	if not (CurrentTokenContentualKind in [ptPlatform, ptDeprecated, ptLibrary, ptExperimental]) then
	begin
		Result := SynErrorFmt('Expected %s but was %s', ['PortabilityDirective', CurrentToken.Text]);
		Exit;
	end;

	Result := TSyntaxNode2.Create(ntPortabilityDirective);

	case CurrentTokenContentualKind of
	ptPlatform:
		begin
			Result.AddChild(EatTokenEx(ptPlatform));
			Result.Attributes[anPlatform] := AttributeValueToStr(avTrue);
		end;
	ptLibrary:
		begin
			Result.AddChild(EatTokenEx(ptLibrary));
			Result.Attributes[anLibrary] := AttributeValueToStr(avTrue);
		end;
	ptExperimental:
		begin
			Result.AddChild(EatTokenEx(ptExperimental));
			Result.Attributes[anExperimental] := AttributeValueToStr(avTrue);
		end;
	ptDeprecated:
		begin
			Result.AddChild(EatTokenEx(ptDeprecated));
			Result.Attributes[anDeprecated] := AttributeValueToStr(avTrue);

			// Check for deprecated 'use this other unit'
			// [ptDeprecated] [ptStringLiteral]
			if CurrentTokenKind = ptStringLiteral then
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
	Result := (CurrentTokenContentualKind in [ptPlatform, ptDeprecated, ptLibrary, ptExperimental]);
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

	case CurrentTokenContentualKind of
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

function TDelphiParser.ParseScriptFile: TSyntaxNode2;
const
	DECL_START_TOKENS = [ptClass, ptConst, ptConstructor, ptDestructor, ptExports,
		ptFunction, ptLabel, ptOperator, ptProcedure, ptResourceString, ptType,
		ptThreadVar, ptVar];
var
	stmt: TSyntaxNode2;
begin
{
Script mode accepts a mix of top-level declarations and executable statements.

- Parse as a statement list by default.
- Fall back to declaration parsing only when the first token is a known declaration-start keyword.
}
	if not (CurrentTokenKind in [ptClass, ptConst, ptConstructor, ptDestructor, ptExports,
		ptFunction, ptLabel, ptOperator, ptProcedure, ptResourceString, ptType, ptThreadVar, ptVar]) then
	begin
		Result := ParseStatementList;
		Exit;
	end;

	Result := TSyntaxNode2.Create(ntStatementList);

	while CurrentTokenKind <> ptEof do
	begin
		case CurrentTokenKind of
		// In script mode, method starters should parse full implementations
		// (header + optional directives + body), not declaration-only sections.
		ptClass:				Result.AddChild(ParseMethodImplementation);
		ptConst:				Result.AddChild(ParseConstSection);
		ptConstructor:		Result.AddChild(ParseMethodImplementation);
		ptDestructor:		Result.AddChild(ParseMethodImplementation);
		ptExports:			Result.AddChild(ParseExportsStatement);
		ptFunction:			Result.AddChild(ParseMethodImplementation);
		ptLabel:				Result.AddChild(ParseLabelDeclSection);
		ptOperator:			Result.AddChild(ParseMethodImplementation);
		ptProcedure:		Result.AddChild(ParseMethodImplementation);
		ptResourceString:	Result.AddChild(ParseConstSection);
		ptType:				Result.AddChild(ParseTypeSection);
		ptThreadVar:		Result.AddChild(ParseVarSection);
		ptVar:				Result.AddChild(ParseVarSection);
		ptSemicolon:		Result.AddChild(EatToken);
		else
			begin
				// Everything else is treated as executable script content.
				stmt := ParseStatement;
				if stmt.HasChildren then
					Result.AddChild(stmt)
				else
				begin
					stmt.Free;
					if CurrentTokenKind in DECL_START_TOKENS then
						Continue;
					Result.AddChild(SynErrorFmt(SE2029, ['Top-level declaration or statement', CurrentToken.Text]));
				end;
			end;
		end;
	end;
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

ntIdentifier anName="firstName"
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

function TDelphiParser.ParseExtendedIdent: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#ExtendedIdent

ExtendedIdent                              Backlinks: Atom
	-> Ident
	-> <semikeyword>

For now, delegates to ParseIdent. Semikeywords are already identifiers
at the token level (Kind = ptIdentifier), so ParseIdent handles them.
}
	Result := ParseIdent;
end;

function TDelphiParser.IsPossibleIdent: Boolean;
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
	else if (CurrentTokenKind = ptOpenBracket) and (PeekTokenExID = ptAssembly) then
		Result := True
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

function TDelphiParser.ParseInheritedVariableReference: TSyntaxNode2;
begin
{
	Handles:
		inherited;                    -> bare inherited (ntInherited with token only)
		inherited Create;             -> ntInherited > ntParticle > ntIdentifier("Create")
		inherited Create(args);       -> ntInherited > ntParticle > [ntIdentifier("Create"), args...]
		inherited Assign(Source);     -> same pattern
}
	Result := TSyntaxNode2.Create(ntInherited);
	Result.AddChild(EatToken(ptInherited));
	if IsPossibleIdent then
		Result.AddChild(ParseAtom);
end;

function TDelphiParser.ParseAttributeList: TSyntaxNode2;
begin
{
	[
}
	Result := ParseAttribute;

	while CurrentToken.Kind = ptComma do
	begin
		Result.AddChild(EatToken(ptComma));
		Result.AddChild(ParseAttributeList);
	end;
end;

function TDelphiParser.ParseAttribute: TSyntaxNode2;
begin
{
Attribute
   -> AttributeName

Sample Code
-----------

[]
}
	Result := TSyntaxNode2.Create(ntAttribute);

	Result.AddChild(ParseAttributeName);
	if CurrentToken.Kind = ptOpenParen then
		Result.AddChild(ParseAttributeArguments);
end;

function TDelphiParser.ParseAttributeName: TSyntaxNode2;
begin
	Result := TSyntaxNode2.Create(ntName);
	Result[anName] := CurrentToken.ValueText;

	case CurrentToken.Kind of
	ptIn, ptOut, ptConst, ptVar, ptUnsafe: Result.AddChild(EatToken);
	else
		begin
			Result.AddChild(EatToken(ptIdentifier));
			while CurrentToken.Kind = ptDot do
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

		if CurrentToken.Kind = ptEquals then
			Result.AddChild(ParseNamedArgumentList)
	end;
	Result.AddChild(EatToken(ptCloseParen));
end;

function TDelphiParser.ParsePositionalArgumentList: TSyntaxNode2;
begin
	Result := ParsePositionalArgument;
	while CurrentToken.Kind = ptComma do
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
	// When out of range, return the last token in FTokens (the real EOF emitted
	// by the tokenizer).  This avoids creating an orphan TSyntaxToken.Eof that
	// nobody owns and that would leak or, worse, get freed twice.
	if (FTokens = nil) or (FTokens.Count = 0) then
		Result := TSyntaxToken.Create(ptEOF, 0, 0, '')  // degenerate case: no tokens at all
	else if FCurrent < 0 then
		Result := FTokens[FTokens.Count - 1]
	else if FCurrent >= FTokens.Count then
		Result := FTokens[FTokens.Count-1]
	else
		Result := FTokens[FCurrent];
end;

function TDelphiParser.ParseAssemblyAttribute: TSyntaxNode2;
begin
{
http://dgrok.excastle.com/Grammar.html#AssemblyAttribute

AssemblyAttribute
	-> '[' ASSEMBLY ':' Expression ']'

Returns:
	ntAttribute
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
	FChildNodes := TObjectList<TSyntaxNodeOrToken>.Create(True); //owns the child objects
end;

destructor TSyntaxNode2.Destroy;
begin
	FNodeType := ntUnknown;
	FreeAndNil(FAttributes);
	FreeAndNil(FChildNodes);
	FWidth := 0;
	FFullWidth := 0;
	FFilename := '';

	inherited;
end;

class function TSyntaxNode2.DumpPascal(Node: TSyntaxNode2; ShowParserTags: Boolean=False): string;

	function DumpToken(const Token: TSyntaxToken): string;
	var
		i: Integer;
		trivia: TSyntaxToken;
	begin
		Result := '';

		// Add leading trivia
		for i := 0 to token.LeadingTriviaCount-1 do
		begin
			trivia := token.LeadingTrivia[i];
			Result := Result + trivia.Text;
		end;

		// Add token
		Result := Result + token.Text;

		// Add trailing trivia
		for i := 0 to token.TrailingTriviaCount-1 do
		begin
			trivia := token.TrailingTrivia[i];
			Result := Result + trivia.Text;
		end;
	end;

	function DumpTrivia(const Current: TSyntaxNode2): string;
	var
		child: TSyntaxNodeOrToken;
	begin
		Result := '';

		if ShowParserTags then
			Result := Result+#13#10+'{['+Current.DisplayName+']}';

		// it's a node; iterate the children recursively
		for child in Current.ChildNodes do
		begin
			if child.IsToken then
				Result := Result+DumpToken(child.AsToken)
			else
				Result := Result+DumpTrivia(child.AsNode);
		end;
	end;
begin
	// We're dumping the trivia, which are the actual artifacts of the source code text.
	Result := DumpTrivia(Node);
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
		middlePrefix: string = '	'; //#$251C#$2500' '; // '|- '
		finalPrefix:  string = '	'; //#$2570#$2500' '; // '\- '
		nestedPrefix: string = '	'; //#$2502'  ';      // '|  '
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
					sChild := DumpNode(child.AsNode, prefix+nestedPrefix)
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

function TSyntaxNode2.get_IsMissing: Boolean;
begin
	Result := (Self.Attributes[anMissing] <> '');
end;

function TSyntaxNode2.get_Value: string;
begin
	Result := Self.Attributes[anName];
end;

function TSyntaxNode2.get_DisplayName: string;
var
	attributes: string;

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
	Result := SyntaxNodeTypeToStr(Self.NodeType);

	attributes := SyntaxNodeAttributesToStr(Self);
	if attributes <> '' then
		Result := Result+' '+attributes;
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
//	FreeAndNil(FObject); now held in the areana

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


{ TSyntaxTree }

constructor TSyntaxTree.Create(const ARoot: TSyntaxNode2);
begin
	inherited Create;

	FRoot := ARoot;
	FTokenArena := TObjectList.Create(True); //
	FNodeArena := TObjectList.Create(True);
end;

destructor TSyntaxTree.Destroy;
begin
	FRoot := nil; // root is owned by FNodeArena; do not free separately
	FreeAndNil(FNodeArena);
	FreeAndNil(FTokenArena);

	inherited;
end;

end.
