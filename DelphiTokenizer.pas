unit DelphiTokenizer;

{
The Tokenizer accepts a series of characters and emits an series of TSyntaxToken objects.

For example, the code fragment:

    var i: Integer := Items.Count;

Returns the tokens:

	| TptTokenKind			| Value					|
	|--------------------|--------------------|
	| ptVar					| 'var'					|
	| ptWhitespace			| ' ' (space)			|
	| ptIdentifier			| 'i'						|
	| ptColon				| ':'						|
	| ptWhitespace			| ' ' (space)			|
	| ptIdentifier			| 'Integer'				|
	| ptWhitespace			| ' ' (space)			|
	| ptAssign				| ':='					|
	| ptWhitespace			| ' ' (space)			|
	| ptIdentifier			| 'Items'				|
	| ptPoint				|  '.'					|
	| ptIdentifier			| 'Count'				|
	| ptSemicolon			| ';'						|

There are 4 categories of tokens:

	- Reserved words									(e.g., begin, end, case, for, if)
	- Directives										(e.g., absolute, abstract, cdecl, ...)
	- Special (single character  ) symbols		(e.g., @ $ & ' ( ) * + , - . / : ; < = > [ ] ^ { )
	- Special (multiple character) symbols		(e.g., (* *) (.  .) .. // := <= >= <>)

Sample Usage
============

var
	szSourceCode: string := 'var i: Integer := Items.Count;';
	tokenList: TObjectList := TObjectList.Create(True);

	TDelphiTokenizer.Tokenize(szSourceCode, tokenList);

	for token: TSyntaxToken in tokenList do
	begin
		var text: string := token.ToString;

Format('Token: %s (%d, %d) "%s"%s', [
				TokenKindToStr(token.TokenKind), 	// ptKeyword
				token.Line, token.Column,				// 39,29
				token.Text,									// var
				statusInfo]);
	end;


	https://dotnetfiddle.net/Sh2ism


Keywords
========

In Delphi, certain magic words are referred to as "keywords".

Keywords are then broken down into two categories:

- reserved words	(e.g. begin, end, case, for, if)
- directives 		(e.g. public, strict, safecall)



	Keyword					Example		Token.TokenKind	Token.DirectiveID   Token.GenID
	===================	==========	================	================= 	============
	(reserved word)		'of'			ptOf					ptUnknown (n/a)		ptOf
	(directive)				'out'			ptIdentifier		ptOut						ptOut
	(identifier)			'Contoso'	ptIdentifier		ptUnknown				ptIdentifier

	Token.DirectiveID is only valid if TokenKind is ptIdentifier.
	Token.GenID is a helper function that will coalesce (DirectiveID|TokenKind)


1. Reserved words (e.g., begin, end, case, for, if, of)

	Reserved words are keywords that have a special meaning in the language and cannot be used as identifiers, e.g.:

		begin	==> ptBegin
		end   ==> ptEnd
		case  ==> ptCase
		for   ==> ptFor
		if		==> ptIf

	See the ReservedWords[] constant array for the list of reserved words.
	Can call TDelphiTokenizer.GetReservedTokenKind('of') that will return ptOf, or ptUnknown if it isn't a reserved word

2. Directives (e.g., absolute, abstract, cdecl, out, ...)

	Complier directives are similar to reserved words, except they can be reused as identifiers.
	If it is being used as an identifier, it will instead have a TokenKind of ptIdentifier.

	For example:

		procedure DoIt(out s: string);	// "out" is a directive   (ptOut)
		var out: Boolean;						// "out" is an identifier (ptIdentifier)

	This means that the meaning of the keyword "out" depends on its context:

		- if "out" represents a directive, it will have TokenKind ptOut
		- otherwise it will have a TokenKind of ptIdentifier

	This context is determined by the parser, and it can change based on how the keyword is used in the code.

	The tokenizer returns these directive tokens as ptIdentifier,
	and the parser sets TSyntaxToken.GenID to a particular ptXxxx directive

	See the Directives[] constant array of the list of directives.
	Call TDelphiParser.GetDirectiveTokenKind('out') to return ptOut if it is a directive, or ptUnkonwn if it isn't.


3. Identifiers (e.g., FirstName, SaveToDatabase, TCustomer)

	Identifiers are names given to entities in the code, such as variables, functions, and classes.
	They are user-defined and can be any valid name that follows the naming rules of the language.

	Examples:

		MyVar: Integer;
		AnotherVar: string;

	Identifiers are not reserved or restricted in any way, and they can be used freely throughout the code.


Tokenization Examples
=====================

	class procedure Tokenize(const SourceCode: string): TArray<TSyntaxToken>;

would return the tokens:

- ptClass			(Text: 'class')		reserved word
- ptProcedure		(Text: 'procedure')	reserved word
- ptOpenParen		(Text: '(')				special single character symbol
- ptConst			(Text: 'const')		reserved word
- ptIdentifier		(Text: 'SourceCode')
- ptColon			(Text: ':')				special single character symbol
- ptIdentifier		(Text: 'string')
- ptCloseParen		(Text: ')')				special single character symbol
- ptColon			(Text: ':')				special single character symbol
- ptIdentifier		(Text: 'TArray')
- ptLessThan		(Text: '<')				special single character symbol
- ptIdentifier		(Text: 'TSyntaxToken')
- ptGreaterThan	(Text: '<')				special single character symbol
- ptSemicolon		(Text: ';')				special single character symbol


Issues
=======

1. **Token Position Tracking** (Lines 678-679)
   - Properties `StartOffset` and `TokenLength` are declared but NEVER populated
   - No assignments found anywhere in codebase (`.StartOffset :=` or `.TokenLength :=`)
   - **Action needed**: Populate these in `TSyntaxToken.Create()` or token creation methods

2. **Compiler Directive Parsing** (Lines 652-659, 1670-1699)
   - `TDirectiveData` structure defined but unused
   - `DoCompilerDirective()` has TODO comment about parsing (line 1699)
   - **Action needed**: Implement directive parsing to populate `TDirectiveData`

3. **Keyword Lookup Performance** (Lines 1267-1273)
   - Linear O(n) search through `ReservedWords` array
   - No hash table or `TDictionary` implementation
   - **Action needed**: Implement `TDictionary<string, TptTokenKind>` for O(1) lookup

4. **Maximum Line Length** A line cannot be longer than 2260 characters.
      Error: F2069 Line too long (more than 1023 characters)
      Delphi 12.0 says 1023, but in reality it's 2260 characters.

}


interface

uses
	Windows, SysUtils, Classes, ActiveX, ComObj, Variants, StrUtils, Math,
	System.Generics.Collections,
	{$IFDEF UnitTests}DelphiTokenizerTests,{$ENDIF}
	Avatar.Exceptions;

const
	CP_UTF16	= 1200;
	UEOF		= Char($FFFF);

{
	The TSyntaxToken.TokenKind.
}
type
	TptTokenKind = (
		ptUnknown,					// Dummy sentinel value
		ptEof,						// This is the last token from the tokenizer
//		ptError,						// Error token

		// *** Reserved words (e.g., begin, end, case, for, if)
		// Reserved words are keywords that have a special meaning in the language and cannot be used as identifiers.
		// See the ReservedWords[] array for the list of reserved words
		ptAbort,
		ptAnd,
		ptArray,
		ptAs,
		ptAsm,
		ptBegin,
		ptCase,
		ptClass,						// class
		ptConst,
		ptConstructor,
		ptDestructor,
		ptDispInterface,
		ptDiv,
		ptDo,
		ptDownTo,
		ptElse,
		ptEnd,
		ptExcept,
		ptExports,
		ptFile,
		ptFinalization,
		ptFinally,
		ptFor,
		ptFunction,
		ptGoto,
		ptIf,
		ptImplementation,
		ptIn,
		ptInherited,
		ptInitialization,
		ptInterface,
		ptIs,
		ptLabel,
		ptLibrary,						// library file type, and library portability directive
		ptMod,
		ptNil,
		ptNot,							// if not Enabled
		ptObject,
		ptOf,
		ptOr,
		ptPacked,
		ptProcedure,
		ptProgram,
		ptProperty,						// [RESERVED WORD]
		ptRaise,
		ptRecord,
		ptRepeat,
		ptResourceString,
		ptSet,
		ptShl,
		ptShr,
		ptString,						// the literal "string"
		ptThen,
		ptThreadVar,
		ptTo,								// reserved word
		ptTry,
		ptType,
		ptUnit,
		ptUntil,
		ptUses,
		ptVar,
		ptWhile,
		ptWith,
		ptXor,

		// *** Directives (e.g., absolute, abstract, cdecl, ...) ***
		// Complier directives are similar to reserved words, except they can be reused as identifiers.
		// See Directives[] constant array for the list of directives
		ptAt,							// raise Exception.Create('Hello') at @SaveChanges; // https://stackoverflow.com/a/8951057/12597
		ptAbsolute,

		// directive tokens that apply to methods
		ptAbstract,
		ptCDecl,
		ptDynamic,
		ptMessage,
		ptOverride,
		ptOverload,
		ptPascal,
		ptRegister,
		ptReintroduce,
		ptSafecall,
		ptStdcall,
		ptVirtual,
		ptStatic,
		ptInline,
		ptFinal,
		ptDispid,


		ptAlign,						// TMyAlignedRecord = record Field1: Byte; Field2: Integer; end align 8;
		ptAssembler,
		ptAssembly,
		ptAutomated,
		ptContains,
		ptDefault,
		ptDelayed,
		ptExport,
		ptExternal,
		ptFar,
		ptForward,
		ptHelper,
		ptImplements,
		ptIndex,
		ptLocal,
		ptNear,
		ptNodefault,
		ptOperator,
		ptOut,
		ptPackage,

		// Hinting directives
		ptPlatform,
		ptDeprecated,
		//ptLibrary; but that's also the keyword that starts a library unit.
		ptExperimental,

		ptPrivate,
		ptProtected,
		ptPublic,
		ptPublished,
		ptRead,
		ptReadonly,
		ptReference,
		ptRequires,
		ptResident,
		ptSealed,
		ptStored,
		ptStrict,
		ptUnsafe,
		ptVarargs,
		ptWrite,
		ptWriteonly,

		// *** Special (single character) symbols ***
		// https://docwiki.embarcadero.com/RADStudio/Sydney/en/Fundamental_Syntactic_Elements_(Delphi)#Special_Symbols
		// # $ & ' ( ) * + , - . / : ; < = > @ [ ] ^ { }
		ptAmpersand,					// '&' symbol
		ptColon,							// ':' symbol
		ptSemicolon,					// ';' symbol
		ptOpenParen,					// '(' symbol
		ptCloseParen,					// ')' symbol
		ptOpenBracket,					// '[' symbol
		ptCloseBracket,				// ']' symbol
		ptOpenBrace,					// '{' symbol
		ptCloseBrace,					// '}' symbol
		ptPlus,							// '+' symbol
		ptMinus,							// '-' symbol
		ptComma,							// ',' symbol
		ptSlash,							// '/' symbol
		ptAsterisk,						// '*' symbol
		ptEquals,						// '=' symbol
		ptGreaterThan,					// '>' symbol
		ptLessThan,						// '<' symbol
		ptDot,							// '.' symbol
		ptCaret,							// '^' symbol (pointer dereference)

		// *** Special (multiple character) symbols ***
		// https://docwiki.embarcadero.com/RADStudio/Sydney/en/Fundamental_Syntactic_Elements_(Delphi)#Special_Symbols
		// (* (. *) .) .. // := <= >= <>
		ptDotDot,						// '..' range operator
		ptAssign,						// ':=' assignment operator
		ptGreaterThanEquals,			// '>=' operator
		ptNotEqual,						// '<>' operator
		ptLessThanEquals,				// '<=' operator
		ptDoubleAddressOp,			// '@@' double address operator
		ptAddressOp,					// '^' address operator (duplicate)
		ptRoundDotOpen,				// '(.' symbol  (legacy versoin of [)
		ptRoundDotClose,				// '.)' symbol  (legacy version of ])

		// *** Constant values ***
		ptStringLiteral,				// String literal, that thing is 'wrapped in single quotes'
		ptMultilineStringLiteral,	// Multi-line string literal, that thing that is wraped in triple quotes '''...'''
//		ptStringDQLiteral,			// Double-quoted string constant
		ptAsciiChar,					// ASCII character constant e.g. #149, #$4E
		ptPointerSymbol,				// '^' symbol (e.g. ^M)
		ptFloat,							// Floating-point constant	(e.g. 1234567890.987654321)
		ptIntegerConst,				// Integer constant			(e.g. 123456789087654321)

		ptSymbol,						//  !  ?  \  `  ~
		ptOn,								// On is weird. It can be redefined, but does not have a declaration in system.pas, but it bolds in the IDE like it's a reserved word. It's for try..except on...


		ptCompilerDirective,			// Compiler directive
//		ptDefineDirect,				// '{$DEFINE xxx}'	compiler directive
//		ptUndefDirect,					// '{$UNDEF}'			compiler directive
//		ptIfDefDirect,					// '{$IFDEF xxx}'		compiler directive
//		ptElseDirect,					// '{$ELSE}'			compiler directive
//		ptEndIfDirect,					// '{$ENDIF}'			compiler directive
//		ptIfDirect,						// '{$IF xxx}'			compiler directive
//		ptElseIfDirect,				// '{$ELSEIF}'			compiler directive
//		ptIfEndDirect,					// '{$IFEND}'			compiler directive
//		ptIfNDefDirect,				// '{$IFNDEF}'			compiler directive
//		ptIfOptDirect,					// '{$IFOPT xxx}'		compiler directive
//		ptIncludeDirect,				// '{$INCLUDE xxx}'	compiler directive
//		ptResourceDirect,				// '{$RESOURCE xxx}'	compiler directive
//		ptScopedEnumsDirect,			// '{$SCOPEDENUMS xxx}' compiler directive

		// Will be the GenID of a string token (I am guessing at that, that seems reasonable to me)
		ptName,						// The name of an external function. e.g. procedure Foo; external 'glib.dll' name 'WabashTheMagicDragon';

		// *** Identifiers ***
		ptIdentifier,				// User-defined identifier

		// *** Trivia - Whitespace ***
		ptSpace,						// whitespace (#1..#32; excl. CR and LF)
		ptWhitespace,				// space, cr, lf, tab, etc
		ptCRLF,						// Carriage return and line feed

		// *** Trivia - Comments ***
		ptAnsiComment,				// (*  *)
		ptBorComment,				// {    } block comment
		ptSlashesComment,			// //     single-line comment
		ptCRLFCo,					// CRLF   inside a comment

{
		Predefined constants, types, procedures, and functions (e.g. True, Integer, or WriteLn)
		These do not have actual declarations.
		Instead they are built into the compiler, and are treated as if they were declared
		at the beginning of the System unit.
		These are not keywords (either reserved words or directives) and can be redefined.
		I suggest we never use these in the tokenizer, since they are context sensitive.

		E.g. the following is valid:

			procedure TDelphiTokenizer.SelfTest;
			type
				Boolean = Real;
			var
				AnsiString: Boolean;
			begin
				AnsiString := 3.14;
			end;

		That is why i suggest recognizing identifiers into types be done by the parser
		rather than the tokenizer. The tokenzier is not equipped to resolve these
		identifiers into built-in types, redefined types, or variable names.

			> But if it says `Continue` don't i always know it means continue a loop?

		Nope, Continue is a built-in function, that can be redefined in a closer scope.

		But let us compile a list of built-in things hidden in System.pas,
		especially for the set of things we will want (e.g. flow control Continue, Break)

		The parser can eventually recognize these built-in identifiers.
}

		// ## Delphi Intrinsic Routines
		// https://docwiki.embarcadero.com/RADStudio/Athens/en/Delphi_Intrinsic_Routines
		ptBreak,						// procedure System.Break
		ptContinue,					// procedure System.Continue
		ptExit,						// procedure System.Exit
		ptHalt,						// procedure System.Halt(ExitCode: Integer)

{
		I have to imagine these other built-in routines would be useful for a linter.
		They deal with program correctness, type safety, overflow, range checking, memory allocations, etc.
		If people didn't know any better they would have sworn up and down that they're
		all keywords anyway; rather than built-in.
		But the original parser didn't include them.
}
		ptAddr,						// function  System.Addr(var X): Pointer
		ptAssert,  					// procedure System.Assert(Condition: Boolean)
		ptAssigned,					// function  System.Assigned(var P): Boolean

		ptInc,						// procedure System.Inc(var X: Integer; N: Integer)
		ptDec,						// procedure System.Dec(var X: Integer; N: Integer)

		ptCopy,						// function  System.Copy(S: String; Index: Integer; Count: Integer): string
		ptInsert,					// procedure System.Insert(Substr: String; var Dest: String; Index: Integer)
		ptDelete,					// procedure System.Delete(var S: String; Index: Integer; Count: Integer)
		ptFillChar,					// procedure System.FillChar(var X; Count: NativeInt; Value: Integer);
		ptLength,					// function  System.Length(S: String): Integer
		ptGetMem,					// procedure System.GetMem(var P: Pointer; Size: NativeInt)
		ptFreeMem,					// procedure System.FreeMem(var P: Pointer)

		ptLo,							// function  System.Lo(X: Integer): Byte
		ptHi,							// function  System.Hi(X: Integer): Byte

		ptLow,						// function  System.Low(var X): Integer
		ptHigh,						// function  System.High(var X): Integer
		ptSizeOf,					// function  System.SizeOf(var X): Integer;

										// and many, many, MANY, more

		// ### Simple Types
		// https://docwiki.embarcadero.com/RADStudio/Athens/en/Simple_Types_(Delphi)
		// Simple types - which include ordinal types and real types - define ordered sets of values.

		// #### Platform-independant integer types
		ptShortInt,					// type System.ShortInt			= -128..127 (Int8)
		ptSmallint,					// type System.SmallInt			= -32768..32767 (Int16)
		ptInteger,					// type System.Integer			= -2147483648..2147483647 (Int32)
		ptInt64,						// type System.Int64				= -9223372036854775808..9223372036854775807
		ptByte,						// type System.Byte				= 0..255 (UInt8)
		ptWord,						// type System.Word				= 0..65535 (UInt16)
		ptCardinal,					// type System.Cardinal			= 0..4294967295 (UInt32)
		ptUInt64,					// type System.UInt64			= 0..18446744073709551615
		ptFixedInt,					// type System.FixedInt			= LongInt  (Windows), Integer  (iOS, Android)
		ptFixedUInt,				// type System.FixedUInt		= LongWord (Windows), Cardinal (iOS, Android)

		// #### Platform-dependant integer types
		ptLongInt,					// type System.LongInt			= Integer  (32-bit platforms, and 64-bit Windows), Int64  (64-bit POSIX)
		ptLongword,					// type System.LongWord			= Cardinal (32-bit platforms, and 64-bit Windows), UInt64 (64-bit POSIX)
		ptNativeInt,				// type System.NativeInt		= Integer  (32-bit platforms),                     Int64  (64-bit platforms)
		ptNativeUInt,				// type System.NativeUInt		= Cardinal (32-bit platforms),                     UInt64 (64-bit platforms)

		// #### Character Types
		// NOTE: "string" is a keyword; that's why it's not here.
		ptChar,						// type System.Char				= #0..#FFFF
		ptAnsiChar,					// type System.AnsiChar			= #0..#255;
		ptWideChar,					// type System.WideChar			= Char
//		ptUCS2Char,					// type System.UCS2Char			= WideChar							UCS2Char actually is declared in System.pas
//		ptUCS4Char,					// type System.UCS4Char			= Cardinal							UCS4Char actually is declared in System.pas

		ptUnicodeString,			// type System.UnicodeString	= string
		ptAnsiString,				// type System.AnsiString		= AnsiString
		ptWideString,				// type System.WideString		= WideString
		ptShortString,				// type System.ShortString		= String[255]

		ptUtf8String,				// type System.UTF8String		= type AnsiString(65001)		UTF8String    actually is declared in System.pas
		ptRawByteString,			// type System.RawByteString	= type AnsiString($ffff)		RawByteString actually is declared in System.pas

		ptPChar,						// type System.PChar				= PWideChar
		ptPWideChar,				// type System.PWideChar		= ^Char;
		ptPAnsiChar,				// type System.PAnsiChar		= &AnsiChar

		ptPointer,					// type System.Pointer			= Pointer
//		ptPByte,						// type System.PByte				= ^Byte			PByte is declared in System.pas, but i mention it because it is the pointer type that it allows $POINTERMATH

		ptBoolean,					// type System.Boolean			= False..True (8-bit)
		ptByteBool,					// type System.ByteBool			= False..True (8-bit)
		ptWordBool,					// type System.WordBool			= False..True (16-bit)
		ptLongBool,					// type System.LongBool			= False..True (32-bit)

		ptReal48,					// type System.Real48			= Real48
		ptSingle,					// type System.Single			= Single
		ptDouble,					// type System.Double			= Double
		ptReal,						// type System.Real				= Double
		ptExtended,					// type System.Extended			= Extended
		ptComp,						// type System.Comp				= Comp
		ptCurrency,					// type System.Currency			= Currency

		ptOleVariant,				// type System.OleVariant		= OleVariant
		ptVariant					// type System.Variant			= Variant

//		ptAdd, 						// TODO: AddAccessIdentifier
//		ptRemove,					// TODO: RemoveAccessIdentifier

//		ptRunError,					// ???
//		ptStringresource,			// ???

//		ptDWORD,						// DWORD is not a built-in type; it's Windows.DWORD

//		ptClassForward,				(not in the tokenizer)
//		ptClassFunction,				(not in the tokenizer)
//		ptClassProcedure,				(not in the tokenizer)
//		ptNone,							(not in the tokenizer)
	);

	function TokenKindToStr(const ATokenType: TptTokenKind): string;		// e.g. ptDownTo ==> "ptDownTo"
	function TokenKindIsTrivia(const ATokenKind: TptTokenKind): Boolean;	// whitespace and comments
	function TokenName(const ATokenKind: TptTokenKind): string;				// same as TokenKindToStr(ATokenKind)
	function IsReservedWord(const ATokenKind: TptTokenKind): Boolean;
	function TokensToStr(Tokens: TList): string;



const
	TriviaTokenSet: set of TptTokenKind = [
		ptSpace,
		ptWhitespace,
		ptCRLF,
		ptAnsiComment,
		ptBorComment,
		ptSlashesComment,
		ptCRLFCo
	];



type
	TInputStream = class;	// forward. Supplies a series of UtF-16 from an input ISequentialStream

(*
	Population rules in the lexer:

	- classify { $ ...} and ( *$ ...* ) as ptDirective.

	- extract Name (letters/underscores) case-insensitive; set Args := rest.trim.

	- resolve short forms (A→ALIGN, I→INCLUDE) and fill Kind. unknown → dkUnknown.

	- leave $IF expression text in Args verbatim; don’t evaluate in the lexer.
*)
	TDirectiveKind = (
			dkUnknown, dkDefine, dkUndef, dkIf, dkIfDef, dkIfNDef, dkElse, dkElseIf, dkEndIf, dkIfEnd,
			dkInclude, dkAlign, dkIfOpt, dkResource, dkScopedEnums, dkWarn, dkHints, dkRegion, dkEndRegion);


	TDirectiveData = record
		Raw: string;            // same as Text (kept for convenience)
		Name: string;           // first word after '$' (normalized upper)
		Args: string;           // trimmed remainder after Name (verbatim)
		Kind: TDirectiveKind;   // convenience enum resolved by the lexer
		ShortForm: Boolean;     // e.g. $A for $ALIGN, $I for $INCLUDE
	end;
	PDirectiveData = ^TDirectiveData;

	TDirectiveDelimiter = (ddBrace, ddParenStar);

{
	Represents an individual token returned by the tokenizer's GetNext
}
	TSyntaxToken = class
	private
		FLeadingTrivia: TList<TSyntaxToken>;
		FTrailingTrivia: TList<TSyntaxToken>;
		function get_GenID: TptTokenKind;
		function get_LeadingTriviaCount: Integer;
		function get_LeadingTrivia(I: Integer): TSyntaxToken;
		function get_TrailingTriviaCount: Integer;
		function get_TrailingTrivia(I: Integer): TSyntaxToken;
	public
		TokenKind: TptTokenKind;	// base kind: identifier, string, symbol, comment, directive, etc.
		Line: Integer;
		Column: Integer;
		StartOffset: Integer;		// absolute byte/char offset in the file (0-based)
		TokenLength: Integer;		// token length in source units
		Text: string;					// exact slice, e.g. `'Waiting for server...'` or `{$IFDEF DEBUG}`
		ValueText: string;			// unwrapped/unescaped where purely lexical (strings, comments)

		IsMissing: Boolean;			// synthesized if parser expected it but lexer didn't provide it

		// Error tracking for robust parsing and linting
		HasErrors: Boolean;			// true if this token has parsing errors
		HasWarnings: Boolean;		// true if this token has parsing warnings
		ErrorMessage: string;		// primary error message
		WarningMessage: string;		// primary warning message

{

		Reserved words cannot be redefined or used as an identifier.

The following reserved words cannot be redefined or used as identifiers.

		Complier directives are similar to reserved words, except they can be reused as identifiers.
		Hence -- although it is inadvisable to do so -- you can define an identifier that looks exactly like a directive.

		In order to support both types (identifier mode vs directive mode) in one token, we have two properties:

			- TokenKind:   The kind of TSyntaxToken, and will be ptIdentifier for possible identifiers (e.g. "Self")
			- DirectiveID: When the identifier is also a Compiler Directive, contains the token type   (e.g. ptSelf)

		And then you can just read the ExtendedID property, which will prefer ptSelf --> ptIdentifier
}
		DirectiveID: TptTokenKind;			// DirectiveID. See constant Directives[]
		DirectiveDelimeter: TDirectiveDelimiter; // (ddBrace, ddParenStar)

		// optional directive payload (only for TokenKind = ptDirective)
//		Directive: PDirectiveData; // nil unless a directive

		constructor Create(ATokenKind: TptTokenKind; const Line, Column: Integer; const Text: string);
		destructor Destroy; override;

		property LeadingTriviaCount: Integer read get_LeadingTriviaCount;
		property LeadingTrivia[n: Integer]: TSyntaxToken read get_LeadingTrivia;
		property TrailingTriviaCount: Integer read get_TrailingTriviaCount;
		property TrailingTrivia[n: Integer]: TSyntaxToken read get_TrailingTrivia;

		/// <summary>GenID if set; otherwise TokenKind if GenID is empty</summary>
		// Prefers compiler directive over identifier (e.g. ptSelf --> ptIdentifier)
		property GenID: TptTokenKind read get_GenID;

		function ToString: string; override;

		class function Eof: TSyntaxToken; //singleton
	end;

	TDelphiTokenizer = class
	private
		FStream: TInputStream;

		FCurrentInputCharacter: WideChar; // updated with Consume
		FCurrentLine: Integer;		// Starting at 1
		FCurrentColumn: Integer;	// Starting at 1

		FNextToken: TSyntaxToken;
		FEofEmitted: Boolean;     // has EOF been emitted once by GetNextToken?
		FPeeking: Boolean;        // true while PeekTokenKind is prefetching a token

		//
		FCompilerDirectives: TStrings;

		function GetNextChar: WideChar;	// Get the next character from the stream. UEOF if at end of stream.
		function Consume: WideChar;		// Consume the next character from the stream.

		// is this character allowed as an identifier
		function IsStartOfIdentifierCharacter(const ch: WideChar): Boolean;
		function IsIdentifierCharacter(const ch: WideChar): Boolean;

		function IsStartOfNumeralCharacter(	const ch: WideChar): Boolean;
		function IsHexCharacter(				const ch: WideChar): Boolean;
		function IsBinaryCharacter(			const ch: WideChar): Boolean;

		function IsWhitespaceCharacter(const ch: WideChar): Boolean;

		// Detect the special keywords (e.g. reserved words, directives)
		function GetReservedTokenKind(  const Keyword: string): TptTokenKind; // GetReservedTokenKind
		function GetDirectiveTokenKind(const Keyword: string): TptTokenKind;  // GetDirectiveTokenKind

		// String processing utilities
		function ProcessStringEscapes(const rawString: string): string;
		function IsValidSurrogatePair(highSurrogate, lowSurrogate: WideChar): Boolean;
	private
		function DoDoubleAddressOp(			const ch: WideChar): TSyntaxToken;	// ptDoubleAddressOp			@@
		function DoAddressOp(					const ch: WideChar): TSyntaxToken;	// ptAddressOp					@
		function DoDotDot(						const ch: WideChar): TSyntaxToken;	// ptDotDot						..
		function DoPoint(							const ch: WideChar): TSyntaxToken;	// ptDot							.
		function DoComma(							const ch: WideChar): TSyntaxToken;	// ptComma						,
		function DoAssign(						const ch: WideChar): TSyntaxToken;	// ptAssign						:=
		function DoColon(							const ch: WideChar): TSyntaxToken;	// ptColon						:
		function DoSemicolon(					const ch: WideChar): TSyntaxToken;	// ptSemicolon					;
		function DoOpenParen(					const ch: WideChar): TSyntaxToken;	// ptOpenParen					(
		function DoRoundDotOpen(				const ch: WideChar): TSyntaxToken;	// ptRoundOpenDot				(.
		function DoRoundDotClose(				const ch: WideChar): TSyntaxToken;	// ptRoundDotClose			.)
		function DoCloseParen(					const ch: WideChar): TSyntaxToken;	// ptCloseParen				)
		function DoOpenBracket(					const ch: WideChar): TSyntaxToken;	// ptOpenBracket				[
		function DoCloseBracket(				const ch: WideChar): TSyntaxToken;	// ptCloseBracket				]
		function DoCaret(							const ch: WideChar): TSyntaxToken;	// ptCaret						^
		function DoAsterisk(						const ch: WideChar): TSyntaxToken;	// ptAsterisk						*
		function DoAmpersand(					const ch: WideChar): TSyntaxToken;	// ptIdentifier				&begin: Integer;
		function DoPlus(							const ch: WideChar): TSyntaxToken;	// ptPlus						+
		function DoMinus(							const ch: WideChar): TSyntaxToken;	// ptMinus						-
		function DoSlash(							const ch: WideChar): TSyntaxToken;	// ptSlash						/
		function DoLessThanEquals(				const ch: WideChar): TSyntaxToken;	// ptLessThanEquals			<=
		function DoNotEqual(						const ch: WideChar): TSyntaxToken;	// ptNotEqual					<>
		function DoLessThan(						const ch: WideChar): TSyntaxToken;	// ptLessThan					<
		function DoGreaterThanEquals(			const ch: WideChar): TSyntaxToken;	// ptGreaterThanEquals		>=
		function DoGreater(						const ch: WideChar): TSyntaxToken;	// ptGreaterThan				>
		function DoEqual(							const ch: WideChar): TSyntaxToken;	// ptEquals						=

		function DoStringLiteral(				const ch: WideChar): TSyntaxToken;	// ptStringLiteral			'...'
		function DoMultilineStringLiteral(	const ch: WideChar): TSyntaxToken;	// ptMultilineStringLiteral		'''...'''
		function DoAsciiChar(					const ch: WideChar): TSyntaxToken;	// ptAsciiChar					#149, #$3E

		function DoAnsiComment(					const ch: WideChar): TSyntaxToken;	// ptAnsiComment				(*  *)
		function DoBorComment(					const ch: WideChar): TSyntaxToken;	// ptBorComment				{    }
		function DoSlashesComment(				const ch: WideChar): TSyntaxToken;	// ptSlashesComment			//

		function DoCompilerDirective(			const ch: WideChar): TSyntaxToken;	// ptCompDirect				{$xxx xx}

		function DoReadIdentifier(				const ch: WideChar): TSyntaxToken;	// ptIdentifier				var, begin, firstname
		function DoWhitespace(					const ch: WideChar): TSyntaxToken;  // ptWhitespace

		function DoNumber(						const ch: WideChar): TSyntaxToken;	// ptIntegerConst/ptFloat	0..9
		function DoHexNumber(					const ch: WideChar): TSyntaxToken; 	// ptIntegerConst				$BAADF00D
		function DoBinaryNumber(				const ch: WideChar): TSyntaxToken;	// ptIntegerConst				%1011000110

		procedure Log(const s: string);
		procedure LogFmt(const s: string; const Args: array of const);
		function GetPosXY: TPoint;
//		procedure GetDefaultConditionalDirectives(TargetList: TStrings);
	protected
		function Peek(NumberOfCharactersAhead: Integer=1): WideChar;			// Peek at the next character from the stream.
		function PeekNext: WideChar;		// Peek at the character after the next character from the stream.

		// Peek at the next token, returning it on the next call to NextToken
		function GetNextToken(out AToken: TSyntaxToken): Boolean;

		procedure SelfTest;
	public
		constructor Create(SourceStream: ISequentialStream; Encoding: Word); overload;
		constructor Create(const SourceCode: UnicodeString); overload;
		destructor Destroy; override;

		// Cough up the next token
		function NextToken(out AToken: TSyntaxToken): Boolean;
		function PeekTokenKind: TptTokenKind;

		property CompilerDirectives: TStrings read FCompilerDirectives;

		// This is how you do it. Fills list with TSyntaxToken objects
		class procedure Tokenize(const SourceCode: string; const TargetList: TList);

		property PosXY: TPoint read GetPosXY;
	end;


	{
	TInputStream
	=================
	Reads a forward-only byte stream as UTF-16 characters with support for peeking
	and a small bounded lookahead buffer implemented on top of a UnicodeString.

	Contract / API:
	- Backing store: UnicodeString (1-based indexing).
	- Active window semantics (no wrap-around):
	  - FBufferPosition: 1-based index of the next character in the active window.
	  - FBufferSize: number of valid lookahead characters starting at FBufferPosition.
	  - When FBufferSize > 0: FBufferPosition + FBufferSize - 1 <= Length(FBuffer).

	- Methods:
	  - TryRead(out ch: WideChar): Boolean
	    Returns the next UTF-16 character. If the active window is non-empty, it
	    consumes from the buffer; otherwise it reads from the underlying stream.
	    Returns False (and sets EOF) only after UEOF is actually consumed.

	  - Peek(k: Integer): WideChar
	    k must be >= 1. Ensures the active window has at least k characters by
	    appending at index FBufferPosition + FBufferSize (linear, no wrap). If the
	    underlying stream ends during fill, returns UEOF. If appending would exceed
	    Length(FBuffer), raises an exception indicating buffer capacity exceeded.

	  - Consume: WideChar (internal)
	    Returns next buffered char, advances FBufferPosition, decreases FBufferSize.
	    When FBufferSize reaches 0, resets FBufferPosition to 1.

	- Invariants:
	  - FBufferPosition >= 1
	  - FBufferSize >= 0
	  - If FBufferSize = 0, the next buffered append will happen at index 1.
	  - No wrap-around arithmetic is used for indexing.

	- Error modes:
	  - Peek raises for k < 1.
	  - Peek raises if ensuring k characters would exceed allocated capacity.
	}
	TInputStream = class
	private
		FStream: ISequentialStream;
		FEncoding: Word;
		FEOF: Boolean;

		FBuffer: UnicodeString;
		FBufferPosition: Integer; // 1-based index of current position in the active window
		FBufferSize: Integer;     // number of valid characters in the active window (from FBufferPosition)

		// Return the next character from the source input; taking into account the encoding
		function GetNextCharacterFromStream: WideChar;

		// Read a UTF-16 encoded character from the stream
		function GetNextCharacterFromStreamUTF16: WideChar;

		// Extract the next character from our (possibly buffered) stream
		function Consume: WideChar;

		function FetchNextCharacterInfoBuffer: Boolean;
		procedure LogFmt(const s: string; const Args: array of const);
	public
		constructor Create(ByteStream: ISequentialStream; Encoding: Word=CP_UTF16);

		function TryRead(out ch: WideChar): Boolean; //Returns the next UTF-16 character value.
		function Peek(k: Integer): WideChar; //peek the k-th upcoming character

		property EOF: Boolean read FEOF;
	end;



const
	//The following keywords are "reserved" words, that cannot be redefined or used as identifiers.
	ReservedWords: array[0..63] of record
		keyword: string;
		tokenType: TptTokenKind;
	end = (
		(keyword: 'and';					tokenType: ptAnd),
		(keyword: 'array';				tokenType: ptArray),
		(keyword: 'as';					tokenType: ptAs),
		(keyword: 'asm';					tokenType: ptAsm),
		(keyword: 'begin';				tokenType: ptBegin),
		(keyword: 'case';					tokenType: ptCase),
		(keyword: 'class';				tokenType: ptClass),
		(keyword: 'const';				tokenType: ptConst),
		(keyword: 'constructor';		tokenType: ptConstructor),
		(keyword: 'destructor';			tokenType: ptDestructor),
		(keyword: 'dispinterface';		tokenType: ptDispInterface),
		(keyword: 'div';					tokenType: ptDiv),
		(keyword: 'do';					tokenType: ptDo),
		(keyword: 'downto';				tokenType: ptDownto),
		(keyword: 'else';					tokenType: ptElse),
		(keyword: 'end';					tokenType: ptEnd),
		(keyword: 'except';				tokenType: ptExcept),
		(keyword: 'exports';				tokenType: ptExports),
		(keyword: 'file';					tokenType: ptFile),
		(keyword: 'finalization';		tokenType: ptFinalization),
		(keyword: 'finally';				tokenType: ptFinally),
		(keyword: 'for';					tokenType: ptFor),
		(keyword: 'function';			tokenType: ptFunction),
		(keyword: 'goto';					tokenType: ptGoto),
		(keyword: 'if';					tokenType: ptIf),
		(keyword: 'implementation';	tokenType: ptImplementation),
		(keyword: 'in';					tokenType: ptIn),
		(keyword: 'inherited';			tokenType: ptInherited),
		(keyword: 'initialization';	tokenType: ptInitialization),
		(keyword: 'inline';				tokenType: ptInline),
		(keyword: 'interface';			tokenType: ptInterface),
		(keyword: 'is';					tokenType: ptIs),
		(keyword: 'label';				tokenType: ptLabel),
		(keyword: 'library';				tokenType: ptLibrary),  // library is also a keyword when used as the first token in project source code; it indicates a DLL target. Otherwise, it marks a symbol so that it produces a library warning when used.
		(keyword: 'mod';					tokenType: ptMod),
		(keyword: 'nil';					tokenType: ptNil),
		(keyword: 'not';					tokenType: ptNot),
		(keyword: 'object';				tokenType: ptObject),
		(keyword: 'of';					tokenType: ptOf),
		(keyword: 'or';					tokenType: ptOr),
		(keyword: 'packed';				tokenType: ptPacked),
		(keyword: 'procedure';			tokenType: ptProcedure),
		(keyword: 'program';				tokenType: ptProgram),
		(keyword: 'property';			tokenType: ptProperty),
		(keyword: 'raise';				tokenType: ptRaise),
		(keyword: 'record';				tokenType: ptRecord),
		(keyword: 'repeat';				tokenType: ptRepeat),
		(keyword: 'resourcestring';	tokenType: ptResourceString),
		(keyword: 'set';					tokenType: ptSet),
		(keyword: 'shl';					tokenType: ptShl),
		(keyword: 'shr';					tokenType: ptShr),
		(keyword: 'string';				tokenType: ptString),
		(keyword: 'then';					tokenType: ptThen),
		(keyword: 'threadvar';			tokenType: ptThreadVar),
		(keyword: 'to';					tokenType: ptTo),
		(keyword: 'try';					tokenType: ptTry),
		(keyword: 'type';					tokenType: ptType),
		(keyword: 'unit';					tokenType: ptUnit),
		(keyword: 'until';				tokenType: ptUntil),
		(keyword: 'uses';					tokenType: ptUses),
		(keyword: 'var';					tokenType: ptVar),
		(keyword: 'while';				tokenType: ptWhile),
		(keyword: 'with';					tokenType: ptWith),
		(keyword: 'xor';					tokenType: ptXor)
	);


	// Complier directives are similar to reserved words, except they can be reused as identifiers.
	// Hence -- although it is inadvisable to do so -- you can define an identifier that looks exactly like a directive.
	Directives: array[0..55] of record
		directive: string;
		tokenType: TptTokenKind;
	end = (
		(directive: 'at';					tokenType:ptAt),
		(directive: 'absolute';			tokenType:ptAbsolute),
		(directive: 'abstract';			tokenType:ptAbstract),
		(directive: 'align';				tokenType:ptAlign),
		(directive: 'assembler';		tokenType:ptAssembler),
		(directive: 'assembly';			tokenType:ptAssembly),
		(directive: 'automated';		tokenType:ptAutomated),
		(directive: 'CDecl';				tokenType:ptCDecl),
		(directive: 'contains';			tokenType:ptContains),
		(directive: 'default';			tokenType:ptDefault),
		(directive: 'delayed';			tokenType:ptDelayed),
		(directive: 'dispid';			tokenType:ptDispid),
		(directive: 'dynamic';			tokenType:ptDynamic),
		(directive: 'export';			tokenType:ptExport),
		(directive: 'external';			tokenType:ptExternal),
		(directive: 'far';				tokenType:ptFar),
		(directive: 'final';				tokenType:ptFinal),
		(directive: 'forward';			tokenType:ptForward),
		(directive: 'helper';			tokenType:ptHelper),
		(directive: 'implements';		tokenType:ptImplements),
		(directive: 'index';				tokenType:ptIndex),
		(directive: 'local';				tokenType:ptLocal),
		(directive: 'message';			tokenType:ptMessage),
		(directive: 'near';				tokenType:ptNear),
		(directive: 'nodefault';		tokenType:ptNodefault),
		(directive: 'operator';			tokenType:ptOperator),
		(directive: 'out';				tokenType:ptOut),
		(directive: 'overload';			tokenType:ptOverload),
		(directive: 'override';			tokenType:ptOverride),
		(directive: 'package';			tokenType:ptPackage),
		(directive: 'pascal';			tokenType:ptPascal),

		(directive: 'platform';			tokenType:ptPlatform),
		(directive: 'deprecated';		tokenType:ptDeprecated),
		(directive: 'experimental';	tokenType:ptExperimental),

		(directive: 'private';			tokenType:ptPrivate),
		(directive: 'protected';		tokenType:ptProtected),
		(directive: 'public';			tokenType:ptPublic),
		(directive: 'published';		tokenType:ptPublished),
		(directive: 'read';				tokenType:ptRead),
		(directive: 'readonly';			tokenType:ptReadonly),
		(directive: 'reference';		tokenType:ptReference),
		(directive: 'register';			tokenType:ptRegister),
		(directive: 'reintroduce';		tokenType:ptReintroduce),
		(directive: 'requires';			tokenType:ptRequires),
		(directive: 'resident';			tokenType:ptResident),
		(directive: 'safecall';			tokenType:ptSafecall),
		(directive: 'sealed';			tokenType:ptSealed),
		(directive: 'static';			tokenType:ptStatic),
		(directive: 'stdcall';			tokenType:ptStdcall),
		(directive: 'stored';			tokenType:ptStored),
		(directive: 'strict';			tokenType:ptStrict),
		(directive: 'unsafe';			tokenType:ptUnsafe),
		(directive: 'varargs';			tokenType:ptVarargs),
		(directive: 'virtual';			tokenType:ptVirtual),
		(directive: 'write';				tokenType:ptWrite),
		(directive: 'writeonly';		tokenType:ptWriteonly)
	);


implementation

uses
	TypInfo, System.Types, System.Character,
	SqmApi;

function TokenKindToStr(const ATokenType: TptTokenKind): string;
begin
	Result := Typinfo.GetEnumName(TypeInfo(TptTokenKind), Ord(ATokenType));
end;

function TokenKindIsTrivia(const ATokenKind: TptTokenKind): Boolean;
begin
(*
	// *** Trivia - Whitespace ***
	ptSpace,						// whitespace (#1..#32; excl. CR and LF)
	ptWhitespace,				// space, cr, lf, tab, etc
	ptCRLF,						// Carriage return and line feed

	// *** Trivia - Comments ***
	ptAnsiComment,				// (*  * }
	ptBorComment,				// {    } block comment
	ptSlashesComment,			// //     single-line comment
	ptCRLFCo:					// CRLF   inside a comment
*)
	Result := (ATokenKind in TriviaTokenSet);
end;

function IsReservedWord(const ATokenKind: TptTokenKind): Boolean;
{Reserved words (e.g., begin, end, case, for, if)

Reserved words are keywords that have a special meaning in the language and cannot be used as identifiers.
}


const
	ReservedWordsSet: set of TptTokenKind = [
		ptAbort,
		ptAnd,
		ptArray,
		ptAs,
		ptAsm,
		ptBegin,
		ptCase,
		ptClass,
		ptConst,
		ptConstructor,
		ptDestructor,
		ptDispInterface,
		ptDiv,
		ptDo,										// for i := 1 to 10 do
		ptDownTo,
		ptElse,
		ptEnd,
		ptExcept,
		ptExports,
		ptFile,
		ptFinalization,
		ptFinally,
		ptFor,
		ptFunction,
		ptGoto,
		ptIf,
		ptImplementation,
		ptIn,
		ptInherited,
		ptInitialization,
		ptInline,
		ptInterface,
		ptIs,
		ptLabel,
		ptLibrary,
		ptMod,
		ptNil,
		ptNot,							// if not Enabled
		ptObject,
		ptOf,
		ptOr,
		ptPacked,
		ptProcedure,
		ptProgram,
		ptProperty,
		ptRaise,
		ptRecord,
		ptRepeat,
		ptResourceString,
		ptSet,
		ptShl,
		ptShr,
		ptString,						// the literal "string"
		ptThen,
		ptThreadVar,
		ptTo,								// for i := 1 to 10 do
		ptTry,							// try finally end;
		ptType,							// TBirthDate = type TDate;
		ptUnit,							// unit Grobber;
		ptUntil,							// repeat until
		ptUses,							// uses Windows.Winapi;
		ptVar,							// var dt: TBirthDate;
		ptWhile,							// while do
		ptWith,							// with user begin end; // never use this ever
		ptXor								// a xor b
	];

begin
	// *** Reserved words (e.g., begin, end, case, for, if)
	// Reserved words are keywords that have a special meaning in the language and cannot be used as identifiers.
	// See the ReservedWords[] array for the list of reserved words
	Result := ATokenKind in ReservedWordsSet;
end;

function TokenName(const ATokenKind: TptTokenKind): string; // same as TokenKindToStr
begin
	Result := TokenKindToStr(ATokenKind);
end;

function TokensToStr(Tokens: TList): string;
var
	i: Integer;
	t: TSyntaxToken;
begin
	Result := 'Tokens';

	for i := 0 to Tokens.Count-1 do
	begin
		t := TObject(Tokens[i]) as TSyntaxToken;

		Result := Result+#13#10;
		Result := Result+'	['+IntToStr(i)+']='+t.ToString();
	end;
end;

{ DelphiTokenizer }

function TDelphiTokenizer.Consume: WideChar;
var
	lineEnd: Boolean;
begin
	FCurrentInputCharacter := Self.GetNextChar;

	// If we got an EOF signal, then simply return the EOF character.
	if FCurrentInputCharacter = UEOF then
		Exit(FCurrentInputCharacter);

	// If the current char is CR, and the next is LF, then hold-off on incrementing
	// If the current char is CR, and the next is not LF, then increment now
	// If the current char is LF, then increment now
	lineEnd :=
			((FCurrentInputCharacter = #13) and (Peek <> #10))
			or (FCurrentInputCharacter = #10);

	if lineEnd then
	begin
		Inc(FCurrentLine);
		FCurrentColumn := 1;
	end
	else if Peek = #10 then
		// don't increment the column, as the following LF is "part of" this column/character
	else
		Inc(FCurrentColumn);

	Result := FCurrentInputCharacter;
end;

constructor TDelphiTokenizer.Create(SourceStream: ISequentialStream; Encoding: Word);
begin
	inherited Create;

	FCompilerDirectives := TStringList.Create;

	FCurrentInputCharacter := UEOF;
	FCurrentLine := 1;
	FCurrentColumn := 1;

//	FState2 := tsDataState; //The state machine must start in the data state.
//	FCurrentInputCharacter := MaxInt; //UCS4 doesn't allow negative, so we use $FFFFFFFF

	FStream := TInputStream.Create(SourceStream, Encoding);
end;

function CreateStreamOnMemory(pData: Pointer; nCount: DWORD): IStream;
var
	hMem: HGLOBAL;
	dwError: DWORD;
	p: Pointer;
begin
	hMem := GlobalAlloc(GHND, nCount);
			//CreateStreamOnHGlobal says "The handle must be allocated as movable and nondiscardable."
			//GlobalAlloc says GMEM_NODISCARD is provided for 16-bit compatibilty and is ignored
			//GHND is GMEM_MOVEABLE + GMEM_ZEROINIT
			//GPTR is GMEM_FIXED    + GMEM_ZEROINIT
	if hMem = 0 then
		RaiseLastOSError;

	if nCount > 0 then
	begin
		p := GlobalLock(hMem);
		if p = nil then
		begin
			dwError := GetLastError;
			GlobalFree(hMem);
			raise EOSError.Create('Could not lock global memory object: '+SysErrorMessage(Integer(dwError)));
		end;
		try
			CopyMemory(p, pData, nCount);
		finally
			GlobalUnlock(hMem);
		end;
	end;

	OleCheck(CreateStreamOnHGlobal(hMem, True, Result)); //Because we pass True, the stream will take care of freeing the HGLOBAL when the stream is released
end;

constructor TDelphiTokenizer.Create(const SourceCode: UnicodeString);
var
	stm: ISequentialStream;
begin
{
	Wrap the stream up in the ISequentialStream you were going to ideally provide,
	and call the other constructor.
}

	stm := CreateStreamOnMemory(Pointer(SourceCode), Length(SourceCode)*sizeof(WideChar));

	Self.Create(stm, CP_UTF16);
end;

procedure TDelphiTokenizer.Log(const s: string);
begin
	OutputDebugString(PChar(s));
end;

procedure TDelphiTokenizer.LogFmt(const s: string; const Args: array of const);
begin
	Log(Format(s, Args));
end;

function TDelphiTokenizer.GetDirectiveTokenKind(const Keyword: string): TptTokenKind;
begin
{
	Check if 'Keyword' is a directive (a special type of keyword).

	If it is a recognized directive, it returns it as a ptTokenKind. For example:

			platform ==> true

	In Delphi the magic words are referred to as "keywords".

	Keywords are then broken down into two categories:

		- reserved words (e.g. begin, end, case, for, if)
		- directives (e.g. public, strict, safecall)

	Our job here is to compare the supplied Keyword with the list of known
	directies, and return us that TokenKind.

	If the keyword is not a recognized directive, it returns ptUnknown
}
	Result := ptUnknown;

	for var v in Directives do
	begin
		if SameText(Keyword, v.directive) then
		begin
			Result := v.tokenType;
			Exit;
		end;
	end;
end;

function TDelphiTokenizer.GetReservedTokenKind(const Keyword: string): TptTokenKind;
begin
{
	Check if 'Keyword' is one of the special Reserved keywords.

	In Delphi the magic words are referred to as "keywords".

	Keywords are then broken down into two categories:

		- reserved words (e.g. begin, end, case, for, if)
		- directives (e.g. public, strict, safecall)

	Our job here is to compare the supplied Keyword
}
	Result := ptUnknown;

	for var v in ReservedWords do
	begin
		if SameText(Keyword, v.Keyword) then
		begin
			Result := v.tokenType;
			Exit;
		end;
	end;
end;

function TDelphiTokenizer.ProcessStringEscapes(const rawString: string): string;
begin
{
	Process escape sequences in a Delphi string literal.
	Currently handles: '' -> ' (double apostrophe becomes single)
	Future: Could handle other escape sequences if needed
}
	Result := rawString;
	// Handle double apostrophe escape sequence
	Result := StringReplace(Result, '''''', '''', [rfReplaceAll]);
end;

function TDelphiTokenizer.IsValidSurrogatePair(highSurrogate, lowSurrogate: WideChar): Boolean;
begin
{
	Check if two characters form a valid Unicode surrogate pair
}
	Result := (Ord(highSurrogate) >= $D800) and (Ord(highSurrogate) <= $DBFF) and
	          (Ord(lowSurrogate) >= $DC00) and (Ord(lowSurrogate) <= $DFFF);
end;

function TDelphiTokenizer.NextToken(out AToken: TSyntaxToken): Boolean;
var
	triviaToken: TSyntaxToken;
	leadingTrivia: TObjectList<TSyntaxToken>;
begin
{
	Returns the next token. Returns true if a token was produced (including the EOF token).
	After returning the EOF token, subsequent calls will return False+nil.

	Consume the next input token, and take action depending on what it is.

	The understanding is that each handler will then greedily munch (Consume) the input
	and return the corresponding token.

	Since we are a whitespace and comment preserving tokenizer, our goal is to

		- eat trivia tokens
		- store them in the trivia list
		- until we come across an actual token

	Then we add the trivia to the token.LeadingTrivia list.

	Gotcha: After we read a token, we need to read trivia ahead until we can peek
	at the next token. Because if there is no next token (EOF), then we
	have to add the trivia to **this** token before it goes out.

	That's how Roslyn does it
}

	leadingTrivia := TObjectList<TSyntaxToken>.Create(False); // we don't own these, they'll be owned by the token
	try
		// Peek for trivia tokens, and eat them into a leading trivia list
		while TokenKindIsTrivia(PeekTokenKind) do
		begin
			// It's a trivia token. Add to our delay list, and move on
			if GetNextToken({out}triviaToken) then
			begin
				leadingTrivia.Add(triviaToken);
				triviaToken := nil;
			end;
		end;

		// Now we should be at a real token (or EOF)
		Result := GetNextToken({out}AToken);
		if Result then
		begin
			// It's a real token. Add the pending leading trivia to this token
			AToken.FLeadingTrivia.AddRange(leadingTrivia.ToArray);

			// And now eat any trailing trivia after this token and add it as trailing trivia
			while TokenKindIsTrivia(PeekTokenKind) do
			begin
				// It's a trivia token. Add to our delay list, and move on
				if GetNextToken({out}triviaToken) then
				begin
					AToken.FTrailingTrivia.Add(triviaToken);
					triviaToken := nil;
				end;
			end;

			// Return the token
			Exit;
		end;
		AToken := nil;
		Result := False;
	finally
		leadingTrivia.Free;
	end;
end;

function TDelphiTokenizer.DoAnsiComment(const ch: WideChar): TSyntaxToken;
var
	s: string;
	nextChar: Char;
	hasError: Boolean;
	errorMsg: string;
begin
{
	ptAnsiComment

	Text:			(* These multiline comments take priority over line and braces comments  *)
	ValueText:	These multiline comments take priority over line and braces comments
}

	hasError := False;
	errorMsg := '';

	s := ch;          // add the (
	s := s + Consume; // consume the *

	nextChar := Peek;
	while (nextChar <> UEOF) do
	begin
		if (nextChar = '*') and (PeekNext = ')') then
		begin
			// We're at the end
			s := s + Consume; // consume the closing *
			s := s + Consume; // consume the closing )
			Break;
		end;
		s := s + Consume;
		nextChar := Peek;
	end;

	// If we stopped because of EOF, it's unterminated
	if nextChar = UEOF then
	begin
		hasError := True;
		errorMsg := 'Unterminated ANSI comment';
	end;

	Result := TSyntaxToken.Create(ptAnsiComment, FCurrentLine, FCurrentColumn, s);
	Result.HasErrors := hasError;
	Result.ErrorMessage := errorMsg;
	if hasError then
		Result.IsMissing := True;
end;

function TDelphiTokenizer.DoAssign(const ch: WideChar): TSyntaxToken;
begin
{
   :=
   ptAssign
}
	Consume; // consume the equals
	Result := TSyntaxToken.Create(ptAssign, FCurrentLine, FCurrentColumn, ':=');
end;

function TDelphiTokenizer.DoAsterisk(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptAsterisk, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoAmpersand(const ch: WideChar): TSyntaxToken;
var
	s: UnicodeString;
	chNext: WideChar;
begin
{
	Handle ampersand-escaped identifiers: &for, &begin, etc.
	The ampersand allows reserved keywords to be used as identifiers.
	Example: var &for: Integer;
	
	The token text should include the ampersand, but tokenKind is always ptIdentifier.
}
	s := ch; // Start with '&'
	
	chNext := Peek;
	
	// The character after & must be a valid identifier start character
	if not IsStartOfIdentifierCharacter(chNext) then
	begin
		// Just return ampersand as a symbol token if not followed by identifier
		Result := TSyntaxToken.Create(ptAmpersand, FCurrentLine, FCurrentColumn, ch);
		Exit;
	end;
	
	// Consume the identifier part (even if it's a reserved word)
	s := s + Consume; // First char of identifier
	chNext := Peek;
	while IsIdentifierCharacter(chNext) do
	begin
		s := s + Consume;
		chNext := Peek;
	end;
	
	// Always return as ptIdentifier, regardless of whether the text after & is a keyword
	Result := TSyntaxToken.Create(ptIdentifier, FCurrentLine, FCurrentColumn, s);
end;

function TDelphiTokenizer.DoStringLiteral(const ch: WideChar): TSyntaxToken;
var
	s: string;
	nextChar: WideChar;
	startLine, startColumn: Integer;
	hasError, hasWarning: Boolean;
	errorMsg, warningMsg: string;
	lowSurrogate: WideChar;
begin
{
	Enhanced string literal parser with robust error handling.
	
	Handles:
	- Unterminated strings (error)
	- Control characters (warning)
	- Unicode surrogate pairs (validation)
	- Line ending detection (warning/error)
	- Length validation (warning)
	- Proper escape sequence processing
}
	s := '';  // will contain the string content without leading/trailing apostrophe
	startLine := FCurrentLine;
	startColumn := FCurrentColumn;
	hasError := False;
	hasWarning := False;
	errorMsg := '';
	warningMsg := '';

	nextChar := Consume;
	while (nextChar <> UEOF) do
	begin
		if nextChar = '''' then
		begin
			// Check for double apostrophe, which means to insert a single '
			if Peek <> '''' then
			begin
				// The thing after the apostrophe is not a 2nd ', which means the string is done
				Break;
			end;

			// The string is not ending, we're just inserting a literal apostrophe (')
			s := s + Consume; // consume the second apostrophe
		end
		else if CharInSet(nextChar, [#13, #10]) then
		begin
			// Strings end on line breaks - this is usually an error
			if not hasWarning then
			begin
				hasError := True;
				errorMsg := 'E2052 Unterminated string';
			end;
			LogFmt('Warning at line %d: String literal not closed before end of line', [startLine]);
			// Don't consume the line ending character - let it be processed normally
			Break;
		end
		else if (Ord(nextChar) >= $D800) and (Ord(nextChar) <= $DBFF) then
		begin
			// High surrogate - should be followed by low surrogate
			lowSurrogate := Peek;
			if IsValidSurrogatePair(nextChar, lowSurrogate) then
			begin
				s := s + nextChar + Consume; // consume both parts of surrogate pair
			end
			else
			begin
				if not hasWarning then
				begin
					hasWarning := True;
					warningMsg := 'Invalid Unicode surrogate pair';
				end;
				LogFmt('Warning at line %d: Invalid Unicode surrogate pair', [startLine]);
				s := s + nextChar; // include anyway for recovery
			end;
		end
		else
		begin
			// Normal character
			s := s + nextChar;
		end;

		// Check string length limit
		// Delphi 12.0+ raised it to unlimited.
		if CompilerVersion < 36.0 then
		begin
			if Length(s) > 1023 then
			begin
				if not hasWarning then
				begin
					hasError := True;
					warningMsg := 'F2069 Line too long (more than 1023 characters)';
				end;
				LogFmt('Warning at line %d: String literal exceeds recommended maximum length', [startLine]);
			end;
		end;

		nextChar := Consume;
	end;

	// Check for unterminated string (reached EOF)
	if nextChar = UEOF then
	begin
		hasError := True;
		errorMsg := 'Unterminated string literal';
		LogFmt('Error at line %d: Unterminated string literal', [startLine]);
	end;

	// Create the token
	Result := TSyntaxToken.Create(ptStringLiteral, startLine, startColumn, '''' + s + '''');

	// Set error/warning flags
	Result.HasErrors := hasError;
	Result.HasWarnings := hasWarning;
	Result.ErrorMessage := errorMsg;
	Result.WarningMessage := warningMsg;

	// Process escape sequences for ValueText
	Result.ValueText := ProcessStringEscapes(s);

	// Mark as missing if there was an error (for parser recovery)
	if hasError then
		Result.IsMissing := True;
end;

function TDelphiTokenizer.DoMultilineStringLiteral(const ch: WideChar): TSyntaxToken;
var
	s: string;
	rawContent: string;
	nextChar: WideChar;
	startLine, startColumn: Integer;
	hasError, hasWarning: Boolean;
	errorMsg, warningMsg: string;
	quoteCount: Integer;
	closingQuoteCount: Integer;
	lines: TStringList;
	baseIndentation: Integer;
	i, lineIndent: Integer;
	processedContent: string;
begin
	// ptMultilineStringLiteral		'''...'''
(*

https://docwiki.embarcadero.com/RADStudio/Florence/en/String_Types_(Delphi)

Multiline String Literals
==========================

A multiline string is introduced by a triple quote (''') and a new line,
		can spawn multiple lines of the source code,
		and ends with a closing triple quote (''').

For example:

const
	strML1 = '''
			The quick brown fox jumps
			over the lazy dog.
			''';

	strHTML = '''
			<UL>
				<LI>Item 1</LI>
				<LI>Item 2</LI>
				<LI>Item 3</LI>
				<LI>Item 4</LI>
			</UL>
			''';
	strJSON = '''
			[
				{"id" : "1", "name" : "Large"},
				{"id" : "2", "name" : "Medium"},
				{"id" : "2", "name" : "Small"}
			]
			''';
	strSQL= '''
			SELECT *
			FROM Customers
			WHERE Department = 'R&D'
			ORDER BY Name;
			''';

Multiline string indentation and formatting logic are used very specifically.
A multiline string treats a leading white space this way:

- The closing ''' needs to be in a line of its own, not at the end of the last line of the string itself.
- The indentation of the closing ''' determines the base indentation of the entire string.
- Each blank space before that indentation level is removed in the final string for each of the lines.
- None of the lines can be less indented than the base indentation (the closing '''). This is a compiler error, showing also as Error Insight.
- The last newline before the closing ''' is omitted. If you want to have a final new line, you should add an empty line at the end.

Note: When pasting multiple lines of text from an external application into the RAD Studio editor,
		there might be special non-visible characters, control characters, specific newline combinations,
		and uncommon Unicode characters that can potentially confuse the editor.

Note: Notice that the triple quotes (''') can also be replaced with a large odd number of quotes,
		like 5 or 7. This allows embedding an actual triple quote within a multiline string.

For example:

var s := '''''''
		some text
		and now '''
		some more text
		''''''';


	//Error that should be returned: E2657 Inconsistent indent characters
	sourceCode := T3+CRLF+
'			The quick brown fox jumps'+CRLF+
'         over the lazy dog.'+CRLF+
'         '+T3;

Multiline string cannot mix tabs and spaces inconsistently

var s := '''
	   SELECT *          
		FROM Customers		
	   '''
		
is invalid because:

- [tab][space][space]
- [tab][tab]

	Error: E2657 Inconsistent indent characters

*)
	s := '';  // will contain the entire token including opening quotes
	rawContent := '';  // will contain just the content between quotes
	startLine := FCurrentLine;
	startColumn := FCurrentColumn;
	hasError := False;
	hasWarning := False;
	errorMsg := '';
	warningMsg := '';

	// Count opening quotes (should be at least 3, but can be 5, 7, etc.)
	quoteCount := 1;  // We already consumed the first '
	while Peek = '''' do
	begin
		Consume;
		Inc(quoteCount);
	end;

	// Multiline strings require odd number of quotes >= 3
	if (quoteCount < 3) or ((quoteCount mod 2) = 0) then
	begin
		hasError := True;
		errorMsg := 'Invalid multiline string literal - requires odd number of quotes (3, 5, 7, etc.)';
		LogFmt('Error at line %d: Invalid multiline string literal', [startLine]);
	end;

	// Build opening quotes string
	s := StringOfChar('''', quoteCount);

	// Multiline strings should start with a newline after opening quotes
	nextChar := Peek;
	if not CharInSet(nextChar, [#13, #10]) then
	begin
		hasWarning := True;
		warningMsg := 'Multiline string should start with a newline after opening quotes';
		LogFmt('Warning at line %d: Multiline string should start with newline', [startLine]);
	end;

	// Consume characters until we find the closing quotes
	nextChar := Consume;
	while (nextChar <> UEOF) do
	begin
		// Check for potential closing quotes
		if nextChar = '''' then
		begin
			closingQuoteCount := 1;
			
			// Count consecutive quotes
			while Peek = '''' do
			begin
				Consume;
				Inc(closingQuoteCount);
			end;

			// If we found matching number of quotes, we're done
			if closingQuoteCount = quoteCount then
			begin
				// Validate that the closing quotes are on their own line with only optional whitespace before them
				var lastCR := LastDelimiter(#13#10, rawContent);
				var lastLine: string;
				if lastCR = 0 then
					lastLine := rawContent
				else
					lastLine := Copy(rawContent, lastCR + 1, MaxInt);

				// If there's any non-whitespace before the closing quotes on the same line, it's E2658
				if Trim(lastLine) <> '' then
				begin
					hasError := True;
					errorMsg := 'E2658 There should be no non-whitespace characters before the closing quotes of the text block';
					LogFmt('Error at line %d: %s', [startLine, errorMsg]);
				end;

				s := s + rawContent + StringOfChar('''', closingQuoteCount);
				Break;
			end
			else
			begin
				// Not the closing quotes, include them in the content
				rawContent := rawContent + StringOfChar('''', closingQuoteCount);
			end;
		end
		else
		begin
			// Normal character
			rawContent := rawContent + nextChar;
		end;

		nextChar := Consume;
	end;

	// Check for unterminated multiline string (reached EOF)
	if nextChar = UEOF then
	begin
		hasError := True;
		errorMsg := 'Unterminated multiline string literal';
		LogFmt('Error at line %d: Unterminated multiline string literal', [startLine]);
		s := s + rawContent;  // Include what we have
	end;

	// Create the token
	Result := TSyntaxToken.Create(ptMultilineStringLiteral, startLine, startColumn, s);

	// Set error/warning flags
	Result.HasErrors := hasError;
	Result.HasWarnings := hasWarning;
	Result.ErrorMessage := errorMsg;
	Result.WarningMessage := warningMsg;

	// Process indentation rules for ValueText
	if not hasError then
	begin
		lines := TStringList.Create;
		try
			lines.Text := rawContent;

			// Remove first line if it's empty (the newline after opening quotes)
			if (lines.Count > 0) and (Trim(lines[0]) = '') then
				lines.Delete(0);

			// Determine base indentation and pattern from the closing quote position
			// The last line contains only the indentation before the closing '''
			// This determines the base indentation (count and exact whitespace pattern)
			baseIndentation := 0;
			var baseIndentStr: string := '';
			if (lines.Count > 0) and (Trim(lines[lines.Count - 1]) = '') then
			begin
				// The last line should contain only whitespace (the indentation before closing ''')
				lineIndent := 0;
				while (lineIndent < Length(lines[lines.Count - 1])) and
					  CharInSet(lines[lines.Count - 1][lineIndent + 1], [' ', #9]) do
					Inc(lineIndent);
				baseIndentation := lineIndent;
				baseIndentStr := Copy(lines[lines.Count - 1], 1, baseIndentation);

				// Remove the last line (the closing quote indentation line)
				lines.Delete(lines.Count - 1);
			end;

			// Validate indentation of all non-empty lines against the base indentation
			// Rules:
			// - If a line has less leading whitespace than base, it's an error (E2657) but we still recover.
			// - If a line has at least baseIndentation whitespace but the first baseIndentation characters
			//   don't match baseIndentStr, it's an error (E2657) due to inconsistent indent characters.
			if baseIndentation > 0 then
			begin
				for i := 0 to lines.Count - 1 do
				begin
					if Trim(lines[i]) = '' then
						Continue;

					// compute this line's leading whitespace length and prefix
					lineIndent := 0;
					while (lineIndent < Length(lines[i])) and CharInSet(lines[i][lineIndent + 1], [' ', #9]) do
						Inc(lineIndent);

					if lineIndent < baseIndentation then
					begin
						if not hasError then
						begin
							hasError := True;
							errorMsg := 'E2657 Inconsistent indent characters';
							Result.HasErrors := True;
							Result.ErrorMessage := errorMsg;
							LogFmt('Error at line %d: E2657 Inconsistent indent characters (less indented line)', [startLine]);
						end;
					end
					else if Copy(lines[i], 1, baseIndentation) <> baseIndentStr then
					begin
						if not hasError then
						begin
							hasError := True;
							errorMsg := 'E2657 Inconsistent indent characters';
							Result.HasErrors := True;
							Result.ErrorMessage := errorMsg;
							LogFmt('Error at line %d: E2657 Inconsistent indent characters (mixed whitespace)', [startLine]);
						end;
					end;
				end;
			end;

			// Remove base indentation from each line (recover for less-indented lines by stripping what they have)
			processedContent := '';
			for i := 0 to lines.Count - 1 do
			begin
				if i > 0 then
					processedContent := processedContent + sLineBreak;

				if Trim(lines[i]) = '' then
				begin
					// preserve empty lines
					processedContent := processedContent;
					Continue;
				end;

				// compute actual leading whitespace count for this line
				lineIndent := 0;
				while (lineIndent < Length(lines[i])) and CharInSet(lines[i][lineIndent + 1], [' ', #9]) do
					Inc(lineIndent);

				// Remove min(baseIndentation, lineIndent) leading characters
				var removeCount := baseIndentation;
				if lineIndent < removeCount then
					removeCount := lineIndent;

				processedContent := processedContent + Copy(lines[i], removeCount + 1, MaxInt);
			end;

			Result.ValueText := processedContent;
		finally
			lines.Free;
		end;
	end
	else
	begin
		Result.ValueText := rawContent;
	end;

	// Mark as missing if there was an error (for parser recovery)
	if hasError then
		Result.IsMissing := True;
end;

function TDelphiTokenizer.DoAsciiChar(const ch: WideChar): TSyntaxToken;
var
	s: string;
	isHexMode: Boolean;
begin
	// ptAsciiChar				#149, #$3E
	s := ch;

	isHexMode := (Peek = '$');

	if isHexMode then
	begin
		s := s+Consume;   // consume the hex $

		// Hex characters
		while CharInSet(Peek, ['A'..'F', 'a'..'f', '0'..'9']) do
			s := s+Consume;
	end
	else
	begin
		// decimal characters
		while CharInSet(Peek, ['A'..'F', 'a'..'f', '0'..'9']) do
			s := s+Consume;
	end;

	Result := TSyntaxToken.Create(ptAsciiChar, FCurrentLine, FCurrentColumn, s);
end;

function TDelphiTokenizer.DoDoubleAddressOp(const ch: WideChar): TSyntaxToken;
begin
	// ptDoubleAddressOp    @@
	Consume; // consume the second at sign
	Result := TSyntaxToken.Create(ptDoubleAddressOp, FCurrentLine, FCurrentColumn, '@@');
end;

destructor TDelphiTokenizer.Destroy;
begin
	// PeekTokenKind can prefetch one token into FNextToken.
	// If callers stop before draining, the tokenizer still owns that token.
	FreeAndNil(FNextToken);
	FreeAndNil(FStream);
	FreeAndNil(FCompilerDirectives);

	inherited;
end;

function TDelphiTokenizer.DoAddressOp(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptAddressOp, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoBorComment(const ch: WideChar): TSyntaxToken;
var
	s: string;
	nextChar: WideChar;
	hasError: Boolean;
	errorMsg: string;
begin
(*
	ptBorComment

	Value:		{ comment }
	ValueText:  comment
*)
	hasError := False;
	errorMsg := '';

	s := ch; // the open brace

	nextChar := Peek;
	while nextChar <> UEOF do
	begin
		if nextChar = '}' then
		begin
			s := s + Consume;
			Break;
		end;
		s := s + Consume;
		nextChar := Peek;
	end;

	// If we stopped because of EOF, it's unterminated
	if nextChar = UEOF then
	begin
		hasError := True;
		errorMsg := 'Unterminated Borland comment';
	end;

	Result := TSyntaxToken.Create(ptBorComment, FCurrentLine, FCurrentColumn, s);
	Result.HasErrors := hasError;
	Result.ErrorMessage := errorMsg;
	if hasError then
		Result.IsMissing := True;

	if (s <> '') and (s[1] = '{') then
		s := Copy(s, 2, Length(s));
	if (s <> '') and (s[Length(s)-1] = '}') then
		s := Copy(s, 1, Length(s)-1);
	s := Trim(s);
  Result.ValueText := s; // TODO: get the interior comment text
end;

function TDelphiTokenizer.DoCaret(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptCaret, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoColon(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptColon, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoComma(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptComma, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoCompilerDirective(const ch: WideChar): TSyntaxToken;
var
	s: string;
	nextChar: WideChar;
	hasError: Boolean;
	errorMsg: string;
	startLine: Integer;
	startColumn: Integer;
	delimiter: TDirectiveDelimiter;
begin
(*
	ptCompDirect

		Text:			{$foo bar}
		ValueText:

		Text:       (*$foo bar*﻿)

	ptDirective

		Text:			'{$IFDEF DEBUG}'
		ValueText:	'IFDEF DEBUG' (optional, same as Name + ' ' + Args)

		Directive^.Name: 	'IFDEF'
		Directive^.Args:	'DEBUG'
		Directive^.Kind"	dkIfDef
		ShortForm: 			False


	Attribute akDirectiveDelimeter
			ddBrace
			ddParenStar
*)
	hasError := False;
	errorMsg := '';
	startLine := FCurrentLine;
	startColumn := FCurrentColumn;

	case ch of
	'{':
		begin
			s := ch; // opening brace
			delimiter := ddBrace;

			// Keep reading until we reach the closing }
			nextChar := Peek;
			while (nextChar <> '}') and (nextChar <> UEOF) and not CharInSet(nextChar, [#13, #10]) do
			begin
				s := s+Consume;
				nextChar := Peek;
			end;
		end;
	'(':
		begin
			s := ch;				// add the (
			s := s+Consume;	// consume the *
			s := s+Consume;	// consume the $
			delimiter := ddParenStar;

			// Keep reading until we reach the closing *)
			// Unlike block comments, directives end at the end of the line.
			nextChar := Peek;
			while (nextChar <> UEOF) and not CharInSet(nextChar, [#13, #10]) do
			begin
				if (nextChar = '*') and (PeekNext = ')') then
				begin
					// We're at the end
					s := s + Consume; // consume the closing *
					s := s + Consume; // consume the closing )
					Break;
				end;

				s := s+Consume;
				nextChar := Peek;
			end;
		end;
	else
		raise Exception.Create('DoCompilerDirective expects $ or (');
	end;

	// Check for unterminated compiler directive (reached EOF)
	if nextChar = UEOF then
	begin
		hasError := True;
		errorMsg := 'Unterminated compiler directive';
		LogFmt('Error at line %d: Unterminated compiler directive (EOF reached)', [startLine]);
	end
	// Check for unterminated compiler directive (reached line end)
	else if CharInSet(nextChar, [#13, #10]) then
	begin
		hasError := True;
		errorMsg := 'Unterminated compiler directive';
		LogFmt('Error at line %d: Unterminated compiler directive (line end reached)', [startLine]);
	end
	else
	begin
		s := s+Consume; //consume the }
	end;

	Result := TSyntaxToken.Create(ptCompilerDirective, startLine, startColumn, s);
	Result.DirectiveDelimeter := delimiter;

	// Set error flags
	Result.HasErrors := hasError;
	Result.ErrorMessage := errorMsg;
	
	// Mark as missing if there was an error (for parser recovery)
	if hasError then
		Result.IsMissing := True;
		
//	Result.ValueText := Should we do something to parse the compiler directive?

(*
		//The pre-processor (not yet written) is the one who will keep track of compiler directives.
		//But surely we can do a *little* parsing? and parse out something?

		// The format of a compiler directive is:
		//   {$DIRECTIVE_NAME optional arguments}
		//
		//    {$ENDIF}			==> name:ENDIF
		//    {$ELSE}			==> name:ELSE
		//    {$I+}				==> name:I, arg1:+
		//    {$HINTS ON}		==> name:HINTS, arg1:ON
*)
end;

function TDelphiTokenizer.DoPoint(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptDot, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoDotDot(const ch: WideChar): TSyntaxToken;
begin
{
	..
	ptDotDot
}
   Consume; // consume the second dot
	Result := TSyntaxToken.Create(ptDotDot, FCurrentLine, FCurrentColumn, '..');
end;

function TDelphiTokenizer.DoEqual(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptEquals, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoGreater(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptGreaterThan, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoGreaterThanEquals(const ch: WideChar): TSyntaxToken;
begin
{
	>=

}
	Consume; // consume the equals
	Result := TSyntaxToken.Create(ptGreaterThanEquals, FCurrentLine, FCurrentColumn, '>=');
end;

function TDelphiTokenizer.DoHexNumber(const ch: WideChar): TSyntaxToken;
var
	s: string;
	nextChar: WideChar;
begin
{
	ptIntegerConst    (starting with $)

   "$" 0-9A-Fa-f_

   For example

      $1F_9C_3B_28
}
	s := ch;

	nextChar := Peek;
	while IsHexCharacter(nextChar) do
	begin
		s := s+Consume;

		nextChar := Peek;
	end;

	Result := TSyntaxToken.Create(ptIntegerConst, FCurrentLine, FCurrentColumn, s);
end;

function TDelphiTokenizer.DoBinaryNumber(const ch: WideChar): TSyntaxToken;
var
	s: string;
	nextChar: WideChar;
begin
{
	ptIntegerConst

	e.g. %1011000110
}
	s := ch;

	nextChar := Peek;
	while IsBinaryCharacter(nextChar) do
	begin
		s := s+Consume;
		nextChar := Peek;
	end;

	Result := TSyntaxToken.Create(ptIntegerConst, FCurrentLine, FCurrentColumn, s);
end;

function TDelphiTokenizer.DoLessThanEquals(const ch: WideChar): TSyntaxToken;
begin
{
	<=
	ptLessThanEquals
}
	Consume; // consume the equals
	Result := TSyntaxToken.Create(ptLessThanEquals, FCurrentLine, FCurrentColumn, '<=');
end;

function TDelphiTokenizer.DoLessThan(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptLessThan, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoMinus(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptMinus, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoNotEqual(const ch: WideChar): TSyntaxToken;
begin
{
	<>
	ptNotEqual
}
	Consume; //consume the greater-than
	Result := TSyntaxToken.Create(ptNotEqual, FCurrentLine, FCurrentColumn, '<>');
end;

function TDelphiTokenizer.DoNumber(const ch: WideChar): TSyntaxToken;
var
	s: string;
	tokenKind: TptTokenKind;
	c: WideChar;
	hadFraction, hadExponent: Boolean;

	function IsDigit(const ch: WideChar): Boolean; inline;
	begin
		Result := CharInSet(ch, ['0'..'9']);
	end;

	function IsDigitOrUnderscore(const ch: WideChar): Boolean; inline;
	begin
		Result := IsDigit(ch) or (ch = '_');
	end;

	procedure ConsumeDigitsAndUnderscores;
	begin
		while IsDigitOrUnderscore(Peek) do
			s := s + Consume;
	end;

begin
{
	Scan a number that starts with a digit. Supports:


	- Decimal integers with underscores (e.g. 1_000)
	- Fractions with a dot, but does NOT consume '.' if it's part of '..' (range).
		Allows literals such as "1." or "1.e3" where no digit follows the dot.
	- Exponent part with 'e' or 'E', optional sign immediately after, and requires
		at least one digit (digits may include underscores). Only then it's a float.
}
	s := ch;
	hadFraction := False;
	hadExponent := False;

	// Integer part (already have first digit in s)
	ConsumeDigitsAndUnderscores;

		// Fractional part: only if next is '.' AND next-next is not '.'
	if (Peek = '.') then
	begin
		// Don't treat '..' as fraction. If it's a single '.', allow optional digits afterwards
		// so that literals like "42." or "42.e3" are treated as floats.
		if (PeekNext <> '.') then
		begin
			s := s + Consume; // consume '.'
			hadFraction := True;
			if IsDigit(Peek) then
				ConsumeDigitsAndUnderscores;
		end;
	end;

	// Exponent part: 'e' or 'E'
	if CharInSet(Peek, ['e', 'E']) then
	begin
		// Look ahead to ensure this is a real exponent: optional sign, then digit
		// Examine char right after e/E and, if sign, the char after that
		c := Peek(2); // char after e/E
		if (c = '+') or (c = '-') then
		begin
			c := Peek(3);	// char after optional sign
		end;

		if IsDigit(c) then
		begin
			// Valid exponent: consume e/E
			s := s + Consume; // e/E
			// optional sign
			if CharInSet(Peek, ['+', '-']) then
				s := s + Consume;
			// at least one digit, then digits/underscores
			ConsumeDigitsAndUnderscores;
			hadExponent := True;
		end;
	end;

	if hadFraction or hadExponent then
		tokenKind := ptFloat
	else
		tokenKind := ptIntegerConst;

	Result := TSyntaxToken.Create(tokenKind, FCurrentLine, FCurrentColumn, s);
end;

function TDelphiTokenizer.DoPlus(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptPlus, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoReadIdentifier(const ch: WideChar): TSyntaxToken;
var
	s: UnicodeString;
	tokenKind: TptTokenKind;
	chNext: WideChar;
begin
{
	ptIdentifier
}
	s := ch;

	chNext := Peek;
	while IsIdentifierCharacter(chNext) do
	begin
		s := s+Consume;
		chNext := Peek;
	end;

	tokenKind := GetReservedTokenKind(s);
	if tokenKind = ptUnknown then
		tokenKind := ptIdentifier;

	Result := TSyntaxToken.Create(tokenKind, FCurrentLine, FCurrentColumn, s);

	// Populate the directive TokenKind if the identifier has the same name as a directive
	if tokenKind = ptIdentifier then
		Result.DirectiveID := GetDirectiveTokenKind(s);
end;

function TDelphiTokenizer.DoCloseParen(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptCloseParen, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoOpenParen(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptOpenParen, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoRoundDotClose(const ch: WideChar): TSyntaxToken;
begin
{
   .)
   ptRoundDotClose
}
	Consume; // Consume the close round parenthesis
	Result := TSyntaxToken.Create(ptRoundDotClose, FCurrentLine, FCurrentColumn, '.)');
end;

function TDelphiTokenizer.DoRoundDotOpen(const ch: WideChar): TSyntaxToken;
begin
{
   (.
	ptRoundDotOpen
}
	Consume; // consume the dot
	Result := TSyntaxToken.Create(ptRoundDotOpen, FCurrentLine, FCurrentColumn, '(.');
end;

function TDelphiTokenizer.DoSemicolon(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptSemicolon, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoSlash(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptSlash, FCurrentLine, FCurrentColumn, ch);
end;

function TDelphiTokenizer.DoSlashesComment(const ch: WideChar): TSyntaxToken;
var
	s: string;
	nextChar: WideChar;

	function IsLineEnd(const AChar: WideChar): Boolean;
	begin
		case AChar of
		#13, #10: Result := True;
		UEOF: Result := True;
		else
			Result := False;
		end;
	end;
begin
{
   ptSlashesComment	//
}
	Consume; //consume the 2nd slash

	s := '';

	// Read to the end of the line
	nextChar := Peek;
	while not IsLineEnd(nextChar) do
	begin
		s := s+Consume;
		nextChar := Peek;
	end;

	Result := TSyntaxToken.Create(ptSlashesComment, FCurrentLine, FCurrentColumn, '//'+s);
	Result.ValueText := s;
end;

function TDelphiTokenizer.DoCloseBracket(const ch: WideChar): TSyntaxToken;
begin
	Result := TSyntaxToken.Create(ptCloseBracket, FCurrentLine, FCurrentColumn, ']');
end;

function TDelphiTokenizer.DoOpenBracket(const ch: WideChar): TSyntaxToken;
begin
{
	xmas
	Nice scrabble board. raised edges so the pieces don't slide off.
	hardwood.
}
	Result := TSyntaxToken.Create(ptOpenBracket, FCurrentLine, FCurrentColumn, '[');
end;

function TDelphiTokenizer.DoWhitespace(const ch: WideChar): TSyntaxToken;
var
	s: string;
	chNext: WideChar;
begin
{
	ptWhitespace
}
	s := ch;

	chNext := Peek;
	while IsWhitespaceCharacter(chNext) do
	begin
		s := s+Consume;
		chNext := Peek;
	end;

	Result := TSyntaxToken.Create(ptWhitespace, FCurrentLine, FCurrentColumn, s);
end;

function TDelphiTokenizer.GetNextChar: WideChar;
var
	res: Boolean;
begin
{
	Gets the next character from the input stream.
	Returns UEOF if we've reached the end of the input stream.

	Do not use this generally to fetch the next character, instead use .Consume.

	The difference is that:

		-  in addition to the fact that Consume sets the FCurrentInputCharacter,
		- is that Consume will eventually support Reconsume (when required)
}
	res := FStream.TryRead({out}Result);
	if not res then
	begin
		Result := UEOF;
		Exit;
	end;
end;

function TDelphiTokenizer.GetNextToken(out AToken: TSyntaxToken): Boolean;
var
	ch: WideChar;
	tokenStartLine, tokenStartColumn: Integer;
begin
{
	Consume the next input character, and take action depending on what it is.

	The understanding is that each handler will then greedily munch (Consume) the input
	and return the corresponding token.

	GetNextToken will return true every time a token is returned.
	When EOF is reached, a ptEOF token will be returned along with True.
	Subsequent calls to GetNextToken after the EOF will return False and nil.
}
	if FNextToken <> nil then
	begin
		AToken := FNextToken;
		FNextToken := nil;
		if AToken.TokenKind = ptEof then
			FEofEmitted := True;
		Result := True;
		Exit;
	end;

	AToken := nil;
	tokenStartLine := FCurrentLine;
	tokenStartColumn := FCurrentColumn;

	// Consume the next character, which advances the input string one character,
	// updates the FCurrentInputCharacter member, and returns the character.
	ch := Consume;

	// If the character is null it is an error. That should not have happened.
	if ch = #0 then
	begin
		Log('The consume should never return null; how?');
		Result := False;
		Exit;
	end;

	// If we're done, then produce the EOF sentinel exactly once.
	if ch = UEOF then
	begin
		if FEofEmitted then
		begin
			Result := False;
			Exit;
		end;

		AToken := TSyntaxToken.Create(ptEof, tokenStartLine, tokenStartColumn, '');
		if not FPeeking then
			FEofEmitted := True;
		Result := True;
		Exit;
	end;

	// Special symbol (single character and multiple-character) symbols
	if ch = '@' then
	begin
		if Peek = '@' then
			AToken := DoDoubleAddressOp(ch)	// ptDoubleAdderssOp    @@
		else
			AToken := DoAddressOp(ch)			//	ptAddressOp            @
	end
	else if ch = '.' then
	begin
		if Peek = '.' then
			AToken := DoDotDot(ch)				// ptDotDot					..
		else if Peek = ')' then
			AToken := DoRoundDotClose(ch)		// ptRoundDotClose		.)
		else
			AToken := DoPoint(ch);				// ptPoint					.
	end
	else if ch = ',' then
		AToken := DoComma(ch)					// ptComma					,
	else if ch = ':' then
	begin
		if Peek = '=' then
			AToken := DoAssign(ch)				// ptAssign			:=
		else
			AToken := DoColon(ch)				// ptColon					:
	end
	else if ch = ';' then
		AToken := DoSemicolon(ch)				// ptSemicolon				;
	else if ch = '(' then
	begin
		if Peek = '*' then
		begin
			if Peek(2) = '$' then
				AToken := DoCompilerDirective(ch)
			else
				AToken := DoAnsiComment(ch)		//								(* ... *)
		end
		else if Peek = '.' then
			AToken := DoRoundDotOpen(ch)		// ptRoundDotOpen			(.
		else
			AToken := DoOpenParen(ch);			// ptOpenParen				(
	end
	else if ch = ')' then
		AToken := DoCloseParen(ch)				// ptCloseParen			)
	else if ch = '[' then
		AToken := DoOpenBracket(ch)			// ptOpenBrakcet			[
	else if ch = ']' then
		AToken := DoCloseBracket(ch)			// ptCloseBracket			]
	else if ch = '{' then
	begin
		if Peek = '$' then
			AToken := DoCompilerDirective(ch)	// ptCompDirect			{$xxx xx}
		else
			AToken := DoBorComment(ch)			// ptBorComment			{   }
	end
	else if ch = '/' then
	begin
		if Peek = '/' then
			AToken := DoSlashesComment(ch)	// ptSlashesComment		//
		else
			AToken := DoSlash(ch);				// ptSlash					/
	end
	else if ch =   '<' then
	begin
		if Peek = '=' then
			AToken := DoLessThanEquals(ch)		// ptLessThanEquals		<=
		else if Peek = '>' then
			AToken := DoNotEqual(ch)				// ptNotEqual				<>
		else
			AToken := DoLessThan(ch);				// ptLessThan				<
	end
	else if ch =   '>' then
	begin
		if Peek = '=' then
			AToken := DoGreaterThanEquals(ch)	// ptGreaterThanEquals	>=
		else
			AToken := DoGreater(ch);				// ptGreater				>
	end
	else if ch = '^' then
		AToken := DoCaret(ch)						// ptCaret					^
	else if ch = '*' then
		AToken := DoAsterisk(ch)					// ptAsterisk				*
	else if ch = '&' then
		AToken := DoAmpersand(ch)					// ptAmpersand or ptIdentifier (escaped keyword)
	else if ch = '+' then
		AToken := DoPlus(ch)							// ntPlus					+
	else if ch = '-' then
		AToken := DoMinus(ch)						// ptMinus					-
	else if ch = '=' then
		AToken := DoEqual(ch)						// ptEquals					=
	else if ch = '''' then							// ptStringConst			'   '
	begin
		// Check if this is a multiline string literal (''' or more)
		if (Peek = '''') and (PeekNext = '''') then
			AToken := DoMultilineStringLiteral(ch)	// ptMultilineStringLiteral	'''...'''
		else
			AToken := DoStringLiteral(ch);			// ptStringLiteral			'...'
	end

	else if IsStartOfNumeralCharacter(ch) then
		AToken := DoNumber(ch)						//								0..9, +, -, E
	else if ch = '$' then
		AToken := DoHexNumber(ch)					// ptInteger
	else if ch = '%' then
		AToken := DoBinaryNumber(ch)				// ptIntegerConst

	else if ch = '#' then
		AToken := DoAsciiChar(ch)					// ptAsciiChar				 #149, #$4E

	else if IsStartOfIdentifierCharacter(ch) then
		AToken := DoReadIdentifier(ch)			// ptIdentifier			A..Z, a..z, 0..9, _

	else if IsWhitespaceCharacter(ch) then
		AToken := DoWhitespace(ch)					// ptWhitespace
	else
	begin
		AToken := TSyntaxToken.Create(ptUnknown, FCurrentLine, FCurrentColumn, ch);
		if IsDebuggerPresent then
			raise ENotImplemented.CreateFmt('[TDelphiTokenizer.NextToken] Unknown character type U+%.4d (%s)', [Word(ch), ch]);
	end;

	if AToken = nil then
		raise EParserError.Create('No token was returned');

	AToken.Line   := tokenStartLine;
	AToken.Column := tokenStartColumn;

	Result := True;
end;

function TDelphiTokenizer.GetPosXY: TPoint;
begin
	Result := Point(FCurrentLine, FCurrentColumn);
end;

function TDelphiTokenizer.Peek(NumberOfCharactersAhead: Integer=1): Char;
begin
{
	Peek at the next character.
	The character will not be consumed, and will be returned the next time Consume is called.
}
	Result := FStream.Peek(NumberOfCharactersAhead);
end;

function TDelphiTokenizer.PeekNext: Char;
begin
{
	Peek 2 characters ahead.
	The character will not be consumed, and will be returned on the future call to Consume.

	DONE: Maybe change this to Peek(2), and replace PeekNext with Peek(2), and Peek with Peek(Offset: Integer=1)
}
	Result := Peek(2);
end;

function TDelphiTokenizer.PeekTokenKind: TptTokenKind;
var
	tok: TSyntaxToken;
begin
	// If we already have a buffered token, return it.
	if FNextToken <> nil then
	begin
		Result := FNextToken.TokenKind;
		Exit;
	end;

	// Otherwise try to read the next token.
	FPeeking := True;
	try
		if GetNextToken({out}tok) then
		begin
			FNextToken := tok; // buffer it for future consumption
			Result := FNextToken.TokenKind;
			Exit;
		end;
	finally
		FPeeking := False;
	end;

	// No more consumable tokens.
	Result := ptEof;
end;

procedure TDelphiTokenizer.SelfTest;
var
	x: Integer;
	s: string;
begin
	x := 7 mod %10;

	// Max line lenth of 2260 characters; even though technically strings can be unlimited length
	s :=
			'123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|hundred->|'+
			'123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|hundred->|'+
			'123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|hundred->|';

	if x = 3 then
		s := '''
			   one line'+CRLF+
			   two line'+CRLF+
			   ''';

	LogFmt('%d', [x]);
	// 2400 / 4 = 600
	try
//		BuiltInLn(x);
//		ln(x);
	except
	end;
end;

class procedure TDelphiTokenizer.Tokenize(const SourceCode: string; const TargetList: TList);
var
	tokenizer: TDelphiTokenizer;
	token: TSyntaxToken;
	t1: Int64;
begin
{
	Tokenize the supplied code, and add the tokens to Targetlist.

	Ideally you would call NextToken, and parse the tokens as you get them
	(since we're trying to not load multiple copies of everything into our 2B virtual address space).
}
	t1 := Sqm.GetTimestamp;
	tokenizer := TDelphiTokenizer.Create(SourceCode);
	try
		while tokenizer.NextToken({out}token) do
			TargetList.Add(token);
	finally
		tokenizer.Free;
	end;
	Sqm.TimerStop('TDelphiTokenizer/Tokenize', t1);
end;

function TDelphiTokenizer.IsHexCharacter(const ch: WideChar): Boolean;
begin
	// Delphi Something allows _ in numbers; include hexadecimal;
	// 0..9, A..F, a..f, _
	Result := CharInSet(ch, ['0'..'9', 'A'..'F', 'a'..'f', '_']);

	// TODO: Investigate possible perf gain by using a lookup table instead.
end;

function TDelphiTokenizer.IsBinaryCharacter(const ch: WideChar): Boolean;
begin
	// Delphi Something allows _ in numbers; include binary
	// 0, 1, _
	Result := CharInSet(ch, ['0', '1', '_']);

	// TODO: Investigate possible perf gain by using a lookup table instead.
end;

function TDelphiTokenizer.IsIdentifierCharacter(const ch: WideChar): Boolean;
begin
	// Identifiers include letters, digits, and underscores.
	Result := ch.IsLetter or CharInSet(ch, ['0'..'9']) or (ch = '_');
end;


function TDelphiTokenizer.IsStartOfIdentifierCharacter(const ch: WideChar): Boolean;
begin
	// Identifiers start with a letter or underscore.
	// Identifiers are used to name variables, functions, and other entities.
	Result :=
			ch.IsLetter
			or (ch = '_');
end;

function TDelphiTokenizer.IsStartOfNumeralCharacter(const ch: WideChar): Boolean;
begin
	case ch of
	'0'..'9': Result := True;
	else
		Result := False;
	end;
end;

function TDelphiTokenizer.IsWhitespaceCharacter(const ch: WideChar): Boolean;
begin
	case ch of
	#13, #10: Result := True;
   ' ': Result := True;       // Space
	'	': Result := True;		// Tab
   else
		Result := False;
	end;
end;

{ TInputStream }

function TInputStream.Consume: WideChar;
begin
{
	Extract the next character from our (possibly buffered) stream.
}
	//Get the next character from the buffer
	if FBufferSize > 0 then
	begin
		// Return current buffered char
		Result := FBuffer[FBufferPosition];
		// Advance position linearly; do not wrap during consume
		Inc(FBufferPosition);
		Dec(FBufferSize);
		// Once the buffer is empty, reset position to start
		if FBufferSize = 0 then
			FBufferPosition := 1;
		Exit;
	end;

	Result := GetNextCharacterFromStream;
end;

constructor TInputStream.Create(ByteStream: ISequentialStream; Encoding: Word);
const
	BUFFER_SIZE = 1024;
begin
	inherited Create;

	FStream := ByteStream;
	FEncoding := Encoding;
	FEOF := False;

	//FBuffer is a ring-buffer, with the current position at "FBufferPosition"
	//and there is FBufferSize valid characters
	SetLength(FBuffer, BUFFER_SIZE);
	FBufferSize := 0;
	// UnicodeString uses 1-based indexing; start at index 1
	FBufferPosition := 1;

	TConstraints.Same(0, FBufferSize, 'FBufferSize must be zero');
end;

function TInputStream.FetchNextCharacterInfoBuffer: Boolean;
var
	ch: WideChar;
	n: Integer;
begin
	ch := Self.GetNextCharacterFromStream;
	if ch = UEOF then
	begin
		Result := False;
		Exit;
	end;

	// Compute write index (end of current valid window), linear (no wrap)
	n := FBufferPosition + FBufferSize;
	if n > Length(FBuffer) then
		raise Exception.CreateFmt('[TInputStream.FetchNextCharacterintoBuffer] Exceeded buffer capacity while peeking '+
				'(CurrentPosition:%d, BufferSize:%d)',
				[FBufferPosition, FBufferSize]);
	FBuffer[n] := ch;
	Inc(FBufferSize);
	Result := True;
end;

function TInputStream.GetNextCharacterFromStream: WideChar;
begin
	case FEncoding of
	CP_UTF16: Result := Self.GetNextCharacterFromStreamUtf16
	else
		raise Exception.Create('Unknown encoding');
	end;
end;

function TInputStream.GetNextCharacterFromStreamUTF16: WideChar;
var
  hr: HRESULT;
  cbRead: FixedUInt;
begin
{
	Get the next UTF-16 character from the stream.
}
  hr := FStream.Read(@Result, SizeOf(WideChar), @cbRead);
  // S_FALSE is expected at EOF (fewer than requested bytes)
  if (hr <> S_OK) and (hr <> S_FALSE) then
    OleCheck(hr);
  if cbRead < SizeOf(WideChar) then
  begin
    Result := UEOF;
    // Do not set FEOF here; only when the consumer actually consumes UEOF
    Exit;
  end;

  // It's perfectly reasonable to have U+0000 in the stream. (i think)
//	if Result = #0 then
//	begin
//		raise Exception.Create('How did we get a U+0000 from the stream?');
//		Result := UEOF;
//	end;
end;

procedure TInputStream.LogFmt(const s: string; const Args: array of const);
begin

end;

function TInputStream.Peek(k: Integer): WideChar;
begin
	// Validate peek count
	TConstraints.Positive(k, 'Peek count must be positive');
{
	Return the k-th character (e.g. 1st, 2nd, 3rd) without popping it.
	Returns UEOF if there are no more characters to read.
}

{
	Ensure the buffer has at least k characters
	If k<n, that means k-th character is in the buffer.
	Then return the k-th character from the buffer.
}
	while k > FBufferSize do
	begin
		//If k > n, that means k-th character is not in the buffer.
		//Read up to the k-th character and add it to the buffer.
		//Since already n characters are in the buffer,
		//total k-n number of characters will be read.
		//Then return the k-th character from the buffer.
		if not FetchNextCharacterInfoBuffer then
		begin
			Result := UEOF;
			Exit;
		end;
	end;

	// Return k-th upcoming character using linear indexing (no wrap)
	Result := FBuffer[FBufferPosition + (k - 1)];

	LogFmt('    Peek: [%s]', []);
end;

function TInputStream.TryRead(out ch: WideChar): Boolean;
begin
{
	Read the next unicode character from the input stream.
}
	ch := Consume;

	if ch = UEOF then
	begin
		Result := False;
		FEOF := True;
		Exit;
	end;

	Result := True;
end;

{ TSyntaxToken }

constructor TSyntaxToken.Create(ATokenKind: TptTokenKind; const Line, Column: Integer; const Text: string);
begin
	inherited Create;

	FLeadingTrivia  := TObjectList<TSyntaxToken>.Create(True);
	FTrailingTrivia := TObjectList<TSyntaxToken>.Create(True);

	Self.TokenKind := ATokenKind;
	Self.Line := Line;
	Self.Column := Column;
	Self.Text := Text;
	Self.ValueText := Text;
	
	// Initialize error tracking properties
	Self.HasErrors := False;
	Self.HasWarnings := False;
	Self.ErrorMessage := '';
	Self.WarningMessage := '';
	Self.IsMissing := False;
	Self.DirectiveID := ptUnknown;
end;

destructor TSyntaxToken.Destroy;
begin
	FreeAndNil(FLeadingTrivia);
	FreeAndNil(FTrailingTrivia);

{$IFDEF DEBUG}
	// Help debug use-after-free
	TokenKind := High(TptTokenKind);
	Line := -1;
	Column := -1;
	StartOffset := -1;
	TokenLength := -1;
	Text := '<destroyed>';
	ValueText := '<destroyed>';
	IsMissing := True;
	HasErrors := True;
	ErrorMessage := 'TSyntaxToken used after Destroy';
{$ENDIF}


	inherited;
end;

var
	_eofToken: TSyntaxToken = nil;

class function TSyntaxToken.Eof: TSyntaxToken;
begin
	// singleton eof token
	if _eofToken = nil then
		_eofToken := TSyntaxToken.Create(ptEof, -1, -1, '');

	Result := _eofToken;
end;

function TSyntaxToken.get_LeadingTriviaCount: Integer;
begin
	Result := FLeadingTrivia.Count;
end;

function TSyntaxToken.get_GenID: TptTokenKind;
begin
{
	If the identifier could also be a directive,
	then GenID returns the directive TokenKind.

	Otherwise it just returns identifier.
}
	Result := Self.TokenKind;
	if Result = ptIdentifier then
		if DirectiveID <> ptUnknown then
			Result := DirectiveID;
end;

function TSyntaxToken.get_LeadingTrivia(I: Integer): TSyntaxToken;
begin
	Result := FLeadingTrivia[I];
end;

function TSyntaxToken.get_TrailingTriviaCount: Integer;
begin
	Result := FTrailingTrivia.Count;
end;

function TSyntaxToken.get_TrailingTrivia(I: Integer): TSyntaxToken;
begin
   Result := FTrailingTrivia[I];
end;

function TSyntaxToken.ToString: string;
var
	statusInfo: string;
begin
	statusInfo := '';
	if HasErrors then
		statusInfo := statusInfo + ' [ERROR: ' + ErrorMessage + ']';
	if HasWarnings then
		statusInfo := statusInfo + ' [WARNING: ' + WarningMessage + ']';
	if IsMissing then
		statusInfo := statusInfo + ' [MISSING]';
		
	Result := Format('Token: %s (%d, %d) "%s"%s', [TokenKindToStr(TokenKind), Line, Column, Text, statusInfo]);
end;

initialization

finalization
	FreeAndNil(_eofToken);

end.
