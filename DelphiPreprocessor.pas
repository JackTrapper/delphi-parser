unit DelphiPreprocessor;

(*
Conditional-compilation preprocessor.

	parser  <--  preprocessor  <--  tokenizer

Pull model: the preprocessor wraps a TDelphiTokenizer and exposes the same interface:

	NextToken(out AToken): Boolean

When the parser asks for a token, the preprocessor pulls from the	tokenizer,
evaluates conditional directives, and either returns the token	or swallows it
(demoting it to trivia on the next real token).

- Directive tokens always become trivia (leading trivia of the next real token).
- Tokens inside a false branch are merged into a single ptDisabledText trivia
		token containing the raw concatenated text of the skipped region.
- Tokens inside a true branch pass through as normal real tokens.
- $DEFINE X / $UNDEF X modify the active define set when in an active branch.

Conditional defines
-------------------

The Paser initializes the preprocessor with a set of conditional defines
using the TPreprocessor.Defines property.

C# example
----------

https://sharplab.io/#v2:C4LghgzsA0AmIGoA+ABATARgLACgUGYACdQgYUIG9dCbiiUAWQgWQAoBKS62ngYgEsAZoUFgANhACm3HrML8AdsHmEAvITQBuGbN6SFsIToC+uY0A===

```cs
using System;
public class C {
	public void M() {
		#if false
			int i = 2;
		#endif
	}
}
```

- CompilationUnit
   + UsingDirective
   - ClassDeclaration
      + PublicKeyword
      + Keyword: ClassKeyword
      + Identifier: IdentifierToken
      + OpenBraceToken: OpenBraceToken
      - MethodDeclaration
         o Operation: MethodBodyOperation
         + PublicKeyword
         + ReturnType: PredifinedType
         - Identifier: M  IdentifierToken
         - ParameterBodyList: ParameterList
         - Body: Block
            - Operation: Block
               - Locals: <skipped>
            - OpenBraceToken: OpenBraceToken
            	- {
               - \r\n   EndOfLineTrivia
				- CloseBraceToken: CloseBraceToken
					- <space:8>WhitespaceTrivia
               - IfDirectiveTrivia
						- Structure: IfDirectiveTrivia
							- HashToken: #	HashToken
							- IfKeyword: IfKeyword
                        - if
                        - <sapce>	WhitespaceTrivia
                     - Condition: FalseLiteralExpression
                        - Token: false		FalseKeyword
               		- EndOfDirectiveToken: EndOfDirectiveToken
								-
								- \r\n	EndOfLineTrivia
                  - <space:12>int i = 2;\r\n DisabledTextTrivia
                  - <space:8>	WhitespaceTrivia
               - EndIfDirectiveTrivia
                  - Structure: EndIfDirectiveTrivia
							- HashToken: #	HashToken
							- EndIfKeyword: endif	EndIfKeyword
                     - EndOfDirectiveToken: EndOfDirectiveToken
                  - <space:4>	WhitespaceTrivia
                  - }
                  - \r\n	EndOfLineTrivia
      - CloseBraceToken: }	CloseBraceToken
   - EndOfFileToken: EndOfFileToken


Design Notes
============

I realized that the tokenizer **has** to run *during* the lexing process.
When you reach a conditional define that isn't enabled:

	{$IFDEF FrobbingTheGrobber}
   Since FrobbingTheGrobber is not defined, this text can be anything.
   Even var invalid := code asm that begin doesn't procedure (; mean
	anything useful out and isn't valid text.
	{$ENDIF}

The issue is that we cannot fail on anything that doesn't tokenizer correctly;
which means the tokenizer has to know how to just treat everything inside the define
as trivial.

Unless, of course, we are certain there is never any situation, or combination of
characters, that could trip up the tokenzier? And then we can just concatenate all the .Text
into a long piece of trivia text?

But certainly the preferred way is for the tokenizer to know to just skip ahead
to the closing {$ENDIF} and collect all that into a disabled trivia token?


*)

interface

uses
	SysUtils,
	Classes,
	Contnrs,
	System.Generics.Collections,
	DelphiTokenizer;

type
	TDirectiveKind = (
		dkUnknown,								// sentinal value

		// Conditional Compilation
		dkDefine, 								// $DEFINE directive
		dkUnDef,									// $UNDEF directive
		dkIfDef,									// $IFDEF name
		dkIfNDef,								// $IFNDEF name
		dkElseIf,								// $ELSEIF
		dkElse,									// $ELSE
		dkEndIf,									// $ENDIF
		dkIf,										// $IF expression
		dkIfEnd,									// $IFEND								$IF..$IFEND (although $ENDIF is allowed since XE4)
		dkIfOpt,									// $IFOPT switch
		dkLegagyIfEnd,							// $LEGACYIFEND OFF					Require $IFEND to close $IF statements (pre-XE4)

		// Switches
		dkDebugInfo,							// [GLOBAL] $D+,$DEBUGINFO ON, $D-, $DEBUGINFO OFF
		dkObjExportAll,						// [GLOBAL] $ObjExportAll Off
		dkExtendedSytnax,						// [GLOBAL] $X+ , $EXTENDEDSYNTAX ON
		dkImplicitBuild,						// [GLOBAL] $IMPLICITBUILD ON
		dkLibPrefix,							// [GLOBAL] $LIBPREFIX
		dkLibSuffix,							// [GLOBAL] $LIBSUFFIX
		dkLibVersion,							// [GLOBAL] $LIBVERSION
		dkLocalSymbols,						// [GLOBAL] $L+, $LOCALSYMBOLS ON
		dkSetPEOSVersion,						// [GLOBAL] $SETPEOSVERSION
		dkStrongLinkTypes,					// [GLOBAL] $STRONGLINKTYPES OFF
		dkReferenceInfo,						// [GLOBAL] $YD, $REFERENCEINFO ON, DEFINITIONINFO ON
		dkTypeAddress,							// [GLOBAL] $T-, $TYPEDADDRESS OFF
		dkAlign,									// [LOCAL ] $A8, $ALIGN 8
		dkAssertions,							// [LOCAL ] $C+, $ASSERTIONS ON
		dkBoolEval,								// [LOCAL ] $B-, $BOOLEVAL OFF
		dkCodeAlign,							// [LOCAL ] $CODEALIGN n
		dkDenyPackageUnit,					// [LOCAL ] $DENYPACKAGEUNIT OFF
		dkDesignOnly,							// [LOCAL ] $DESIGNONLY OFF
		dkExtendedCompatibility,			// [LOCAL ] $EXTENDEDCOMPATIBILITY OFF
		dkExcessPrecision,					// [LOCAL ] $EXCESSPRECISION ON
		dkHighCharUnicode,					// [LOCAL ] $HIGHCHARUNICODE OFF
		dkHints,									// [LOCAL ] {$HINTS ON
		dkImportedData,						// [LOCAL ] $G+, $IMPORTEDDATA ON
		dkIOChecks,								// [LOCAL ] $I+, $IOCHECKS ON
		dkLongStrings,							// [LOCAL ] $H+, $LONGSTRINGS ON
		dkMethodInfo,							// [LOCAL ] $METHODINFO OFF
		dkOpenStrings,							// [LOCAL ] $P+,$OPENSTRINGS ON
		dkOptimization,						// [LOCAL ] $O+, $OPTIMIZATION ON
		dkOverflowChecks,						// [LOCAL ] $Q-, $OVERFLOWCHECKS OFF
		dkSafeDivide,							// [LOCAL ] $U-, $SAFEDIVIDE OFF
		dkPointerMath,							// [LOCAL ] $POINTERMATH OFF
		dkRangeChecks,							// [LOCAL ] $R-, $RANGECHECKS OFF
		dkRealCompatibility,					// [LOCAL ] $REALCOMPATIBILITY OFF
		dkRunOnly,								// [LOCAL ] $RUNONLY OFF
		dkTypeInfo,								// [LOCAL ] $M-,$TYPEINFO OFF
		dkScopedEnums,							// [LOCAL ] $SCOPEDENUMS OFF
		dkStackFrames,							// [LOCAL ] $W-, $STACKFRAMES OFF
		dkVarStringChecks,					// [LOCAL ] $V+, $VARSTRINGCHECKS ON
		dkWarn,									// [LOCAL ] $WARN identifier ON | OFF | ERROR | DEFAULT
		dkMessage,								// [      ] $MESSAGE HINT|WARN|ERROR|FATAL 'text string'
		dkWarnings,								// [LOCAL ] $WARNINGS ON
		dkWeakPackageUnit,					// [LOCAL ] $WEAKPACKAGEUNIT OFF
		dkWeakLinkRtti,						// [LOCAL ] $WEAKLINKRTTI OFF
		dkWriteableConst,						// [LOCAL ] $J-, $WRITEABLECONST OFF
		dkZeroBasedStrings,					// [LOCAL ] $ZEROBASEDSTRINGS OFF

		// Parameters
		dkAppType,								// [GLOBAL] $APPTYPE GUI
		dkDescription,							// [GLOBAL] $D, $DESCRIPTION 'text'
		dkImageBase,							// [GLOBAL] $IMAGEBASE number
		dkMemoryAllocation,					// [GLOBAL] $M minstacksize,maxstacksize,$MINSTACKSIZE number, $MAXSTACKSIZE number
		dkObjTypeName,							// [GLOBAL] $OBJTYPENAME typeIdent ['{B|N}typeNameInObj']
		dkResourceReserve,					// [GLOBAL] $RESOURCERESERVE reservedbytes
		dkInclude,								// [LOCAL ] $I filename, $INCLUDE filename
		dkLink,									// [LOCAL ] $L filename, $LINK filename
		dkMinEnumSize,							// [LOCAL ] $Z1, $Z2, $Z4, $MINENUMSIZE 1, $MINENUMSIZE 2, $MINENUMSIZE 4
		dkResource,								// [LOCAL ] $R filename, $RESOURCE filename, $R *.xxx, $R filename.res filename.rc
		dkExtension,							// [      ] $E exe, $EXTENSION exe
		dkExternalSym,							// [      ] $EXTERNALSYM [ 'typeNameInHpp' [ 'typeNameInHppUnion' ]]
		dkHppEmit,								// [      ] $HPPEMIT 'string'
		dkNoDefine,								// [      ] $NODEFINE [ 'typeNameInHpp' [ 'typeNameInHppUnion' ]]
		dkNoInclude,							// [      ] $NOINCLUDE
		dkRegion,								// [      ] $REGION '<region description>'
		dkEndRegion,							// [      ] $ENDREGION

		// Flags
		dkSetPEFlags,							// [LOCAL ] $SetPEFlags,
		dkSetPEOptFlags,						// [LOCAL ] $SetPEOptFlags
		dkSetPESubsystemVersion,			// [LOCAL ] $SETPESUBSYSVERSION
		dkSetPeUserVerison,					// [LOCAL ] $SETPEUSERVERSION <major>.<minor>

		// Expression
		dkRtti,									// [LOCAL ] $RTTI INHERIT|EXPLICIT [visibility clause]=

		dkOldTypeLayout						// [      ] $OLDTYPELAYOUT ON
	);

	TDelphiPreprocessor = class
	private
		FTokenizer: TDelphiTokenizer;		// reference to the tokenzier being used; we do not own it, we do not free it

		FConditionalDefines: TStringList;			//e.g. "Strict", "UnitTests"
		FSwitches: TDictionary<string, string>;	//e.g. [ALIGN, 8], [MINENUMSIZE, 1]
	protected
		function IsActive: Boolean;
		procedure ParseDirective(Token: TSyntaxToken; out DirectiveKind: TDirectiveKind; out Arg: string);
	public
		constructor Create(ATokenizer: TDelphiTokenizer; const Defines: TStrings);
		destructor Destroy; override;

		{	Pull-model token source. Returns the next token from the underlying tokenizer,
			applying conditional-compilation filtering.

			Returns True and sets AToken when a token is available (including EOF).
			Returns False when the token stream is exhausted (after EOF has been returned).

			For now this is a pass-through: delegates directly to the tokenizer. }
		function NextToken(out AToken: TSyntaxToken): Boolean;

		property ConditinalDefines: TStringList read FDefines;
	end;

implementation

const
	KnownCompilerDirectives: array[0..78] of record Name: string; DirectiveKind: TDirectiveKind; end = (
		// Conditional Compilation
		(Name: 'Define';						DirectiveKind: dkDefine),			// $DEFINE directive
		(Name: 'UnDef';						DirectiveKind: dkUnDef),			// $UNDEF directive
		(Name: 'IfDef';						DirectiveKind: dkIfDef),			// $IFDEF name
		(Name: 'IfNDef';						DirectiveKind: dkIfNDef),			// $IFNDEF name
		(Name: 'ElseIf';						DirectiveKind: dkElseIf),			// $ELSEIF
		(Name: 'Else';							DirectiveKind: dkElse),				// $ELSE
		(Name: 'EndIf';						DirectiveKind: dkEndIf),			// $ENDIF
		(Name: 'If';							DirectiveKind: dkIf),				// $IF expression
		(Name: 'IfEnd';						DirectiveKind: dkIfEnd),			// $IFEND
		(Name: 'IfOpt';						DirectiveKind: dkIfOpt),			// $IFOPT switch
		(Name: 'LegacyIfEnd';				DirectiveKind: dkLegagyIfEnd),	// $LEGACYIFEND OFF

		// Switches
		(Name: 'DebugInfo';					DirectiveKind: dkDebugInfo),					// [GLOBAL] $D+,$DEBUGINFO ON
		(Name: 'ObjExportAll';				DirectiveKind: dkObjExportAll),				// [GLOBAL] $ObjExportAll Off
		(Name: 'ExtendedSyntax';			DirectiveKind: dkExtendedSytnax),			// [GLOBAL] $X+ , $EXTENDEDSYNTAX ON
		(Name: 'ImplicitBuild';				DirectiveKind: dkImplicitBuild),				// [GLOBAL] $IMPLICITBUILD ON
		(Name: 'LibPrefix';					DirectiveKind: dkLibPrefix),					// [GLOBAL] $LIBPREFIX
		(Name: 'LibSuffix';					DirectiveKind: dkLibSuffix),					// [GLOBAL] $LIBSUFFIX
		(Name: 'LibVersion';					DirectiveKind: dkLibVersion),					// [GLOBAL] $LIBVERSION
		(Name: 'LocalSymbols';				DirectiveKind: dkLocalSymbols),				// [GLOBAL] $L+, $LOCALSYMBOLS ON
		(Name: 'StrongLinkTypes';			DirectiveKind: dkStrongLinkTypes),			// [GLOBAL] $STRONGLINKTYPES OFF
		(Name: 'ReferenceInfo';				DirectiveKind: dkReferenceInfo),				// [GLOBAL] $YD, $REFERENCEINFO ON, DEFINITIONINFO ON
		(Name: 'TypedAddress';				DirectiveKind: dkTypeAddress),				// [GLOBAL] $T-, $TYPEDADDRESS OFF

		(Name: 'Align';						DirectiveKind: dkAlign),						// [LOCAL ] $A8, $ALIGN 8
		(Name: 'Assertions';					DirectiveKind: dkAssertions),					// [LOCAL ] $C+, $ASSERTIONS ON
		(Name: 'BoolEval';					DirectiveKind: dkBoolEval),					// [LOCAL ] $B-, $BOOLEVAL OFF
		(Name: 'CodeAlign';					DirectiveKind: dkCodeAlign),					// [LOCAL ] $CODEALIGN n
		(Name: 'DenyPackageUnit';			DirectiveKind: dkDenyPackageUnit),			// [LOCAL ] $DENYPACKAGEUNIT OFF
		(Name: 'DesignOnly';					DirectiveKind: dkDesignOnly),					// [LOCAL ] $DESIGNONLY OFF
		(Name: 'ExtendedCompatibility';	DirectiveKind: dkExtendedCompatibility),	// [LOCAL ] $EXTENDEDCOMPATIBILITY OFF
		(Name: 'ExcessPrecision';			DirectiveKind: dkExcessPrecision),			// [LOCAL ] $EXCESSPRECISION ON
		(Name: 'HighCharUnicode';			DirectiveKind: dkHighCharUnicode),			// [LOCAL ] $HIGHCHARUNICODE OFF
		(Name: 'Hints';						DirectiveKind: dkHints),						// [LOCAL ] {$HINTS ON
		(Name: 'ImportedData';				DirectiveKind: dkImportedData),				// [LOCAL ] $G+, $IMPORTEDDATA ON
		(Name: 'IOChecks';					DirectiveKind: dkIOChecks),					// [LOCAL ] $I+, $IOCHECKS ON
		(Name: 'LongStrings';				DirectiveKind: dkLongStrings),				// [LOCAL ] $H+, $LONGSTRINGS ON
		(Name: 'MethodInfo';					DirectiveKind: dkMethodInfo),					// [LOCAL ] $METHODINFO OFF
		(Name: 'OldTypeLayout';				DirectiveKind: dkOldTypeLayout),				// [LOCAL ] $OLDTYPELAYOUT ON
		(Name: 'OpenStrings';				DirectiveKind: dkOpenStrings),				// [LOCAL ] $P+,$OPENSTRINGS ON
		(Name: 'Optimization';				DirectiveKind: dkOptimization),				// [LOCAL ] $O+, $OPTIMIZATION ON
		(Name: 'OverflowChecks';			DirectiveKind: dkOverflowChecks),			// [LOCAL ] $Q-, $OVERFLOWCHECKS OFF
		(Name: 'SafeDivide';					DirectiveKind: dkSafeDivide),					// [LOCAL ] $U-, $SAFEDIVIDE OFF
		(Name: 'SetPEOSVersion';			DirectiveKind: dkSetPEOSVersion),			// [LOCAL ] $SETPEOSVERSION
		(Name: 'PointerMath';				DirectiveKind: dkPointerMath),				// [LOCAL ] $POINTERMATH OFF
		(Name: 'RangeChecks';				DirectiveKind: dkRangeChecks),				// [LOCAL ] $R-, $RANGECHECKS OFF
		(Name: 'RealCompatibility';		DirectiveKind: dkRealCompatibility),		// [LOCAL ] $REALCOMPATIBILITY OFF
		(Name: 'RunOnly';						DirectiveKind: dkRunOnly),						// [LOCAL ] $RUNONLY OFF
		(Name: 'TypeInfo';					DirectiveKind: dkTypeInfo),					// [LOCAL ] $M-,$TYPEINFO OFF
		(Name: 'ScopedEnums';				DirectiveKind: dkScopedEnums),				// [LOCAL ] $SCOPEDENUMS OFF
		(Name: 'StackFrames';				DirectiveKind: dkStackFrames),				// [LOCAL ] $W-, $STACKFRAMES OFF
		(Name: 'VarStringChecks';			DirectiveKind: dkVarStringChecks),			// [LOCAL ] $V+, $VARSTRINGCHECKS ON
		(Name: 'Warn';							DirectiveKind: dkWarn),							// [LOCAL ] $WARN identifier ON | OFF | ERROR | DEFAULT
		(Name: 'Message';						DirectiveKind: dkMessage),						// [LOCAL ] $MESSAGE HINT|WARN|ERROR|FATAL 'text string'
		(Name: 'Warnings';					DirectiveKind: dkWarnings),					// [LOCAL ] $WARNINGS ON
		(Name: 'WeakPackageUnit';			DirectiveKind: dkWeakPackageUnit),			// [LOCAL ] $WEAKPACKAGEUNIT OFF
		(Name: 'WeakLinkRtti';				DirectiveKind: dkWeakLinkRtti),				// [LOCAL ] $WEAKLINKRTTI OFF
		(Name: 'WriteableConst';			DirectiveKind: dkWriteableConst),			// [LOCAL ] $J-, $WRITEABLECONST OFF
		(Name: 'ZeroBasedStrings';			DirectiveKind: dkZeroBasedStrings),			// [LOCAL ] $ZEROBASEDSTRINGS OFF

		// Parameters
		(Name: 'AppType';						DirectiveKind: dkAppType),						// [GLOBAL] $APPTYPE GUI
		(Name: 'Description';				DirectiveKind: dkDescription),				// [GLOBAL] $D, $DESCRIPTION 'text'
		(Name: 'ImageBase';					DirectiveKind: dkImageBase),					// [GLOBAL] $IMAGEBASE number
		(Name: 'MemoryAllocation';			DirectiveKind: dkMemoryAllocation),			// [GLOBAL] $M minstacksize,maxstacksize,$MINSTACKSIZE number, $MAXSTACKSIZE number
		(Name: 'ObjTypeName';				DirectiveKind: dkObjTypeName),				// [GLOBAL] $OBJTYPENAME typeIdent ['{B|N}typeNameInObj']
		(Name: 'ResourceReserve';			DirectiveKind: dkResourceReserve),			// [GLOBAL] $RESOURCERESERVE reservedbytes
		(Name: 'Extension';					DirectiveKind: dkExtension),					// [GLOBAL] $E exe, $EXTENSION exe

		(Name: 'Include';						DirectiveKind: dkInclude),						// [LOCAL ] $I filename, $INCLUDE filename
		(Name: 'Link';							DirectiveKind: dkLink),							// [LOCAL ] $L filename, $LINK filename
		(Name: 'MinEnumSize';				DirectiveKind: dkMinEnumSize),				// [LOCAL ] $Z1, $Z2, $Z4, $MINENUMSIZE 1, $MINENUMSIZE 2, $MINENUMSIZE 4
		(Name: 'Resource';					DirectiveKind: dkResource),					// [LOCAL ] $R filename, $RESOURCE filename, $R *.xxx, $R filename.res filename.rc
		(Name: 'ExternalSym';				DirectiveKind: dkExternalSym),				// [LOCAL ] $EXTERNALSYM [ 'typeNameInHpp' [ 'typeNameInHppUnion' ]]
		(Name: 'HppEmit';						DirectiveKind: dkHppEmit),						// [LOCAL ] $HPPEMIT 'string'
		(Name: 'NoDefine';					DirectiveKind: dkNoDefine),					// [LOCAL ] $NODEFINE [ 'typeNameInHpp' [ 'typeNameInHppUnion' ]]
		(Name: 'NoInclude';					DirectiveKind: dkNoInclude),					// [LOCAL ] $NOINCLUDE
		(Name: 'Region';						DirectiveKind: dkRegion),						// [LOCAL ] $REGION '<region description>'
		(Name: 'EndRegion';					DirectiveKind: dkEndRegion),					// [LOCAL ] $ENDREGION

		// Flags
		(Name: 'SetPEFlags';					DirectiveKind: dkSetPEFlags),					// [LOCAL ] $SetPEFlags,
		(Name: 'SetPEOptFlags';				DirectiveKind: dkSetPEOptFlags),				// [LOCAL ] $SetPEOptFlags
		(Name: 'SetPESubsystemVersion';	DirectiveKind: dkSetPESubsystemVersion),	// [LOCAL ] $SETPESUBSYSVERSION
		(Name: 'SetPeUserVersion';			DirectiveKind: dkSetPeUserVerison),			// [LOCAL ] $SETPEUSERVERSION <major>.<minor>

		// Expression
		(Name: 'Rtti';							DirectiveKind: dkRtti)							// [LOCAL ] $RTTI INHERIT|EXPLICIT [visibility clause]=
	);



{ TDelphiPreprocessor }

constructor TDelphiPreprocessor.Create(ATokenizer: TDelphiTokenizer; const Defines: TStrings);
begin
	inherited Create;

	FTokenizer := ATokenizer; // borrowed reference; caller owns tokenizer lifetime

	FConditionalDefines := TStringList.Create;
	FConditionalDefines.CaseSensitive := False;
	FConditionalDefines.Sorted := True;
	FConditionalDefines.Duplicates := dupIgnore;
	if Assigned(Defines) then
		FConditionalDefines.Assign(Defines);

	FSwitches := TDictionary<string,string>.Create;
end;

destructor TDelphiPreprocessor.Destroy;
begin
	FreeAndNil(FStack);
	FreeAndNil(FDefines);
	// FTokenizer is NOT freed here: caller owns it
	inherited Destroy;
end;

function TDelphiPreprocessor.IsActive: Boolean;
var
	i: Integer;
begin
	// Active only when every frame on the stack is active
	Result := True;
	for i := 0 to FStack.Count-1 do
	begin
		if not FStack[i].Active then
		begin
			Result := False;
			Exit;
		end;
	end;
end;

function TDelphiPreprocessor.NextToken(out AToken: TSyntaxToken): Boolean;
var
	nextToken: TSyntaxToken;
	dk: TDirectiveKind;
	arg: string;
begin
{
	Pass-through implementation.

	TODO: Implement conditional compilation logic:
	  - Pull tokens from FTokenizer.NextToken
	  - When a conditional directive is encountered, evaluate it against FDefines/FStack
	  - Demote directive tokens and disabled content to trivia
	  - Return only active tokens to the caller

	For now, all tokens pass through from the tokenizer unmodified.
}
	Result := FTokenizer.NextToken({out}AToken);

	if Result and (AToken.Kind = ptCompilerDirective) then
	begin
		ParseDirective(AToken, {out}dk, {out}arg);
//		AToken.
	end;
end;

procedure TDelphiPreprocessor.ParseDirective(Token: TSyntaxToken; out DirectiveKind: TDirectiveKind; out Arg: string);
var
	s: string;
	nameStart, nameEnd: Integer;
	directiveName: string;
begin
{
Extract the directive name and argument from a ptCompilerDirective token.

	Input								Kind					ValueText
	=========================	================	===============
	$IFDEF UnitTests				dkIfDef				'UnitTests'
	$ENDIF							dkEndIf
	$DEFINE FOO						dkDefine				'FOO'
	$ALIGN ON						dkAlign				'ON'
	$A+								dkAlign				'ON'
	$ALIGN 2							dkAlign				'2'
	$A2								dkAlign				'2'
	$ASSERTIONS ON					dkAssertions		'ON'
	$C+								dkAssertions		'ON'
	$BOOLEVAL OFF					dkBooleanEval		'OFF'
	$B-								dkBooleanEval		'OFF'
	$EXTENDEDSYNTAX ON			dkExtendedSyntax	'ON'
	$X+								dkExtendedSyntax	'ON'
	$INCLUDE filename				dkInclude			filename
	$I filename						dkincldue			filename
	$REGION							dkRegion
	$REGION 'legacy stuff'		dkRegion				'legacy stuff'
	$ENDREGION						dkEndRegion
}
	DirectiveKind := dkUnknown;
	Arg := '';
	s := Token.Text;
	if Length(s) < 3 then
		Exit;

	// Find the '$' - should be at position 2 for '{$' or position 3 for '(*$'
	nameStart := Pos('$', s);
	if nameStart = 0 then
		Exit;
	Inc(nameStart); // skip the '$'

	// Read the directive name (letters and underscores)
	nameEnd := nameStart;
	while (nameEnd <= Length(s)) and CharInSet(s[nameEnd], ['A'..'Z', 'a'..'z', '_', '0'..'9']) do
		Inc(nameEnd);

	directiveName := UpperCase(Copy(s, nameStart, nameEnd - nameStart));

	// Extract argument (remainder after name, trimmed, excluding closing delimiter)
	Arg := Trim(Copy(s, nameEnd, Length(s) - nameEnd)); // strips trailing } or *)
	// Remove trailing delimiter characters
	if (Length(Arg) > 0) and (Arg[Length(Arg)] = '}') then
		Arg := Trim(Copy(Arg, 1, Length(Arg) - 1))
	else if (Length(Arg) >= 2) and (Arg[Length(Arg)-1] = '*') and (Arg[Length(Arg)] = ')') then
		Arg := Trim(Copy(Arg, 1, Length(Arg) - 2));

	// Map name to kind
	if directiveName = 'IFDEF' then
		DirectiveKind := dkIfDef
	else if directiveName = 'IFNDEF' then
		DirectiveKind := dkIfNDef
	else if directiveName = 'IF' then
		DirectiveKind := dkIf
	else if directiveName = 'ELSE' then
		DirectiveKind := dkElse
	else if directiveName = 'ELSEIF' then
		DirectiveKind := dkElseIf
	else if directiveName = 'ENDIF' then
		DirectiveKind := dkEndIf
	else if directiveName = 'IFEND' then
		DirectiveKind := dkIfEnd
	else if directiveName = 'DEFINE' then
		DirectiveKind := dkDefine
	else if directiveName = 'UNDEF' then
		DirectiveKind := dkUndef
	else if directiveName = 'IFOPT' then
		DirectiveKind := dkIfOpt
	else if directiveName = 'INCLUDE' then
		DirectiveKind := dkInclude
	else if directiveName = 'I' then
		DirectiveKind := dkInclude   // short form
	else if directiveName = 'REGION' then
		DirectiveKind := dkRegion
	else if directiveName = 'ENDREGION' then
		DirectiveKind := dkEndRegion
	else
		DirectiveKind := dkUnknown;
end;

end.

