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

The proprocessor then maintains an internal stack of conditional frames to track
as it encounters new $DEFINE, $UNDEF, and other conditional directives.

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




*)

interface

uses
	SysUtils,
	Classes,
	Contnrs,
	System.Generics.Collections,
	DelphiTokenizer;

type
	TConditionalFrame = record
		BranchTaken: Boolean;	// has any branch in this IFDEF/ELSE chain been active?
		Active: Boolean;			// is the current branch active?
	end;

	TDelphiPreprocessor = class
	private
		FTokenizer: TDelphiTokenizer;	// refernece to the tokenzier being used; we do not own it, we do not free it
		FDefines: TStringList;
		FStack: TList<TConditionalFrame>;
	protected
		function IsActive: Boolean;
		procedure ParseDirective(Token: TSyntaxToken; out Kind: TDirectiveKind; out Arg: string);
	public
		constructor Create(ATokenizer: TDelphiTokenizer; const Defines: TStrings);
		destructor Destroy; override;

		{	Pull-model token source. Returns the next token from the underlying
			tokenizer, applying conditional-compilation filtering.

			Returns True and sets AToken when a token is available (including EOF).
			Returns False when the token stream is exhausted (after EOF has been returned).

			For now this is a pass-through: delegates directly to the tokenizer. }
		function NextToken(out AToken: TSyntaxToken): Boolean;

		property Defines: TStringList read FDefines;
	end;

implementation

{ TDelphiPreprocessor }

constructor TDelphiPreprocessor.Create(ATokenizer: TDelphiTokenizer; const Defines: TStrings);
begin
	inherited Create;

	FTokenizer := ATokenizer; // borrowed reference; caller owns tokenizer lifetime

	FDefines := TStringList.Create;
	FDefines.CaseSensitive := False;
	FDefines.Sorted := True;
	FDefines.Duplicates := dupIgnore;
	if Assigned(Defines) then
		FDefines.Assign(Defines);

	FStack := TList<TConditionalFrame>.Create;
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
end;

procedure TDelphiPreprocessor.ParseDirective(Token: TSyntaxToken; out Kind: TDirectiveKind; out Arg: string);
var
	s: string;
	nameStart, nameEnd: Integer;
	directiveName: string;
begin
{
	Extract the directive name and argument from a ptCompilerDirective token.

	Input examples:
		'$IFDEF UnitTests'    -->  Kind=dkIfDef,  Arg='UnitTests'
		'$ENDIF'              -->  Kind=dkEndIf,  Arg=''
		'$DEFINE FOO'         -->  Kind=dkDefine, Arg='FOO'
		'(*$IFDEF BAR*)'      -->  Kind=dkIfDef,  Arg='BAR'
}
	Kind := dkUnknown;
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
		Kind := dkIfDef
	else if directiveName = 'IFNDEF' then
		Kind := dkIfNDef
	else if directiveName = 'IF' then
		Kind := dkIf
	else if directiveName = 'ELSE' then
		Kind := dkElse
	else if directiveName = 'ELSEIF' then
		Kind := dkElseIf
	else if directiveName = 'ENDIF' then
		Kind := dkEndIf
	else if directiveName = 'IFEND' then
		Kind := dkIfEnd
	else if directiveName = 'DEFINE' then
		Kind := dkDefine
	else if directiveName = 'UNDEF' then
		Kind := dkUndef
	else if directiveName = 'IFOPT' then
		Kind := dkIfOpt
	else if directiveName = 'INCLUDE' then
		Kind := dkInclude
	else if directiveName = 'I' then
		Kind := dkInclude   // short form
	else if directiveName = 'REGION' then
		Kind := dkRegion
	else if directiveName = 'ENDREGION' then
		Kind := dkEndRegion
	else
		Kind := dkUnknown;
end;

end.

