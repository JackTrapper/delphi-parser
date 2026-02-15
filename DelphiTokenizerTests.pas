unit DelphiTokenizerTests;

interface

uses
	Classes, Contnrs,
	TestFramework;


type
	TDelphiTokenizerTests = class(TTestCase)
	protected
		function HasTriviaTokens(tokens: TObjectList): Boolean;

		function TokensToStr(Tokens: TList): string;
	public
		procedure TestTokenPositions;					// positions aren't done yet
		procedure TestMultiCharOperatorPositions;	// positions aren't done yet
	published
		procedure Tokenize;

		procedure TestTokenizeIncludesEofToken;
		procedure TestParserReuseResetsState;
		procedure TestTriviaIsAttachedToEOFToken;

		procedure TestUnicodeIdentifiers;

		procedure TestUnterminatedCompilerDirective;

		procedure TestCompilerDirective;
		procedure TestCompilerDirectiveUnterminatedEOF;
		procedure TestCompilerDirectiveUnterminatedLineEnd;
		procedure TestReservedWords;

		// Numbers
		procedure TestHexNumbers;
		procedure TestBinaryLiteral;  // x = %1101;

		procedure TestFloatingPointNumbers;
		procedure TestFloatingPointNumbersWithNoDigitsAfterDecimalMark;				// 42.
		procedure TestFlaotingPointNumbersWithNoDigitAfterExponentialDecimalMark;	// 42.e3
		procedure TestNumberExponentLowercase;
		procedure TestNumberExponentSigns;
		procedure TestNumberNoDigitsAfterExponent;
		procedure TestNumberSignNotAfterExponent;

		procedure TestDigitSeparator;

		// Organized number tests
		procedure TestNumberUnderscoresDecimal;
		procedure TestNumberUnderscoresHex;
		procedure TestDotVsRangeWithNumbers;

		procedure TestAsciiCharLiterals;
		procedure TestAnsiComments;
		procedure TestBorComments;
		procedure TestSlashComments;
		procedure TestMultiCharOperators;
		procedure TestSingleCharOperators;
		procedure TestWhitespace;

		procedure TestStringLiterals;
		procedure TestUnterminatedString;
		procedure TestUnterminatedStringEOF;
		procedure TestStringUnterminatedByLineEnd;
		// String escape sequence tests (for Section 5.2 improvements)
		procedure TestStringWithEscapedQuotes;
		procedure TestStringWithSurrogatePairs;
		procedure TestStringWithInvalidSurrogatePair;
		procedure TestStringWithNullCharacter;
		procedure TestStringExceedingMaxLength; // string line length limits were removed in Delphi 12.0+ (sorta)
		procedure TestStringWithTabCharacter;
		procedure TestMultilineString;
		procedure TestMultilineStringSpacesAndTabs;
		procedure TestMultilineStringMixingSpacesAndTabsIncorrectly;
		procedure TestMultilineStringMixingRecoverLessIndentedLines;
		procedure TestMultilineStringWithoutWhitespaceBeforeIt;


		// Organized comment/directive/identifier tests
		procedure TestUnterminatedAnsiCommentEOF;
		procedure TestUnterminatedBorCommentEOF;
		procedure TestIdentifierEscapedKeywordWithAmpersand;
	end;

	TInputStreamTests = class(TTestCase)
	public
		procedure TestRingBufferWrapStress;
		procedure TestHugePeekGrowth;
	published
		procedure TestReadChar;
		procedure TestPeekChar;
		procedure TestUnreadChar;
	end;

implementation

uses
	SysUtils, Math, ComObj, Windows, ActiveX,
	Toolkit, Avatar.Exceptions,
	DelphiTokenizer,
	DelphiParser;

function CreateStreamOnMemoryFromUnicode(const S: UnicodeString): ISequentialStream;
var
	hMem: HGLOBAL;
	p: Pointer;
	byteLen: SIZE_T;
begin
	byteLen := Length(S) * SizeOf(WideChar);
	hMem := GlobalAlloc(GHND, byteLen);
	if hMem = 0 then
		RaiseLastOSError;
	p := GlobalLock(hMem);
	if p = nil then
	begin
		GlobalFree(hMem);
		RaiseLastOSError;
	end;
	try
		if byteLen > 0 then
			Move(Pointer(S)^, p^, byteLen);
	finally
		GlobalUnlock(hMem);
	end;
	OleCheck(CreateStreamOnHGlobal(hMem, True, IStream(Result)));
end;

{ TInputStreamTests }

procedure TInputStreamTests.TestReadChar;
var
  stream: ISequentialStream;
  input: TInputStream;
  ch: WideChar;
  ok: Boolean;
begin
	stream := CreateStreamOnMemoryFromUnicode('ABC');
	input := TInputStream.Create(stream, CP_UTF16);
	try
		ok := input.TryRead(ch);
		CheckTrue(ok);
		CheckEqualsString('A', string(ch));

		ok := input.TryRead(ch);
		CheckTrue(ok);
		CheckEqualsString('B', string(ch));

		ok := input.TryRead(ch);
		CheckTrue(ok);
		CheckEqualsString('C', string(ch));

		ok := input.TryRead(ch);
		CheckFalse(ok, 'Should be EOF after consuming all characters');
		CheckTrue(input.EOF, 'EOF flag should be set after failed read');
	finally
		input.Free;
	end;
end;

procedure TInputStreamTests.TestRingBufferWrapStress;
const
	// Internal knowledge: initial ring buffer capacity in TInputStream.Create
	INITIAL_BUFFER_CAPACITY = 1024;
	TOTAL_READS = INITIAL_BUFFER_CAPACITY * 3; // enough to force wrap multiple times
var
	stream: ISequentialStream;
	input: TInputStream;
	src: UnicodeString;
	i: Integer;
	ch: WideChar;
	ok: Boolean;
	expected: WideChar;
begin
	// Build a predictable source string longer than we will read + prefill window
	SetLength(src, TOTAL_READS+INITIAL_BUFFER_CAPACITY+16);
	for i := 1 to Length(src) do
		src[i] := WideChar(Ord('A') + ((i - 1) mod 26));

	stream := CreateStreamOnMemoryFromUnicode(src);
	input := TInputStream.Create(stream, CP_UTF16);
	try
		// Maintain a small lookahead while consuming until the active window
		// approaches the end of capacity, at which point peeking should raise.
		for i := 1 to TOTAL_READS do
		begin
			ok := input.TryRead(ch);
			CheckTrue(ok, 'Unexpected EOF during wrap stress at i=' + IntToStr(i));
			expected := src[i];
			CheckEqualsString(string(expected), string(ch), 'Mismatch at i=' + IntToStr(i));

			try
				// Keep lookahead modest; eventually this append will exceed capacity
				input.Peek(8);
			except
				on E: Exception do
				begin
					CheckTrue(Pos('exceeded buffer capacity', LowerCase(E.Message)) > 0,
						'Unexpected exception: ' + E.Message);
					Exit; // test satisfied
				end;
			end;
		end;

		// If we completed the loop without exception, something is wrong
		Fail('Expected buffer capacity exceeded exception was not raised');
	finally
		input.Free;
	end;
end;

procedure TInputStreamTests.TestHugePeekGrowth;
const
	HUGE_PEEK = 16384;
var
	stream: ISequentialStream;
	input: TInputStream;
	src: UnicodeString;
		ch: WideChar;
		ok: Boolean;
		i: Integer;
begin
	// Build a very long string so HUGE_PEEK is within range
	SetLength(src, HUGE_PEEK + 1000);

	// Fill with 'X'
	for i := 1 to Length(src) do
		src[i] := 'X';

	// Place sentinel at HUGE_PEEK
	src[HUGE_PEEK] := 'Y';

	// Fill remainder with 'Z' for variance
	for i := HUGE_PEEK + 1 to Length(src) do
		src[i] := 'Z';

	stream := CreateStreamOnMemoryFromUnicode(src);
	input := TInputStream.Create(stream, CP_UTF16);
	try
		// Huge peek should raise capacity exceeded error (no wrap, no growth)
		CheckEqualsString('X', string(input.Peek(1)));
		try
			input.Peek(HUGE_PEEK);
			Fail('Expected buffer capacity exceeded exception was not raised');
		except
			on E: Exception do
				CheckTrue(Pos('exceeded buffer capacity', LowerCase(E.Message)) > 0,
					'Unexpected exception: ' + E.Message);
		end;

		ok := input.TryRead(ch);
		CheckTrue(ok);
		CheckEqualsString('X', string(ch));
	finally
		input.Free;
	end;
end;

procedure TInputStreamTests.TestPeekChar;
var
  stream: ISequentialStream;
  input: TInputStream;
  ch: WideChar;
  ok: Boolean;
begin
	stream := CreateStreamOnMemoryFromUnicode('AB');
	input := TInputStream.Create(stream, CP_UTF16);
	try
		CheckEqualsString('A', string(input.Peek(1)));
		CheckEqualsString('B', string(input.Peek(2)));
		// Peeking should not consume
		CheckEqualsString('A', string(input.Peek(1)));

		ok := input.TryRead(ch);
		CheckTrue(ok);
		CheckEqualsString('A', string(ch));

		ok := input.TryRead(ch);
		CheckTrue(ok);
		CheckEqualsString('B', string(ch));
	finally
		input.Free;
	end;
end;

procedure TInputStreamTests.TestUnreadChar;
var
  stream: ISequentialStream;
  input: TInputStream;
  ch: WideChar;
  ok: Boolean;
begin
	// Exercise peek(2) then consume sequence and ensure buffer advances correctly
	stream := CreateStreamOnMemoryFromUnicode('ABCDE');
	input := TInputStream.Create(stream, CP_UTF16);
	try
		CheckEqualsString('B', string(input.Peek(2)));

		ok := input.TryRead(ch);
		CheckTrue(ok);
		CheckEqualsString('A', string(ch));

		// After consuming one, previously peeked 'B' should now be next
		CheckEqualsString('B', string(input.Peek(1)));

		ok := input.TryRead(ch);
		CheckTrue(ok);
		CheckEqualsString('B', string(ch));

		// Buffer should roll forward cleanly
		CheckEqualsString('C', string(input.Peek(1)));
	finally
		input.Free;
	end;
end;

{ TDelphiTokenizerTests }

procedure TDelphiTokenizerTests.Tokenize;
var
	sourceCode: string;
	tokens: TObjectList;
begin
	sourceCode := 'unit test;';
	tokens := TObjectList.Create(True);
	using(tokens);

	TDelphiTokenizer.Tokenize(sourceCode, tokens);

	CheckTrue(tokens.Count > 0);
end;

procedure TDelphiTokenizerTests.TestTokenizeIncludesEofToken;
var
	tokens: TObjectList;
	lastToken: TSyntaxToken;
begin
	// Regression guard: tokenization should always surface an explicit EOF token
	// so downstream consumers (parser, lookahead) can detect end of input.
	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize('', tokens);

		CheckEquals(1, tokens.Count, 'Token stream should end with a dedicated EOF token');
		lastToken := tokens[tokens.Count - 1] as TSyntaxToken;
		CheckEquals(Ord(ptEof), Ord(lastToken.TokenKind), 'Final token kind should be ptEof');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestParserReuseResetsState;
const
	FIRST_UNIT_NAME  = 'ParserReuseFirstUnit';
	SECOND_UNIT_NAME = 'ParserReuseSecondUnit';
var
	parser: TDelphiParser;
	tokens1, tokens2: TObjectList;
	tree: TSyntaxNode2;

   function ExtractUnitName(const Root: TSyntaxNode2): string;
   var
      i: Integer;
      childWrapper: TSyntaxNodeOrToken;
      childNode: TSyntaxNode2;
   begin
      Result := '';
      if (Root = nil) or (Root.ChildNodes = nil) then
         Exit;

      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
         childWrapper := Root.ChildNodes[i];
         if childWrapper.IsNode then
         begin
            childNode := childWrapper.AsNode;
            if childNode.NodeType = ntUnitDeclaration then
            begin
               Result := childNode.Attributes[anName];
               Exit;
            end;
         end;
      end;
   end;

	function BuildUnitSource(const UnitName: string): string;
	begin
		{
			unit ParserReuseFirstUnit;
			interface
			implementation
			end;
		}
		Result :=
			Format('unit %s;%sinterface%simplementation%send.%s',
				[UnitName, sLineBreak, sLineBreak, sLineBreak, sLineBreak]);
	end;

begin
{

}
	parser := TDelphiParser.Create;
	try
		tokens1 := TObjectList.Create(False); // parser will take ownership of produced tokens
		tokens2 := TObjectList.Create(False);
		try
			TDelphiTokenizer.Tokenize(BuildUnitSource(FIRST_UNIT_NAME),  tokens1);
			TDelphiTokenizer.Tokenize(BuildUnitSource(SECOND_UNIT_NAME), tokens2);

			tree := parser.Parse(tokens1);
			try
				CheckEqualsString(FIRST_UNIT_NAME, ExtractUnitName(tree),
					'First parse should capture the correct unit name');
			finally
				tree.Free;
			end;

			tree := parser.Parse(tokens2);
			try
				CheckEqualsString(SECOND_UNIT_NAME, ExtractUnitName(tree),
					'Reusing a parser instance should reset token state between parses');
			finally
				tree.Free;
			end;
		finally
			tokens1.Free;
			tokens2.Free;
		end;
	finally
		parser.Free;
	end;
end;

function TDelphiTokenizerTests.TokensToStr(Tokens: TList): string;
var
	i: Integer;
	t: TSyntaxToken;
begin
	Result := 'Tokens';

	for i := 0 to Tokens.Count-1 do
	begin
		t := TObject(Tokens[i]) as TSyntaxToken;

		Result := Result+CRLF;
		Result := Result+'	['+IntToStr(i)+']='+t.ToString();
	end;
end;


procedure TDelphiTokenizerTests.TestNumberUnderscoresDecimal;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	countWithUnderscore: Integer;
begin
	// Decimal integers may include underscores as digit separators
	sourceCode := '1_000 12_34_56';

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		countWithUnderscore := 0;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if (token.TokenKind = ptIntegerConst) and (Pos('_', token.Text) > 0) then
				Inc(countWithUnderscore);
		end;

		CheckTrue(countWithUnderscore >= 2, 'Should recognize decimal integers with underscores');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestNumberUnderscoresHex;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	countWithUnderscore: Integer;
begin
	// Hex integers may include underscores as digit separators
	sourceCode := '$FF_FF $AB_CD';

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		countWithUnderscore := 0;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if (token.TokenKind = ptIntegerConst) and (Pos('$', token.Text) = 1) and (Pos('_', token.Text) > 0) then
				Inc(countWithUnderscore);
		end;

		CheckTrue(countWithUnderscore >= 2, 'Should recognize hex integers with underscores');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestDigitSeparator;
var
	sourceCode: string;
	tokens: TObjectList;
	token: TSyntaxToken;
begin
{
Delphi 11 Alexandria Got released, with many changes!
https://www.systemcamp.com/delphi-11-got-released-with-many-changes/

Digit Separator
===============

The language also introduces a digit separator that can be used to improve the readability of literal values with many digits. The separator is the underscore “_” and it is basically ignored when parsing and compiling the code. This is very similar to the feature introduced in C# 7.0.

const
	AMillion = 1_000_000;

Of course, you can use the digit separators for binary literals.
}
	sourceCode := '144_00000619';

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		CheckEquals(2, tokens.Count);  // [IntegerConst][eof]
		token := tokens[0] as TSyntaxToken;
		CheckTrue(Assigned(token));
		CheckEquals(TokenKindToStr(ptIntegerConst), TokenKindToStr(token.TokenKind), 'TokenKind');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestDotVsRangeWithNumbers;
var
	sourceCode: string;
	tokens: TObjectList;
	nonTriviaKinds: array of TptTokenKind;
	nonTriviaTexts: array of string;
	i, n: Integer;
	token: TSyntaxToken;
	dotDotCount: Integer;
	found314, found50: Boolean;
begin
	// Ensure '.' is not absorbed from the range operator and floats remain intact
	sourceCode := '1..2 3.14 .. 5.0..7';

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		SetLength(nonTriviaKinds, 0);
		SetLength(nonTriviaTexts, 0);
		n := 0;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if not TokenKindIsTrivia(token.TokenKind) then
			begin
				SetLength(nonTriviaKinds, n + 1);
				SetLength(nonTriviaTexts, n + 1);
				nonTriviaKinds[n] := token.TokenKind;
				nonTriviaTexts[n] := token.Text;
				Inc(n);
			end;
		end;

		// Basic expectations
		CheckTrue(n >= 7, 'Expected at least 7 non-trivia tokens');

		dotDotCount := 0;
		found314 := False;
		found50 := False;
		for i := 0 to n - 1 do
		begin
			if nonTriviaKinds[i] = ptDotDot then
				Inc(dotDotCount)
			else if (nonTriviaKinds[i] = ptFloat) and (nonTriviaTexts[i] = '3.14') then
				found314 := True
			else if ((nonTriviaKinds[i] = ptFloat) and (nonTriviaTexts[i] = '5.0')) then
				found50 := True;
		end;

		CheckTrue(dotDotCount >= 2, 'Should recognize at least two ".." tokens');
		CheckTrue(found314, 'Should recognize 3.14 as a float');
		CheckTrue(found50, 'Should recognize 5.0 as a float');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestUnterminatedAnsiCommentEOF;
var
	sourceCode: string;
	tokens: TObjectList;
	i, j: Integer;
	token, trivia: TSyntaxToken;
	foundComment, hasErrorFlag: Boolean;
begin
	// Unterminated ANSI comment should be surfaced as trivia with error state
	sourceCode := '(* unterminated';

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundComment := False;
		hasErrorFlag := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			for j := 0 to token.LeadingTriviaCount - 1 do
			begin
				trivia := token.LeadingTrivia[j];
				if trivia.TokenKind = ptAnsiComment then
				begin
					foundComment := True;
					if trivia.HasErrors or (Pos('Unterminated', trivia.ErrorMessage) > 0) or trivia.IsMissing then
						hasErrorFlag := True;
					Break;
				end;
			end;
			if foundComment then Break;
		end;

		CheckTrue(foundComment, 'Should produce ANSI comment trivia for unterminated comment');
		CheckTrue(hasErrorFlag, 'Unterminated ANSI comment should carry an error state');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestUnterminatedBorCommentEOF;
var
	sourceCode: string;
	tokens: TObjectList;
	i, j: Integer;
	token, trivia: TSyntaxToken;
	foundComment, hasErrorFlag: Boolean;
begin
	// Unterminated Borland comment should be surfaced as trivia with error state
	sourceCode := '{ unterminated';

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundComment := False;
		hasErrorFlag := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			for j := 0 to token.LeadingTriviaCount - 1 do
			begin
				trivia := token.LeadingTrivia[j];
				if trivia.TokenKind = ptBorComment then
				begin
					foundComment := True;
					if trivia.HasErrors or (Pos('Unterminated', trivia.ErrorMessage) > 0) or trivia.IsMissing then
						hasErrorFlag := True;
					Break;
				end;
			end;
			if foundComment then Break;
		end;

		CheckTrue(foundComment, 'Should produce Borland comment trivia for unterminated comment');
		CheckTrue(hasErrorFlag, 'Unterminated Borland comment should carry an error state');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestIdentifierEscapedKeywordWithAmpersand;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundVar, foundBegin_, foundEnd_: Boolean;
	foundEscapedForAsIdentifier: Boolean;
begin
	// Ampersand should allow keyword to be used as identifier: &for
	sourceCode := 'var &for: Integer; begin &for := 1; end.';

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundVar := False;
		foundBegin_ := False;
		foundEnd_ := False;
		foundEscapedForAsIdentifier := False;

		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptVar then foundVar := True
			else if token.TokenKind = ptBegin then foundBegin_ := True
			else if token.TokenKind = ptEnd then foundEnd_ := True
			else if (token.TokenKind = ptIdentifier) and ((token.Text = '&for') or (token.Text = 'for')) then
				foundEscapedForAsIdentifier := True
			else if token.TokenKind = ptFor then
			begin
				// If the tokenizer returned keyword 'for' despite ampersand, that's incorrect
				CheckTrue(False, 'Ampersand-escaped keyword should not be tokenized as ptFor');
			end;
		end;

		CheckTrue(foundVar, 'Should find var keyword');
		CheckTrue(foundBegin_, 'Should find begin keyword');
		CheckTrue(foundEnd_, 'Should find end keyword');
		CheckTrue(foundEscapedForAsIdentifier, 'Should treat &for as an identifier');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestUnicodeIdentifiers;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundCafeIdentifier: Boolean;
	debugOutput: string;
begin
	// Start with a simpler test case to debug the issue
	sourceCode := 'unit Test; var café: string; begin end.';

	tokens := TObjectList.Create(True);
   using(tokens);

   TDelphiTokenizer.Tokenize(sourceCode, tokens);

   // Debug: log what tokens we actually got
   debugOutput := Format('Total tokens: %d'#13#10, [tokens.Count]);
   for i := 0 to Min(tokens.Count - 1, 15) do // Show first 15 tokens
   begin
      token := tokens[i] as TSyntaxToken;
      debugOutput := debugOutput + Format('Token %d: %s = "%s"'#13#10,
         [i, TokenKindToStr(token.TokenKind), token.Text]);
   end;

   // First, just check that we got some tokens at all
   CheckTrue(tokens.Count > 5, Format('Should have gotten some tokens. Got %d. Debug:'#13#10'%s',
      [tokens.Count, debugOutput]));

   // Check what actually got tokenized
   foundCafeIdentifier := False;

   for i := 0 to tokens.Count - 1 do
   begin
      token := tokens[i] as TSyntaxToken;
      if token.TokenKind = ptIdentifier then
      begin
         if token.Text = 'café' then
            foundCafeIdentifier := True;
      end;
   end;

   // Test Unicode identifier recognition
   if foundCafeIdentifier then
      CheckTrue(True, 'Unicode identifier "café" was successfully recognized!')
   else
      CheckTrue(False, Format('Unicode identifier "café" was NOT recognized. Debug info:'#13#10'%s', [debugOutput]));
end;

procedure TDelphiTokenizerTests.TestTokenPositions;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	debugMsg: string;
	foundUnit, foundTest, foundSemicolon: Boolean;
	expectedPositions: array[0..7] of record
		TokenText: string;
		ExpectedStartOffset: Integer;
		ExpectedTokenLength: Integer;
	end;
begin
	// Test simple source with predictable token positions
	sourceCode := 'unit Test;';  // 'unit' at 0-3, ' ' at 4, 'Test' at 5-8, ';' at 9
//	               0123456789

	// Define expected positions
	expectedPositions[0].TokenText := 'unit';
	expectedPositions[0].ExpectedStartOffset := 0;
	expectedPositions[0].ExpectedTokenLength := 4;
	
	expectedPositions[1].TokenText := 'Test';
	expectedPositions[1].ExpectedStartOffset := 5;
	expectedPositions[1].ExpectedTokenLength := 4;
	
	expectedPositions[2].TokenText := ';';
	expectedPositions[2].ExpectedStartOffset := 9;
	expectedPositions[2].ExpectedTokenLength := 1;
	
	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		// Debug: Show what tokens we actually got
		debugMsg := '';
		for i := 0 to Min(tokens.Count-1, 2) do
		begin
			token := tokens[i] as TSyntaxToken;
			if i > 0 then
				debugMsg := debugMsg + ', ';
			debugMsg := debugMsg + '"' + token.Text + '"';
		end;
		
		CheckTrue(tokens.Count >= 2, Format('Expected at least 2 tokens (unit, Test, ;), got %d. Tokens: [%s]', 
				[tokens.Count, debugMsg]));
		
		// Look for the specific tokens we care about
		foundUnit := False;
		foundTest := False;
		foundSemicolon := False;
		
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.Text = 'unit' then foundUnit := True
			else if token.Text = 'Test' then foundTest := True
			else if token.Text = ';' then foundSemicolon := True;
		end;
		
		CheckTrue(foundUnit, 'Should find "unit" token');
		CheckTrue(foundTest, 'Should find "Test" token - this is the missing one!');
		CheckTrue(foundSemicolon, 'Should find ";" token');
		
		// Verify position tracking for key tokens (skip whitespace tokens)
		for i := 0 to tokens.Count-1 do
		begin
			token := tokens[i] as TSyntaxToken;
			
			// Check if this is one of our expected tokens
			if (token.Text = 'unit') then
			begin
				CheckEquals(0, token.StartOffset, 'unit token StartOffset');
			end
			else if (token.Text = 'Test') then
			begin
				CheckEquals(5, token.StartOffset, 'Test token StartOffset');
			end
			else if (token.Text = ';') then
			begin
				CheckEquals(9, token.StartOffset, '; token StartOffset');
			end;
		end;
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestTriviaIsAttachedToEOFToken;
var
	sourceCode: string;
	tokens: TObjectList;
	lastToken: TSyntaxToken;
begin
	sourceCode := '//his comment should be leading trivia attached to the EOF token';

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		CheckEquals(1, tokens.Count, 'Token stream should end with a dedicated EOF token');
		lastToken := tokens[tokens.Count-1] as TSyntaxToken;
		CheckEquals(Ord(ptEof), Ord(lastToken.TokenKind), 'Final token kind should be ptEof');

		CheckEquals(1, lastToken.LeadingTriviaCount, 'LeadingTriviaCount');
		CheckTrue(lastToken.LeadingTrivia[0].TokenKind = ptSlashesComment, 'leading trivia kind not ptSlashesComment');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestMultiCharOperatorPositions;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
begin
	// Test multi-character operators: ':=', '<=', '>=', '<>', '..'
	sourceCode := 'x:=5; if a<=b then c>=d else e<>f..g';
	//             0123456789012345678901234567890123456
	//             0         1         2         3
	
	tokens := TObjectList.Create(True);
	using(tokens);

   TDelphiTokenizer.Tokenize(sourceCode, tokens);

   CheckTrue(tokens.Count > 10, Format('Expected many tokens, got %d', [tokens.Count]));

   // Verify position tracking for multi-character operators
   for i := 0 to tokens.Count - 1 do
   begin
      token := tokens[i] as TSyntaxToken;

      // Check multi-character operators
      if (token.Text = ':=') then
      begin
         CheckEquals(1, token.StartOffset, ':= token StartOffset');
         CheckEquals(2, token.TokenLength, ':= token TokenLength');
      end
      else if (token.Text = '<=') then
      begin
         CheckEquals(12, token.StartOffset, '<= token StartOffset');
         CheckEquals(2, token.TokenLength, '<= token TokenLength');
      end
      else if (token.Text = '>=') then
      begin
         CheckEquals(22, token.StartOffset, '>= token StartOffset');
         CheckEquals(2, token.TokenLength, '>= token TokenLength');
      end
      else if (token.Text = '<>') then
      begin
         CheckEquals(29, token.StartOffset, '<> token StartOffset');
         CheckEquals(2, token.TokenLength, '<> token TokenLength');
      end
      else if (token.Text = '..') then
      begin
         CheckEquals(32, token.StartOffset, '.. token StartOffset');
         CheckEquals(2, token.TokenLength, '.. token TokenLength');
      end;
   end;
end;

procedure TDelphiTokenizerTests.TestUnterminatedCompilerDirective;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundDirective: Boolean;
	debugOutput: string;
begin
	// Test Issue #1: Infinite loop on unterminated compiler directive
	// This should NOT cause an infinite loop anymore - the fix adds UEOF check
	sourceCode := 'unit Test; {$IFDEF DEBUG'; // Missing closing brace - this used to cause infinite loop
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		// Debug: log what tokens we actually got
		debugOutput := Format('Total tokens: %d'#13#10, [tokens.Count]);
		for i := 0 to Min(tokens.Count - 1, 10) do // Show first 10 tokens
		begin
			token := tokens[i] as TSyntaxToken;
			debugOutput := debugOutput + Format('Token %d: %s = "%s"'#13#10, 
				[i, TokenKindToStr(token.TokenKind), token.Text]);
		end;
		
		// Verify we got some tokens (not stuck in infinite loop)
		CheckTrue(tokens.Count > 0, Format('Should have gotten tokens without infinite loop. Got %d.'#13#10'%s', 
			[tokens.Count, debugOutput]));
		
		// Look for the unterminated compiler directive token
		foundDirective := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptCompilerDirective then
			begin
				foundDirective := True;
				// Verify the directive contains what we expect (opening brace but no closing)
				CheckTrue(Pos('{$IFDEF', token.Text) > 0, 
					Format('Compiler directive should contain "{$IFDEF", got: "%s"', [token.Text]));
				Break;
			end;
		end;
		
		CheckTrue(foundDirective, 
			Format('Should have found compiler directive token.'#13#10'Debug:'#13#10'%s', [debugOutput]));
		
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestCompilerDirective;
var
	sourceCode: string;
	tokens: TObjectList;
	token: TSyntaxToken;
begin
{
Compiler directives can be both curly braces, and open paren star

https://docwiki.embarcadero.com/RADStudio/Sydney/en/Fundamental_Syntactic_Elements_%28Delphi%29

> A comment that contains a dollar sign ($) immediately after the opening { or ( * is a compiler directive.

}
	tokens := TObjectList.Create(True); // owns objects
	try
		sourceCode := '{$IFDEF DEBUG}';
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		CheckEquals(2, tokens.Count, 'tokens.Count');  // directive+eof
		token := tokens[0] as TSyntaxToken;
		CheckEquals(TokenKindToStr(ptCompilerDirective), TokenKindToStr(token.TokenKind));
		CheckTrue(token.DirectiveDelimeter = ddBrace, 'DirectiveDelimiter');

		tokens.Clear;

		sourceCode := '(*$IFDEF DEBUG*)';
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		CheckEquals(2, tokens.Count, 'tokens.Count');  // directive+eof
		token := tokens[0] as TSyntaxToken;
		CheckEquals(TokenKindToStr(ptCompilerDirective), TokenKindToStr(token.TokenKind));
		CheckTrue(token.DirectiveDelimeter = ddParenStar, 'DirectiveDelimiter');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestCompilerDirectiveUnterminatedEOF;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundDirective: Boolean;
	debugOutput: string;
begin
	// Test compiler directive that reaches EOF before closing brace
	sourceCode := 'unit Test; {$IFDEF DEBUG'; // Missing closing brace }

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		// Debug: log what tokens we actually got
		debugOutput := Format('Total tokens: %d'#13#10, [tokens.Count]);
		for i := 0 to Min(tokens.Count - 1, 10) do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.HasErrors then
				debugOutput := debugOutput + Format('Token %d: %s = "%s", HasErrors=True'#13#10,
					[i, TokenKindToStr(token.TokenKind), token.Text])
			else
				debugOutput := debugOutput + Format('Token %d: %s = "%s", HasErrors=False'#13#10,
					[i, TokenKindToStr(token.TokenKind), token.Text]);
		end;

		// Verify we got tokens without infinite loop
		CheckTrue(tokens.Count > 0, Format('Should have gotten tokens. Got %d.'#13#10'%s',
			[tokens.Count, debugOutput]));

		// Look for the compiler directive token with error flag
		foundDirective := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptCompilerDirective then
			begin
				foundDirective := True;

				// Verify the error flag is set
				if token.HasErrors then
					CheckTrue(True, 'Unterminated compiler directive correctly has HasErrors=True')
				else
					CheckTrue(False, 'Unterminated compiler directive should have HasErrors=True, but got False');

				// Verify the error message
				CheckTrue(Pos('Unterminated', token.ErrorMessage) > 0,
					Format('Error message should mention "Unterminated". Got: "%s"', [token.ErrorMessage]));

				// Verify IsMissing flag is set for parser recovery
				if token.IsMissing then
					CheckTrue(True, 'Unterminated compiler directive correctly has IsMissing=True for parser recovery')
				else
					CheckTrue(False, 'Unterminated compiler directive should have IsMissing=True for parser recovery');

				Break;
			end;
		end;

		CheckTrue(foundDirective,
			Format('Should have found compiler directive token with error.'#13#10'Debug:'#13#10'%s', [debugOutput]));

	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestCompilerDirectiveUnterminatedLineEnd;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundDirective: Boolean;
	debugOutput: string;
begin
	// Test compiler directive that reaches line end before closing brace
	sourceCode := 'unit Test;'#13#10'{$IFDEF DEBUG'#13#10'begin end.'; // Line break before closing brace
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		// Debug: log what tokens we actually got
		debugOutput := Format('Total tokens: %d'#13#10, [tokens.Count]);
		for i := 0 to Min(tokens.Count - 1, 15) do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.HasErrors then
				debugOutput := debugOutput + Format('Token %d: %s = "%s", HasErrors=True'#13#10, 
					[i, TokenKindToStr(token.TokenKind), token.Text])
			else
				debugOutput := debugOutput + Format('Token %d: %s = "%s", HasErrors=False'#13#10, 
					[i, TokenKindToStr(token.TokenKind), token.Text]);
		end;
		
		// Verify we got tokens
		CheckTrue(tokens.Count > 0, Format('Should have gotten tokens. Got %d.'#13#10'%s', 
			[tokens.Count, debugOutput]));
		
		// Look for the compiler directive token with error flag
		foundDirective := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptCompilerDirective then
			begin
				foundDirective := True;
				
				// Verify the error flag is set
				if token.HasErrors then
					CheckTrue(True, 'Unterminated compiler directive (line end) correctly has HasErrors=True')
				else
					CheckTrue(False, 'Unterminated compiler directive (line end) should have HasErrors=True, but got False');
				
				// Verify the error message mentions unterminated
				CheckTrue(Pos('Unterminated', token.ErrorMessage) > 0, 
					Format('Error message should mention "Unterminated". Got: "%s"', [token.ErrorMessage]));
				
				// Note: The detailed distinction (EOF vs line end) is in the LogFmt output, 
				// not in the token's ErrorMessage property
				
				// Verify IsMissing flag is set for parser recovery
				if token.IsMissing then
					CheckTrue(True, 'Unterminated compiler directive correctly has IsMissing=True for parser recovery')
				else
					CheckTrue(False, 'Unterminated compiler directive should have IsMissing=True for parser recovery');
				
				Break;
			end;
		end;
		
		CheckTrue(foundDirective, 
			Format('Should have found compiler directive token with error.'#13#10'Debug:'#13#10'%s', [debugOutput]));
		
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestReservedWords;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundBegin, foundEnd, foundIf, foundThen: Boolean;
begin
	sourceCode := 'begin if x then end';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundBegin := False;
		foundEnd := False;
		foundIf := False;
		foundThen := False;
		
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptBegin then foundBegin := True
			else if token.TokenKind = ptEnd then foundEnd := True
			else if token.TokenKind = ptIf then foundIf := True
			else if token.TokenKind = ptThen then foundThen := True;
		end;
		
		CheckTrue(foundBegin, 'Should recognize "begin" as reserved word');
		CheckTrue(foundEnd, 'Should recognize "end" as reserved word');
		CheckTrue(foundIf, 'Should recognize "if" as reserved word');
		CheckTrue(foundThen, 'Should recognize "then" as reserved word');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestStringLiterals;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
begin
	sourceCode := '''Hello World''';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				CheckEquals('''Hello World''', token.Text, 'String literal text should match');
				Break;
			end;
		end;
		
		CheckTrue(foundString, 'Should find string literal token');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestUnterminatedString;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
begin
	sourceCode := '''Unterminated string'; // Missing closing quote
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				CheckTrue(token.HasErrors, 'Unterminated string should have error flag');
				CheckTrue(Pos('Unterminated', token.ErrorMessage) > 0, 
					Format('Error message should mention "Unterminated". Got: "%s"', [token.ErrorMessage]));
				Break;
			end;
		end;
		
		CheckTrue(foundString, 'Should still create string token for unterminated string');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestUnterminatedStringEOF;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
begin
	sourceCode := '''Unterminated string'; // Missing closing quote

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				CheckTrue(token.HasErrors, 'Unterminated string should have error flag');
				CheckTrue(Pos('Unterminated', token.ErrorMessage) > 0,
					Format('Error message should mention "Unterminated". Got: "%s"', [token.ErrorMessage]));
				Break;
			end;
		end;

		CheckTrue(foundString, 'Should still create string token for unterminated string');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestStringUnterminatedByLineEnd;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
begin
	sourceCode := '''UnterminatedString'+CRLF;

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundString := False;
		for i := 0 to tokens.Count-1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				CheckTrue(token.HasErrors, 'Unterminated string should have error flag');
				CheckTrue(Pos('Unterminated', token.ErrorMessage) > 0,
					Format('Error message should mention "Unterminated". Got: "%s"', [token.ErrorMessage]));
				Break;
			end;
		end;

		CheckTrue(foundString, 'Should still create string token for unterminated string');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestHexNumbers;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundHex: Boolean;
begin
	sourceCode := '$1F $ABCD $FF';

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundHex := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if (token.TokenKind = ptIntegerConst) and (Pos('$', token.Text) = 1) then
			begin
				foundHex := True;
				Break;
			end;
		end;
		
		CheckTrue(foundHex, 'Should recognize hex numbers starting with $');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;


procedure TDelphiTokenizerTests.TestBinaryLiteral;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundBinary: Boolean;
begin
{
Delphi 11 Alexandria Got released, with many changes!
=====================================================

https://www.systemcamp.com/delphi-11-got-released-with-many-changes/


Binary Literals
---------------

The Delphi language in Olympus adds support for binary literals, in addition to
decimal and hexadecimal ones. A binary literal uses the % symbol as a prefix
(the same syntax used by other Pascal compilers):

const
	Four = %100;
var
	x: Integer;
begin
	x := %1001001;

Of course, you can use the digit separators for binary literals.

}
	sourceCode := '%1010 %11110000';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundBinary := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if (token.TokenKind = ptIntegerConst) and (Pos('%', token.Text) = 1) then
			begin
				foundBinary := True;
				Break;
			end;
		end;
		
		CheckTrue(foundBinary, 'Should recognize binary numbers starting with %');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestFlaotingPointNumbersWithNoDigitAfterExponentialDecimalMark;
var
	sourceCode: string;
	tokens: TObjectList;
	token: TSyntaxToken;
begin
	sourceCode := '42.e3';

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		CheckEquals(2, tokens.Count, 'Expected number of tokens'); // [float][eof]

		token := tokens[0] as TSyntaxToken;
		CheckNotNull(token, 'token');

		CheckEquals(TokenKindToStr(ptFloat), TokenKindToStr(token.TokenKind), 'TokenKind');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestFloatingPointNumbers;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundFloat: Boolean;
begin
	sourceCode := '3.14 2.5e10';

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundFloat := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptFloat then
			begin
				foundFloat := True;
				Break;
			end;
		end;

		CheckTrue(foundFloat, 'Should recognize floating point numbers');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestFloatingPointNumbersWithNoDigitsAfterDecimalMark;
var
	sourceCode: string;
	tokens: TObjectList;
	token: TSyntaxToken;
begin
	sourceCode := '1.';

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		CheckEquals(2, tokens.Count, 'token count');

		token := tokens[0] as TSyntaxToken;
		CheckNotNull(token, 'token');

		CheckEquals(TokenKindToStr(ptFloat), TokenKindToStr(token.TokenKind), 'TokenKind');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestNumberExponentLowercase;
	var
		sourceCode: string;
		tokens: TObjectList;
		i: Integer;
		token: TSyntaxToken;
		found1e10, found2E5: Boolean;
	begin
		// Lowercase 'e' and uppercase 'E' should both be recognized as exponents
		sourceCode := '1e10 2E5';

		tokens := TObjectList.Create(True);
		try
			TDelphiTokenizer.Tokenize(sourceCode, tokens);

			found1e10 := False;
			found2E5 := False;

			for i := 0 to tokens.Count - 1 do
			begin
				token := tokens[i] as TSyntaxToken;
				if (token.Text = '1e10') then
				begin
					CheckTrue(token.TokenKind = ptFloat, '1e10 should be a float literal');
					found1e10 := True;
				end
				else if (token.Text = '2E5') then
				begin
					CheckTrue(token.TokenKind = ptFloat, '2E5 should be a float literal');
					found2E5 := True;
				end;
			end;

			CheckTrue(found1e10, 'Should find token 1e10 as ptFloat');
			CheckTrue(found2E5, 'Should find token 2E5 as ptFloat');
		finally
			tokens.Free;
		end;
	end;

	procedure TDelphiTokenizerTests.TestNumberExponentSigns;
	var
		sourceCode: string;
		tokens: TObjectList;
		i: Integer;
		token: TSyntaxToken;
		countFloats: Integer;
		expected: array[0..3] of string;
		seen: array[0..3] of Boolean;
	begin
		// Signs are only valid immediately after E/e and should be part of the float
		sourceCode := '1e+2 3e-4 5E+6 7E-8';
		expected[0] := '1e+2';
		expected[1] := '3e-4';
		expected[2] := '5E+6';
		expected[3] := '7E-8';
		seen[0] := False;
		seen[1] := False;
		seen[2] := False;
		seen[3] := False;

{
Actual returned tokens:

	[0]=Token: ptFloat (1, 1) "1e+e"       should be 1e+2
	[1]=Token: ptFloat (1, 6) "3e-e"			should be 3e-4
	[2]=Token: ptFloat (1, 11) "5E+E"		should be 5E+6
	[3]=Token: ptFloat (1, 16) "7E-E"		should be 7E-8

It seem like the exponent number is being replaced with E
}
		tokens := TObjectList.Create(True);
		try
			TDelphiTokenizer.Tokenize(sourceCode, tokens);

			Status(TokensToStr(tokens));

			countFloats := 0;
			for i := 0 to tokens.Count - 1 do
			begin
				token := tokens[i] as TSyntaxToken;
				if token.TokenKind = ptFloat then
				begin
					Inc(countFloats);
					if token.Text = expected[0] then seen[0] := True
					else if token.Text = expected[1] then seen[1] := True
					else if token.Text = expected[2] then seen[2] := True
					else if token.Text = expected[3] then seen[3] := True;
				end;
			end;

			CheckEquals(4, countFloats, 'Should recognize four float tokens with signed exponents');
			CheckTrue(seen[0], 'Missing 1e+2');
			CheckTrue(seen[1], 'Missing 3e-4');
			CheckTrue(seen[2], 'Missing 5E+6');
			CheckTrue(seen[3], 'Missing 7E-8');
		finally
			tokens.Free;
		end;
	end;

	procedure TDelphiTokenizerTests.TestNumberNoDigitsAfterExponent;
	var
		sourceCode: string;
		tokens: TObjectList;
		i: Integer;
		token: TSyntaxToken;
		countFloats, countInts: Integer;
		foundIdLowerE, foundIdUpperE, foundId_eX, foundPlus, foundMinus: Boolean;
	begin
		// An exponent must have digits after optional sign; otherwise, don't form a float
		// Cases: '1e'  '2E+'  '3e-'  '4eX'
		sourceCode := '1e 2E+ 3e- 4eX';

		tokens := TObjectList.Create(True);
		try
			TDelphiTokenizer.Tokenize(sourceCode, tokens);

			countFloats := 0; countInts := 0;
			foundIdLowerE := False; foundIdUpperE := False; foundId_eX := False; foundPlus := False; foundMinus := False;

			for i := 0 to tokens.Count - 1 do
			begin
				token := tokens[i] as TSyntaxToken;
				if token.TokenKind = ptFloat then Inc(countFloats)
				else if token.TokenKind = ptIntegerConst then Inc(countInts)
				else if (token.TokenKind = ptIdentifier) and (token.Text = 'e') then foundIdLowerE := True
				else if (token.TokenKind = ptIdentifier) and (token.Text = 'E') then foundIdUpperE := True
				else if (token.TokenKind = ptIdentifier) and (token.Text = 'eX') then foundId_eX := True
				else if token.TokenKind = ptPlus then foundPlus := True
				else if token.TokenKind = ptMinus then foundMinus := True;
			end;

			CheckEquals(0, countFloats, 'None of these forms should be recognized as float');
			CheckTrue(countInts >= 4, 'Should recognize 4 integer prefixes (1,2,3,4)');
			CheckTrue(foundIdLowerE, 'Should see identifier e after 1');
			CheckTrue(foundIdUpperE, 'Should see identifier E after 2');
			CheckTrue(foundId_eX, 'Should see identifier eX after 4');
			CheckTrue(foundPlus, 'Should see + token after 2E');
			CheckTrue(foundMinus, 'Should see - token after 3e');
		finally
			tokens.Free;
		end;
	end;

	procedure TDelphiTokenizerTests.TestNumberSignNotAfterExponent;
	var
		sourceCode: string;
		tokens: TObjectList;
		i: Integer;
		token: TSyntaxToken;
		seqKinds: array of TptTokenKind;
		seqTexts: array of string;
		nonTriviaCount: Integer;
	begin
		// Ensure '+' or '-' are not absorbed into a number unless they follow E/e
		sourceCode := '1+-2';

		tokens := TObjectList.Create(True);
		try
			TDelphiTokenizer.Tokenize(sourceCode, tokens);

			SetLength(seqKinds, 0);
			SetLength(seqTexts, 0);
			nonTriviaCount := 0;

			for i := 0 to tokens.Count - 1 do
			begin
				token := tokens[i] as TSyntaxToken;
				if not TokenKindIsTrivia(token.TokenKind) then
				begin
					SetLength(seqKinds, nonTriviaCount+1);
					SetLength(seqTexts, nonTriviaCount+1);
					seqKinds[nonTriviaCount] := token.TokenKind;
					seqTexts[nonTriviaCount] := token.Text;
					Inc(nonTriviaCount);
				end;
			end;

			// Expect the sequence: 1, +, -, 2 (ignoring EOF sentinel if any)
			CheckTrue(nonTriviaCount >= 4, 'Expected at least 4 non-trivia tokens for "1+-2"');
			CheckTrue(seqKinds[0] = ptIntegerConst, 'First token should be integer 1');
			CheckTrue(seqTexts[0] = '1', 'First token text should be "1"');
			CheckTrue(seqKinds[1] = ptPlus, 'Second token should be plus');
			CheckTrue(seqTexts[1] = '+', 'Second token text should be "+"');
			CheckTrue(seqKinds[2] = ptMinus, 'Third token should be minus');
			CheckTrue(seqTexts[2] = '-', 'Third token text should be "-"');
			CheckTrue(seqKinds[3] = ptIntegerConst, 'Fourth token should be integer 2');
			CheckTrue(seqTexts[3] = '2', 'Fourth token text should be "2"');
		finally
			tokens.Free;
		end;
	end;

procedure TDelphiTokenizerTests.TestAsciiCharLiterals;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundAscii: Boolean;
begin
	sourceCode := '#13#10 #$41';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundAscii := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptAsciiChar then
			begin
				foundAscii := True;
				Break;
			end;
		end;
		
		CheckTrue(foundAscii, 'Should recognize ASCII character literals (#n)');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestAnsiComments;
var
	sourceCode: string;
	tokens: TObjectList;
	i, j: Integer;
	token: TSyntaxToken;
	triviaToken: TSyntaxToken;
	foundComment: Boolean;
begin
	// Comments are stored as trivia on the EOF token
	sourceCode := '(* This is an ANSI comment *)';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		// Comments are attached as trivia to tokens (usually EOF if standalone)
		// Look through all tokens and their trivia
		foundComment := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			
			// Check leading trivia
			for j := 0 to token.LeadingTriviaCount - 1 do
			begin
				triviaToken := token.LeadingTrivia[j];
				if triviaToken.TokenKind = ptAnsiComment then
				begin
					foundComment := True;
					CheckTrue(Pos('(*', triviaToken.Text) = 1, 'ANSI comment should start with (*');
					CheckTrue(Pos('*)', triviaToken.Text) > 0, 'ANSI comment should end with *)');
					Break;
				end;
			end;
			if foundComment then Break;
		end;
		
		CheckTrue(foundComment, 'Should recognize ANSI-style comments (* *) in trivia');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestBorComments;
var
	sourceCode: string;
	tokens: TObjectList;
	i, j: Integer;
	token: TSyntaxToken;
	triviaToken: TSyntaxToken;
	foundComment: Boolean;
begin
	// Comments are stored as trivia on tokens
	sourceCode := '{ This is a Borland comment }';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		// Comments are attached as trivia to tokens (usually EOF if standalone)
		foundComment := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			
			// Check leading trivia
			for j := 0 to token.LeadingTriviaCount - 1 do
			begin
				triviaToken := token.LeadingTrivia[j];
				if triviaToken.TokenKind = ptBorComment then
				begin
					foundComment := True;
					CheckTrue(Pos('{', triviaToken.Text) = 1, 'Bor comment should start with {');
					CheckTrue(Pos('}', triviaToken.Text) > 0, 'Bor comment should end with }');
					Break;
				end;
			end;
			if foundComment then Break;
		end;
		
		CheckTrue(foundComment, 'Should recognize Borland-style comments { } in trivia');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestSlashComments;
var
	sourceCode: string;
	tokens: TObjectList;
	i, j: Integer;
	token: TSyntaxToken;
	triviaToken: TSyntaxToken;
	foundComment: Boolean;
begin
	// Comments are stored as trivia on tokens
	sourceCode := '// This is a slash comment'#13#10'begin end';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		// Comments are attached as trivia to tokens (as leading trivia on the 'begin' token)
		foundComment := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			
			// Check leading trivia
			for j := 0 to token.LeadingTriviaCount - 1 do
			begin
				triviaToken := token.LeadingTrivia[j];
				if triviaToken.TokenKind = ptSlashesComment then
				begin
					foundComment := True;
					CheckTrue(Pos('//', triviaToken.Text) = 1, 'Slash comment should start with //');
					Break;
				end;
			end;
			if foundComment then Break;
		end;
		
		CheckTrue(foundComment, 'Should recognize C++-style comments // in trivia');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestMultiCharOperators;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundAssign, foundLessEquals, foundGreaterEquals, foundNotEqual, foundDotDot, foundDoubleAt: Boolean;
begin
	sourceCode := ':= <= >= <> .. @@';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundAssign := False;
		foundLessEquals := False;
		foundGreaterEquals := False;
		foundNotEqual := False;
		foundDotDot := False;
		foundDoubleAt := False;
		
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptAssign then foundAssign := True
			else if token.TokenKind = ptLessThanEquals then foundLessEquals := True
			else if token.TokenKind = ptGreaterThanEquals then foundGreaterEquals := True
			else if token.TokenKind = ptNotEqual then foundNotEqual := True
			else if token.TokenKind = ptDotDot then foundDotDot := True
			else if token.TokenKind = ptDoubleAddressOp then foundDoubleAt := True;
		end;
		
		CheckTrue(foundAssign, 'Should recognize := operator');
		CheckTrue(foundLessEquals, 'Should recognize <= operator');
		CheckTrue(foundGreaterEquals, 'Should recognize >= operator');
		CheckTrue(foundNotEqual, 'Should recognize <> operator');
		CheckTrue(foundDotDot, 'Should recognize .. operator');
		CheckTrue(foundDoubleAt, 'Should recognize @@ operator');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestMultilineString;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundMultilineString: Boolean;
const
	T3 = '''''''';			// '''   (3 apostrophes)
	T5 = '''''''''''';	// ''''' (5 apostrophes)
begin
	// Test 1: Simple multiline string
	sourceCode := T3+CRLF+
'			The quick brown fox jumps'+CRLF+
'			over the lazy dog.'+CRLF+
'			'+T3;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundMultilineString := False;
		for i := 0 to tokens.Count-1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptMultilineStringLiteral then
			begin
				foundMultilineString := True;
				CheckTrue(Pos(T3, token.Text) = 1, 'Multiline string should start with triple quotes');
				CheckFalse(token.HasErrors, 'Simple multiline string should not have errors');

				CheckEquals('The quick brown fox jumps'+CRLF+'over the lazy dog.', token.ValueText);
				Break;
			end;
		end;
		CheckTrue(foundMultilineString, 'Should recognize triple-quoted multiline string');
	finally
		tokens.Free;
	end;

	// Test 2: HTML content
	sourceCode := T3+CRLF+
'			<UL>'+CRLF+
'				<LI>Item 1</LI>'+CRLF+
'				<LI>Item 2</LI>'+CRLF+
'				<LI>Item 3</LI>'+CRLF+
'				<LI>Item 4</LI>'+CRLF+
'			</UL>'+CRLF+
'			'+T3;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundMultilineString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptMultilineStringLiteral then
			begin
				foundMultilineString := True;
				CheckFalse(token.HasErrors, 'HTML multiline string should not have errors');
				CheckTrue(Pos('<UL>', token.ValueText) > 0, 'ValueText should contain HTML content');

				CheckEquals(
						'<UL>'+CRLF+
						'	<LI>Item 1</LI>'+CRLF+
						'	<LI>Item 2</LI>'+CRLF+
						'	<LI>Item 3</LI>'+CRLF+
						'	<LI>Item 4</LI>'+CRLF+
						'</UL>', token.ValueText);
				Break;
			end;
		end;
		CheckTrue(foundMultilineString, 'Should tokenize HTML multiline string');
	finally
		tokens.Free;
	end;

	// Test 3: JSON content
	sourceCode := T3+CRLF+
'			['+CRLF+
'				{"id" : "1", "name" : "Large"},'+CRLF+
'				{"id" : "2", "name" : "Medium"},'+CRLF+
'				{"id" : "2", "name" : "Small"}'+CRLF+
'			]'+CRLF+
'			'+T3;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundMultilineString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptMultilineStringLiteral then
			begin
				foundMultilineString := True;
				CheckFalse(token.HasErrors, 'JSON multiline string should not have errors');

				CheckEquals(
						'['+CRLF+
						'	{"id" : "1", "name" : "Large"},'+CRLF+
						'	{"id" : "2", "name" : "Medium"},'+CRLF+
						'	{"id" : "2", "name" : "Small"}'+CRLF+
						']', token.ValueText);
				Break;
			end;
		end;
		CheckTrue(foundMultilineString, 'Should tokenize JSON multiline string');
	finally
		tokens.Free;
	end;

	// Test 4: SQL content with embedded single quotes
	sourceCode := T3+CRLF+
'			SELECT *'+CRLF+
'			FROM Customers'+CRLF+
'			WHERE Department = ''R&D'''+CRLF+
'			ORDER BY Name;'+CRLF+
'			'+T3;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundMultilineString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptMultilineStringLiteral then
			begin
				foundMultilineString := True;
				CheckFalse(token.HasErrors, 'SQL multiline string should not have errors');
				// The single quotes inside should be preserved
				CheckTrue(Pos('''R&D''', token.Text) > 0, 'Should preserve single quotes in SQL string');

				CheckEquals(
						'SELECT *'+CRLF+
						'FROM Customers'+CRLF+
						'WHERE Department = ''R&D'''+CRLF+
						'ORDER BY Name;', token.ValueText);
				Break;
			end;
		end;
		CheckTrue(foundMultilineString, 'Should tokenize SQL multiline string with embedded quotes');
	finally
		tokens.Free;
	end;

{Multiline string indentation and formatting logic are used very specifically.
A multiline string treats a leading white space this way:

- The closing ''' needs to be in a line of its own, not at the end of the last line of the string itself.
- The indentation of the closing ''' determines the base indentation of the entire string.
- Each blank space before that indentation level is removed in the final string for each of the lines.
- None of the lines can be less indented than the base indentation (the closing '''). This is a compiler error, showing also as Error Insight.
- The last newline before the closing ''' is omitted. If you want to have a final new line, you should add an empty line at the end.
}

	// Test 5: Five quotes with embedded triple quotes
	// Triple quotes (''') can also be replaced with a large odd number of quotes,
	// like 5 or 7. This allows embedding an actual triple quote within a multiline string.
	sourceCode := T5+CRLF+
'		some text'+CRLF+
'		and now '''''''+T3+CRLF+
'		some more text'+CRLF+
'		'+T5;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundMultilineString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptMultilineStringLiteral then
			begin
				foundMultilineString := True;
				CheckTrue(Pos(T5, token.Text) = 1, 'Multiline string should start with 5 quotes');
				CheckFalse(token.HasErrors, 'Five-quote multiline string should not have errors');
				// The embedded triple quotes should be in the content
				CheckTrue(Pos(T3, token.ValueText) > 0, 'Should preserve embedded triple quotes in ValueText');

				CheckEquals(
						'some text'+CRLF+
						'and now '''''''+T3+CRLF+
						'some more text', token.ValueText);
				Break;
			end;
		end;
		CheckTrue(foundMultilineString, 'Should recognize five-quoted multiline string with embedded triple quotes');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestMultilineStringMixingRecoverLessIndentedLines;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundMultilineString: Boolean;
	s: string;
const
	T3 = '''''''';			// '''   (3 apostrophes)
	T5 = '''''''''''';	// ''''' (5 apostrophes)
begin
	// Multiline string cannot mix tabs and spaces inconsistently
	// Error: E2657 Inconsistent indent characters
	sourceCode := T3+CRLF+
'			The quick brown fox jumps'+CRLF+
'		over the lazy dog.'+CRLF+
'			'+T3;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundMultilineString := False;
		for i := 0 to tokens.Count-1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind <> ptMultilineStringLiteral then
				Continue;

			foundMultilineString := True;

			s := token.ValueText;
			CheckEquals('The quick brown fox jumps'+CRLF+'over the lazy dog.', s{token.ValueText});

			CheckTrue(Pos(T3, token.Text) = 1, 'Multiline string should start with triple quotes');

			CheckTrue(token.HasErrors, 'Mixed indent should give an error');
			CheckEquals(token.ErrorMessage, 'E2657 Inconsistent indent characters');

			Break;
		end;
		CheckTrue(foundMultilineString, 'Should recognize triple-quoted multiline string');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestMultilineStringMixingSpacesAndTabsIncorrectly;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundMultilineString: Boolean;
	s: string;
const
	T3 = '''''''';			// '''   (3 apostrophes)
	T5 = '''''''''''';	// ''''' (5 apostrophes)
begin
	// Multiline string cannot mix tabs and spaces inconsistently
	// Error: E2657 Inconsistent indent characters
	sourceCode := T3+CRLF+
'			The quick brown fox jumps'+CRLF+
'		   over the lazy dog.'+CRLF+
'			'+T3;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundMultilineString := False;
		for i := 0 to tokens.Count-1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind <> ptMultilineStringLiteral then
				Continue;

			foundMultilineString := True;

			s := token.ValueText;
			CheckEquals('The quick brown fox jumps'+CRLF+'  over the lazy dog.', s{token.ValueText});

			CheckTrue(Pos(T3, token.Text) = 1, 'Multiline string should start with triple quotes');

			CheckTrue(token.HasErrors, 'Mixed indent should give an error');
			CheckEquals(token.ErrorMessage, 'E2657 Inconsistent indent characters');

			Break;
		end;
		CheckTrue(foundMultilineString, 'Should recognize triple-quoted multiline string');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestMultilineStringSpacesAndTabs;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundMultilineString: Boolean;
const
	T3 = '''''''';			// '''   (3 apostrophes)
	T5 = '''''''''''';	// ''''' (5 apostrophes)
begin
	// Test 1: Simple multiline string using tabs
	sourceCode := T3+CRLF+
'			The quick brown fox jumps'+CRLF+
'			over the lazy dog.'+CRLF+
'			'+T3;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundMultilineString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptMultilineStringLiteral then
			begin
				foundMultilineString := True;
				CheckTrue(Pos(T3, token.Text) = 1, 'Multiline string should start with triple quotes');
				CheckFalse(token.HasErrors, 'Simple multiline string should not have errors');

				CheckEquals('The quick brown fox jumps'+CRLF+'over the lazy dog.', token.ValueText);
				Break;
			end;
		end;
		CheckTrue(foundMultilineString, 'Should recognize triple-quoted multiline string');
	finally
		tokens.Free;
	end;

	// Test 2: Simple multiline string using spaces
	sourceCode := T3+CRLF+
'         The quick brown fox jumps'+CRLF+
'         over the lazy dog.'+CRLF+
'         '+T3;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundMultilineString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptMultilineStringLiteral then
			begin
				foundMultilineString := True;
				CheckTrue(Pos(T3, token.Text) = 1, 'Multiline string should start with triple quotes');
				CheckFalse(token.HasErrors, 'Simple multiline string should not have errors');

				CheckEquals('The quick brown fox jumps'+CRLF+'over the lazy dog.', token.ValueText);
				Break;
			end;
		end;
		CheckTrue(foundMultilineString, 'Should recognize triple-quoted multiline string');
	finally
		tokens.Free;
	end;

	// Test 3: Simple multiline string using spaces and tabs (consistently)
	// You can use tabs or spaces, but whatever you use it has to be the same for all
	// lines involved.
	sourceCode := T3+CRLF+
'	      The quick brown fox jumps'+CRLF+
'	      over the lazy dog.'+CRLF+
'	      '+T3;

	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundMultilineString := False;
		for i := 0 to tokens.Count-1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptMultilineStringLiteral then
			begin
				foundMultilineString := True;
				CheckTrue(Pos(T3, token.Text) = 1, 'Multiline string should start with triple quotes');
				CheckFalse(token.HasErrors, 'Simple multiline string should not have errors');

				CheckEquals('The quick brown fox jumps'+CRLF+'over the lazy dog.', token.ValueText);
				Break;
			end;
		end;
		CheckTrue(foundMultilineString, 'Should recognize triple-quoted multiline string');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.TestMultilineStringWithoutWhitespaceBeforeIt;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
begin
{
If you have code like:

var s: string = '''
Hello world'''

that is not valid, as the closing ''' must be on its own line:

> E2658 There should be no non-whitespace characters before the closing quotes of the text block

var s: string = '''
Hello world
'''

Test we handle that
}

	sourceCode := '''''''
var s: string = '''
Hello world'''
''''''';
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		Status(TokensToStr(tokens));

		// Find the multiline string literal token, and check that it has the error
		for i := 0 to tokens.Count-1 do
		begin
			token := tokens[i] as TSyntaxToken;
			TConstraints.NotNull(token);
			if token.TokenKind <> ptMultilineStringLiteral then
				Continue;

			CheckTrue(token.HasErrors, 							'Multiline literal should have an error');
			CheckTrue(token.ErrorMessage.Contains('E2658'),	Format('Did not find %s in error message: %s', ['E2658', token.ErrorMessage]));
			Exit;
		end;
	finally
		tokens.Free;
	end;

	// If we got here then we didn't find a multiline string. wtf?
	CheckTrue(False, 'We did not find a ptMultilineStringLiteral');
end;

procedure TDelphiTokenizerTests.TestSingleCharOperators;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundPlus, foundMinus, foundAsterisk, foundSlash, foundEquals: Boolean;
begin
	sourceCode := '+ - * / =';

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundPlus := False;
		foundMinus := False;
		foundAsterisk := False;
		foundSlash := False;
		foundEquals := False;
		
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptPlus then foundPlus := True
			else if token.TokenKind = ptMinus then foundMinus := True
			else if token.TokenKind = ptAsterisk then foundAsterisk := True
			else if token.TokenKind = ptSlash then foundSlash := True
			else if token.TokenKind = ptEquals then foundEquals := True;
		end;
		
		CheckTrue(foundPlus, 'Should recognize + operator');
		CheckTrue(foundMinus, 'Should recognize - operator');
		CheckTrue(foundAsterisk, 'Should recognize * operator');
		CheckTrue(foundSlash, 'Should recognize / operator');
		CheckTrue(foundEquals, 'Should recognize = operator');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestWhitespace;
var
	sourceCode: string;
	tokens: TObjectList;
	i, j: Integer;
	token: TSyntaxToken;
	triviaToken: TSyntaxToken;
	foundWhitespace: Boolean;
begin
	sourceCode := 'begin'#13#10'  end';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		// Whitespace (including line endings) are stored as trivia
		foundWhitespace := False;
		
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			
			// Check both leading and trailing trivia
			for j := 0 to token.LeadingTriviaCount - 1 do
			begin
				triviaToken := token.LeadingTrivia[j];
				if (triviaToken.TokenKind = ptWhitespace) or (triviaToken.TokenKind = ptCRLF) then
					foundWhitespace := True;
			end;
			
			for j := 0 to token.TrailingTriviaCount - 1 do
			begin
				triviaToken := token.TrailingTrivia[j];
				if (triviaToken.TokenKind = ptWhitespace) or (triviaToken.TokenKind = ptCRLF) then
					foundWhitespace := True;
			end;
		end;
		
		CheckTrue(foundWhitespace, 'Should recognize whitespace (including line endings) in trivia');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestStringWithEscapedQuotes;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
begin
	// Test string with escaped quotes (doubled apostrophes)
	// Source: 'He said, ''Hello!'' to me.'
	sourceCode := '''He said, ''''Hello!'''' to me.''';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				// The tokenizer processes escapes during parsing, so both Text and ValueText
				// contain the de-escaped version. Text includes outer quotes.
				CheckEquals('''He said, ''Hello!'' to me.''', token.Text, 
					'Text should contain string with outer quotes and processed inner escapes');
				// ValueText is the same but without outer quotes
				CheckEquals('He said, ''Hello!'' to me.', token.ValueText, 
					'ValueText should contain the string content with processed escapes');
				Break;
			end;
		end;
		
		CheckTrue(foundString, 'Should find string literal with escaped quotes');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestStringWithSurrogatePairs;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
	highSurrogate, lowSurrogate: WideChar;
begin
	// Test string with valid Unicode surrogate pair (emoji: 😀 = U+1F600)
	// High surrogate: U+D83D, Low surrogate: U+DE00
	highSurrogate := WideChar($D83D);
	lowSurrogate := WideChar($DE00);
	sourceCode := '''' + highSurrogate + lowSurrogate + '''';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				// Valid surrogate pair should NOT generate warning
				if token.HasWarnings then
					CheckTrue(False, Format('Valid surrogate pair should not have warning. Got: "%s"', [token.WarningMessage]))
				else
					CheckTrue(True, 'Valid surrogate pair processed without warning');
				Break;
			end;
		end;
		
		CheckTrue(foundString, 'Should find string literal with surrogate pair');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestStringWithInvalidSurrogatePair;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
	highSurrogate: WideChar;
begin
	// Test string with invalid surrogate pair (high surrogate not followed by low)
	highSurrogate := WideChar($D83D); // High surrogate
	sourceCode := '''' + highSurrogate + 'X'''; // Followed by regular char instead of low surrogate
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				// Invalid surrogate pair should generate warning
				CheckTrue(token.HasWarnings, 'Invalid surrogate pair should have warning');
				CheckTrue(Pos('surrogate', LowerCase(token.WarningMessage)) > 0, 
					Format('Warning should mention surrogate. Got: "%s"', [token.WarningMessage]));
				Break;
			end;
		end;
		
		CheckTrue(foundString, 'Should find string literal with invalid surrogate pair');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestStringWithNullCharacter;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
begin
	// Test string containing null character (should not cause any issues at all)
	sourceCode := '''Hello' + #0 + 'World''';
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				// Null character should not generate any warnings or errors
				CheckFalse(token.HasWarnings, 'String with null character should not have warnings');
				CheckFalse(token.HasErrors,   'String with null character should not have errors');
				CheckTrue(token.WarningMessage = '');
				CheckTrue(token.ErrorMessage = '');
				Break;
			end;
		end;
		
		CheckTrue(foundString, 'Should find string literal with null character');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestStringExceedingMaxLength;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
	longString: string;
begin
	// Test string exceeding 255 character limit (should generate warning)
	// Delphi 12.0 increased the limit to 1023 characters
	if not (CompilerVersion < 36) then
	begin
		CheckTrue(CompilerVersion >= 36);
		Exit;
	end;


//	F2069 Line too long (more than 1023 characters)

	SetLength(longString, 1050); // Create a 300-character string
	for i := 1 to 1050 do
		longString[i] := 'A';

	sourceCode := '''' + longString + '''';

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				// Long string should generate warning
				CheckTrue(token.HasErrors, 'String exceeding 1023 chars should have warning');
				CheckTrue((Pos('1023', token.ErrorMessage) > 0) or (Pos('too long', LowerCase(token.WarningMessage)) > 0),
					Format('Warning should mention length limit. Got: "%s"', [token.WarningMessage]));
				Break;
			end;
		end;
		
		CheckTrue(foundString, 'Should find long string literal');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

procedure TDelphiTokenizerTests.TestStringWithTabCharacter;
var
	sourceCode: string;
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;
	foundString: Boolean;
begin
	// Test string with tab character (should be OK, no warning)
	sourceCode := '''Column1' + #9 + 'Column2'''; // #9 = Tab
	
	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);
		
		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.TokenKind = ptStringLiteral then
			begin
				foundString := True;
				// Tab character should be allowed without warning
				if token.HasWarnings then
					CheckTrue(False, Format('Tab character should be allowed without warning. Got: "%s"', [token.WarningMessage]))
				else
					CheckTrue(True, 'Tab character allowed in string');
				Break;
			end;
		end;
		
		CheckTrue(foundString, 'Should find string literal with tab');
	finally
		tokens.Free; // TObjectList automatically frees owned objects
	end;
end;

function TDelphiTokenizerTests.HasTriviaTokens(tokens: TObjectList): Boolean;
var
	i: Integer;
	token: TSyntaxToken;
begin
	Result := False;
	for i := 0 to tokens.Count - 1 do
	begin
		token := tokens[i] as TSyntaxToken;
		if TokenKindIsTrivia(token.TokenKind) then
		begin
			Result := True;
			Exit;
		end;
	end;
end;

initialization
	TestFramework.RegisterTest('DelphiParser\DelphiTokenizer', TDelphiTokenizerTests.Suite);
	TestFramework.RegisterTest('DelphiParser\InputStreamTests', TInputStreamTests.Suite);

end.
