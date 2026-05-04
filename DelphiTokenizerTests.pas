unit DelphiTokenizerTests;

interface

uses
	Classes, Contnrs,
	TestFramework;


type
	TInputStreamTests = class(TTestCase)
	public
		procedure TestRingBufferWrapStress;
		procedure TestHugePeekGrowth;
	published
		procedure TestReadChar;
		procedure TestPeekChar;
		procedure TestUnreadChar;
	end;


	TTokenizerTestCase = record
		FileName: string;
		CaseIndex: Integer;
		Name: string;
		Description: string;
		SourceCode: string;
		ExpectedTokens: string;
		ExpectedValues: string;
		Errors: string;
	end;

	TDelphiTokenizerTests = class(TTestCase)
	protected
		function HasTriviaTokens(tokens: TObjectList): Boolean;
		function TokensToStr(Tokens: TList): string;

		class procedure RegisterDatFileTests;
		function FindDatTestsRoot: string;

		function EnumerateTestFiles: TArray<string>;
		function EnumerateTestCases(const FileName: string): TArray<TTokenizerTestCase>;
		procedure RunTestCase(const ACase: TTokenizerTestCase; Progress, Total: Integer);

		procedure RunDatCases;

		function EscapeDumpText(const S: string): string;
		function DumpTokens(const SourceCode: string): string;
		function DumpTokenValues(const SourceCode: string): string;
		procedure CompareTokens(const SourceCode, ExpectedTokens, CaseName: string);
		procedure CompareTokenValues(const SourceCode, ExpectedValues, CaseName: string);
	published
		procedure Tokenize;

		procedure TestTokenizeIncludesEofToken;
		procedure TestParserReuseResetsState;
		procedure TestTriviaIsAttachedToEOFToken;

		procedure TestWhitespace;

		// String escape sequence tests (for Section 5.2 improvements)
		procedure TestStringWithSurrogatePairs;
		procedure TestStringWithInvalidSurrogatePair;
		procedure TestStringWithNullCharacter;
		procedure TestStringExceedingMaxLength; // string line length limits were removed in Delphi 12.0+ (sorta)
		procedure TestStringWithTabCharacter;

		// BOM handling
		procedure TestBomIsSkippedOrTreatedAsTrivia;
	end;

	{	Dynamic test case: one instance per #name entry in a .dat file.
		Each appears as an individual test in the DUnit GUI/console runner.
		The test suite is named after the .dat file (e.g. "basic_cases.dat")
		and each test method is named after the #name from the file. }
	TDatFileTestCase = class(TDelphiTokenizerTests)
	private
		FCase: TTokenizerTestCase;
		FTotalTests: Integer;
	public
		constructor Create(const ACase: TTokenizerTestCase; const TotalTests: Integer); reintroduce;
		function GetName: string; override;
	published
		procedure RunDynamic;
	end;


implementation

uses
	SysUtils, Math, ComObj, Windows, ActiveX, System.IOUtils,
	DelphiTokenizer,
	Toolkit, Avatar.Exceptions,
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
		CheckEquals(Ord(ptEof), Ord(lastToken.Kind), 'Final token kind should be ptEof');
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
	tree: TSyntaxTree;

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
			unit %s;
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
				CheckEqualsString(FIRST_UNIT_NAME, ExtractUnitName(tree.Root),
					'First parse should capture the correct unit name');
			finally
				tree.Free;
			end;

			tree := parser.Parse(tokens2);
			try
				CheckEqualsString(SECOND_UNIT_NAME, ExtractUnitName(tree.Root),
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
		CheckEquals(Ord(ptEof), Ord(lastToken.Kind), 'Final token kind should be ptEof');

		CheckEquals(1, lastToken.LeadingTriviaCount, 'LeadingTriviaCount');
		CheckTrue(lastToken.LeadingTrivia[0].Kind = ptSlashesComment, 'leading trivia kind not ptSlashesComment');
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.RunTestCase(const ACase: TTokenizerTestCase; Progress, Total: Integer);
begin
	CheckFalse(Trim(ACase.SourceCode)   = '', 'Case "' + ACase.Name + '" is missing #code');
	CheckFalse(Trim(ACase.ExpectedTokens) = '', 'Case "' + ACase.Name + '" is missing #tokens');

	Status(Format('[CASE %d/%d] %s', [Progress, Total, ACase.Name]));
	Status(ACase.Description);

	CompareTokens(ACase.SourceCode, ACase.ExpectedTokens, ACase.Name);
	if Trim(ACase.ExpectedValues) <> '' then
		CompareTokenValues(ACase.SourceCode, ACase.ExpectedValues, ACase.Name);
end;

function TDelphiTokenizerTests.EscapeDumpText(const S: string): string;
begin
	Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
	Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
	Result := StringReplace(Result, CRLF, '\r\n', [rfReplaceAll]);
	Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
	Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
	Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
end;

function TDelphiTokenizerTests.DumpTokens(const SourceCode: string): string;
var
	tokens: TObjectList;
	i, j: Integer;
	token, trivia: TSyntaxToken;

	function FormatToken(t: TSyntaxToken): string;
	begin
		Result := TokenKindToStr(t.ContextualKind) + ' "' + EscapeDumpText(t.Text) + '"';
		if t.HasErrors   then Result := Result + ' [ERROR: '   + t.ErrorMessage   + ']';
		if t.HasWarnings then Result := Result + ' [WARNING: ' + t.WarningMessage + ']';
		if t.IsMissing   then Result := Result + ' [MISSING]';
	end;

	procedure Append(const Line: string);
	begin
		if Result = '' then Result := Line
		else Result := Result + CRLF + Line;
	end;

begin
	Result := '';
	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(SourceCode, tokens);
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if TokenKindIsTrivia(token.Kind) then Continue;

			// Emit the main token line
			Append(FormatToken(token));

			// Emit non-whitespace leading trivia as indented children (source attribution)
			for j := 0 to token.LeadingTriviaCount - 1 do
			begin
				trivia := token.LeadingTrivia[j];
				if not (trivia.Kind in [ptWhitespace, ptSpace, ptCRLF, ptCRLFCo]) then
					Append(#9 + 'leading: ' + FormatToken(trivia));
			end;
		end;
	finally
		tokens.Free;
	end;
end;

function TDelphiTokenizerTests.DumpTokenValues(const SourceCode: string): string;
var
	tokens: TObjectList;
	i: Integer;
	token: TSyntaxToken;

	procedure Append(const Line: string);
	begin
		if Result = '' then Result := Line
		else Result := Result + CRLF + Line;
	end;

begin
	Result := '';
	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(SourceCode, tokens);
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if TokenKindIsTrivia(token.Kind) then Continue;
			if token.ValueText = token.Text then Continue;

			Append(TokenKindToStr(token.Kind) + ' "' + EscapeDumpText(token.ValueText) + '"');
		end;
	finally
		tokens.Free;
	end;
end;

procedure TDelphiTokenizerTests.CompareTokens(const SourceCode, ExpectedTokens, CaseName: string);
var
	actual, expected: string;
begin
	expected := TrimRight(ExpectedTokens);

	actual := DumpTokens(SourceCode);
	actual := TrimRight(actual);
	if actual <> expected then
	begin
		Status('--- Expected ---' + CRLF + expected);
		Status('--- Actual   ---' + CRLF + actual);
	end;

	CheckEqualsString(expected, actual, Format('Token mismatch in case "%s"', [CaseName]));
end;

procedure TDelphiTokenizerTests.CompareTokenValues(const SourceCode, ExpectedValues, CaseName: string);
var
	actual, expected: string;
begin
	expected := TrimRight(ExpectedValues);
	actual := DumpTokenValues(SourceCode);
	actual := TrimRight(actual);
	if actual <> expected then
	begin
		Status('--- Expected Values ---' + CRLF + expected);
		Status('--- Actual Values ---'   + CRLF + actual);
	end;
	CheckEqualsString(expected, actual,
		Format('Token value mismatch in case "%s"', [CaseName]));
end;

procedure TDelphiTokenizerTests.RunDatCases;
var
	files: TArray<string>;
	cases: TArray<TTokenizerTestCase>;
	fileIdx, caseIdx, progress, total: Integer;
begin
	files := EnumerateTestFiles;

	total := 0;
	for fileIdx := 0 to High(files) do
	begin
		cases := EnumerateTestCases(files[fileIdx]);
		Inc(total, Length(cases));
	end;

	progress := 0;
	for fileIdx := 0 to High(files) do
	begin
		cases := EnumerateTestCases(files[fileIdx]);
		for caseIdx := 0 to High(cases) do
		begin
			Inc(progress);
			RunTestCase(cases[caseIdx], progress, total);
		end;
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
	// Need to make the test files handle checking whitespace trivia
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
				if (triviaToken.Kind = ptWhitespace) or (triviaToken.Kind = ptCRLF) then
					foundWhitespace := True;
			end;

			for j := 0 to token.TrailingTriviaCount - 1 do
			begin
				triviaToken := token.TrailingTrivia[j];
				if (triviaToken.Kind = ptWhitespace) or (triviaToken.Kind = ptCRLF) then
					foundWhitespace := True;
			end;
		end;

		CheckTrue(foundWhitespace, 'Should recognize whitespace (including line endings) in trivia');
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
	// Can't replicate this is the test files
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
			if token.Kind = ptStringLiteral then
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
	// Can't replicate this is the test files
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
			if token.Kind = ptStringLiteral then
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
	// Can't replicate this is the test files
	// Test string containing null character (should not cause any issues at all)
	sourceCode := '''Hello' + #0 + 'World''';

	tokens := TObjectList.Create(True); // owns objects
	try
		TDelphiTokenizer.Tokenize(sourceCode, tokens);

		foundString := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if token.Kind = ptStringLiteral then
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
			if token.Kind = ptStringLiteral then
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
			if token.Kind = ptStringLiteral then
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

procedure TDelphiTokenizerTests.TestBomIsSkippedOrTreatedAsTrivia;
var
	tokens: TObjectList;
	firstToken: TSyntaxToken;
begin
   // Can't really replicate this in the test file
//	UTF-8 BOM decodes to U+FEFF in UTF-16. The tokenizer should handle it gracefully.
//		Either skip it, or attach it as trivia.
//	Rather than, you know, crashing on the unknown chacters ?>'
	tokens := TObjectList.Create(True);
	try
		TDelphiTokenizer.Tokenize(#$FEFF + 'unit Test;', tokens);

		// Should produce at least: "unit", "Test", ";", EOF
		Check(tokens.Count >= 4,
				'BOM-prefixed source should tokenize normally, got ' + IntToStr(tokens.Count) + ' tokens');

		firstToken := tokens[0] as TSyntaxToken;
		// The first real token should be "unit", not a BOM error token
		CheckEquals(Ord(ptUnit), Ord(firstToken.Kind),
				'First token after BOM should be ptUnit');
	finally
		tokens.Free;
	end;
end;


function TDelphiTokenizerTests.FindDatTestsRoot: string;
const
	CANDIDATES: array[0..4] of string = (
			'TestData',
			'Library\DelphiParser\TestData\Tokenizer',
			'..\Library\DelphiParser\TestData\Tokenizer',
			'..\..\Library\DelphiParser\TestData\Tokenizer',
			'..\..\..\Library\DelphiParser\TestData\Tokenizer'
	);
var
	baseDir, candidate: string;
	i: Integer;
begin
	Result := '';

	baseDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
	for i := Low(CANDIDATES) to High(CANDIDATES) do
	begin
		candidate := ExpandFileName(baseDir + CANDIDATES[i]);
		if DirectoryExists(candidate) then
			Exit(candidate);
	end;

	Fail('Could not locate test data folder (expected Library\DelphiParser\TestData)');
end;



function TDelphiTokenizerTests.EnumerateTestCases(const FileName: string): TArray<TTokenizerTestCase>;
var
	lines: TArray<string>;
	i, count: Integer;
	line, section, payload: string;
	cur: TTokenizerTestCase;
	nextIndex: Integer;

	procedure AppendLine(var S: string; const Value: string);
	begin
		if S = '' then
			S := Value
		else
			S := S + CRLF + Value;
	end;

	procedure ResetCurrent;
	begin
		cur.Name := '';
		cur.Description := '';
		cur.SourceCode := '';
		cur.ExpectedTokens := '';
		cur.ExpectedValues := '';
		cur.Errors := '';
		cur.FileName := FileName;
		cur.CaseIndex := nextIndex;
	end;

	procedure FlushCurrent;
	begin
		if (Trim(cur.SourceCode) = '') and (Trim(cur.ExpectedTokens) = '') then
			Exit;
		if cur.Name = '' then
			cur.Name := Format('%s:%d', [ExtractFileName(FileName), cur.CaseIndex]);
		Inc(count);
		SetLength(Result, count);
		Result[count-1] := cur;
		Inc(nextIndex);
		ResetCurrent;
	end;

begin
	count := 0;
	nextIndex := 1;
	section := '';
	ResetCurrent;

	lines := TFile.ReadAllLines(FileName, TEncoding.UTF8);
	for i := 0 to Length(lines)-1 do
	begin
		line := lines[i];
		line := TrimRight(line); //remove trailing whitespace

		if SameText(line, '#name') then
		begin
			// New case boundary: finalize the previous case before reading the next name.
			FlushCurrent;
			section := 'name';
			Continue;
		end;
		if SameText(line, '#description') then
		begin
			section := 'description';
			Continue;
		end;
		if SameText(line, '#code') then
		begin
			section := 'data';
			Continue;
		end;
		if SameText(line, '#errors') then
		begin
			section := 'errors';
			Continue;
		end;
		if SameText(line, '#tokens') then
		begin
			section := 'tokens';
			Continue;
		end;
		if SameText(line, '#values') then
		begin
			section := 'values';
			Continue;
		end;

		payload := line;
		if section = 'tokens' then
			AppendLine(cur.ExpectedTokens, payload)
		else if section = 'values' then
			AppendLine(cur.ExpectedValues, payload)
		else if section = 'data' then
			AppendLine(cur.SourceCode, payload)
		else if section = 'errors' then
			AppendLine(cur.Errors, payload)
		else if section = 'description' then
			AppendLine(cur.Description, payload)
		else if section = 'name' then
			AppendLine(cur.Name, payload);
	end;

	FlushCurrent;
end;

function TDelphiTokenizerTests.EnumerateTestFiles: TArray<string>;
var
	root: string;
begin
{
	./TestData/Tokenzier/01 - basic.test
}
	root := FindDatTestsRoot;
	Result := TDirectory.GetFiles(root, '*.dat', TSearchOption.soAllDirectories);
	CheckTrue(Length(Result) > 0, 'No .dat files found in ' + root);
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
		if TokenKindIsTrivia(token.Kind) then
		begin
			Result := True;
			Exit;
		end;
	end;
end;

class procedure TDelphiTokenizerTests.RegisterDatFileTests;
const
	CANDIDATES: array[0..4] of string = (
			'TestData',
			'Library\DelphiParser\TestData\tokenizer',
			'..\Library\DelphiParser\TestData\tokenizer',
			'..\..\Library\DelphiParser\TestData\tokenizer',
			'..\..\..\Library\DelphiParser\TestData\tokenizer'
	);
var
	root, baseDir, fileName: string;
	files: TArray<string>;
	cases: TArray<TTokenizerTestCase>;
	fileSuite: ITestSuite;
	i: Integer;
	helper: TDelphiTokenizerTests;
begin
	{ Locate test data folder without calling Fail() (we are in initialization) }
	baseDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
	root := '';
	for i := Low(CANDIDATES) to High(CANDIDATES) do
	begin
		root := ExpandFileName(baseDir + CANDIDATES[i]);
		if DirectoryExists(root) then
			Break;
		root := '';
	end;
	if root = '' then
		Exit; { silently skip if test data not found during initialization }

	files := TDirectory.GetFiles(root, '*.dat', TSearchOption.soAllDirectories);
	if Length(files) = 0 then
		Exit;

	helper := TDelphiTokenizerTests.Create('Test_DatFiles');
	try
		for fileName in files do
		begin
			cases := helper.EnumerateTestCases(fileName);
			if Length(cases) = 0 then
				Continue;
			fileSuite := TTestSuite.Create(ExtractFileName(fileName));
			for i := 0 to High(cases) do
				fileSuite.AddTest(TDatFileTestCase.Create(cases[i], Length(cases)));
			TestFramework.RegisterTest('DelphiParser/tokenizer', fileSuite);
		end;
	finally
		helper.Free;
	end;
end;


{ TDatFileTestCase }

constructor TDatFileTestCase.Create(const ACase: TTokenizerTestCase; const TotalTests: Integer);
begin
	inherited Create('RunDynamic');
	FCase := ACase;
	FTotalTests := TotalTests;
end;

function TDatFileTestCase.GetName: string;
begin
	Result := FCase.Name;
end;

procedure TDatFileTestCase.RunDynamic;
begin
	RunTestCase(FCase, FCase.CaseIndex, FTotalTests);
end;

initialization
	TestFramework.RegisterTest('DelphiParser\InputStreamTests', TInputStreamTests.Suite);
//	TestFramework.RegisterTest('DelphiParser\DelphiTokenizer', TDelphiTokenizerTests.Suite);
	TDelphiTokenizerTests.RegisterDatFileTests;

end.
