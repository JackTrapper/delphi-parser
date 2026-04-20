unit DelphiParserTests;

interface

uses
	Classes, Contnrs,
	DelphiParser, TestFramework;

type
	TDatParserCase = record
		Name: string;
		Description: string;
		SourceCode: string;
		ExpectedTree: string;
		Errors: string;
		FileName: string;
		CaseIndex: Integer;
	end;

	TDelphiParserTests = class(TTestCase)
	protected
		function ExpectedToTree(ExpectedTree: string): TSyntaxTree;
		function TryExpectedToTree(const ExpectedTree: string; out Node: TSyntaxNode2; out ErrorCode: HRESULT; out ErrorMessage: string): Boolean;

		function CompareSource(sourceCode, ExpectedTree: string; const CaseName: string): Boolean;
		function CompareTrees(expected, actual: TSyntaxNode2): Boolean;
		function CompareNodes(expected, actual: TSyntaxNode2): Boolean;

		function FindDatTestsRoot: string;

		function EnumerateDatFiles: TArray<string>;
		function EnumerateDatTestCases(const FileName: string): TArray<TDatParserCase>;
		procedure RunDatCase(const ACase: TDatParserCase; Progress, Total: Integer);

		procedure Test_DatFiles; deprecated 'moved to dynamic test suites';
	published
		procedure Test_ExpectedToTree_SiblingsAtSameIndent;
		procedure Test_ExpectedToTree_AttributesParsing;
		procedure Test_ExpectedToTree_DeepHierarchy;
		procedure Test_ExpectedToTree_BadIndent_Raises;
		procedure Test_ExpectedToTree_UnterminatedQuote_Raises;
		procedure Test_ExpectedToTree_LineWithoutNt_Raises;
		procedure Test_ExpectedToTree_UnknownNodeType_Raises;
		procedure Test_ExpectedToTree_FirstLineNotRoot_Raises;

		procedure Test_UnitPortabilityDirectives;

		procedure Test_ParseConstWithTrailingDecimalLiteral;

		procedure Test_ParseResStringSection;						// resourcestring SProduct = 'Contoso';
		procedure Test_ParseResStringSection_Concatenated;		// resourcestring SProduct = 'A' + 'B';
		procedure Test_ParseConstSection;

		procedure Test_ParseFieldSection;

		procedure Test_ParseClassType;
		procedure Test_ParseConstructorMethodHeading;

		procedure Test_EmptyCase;
		procedure Test_CaseElseSemicolonHandling;
	end;

	{	Dynamic test case: one instance per #name entry in a .dat file.
		Each appears as an individual test in the DUnit GUI/console runner.
		The test suite is named after the .dat file (e.g. "basic_cases.dat")
		and each test method is named after the #name from the file. }
	TDatFileTestCase = class(TDelphiParserTests)
	private
		FCase: TDatParserCase;
		FTotalTests: Integer;
	public
		constructor Create(const ACase: TDatParserCase; const TotalTests: Integer); reintroduce;
		function GetName: string; override;
	published
		procedure RunDynamic;
	end;



implementation

uses
	SysUtils, Math, ComObj, Windows, ActiveX, TypInfo, IOUtils,
	DelphiTokenizer,
	Toolkit;


{ TDelphiParserTests }

function TDelphiParserTests.FindDatTestsRoot: string;
const
	CANDIDATES: array[0..4] of string = (
			'TestData',
			'Library\DelphiParser\TestData',
			'..\Library\DelphiParser\TestData',
			'..\..\Library\DelphiParser\TestData',
			'..\..\..\Library\DelphiParser\TestData'
	);
var
	baseDir, candidate: string;
	i: Integer;
begin
	baseDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
	for i := Low(CANDIDATES) to High(CANDIDATES) do
	begin
		candidate := ExpandFileName(baseDir + CANDIDATES[i]);
		if DirectoryExists(candidate) then
			Exit(candidate);
	end;
	Fail('Could not locate test data folder (expected Library\DelphiParser\TestData)');
	Result := '';
end;

function TDelphiParserTests.EnumerateDatFiles: TArray<string>;
var
	root: string;
begin
	root := FindDatTestsRoot;
	Result := TDirectory.GetFiles(root, '*.dat', TSearchOption.soAllDirectories);
	CheckTrue(Length(Result) > 0, 'No .dat files found in ' + root);
end;

function TDelphiParserTests.EnumerateDatTestCases(const FileName: string): TArray<TDatParserCase>;
var
	lines: TArray<string>;
	i, count: Integer;
	line, section, payload: string;
	cur: TDatParserCase;
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
		cur.SourceCode := '';
		cur.ExpectedTree := '';
		cur.Errors := '';
		cur.FileName := FileName;
		cur.CaseIndex := nextIndex;
	end;

	procedure FlushCurrent;
	begin
		if (Trim(cur.SourceCode) = '') and (Trim(cur.ExpectedTree) = '') then
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

	lines := TFile.ReadAllLines(FileName);
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
		if SameText(line, '#data') then
		begin
			section := 'data';
			Continue;
		end;
		if SameText(line, '#errors') then
		begin
			section := 'errors';
			Continue;
		end;
		if SameText(line, '#document') then
		begin
			section := 'document';
			Continue;
		end;

		payload := line;
		if section = 'document' then
			AppendLine(cur.ExpectedTree, payload)
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

procedure TDelphiParserTests.RunDatCase(const ACase: TDatParserCase; Progress, Total: Integer);
begin
	CheckFalse(Trim(ACase.SourceCode)   = '', 'Case "' + ACase.Name + '" is missing #data');
	CheckFalse(Trim(ACase.ExpectedTree) = '', 'Case "' + ACase.Name + '" is missing #document');

	Status(Format('[CASE %d/%d] %s', [Progress, Total, ACase.Name]));
	Status(	ACase.Description);

//	Status('Test name: '+ACase.Name + ' (' + ExtractFileName(ACase.FileName) + ')'+CRLF+CRLF);

	CompareSource(ACase.SourceCode, ACase.ExpectedTree, ACase.Name);
end;

function TDelphiParserTests.CompareSource(sourceCode, ExpectedTree: string; const CaseName: string): Boolean;
var
	expected, actual: TSyntaxTree;

	procedure PrintResults(bSuccess: Boolean; const ErrorMsg: string = '');
	var
		s: string;
		actualDump: string;
	begin
		if bSuccess then
		begin
			Status('             '+CaseName+' [PASS]');
			Exit;
		end;

		if actual <> nil then
			actualDump := TSyntaxNode2.DumpTree(actual.Root)
		else
			actualDump := '<actual=nil>';

		s := CRLF +
				'Code'+CRLF+
				'----'+CRLF+
				sourceCode+CRLF+CRLF+
				'ExpectedTree'+CRLF+
				'------------'+CRLF+
				ExpectedTree+CRLF+CRLF+
				'ActualTree'+CRLF+
				'----------'+CRLF+
				actualDump;

		Status(s);

		if ErrorMsg <> '' then
			Fail(ErrorMsg)
		else
			Fail('Mismatched syntax trees');
	end;

begin
	CheckFalse(sourceCode	='');
	CheckFalse(ExpectedTree	='');
	Result := False;

	expected := nil;
	actual := nil;
	try
		// Build the expected tree
		try
			expected := ExpectedToTree(expectedTree);
			CheckTrue(Assigned(expected));
		except
			on E:Exception do
				begin
					PrintResults(False, E.Message);
				end;
		end;

		// Parse the source
		try
			actual := TDelphiParser.ParseText(sourceCode, '');
			CheckTrue(Assigned(actual));
		except
			on E:Exception do
				begin
					PrintResults(False, E.Message);
				end;
		end;

		// and compare the trees
		try
			Result := CompareTrees(expected.Root, actual.Root);
			PrintResults(Result);
		except
			on E:Exception do
			begin
				PrintResults(False, E.Message);
			end;
		end;
	finally
		FreeAndNil(expected);
		FreeAndNil(actual);
	end;
end;

function TDelphiParserTests.CompareTrees(expected, actual: TSyntaxNode2): Boolean;
begin
	CheckTrue(Assigned(expected), 'expected node not assigned');
	CheckTrue(Assigned(actual),	'actual node not assigned');

//	Status(CRLF+CRLF+'Expected'+CRLF+'========'+CRLF+CRLF+TSyntaxNode2.DumpTree(expected));
//	Status(CRLF+CRLF+'Actual  '+CRLF+'========'+CRLF+CRLF+TSyntaxNode2.DumpTree(actual));

	Result := CompareNodes(expected, actual);
end;

function TDelphiParserTests.CompareNodes(expected, actual: TSyntaxNode2): Boolean;
var
	child1, child2: TSyntaxNodeOrToken;
	i, j: Integer;
	attr: TAttributeName;
	expVal, actVal: string;
	expNodeCount, actNodeCount: Integer;
	nodeName: string;
begin
	CheckTrue(Assigned(expected),	'expected node not assigned');
	CheckTrue(Assigned(actual),	'actual node not assigned');

	nodeName := GetEnumName(TypeInfo(TSyntaxNodeType), Ord(expected.NodeType));

	// Compare node types
	CheckEquals(
		GetEnumName(TypeInfo(TSyntaxNodeType), Ord(expected.NodeType)),
		GetEnumName(TypeInfo(TSyntaxNodeType), Ord(actual.NodeType)),
		'NodeType mismatch');

	// Compare attributes
	for attr := Low(TAttributeName) to High(TAttributeName) do
	begin
		expVal := expected.Attributes[attr];
		// If expected value is empty, treat as "don't care" and do not assert.
		if expVal = '' then
			Continue;
		actVal := actual.Attributes[attr];
		CheckEqualsString(expVal, actVal, nodeName+': Attribute '+AttributeNameToStr(attr));
	end;

	// Count node-only children on both sides
	// Expected tree never contains tokens, so count is just ChildNodes.Count
	expNodeCount := expected.ChildNodes.Count;

	actNodeCount := 0;
	for i := 0 to actual.ChildNodes.Count-1 do
		if actual.ChildNodes[i].IsNode then
			Inc(actNodeCount);

	CheckEquals(expNodeCount, actNodeCount,
		nodeName + ': child node count');

	// Walk expected children, skipping tokens in actual
	j := 0;
	for i := 0 to expected.ChildNodes.Count-1 do
	begin
		child1 := expected.ChildNodes[i];

		// Skip token children in actual
		while (j < actual.ChildNodes.Count) and actual.ChildNodes[j].IsToken do
			Inc(j);

		// Safety check (should never hit this if counts matched above)
		CheckTrue(j < actual.ChildNodes.Count,
			nodeName + ': ran out of actual child nodes at expected child ' + IntToStr(i));

		child2 := actual.ChildNodes[j];
		Inc(j);

		// Expected tree only has nodes, so this should always be true
		if child1.IsToken then
			Continue;

		CheckTrue(Self.CompareNodes(child1.AsNode, child2.AsNode));
	end;

	Result := True;
end;


function TDelphiParserTests.TryExpectedToTree(const ExpectedTree: string; out Node: TSyntaxNode2; out ErrorCode: HRESULT; out ErrorMessage: string): Boolean;
var
	sl: TStrings;
	i: Integer;

	function CountLeadingTabs(const s: string): Integer;
	var
		n: Integer;
	begin
		n := 0;
		while (n < Length(s)) and (s[n+1] = #9) do Inc(n);
		Result := n;
	end;

	function StripLeadingTabs(const s: string; tabs: Integer): string;
	begin
		Result := Copy(s, tabs+1, Max(0, Length(s) - tabs));
	end;

	function NextToken(var s: string): string;
	var
		p, len: Integer;
	begin
		s := TrimLeft(s);
		if s = '' then Exit('');
		p := 1; len := Length(s);
		while (p <= len) and (not CharInSet(s[p], [' ', #9])) do Inc(p);
		Result := Copy(s, 1, p-1);
		s := TrimLeft(Copy(s, p, len - p + 1));
	end;

	function Dequote(const v: string): string;
	begin
		if (Length(v) >= 2) and (v[1] = '"') and (v[Length(v)] = '"') then
			Result := Copy(v, 2, Length(v)-2)
		else
			Result := v;
	end;

	function ParseNodeTypeSafe(const nodeStr: string; out nodeType: TSyntaxNodeType; out msg: string): Boolean;
	var
		ordVal: Integer;
		resolvedName: string;
	begin
		msg := '';
		try
			ordVal := GetEnumValue(TypeInfo(TSyntaxNodeType), nodeStr);
		except
			on E: Exception do begin
				msg := Format('Unknown node type: %s', [nodeStr]);
				Exit(False);
			end;
		end;
		if (ordVal < Ord(Low(TSyntaxNodeType))) or (ordVal > Ord(High(TSyntaxNodeType))) then
		begin
			msg := Format('Unknown node type: %s', [nodeStr]);
			Exit(False);
		end;
		resolvedName := GetEnumName(TypeInfo(TSyntaxNodeType), ordVal);
		if not SameText(resolvedName, nodeStr) then
		begin
			msg := Format('Unknown node type: %s', [nodeStr]);
			Exit(False);
		end;
		nodeType := TSyntaxNodeType(ordVal);
		Result := True;
	end;

	function SetNodeAttributeByName(node: TSyntaxNode2; const name, value: string; out msg: string): Boolean;
	var
		ordVal: Integer;
		v: string;
	begin
		msg := '';
		ordVal := GetEnumValue(TypeInfo(TAttributeName), name);
		if ordVal < 0 then
		begin
			msg := Format('Unknown attribute name: %s', [name]);
			Exit(False);
		end;
		v := Dequote(value);
		node.Attributes[TAttributeName(ordVal)] := v;
		Result := True;
	end;

	function ParseAttributesSafe(node: TSyntaxNode2; var rest: string; lineIndex: Integer; out msg: string): Boolean;
	var
		s, name, value: string;
		eqPos: Integer;
	begin
		Result := True;
		msg := '';
		s := TrimLeft(rest);
		while s <> '' do
		begin
			eqPos := Pos('=', s);
			if eqPos = 0 then
			begin
				msg := Format('Expected attribute on line %d to contain =', [lineIndex+1]);
				Exit(False);
			end;
			name := Trim(Copy(s, 1, eqPos-1));
			s := TrimLeft(Copy(s, eqPos+1, Max(0, Length(s)-eqPos)));
			if (s <> '') and (s[1] = '"') then
			begin
				eqPos := 2;
				while (eqPos <= Length(s)) and (s[eqPos] <> '"') do Inc(eqPos);
				if (eqPos > Length(s)) or (s[eqPos] <> '"') then
				begin
					msg := Format('Unterminated quoted attribute value on line %d', [lineIndex+1]);
					Exit(False);
				end;
				value := Copy(s, 1, eqPos);
				s := TrimLeft(Copy(s, eqPos+1, Max(0, Length(s)-eqPos)));
			end else begin
				eqPos := 1;
				while (eqPos <= Length(s)) and (not CharInSet(s[eqPos], [' ', #9])) do Inc(eqPos);
				value := Copy(s, 1, eqPos-1);
				s := TrimLeft(Copy(s, eqPos, Max(0, Length(s)-eqPos+1)));
			end;
			if not SetNodeAttributeByName(node, name, value, msg) then
				Exit(False);
		end;
		rest := s;
	end;

	function ValidateStructureSafe(lines: TStrings; out msg: string): Boolean;
	var
		i, prevIndent, indent: Integer;
		line, work, nodeName: string;
		dummy: TSyntaxNode2;
		nodeType: TSyntaxNodeType;
	begin
		msg := '';
		if (lines.Count = 0) or (Trim(lines[0]) <> 'ntCompilationUnit') then
		begin
			msg := 'Expected first line to be ntCompilationUnit';
			Exit(False);
		end;

		prevIndent := 0;
		for i := 1 to lines.Count - 1 do
		begin
			line := lines[i];
			if Trim(line) = '' then
				Continue;
			indent := CountLeadingTabs(line);
			work := StripLeadingTabs(line, indent);
			work := TrimLeft(work);
			if Copy(work, 1, 2) <> 'nt' then
			begin
				msg := Format('Expected line %d to start with "nt"', [i+1]);
				Exit(False);
			end;
			if indent > prevIndent + 1 then
			begin
				msg := Format('Line %d is too deeply indented without a previous parent node', [i+1]);
				Exit(False);
			end;
			nodeName := NextToken(work);
			if not ParseNodeTypeSafe(nodeName, nodeType, msg) then
				Exit(False);
			if Trim(work) <> '' then
			begin
				dummy := TSyntaxNode2.Create(ntUnknown);
				try
					if not ParseAttributesSafe(dummy, work, i, msg) then
						Exit(False);
				finally
					dummy.Free;
				end;
			end;
			prevIndent := indent;
		end;
		Result := True;
	end;

	procedure ParseIndentedSafe(parent: TSyntaxNode2; parentIndent: Integer; lines: TStrings; var idx: Integer; out msg: string);
	var
		line, work, nodeName: string;
		indent: Integer;
		child, lastChild: TSyntaxNode2;
		wrapper: TSyntaxNodeOrToken;
		nodeType: TSyntaxNodeType;
	begin
		msg := '';
		while idx < lines.Count do
		begin
			line := lines[idx];
			if Trim(line) = '' then begin Inc(idx); Continue; end;
			indent := CountLeadingTabs(line);
			if indent <= parentIndent then Exit;
			if indent > parentIndent + 1 then
			begin
				if (parent.ChildNodes.Count = 0) or (not parent.ChildNodes[parent.ChildNodes.Count-1].IsNode) then
				begin
					msg := Format('Line %d is too deeply indented without a previous parent node', [idx+1]);
					Exit;
				end;
				lastChild := parent.ChildNodes[parent.ChildNodes.Count-1].AsNode;
				ParseIndentedSafe(lastChild, parentIndent+1, lines, idx, msg);
				if msg <> '' then Exit;
				Continue;
			end;
			work := StripLeadingTabs(line, indent);
			work := TrimLeft(work);
			if Copy(work, 1, 2) <> 'nt' then
			begin
				msg := Format('Expected line %d to start with "nt"', [idx+1]);
				Exit;
			end;
			nodeName := NextToken(work);
			if not ParseNodeTypeSafe(nodeName, nodeType, msg) then Exit;
			child := TSyntaxNode2.Create(nodeType);
			try
				if Trim(work) <> '' then
					if not ParseAttributesSafe(child, work, idx, msg) then Exit;
				wrapper := TSyntaxNodeOrToken.Create(child);
				try
					parent.ChildNodes.Add(wrapper);
					lastChild := child;
					wrapper := nil;
					child := nil;
				finally
					wrapper.Free;
				end;
			finally
				child.Free;
			end;
			Inc(idx);
			if (idx < lines.Count) and (CountLeadingTabs(lines[idx]) > indent) then
				ParseIndentedSafe(lastChild, indent, lines, idx, msg);
			if msg <> '' then Exit;
		end;
	end;

var
	msg: string;
begin
	Node := nil;
	ErrorCode := S_OK;
	ErrorMessage := '';

	sl := TStringList.Create;
	try
		sl.Text := ExpectedTree;
		if not ValidateStructureSafe(sl, msg) then
		begin
			ErrorCode := E_INVALIDARG;
			ErrorMessage := msg;
			Exit(False);
		end;

		Node := TSyntaxNode2.Create(ntCompilationUnit);
		i := 1;
		ParseIndentedSafe(Node, 0, sl, i, msg);
		if msg <> '' then
		begin
			FreeAndNil(Node);
			ErrorCode := E_INVALIDARG;
			ErrorMessage := msg;
			Exit(False);
		end;

		Result := True;
	finally
		sl.Free;
	end;
end;

function TDelphiParserTests.ExpectedToTree(ExpectedTree: string): TSyntaxTree;
var
	root: TSyntaxNode2;
	errorCode: HRESULT;
	errorMessage: string;
begin
	if not TryExpectedToTree(ExpectedTree, root, errorCode, errorMessage) then
	begin
		Fail(errorMessage);
		Result := nil; // Fails throws an exception, but you get the idea
		Exit;
	end;

	Result := TSyntaxTree.Create(root);
end;

procedure TDelphiParserTests.Test_DatFiles;
var
	files: TArray<string>;
	fileName: string;
	cases: TArray<TDatParserCase>;
	testCase: TDatParserCase;
	i: Integer;
//	failures: TStringList;
	nTest: Integer;
	nFailures: Integer;
	continueOnFailure: Boolean;
const
	// We're trying to find which test case is exposing the leak.
	MAX_TESTS: Integer = 0;
begin
//	Status('DateFiles test disabled');
//	Exit;

	nTest := 0;
	nFailures := 0;

	continueOnFailure := True; //FindCmdLineSwitch('continueOnFailure');

	//1. List of files
	files := EnumerateDatFiles;
	for fileName in files do
	begin
		//2. List of tests in the file
		cases := EnumerateDatTestCases(fileName);
		CheckTrue(Length(cases) > 0, 'No test cases found in ' + fileName);

		//3. Enumerate all the tests
		for i := 0 to High(cases) do
		begin
			if (MAX_TESTS > 0) and (nTest >= MAX_TESTS) then
				Exit;

			testCase := cases[i];
			try
				//Inc(nTest);
				RunDatCase(testCase, i+1, Length(cases));
			except
				on E:Exception do
					begin
						Inc(nFailures);
						if continueOnFailure then
							Status('[TEST ERROR] '+testCase.Name+' ('+E.Message+')')
						else
							Fail('[TEST ERROR] '+testCase.Name+' ('+E.Message+')');
						Continue;
					end;
			end;
		end;
	end;

	if nFailures > 0 then
		Fail(Format('Test_DatFiles had %d failing case(s). See [TEST ERROR] entries for details.', [nFailures]));
end;

procedure TDelphiParserTests.Test_ParseConstSection;
var
	sourceCode: string;
	expectedTree: string;
begin
	sourceCode := '''
unit Unit1;
interface
const
   A = 1;
	B= 2;
   C =3;
implementation
end.
''';

	expectedTree := '''
ntCompilationUnit
	ntUnitDeclaration anName="Unit1"
		ntQualifiedIdentifier anName="Unit1"
		ntInterfaceSection
			ntConstants
				ntConstant anName="A" anValueText="1"
					ntIdentifier anName="A"
					ntExpression anValueText="1"
				ntConstant anName="B" anValueText="2"
					ntIdentifier anName="B"
					ntExpression anValueText="2"
				ntConstant anName="C" anValueText="3"
					ntIdentifier anName="C"
					ntExpression anValueText="3"
		ntImplementation
''';

	CompareSource(sourceCode, expectedTree, 'Test_ParseConstSection');
end;

procedure TDelphiParserTests.Test_ParseResStringSection;
var
	sourceCode: string;
	expectedTree: string;
begin
	sourceCode := '''
unit TestCase.ParseStringSection;
interface
resourcestring
	SProduct = 'Hello world!';
implementation
end.
''';

	expectedTree := '''
ntCompilationUnit
	ntUnitDeclaration anName="TestCase.ParseStringSection"
		ntQualifiedIdentifier anName="TestCase.ParseStringSection"
		ntInterfaceSection
			ntResourceStrings
				ntResourceString anName="SProduct" anValueText="Hello world!"
					ntExpression anValueText="Hello world!"
		ntImplementation
''';

	CompareSource(sourceCode, expectedTree, 'Test_ParseResStringSection');
end;

procedure TDelphiParserTests.Test_ParseResStringSection_Concatenated;
var
	sourceCode: string;
	expectedTree: string;
begin
	sourceCode := '''
unit Unit1;
interface
resourcestring
	S1 = 'a'+'b';
implementation
end.
''';

	expectedTree := '''
ntCompilationUnit
	ntUnitDeclaration anName="Unit1"
		ntQualifiedIdentifier anName="Unit1"
		ntInterfaceSection
			ntResourceStrings
				ntResourceString anName="S1" anValueText="ab"
					ntExpression anValueText="ab"
		ntImplementation
''';
//ERRORS
//	E2026 Constant expression required

	CompareSource(sourceCode, expectedTree, 'Test_ParseResStringSection_Concatenated');
end;

procedure TDelphiParserTests.Test_ParseFieldSection;
var
	sourceCode: string;
	expectedTree: string;
begin
{
{
http://dgrok.excastle.com/Grammar.html#FieldSection

FieldSection			Backlinks: VisibilitySectionContent
	-> CLASS VAR (FieldDecl)+			; class var requires at least one field
	-> VAR (FieldDecl)+					; var requires at least one field
	-> FieldDecl							; bare field (no prefix), exactly one at a time

This is a little-known syntax:

	TCustomer = class

	// the syntax we all know
	private
		A: Integer;
	protected
		B: Integer;
	public
		C: Integer;
	published
		D: Integer;

	// var is allowed too
	private
		var
			E: Integer;
	protected
		var
			F: Integer;
	public
		var
			G: Integer;
	published
		var
			H: Integer;

	// a bare var is assumed to be public
	var
		I: Integer;

	// const is also allowed in addition to var
	private
		const
			J = 7;
			K: Integer = 8;
	protected
		const
			L = 9;
			M: Integer = 10;
	public
		var
			N = 11;
			O: Integer = 12;
	published
		var
			P = 13;
			Q: Integer = 14;]
	end;

See also: https://docwiki.embarcadero.com/RADStudio/Athens/en/Fields_%28Delphi%29
}
	sourceCode := '''
unit Unit1;
interface
type
TCustomer = class
	// the syntax we all know
	private
		A: Integer;
	protected
		B: Integer;
	public
		C: Integer;
	published
		D: Integer;

	// var is allowed too
	private
		var
			E: Integer;
	protected
		var
			F: Integer;
	public
		var
			G: Integer;
	published
		var
			H: Integer;

	// a bare var is assumed to be public
	var
		I: Integer;

	// const is also allowed in addition to var
	private
		const
			J = 7;
			K: Integer = 8;
	protected
		const
			L = 9;
			M: Integer = 10;
	public
		var
			N = 11;
			O: Integer = 12;
	published
		var
			P = 13;
			Q: Integer = 14;
	end;
implementation
end.
''';

	expectedTree := '''
ntCompilationUnit
	ntUnitDeclaration anName="Unit1"
		ntQualifiedIdentifier anName="Unit1"
		ntInterfaceSection
			ntTypeSection
				ntTypeDecl anName="TCustomer"
					ntType anType="avClass"
						ntVisibilitySection anVisibility="private"
							ntField
								ntName anName="A"
								ntType anName="Integer"
						ntVisibilitySection anVisibility="protected"
							ntField
								ntName anName="B"
								ntType anName="Integer"
						ntVisibilitySection anVisibility="public"
							ntField
								ntName anName="C"
								ntType anName="Integer"
						ntVisibilitySection anVisibility="published"
							ntField
								ntName anName="D"
								ntType anName="Integer"
						ntVisibilitySection anVisibility="private"
							ntFieldSection
								ntField
									ntName anName="E"
									ntType anName="Integer"
						ntVisibilitySection anVisibility="protected"
							ntFieldSection
								ntField
									ntName anName="F"
									ntType anName="Integer"
						ntVisibilitySection anVisibility="public"
							ntFieldSection
								ntField
									ntName anName="G"
									ntType anName="Integer"
						ntVisibilitySection anVisibility="published"
							ntFieldSection
								ntField
									ntName anName="H"
									ntType anName="Integer"
						ntFieldSection
							ntField
								ntName anName="I"
								ntType anName="Integer"
						ntVisibilitySection anVisibility="private"
							ntConstants
								ntConstant anName="J" anValueText="7"
									ntIdentifier anName="J"
									ntExpression anValueText="7"
								ntConstant anName="K" anValueText="8"
									ntIdentifier anName="K"
									ntType anName="Integer"
									ntExpression anValueText="8"
						ntVisibilitySection anVisibility="protected"
							ntConstants
								ntConstant anName="L" anValueText="9"
									ntIdentifier anName="L"
									ntExpression anValueText="9"
								ntConstant anName="M" anValueText="10"
									ntIdentifier anName="M"
									ntType anName="Integer"
									ntExpression anValueText="10"
						ntVisibilitySection anVisibility="public"
							ntFieldSection
								ntConstant anName="N" anValueText="11"
									ntIdentifier anName="N"
									ntExpression anValueText="11"
								ntField
									ntName anName="O"
									ntType anName="Integer"
									ntExpression anValueText="12"
						ntVisibilitySection anVisibility="published"
							ntFieldSection
								ntConstant anName="P" anValueText="13"
									ntIdentifier anName="P"
									ntExpression anValueText="13"
								ntField
									ntName anName="Q"
									ntType anName="Integer"
									ntExpression anValueText="14"
		ntImplementation
''';

	CompareSource(sourceCode, expectedTree, 'Test_ParseFieldSection');
end;

procedure TDelphiParserTests.Test_ParseClassType;
var
	sourceCode: string;
	expectedTree: string;
begin
	sourceCode := '''
unit TestParseClassType;
interface
type
	TSpecial = class
		FTime: Integer;
	end;
implementation
end.
''';

	expectedTree := '''
ntCompilationUnit
	ntUnitDeclaration anName="TestParseClassType"
		ntQualifiedIdentifier anName="TestParseClassType"
		ntInterfaceSection
			ntTypeSection
				ntTypeDecl anName="TSpecial"
					ntType anType="avClass"
						ntField
							ntName anName="FTime"
							ntType anName="Integer"
		ntImplementation
''';

	CompareSource(sourceCode, expectedTree, 'Test_ParseClassType');
end;

procedure TDelphiParserTests.Test_ParseConstructorMethodHeading;
var
	sourceCode: string;
	expectedTree: string;
begin
	sourceCode := '''
unit U;
interface
type
	T = class
		constructor Create;
	end;
implementation
end.
''';

	expectedTree := '''
ntCompilationUnit
	ntUnitDeclaration anName="U"
		ntQualifiedIdentifier anName="U"
		ntInterfaceSection
			ntTypeSection
				ntTypeDecl anName="T"
					ntType anType="avClass"
						ntMethod anKind="avConstructor"
							ntQualifiedIdentifier anName="Create"
		ntImplementation
''';

	CompareSource(sourceCode, expectedTree, 'Test_ParseConstructorMethodHeading');
end;

procedure TDelphiParserTests.Test_ParseConstWithTrailingDecimalLiteral;
const
	Source: string =
		'unit TrailingDecimal;' + sLineBreak +
		'interface' + sLineBreak +
		'const' + sLineBreak +
		'  Value = 1.;' + sLineBreak +
		'implementation' + sLineBreak +
		'end.';
var
	tokens: TObjectList;
	parser: TDelphiParser;
	tree: TSyntaxTree;
	floatFound: Boolean;
	i: Integer;
	token: TSyntaxToken;
begin
	tokens := TObjectList.Create(False {parser will own tokens});
	try
		TDelphiTokenizer.Tokenize(Source, tokens);

		floatFound := False;
		for i := 0 to tokens.Count - 1 do
		begin
			token := tokens[i] as TSyntaxToken;
			if (token.Kind = ptFloat) and SameText(token.Text, '1.') then
			begin
				floatFound := True;
				Break;
			end;
		end;
		CheckTrue(floatFound, 'Tokenizer should classify "1." as a single ptFloat token');

		parser := TDelphiParser.Create;
		try
			tree := parser.Parse(tokens);
			try
				CheckTrue(Assigned(tree), 'Parser should successfully build a syntax tree for trailing-dot literals');
			finally
				tree.Free;
			end;
		finally
			parser.Free;
		end;
	finally
		tokens.Free;
	end;
end;

procedure TDelphiParserTests.Test_ExpectedToTree_UnknownNodeType_Raises;
var
	expected: string;
	ok: Boolean;
	root: TSyntaxNode2;
	hr: HRESULT;
	msg: string;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'ntDefinitelyNotARealNode anName="X"';

	ok := TryExpectedToTree(expected, root, hr, msg);
	if Assigned(root) then root.Free;
	CheckFalse(ok, 'Expected TryExpectedToTree to fail for unknown node type');
	CheckEquals(E_INVALIDARG, hr, 'HR should be E_INVALIDARG');
	CheckTrue(msg <> '', 'Error message should be provided');
end;

procedure TDelphiParserTests.Test_ExpectedToTree_FirstLineNotRoot_Raises;
var
	expected: string;
	ok: Boolean;
	root: TSyntaxNode2;
	hr: HRESULT;
	msg: string;
begin
	// Root line must be ntCompilationUnit
	expected := 'ntInterfaceSection';

	ok := TryExpectedToTree(expected, root, hr, msg);
	if Assigned(root) then root.Free;
	CheckFalse(ok, 'Expected TryExpectedToTree to fail when first line is not ntCompilationUnit');
	CheckEquals(E_INVALIDARG, hr);
	CheckTrue(Pos('ntCompilationUnit', msg) > 0, 'Message should mention ntCompilationUnit');
end;

procedure TDelphiParserTests.Test_ExpectedToTree_BadIndent_Raises;
var
	expected: string;
	ok: Boolean;
	root: TSyntaxNode2;
	hr: HRESULT;
	msg: string;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'ntUnitDeclaration anName="U"' + CRLF +
		// Too-deep indent without an immediate parent line preceding it
		#9#9#9'ntQualifiedIdentifier anName="U"';

	ok := TryExpectedToTree(expected, root, hr, msg);
	if Assigned(root) then root.Free;
	CheckFalse(ok, 'Expected TryExpectedToTree to fail for bad indent');
	CheckEquals(E_INVALIDARG, hr);
	CheckTrue(Pos('too deeply indented', msg) > 0, 'Message should indicate indent error');
end;

procedure TDelphiParserTests.Test_ExpectedToTree_UnterminatedQuote_Raises;
var
	expected: string;
	ok: Boolean;
	root: TSyntaxNode2;
	hr: HRESULT;
	msg: string;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'ntTypeDecl anName="TSpecial' + CRLF + // missing closing quote
		#9#9'ntType anType="avClass"';

	ok := TryExpectedToTree(expected, root, hr, msg);
	if Assigned(root) then root.Free;
	CheckFalse(ok, 'Expected TryExpectedToTree to fail for unterminated quote');
	CheckEquals(E_INVALIDARG, hr);
	CheckTrue(Pos('Unterminated', msg) > 0, 'Message should indicate unterminated quote');
end; 

procedure TDelphiParserTests.Test_ExpectedToTree_LineWithoutNt_Raises;
var
	expected: string;
	ok: Boolean;
	root: TSyntaxNode2;
	hr: HRESULT;
	msg: string;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'UnitDeclaration anName="U"'; // does not start with nt

	ok := TryExpectedToTree(expected, root, hr, msg);
	if Assigned(root) then root.Free;
	CheckFalse(ok, 'Expected TryExpectedToTree to fail when line does not start with nt');
	CheckEquals(E_INVALIDARG, hr);
	CheckTrue(Pos('start with', msg) > 0, 'Message should indicate missing nt prefix');
end;


procedure TDelphiParserTests.Test_ExpectedToTree_SiblingsAtSameIndent;
var
	expected: string;
	tree: TSyntaxTree;
	unitNode, child: TSyntaxNode2;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'ntUnitDeclaration anName="U"' + CRLF +
		#9#9'ntQualifiedIdentifier anName="U"' + CRLF +
		#9#9'ntInterfaceSection' + CRLF +
		#9#9'ntImplementation';

	tree := nil;
	try
		tree := ExpectedToTree(expected);
		CheckNotNull(tree, 'Tree should not be nil');
		CheckNotNull(tree.Root, 'Tree.Root should not be nil');

		// Root should have exactly one child: UnitDeclaration
		CheckEquals(1, tree.Root.ChildNodes.Count, 'Root should have one child');
		CheckTrue(tree.Root.ChildNodes[0].IsNode, 'Root child should be a node');
		unitNode := tree.Root.ChildNodes[0].AsNode;
		CheckEquals(Ord(ntUnitDeclaration), Ord(unitNode.NodeType), 'First child should be ntUnitDeclaration');
		CheckEqualsString('U', unitNode.Attributes[anName], 'UnitDeclaration anName should be U');

		// Under UnitDeclaration: three siblings at same indent
		CheckTrue(unitNode.ChildNodes.Count = 3, 'Unit should have three children');
		CheckTrue(unitNode.ChildNodes[0].IsNode);
		CheckTrue(unitNode.ChildNodes[1].IsNode);
		CheckTrue(unitNode.ChildNodes[2].IsNode);
		child := unitNode.ChildNodes[0].AsNode;
		CheckEquals(Ord(ntQualifiedIdentifier), Ord(child.NodeType));
		CheckEqualsString('U', child.Attributes[anName], 'QualifiedIdentifier anName should be U');
		child := unitNode.ChildNodes[1].AsNode;
		CheckEquals(Ord(ntInterfaceSection), Ord(child.NodeType));
		child := unitNode.ChildNodes[2].AsNode;
		CheckEquals(Ord(ntImplementation), Ord(child.NodeType));
	finally
		tree.Free;
	end;
end;

procedure TDelphiParserTests.Test_ExpectedToTree_AttributesParsing;
var
	expected: string;
	tree: TSyntaxTree;
	typeDecl, typeNode: TSyntaxNode2;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'ntTypeDecl anName="TSpecial"' + CRLF +
		#9#9'ntType anType="avClass"';

	tree := nil;
	try
		tree := ExpectedToTree(expected);
		CheckNotNull(tree);
		CheckNotNull(tree.Root);

		CheckEquals(1, tree.Root.ChildNodes.Count, 'Root should have one child');
		typeDecl := tree.Root.ChildNodes[0].AsNode;
		CheckEquals(Ord(ntTypeDecl), Ord(typeDecl.NodeType));
		CheckEqualsString('TSpecial', typeDecl.Attributes[anName]);

		CheckEquals(1, typeDecl.ChildNodes.Count, 'TypeDecl should have one child');
		typeNode := typeDecl.ChildNodes[0].AsNode;
		CheckEquals(Ord(ntType), Ord(typeNode.NodeType));
		CheckEqualsString('avClass', typeNode.Attributes[anType], 'anType should be avClass as text');
	finally
		tree.Root.Free;
	end;
end;

procedure TDelphiParserTests.Test_ExpectedToTree_DeepHierarchy;
var
	expected: string;
	tree: TSyntaxTree;
	unitNode, implNode, usesNode, usedUnitNode: TSyntaxNode2;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'ntUnitDeclaration anName="U"' + CRLF +
		#9#9'ntQualifiedIdentifier anName="U"' + CRLF +
		#9#9'ntImplementation' + CRLF +
		#9#9#9'ntUses' + CRLF +
		#9#9#9#9'ntUsedUnit anName="ComObj"';

	tree := nil;
	try
		tree := ExpectedToTree(expected);
		CheckNotNull(tree);
		CheckNotNull(tree.Root);

		unitNode := tree.Root.ChildNodes[0].AsNode;
		CheckEquals(Ord(ntUnitDeclaration), Ord(unitNode.NodeType));

		// Find Implementation node (child index 1, given earlier test shape)
		implNode := unitNode.ChildNodes[1].AsNode;
		CheckEquals(Ord(ntImplementation), Ord(implNode.NodeType));

		// Implementation -> Uses -> UsedUnit
		CheckTrue(implNode.ChildNodes.Count >= 1, 'Implementation should have at least one child');
		usesNode := implNode.ChildNodes[0].AsNode;
		CheckEquals(Ord(ntUses), Ord(usesNode.NodeType));
		CheckEquals(1, usesNode.ChildNodes.Count, 'Uses should have one child');
		usedUnitNode := usesNode.ChildNodes[0].AsNode;
		CheckEquals(Ord(ntUsedUnit), Ord(usedUnitNode.NodeType));
		CheckEqualsString('ComObj', usedUnitNode.Attributes[anName]);
	finally
		tree.Root.Free;
	end;
end;

procedure TDelphiParserTests.Test_UnitPortabilityDirectives;
var
	sourceCode: string;
	expectedTree: string;
begin
	sourceCode := '''
unit A library platform deprecated 'use Fabrikam.SpecialCharactersDemo' experimental;
interface
implementation
end.
''';

	expectedTree := '''
ntCompilationUnit
	ntUnitDeclaration anName="A"
		ntQualifiedIdentifier anName="A"
		ntPortabilityDirective anLibrary="avTrue"
		ntPortabilityDirective anPlatform="avTrue"
		ntPortabilityDirective anDeprecated="use Fabrikam.SpecialCharactersDemo"
		ntPortabilityDirective anExperimental="avTrue"
		ntInterfaceSection
		ntImplementation

''';

	CompareSource(sourceCode, expectedTree, 'Test_UnitPortabilityDirectives');
end;

procedure TDelphiParserTests.Test_EmptyCase;
var
	sourceCode: string;
	expectedTree: string;
begin
{
The case ELSE block uses a special ntCaseElse rather than ntElse or ntStatementList.
}
	sourceCode := '''
unit U;
interface
implementation
procedure P;
begin
	case FErrorLevel of
	1: LogError(s);
	else
	end;
end;
end.
''';

	expectedTree := '''
ntCompilationUnit
	ntUnitDeclaration anName="U"
		ntQualifiedIdentifier anName="U"
		ntInterfaceSection
		ntImplementation
			ntMethod anKind="avProcedure"
				ntQualifiedIdentifier anName="P"
				ntStatementList
					ntStatement
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
							ntCaseElse
''';

	CompareSource(sourceCode, expectedTree, 'Test_CaseElseSemicolonHandling');
end;



procedure TDelphiParserTests.Test_CaseElseSemicolonHandling;
var
	sourceCode: string;
	expectedTree: string;
begin
{
From:
BNF notation for Delphi Grammar
https://comp.compilers.narkive.com/x5k5JSG1/bnf-notation-for-delphi-grammar#post5

Hans-Peter Diettrich		6/8/2005 19:59:36 UTC

> Post by George Neuner
>
> The Borland Pascal's used to contain the language BNF in the manual
appendixes. Did they discontinue this practice with Delphi?

The practice is continued, but the grammar is neither complete nor valid
nor up-to-date since the introduction of case-else.

I found railroad diagrams supplied with Delphi 2, that already differed
from what the compiler accepts. Later versions come with some kind of
extended BNF syntax, with the same problems and a missing description
for the grammar syntax or semantics.

Some people have tried to construct EBNF grammars for various Delphi
versions in the past, but the results are questionable, for several
reasons:

- "directives" are reserved words in specific context.
- the semantics are too far away from the syntax, in detail
- semicolons can have unexpected (context sensitive) effects.

My favorite example:

	case i of
	0: if a then b
	; //<----- illegal, optional or required?
	else c
	end;

Normally a semicolon is illegal in this place, because it's intended to
only separate multiple case-labels. In this special case the marked
semicolon is not optional, in fact it indicates whether the following
dangling "else" is part of the "if" or of the "case" statement.

Now try to construct an according context-free grammar :-(

DoDi
}
	sourceCode := '''
unit U;
interface
implementation
procedure A;
var
	i: Integer;
	b: Boolean;
	procedure c;
	begin
	end;
begin
	i := 0;
	b := False;
	case i of
	0: if b then c
	; //<----- illegal, optional or required?
	else c
	end;
end;
end.
''';

	expectedTree := '''
ntCompilationUnit
	ntUnitDeclaration anName="U"
		ntQualifiedIdentifier anName="U"
		ntInterfaceSection
		ntImplementation
			ntMethod anKind="avProcedure"
				ntQualifiedIdentifier anName="A"
				ntVarSection
					ntVariable
						ntIdentifierList
							ntIdentifier anName="i"
						ntType anName="Integer"
					ntVariable
						ntIdentifierList
							ntIdentifier anName="b"
						ntType anName="Boolean"
				ntMethod anKind="avProcedure"
					ntQualifiedIdentifier anName="c"
					ntStatementList
				ntStatementList
					ntStatement
						ntAssign
							ntParticle
								ntIdentifier anName="i"
							ntParticle
					ntStatement
						ntAssign
							ntParticle
								ntIdentifier anName="b"
							ntParticle
								ntIdentifier anName="False"
					ntStatement
						ntCaseStatement
							ntParticle
								ntIdentifier anName="i"
							ntCaseSelector
								ntCaseLabels
									ntCaseLabel
										ntExpression
											ntParticle
								ntStatement
									ntIf
										ntParticle
											ntIdentifier anName="b"
										ntThen
											ntStatement
												ntParticle
													ntIdentifier anName="c"
							ntCaseElse
								ntStatementList
									ntStatement
										ntParticle
											ntIdentifier anName="c"
''';

	CompareSource(sourceCode, expectedTree, 'Test_CaseElseSemicolonHandling');
end;

{ TDatFileTestCase }

constructor TDatFileTestCase.Create(const ACase: TDatParserCase; const TotalTests: Integer);
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
	RunDatCase(FCase, FCase.CaseIndex, FTotalTests);
end;

procedure RegisterDatFileTests;
const
	CANDIDATES: array[0..4] of string = (
			'TestData',
			'Library\DelphiParser\TestData',
			'..\Library\DelphiParser\TestData',
			'..\..\Library\DelphiParser\TestData',
			'..\..\..\Library\DelphiParser\TestData'
	);
var
	root, baseDir, fileName: string;
	files: TArray<string>;
	cases: TArray<TDatParserCase>;
	fileSuite: ITestSuite;
	i: Integer;
	helper: TDelphiParserTests;
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

	helper := TDelphiParserTests.Create('Test_DatFiles');
	try
		for fileName in files do
		begin
			cases := helper.EnumerateDatTestCases(fileName);
			if Length(cases) = 0 then
				Continue;
			fileSuite := TTestSuite.Create(ExtractFileName(fileName));
			for i := 0 to High(cases) do
				fileSuite.AddTest(TDatFileTestCase.Create(cases[i], Length(cases)));
			TestFramework.RegisterTest('DelphiParser/DatFiles', fileSuite);
		end;
	finally
		helper.Free;
	end;
end;

initialization
	TestFramework.RegisterTest('DelphiParser', TDelphiParserTests.Suite);
	RegisterDatFileTests;

end.
