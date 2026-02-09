unit DelphiParserTests;

interface

uses
	Classes, Contnrs,
	DelphiParser, TestFramework;

type
	TDatParserCase = record
		Name: string;
		SourceCode: string;
		ExpectedTree: string;
		Errors: string;
		FileName: string;
		CaseIndex: Integer;
	end;

	TDelphiParserTests = class(TTestCase)
	protected
		function ExpectedToTree(ExpectedTree: string): TSyntaxNode2;
		function TryExpectedToTree(const ExpectedTree: string; out Node: TSyntaxNode2; out ErrorCode: HRESULT; out ErrorMessage: string): Boolean;

		function CompareSource(sourceCode, ExpectedTree: string; const CaseName: string = ''): Boolean;
		function CompareTrees(expected, actual: TSyntaxNode2): Boolean;
		function CompareNodes(expected, actual: TSyntaxNode2): Boolean;
		function FindDatTestsRoot: string;
		function EnumerateDatFiles: TArray<string>;
		function EnumerateDatTestCases(const FileName: string): TArray<TDatParserCase>;
		procedure RunDatCase(const ACase: TDatParserCase);
	published
		procedure Test_ExpectedToTree_SiblingsAtSameIndent;
		procedure Test_ExpectedToTree_AttributesParsing;
		procedure Test_ExpectedToTree_DeepHierarchy;
		procedure Test_ExpectedToTree_BadIndent_Raises;
		procedure Test_ExpectedToTree_UnterminatedQuote_Raises;
		procedure Test_ExpectedToTree_LineWithoutNt_Raises;
		procedure Test_ExpectedToTree_UnknownNodeType_Raises;
		procedure Test_ExpectedToTree_FirstLineNotRoot_Raises;
		procedure Test_ParseConstWithTrailingDecimalLiteral;

		procedure Test_ParseResStringSection;						// resourcestring SProduct = 'Contoso';
		procedure Test_ParseResStringSection_Concatenated;		// resourcestring SProduct = 'A' + 'B';
		procedure Test_ParseConstSection;

		procedure Test_DatFiles;

		procedure Test_ParseClassType;
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
		'Library\DelphiParser\TestData',
		'..\Library\DelphiParser\TestData',
		'..\..\Library\DelphiParser\TestData',
		'..\..\..\Library\DelphiParser\TestData',
		'TestData'
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
		if SameText(line, '#data') then
		begin
			FlushCurrent;
			section := 'data';
			continue;
		end;
		if SameText(line, '#errors') then
		begin
			section := 'errors';
			continue;
		end;
		if SameText(line, '#document') then
		begin
			section := 'document';
			continue;
		end;
		if SameText(line, '#name') then
		begin
			section := 'name';
			continue;
		end;

		payload := line;
		if section = 'document' then
		begin
			if (payload <> '') and (payload[1] = '|') then
			begin
				Delete(payload, 1, 1);
				if (payload <> '') and (payload[1] = ' ') then
					Delete(payload, 1, 1);
			end;
			AppendLine(cur.ExpectedTree, payload);
		end
		else if section = 'data' then
			AppendLine(cur.SourceCode, payload)
		else if section = 'errors' then
			AppendLine(cur.Errors, payload)
		else if section = 'name' then
			AppendLine(cur.Name, payload);
	end;

	FlushCurrent;
end;

procedure TDelphiParserTests.RunDatCase(const ACase: TDatParserCase);
begin
	CheckFalse(Trim(ACase.SourceCode) = '', 'Case "' + ACase.Name + '" is missing #data');
	CheckFalse(Trim(ACase.ExpectedTree) = '', 'Case "' + ACase.Name + '" is missing #document');

	Status('DAT Case: ' + ACase.Name + ' (' + ExtractFileName(ACase.FileName) + ')');
	Status('Source'+CRLF+'------'+CRLF+ACase.SourceCode);
	Status('ExpectedTree'+CRLF+'------------'+CRLF+ACase.ExpectedTree);

	CompareSource(ACase.SourceCode, ACase.ExpectedTree, ACase.Name);
end;

function TDelphiParserTests.CompareSource(sourceCode, ExpectedTree: string; const CaseName: string): Boolean;
var
	expected, actual: TSyntaxNode2;
	tokens: TObjectList;
begin
	CheckFalse(sourceCode='');
	CheckFalse(ExpectedTree='');
	if CaseName <> '' then
		Status('Case: ' + CaseName);

	// Build the expected tree
	expected := ExpectedToTree(expectedTree);
	CheckTrue(Assigned(expected));

	// Tokenize the source code
	tokens := TObjectList.Create(True); //owns objects
	TDelphiTokenizer.Tokenize(sourceCode, tokens);
	Status(TokensToStr(tokens));

	// Parse the source tokens
	actual := TDelphiParser.ParseText(sourceCode, '');
	CheckTrue(Assigned(actual));

	// and compare the trees
	Result := CompareTrees(expected, actual);
	if CaseName = '' then
		CheckTrue(Result, 'Trees must be equal')
	else
		CheckTrue(Result, 'Trees must be equal: ' + CaseName);
end;

function TDelphiParserTests.CompareTrees(expected, actual: TSyntaxNode2): Boolean;
begin
	CheckTrue(Assigned(expected), 'expected node not assigned');
	CheckTrue(Assigned(actual), 'actual node not assigned');

	Status(CRLF+CRLF+'Expected'+CRLF+'========'+CRLF+CRLF+TSyntaxNode2.DumpTree(expected));
	Status(CRLF+CRLF+'Actual  '+CRLF+'========'+CRLF+CRLF+TSyntaxNode2.DumpTree(actual));

	Result := CompareNodes(expected, actual);
end;

function TDelphiParserTests.CompareNodes(expected, actual: TSyntaxNode2): Boolean;
var
	child1, child2: TSyntaxNodeOrToken;
	i, j: Integer;
	attr: TAttributeName;
	expVal, actVal: string;
	expHasNodeChildren, actHasNodeChildren: Boolean;
begin
	CheckTrue(Assigned(expected), 'expected node not assigned');
	CheckTrue(Assigned(actual), 'actual node not assigned');

	// Compare attributes
	for attr := Low(TAttributeName) to High(TAttributeName) do
	begin
		expVal := expected.Attributes[attr];
		if expVal = '' then
			Continue;
		actVal := actual.Attributes[attr];
		CheckEqualsString(expVal, actVal, 'Attribute '+AttributeNameToStr(attr));
	end;

	// Check children
	expHasNodeChildren := False;
	for i := 0 to expected.ChildNodes.Count-1 do
		if expected.ChildNodes[i].IsNode then
		begin
			expHasNodeChildren := True;
			Break;
		end;
	actHasNodeChildren := False;
	for i := 0 to actual.ChildNodes.Count-1 do
		if actual.ChildNodes[i].IsNode then
		begin
			actHasNodeChildren := True;
			Break;
		end;
	CheckEquals(expHasNodeChildren, actHasNodeChildren, 'HasChildren');

	// We can't compare child counts, as the actual tree contains tokens.
	// We are only comparing nodes
//	CheckEquals(expected.ChildNodes.Count,	actual.ChildNodes.Count);


	j := 0; // j walks the actual child nodes in parallel with the for loop through the expected nodes
	for i := 0 to expected.ChildNodes.Count-1 do
	begin
		child1 := expected.ChildNodes[i];

		// the actual tree will contain token nodes. We need to skip over those.
		child2 := actual.ChildNodes[j];
		while child2.IsToken do
		begin
			Inc(j);
			CheckTrue(j <= actual.ChildNodes.Count-1, 'actual.ChildNodes index out of bounds');
			child2 := actual.ChildNodes[j];
		end;

		// j runs parallel to the for loop, so we have to be sure to Inc it manually before the loop continues
		Inc(j);

		CheckEquals(child1.IsNode,  child2.IsNode,  'IsNode');
		CheckEquals(child1.IsToken, child2.IsToken, 'IsToken'); // it's either Node or Token. Not both. And one is the inverse of the other. So this test should easiliy and always pass

		// For now the expected tree will not contain tokens
		// If they do arrive, we will ignore them. Perhaps adding a compare is a future thing?
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
			if Trim(work) <> '' then
				if not ParseAttributesSafe(child, work, idx, msg) then Exit;
			wrapper := TSyntaxNodeOrToken.Create(child);
			parent.ChildNodes.Add(wrapper);
			Inc(idx);
			if (idx < lines.Count) and (CountLeadingTabs(lines[idx]) > indent) then
				ParseIndentedSafe(child, indent, lines, idx, msg);
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

function TDelphiParserTests.ExpectedToTree(ExpectedTree: string): TSyntaxNode2;
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

	function ParseNodeType(const nodeStr: string): TSyntaxNodeType;
	var
		ordVal: Integer;
		resolvedName: string;
	begin
		// Be robust across Delphi versions:
		// GetEnumValue may raise or may return a value; verify round-trip.
		try
			ordVal := GetEnumValue(TypeInfo(TSyntaxNodeType), nodeStr);
		except
			on E: Exception do
				raise Exception.CreateFmt('Unknown node type: %s', [nodeStr]);
		end;

		if (ordVal < Ord(Low(TSyntaxNodeType))) or (ordVal > Ord(High(TSyntaxNodeType))) then
			raise Exception.CreateFmt('Unknown node type: %s', [nodeStr]);

		// Extra safety: ensure the name maps back exactly to the same identifier
		resolvedName := GetEnumName(TypeInfo(TSyntaxNodeType), ordVal);
		if not SameText(resolvedName, nodeStr) then
			raise Exception.CreateFmt('Unknown node type: %s', [nodeStr]);

		Result := TSyntaxNodeType(ordVal);
	end;

	  procedure SetNodeAttributeByName(node: TSyntaxNode2; const name, value: string);
	  var
	    ordVal: Integer;
	    v: string;
	  begin
	    ordVal := GetEnumValue(TypeInfo(TAttributeName), name);
	    if ordVal < 0 then
	      raise Exception.CreateFmt('Unknown attribute name: %s', [name]);
	    v := Dequote(value);
	    node.Attributes[TAttributeName(ordVal)] := v;
	  end;

	  procedure ParseAttributes(node: TSyntaxNode2; var rest: string; lineIndex: Integer);
	  var
	    s, name, value: string;
	    eqPos: Integer;
	  begin
	    s := TrimLeft(rest);
	    while s <> '' do
	    begin
	      // expect name=value (value may be quoted)
	      eqPos := Pos('=', s);
	      if eqPos = 0 then
	        raise Exception.CreateFmt('Expected attribute on line %d to contain =', [lineIndex+1]);
	      name := Trim(Copy(s, 1, eqPos-1));
	      s := TrimLeft(Copy(s, eqPos+1, Max(0, Length(s)-eqPos)));
	      // parse value token (quoted or next whitespace)
	      if (s <> '') and (s[1] = '"') then
	      begin
	        // find closing quote
	        eqPos := 2;
	        while (eqPos <= Length(s)) and (s[eqPos] <> '"') do Inc(eqPos);
	        if (eqPos > Length(s)) or (s[eqPos] <> '"') then
	          raise Exception.CreateFmt('Unterminated quoted attribute value on line %d', [lineIndex+1]);
	        value := Copy(s, 1, eqPos);
	        s := TrimLeft(Copy(s, eqPos+1, Max(0, Length(s)-eqPos)));
	      end else begin
	        // read until whitespace
	        eqPos := 1;
	        while (eqPos <= Length(s)) and (not CharInSet(s[eqPos], [' ', #9])) do Inc(eqPos);
	        value := Copy(s, 1, eqPos-1);
	        s := TrimLeft(Copy(s, eqPos, Max(0, Length(s)-eqPos+1)));
	      end;
	      SetNodeAttributeByName(node, name, value);
	    end;
	    rest := s;
	  end;

	  procedure ParseIndented(parent: TSyntaxNode2; parentIndent: Integer; lines: TStrings; var idx: Integer);
	  var
	    line: string;
	    indent: Integer;
	    work: string;
	    nodeName: string;
	    child: TSyntaxNode2;
	    lastChild: TSyntaxNode2;
	    wrapper: TSyntaxNodeOrToken;
	  begin
	    while idx < lines.Count do
	    begin
	      line := lines[idx];
	      if Trim(line) = '' then begin Inc(idx); Continue; end;
	      indent := CountLeadingTabs(line);

	      // Stop when we reach a line not under this parent
	      if indent <= parentIndent then Exit;

	      // If we are deeper than an immediate child, recurse into the last child
	      if indent > parentIndent + 1 then
	      begin
	        if (parent.ChildNodes.Count = 0) or (not parent.ChildNodes[parent.ChildNodes.Count-1].IsNode) then
	          raise Exception.CreateFmt('Line %d is too deeply indented without a previous parent node', [idx+1]);
	        lastChild := parent.ChildNodes[parent.ChildNodes.Count-1].AsNode;
	        ParseIndented(lastChild, parentIndent+1, lines, idx);
	        Continue;
	      end;

	      // indent == parentIndent + 1: create a new child node
	      work := StripLeadingTabs(line, indent);
	      work := TrimLeft(work);
	      if Copy(work, 1, 2) <> 'nt' then
	        raise Exception.CreateFmt('Expected line %d to start with "nt"', [idx+1]);

	      nodeName := NextToken(work);
	      child := TSyntaxNode2.Create(ParseNodeType(nodeName));
	      // parse attributes from remainder of the line
	      if Trim(work) <> '' then
	        ParseAttributes(child, work, idx);

	      // Use public wrapper to add child instead of calling private AddChild
	      wrapper := TSyntaxNodeOrToken.Create(child);
	      parent.ChildNodes.Add(wrapper);

	      Inc(idx);
	      // Recurse into this child if the next line is more indented
	      if (idx < lines.Count) and (CountLeadingTabs(lines[idx]) > indent) then
	        ParseIndented(child, indent, lines, idx);
	    end;
	  end;

	  procedure ValidateStructure(lines: TStrings);
	  var
	    i, prevIndent, indent: Integer;
	    line, work, nodeName: string;
	    dummy: TSyntaxNode2;
	  begin
	    if (lines.Count = 0) or (Trim(lines[0]) <> 'ntCompilationUnit') then
	      raise Exception.Create('Expected first line to be ntCompilationUnit');

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
	        raise Exception.CreateFmt('Expected line %d to start with "nt"', [i+1]);

	      // Ensure no multi-level jump without intermediate parent line
	      if indent > prevIndent + 1 then
	        raise Exception.CreateFmt('Line %d is too deeply indented without a previous parent node', [i+1]);

	      // Validate node type and attributes parsing (including quoted values)
	      nodeName := NextToken(work);
	      // Will raise if the node type name is invalid
	      ParseNodeType(nodeName);
	      // Parse attributes into a dummy node to validate quoting and names
	      if Trim(work) <> '' then
	      begin
	        dummy := TSyntaxNode2.Create(ntUnknown);
	        try
	          ParseAttributes(dummy, work, i);
	        finally
	          dummy.Free;
	        end;
	      end;

	      prevIndent := indent;
	    end;
	  end;

	begin
	  // Manually create the root node and then parse descendants recursively
	  Result := TSyntaxNode2.Create(ntCompilationUnit);

	  sl := TStringList.Create;
	  try
	    sl.Text := ExpectedTree;
	    // Pre-validate structure so tests reliably get exceptions for malformed inputs
	    ValidateStructure(sl);

	    i := 1; // start after the root line
	    ParseIndented(Result, 0 {root indent}, sl, i);
	  finally
	    sl.Free;
	  end;
end;

procedure TDelphiParserTests.Test_DatFiles;
var
	files: TArray<string>;
	fileName: string;
	cases: TArray<TDatParserCase>;
	testCase: TDatParserCase;
begin
	files := EnumerateDatFiles;
	for fileName in files do
	begin
		cases := EnumerateDatTestCases(fileName);
		CheckTrue(Length(cases) > 0, 'No test cases found in ' + fileName);
		for testCase in cases do
			RunDatCase(testCase);
	end;
end;

procedure TDelphiParserTests.Test_ParseConstSection;
var
	sourceCode: string;
	expectedTree: string;
const
   A = 1;
	B= 2;
	C =3;
   d = a+b;
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
					ntExpression anValueText="1"
		ntImplementation
''';

	CompareSource(sourceCode, expectedTree);
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

	CompareSource(sourceCode, expectedTree);
end;

procedure TDelphiParserTests.Test_ParseResStringSection_Concatenated;
var
	sourceCode: string;
	expectedTree: string;
resourcestring
	S1 = 'a'+'b';
//2 = S1 + 'b';   E2026 Constant expression expected	S3 = S1 + S2;
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

	CompareSource(sourceCode, expectedTree);
end;

procedure TDelphiParserTests.Test_ParseClassType;
var
	sourceCode: string;
	expectedTree: string;
begin
{
unit TestParseClassType;
interface
type
  TSpecial = class
    FTime: Integer
  end;
implementation
uses
   ComObj;
end.

ntCompilationUnit
	ntUnitDeclaration anName="TestParseClassType"
		ntQualifiedIdentifier anName="TestParseClassType"
		ntInterfaceSection
			ntTypeSection
				ntTypeDecl anName="TSpecial"
					ntType anType="atClass"
						ntField
							ntName anName="FTime"
							ntType anName="Integer"
		ntImplementation
			ntUses
				ntUnit anName="ComObj"
}

	sourceCode := '''
unit TestParseClassType;
interface
type
	TSpecial = class
	private
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
					ntType anType="atClass"
						ntField
							ntName anName="FTime"
							ntType anName="Integer"
		ntImplementation
			ntUses
				ntQualifiedIdentifier anName="ComObj"
''';

	CompareSource(sourceCode, expectedTree);
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
	tree: TSyntaxNode2;
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
			if (token.TokenKind = ptFloat) and SameText(token.Text, '1.') then
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
		#9#9'ntType anType="atClass"';

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
	root, unitNode, child: TSyntaxNode2;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'ntUnitDeclaration anName="U"' + CRLF +
		#9#9'ntQualifiedIdentifier anName="U"' + CRLF +
		#9#9'ntInterfaceSection' + CRLF +
		#9#9'ntImplementation';

	root := ExpectedToTree(expected);
	CheckNotNull(root, 'Root should not be nil');

	// Root should have exactly one child: UnitDeclaration
	CheckEquals(1, root.ChildNodes.Count, 'Root should have one child');
	CheckTrue(root.ChildNodes[0].IsNode, 'Root child should be a node');
	unitNode := root.ChildNodes[0].AsNode;
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
end;

procedure TDelphiParserTests.Test_ExpectedToTree_AttributesParsing;
var
	expected: string;
	root, typeDecl, typeNode: TSyntaxNode2;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'ntTypeDecl anName="TSpecial"' + CRLF +
		#9#9'ntType anType="atClass"';

	root := ExpectedToTree(expected);
	CheckNotNull(root);
	CheckEquals(1, root.ChildNodes.Count, 'Root should have one child');
	typeDecl := root.ChildNodes[0].AsNode;
	CheckEquals(Ord(ntTypeDecl), Ord(typeDecl.NodeType));
	CheckEqualsString('TSpecial', typeDecl.Attributes[anName]);

	CheckEquals(1, typeDecl.ChildNodes.Count, 'TypeDecl should have one child');
	typeNode := typeDecl.ChildNodes[0].AsNode;
	CheckEquals(Ord(ntType), Ord(typeNode.NodeType));
	CheckEqualsString('atClass', typeNode.Attributes[anType], 'anType should be atClass as text');
end;

procedure TDelphiParserTests.Test_ExpectedToTree_DeepHierarchy;
var
	expected: string;
	root, unitNode, implNode, usesNode, usedUnitNode: TSyntaxNode2;
begin
	expected :=
		'ntCompilationUnit' + CRLF +
		#9'ntUnitDeclaration anName="U"' + CRLF +
		#9#9'ntQualifiedIdentifier anName="U"' + CRLF +
		#9#9'ntImplementation' + CRLF +
		#9#9#9'ntUses' + CRLF +
		#9#9#9#9'ntUsedUnit anName="ComObj"';

	root := ExpectedToTree(expected);
	CheckNotNull(root);
	unitNode := root.ChildNodes[0].AsNode;
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
end;

initialization
	TestFramework.RegisterTest('DelphiParser\DelphiParser', TDelphiParserTests.Suite);

end.
