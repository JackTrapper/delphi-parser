unit PascalWriter;

interface

uses
	Classes, SysUtils,
	DelphiParser;

type
	TPascalWriter = class
	private
		FShowParseTags: Boolean;	// insert parse tags into the output

		procedure WriteNode(Node: TSyntaxNode2; sb: TStringBuilder);
		procedure WriteToken(Token: TSyntaxToken; sb: TStringBuilder);
	public
		constructor Create;
		function Execute(const Root: TSyntaxNode2): string;

		class function ToSource(const Root: TSyntaxNode2; ShowParsingTags: Boolean): string; static;
	end;


	TASTWriter = class
	public
		class function DumpTree(Node: TSyntaxNode2): string;
	end;

	EPascalWriterException = class(Exception);

function NodeTypeToStr(NodeType: TSyntaxNodeType): string;

implementation

uses
	Windows,
	TypInfo,
	Constraints;

{
	Tag Soup: Crazy parsing adventures
	http://ln.hixie.ch/?start=1137740632&count=1
	https://archive.ph/eFzHf
}

const
	SIndent = '   '; //3 *twitch* spaces
	CRLF = #13#10;

type
	TSyntaxNodeHelper = class helper for TSyntaxNode2
	public
		function First: TSyntaxNode2;  // returns the first child; throws if there is none
		function Single: TSyntaxNode2; // returns the first child; throws error if there is not exactly one
		function DisplayName: string;
		function ChildCount: Integer;
	end;

function NodeTypeToStr(NodeType: TSyntaxNodeType): string;
begin
	Result := TypInfo.GetEnumName(TypeInfo(TSyntaxNodeType), Ord(NodeType));
end;

procedure TPascalWriter.WriteNode(Node: TSyntaxNode2; sb: TStringBuilder);
var
	child: TSyntaxNodeOrToken;
begin
{
	Call the handler for the node type being presented.

	If it's a node type we don't support, outputs a raw descriptive comment.
}
{	if FShowParseTags then
	begin
		sb.Append(GetDefaultText(Node));
		if Node.ChildNodes.Count > 1 then
			sb.AppendLine;
	end;}

	for child in Node.ChildNodes do
	begin
		if child.IsNode then
			WriteNode(child.AsNode, sb)
		else if child.IsToken then
			WriteToken(child.AsToken, sb);
	end;
end;

procedure TPascalWriter.WriteToken(Token: TSyntaxToken; sb: TStringBuilder);
var
	i: Integer;
begin
	TConstraints.NotNull(Token);
	TConstraints.NotNull(sb);

	for i := 0 to Token.LeadingTriviaCount-1 do
		sb.Append(Token.LeadingTrivia[i].ValueText);

	sb.Append(Token.ValueText);

	for i := 0 to Token.TrailingTriviaCount-1 do
		sb.Append(Token.TrailingTrivia[i].ValueText);
end;

function TPascalWriter.Execute(const Root: TSyntaxNode2): string;
var
	sb: TStringBuilder;
begin
	TConstraints.NotNull(Root);

	sb := TStringBuilder.Create;
	try
		WriteNode(Root, sb);
		Result := sb.ToString;
	finally
		sb.Free;
	end;
end;

constructor TPascalWriter.Create;
begin
	inherited;

	FShowParseTags := True;
end;

class function TPascalWriter.ToSource(const Root: TSyntaxNode2; ShowParsingTags: Boolean): string;
var
	writer: TPascalWriter;
begin
	writer := TPascalWriter.Create;
	try
		writer.FShowParseTags := ShowParsingTags;
		Result := writer.Execute(Root);
	finally
		FreeAndNil(writer);
	end;
end;

{ TASTWriter }

class function TASTWriter.DumpTree(Node: TSyntaxNode2): string;

	function DumpNode(const Node: TSyntaxNode2; Prefix: UnicodeString): UnicodeString;
	var
		sChild: UnicodeString;
		i: Integer;
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

		for i := 0 to Node.ChildCount-1 do
		begin
			if not Node.ChildNodes[i].IsNode then
				Continue;

			Result := Result+CRLF+
					prefix;


			if i < Node.ChildCount-1 then
			begin
				Result := Result + middlePrefix;
				sChild := DumpNode(Node.ChildNodes[i].AsNode, prefix+nestedPrefix);
			end
			else
			begin
				Result := Result + finalPrefix;
				sChild := DumpNode(Node.ChildNodes[i].AsNode, prefix+'   ');
			end;

			Result := Result+sChild;
		end;
	end;

begin
	Result := DumpNode(Node, '');
end;

{ TSyntaxNodeHelper }

function TSyntaxNodeHelper.ChildCount: Integer;
begin
	Result := Self.ChildNodes.Count;
end;

function TSyntaxNodeHelper.DisplayName: string;
var
	s: string;

	procedure Add(const Content: string);
	begin
      if s <> '' then
			s := s+', ';
		s := s+Content;
	end;

begin
	Result := '['+GetEnumName(TypeInfo(TSyntaxNodeType), Ord(Self.NodeType))+'(';

	s := '';

	if Self.HasChildren then
		Add('Children='+IntToStr(Self.ChildCount));

	Result := Result+s+')]';
end;

function TSyntaxNodeHelper.First: TSyntaxNode2;
var
	i: Integer;
begin
	if not Self.HasChildren then  // Use Self to access the original TSyntaxNode2 instance
		raise Exception.Create('No children available');

	for i := 0 to Self.ChildCount-1 do
	begin
		if Self.ChildNodes[i].IsNode then
		begin
			Result := Self.ChildNodes[i].AsNode;
			Exit;
   	end;
	end;

	raise Exception.Create('No children nodes available');
end;

function TSyntaxNodeHelper.Single: TSyntaxNode2;
var
	i: Integer;
	nFound: Integer;
begin
	if Self.ChildNodes.Count = 0 then
		raise Exception.CreateFmt('Expected %s to have exactly one child syntax node', [Self.DisplayName]);

	nFound := 0;
	Result := nil;

	for i := 0 to Self.ChildNodes.Count-1 do
	begin
		if Self.ChildNodes[i].IsNode then
		begin
			Inc(nFound);
			if (nFound > 1) then
				raise Exception.CreateFmt('Expected %s to have exactly one child syntax node', [Self.DisplayName]);

			if Result = nil then
				Result := Self.ChildNodes[i].AsNode;
		end;
	end;
end;

end.
