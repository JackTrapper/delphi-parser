unit -->modulenameParser;

(* NOTE: Any changes made to this file may be overwritten when it gets regenerated
  from within ParserBuilder - unless you run the Delphi Import command*)


interface

uses
  Windows, Messages, SysUtils, Classes,-->UsedUnits

const
  -->constants
  minErrDist = 2;  (* minimal distance (good tokens) between two errors *)
  setsize = 16;  (* sets are stored in 16 bits *)

type
  BITSET = set of 0..15;
  TSymbolSet = array [0..maxT div  setsize] of BITSET;

{PBGLOBAL}

{ENDPBGLOBAL}


-->GlobalDecs

type
  T-->modulenameParser = class(T-->modulenameScanner)
  private
    FSuccessful: boolean;
    {PBPRIVATE}
    -->PrivateDecs
    {ENDPBPRIVATE}
    procedure PerformGet;
    procedure SynError(errNo: integer);
    procedure SemError(errNo: integer);
    function _In(var s: TSymbolSet; x: integer): boolean;
    procedure Expect(n: integer);
    procedure ExpectWeak(n, follow: integer);
    function WeakSeparator(n, syFol, repFol: integer): boolean;
    function GetSuccessful: boolean;
    Procedure SetSourceString(const Value : String);
    -->Classdeclarations
  protected
    {PBPROTECTED}
     -->ProtectedDecs
    {ENDPBPROTECTED}
  public
    {PBPUBLIC}
    -->PublicDecs
    {ENDPBPUBLIC}
    constructor Create;
    destructor Destroy; override;
    procedure Parse;
    procedure LexString(var Lex: string);
    procedure LexName(var Lex: string);
    procedure LookAheadString(var Lex: string);
    procedure LookAheadName(var Lex: string);
  published
    property Successful: boolean read GetSuccessful;
    Property SourceString : String write SetSourceString;
    {PBPUBLISHED}
    -->PublishedDecs
    {ENDPBPUBLISHED}
  end;

implementation

var
  symSet: array [0..-->symSetSize] of TSymbolSet;
  errDist: integer;
  sym: integer;

{  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
   + For ParserBuilder's Delphi import function to work,        -
   + you must place your code in this Arbitrary Source section. -
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- }

{PBARBITRARYSOURCE}


-->ArbitrarySource

{ENDPBARBITRARYSOURCE}

 { T-->modulenameParser - Generated Methods below}
procedure T-->modulenameParser.SemError(errNo: integer);
begin
  if errDist >= minErrDist then
  begin
     -->error
  end;
  errDist := 0;
end;

procedure T-->modulenameParser.SynError(errNo: integer);
begin
  if errDist >= minErrDist then
  begin
     -->error
  end;
  errDist := 0;
end;

procedure T-->modulenameParser.PerformGet;
var
  s: string;
begin
  repeat
    Get(sym);
    if sym <= maxT then
      INC(errDist)
    else
    begin
     -->pragmas
    end;
  until sym <= maxT
  end;

function T-->modulenameParser._In(var s: TSymbolSet; x: integer): boolean;
begin
  _In := x mod setsize in s[x div setsize];
end;

function T-->modulenameParser.GetSuccessful: boolean;
begin
  FSuccessful := errors = 0;
  Result := FSuccessful;
end;

procedure T-->modulenameParser.Expect(n: integer);
begin
  if Sym = n then PerformGet
  else
    SynError(n);
end;

procedure T-->modulenameParser.ExpectWeak(n, follow: integer);
begin
  if Sym = n then PerformGet
  else
  begin
    SynError(n);
    while not _In(symSet[follow], Sym) do PerformGet;
  end
end;

function T-->modulenameParser.WeakSeparator(n, syFol, repFol: integer): boolean;
var
  s: TSymbolSet;
  i: integer;
begin
  if sym = n then
  begin
    PerformGet;
    WeakSeparator := True;
    EXIT;
  end
  else if _In(symSet[repFol], sym) then
  begin
    WeakSeparator := False;
    exit
  end
  else
  begin
    i := 0;
    while i <= maxT div setsize do
    begin
      s[i] := symSet[0, i] + symSet[syFol, i] + symSet[repFol, i];
      INC(i)
    end;
    SynError(n);
    while not _In(s, sym) do PerformGet;
    WeakSeparator := _In(symSet[syFol], sym)
  end
end;

procedure T-->modulenameParser.LexName(var Lex: string);
begin
    GetName(pos, len, Lex)
end;

procedure T-->modulenameParser.LexString(var Lex: string);
begin
   GetString(pos, len, Lex)
end;

procedure T-->modulenameParser.LookAheadName(var Lex: string);
begin
  GetName(nextPos, nextLen, Lex);
end;

procedure T-->modulenameParser.LookAheadString(var Lex: string);
begin
  GetString(nextPos, nextLen, Lex)
end;

Procedure T-->modulenameParser.SetSourceString(const Value : String);
Var sWin32Code : String;
Begin
  if (src<> NIL) then
    src.Free;
  ExecuteWin32OnlyParser(Value,sWin32Code);
  src := TStringStream.Create(sWin32Code);
  //src := TStringStream.Create(Value);
End;
-->productions

procedure T-->modulenameParser.Parse;
begin
  if (src.DataString = '') then 
    lst.WriteString('Empty Source')
    Else Begin    
   -->parseRoot
    End;
end;

destructor T-->modulenameParser.Destroy;
Begin
{PBDESTROY}
  -->Destroy
{ENDPBDESTROY}
  inherited;
End;

constructor T-->modulenameParser.Create;
begin
  inherited;
{PBCREATE}
  -->Create
{ENDPBCREATE}
  errDist := minErrDist;
   -->initialization
end;


end.   (* -->modulenameParser *)
