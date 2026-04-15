unit Win32OnlyComponent;

interface

uses
  Windows, Messages, SysUtils, Classes,Win32OnlyParser;

type
  TWin32OnlyParserComponent = class(TComponent)
  private
    { Private declarations }
    FWin32OnlyParser : TWin32OnlyParser;
    FInput: String;
    FOutput: String;
    procedure SetInput(const Value: String);
    procedure SetOutput(const Value: String);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Aowner : TComponent); override;
    destructor Destroy; override;
    Function Execute : Boolean;
  published
    { Published declarations }
    Property Input : String read FInput write SetInput;
    Property Output : String read FOutput write SetOutput;
  end;


Function ExecuteWin32OnlyParser(Const sInput : String; Var sOutput : String) : Boolean;


implementation

uses Win32OnlyScanner;


Function ExecuteWin32OnlyParser(Const sInput : String; Var sOutput : String) : Boolean;
Begin
   Result := False;
   With  TWin32OnlyParserComponent.Create(NIL) do
   try
     Input := sInput;
     Result := Execute;
     sOutput := Output;
   finally
     Free;
    end;
End;


{ TWin32OnlyParserComponent }

constructor TWin32OnlyParserComponent.Create(Aowner: TComponent);
begin
  inherited;
 FWin32OnlyParser := TWin32OnlyParser.Create;
end;

destructor TWin32OnlyParserComponent.Destroy;
begin
    FWin32OnlyParser.Free;
  inherited;
end;

function TWin32OnlyParserComponent.Execute: Boolean;
begin
  FWin32OnlyParser.SourceString := FInput;
  FWin32OnlyParser.Parse;
  FOutput := FWin32OnlyParser.lst.DataString;
  Result := FWin32OnlyParser.errors = 0;
end;

procedure TWin32OnlyParserComponent.SetOutput(const Value: String);
begin
  FOutput := Value;
end;

procedure TWin32OnlyParserComponent.SetInput(const Value: String);
begin
  FInput := Value;
end;

end.

