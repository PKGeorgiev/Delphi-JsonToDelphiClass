unit Pkg.Json.Utils;

interface

uses
  System.Classes,
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib,
{$ENDIF POSIX}
  System.Json.Writers, System.Json.Readers, System.SysUtils;

type
  TJsonStringWriter = class(TJsonTextWriter)
  private
    FStrinBuilder: TStringBuilder;
    FStringWriter: TStringWriter;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
  end;

  TJsonStringReader = class(TJsonTextReader)
  private
    FStrinReader: TStringReader;
  public
    constructor Create(const AJson: string);
    destructor Destroy; override;
  end;

procedure ShellExecute(aFileName: string);

function MinifyJson(AJson: string): string;
function PrettyPrint(AJson: string): string;

implementation

uses
  System.Json.Types;

procedure ShellExecute(aFileName: string);
begin
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI.ShellExecute(0, 'OPEN', PChar(aFileName), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(aFileName)));
{$ENDIF POSIX}
end;

function JsonReformat(const AJson: string; Indented: Boolean): string;
var
  JsonWriter: TJsonStringWriter;
  JsonReader: TJsonStringReader;
begin
  JsonReader := TJsonStringReader.Create(AJson);
  JsonWriter := TJsonStringWriter.Create;

  if Indented then
    JsonWriter.Formatting := TJsonFormatting.Indented;

  try
    JsonWriter.WriteToken(JsonReader);
    Result := JsonWriter.ToString;
  finally
    JsonWriter.Free;
    JsonReader.Free;
  end;
end;

function MinifyJson(AJson: string): string;
begin
  Result := JsonReformat(AJson, false);
end;

function PrettyPrint(AJson: string): string;
begin
  Result := JsonReformat(AJson, True);
end;

{ TJsonStringWriter }

constructor TJsonStringWriter.Create;
begin
  FStrinBuilder := TStringBuilder.Create;
  FStringWriter := TStringWriter.Create(FStrinBuilder);
  inherited Create(FStringWriter);
end;

destructor TJsonStringWriter.Destroy;
begin
  FStringWriter.Free;
  FStrinBuilder.Free;
  inherited Destroy;
end;

function TJsonStringWriter.ToString: string;
begin
  Result := FStrinBuilder.ToString;
end;

{ TJsonStringReader }

constructor TJsonStringReader.Create(const AJson: string);
begin
  FStrinReader := TStringReader.Create(AJson);
  inherited Create(FStrinReader);
end;

destructor TJsonStringReader.Destroy;
begin
  FStrinReader.Free;
  inherited Destroy;
end;

end.
