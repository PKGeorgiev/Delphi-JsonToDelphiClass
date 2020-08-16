unit Pkg.Json.JSONName;

interface

{$M+}

type
  TJSONName = class
  strict private
    FJsonName: string;
    FDelphiName: string;
    FNeedsAttribute: Boolean;
    FName: string;
    FPureClassName: string;
  protected
    procedure SetName(const Value: string); virtual;
  published
    property JSONName: string read FJsonName;
    property DelphiName: string read FDelphiName;
    property NeedsAttribute: Boolean read FNeedsAttribute;
    property PureClassName: string read FPureClassName write FPureClassName;
    property Name: string read FName write SetName;
  public
    constructor Create(aItemName: string); reintroduce;
    function NameAttribute: string;
    class function CapitalizeFirst(Value: string): string;
  end;

implementation

uses
  System.Classes, System.Sysutils, System.Character;

{ TJSONName }

class function TJSONName.CapitalizeFirst(Value: string): string;
var
  List: TStringList;
  s: string;
  i: Integer;
begin
  Value := Value.ToLower;

  if Value.Substring(1, 4) = 'name' then
    Value := Value[1] + 'Name' + Value.Substring(4);
  if Value.EndsWith('Test', True) then
  begin
    i := Value.Length - 4;
    Value := Value.Substring(0, i) + 'Test';
  end;

  List := TStringList.Create;
  try
    ExtractStrings(['_'], [], PChar(Value), List);
    for i := 0 to List.Count - 1 do
    begin
      s := List[i];
      if s.StartsWith('&') then
        s[2] := s.ToUpper[2]
      else
        s[1] := s.ToUpper[1];
      List[i] := s;
    end;

    List.Delimiter := '_';
    Result := List.DelimitedText;
  finally
    List.Free;
  end;
end;

constructor TJSONName.Create(aItemName: string);
var
  s: string;
  ch: Char;
begin
  inherited Create;

  if aItemName.IsEmpty then
    raise Exception.Create('aItemName can not be empty');

  FNeedsAttribute := False;
  FJsonName := aItemName;

  for ch in FJsonName do
    if ch.IsLetterOrDigit then
      s := s + ch
    else
      s := s + '_';

  if s.StartsWith('_') then
    s := s.Substring(1);

  FDelphiName := CapitalizeFirst(s);
  if FDelphiName = '' then
    FDelphiName := 'Property';

  if not FDelphiName[1].IsLetter then
    FDelphiName := '_' + FDelphiName;

  FNeedsAttribute := not SameText(FDelphiName, FJsonName);
end;

function TJSONName.NameAttribute: string;
begin
  exit('JSONName(' + AnsiQuotedStr(FJsonName, #39) + ')');
end;

procedure TJSONName.SetName(const Value: string);
begin
  FPureClassName := Value;
  FName := 'T' + FPureClassName + 'DTO';
end;

end.
