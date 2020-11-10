unit Pkg.Json.Settings;

interface

uses
  Pkg.Json.BoundObject;

Type
  TSettings = class(TBoundObject)
  private
    class var FInstance: TSettings;

  var
    FUsePascalCase: Boolean;
    FAddJsonPropertyAttributes: Boolean;
    constructor MakeSingleton;
    procedure SetUsePascalCase(const Value: Boolean);
    procedure SetAddJsonPropertyAttributes(const Value: Boolean);
  public
    constructor Create; reintroduce; deprecated 'Don''t use this!';
    class function Instance: TSettings;
    property UsePascalCase: Boolean read FUsePascalCase write SetUsePascalCase;
    property AddJsonPropertyAttributes: Boolean read FAddJsonPropertyAttributes write SetAddJsonPropertyAttributes;
  end;

implementation

uses
  System.SysUtils;

{ TSettings }

constructor TSettings.Create;
begin
  raise Exception.Create('Don''t call the constructor directly!');
end;

class function TSettings.Instance: TSettings;
begin
  if not Assigned(FInstance) then
    FInstance := TSettings.MakeSingleton;
  Result := FInstance;
end;

constructor TSettings.MakeSingleton;
begin
  inherited Create;
  FUsePascalCase := True;
  FAddJsonPropertyAttributes := False;
end;

procedure TSettings.SetAddJsonPropertyAttributes(const Value: Boolean);
begin
  if FAddJsonPropertyAttributes <> Value then
  begin
    Notify('AddJsonPropertyAttributes');
    FAddJsonPropertyAttributes := Value;
  end;
end;

procedure TSettings.SetUsePascalCase(const Value: Boolean);
begin
  if FUsePascalCase <> Value then
  begin
    Notify('UsePascalCase');
    FUsePascalCase := Value;
  end;
end;

initialization

finalization

TSettings.Instance.Free;

end.
