unit Pkg.Json.Settings;

interface

uses
  Pkg.Json.DTO;

Type
  TSettings = class(TJsonDTO)
  private
    class var FInstance: TSettings;

  var
    FAddJsonPropertyAttributes: Boolean;
    FPostFix: string;
    FPostFixClassNames: Boolean;
    FUsePascalCase: Boolean;
    FSuppressZeroDate: Boolean;
    constructor MakeSingleton;
  public
    constructor Create; reintroduce; deprecated 'Don''t use this!';
    class function Instance: TSettings;
    class function GetPostFix: string;
    property AddJsonPropertyAttributes: Boolean read FAddJsonPropertyAttributes write FAddJsonPropertyAttributes;
    property PostFixClassNames: Boolean read FPostFixClassNames write FPostFixClassNames;
    property PostFix: string read FPostFix write FPostFix;
    property UsePascalCase: Boolean read FUsePascalCase write FUsePascalCase;
    property SuppressZeroDate: Boolean read FSuppressZeroDate write FSuppressZeroDate;
  end;

implementation

uses
  System.SysUtils, System.Strutils;

{ TSettings }

constructor TSettings.Create;
begin
  raise Exception.Create('Don''t call the constructor directly!');
end;

class function TSettings.GetPostFix: string;
begin
  Result := IfThen(Instance.PostFixClassNames, Instance.PostFix, string.empty);
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
  FPostFixClassNames := False;
  FPostFix := 'DTO';
  FSuppressZeroDate := True;
end;

initialization

finalization

TSettings.Instance.Free;

end.
