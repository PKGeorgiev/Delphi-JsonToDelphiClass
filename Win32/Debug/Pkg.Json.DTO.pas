unit Pkg.Json.DTO;

interface

uses
  Rest.Json;

type
  TJsonDTO = class
  private
    FOptions: TJsonOptions;
    function GetAsJson: string;
    procedure SetAsJson(const Value: string);
  public
    constructor Create; overload;
    constructor Create(const aJson: string; AOptions: TJsonOptions = [joDateIsUTC, joDateFormatISO8601]); overload;
    property AsJson: string read GetAsJson write SetAsJson;
  end;

implementation

uses
  System.Json, System.Sysutils, Rest.JsonReflect, System.JSONConsts;

{ TJsonDTO }

constructor TJsonDTO.Create(const aJson: string; AOptions: TJsonOptions);
begin
  inherited Create;
  FOptions := AOptions;
  AsJson := aJson;
end;

constructor TJsonDTO.Create;
begin
  inherited;
  FOptions := [joDateIsUTC, joDateFormatISO8601];
end;

function TJsonDTO.GetAsJson: string;
begin
  Result := TJson.ObjectToJsonString(Self, FOptions);
end;

procedure TJsonDTO.SetAsJson(const Value: string);
var
  LJson: string;
  LJSONValue: TJSONValue;
  LJSONObject: TJSONObject;
begin
  LJSONValue := TJSONObject.ParseJSONValue(Value);
  try
    if Assigned(LJSONValue) and (LJSONValue is TJSONObject) then
      LJSONObject := LJSONValue as TJSONObject
    else
    begin
      LJson := Value.Trim;
      if (LJson = '') and not Assigned(LJSONValue) or (LJson <> '') and Assigned(LJSONValue) and LJSONValue.Null then
        Exit
      else
        raise EConversionError.Create(SCannotCreateObject);
    end;

    TJson.JsonToObject(Self, LJSONObject, FOptions);

  finally
    LJSONValue.Free;
  end;
end;

end.
