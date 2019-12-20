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
  Json: string;
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
begin
  JSONValue := TJSONObject.ParseJSONValue(Value);
  try
    if Assigned(JSONValue) and (JSONValue is TJSONObject) then
      JSONObject := JSONValue as TJSONObject
    else
    begin
      Json := Value.Trim;
      if (Json = '') and not Assigned(JSONValue) or (Json <> '') and Assigned(JSONValue) and JSONValue.Null then
        Exit
      else
        raise EConversionError.Create(SCannotCreateObject);
    end;

    TJson.JsonToObject(Self, JSONObject, FOptions);

  finally
    JSONValue.Free;
  end;
end;

end.
