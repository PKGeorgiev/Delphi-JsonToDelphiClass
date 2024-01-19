unit Pkg.Json.JsonValueHelper;

interface

uses System.Json;

type
  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtTrue, jtFalse, jtNumber, jtDateTime, jtBytes, jtInteger, jtInteger64);

  TJsonValueHelper = class
  public
    class function GetJsonType(aJsonValue: TJsonValue): TJsonType;
  end;

implementation

uses System.SysUtils, System.DateUtils;

{ TJsonValueHelper }

class function TJsonValueHelper.GetJsonType(aJsonValue: TJsonValue): TJsonType;
var
  Value: string;
  i: Integer;
  j: Int64;
  b: Boolean;
  d: Double;
  E: Extended;
begin
  if aJsonValue = nil then
    exit(jtObject);

  if aJsonValue is TJSONObject then
    exit(jtObject);

  if aJsonValue is TJSONArray then
    exit(jtArray);

  if (aJsonValue is TJSONNumber) then
  begin
    Value := aJsonValue.AsType<string>;
    if not TryJsonToFloat(Value, d) then
      Result := jtString
    else if TryStrToInt(Value, i) then
      Result := jtInteger
    else if TryStrToInt64(Value, j) then
      Result := jtInteger64
    else
      Result := jtNumber
  end
  else if aJsonValue is TJSONTrue then
    Result := jtTrue
  else if aJsonValue is TJSONFalse then
    Result := jtFalse
  else if aJsonValue is TJsonString then
  begin
    Value := aJsonValue.AsType<string>;

    try
      ISO8601ToDate(Value);
      exit(jtDateTime);
    except

    end;

    if TryStrToFloat(Value, E) then
      Result := jtString
    else if TryStrToBool(Value, b) then
    begin
      if b then
        Result := jtTrue
      else
        Result := jtFalse
    end
    else
      Result := jtString
  end
  else
    Result := jtUnknown;
end;

end.
