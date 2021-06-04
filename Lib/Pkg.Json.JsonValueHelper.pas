unit Pkg.Json.JsonValueHelper;

interface

uses
  System.Json;

type
  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtTrue, jtFalse, jtNumber, jtDateTime, jtBytes, jtInteger, jtInteger64);

  TJsonValueHelper = class
  public
    class function GetJsonType(aJsonValue: TJsonValue): TJsonType;
  end;

implementation

uses
  System.SysUtils, System.RegularExpressions;
{ TJsonValueHelper }

class function TJsonValueHelper.GetJsonType(aJsonValue: TJsonValue): TJsonType;
var
  JsonString: TJsonString;
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
    Result := jtObject
  else if aJsonValue is TJSONArray then
    Result := jtArray
  else if (aJsonValue is TJSONNumber) then
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
    JsonString := (aJsonValue as TJsonString);
    if TRegEx.IsMatch(JsonString.Value,
      '^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])T(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])(\.[0-9]+)?(Z|[+-](?:2[0-3]|[01][0-9]):[0-5][0-9])?$|(([12]\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01])))') then
      Result := jtDateTime
    else if TryStrToFloat(JsonString.Value, E) then
      Result := jtString
    else if TryStrToBool(JsonString.Value, b) then
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
