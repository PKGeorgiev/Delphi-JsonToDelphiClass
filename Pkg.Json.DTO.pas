unit Pkg.Json.DTO;

interface

uses
  System.Classes, System.Json, Rest.Json;

type
  TJsonDTO = class
  private
    FOptions: TJsonOptions;
    function GetAsJson: string;
    procedure SetAsJson(aValue: string);
    class function GetSubTypeItemClassFromList(ObjectList: TObject): TClass;
    class procedure PrettyPrintPair(aJSONValue: TJSONPair; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
    class procedure PrettyPrintJSON(aJSONValue: TJsonValue; aOutputStrings: TStrings; Indent: Integer = 0); overload;
    class procedure PrettyPrintArray(aJSONValue: TJSONArray; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
  public
    constructor Create; virtual;
    class function PrettyPrintJSON(aJson: string): string; overload;
    property AsJson: string read GetAsJson write SetAsJson;
  end;

implementation

uses
  System.Sysutils, Rest.JsonReflect, System.JSONConsts, System.Rtti, System.Generics.Collections;

{ TJsonDTO }

constructor TJsonDTO.Create;
begin
  inherited;
  FOptions := [joDateIsUTC, joDateFormatISO8601];
end;

function TJsonDTO.GetAsJson: string;
begin
  Result := TJson.ObjectToJsonString(Self, FOptions);
end;

const
  INDENT_SIZE = 2;

class procedure TJsonDTO.PrettyPrintJSON(aJSONValue: TJsonValue; aOutputStrings: TStrings; Indent: Integer);
var
  i: Integer;
  Ident: Integer;
begin
  Ident := Indent + INDENT_SIZE;
  i := 0;

  if aJSONValue is TJSONObject then
  begin
    aOutputStrings.Add(StringOfChar(' ', Ident) + '{');
    for i := 0 to TJSONObject(aJSONValue).Count - 1 do
      PrettyPrintPair(TJSONObject(aJSONValue).Pairs[i], aOutputStrings, i = TJSONObject(aJSONValue).Count - 1, Ident);

    aOutputStrings.Add(StringOfChar(' ', Ident) + '}');
  end
  else if aJSONValue is TJSONArray then
    PrettyPrintArray(TJSONArray(aJSONValue), aOutputStrings, i = TJSONObject(aJSONValue).Count - 1, Ident)
  else
    aOutputStrings.Add(StringOfChar(' ', Ident) + aJSONValue.ToString);
end;

class procedure TJsonDTO.PrettyPrintArray(aJSONValue: TJSONArray; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
var
  i: Integer;
begin
  aOutputStrings.Add(StringOfChar(' ', Indent + INDENT_SIZE) + '[');

  for i := 0 to aJSONValue.Count - 1 do
  begin
    PrettyPrintJSON(aJSONValue.Items[i], aOutputStrings, Indent);
    if i < aJSONValue.Count - 1 then
      aOutputStrings[aOutputStrings.Count - 1] := aOutputStrings[aOutputStrings.Count - 1] + ',';
  end;

  aOutputStrings.Add(StringOfChar(' ', Indent + INDENT_SIZE - 2) + ']');
end;

class function TJsonDTO.PrettyPrintJSON(aJson: string): string;
var
  StringList: TStringlist;
  JSONValue: TJsonValue;
begin
  StringList := TStringlist.Create;
  try
    JSONValue := TJSONObject.ParseJSONValue(aJson);
    try
      if JSONValue <> nil then
        PrettyPrintJSON(JSONValue, StringList);
    finally
      JSONValue.Free;
    end;

    Result := StringList.Text;
  finally
    StringList.Free;
  end;
end;

class procedure TJsonDTO.PrettyPrintPair(aJSONValue: TJSONPair; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
const
  TEMPLATE = '%s:%s';
var
  Line: string;
  NewList: TStringlist;
begin
  NewList := TStringlist.Create;
  try
    PrettyPrintJSON(aJSONValue.JSONValue, NewList, Indent);
    Line := Format(TEMPLATE, [aJSONValue.JsonString.ToString, Trim(NewList.Text)]);
  finally
    NewList.Free;
  end;

  Line := StringOfChar(' ', Indent + INDENT_SIZE) + Line;
  if not Last then
    Line := Line + ',';
  aOutputStrings.Add(Line);
end;

class function TJsonDTO.GetSubTypeItemClassFromList(ObjectList: TObject): TClass;
var
  CtxRtti: TRttiContext;
  TypeRtti: TRttiType;
  MethodRtti: TRttiMethod;
  ParameterRtti: TRttiParameter;
begin
  Result := nil;
  CtxRtti := TRttiContext.Create;
  TypeRtti := CtxRtti.GetType(ObjectList.ClassType);
  MethodRtti := TypeRtti.GetMethod('Add');

  for ParameterRtti in MethodRtti.GetParameters do
    if SameText(ParameterRtti.Name, 'Value') then
      if ParameterRtti.ParamType.IsInstance then
        Exit(ParameterRtti.ParamType.AsInstance.MetaclassType);
end;

procedure TJsonDTO.SetAsJson(aValue: string);
var
  JSONValue: TJsonValue;
  JSONObject: TJSONObject;
var
  RttiField: TRttiField;
  RttiContext: TRttiContext;

  Value: TValue;
  List: TObjectList<TObject>;
  ElementType: TClass;
  Element: TObject;
begin
  RttiContext := TRttiContext.Create;
  JSONValue := TJSONObject.ParseJSONValue(aValue);
  try
    if not Assigned(JSONValue) then
      Exit;

    if (JSONValue is TJSONArray) then
    begin
      RttiField := TRttiContext.Create.GetType(ClassInfo).GetField('FItems');
      Value := RttiField.GetValue(Self);
      List := TObjectList<TObject>(Value.AsObject);
      ElementType := GetSubTypeItemClassFromList(Value.AsObject);

      for JSONValue in (JSONValue as TJSONArray) do
      begin
        Element := ElementType.Create;
        if (JSONValue is TJSONObject) then
        begin
          TJson.JsonToObject(Element, TJSONObject(JSONValue), FOptions);
          List.Add(Element);
        end
        else
          Element.Free;
      end;

      Exit;
    end;

    if (JSONValue is TJSONObject) then
      JSONObject := JSONValue as TJSONObject
    else
    begin
      aValue := aValue.Trim;
      if (aValue = '') and not Assigned(JSONValue) or (aValue <> '') and Assigned(JSONValue) and JSONValue.Null then
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
