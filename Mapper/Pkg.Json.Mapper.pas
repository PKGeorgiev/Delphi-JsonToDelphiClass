unit Pkg.Json.Mapper;

interface

uses
  System.Json, Rest.Json, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,

  Pkg.Json.JSONName, Pkg.Json.StubField;

{$M+}

type
  TPkgJsonMapper = class
  strict private
    FStubClasses: TStubClassList;
    FRootClass: TStubClass;
    FUnitName: string;
    FClassName: string;
  strict protected
    function GetJsonType(aJsonValue: TJsonValue): TJsonType;
    function GetFirstArrayItem(aJSONArray: TJSONArray): TJsonValue;
    procedure ProcessJsonObject(aJsonValue: TJsonValue; aParentClass: TStubClass);
  public
    constructor Create;
    destructor Destroy; override;
    // Parses a JSON string and creates internal stub class structure
    function Parse(aJsonString: string): TPkgJsonMapper;
    // Generates result unit
    function GenerateUnit: string;
    function SuggestClassName(aSuggestedClassName: string): string;
    procedure Debug(aLines: TStrings);
  published
    property DestinationClassName: string read FClassName write FClassName;
    property DestinationUnitName: string read FUnitName write FUnitName;
    property RootClass: TStubClass read FRootClass;
    property StubClasses: TStubClassList read FStubClasses;
  end;

implementation

uses
  System.RegularExpressions, System.StrUtils, System.Character,
  Pkg.Json.ReservedWords, uUpdate;

const
  INDENT_SIZE = 2;

{ TPkgJsonMapper }

procedure TPkgJsonMapper.ProcessJsonObject(aJsonValue: TJsonValue; aParentClass: TStubClass);
var
  JSONObject: TJSONObject;
  JsonPair: TJSONPair;
  JSONValue: TJsonValue;
  JsonType: TJsonType;
  StubClass: TStubClass;
  JsonArray: TJSONArray;
begin
  if aJsonValue = nil then
    exit;

  if not(aJsonValue is TJSONObject) then
  begin
    JsonType := GetJsonType(aJsonValue);
    TStubField.Create(aParentClass, (aJsonValue as TJsonString).Value, JsonType);
    exit;
  end;

  JSONObject := aJsonValue as TJSONObject;
  for JsonPair in JSONObject do
  begin
    JSONValue := JsonPair.JSONValue;
    JsonType := GetJsonType(JSONValue);

    case JsonType of
      jtUnknown:
        { do nothing };
      jtObject:
        begin
          StubClass := TStubClass.Construct(aParentClass, JsonPair.JsonString.Value, Self.FStubClasses);
          TStubObjectField.Create(aParentClass, JsonPair.JsonString.Value, StubClass);
          ProcessJsonObject(JSONValue, StubClass);
        end;

      jtArray:
        begin
          JsonArray := TJSONArray(JSONValue);
          JSONValue := GetFirstArrayItem(JsonArray);
          JsonType := GetJsonType(JSONValue);

          StubClass := TStubClass.Construct(aParentClass, JsonPair.JsonString.Value, Self.FStubClasses);
          TStubArrayField.Create(aParentClass, JsonPair.JsonString.Value, JsonType, StubClass);

          for JSONValue in JsonArray do
            ProcessJsonObject(JSONValue, StubClass);
        end;
    else
      TStubField.Create(aParentClass, JsonPair.JsonString.Value, JsonType);
    end;
  end;

  aParentClass.SortFields;
end;

function TPkgJsonMapper.GenerateUnit: string;
var
  StringList: TStringList;
  i: Integer;
begin
  StringList := TStringList.Create;
  try
    StringList.TrailingLineBreak := False;

    StringList.Add('unit ' + FUnitName + ';');
    StringList.Add('');
    StringList.Add('interface');
    StringList.Add('');
    StringList.Add('uses');
    StringList.Add('  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;');
    StringList.Add('');
    StringList.Add('{$M+}');
    StringList.Add('');
    StringList.Add('type');

    for i := FStubClasses.Count - 1 downto 1 do
      StringList.AddIfNotEmpty(FStubClasses[i].GetDeclarationPart);
    StringList.Add(FStubClasses.First.GetDeclarationPart('TJsonDTO'));
    StringList.Add('implementation');

    for i := FStubClasses.Count - 1 downto 0 do
      StringList.AddIfNotEmpty(FStubClasses[i].GetImplementationPart);

    StringList.Add('');
    StringList.Add('end.');

    Result := StringList.Text;
  finally
    StringList.Free;
  end;
end;

function TPkgJsonMapper.GetFirstArrayItem(aJSONArray: TJSONArray): TJsonValue;
begin
  if (aJSONArray = nil) or (aJSONArray.Count = 0) then
    exit(nil);

  Result := aJSONArray.Items[0];
end;

constructor TPkgJsonMapper.Create;
begin
  inherited Create;
  FStubClasses := TStubClassList.Create;
  FClassName := 'Root';
end;

procedure TPkgJsonMapper.Debug(aLines: TStrings);
var
  StubClass: TStubClass;
  StubField: TStubField;
begin
  aLines.Clear;

  for StubClass in FStubClasses do
  begin
    aLines.Add('-------');
    aLines.Add(StubClass.Name);
    for StubField in StubClass.Items do
      aLines.AddFormat('%-15s | %s', [StubField.FieldName, StubField.TypeAsString]);
  end;
end;

destructor TPkgJsonMapper.Destroy;
begin
  FreeAndNil(FStubClasses);
  inherited;
end;

function TPkgJsonMapper.SuggestClassName(aSuggestedClassName: string): string;
var
  StubClass: TStubClass;
  MaxValue, i: Integer;
  s: string;
begin
  Result := aSuggestedClassName;
  MaxValue := 0;

  for StubClass in FStubClasses do
    if StubClass.Name.StartsWith(aSuggestedClassName, True) then
    begin
      s := Copy(StubClass.Name, Length(aSuggestedClassName) + 2);

      if (s.Length = 3) then
      begin
        if TryStrToInt(s, i) then
        begin
          inc(i);
          if i > MaxValue then
            MaxValue := i;
        end;
      end
      else
        MaxValue := 1;
    end;

  if MaxValue > 0 then
    Result := Format('%s_%0.3d', [aSuggestedClassName, MaxValue]);
end;

function TPkgJsonMapper.GetJsonType(aJsonValue: TJsonValue): TJsonType;
var
  JsonString: TJsonString;
  i: Integer;
  j: Int64;
  b: Boolean;
begin
  if aJsonValue = nil then
    exit(jtObject);

  if aJsonValue is TJSONObject then
    Result := jtObject
  else if aJsonValue is TJSONArray then
    Result := jtArray
  else if (aJsonValue is TJSONNumber) then
  begin
    if TryStrToInt(aJsonValue.Value, i) then
      Result := jtInteger
    else if TryStrToInt64(aJsonValue.Value, j) then
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
    if TRegEx.IsMatch(JsonString.Value, '^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}(?:\.\d*))(?:Z|(\+|-)([\d|:]*))?$') then
      Result := jtDateTime
    else if TRegEx.IsMatch(JsonString.Value, '^([0-9]{4})(-?)(1[0-2]|0[1-9])\2(3[01]|0[1-9]|[12][0-9])$') then
      Result := jtDate
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

function TPkgJsonMapper.Parse(aJsonString: string): TPkgJsonMapper;
var
  JSONValue: TJsonValue;
  JsonType: TJsonType;
  StubClass: TStubClass;
  JsonArray: TJSONArray;
begin
  Result := Self;
  FStubClasses.Clear;

  JSONValue := TJSONObject.ParseJSONValue(aJsonString);
  if JSONValue <> nil then
  begin
    try
      FRootClass := TStubClass.Construct(nil, FClassName, Self.FStubClasses);

      case GetJsonType(JSONValue) of
        jtObject:
          ProcessJsonObject(JSONValue, FRootClass);

        jtArray:
          begin
            JsonArray := TJSONArray(JSONValue);
            FRootClass.ArrayProperty := 'Items';
            StubClass := TStubClass.Construct(FRootClass, FRootClass.ArrayProperty, Self.FStubClasses);
            JsonType := GetJsonType(GetFirstArrayItem(JsonArray));
            TStubArrayField.Create(FRootClass, FRootClass.ArrayProperty, JsonType, StubClass);

            for JSONValue in JsonArray do
              ProcessJsonObject(JSONValue, StubClass);
          end;
      end;
    finally
      JSONValue.Free;
    end;
  end
  else
    raise EJsonMapper.Create('Unable to parse the JSON String!');
end;


end.
