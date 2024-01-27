unit Pkg.Json.Mapper;

interface

uses
  System.Json, Rest.Json, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,

  Pkg.Json.JSONName, Pkg.Json.StubField, Pkg.Json.JsonValueHelper;

{$M+}

type
  TPkgJsonMapper = class
  strict private
    FStubClasses: TStubClassList;
    FRootClass: TStubClass;
    FUnitName: string;
    FClassName: string;
  strict private
    FJsonString: string;
  protected
    function GetJsonType(aJsonValue: TJsonValue): TJsonType;
    function GetFirstArrayItem(aJSONArray: TJSONArray): TJsonValue;
    procedure ProcessJsonObject(aJsonValue: TJsonValue; aParentClass: TStubClass);
  public
    constructor Create;
    destructor Destroy; override;
    // Parses a JSON string and creates internal stub class structure
    function Parse(aJsonString: string): TPkgJsonMapper;
    function IsValid(aJsonString: string): boolean;

    function LoadFormFile(aJsonFile: string): TPkgJsonMapper;
    // Generates result unit
    function GenerateUnit: string;
    function SuggestClassName(aSuggestedClassName: string): string;
    procedure Debug(aLines: TStrings);
    procedure SaveToFile(aFileName: string);
  published
    property DestinationClassName: string read FClassName write FClassName;
    property DestinationUnitName: string read FUnitName write FUnitName;
    property RootClass: TStubClass read FRootClass;
    property StubClasses: TStubClassList read FStubClasses;
    property JsonString: string read FJsonString;
  end;

implementation

uses
  System.StrUtils, System.Character, System.IOUtils,
  Pkg.Json.ReservedWords, Pkg.Json.SubTypes;

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
    var name := (aJsonValue as TJsonString).Value;
    if Name = '' then
      Name := 'Element';

    TStubField.Create(aParentClass, Name, JsonType);
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

          StubClass := TStubClass.Construct(aParentClass, JsonPair.JsonString.Value, Self.FStubClasses, '', JsonType = jtObject);
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
  StubClass: TStubClass;
  Tmp: string;
  i: Integer;
  SubClasslist: TStringList;
begin
  TStubClass.ClearNames;
  StringList := TStringList.Create;
  SubClasslist := TStringList.Create;;
  SubClasslist.Duplicates := TDuplicates.dupIgnore;
  SubClasslist.Sorted := True;
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
      SubTypes.AddSubTypes(FStubClasses[i], SubClasslist);

    for Tmp in SubClasslist do
      StringList.Addformat('  %s = class;', [Tmp]);

    if SubClasslist.Count > 0 then
      StringList.Add('');

    SubClasslist.Free;

    for i := FStubClasses.Count - 1 downto 1 do
    begin
      StubClass := FStubClasses[i];
      Tmp := IfThen(StubClass.ArrayItems.Count > 0, 'TJsonDTO');
      StringList.AddIfNotEmpty(StubClass.GetDeclarationPart(Tmp));
    end;

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
      aLines.Addformat('%-15s | %s', [StubField.FieldName, StubField.TypeAsString]);
  end;
end;

destructor TPkgJsonMapper.Destroy;
begin
  FreeAndNil(FStubClasses);
  inherited;
end;

procedure TPkgJsonMapper.SaveToFile(aFileName: string);
var
  ResourceStream: TResourceStream;
  Buffer: TStringList;
begin
  with TStringList.Create do
    try
      Text := GenerateUnit;
      SaveToFile(aFileName);
    finally
      Free;
    end;

  Buffer := TStringList.Create;
  ResourceStream := TResourceStream.Create(HInstance, 'JsonDTO', 'PAS');
  try
    ResourceStream.Position := 0;
    Buffer.LoadFromStream(ResourceStream);
    Buffer.SaveToFile(TPath.GetDirectoryName(aFileName) + TPath.DirectorySeparatorChar + 'Pkg.Json.DTO.pas');
  finally
    ResourceStream.Free;
    Buffer.Free;
  end;
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
begin
  exit(TJsonValueHelper.GetJsonType(aJsonValue))
end;

function TPkgJsonMapper.IsValid(aJsonString: string): boolean;
var
  Value: TJSONValue;
begin
  Value := TJSONObject.ParseJSONValue(aJsonString);
  Result := Value <> nil;
  if Result then
    Value.Free;
end;

function TPkgJsonMapper.LoadFormFile(aJsonFile: string): TPkgJsonMapper;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  StringList.LoadFromFile(aJsonFile);
  Result := Parse(StringList.Text);
  StringList.Free;
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
  FJsonString := aJsonString;
  TStubClass.ClearNames;
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
