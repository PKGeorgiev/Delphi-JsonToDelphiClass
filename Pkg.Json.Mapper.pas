unit Pkg.Json.Mapper;

interface

uses
  System.Json, Rest.Json, System.SysUtils,
  System.Classes, Generics.Collections, Generics.Defaults;

type
{$M+}
  EJsonMapper = class(Exception);

  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtTrue, jtFalse, jtNumber, jtDate, jtDateTime, jtBytes, jtInteger, jtInteger64);

type
  TStubClass = class;
  TPkgJsonMapper = class;

  TSOJName = class abstract
  strict private
    FJsonName: string;
    FDelphiName: string;
    FNeedsAttribute: Boolean;
    FMapper: TPkgJsonMapper;
    FName: string;
    FPureClassName: string;
  protected
    procedure SetName(const Value: string); virtual;
    function CapitaiazeFirst(const Value: string): string;
  published
    property JsonName: string read FJsonName;
    property DelphiName: string read FDelphiName;
    property NeedsAttribute: Boolean read FNeedsAttribute;
    property PureClassName: string read FPureClassName write FPureClassName;
    property Name: string read FName write SetName;
  public
    constructor Create(aMapper: TPkgJsonMapper; aItemName: string); reintroduce;
    function NameAttribute: string;
  end;

  TStubField = class(TSOJName)
  private
    FPropertyName: string;
    FFieldName: string;
    FFieldType: TJsonType;
    FParentClass: TStubClass;
    FJsonFieldName: string;
    FName: string;
  strict protected
    procedure SetName(const Value: string); override;
    class function GetTypeAsString(AType: TJsonType): string; overload;
    function GetTypeAsString: string; overload; virtual;
  public
    function IsObjectArrayField: Boolean;
  published
    property Name: string read FName write SetName;
    property FieldName: string read FFieldName;
    property JsonFieldName: string read FJsonFieldName;
    property PropertyName: string read FPropertyName;
    property FieldType: TJsonType read FFieldType;

  public
    constructor Create(aParentClass: TStubClass; aItemName: string; aFieldType: TJsonType);
    property TypeAsString: string read GetTypeAsString;

  end;

  TStubContainerField = class(TStubField)
  strict private
    FFieldClass: TStubClass;
    FContainedType: TJsonType;
  published
    property ContainedType: TJsonType read FContainedType write FContainedType;
    property FieldClass: TStubClass read FFieldClass write FFieldClass;
  end;

  TStubObjectField = class(TStubContainerField)
  strict protected
    function GetTypeAsString: string; override;
  public
    constructor Create(aParentClass: TStubClass; aItemName: string; aItemClass: TStubClass);
  end;

  TStubArrayField = class(TStubContainerField)
  strict protected
    function GetTypeAsString: string; override;
  public
    constructor Create(aClass: TStubClass; aItemName: string; aItemSubType: TJsonType; aItemClass: TStubClass);
  end;

  TStubClass = class(TSOJName)
  strict private
    FArrayItems: TList<TStubField>;
    FComplexItems: TList<TStubField>;
    FItems: TObjectList<TStubField>;
    FComparison: TComparison<TStubField>;
    FComparer: IComparer<TStubField>;
    FParentClass: TStubClass;
    FMapper: TPkgJsonMapper;
    FArrayProperty: string;
  public
    constructor Create(aParentClass: TStubClass; aClassName: string; aMapper: TPkgJsonMapper; aArrayProperty: string = ''); virtual;
    destructor Destroy; override;
    function GetDeclarationPart(const BaseClass: string = ''): string;
    function GetImplementationPart: string;
    procedure SortFields;
  published
    property Items: TObjectList<TStubField> read FItems write FItems;
    property ArrayProperty: string read FArrayProperty write FArrayProperty;
    property ComplexItems: TList<TStubField> read FComplexItems;
    property ArrayItems: TList<TStubField> read FArrayItems;
  end;

  TPkgJsonMapper = class
  strict private
    FStubClasses: TObjectList<TStubClass>;
    FRootClass: TStubClass;
    FUnitName: string;
    FClassName: string;
  strict protected
    function GetJsonType(aJsonValue: TJsonValue): TJsonType;
    function GetFirstArrayItem(aJsonValue: TJsonValue): TJsonValue;
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
    property StubClasses: TObjectList<TStubClass> read FStubClasses;
  end;

implementation

uses
  System.RegularExpressions, System.StrUtils, System.Character,
  uUpdate;

var
  ReservedWords: TList<string>;

const
  INDENT_SIZE = 2;

type
  TStringsHelper = class helper for TStrings
  public
    procedure AddFormat(const aFormat: string; const Args: array of const);
    procedure AddIfNotEmpty(const s: string);
  end;

  { TStringsHelper }

procedure TStringsHelper.AddIfNotEmpty(const s: string);
begin
  if s <> '' then
    inherited Add(s);
end;

procedure TStringsHelper.AddFormat(const aFormat: string; const Args: array of const);
begin
  Add(Format(aFormat, Args));
end;

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
          StubClass := TStubClass.Create(aParentClass, JsonPair.JsonString.Value, Self);
          TStubObjectField.Create(aParentClass, JsonPair.JsonString.Value, StubClass);
          ProcessJsonObject(JSONValue, StubClass);
        end;

      jtArray:
        begin
          JsonArray := TJSONArray(JSONValue);
          JsonValue := GetFirstArrayItem(JSONValue);
          JsonType := GetJsonType(JsonValue);

          StubClass := TStubClass.Create(aParentClass, JsonPair.JsonString.Value, Self);
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

function TPkgJsonMapper.GetFirstArrayItem(aJsonValue: TJsonValue): TJsonValue;
var
  JsonArray: TJSONArray;
begin
  JsonArray := aJsonValue as TJSONArray;
  if JsonArray.Count = 0 then
    exit(nil)
  else
    exit(JsonArray.Items[0]);
end;

constructor TPkgJsonMapper.Create;
begin
  inherited Create;
  FStubClasses := TObjectList<TStubClass>.Create;
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
  JsonString: TJSONString;
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
  else if aJsonValue is TJSONString then
  begin
    JsonString := (aJsonValue as TJSONString);
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
      FRootClass := TStubClass.Create(nil, FClassName, Self);

      case GetJsonType(JSONValue) of
        jtObject:
          ProcessJsonObject(JSONValue, FRootClass);

        jtArray:
          begin
            JsonArray := TJSONArray(JSONValue);
            FRootClass.ArrayProperty := 'Items';
            StubClass := TStubClass.Create(FRootClass, FRootClass.ArrayProperty, Self);
            JsonType := GetJsonType(GetFirstArrayItem(JSONValue));
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

constructor TStubClass.Create(aParentClass: TStubClass; aClassName: string; aMapper: TPkgJsonMapper; aArrayProperty: string);
begin
  inherited Create(aMapper, aClassName);
  FMapper := aMapper;
  SetName(DelphiName);

  FItems := TObjectList<TStubField>.Create;
  FComplexItems := TList<TStubField>.Create;
  FArrayItems := TList<TStubField>.Create;
  FMapper.StubClasses.Add(Self);
  FArrayProperty := aArrayProperty;

  FParentClass := aParentClass;

  FComparison := function(const Left, Right: TStubField): Integer
    begin
      if Left.Name > Right.Name then
        Result := 1
      else if Left.Name < Right.Name then
        Result := -1
      else
        Result := 0;
    end;

  FComparer := TComparer<TStubField>.Construct(FComparison);
end;

destructor TStubClass.Destroy;
begin
  FreeAndNil(FComplexItems);
  FreeAndNil(FItems);
  FreeAndNil(FArrayItems);
  inherited;
end;

function TStubClass.GetImplementationPart: string;
var
  Lines: TStringList;
  StubField: TStubField;
  StubArrayField: TStubArrayField;
begin
  Lines := TStringList.Create;
  try
    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
    begin
      Lines.Add('');
      Lines.AddFormat('{ %s }', [Name]);
      Lines.Add('');
    end;

    if (FComplexItems.Count > 0) then
    begin
      Lines.AddFormat('constructor %s.Create;', [Name]);
      Lines.Add('begin');
      Lines.Add('  inherited;');

      for StubField in FComplexItems do
        Lines.AddFormat('  %s := %s.Create;', [StubField.FieldName, StubField.TypeAsString]);

      Lines.Add('end;');
      Lines.Add('');
    end;

    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
    begin
      Lines.Add(Format('destructor %s.Destroy;', [Name]));
      Lines.Add('begin');

      for StubField in FComplexItems do
        Lines.AddFormat('  %s.Free;', [StubField.FieldName]);

      for StubField in FArrayItems do
        Lines.AddFormat('  Get%s.Free;', [StubField.Name]);

      Lines.Add('  inherited;');
      Lines.Add('end;');
    end;

    for StubField in FArrayItems do
    begin
      StubArrayField := StubField as TStubArrayField;
      Lines.Add('');
      Lines.AddFormat('function %s.Get%s: TObjectList<%s>;', [Name, StubArrayField.Name, StubArrayField.TypeAsString]);
      Lines.Add('begin');
      Lines.AddFormat('  if not Assigned(%s) then', [StubArrayField.FieldName]);
      Lines.Add('  begin');
      Lines.AddFormat('    %s := TObjectList<%s>.Create;', [StubArrayField.FieldName, StubArrayField.TypeAsString]);
      Lines.AddFormat('    %s.AddRange(%sArray);', [StubArrayField.FieldName, StubArrayField.FieldName]);
      Lines.Add('  end;');
      Lines.AddFormat('  Result := %s;', [StubArrayField.FieldName]);
      Lines.Add('end;');
    end;

    Lines.TrailingLineBreak := False;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TStubClass.SortFields;
begin
  FItems.Sort(FComparer);
end;

function TStubClass.GetDeclarationPart(const BaseClass: string): string;
var
  Lines: TStringList;
  StubField: TStubField;
  StubArrayField: TStubArrayField;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add(Name + ' = class' + IfThen(BaseClass = '', '', '(' + BaseClass + ')'));
    if FItems.Count > 0 then
      Lines.Add('private');

    for StubField in FItems do
    begin
      if StubField.IsObjectArrayField then
      begin
        StubArrayField := StubField as TStubArrayField;
        Lines.Add('  ' + StubField.NameAttribute);
        Lines.AddFormat('  %sArray: TArray<%s>;', [StubField.FieldName, StubField.TypeAsString]);
        Lines.Add('  [GenericListReflect]');
        Lines.AddFormat('  %s: TObjectList<%s>;', [StubField.FieldName, StubArrayField.TypeAsString]);
      end
      else
      begin
        if StubField.NeedsAttribute then
          Lines.Add('  ' + StubField.NameAttribute);

        Lines.AddFormat('  %s: %s;', [StubField.FieldName, StubField.TypeAsString]);
      end;
    end;

    for StubField in FArrayItems do
    begin
      StubArrayField := StubField as TStubArrayField;
      Lines.AddFormat('  function Get%s: TObjectList<%s>;', [StubField.Name, StubArrayField.TypeAsString]);
    end;

    if FItems.Count > 0 then
      Lines.Add('published');

    for StubField in FItems do
    begin
      if (StubField.FieldType = jtUnknown) or ((StubField is TStubContainerField) and ((StubField as TStubContainerField).ContainedType = jtUnknown)) then
        raise EJsonMapper.CreateFmt('The property [%s] has unknown type!', [StubField.PropertyName]);

      if StubField.IsObjectArrayField then
      begin
        StubArrayField := StubField as TStubArrayField;
        Lines.AddFormat('  property %s: TObjectList<%s> read Get%s;', [StubField.Name, StubArrayField.TypeAsString, StubArrayField.Name]);
      end
      else
        Lines.AddFormat('  property %s: %s read %s write %s;', [StubField.PropertyName, StubField.TypeAsString, StubField.FieldName, StubField.FieldName]);

    end;

    if FComplexItems.Count > 0 then
    begin
      Lines.Add('public');
      Lines.Add('  constructor Create;' + IfThen(BaseClass = '', '', ' override;'));
    end;

    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
      Lines.Add('  destructor Destroy; override;');

    Lines.Add('end;');
    Lines.Add('');

    for i := 0 to Lines.Count - 1 do
      Lines[i] := '  ' + Lines[i];

    Lines.TrailingLineBreak := False;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

{ TVirtualClassItemBase }

constructor TStubField.Create(aParentClass: TStubClass; aItemName: string; aFieldType: TJsonType);
begin
  inherited Create(nil, aItemName);

  FParentClass := aParentClass;
  FFieldType := aFieldType;
  SetName(DelphiName);

  if FParentClass <> nil then
    FParentClass.Items.Add(Self);
end;

class function TStubField.GetTypeAsString(AType: TJsonType): string;
begin
  case AType of
    jtUnknown:
      Result := 'Unknown';
    jtString:
      Result := 'string';
    jtTrue, jtFalse:
      Result := 'Boolean';
    jtNumber:
      Result := 'Double';
    jtDate:
      Result := 'TDate';
    jtDateTime:
      Result := 'TDateTime';
    jtBytes:
      Result := 'Byte';
    jtInteger:
      Result := 'Integer';
    jtInteger64:
      Result := 'Int64';
  end;
end;

procedure TStubField.SetName(const Value: string);
begin
  if (FParentClass.ArrayProperty <> '') and (FParentClass.ArrayProperty = Name) then
    FParentClass.ArrayProperty := Value;

  PureClassName := Value;
  FName := Value;
  FFieldName := 'F' + DelphiName;

  if ReservedWords.Contains(Value.ToLower) then
    FPropertyName := '&' + Value
  else
    FPropertyName := Value;
end;

function TStubField.GetTypeAsString: string;
begin
  Result := GetTypeAsString(FFieldType);
end;

function TStubField.IsObjectArrayField: Boolean;
begin
  Result := (Self is TStubArrayField) and ((Self as TStubArrayField).ContainedType = jtObject);
end;

{ TArrayItem }

constructor TStubArrayField.Create(aClass: TStubClass; aItemName: string; aItemSubType: TJsonType; aItemClass: TStubClass);
begin
  inherited Create(aClass, aItemName, jtArray);
  ContainedType := aItemSubType;
  FieldClass := aItemClass;
  if ContainedType = TJsonType.jtObject then
    aClass.ArrayItems.Add(Self);
end;

function TStubArrayField.GetTypeAsString: string;
begin
  case ContainedType of
    jtObject:
      Result := FieldClass.Name;
    jtArray:
      raise EJsonMapper.Create('Nested arrays are not supported!');
  else
    Result := Format('TArray<%s>', [GetTypeAsString(ContainedType)]);
  end;
end;

{ TStubObjectField }

constructor TStubObjectField.Create(aParentClass: TStubClass; aItemName: string; aItemClass: TStubClass);
begin
  inherited Create(aParentClass, aItemName, jtObject);
  FieldClass := aItemClass;
  aParentClass.ComplexItems.Add(Self);
  ContainedType := jtObject;
end;

function TStubObjectField.GetTypeAsString: string;
begin
  Result := FieldClass.Name;
end;

{ TSOJName }

function TSOJName.CapitaiazeFirst(const Value: string): string;
var
  List: TStringList;
  s: string;
  i: Integer;
begin
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

constructor TSOJName.Create(aMapper: TPkgJsonMapper; aItemName: string);
var
  s: string;
  ch: Char;
begin
  inherited Create;

  if aItemName.IsEmpty then
    raise Exception.Create('aItemName can not be empty');

  FMapper := aMapper;
  FNeedsAttribute := False;
  FJsonName := aItemName;

  for ch in FJsonName do
    if ch.IsLetterOrDigit then
      s := s + ch
    else
      s := s + '_';

  FNeedsAttribute := (s <> FJsonName) or (aItemName.StartsWith('_'));

  if s.StartsWith('_') then
    s := s.Substring(1);

  FDelphiName := CapitaiazeFirst(s);
  if not FDelphiName[1].IsLetter then
    FDelphiName := '_' + FDelphiName;
end;

function TSOJName.NameAttribute: string;
begin
  exit('[JSONName(' + AnsiQuotedStr(FJsonName, #39) + ')]');
end;

procedure TSOJName.SetName(const Value: string);
begin
  FPureClassName := Value;
  FName := FMapper.SuggestClassName('T' + FPureClassName + 'DTO');
end;

initialization

ReservedWords := TList<string>.Create;
ReservedWords.Add('type');
ReservedWords.Add('for');
ReservedWords.Add('var');
ReservedWords.Add('begin');
ReservedWords.Add('end');
ReservedWords.Add('function');
ReservedWords.Add('procedure');
ReservedWords.Add('class');
ReservedWords.Add('record');
ReservedWords.Add('string');
ReservedWords.Add('initialization');
ReservedWords.Add('finalization');

finalization

FreeAndNil(ReservedWords);

end.
