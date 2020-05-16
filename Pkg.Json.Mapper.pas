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

  TSOJName = class
  private
    FJsonName: string;
    FDelphiName: string;
    FNeedsAttribute: Boolean;
  protected
    function CapitaiazeFirst(const Value: string): string;
  published
    property JsonName: string read FJsonName;
    property DelphiName: string read FDelphiName;
    property NeedsAttribute: Boolean read FNeedsAttribute;
  public
    constructor Create(aItemName: string); reintroduce;
    function NameAttribute: string;
  end;

  TStubField = class(TSOJName)
  private
    FName: string;
    FPropertyName: string;
    FFieldName: string;
    FFieldType: TJsonType;
    FParentClass: TStubClass;
    FJsonFieldName: string;
    class function GetTypeAsString(AType: TJsonType): string; overload;
    procedure SetName(const Value: string);
  published
    property name: string read FName write SetName;
    property FieldName: string read FFieldName;
    property JsonFieldName: string read FJsonFieldName;
    property PropertyName: string read FPropertyName;
    property FieldType: TJsonType read FFieldType;
  public
    constructor Create(aParentClass: TStubClass; aItemName: string; aFieldType: TJsonType);
    function GetTypeAsString: string; overload; virtual;
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
  public
    constructor Create(aParentClass: TStubClass; aItemName: string; aItemClass: TStubClass);
    function GetTypeAsString: string; override;
  end;

  TStubArrayField = class(TStubContainerField)
  public
    constructor Create(aClass: TStubClass; aItemName: string; aItemSubType: TJsonType; aItemClass: TStubClass);
    function GetTypeAsString: string; override;
  end;

  TStubClass = class(TSOJName)
  strict private
    FArrayItems: TList<TStubField>;
    FComplexItems: TList<TStubField>;
    FItems: TObjectList<TStubField>;
    FName: string;
    FComparison: TComparison<TStubField>;
    FComparer: IComparer<TStubField>;
    FParentClass: TStubClass;
    FMapper: TPkgJsonMapper;
    FPureClassName: string;
    FArrayProperty: string;
    FStubClasses: TDictionary<string, TStubClass>;
    procedure SetName(const Value: string);
  public
    constructor Create(aParentClass: TStubClass; aClassName: string; aMapper: TPkgJsonMapper; aArrayProperty: string = '');
    destructor Destroy; override;
    function GetDeclarationPart(const BaseClass: string = ''): string;
    function GetImplementationPart: string;
    function HasPropety(aJsonName: string): Boolean;
    function GetItemFromName(aJsonName: string): TStubField;
    procedure SortFields;
  published
    property name: string read FName;
    property Items: TObjectList<TStubField> read FItems;
    property PureClassName: string read FPureClassName;
    property ArrayProperty: string read FArrayProperty write FArrayProperty;
    property ComplexItems: TList<TStubField> read FComplexItems;
    property ArrayItems: TList<TStubField> read FArrayItems;
    property StubClasses: TDictionary<string, TStubClass> read FStubClasses;
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
  JSONValue, JsonValue2: TJsonValue;
  JsonArray: TJSONArray;
  JsonType, JsonType2: TJsonType;
  StubClass: TStubClass;
  JsonName: string;

  function GetStub(aParentClass: TStubClass; aClassName: string; aMapper: TPkgJsonMapper): TStubClass;
  begin
    aParentClass.StubClasses.TryGetValue(aClassName, Result);
    if Result = nil then
      Exit(TStubClass.Create(aParentClass, JsonName, Self));
  end;

begin
  JSONObject := aJsonValue as TJSONObject;

  for JsonPair in JSONObject do
  begin
    JSONValue := JsonPair.JSONValue;
    JsonType := GetJsonType(JSONValue);
    JsonName := JsonPair.JsonString.Value;

    case JsonType of
      jtUnknown:
        { do nothing };
      jtObject:
        begin
          StubClass := TStubClass.Create(aParentClass, JsonName, Self);
          TStubObjectField.Create(aParentClass, JsonName, StubClass);
          ProcessJsonObject(JSONValue, StubClass);
        end;

      jtArray:
        begin
          StubClass := nil;
          JsonArray := JSONValue as TJSONArray;

          JsonType2 := jtObject;

          if JsonArray.Count = 0 then // if we meet an empty array then
            StubClass := GetStub(aParentClass, JsonName, Self);

          for JsonValue2 in JsonArray do
          begin
            JsonType2 := GetJsonType(JsonValue2);
            case JsonType2 of
              jtObject:
                begin
                  StubClass := GetStub(aParentClass, JsonName, Self);
                  ProcessJsonObject(JsonValue2, StubClass);
                end;
              jtArray:
                raise EJsonMapper.Create('Nested Arrays are not supported!');
            end;
          end;

          TStubArrayField.Create(aParentClass, JsonName, JsonType2, StubClass);
        end;
    else
      begin
        if (aParentClass = nil) or (not aParentClass.HasPropety(JsonName)) then
          TStubField.Create(aParentClass, JsonName, JsonType);
      end;
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
    StringList.free;
  end;
end;

function TPkgJsonMapper.GetFirstArrayItem(aJsonValue: TJsonValue): TJsonValue;
var
  JsonArray: TJSONArray;
begin
  JsonArray := aJsonValue as TJSONArray;
  if JsonArray.Count = 0 then
    Exit(nil)
  else
    Exit(JsonArray.Items[0]);
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
      aLines.AddFormat('%-15s | %s', [StubField.FieldName, StubField.GetTypeAsString]);
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
  begin
    if StubClass.Name.StartsWith(aSuggestedClassName, True) then
    begin
      s := Copy(StubClass.Name, length(aSuggestedClassName) + 2);

      if (s.length = 3) then
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
  end;

  if MaxValue > 0 then
    Result := Format('%s_%0.3d', [aSuggestedClassName, MaxValue]);
end;

function TPkgJsonMapper.GetJsonType(aJsonValue: TJsonValue): TJsonType;
var
  JsonString: TJSONString;
  i: Integer;
  j: Int64;
begin
  Result := jtUnknown;

  if aJsonValue is TJSONObject then
    Result := jtObject
  else if aJsonValue is TJSONArray then
    Result := jtArray
  else if (aJsonValue is TJSONNumber) then
  begin
    if TryStrToInt(aJsonValue.Value, i) then
      Exit(jtInteger);

    if TryStrToInt64(aJsonValue.Value, j) then
      Exit(jtInteger64);

    if (aJsonValue is TJSONNumber) then
      Exit(jtNumber);
  end;

  if aJsonValue is TJSONTrue then
    Exit(jtTrue);

  if aJsonValue is TJSONFalse then
    Exit(jtFalse);

  if aJsonValue is TJSONString then
  begin
    JsonString := (aJsonValue as TJSONString);
    if TRegEx.IsMatch(JsonString.Value, '^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}(?:\.\d*))(?:Z|(\+|-)([\d|:]*))?$') then
      Exit(jtDateTime);

    if TRegEx.IsMatch(JsonString.Value, '^([0-9]{4})(-?)(1[0-2]|0[1-9])\2(3[01]|0[1-9]|[12][0-9])$') then
      Exit(jtDate);

    Result := jtString
  end;
end;

function TPkgJsonMapper.Parse(aJsonString: string): TPkgJsonMapper;
var
  JSONValue, JsonValue2: TJsonValue;
  JsonType: TJsonType;
  StubClass: TStubClass;
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
            JsonType := jtUnknown;
            StubClass := nil;

            JsonValue2 := GetFirstArrayItem(JSONValue);
            if JsonValue2 <> nil then
            begin
              JsonType := GetJsonType(JsonValue2);
              StubClass := TStubClass.Create(FRootClass, 'Item', Self);
            end;

            FRootClass.ArrayProperty := 'Items';
            TStubArrayField.Create(FRootClass, 'Items', JsonType, StubClass);
            ProcessJsonObject(JsonValue2, StubClass);
          end;
      end;
    finally
      JSONValue.free;
    end;
  end
  else
    raise EJsonMapper.Create('Unable to parse the JSON String!');
end;

constructor TStubClass.Create(aParentClass: TStubClass; aClassName: string; aMapper: TPkgJsonMapper; aArrayProperty: string);
begin
  inherited Create(aClassName);
  FMapper := aMapper;
  SetName(DelphiName);

  FItems := TObjectList<TStubField>.Create;
  FComplexItems := TList<TStubField>.Create;
  FArrayItems := TList<TStubField>.Create;
  FMapper.StubClasses.Add(Self);
  FArrayProperty := aArrayProperty;
  FStubClasses := TDictionary<string, TStubClass>.Create();

  FParentClass := aParentClass;

  if FParentClass <> nil then
    FParentClass.StubClasses.Add(aClassName, Self);

  FComparison := function(const Left, Right: TStubField): Integer
    begin
      if Left.FName > Right.FName then
        Result := 1
      else if Left.FName < Right.FName then
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
      Lines.AddFormat('{ %s }', [FName]);
      Lines.Add('');
    end;

    if (FComplexItems.Count > 0) then
    begin
      Lines.AddFormat('constructor %s.Create;', [FName]);
      Lines.Add('begin');
      Lines.Add('  inherited;');

      for StubField in FComplexItems do
        Lines.AddFormat('  %s := %s.Create;', [StubField.FieldName, StubField.GetTypeAsString]);

      Lines.Add('end;');
      Lines.Add('');
    end;

    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
    begin
      Lines.Add(Format('destructor %s.Destroy;', [FName]));
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
      Lines.AddFormat('function %s.Get%s: TObjectList<%s>;', [FName, StubArrayField.Name, StubArrayField.FieldClass.Name]);
      Lines.Add('begin');
      Lines.AddFormat('  if not Assigned(%s) then', [StubArrayField.FieldName]);
      Lines.Add('  begin');
      Lines.AddFormat('    %s := TObjectList<%s>.Create;', [StubArrayField.FieldName, StubArrayField.FieldClass.Name]);
      Lines.AddFormat('    %s.AddRange(%sArray);', [StubArrayField.FieldName, StubArrayField.FieldName]);
      Lines.Add('  end;');
      Lines.AddFormat('  Result := %s;', [StubArrayField.FieldName]);
      Lines.Add('end;');
    end;

    Lines.TrailingLineBreak := False;
    Result := Lines.Text;
  finally
    Lines.free;
  end;
end;

function TStubClass.GetItemFromName(aJsonName: string): TStubField;
var
  StubField: TStubField;
begin
  for StubField in FItems do
    if AnsiSameText(aJsonName, StubField.JsonName) then
      Exit(StubField);

  Result := nil;
end;

function TStubClass.HasPropety(aJsonName: string): Boolean;
var
  StubField: TStubField;
begin
  for StubField in FItems do
    if AnsiSameText(aJsonName, StubField.JsonName) then
      Exit(True);
  Result := False;
end;

procedure TStubClass.SetName(const Value: string);
begin
  FPureClassName := Value;
  FName := FMapper.SuggestClassName('T' + FPureClassName + 'DTO');
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
    Lines.Add(FName + ' = class' + IfThen(BaseClass = '', '', '(' + BaseClass + ')'));
    if FItems.Count > 0 then
      Lines.Add('private');

    for StubField in FItems do
    begin
      if StubField is TStubArrayField then
      begin
        StubArrayField := StubField as TStubArrayField;
        Lines.Add('  ' + StubField.NameAttribute);
        Lines.AddFormat('  %sArray: %s;', [StubField.FieldName, StubField.GetTypeAsString]);
        Lines.Add('  [GenericListReflect]');
        Lines.AddFormat('  %s: TObjectList<%s>;', [StubField.FieldName, StubArrayField.FieldClass.Name]);
      end
      else
      begin
        if StubField.NeedsAttribute then
          Lines.Add('  ' + StubField.NameAttribute);

        Lines.AddFormat('  %s: %s;', [StubField.FieldName, StubField.GetTypeAsString]);
      end;
    end;

    for StubField in FArrayItems do
    begin
      StubArrayField := StubField as TStubArrayField;
      Lines.AddFormat('  function Get%s: TObjectList<%s>;', [StubField.Name, StubArrayField.FieldClass.Name]);
    end;

    if FItems.Count > 0 then
      Lines.Add('published');

    for StubField in FItems do
    begin
      if (StubField.FieldType = jtUnknown) or ((StubField is TStubContainerField) and ((StubField as TStubContainerField).ContainedType = jtUnknown)) then
        raise EJsonMapper.CreateFmt('The property [%s] has unknown type!', [StubField.PropertyName]);

      if StubField is TStubArrayField then
      begin
        StubArrayField := StubField as TStubArrayField;
        Lines.AddFormat('  property %s: TObjectList<%s> read Get%s;', [StubField.Name, StubArrayField.FieldClass.Name, StubArrayField.Name]);
      end
      else
        Lines.AddFormat('  property %s: %s read %s write %s;', [StubField.PropertyName, StubField.GetTypeAsString, StubField.FieldName, StubField.FieldName]);

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
    Lines.free;
  end;
end;

{ TVirtualClassItemBase }

constructor TStubField.Create(aParentClass: TStubClass; aItemName: string; aFieldType: TJsonType);
begin
  inherited Create(aItemName);

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
  if (FParentClass.ArrayProperty <> '') and (FParentClass.ArrayProperty = FName) then
    FParentClass.ArrayProperty := Value;

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
var
  SubType: string;
begin
  case ContainedType of
    jtObject:
      SubType := FieldClass.Name;
    jtArray:
      raise EJsonMapper.Create('Nested arrays are not supported!');
  else
    SubType := GetTypeAsString(ContainedType);
  end;

  Result := Format('TArray<%s>', [SubType]);
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
    List.free;
  end;
end;

constructor TSOJName.Create(aItemName: string);
var
  s: string;
  ch: Char;
begin
  inherited Create;

  if aItemName.IsEmpty then
    raise Exception.Create('aItemName can not be empty');

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
  Exit('[JSONName(' + AnsiQuotedStr(FJsonName, #39) + ')]');
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
