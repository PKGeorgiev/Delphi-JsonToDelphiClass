unit Pkg.Json.Mapper;

interface

uses
  System.Json, Rest.Json, System.SysUtils,
  System.Classes, Generics.Collections, Generics.Defaults;

type
{$M+}
  EJsonMapper = class(Exception);

  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtTrue, jtFalse, jtNumber, jtDate, jtDateTime, jtBytes, jtInteger, jtInteger64);

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
    procedure SetName(const Value: string);
  public
    constructor Create(aParentClass: TStubClass; aItemName: string; aFieldType: TJsonType);
    property FieldName: string read FFieldName write FFieldName;
    property name: string read FName;
    property JsonFieldName: string read FJsonFieldName write FJsonFieldName;
    property PropertyName: string read FPropertyName write FPropertyName;
    property FieldType: TJsonType read FFieldType write FFieldType;
    function GetTypeAsString: string; overload; virtual;
    class function GetTypeAsString(AType: TJsonType): string; overload;
  end;

  TStubContainerField = class(TStubField)
  private
    FFieldClass: TStubClass;
    FContainedType: TJsonType;
  public
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
  private
    FItems: TObjectList<TStubField>;
    FComplexItems, FArrayItems: TList<TStubField>;
    FName: string;
    FComparison: TComparison<TStubField>;
    FComparer: IComparer<TStubField>;
    FParentClass: TStubClass;
    FMapper: TPkgJsonMapper;
    FPureClassName: string;
    FArrayProperty: string;
    procedure SortFields;
    procedure SetName(const Value: string);
    procedure SetPureClassName(const Value: string);
  public
    constructor Create(aParentClass: TStubClass; aClassName: string; aMapper: TPkgJsonMapper; aArrayProperty: string = '');
    destructor Destroy; override;
    property name: string read FName write SetName;

    property Items: TObjectList<TStubField> read FItems write FItems;
    function GetDeclarationPart(const BaseClass: string): string;
    function GetImplementationPart: string;
    property PureClassName: string read FPureClassName write SetPureClassName;
    property ArrayProperty: string read FArrayProperty write FArrayProperty;
  end;

  TPkgJsonMapper = class
  private
    FStubClasses: TObjectList<TStubClass>;
    FRootClass: TStubClass;
    FUnitName: string;
    FClassName: string;
    procedure SetUnitName(const Value: string);
  protected
    function GetJsonType(aJsonValue: TJsonValue): TJsonType;
    function GetFirstArrayItem(aJsonValue: TJsonValue): TJsonValue;
    procedure ProcessJsonObject(aJsonValue: TJsonValue; aParentClass: TStubClass);
    function SuggestClassName(aSuggestedClassName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    // Parses a JSON string and creates internal stub class structure
    function Parse(aJsonString: string): TPkgJsonMapper;
    // Generates result unit
    function GenerateUnit: string;
    procedure Debug(aLines: TStrings);
    property DestinationClassName: string read FClassName write FClassName;
    property DestinationUnitName: string read FUnitName write SetUnitName;
    property RootClass: TStubClass read FRootClass;
  end;

procedure PrettyPrintJSON(JSONValue: TJsonValue; OutputStrings: TStrings; Indent: Integer = 0);

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
  end;

  { TStringsHelper }

procedure TStringsHelper.AddFormat(const aFormat: string; const Args: array of const);
begin
  Add(Format(aFormat, Args));
end;

function CapitaiazeFirst(const Value: string): string;
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

// http://stackoverflow.com/a/12198174
procedure PrettyPrintPair(JSONValue: TJSONPair; OutputStrings: TStrings; Last: Boolean; Indent: Integer);
const
  TEMPLATE = '%s:%s';
var
  Line: string;
  NewList: TStringList;
begin
  NewList := TStringList.Create;
  try
    PrettyPrintJSON(JSONValue.JSONValue, NewList, Indent);
    Line := Format(TEMPLATE, [JSONValue.JsonString.ToString, Trim(NewList.Text)]);
  finally
    NewList.Free;
  end;

  Line := StringOfChar(' ', Indent + INDENT_SIZE) + Line;
  if not Last then
    Line := Line + ',';
  OutputStrings.Add(Line);
end;

procedure PrettyPrintArray(JSONValue: TJSONArray; OutputStrings: TStrings; Last: Boolean; Indent: Integer);
var
  i: Integer;
begin
  OutputStrings.Add(StringOfChar(' ', Indent + INDENT_SIZE) + '[');

  for i := 0 to JSONValue.Count - 1 do
  begin
    PrettyPrintJSON(JSONValue.Items[i], OutputStrings, Indent);
    if i < JSONValue.Count - 1 then
      OutputStrings[OutputStrings.Count - 1] := OutputStrings[OutputStrings.Count - 1] + ',';
  end;

  OutputStrings.Add(StringOfChar(' ', Indent + INDENT_SIZE - 2) + ']');
end;

procedure PrettyPrintJSON(JSONValue: TJsonValue; OutputStrings: TStrings; Indent: Integer = 0);
var
  i: Integer;
  Ident: Integer;
begin
  Ident := Indent + INDENT_SIZE;
  i := 0;

  if JSONValue is TJSONObject then
  begin
    OutputStrings.Add(StringOfChar(' ', Ident) + '{');
    for i := 0 to TJSONObject(JSONValue).Count - 1 do
      PrettyPrintPair(TJSONObject(JSONValue).Pairs[i], OutputStrings, i = TJSONObject(JSONValue).Count - 1, Ident);

    OutputStrings.Add(StringOfChar(' ', Ident) + '}');
  end
  else if JSONValue is TJSONArray then
    PrettyPrintArray(TJSONArray(JSONValue), OutputStrings, i = TJSONObject(JSONValue).Count - 1, Ident)
  else
    OutputStrings.Add(StringOfChar(' ', Ident) + JSONValue.ToString);
end;

{ TPkgJsonMapper }

procedure TPkgJsonMapper.ProcessJsonObject(aJsonValue: TJsonValue; aParentClass: TStubClass);
var
  JSONObject: TJSONObject;
  JsonPair: TJSONPair;
  JSONValue, JsonValue2: TJsonValue;
  JsonType, JsonType2: TJsonType;
  StubClass: TStubClass;
begin
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
          StubClass := nil;
          JsonType2 := jtUnknown;

          JsonValue2 := GetFirstArrayItem(JSONValue);
          if JsonValue2 <> nil then
          begin
            JsonType2 := GetJsonType(JsonValue2);
            case JsonType2 of
              jtObject:
                begin
                  StubClass := TStubClass.Create(aParentClass, JsonPair.JsonString.Value, Self);
                  ProcessJsonObject(JsonValue2, StubClass);
                end;
              jtArray:
                raise EJsonMapper.Create('Nested Arrays are not supported!');
            end;
          end;

          TStubArrayField.Create(aParentClass, JsonPair.JsonString.Value, JsonType2, StubClass);
        end;
    else
      TStubField.Create(aParentClass, JsonPair.JsonString.Value, JsonType);
    end;
  end;

  aParentClass.SortFields;
end;

function TPkgJsonMapper.GenerateUnit: string;
var
  StubClass: TStubClass;
  StubField: TStubField;
  k: Integer;
  StringList: TStringList;
  NeedsAttribute: Boolean;
begin
  NeedsAttribute := false;
  for StubClass in FStubClasses do
  begin
    if StubClass.NeedsAttribute then
    begin
      NeedsAttribute := True;
      Break;
    end;

    for StubField in StubClass.Items do
      if StubField.NeedsAttribute then
      begin
        NeedsAttribute := True;
        Break;
      end;

    if NeedsAttribute then
      Break;
  end;

  StringList := TStringList.Create;
  try
    StringList.Add('unit ' + FUnitName + ';');
    StringList.Add('');
    StringList.Add('interface');
    StringList.Add('');
    StringList.Add('uses');
    StringList.Add('  Pkg.Json.DTO' + IfThen(NeedsAttribute, ', REST.Json.Types', '') + ';');
    StringList.Add('');
    StringList.Add('{$M+}');
    StringList.Add('');
    StringList.Add('type');

    for k := FStubClasses.Count - 1 downto 1 do
    begin
      StubClass := FStubClasses[k];
      StringList.Add(StubClass.GetDeclarationPart('').TrimRight);
      StringList.Add('');
    end;

    StringList.Add(FStubClasses.First.GetDeclarationPart('TJsonDTO').TrimRight + sLineBreak);
    StringList.Add('implementation');

    for k := FStubClasses.Count - 1 downto 0 do
    begin
      StubClass := FStubClasses[k];
      if k = 0 then
        StringList.Add('');
      StringList.Add(StubClass.GetImplementationPart.TrimRight);
    end;

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
    for StubField in StubClass.FItems do
      aLines.AddFormat('%-15s | %s', [StubField.FieldName, StubField.GetTypeAsString]);
  end;
end;

destructor TPkgJsonMapper.Destroy;
begin
  FreeAndNil(FStubClasses);
  inherited;
end;

procedure TPkgJsonMapper.SetUnitName(const Value: string);
begin
  FUnitName := Value;
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
  b: Boolean;
begin
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
      JSONValue.Free;
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
  FMapper.FStubClasses.Add(Self);
  FArrayProperty := aArrayProperty;

  FParentClass := aParentClass;

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
  ClassName: string;
  StubField: TStubField;
begin
  Lines := TStringList.Create;
  try
    ClassName := Format('%s', [FName]);

    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
    begin
      Lines.AddFormat('{ %s }', [ClassName]);
      Lines.Add('');
    end;

    if FComplexItems.Count > 0 then
    begin
      Lines.Add(Format('constructor %s.Create;', [ClassName]));
      Lines.Add('begin');
      Lines.Add('  inherited;');

      for StubField in FComplexItems do
        Lines.AddFormat('  %s := %s.Create;', [StubField.FieldName, (StubField).GetTypeAsString]);

      Lines.Add('end;');
      Lines.Add('');
    end;

    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
    begin
      Lines.Add(Format('destructor %s.Destroy;', [ClassName]));

      if FArrayItems.Count > 0 then
      begin
        Lines.Add('var');
        for StubField in FArrayItems do
          Lines.AddFormat('  %sItem: %s;', [StubField.FName, (StubField as TStubContainerField).FieldClass.Name]);
      end;

      Lines.Add('begin');

      if FArrayItems.Count > 0 then
      begin
        for StubField in FArrayItems do
        begin
          Lines.AddFormat('  for %sItem in %s do', [StubField.Name, StubField.FieldName]);
          Lines.AddFormat('    %sItem.Free;', [StubField.Name]);
        end;
        Lines.Add('');
      end;

      for StubField in FComplexItems do
        Lines.AddFormat('  %s.Free;', [StubField.FieldName]);

      Lines.Add('  inherited;');
      Lines.Add('end;')
    end;

    Lines.TrailingLineBreak := false;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TStubClass.SetName(const Value: string);
begin
  FPureClassName := Value;
  FName := FMapper.SuggestClassName('T' + FPureClassName + 'DTO');
end;

procedure TStubClass.SetPureClassName(const Value: string);
begin
  FPureClassName := Value;
end;

procedure TStubClass.SortFields;
begin
  FItems.Sort(FComparer);
end;

function TStubClass.GetDeclarationPart(const BaseClass: string): string;
var
  Lines: TStringList;
  s: string;
  StubField: TStubField;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    if NeedsAttribute then
      Lines.Add(NameAttribute);
    Lines.Add(FName + ' = class' + IfThen(BaseClass = '', '', '(' + BaseClass + ')'));
    Lines.Add('private');

    for StubField in FItems do
    begin
      if StubField.NeedsAttribute then
        Lines.Add('  ' + StubField.NameAttribute);

      s := Format('  %s: %s;', [StubField.FieldName, StubField.GetTypeAsString]);
      Lines.Add(s);
    end;

    Lines.Add('published');

    for StubField in FItems do
    begin
      if (StubField.FieldType = jtUnknown) or ((StubField is TStubContainerField) and ((StubField as TStubContainerField).ContainedType = jtUnknown)) then
        raise EJsonMapper.CreateFmt('The property [%s] has unknown type!', [StubField.PropertyName]);

      s := Format('  property %s: %s read %s write %s;', [StubField.PropertyName, StubField.GetTypeAsString, StubField.FieldName, StubField.FieldName]);
      Lines.Add(s);
    end;

    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
      Lines.Add('public');

    if FComplexItems.Count > 0 then
      Lines.Add('  constructor Create;');

    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
      Lines.Add('  destructor Destroy; override;');

    Lines.Add('end;');
    Lines.Add('');

    i := 0;

    for i := 0 to Lines.Count - 1 do
      Lines[i] := '  ' + Lines[i];

    Lines.TrailingLineBreak := false;
    Result := Lines.Text;
  finally
    Lines.Free;
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
    FParentClass.FItems.Add(Self);
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
  if (FParentClass.FArrayProperty <> '') and (FParentClass.FArrayProperty = FName) then
    FParentClass.FArrayProperty := Value;

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
  FContainedType := aItemSubType;
  FFieldClass := aItemClass;
  if FContainedType = TJsonType.jtObject then
    aClass.FArrayItems.Add(Self);
end;

function TStubArrayField.GetTypeAsString: string;
var
  LSubType: string;
begin
  case FContainedType of
    jtObject:
      LSubType := FFieldClass.Name;
    jtArray:
      raise EJsonMapper.Create('Nested arrays are not supported!');
  else
    LSubType := GetTypeAsString(FContainedType);
  end;
  Result := Format('TArray<%s>', [LSubType]);
end;

{ TStubObjectField }

constructor TStubObjectField.Create(aParentClass: TStubClass; aItemName: string; aItemClass: TStubClass);
begin
  inherited Create(aParentClass, aItemName, jtObject);
  FFieldClass := aItemClass;
  aParentClass.FComplexItems.Add(Self);
  FContainedType := jtObject;
end;

function TStubObjectField.GetTypeAsString: string;
begin
  Result := FFieldClass.Name;
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

constructor TSOJName.Create(aItemName: string);
var
  s: string;
  ch: Char;
begin
  inherited Create;

  if aItemName.IsEmpty then
    raise Exception.Create('aItemName can not be empty');

  FNeedsAttribute := false;
  FJsonName := aItemName;

  for ch in FJsonName do
    if ch.IsLetterOrDigit then
      s := s + ch
    else
      s := s + '_';

  FNeedsAttribute := s <> FJsonName;

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
