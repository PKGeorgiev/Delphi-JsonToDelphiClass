unit Pkg.Json.Mapper;

interface
uses FMX.TreeView, System.JSON, Rest.Json, RTTI, RegularExpressions, TypInfo,
  SysUtils, classes, Generics.Collections, Generics.Defaults;

type

  EJsonMapper = class(Exception);

  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtTrue, 
    jtFalse, jtNumber, jtDate, jtDateTime, jtBytes);

  TStubClass = class;
  TPkgJsonMapper = class;

  TStubField = class
  private
    FName: string;
    FPropertyName: string;
    FFieldName: string;
    FFieldType: TJsonType;
    FParentClass: TStubClass;
    procedure SetName(const Value: string);
  public
    constructor     Create(AParentClass: TStubClass; AItemName: string; AFieldType: TJsonType);
    destructor      Destroy; override;
    property        Name: string read FName write SetName;
    property        FieldName: string read FFieldName write FFieldName;
    property        PropertyName: string read FPropertyName write FPropertyName;
    property        FieldType: TJsonType read FFieldType write FFieldType;
    function        GetTypeAsString: string; overload; virtual;
    class function  GetTypeAsString(AType: TJsonType): string; overload;
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
  private
  public
    constructor Create(AParentClass: TStubClass; AItemName: string; AItemClass: TStubClass);
    function    GetTypeAsString: string; override;
  end;

  TStubArrayField = class(TStubContainerField)
  private
  public
    constructor Create(AClass: TStubClass; AItemName: string; AItemSubType: TJsonType; AItemClass: TStubClass);
    function    GetTypeAsString: string; override;
  end;

  TStubClass = class
  private
    FItems,
    FComplexItems,
    FArrayItems: TList<TStubField>;
    FName: string;
    FComparison: TComparison<TStubField>;
    FComparer: IComparer<TStubField>;
    FParentClass: TStubClass;
    FMapper: TPkgJsonMapper;
    FPureClassName: string;
    procedure SortFields;
    procedure SetName(const Value: string);
    procedure SetPureClassName(const Value: string);
  public
    constructor Create(AParentClass: TStubClass; AClassName: string; AMapper: TPkgJsonMapper);
    destructor  Destroy; override;
    property    Name: string read FName write SetName;
    property    Items: TList<TStubField> read FItems write FItems;
    function    GetDeclarationPart: string;
    function    GetImplementationPart: string;
    property    PureClassName: string read FPureClassName write SetPureClassName;

  end;

  TPkgJsonMapper = class
    private
      FTreeView: TTreeView;
      FClasses: TList<TStubClass>;
      FRootClass: TStubClass;
      FUnitName: string;
      procedure SetUnitName(const Value: string);
    protected
      function  GetJsonType(AJsonValue: TJsonValue): TJsonType;
      function  GetFirstArrayItem(AJsonValue: TJsonValue): TJsonValue;
      procedure ProcessJsonObject(AJsonValue: TJsonValue; AParentClass: TStubClass);
      procedure ClearClasses;
      procedure InternalFormatTreeViewFields(AItem: TTreeViewItem);
      procedure FormatFields(ATreeView: TTreeView);
      procedure InternalVisualize(ATreeViewItem: TTreeViewItem; AClass: TStubClass; AItemStyleLookup: string);
      function  SuggestClassName(ASuggestedClassName: string): string;
    public
      constructor Create(ATreeView: TTreeView);
      destructor  Destroy; override;
      //  Parses a JSON string and creates internal stub class structure
      procedure   Parse(AJsonString: string; ARootClassName: string = 'Root');
      //  Generates resultant unit
      function    GenerateUnit: string;
      procedure   Debug(ALines: TStrings);
      //  Visualizes stub class structure in a treeview
      procedure   Visualize(ATreeView: TTreeView; AItemStyleLookup: string);
      property    DestinationUnitName: string read FUnitName write SetUnitName;
  end;

procedure PrettyPrintJSON(JSONValue: TJSONValue; OutputStrings: TStrings; indent: integer = 0);

var
  PointDsFormatSettings: TFormatSettings;

implementation

uses uUpdate;

var
  ReservedWords: TList<string>;

const INDENT_SIZE = 2;

//  http://stackoverflow.com/a/12198174
procedure PrettyPrintPair(JSONValue: TJSONPair; OutputStrings: TStrings; last: boolean; indent: integer);
const TEMPLATE = '%s:%s';
var
  line: string;
  newList: TStringList;
begin
  newList := TStringList.Create;
  try
    PrettyPrintJSON(JSONValue.JsonValue, newList, indent);
    line := format(TEMPLATE, [JSONValue.JsonString.ToString, Trim(newList.Text)]);
  finally
    newList.Free;
  end;

  line := StringOfChar(' ', indent + INDENT_SIZE) + line;
  if not last then
    line := line + ',';
  OutputStrings.add(line);
end;

procedure PrettyPrintArray(JSONValue: TJSONArray; OutputStrings: TStrings; last:     boolean; indent: integer);
var i: integer;
begin
   OutputStrings.add(StringOfChar(' ', indent + INDENT_SIZE) + '[');
  for i := 0 to JSONValue.Count - 1 do
      begin
      PrettyPrintJSON(JSONValue.Items[i], OutputStrings, indent);
      if i < JSONValue.Count - 1 then
         OutputStrings[OutputStrings.Count-1] := OutputStrings[OutputStrings.Count-1] + ',';
      end;
   OutputStrings.add(StringOfChar(' ', indent + INDENT_SIZE - 2) + ']');
end;

procedure PrettyPrintJSON(JSONValue: TJSONValue; OutputStrings: TStrings; indent: integer = 0);
var
  i: integer;
  LIdent: integer;
begin
  LIdent := indent + INDENT_SIZE;
  i := 0;

  if JSONValue is TJSONObject then
  begin
    OutputStrings.add(StringOfChar(' ', LIdent) + '{');
    for i := 0 to TJSONObject(JSONValue).Count - 1 do
      PrettyPrintPair(TJSONObject(JSONValue).Pairs[i], OutputStrings, i = TJSONObject(JSONValue).Count - 1, LIdent);
    OutputStrings.add(StringOfChar(' ', LIdent) + '}');
  end
  else if JSONValue is TJSONArray then
    PrettyPrintArray(TJSONArray(JSONValue), OutputStrings, i = TJSONObject(JSONValue).Count - 1, LIdent)
  else OutputStrings.add(StringOfChar(' ', LIdent) + JSONValue.ToString);
end;


{ TPkgJsonMapper }


procedure TPkgJsonMapper.ProcessJsonObject(AJsonValue: TJsonValue; AParentClass: TStubClass);
var
  LJsonObj: TJSONObject;
  LJsonPair: TJSONPair;
  LJsonVal,
  LJsonVal2: TJSONValue;
  LJsonType,
  LJsonType2: TJsonType;
  LClass: TStubClass;
begin
  LJsonObj := AJsonValue as TJSONObject;

  for LJsonPair in LJsonObj do
  begin
    LJsonVal := LJsonPair.JsonValue;
    LJsonType := GetJsonType(LJsonVal);

    case LJsonType of
      jtObject:
      begin
        LClass := TStubClass.Create(AParentClass, LJsonPair.JsonString.Value, self);
        TStubObjectField.Create(AParentClass, LJsonPair.JsonString.Value, LClass);
        ProcessJsonObject(LJsonVal, LClass);
      end;

      jtArray:
      begin
        LClass := nil;
        LJsonType2 := jtUnknown;

        LJsonVal2 := GetFirstArrayItem(LJsonVal);
        if LJsonVal2 <> nil then
        begin
          LJsonType2 := GetJsonType(LJsonVal2);
          case LJsonType2 of
            jtObject:
            begin
              LClass := TStubClass.Create(AParentClass, LJsonPair.JsonString.Value, self);
              ProcessJsonObject(LJsonVal2, LClass);
            end;
            jtArray: raise EJsonMapper.Create('Nested Arrays are not supported!');
          end;
        end;

        if LJsonType2<>jtUnknown then
          TStubArrayField.Create(AParentClass, LJsonPair.JsonString.Value, LJsonType2, LClass);

      end;
      jtNumber,
      jtString,
      jtDate,
      jtDateTime,
      jtTrue,
      jtFalse: TStubField.Create(AParentClass, LJsonPair.JsonString.Value, LJsonType);
    end;
  end;

  AParentClass.SortFields;
end;


function TPkgJsonMapper.GenerateUnit: string;
var
  LClass: TStubClass;
  k: integer;
  LList: TStringList;
begin

  LList := TStringList.Create;
  try

    LList.Add('unit ' + FUnitName + ';');
    LList.Add('');
    LList.Add('//  *************************************************');
    LList.Add('//    Generated By: JsonToDelphiClass - ' + FloatToStr(ProgramVersion, PointDsFormatSettings));
    LList.Add('//    Project link: https://github.com/PKGeorgiev/Delphi-JsonToDelphiClass');
    LList.Add('//    Generated On: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', now));
    LList.Add('//  *************************************************');
    LList.Add('//    Created By  : Petar Georgiev - 2014');
    LList.Add('//    WebSite     : http://pgeorgiev.com');
    LList.Add('//  *************************************************');
    LList.Add('');
    LList.Add('interface');
    LList.Add('');
    LList.Add('uses Generics.Collections, Rest.Json;');
    LList.Add('');
    LList.Add('type');

    for k := FClasses.Count - 1 downto 0 do
    begin
      LClass := FClasses[k];
      LList.Add(LClass.GetDeclarationPart.TrimRight);
    end;             

    LList.Add('');
    LList.Add('implementation');

    for k := FClasses.Count - 1 downto 0 do
    begin
      LClass := FClasses[k];
      LList.Add(LClass.GetImplementationPart.TrimRight);
    end;

    LList.Add('');
    LList.Add('end.');

    result := LList.Text;
  
  finally
    LList.Free;
  end;
  
end;

procedure TPkgJsonMapper.Visualize(ATreeView: TTreeView; AItemStyleLookup: string);
var
  LItem: TTreeViewItem;
begin
  ATreeView.Clear;
  if FRootClass <> nil then
  begin
    ATreeView.BeginUpdate;
    LItem := TTreeViewItem.Create(ATreeView);
    LItem.Text := FRootClass.Name;
    LItem.TagObject := FRootClass;
    LItem.WordWrap := false;
    ATreeView.AddObject(LItem);
    InternalVisualize(LItem, FRootClass, AItemStyleLookup);
    FormatFields(ATreeView);
    ATreeView.EndUpdate;
    ATreeView.ExpandAll;
  end;
end;

function TPkgJsonMapper.GetFirstArrayItem(AJsonValue: TJsonValue): TJsonValue;
var
  LJsonArray: TJsonArray;
  LJsonValue: TJSONValue;
begin
  result := nil;
  LJsonArray := AJsonValue as TJsonArray;
  for LJsonValue in LJsonArray do
  begin
    result := LJsonValue;
    break;
  end;
end;

procedure TPkgJsonMapper.ClearClasses;
var
  LClass: TStubClass;
begin
  for LClass in FClasses do
  begin
    LClass.Free;
  end;

  FClasses.Clear;
end;

constructor TPkgJsonMapper.Create(ATreeView: TTreeView);
begin
  inherited Create;
  FTreeView := ATreeView;
  FClasses := TList<TStubClass>.Create;
end;

procedure TPkgJsonMapper.Debug(ALines: TStrings);
var
  LClass: TStubClass;
  LField: TStubField;
begin
  ALines.Clear;

  for LClass in FClasses do
  begin
    ALines.Add('-------');
    ALines.Add(LClass.Name);
    for LField in LClass.FItems do
    begin
      ALines.Add(format('%-15s | %s', [LField.FieldName, LField.GetTypeAsString]));
    end;
  end;
end;

destructor TPkgJsonMapper.Destroy;
begin
  ClearClasses;
  FreeAndNil(FClasses);
  inherited;
end;

procedure TPkgJsonMapper.FormatFields(ATreeView: TTreeView);
begin
  if ATreeView.Count = 1 then
  begin
    InternalFormatTreeViewFields(ATreeView.Items[0]);
  end;
end;

procedure TPkgJsonMapper.SetUnitName(const Value: string);
begin
  FUnitName := Value;
end;

function TPkgJsonMapper.SuggestClassName(ASuggestedClassName: string): string;
var
  LClass: TStubClass;
  LMax, LVal: integer;
  LString: string;
begin
  result := ASuggestedClassName;
  LMax := 0;
  for LClass in FClasses do
  begin
    if LClass.Name.StartsWith(ASuggestedClassName, true) then
    begin
      LString := Copy(LClass.Name, length(ASuggestedClassName) + 2);
      if (LString.Length = 3) then
      begin
        if TryStrToInt(LString, LVal) then
        begin
          inc(LVal);
          if LVal > LMax then
            LMax := LVal;
        end;
      end
      else
        LMax := 1;
    end;
  end;

  if LMax > 0 then
    result := format('%s_%0.3d', [ASuggestedClassName, LMax]);
end;

function TPkgJsonMapper.GetJsonType(AJsonValue: TJsonValue): TJsonType;
var
  LJsonString: TJSONString;
begin
  if AJsonValue is TJSONObject then
    result := jtObject
  else
    if AJsonValue is TJSONArray then
      result := jtArray
    else
      if (AJsonValue is TJSONNumber) then
        result := jtNumber
      else
        if AJsonValue is TJSONTrue then
          result := jtTrue
        else
          if  AJsonValue is TJSONFalse then
            result := jtFalse
          else
            if AJsonValue is TJSONString then
            begin
              LJsonString := (AJsonValue as TJSONString);
              if TRegEx.IsMatch(LJsonString.Value, '^([0-9]{4})-?(1[0-2]|0[1-9])-?(3[01]|0[1-9]|[12][0-9])(T| )(2[0-3]|[01][0-9]):?([0-5][0-9]):?([0-5][0-9])$') then
                result := jtDateTime
              else
                if TRegEx.IsMatch(LJsonString.Value, '^([0-9]{4})(-?)(1[0-2]|0[1-9])\2(3[01]|0[1-9]|[12][0-9])$') then
                  result := jtDate
                else
                  result := jtString
            end
            else
              result := jtUnknown;
end;

procedure TPkgJsonMapper.InternalFormatTreeViewFields(AItem: TTreeViewItem);
var
  LItem: TTreeViewItem;
  k: Integer;
  LSize, LPos: integer;
begin
  LSize := 0;

  //  Find max len
  for k := 0 to AItem.Count - 1 do
  begin
    LItem := AItem.Items[k];
    LPos := Pos(':', LItem.Text);
    if (LPos > 0) AND (LPos > LSize) then
      LSize := LPos;
  end;

  for k := 0 to AItem.Count - 1 do
  begin
    LItem := AItem.Items[k];
    LPos := LSize - Pos(':', LItem.Text);
    if (LPos > 0) then
      LItem.Text := LItem.Text.Replace(':', StringOfChar(' ', LPos) + ':');

    InternalFormatTreeViewFields(LItem);
  end;
  
end;

procedure TPkgJsonMapper.InternalVisualize(ATreeViewItem: TTreeViewItem;
  AClass: TStubClass; AItemStyleLookup: string);
var
  LField: TStubField;
  LItem: TTreeViewItem;
begin
  for LField in AClass.FItems do
  begin

    LItem := TTreeViewItem.Create(ATreeViewItem);
    LItem.StyleLookup := AItemStyleLookup;
    LItem.TagObject := LField;
    LItem.WordWrap := false;

    case LField.FieldType of
      jtObject:
      begin
        LItem.Text := LField.Name + ': {} ' + LField.GetTypeAsString;
        InternalVisualize(LItem, (LField as TStubObjectField).FieldClass, AItemStyleLookup);
      end;

      jtArray:
      begin
        LItem.Text := LField.Name + ': [] ' + LField.GetTypeAsString;
        if (LField as TStubArrayField).ContainedType = jtObject then
        begin
          InternalVisualize(LItem, (LField as TStubArrayField).FieldClass, AItemStyleLookup);
        end;
      end;

      else
      begin
        LItem.Text := LField.Name + ': ' + LField.GetTypeAsString;
      end;
    end;

    ATreeViewItem.AddObject(LItem);

  end;
end;

procedure TPkgJsonMapper.Parse(AJsonString: string; ARootClassName: string);
var
  LJsonValue,
  LJsonValue2: TJSONValue;
  LJsonType: TJsonType;
  LClass: TStubClass;
begin

  ClearClasses;

  LJsonValue := TJSONObject.ParseJSONValue(AJsonString);
  if LJsonValue <> nil then
  begin
    try
      FRootClass := TStubClass.Create(nil, ARootClassName, self);

      case GetJsonType(LJsonValue) of
        jtObject:
        begin
          ProcessJsonObject(LJsonValue, FRootClass);
        end;

        jtArray:
        begin
          LJsonType := jtUnknown;
          LClass := nil;

          LJsonValue2 := GetFirstArrayItem(LJsonValue);
          if LJsonValue2 <> nil then
          begin
            LJsonType := GetJsonType(LJsonValue2);
            LClass := TStubClass.Create(FRootClass, 'Item', self);
          end;
          if LJsonType<>jtUnknown then
          begin
            TStubArrayField.Create(FRootClass, 'Items', LJsonType, LClass);
            ProcessJsonObject(LJsonValue2, LClass);
          end;
        end;
      end;
    finally
      LJsonValue.Free;
    end;
  end
  else
    raise EJsonMapper.Create('Unable to parse the JSON String!');

  FTreeView.ExpandAll;
end;

{ TVirtualClass }

constructor TStubClass.Create(AParentClass: TStubClass; AClassName: string; AMapper: TPkgJsonMapper);
begin
  inherited Create;
  FMapper := AMapper;
  Name := AClassName;

  FItems := TList<TStubField>.Create;
  FComplexItems := TList<TStubField>.Create;
  FArrayItems := TList<TStubField>.Create;
  FMapper.FClasses.Add(self);

  FParentClass := AParentClass;

  FComparison :=
    function(const Left, Right: TStubField): Integer
    begin
      if Left.FName > Right.FName then
        result := 1
      else
        if Left.FName < Right.FName then
          result := -1
        else
          result := 0;
    end;  

  FComparer := TComparer<TStubField>.Construct(FComparison);
  
end;

destructor TStubClass.Destroy;
var
  LItem: TStubField;
begin

  //  ToArray is needed because stub field remove themselves from FItems
  for LItem in FItems.ToArray do
  begin
    LItem.Free;
  end;

  FreeAndNil(FComplexItems);
  FreeAndNil(FItems);
  FreeAndNil(FArrayItems);
  inherited;
end;

function TStubClass.GetImplementationPart: string;
var
  LLines: TStringList;
  LString: string;
  LClassName: string;
  LItem: TStubField;
begin
  LLines := TStringList.Create;
  try
    LClassName := format('%s', [FName]);
    LLines.Add('');
    LLines.Add(format('{%s}', [LClassName]));
    LLines.Add('');
    if FComplexItems.Count > 0 then
    begin

      LLines.Add(format('constructor %s.Create;', [LClassName]));
      LLines.Add('begin');
      LLines.Add('  inherited;');

      for LItem in FComplexItems do
      begin
        LString := format('  %s := %s.Create();', [LItem.FieldName, (LItem).GetTypeAsString]);
        LLines.Add(LString);
      end;

      LLines.Add('end;');
      LLines.Add('');
    end;

    if (FComplexItems.Count > 0) OR (FArrayItems.Count > 0) then
    begin

      LLines.Add(format('destructor %s.Destroy;', [LClassName]));

      if FArrayItems.Count > 0 then
      begin
        LLines.Add('var');
        for LItem in FArrayItems do
        begin
          LString := format('  L%sItem: %s;', [LItem.FName, (LItem as TStubContainerField).FFieldClass.Name]);
          LLines.Add(LString);
        end;
      end;


      LLines.Add('begin');

      if FArrayItems.Count > 0 then
      begin
        LLines.Add('');
        for LItem in FArrayItems do
        begin
          LLines.Add(format(' for L%sItem in %s do', [LItem.FName, LItem.FieldName]));
          LLines.Add(format('   L%sItem.free;', [LItem.FName]));
        end;
        LLines.Add('');
      end;


      for LItem in FComplexItems do
      begin
        LString := format('  %s.free;', [LItem.FieldName]);
        LLines.Add(LString);
      end;

      LLines.Add('  inherited;');
      LLines.Add('end;')
    end;

    LLines.Add('');
    LLines.Add(format('function %s.ToJsonString: string;', [LClassName]));
    LLines.Add('begin');
    LLines.Add('  result := TJson.ObjectToJsonString(self);');
    LLines.Add('end;');

    LLines.Add('');
    LLines.Add(format('class function %s.FromJsonString(AJsonString: string): %s;', [LClassName, LClassName]));
    LLines.Add('begin');
    LLines.Add(format('  result := TJson.JsonToObject<%s>(AJsonString)', [LClassName]));
    LLines.Add('end;');

    result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

procedure TStubClass.SetName(const Value: string);
var
  LName: string;
begin
  FPureClassName := String(Copy(Value, 1, 1)).ToUpper + Copy(Value, 2);

  LName := 'T' + FPureClassName + 'Class';

  FName := FMapper.SuggestClassName(LName);
end;

procedure TStubClass.SetPureClassName(const Value: string);
begin
  FPureClassName := Value;
end;

procedure TStubClass.SortFields;
begin
  FItems.Sort(FComparer);
end;

function TStubClass.GetDeclarationPart: string;
var
  LLines: TStringList;
  LString: string;
  LItem: TStubField;
begin
  LLines := TStringList.Create;
  try
    LLines.Add('');
    LLines.Add(FName + ' = class');
    LLines.Add('private');

    for LItem in FItems do
    begin
      LString := format('  %s: %s;', [LItem.FieldName, LItem.GetTypeAsString]);
      LLines.Add(LString);
    end;

    LLines.Add('public');

    for LItem in FItems do
    begin
      if (LItem.FieldType = jtUnknown) OR ((LItem is TStubContainerField) AND ((LItem as TStubContainerField).ContainedType = jtUnknown)) then
        raise EJsonMapper.CreateFmt('The property [%s] has unknown type!', [LItem.PropertyName]);

      LString := format('  property %s: %s read %s write %s;', [LItem.PropertyName, LItem.GetTypeAsString, LItem.FieldName, LItem.FieldName]);
      LLines.Add(LString);
    end;

    if FComplexItems.Count > 0 then
    begin
      LLines.Add('  constructor Create;');
    end;

    if (FComplexItems.Count > 0) OR (FArrayItems.Count > 0) then
    begin
      LLines.Add('  destructor Destroy; override;');
    end;

    LLines.Add('  function ToJsonString: string;');
    LLines.Add(format('  class function FromJsonString(AJsonString: string): %s;', [FName]));

    LLines.Add('end;');

    result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

{ TVirtualClassItemBase }

constructor TStubField.Create(AParentClass: TStubClass; AItemName: string; AFieldType: TJsonType);
begin
  inherited Create;

  if AItemName.Contains('-') then
    raise EJsonMapper.CreateFmt('%s: Hyphens are not allowed!', [AItemName]);

  FFieldType := AFieldType;
  Name := AItemName;

  FParentClass := AParentClass;
  if FParentClass <> nil then
    FParentClass.FItems.Add(self);
end;

destructor TStubField.Destroy;
begin
  if FParentClass <> nil then
    FParentClass.FItems.Remove(self);
  inherited;
end;

class function TStubField.GetTypeAsString(AType: TJsonType): string;
begin
  case AType of
    jtUnknown: result := 'Unknown';
    jtString: result := 'String';
    jtTrue,
    jtFalse: result := 'Boolean';
    jtNumber: result := 'Extended';
    jtDate: result := 'TDate';
    jtDateTime: result := 'TDateTime';
    jtBytes: result := 'Byte';
  end;
end;

procedure TStubField.SetName(const Value: string);
begin
  FName := Value;

  FFieldName := 'F' + String(Copy(Value, 1, 1)).ToUpper + Copy(Value, 2);

  if ReservedWords.Contains(Value.ToLower) then
    FPropertyName := '&' + Value
  else
    FPropertyName := Value;

end;

function TStubField.GetTypeAsString: string;
begin
  result := GetTypeAsString(FFieldType);
end;

{ TArrayItem }

constructor TStubArrayField.Create(AClass: TStubClass; AItemName: string; AItemSubType: TJsonType; AItemClass: TStubClass);
begin
  inherited Create(AClass, AItemName, jtArray);
  FContainedType := AItemSubType;
  FFieldClass := AItemClass;
  if FContainedType = TJsonType.jtObject then
    AClass.FArrayItems.Add(self);
end;

function TStubArrayField.GetTypeAsString: string;
var
  LSubType: string;
begin
  case FContainedType of
    jtObject: LSubType := FFieldClass.Name;
    jtArray: raise EJsonMapper.Create('Nested arrays are not supported!');
    else
      LSubType := GetTypeAsString(FContainedType);
  end;
  result := format('TArray<%s>', [LSubType]);
end;

{ TStubObjectField }

constructor TStubObjectField.Create(AParentClass: TStubClass; AItemName: string; AItemClass: TStubClass);
begin
  inherited Create(AParentClass, AItemName, jtObject);
  FFieldClass := AItemClass;
  AParentClass.FComplexItems.Add(self);
  FContainedType := jtObject;
end;

function TStubObjectField.GetTypeAsString: string;
begin
  result := FFieldClass.Name;
end;

initialization

  PointDsFormatSettings := TFormatSettings.Create();
  PointDsFormatSettings.DecimalSeparator := '.';

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
