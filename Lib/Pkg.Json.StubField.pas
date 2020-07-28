unit Pkg.Json.StubField;

interface

uses
  System.SysUtils, System.Generics.Defaults, System.Generics.Collections,
  Pkg.Json.JSONName, Pkg.Json.Lists;

type
  EJsonMapper = class(Exception);
  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtTrue, jtFalse, jtNumber, jtDate, jtDateTime, jtBytes, jtInteger, jtInteger64);

  TStubClass = class;
  TStubClassList = class;

  TStubField = class(TJsonName)
  strict private
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

  TStubContainerField = class abstract(TStubField)
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

  TStubFieldList = class(TJsonNameList<TStubField>)
  end;

  TStubFieldObjectList = class(TJsonNameObjectList<TStubField>)
  end;

  TStubClass = class(TJsonName)
  strict private
    FArrayItems: TStubFieldList;
    FComplexItems: TStubFieldList;
    FItems: TStubFieldObjectList;
    FComparison: TComparison<TStubField>;
    FComparer: IComparer<TStubField>;
    FParentClass: TStubClass;
    FStubClasses: TStubClassList;
    FArrayProperty: string;
  strict protected
    constructor Create(aParentClass: TStubClass; aClassName: string; aStubClasses: TStubClassList; aArrayProperty: string = ''); virtual;
  public
    class function Construct(aParentClass: TStubClass; aClassName: string; aStubClasses: TStubClassList; aArrayProperty: string = ''): TStubClass;
    destructor Destroy; override;
    function GetDeclarationPart(const BaseClass: string = ''): string;
    function GetImplementationPart: string;
    procedure SortFields;
  published
    property Items: TStubFieldObjectList read FItems write FItems;
    property ArrayProperty: string read FArrayProperty write FArrayProperty;
    property ComplexItems: TStubFieldList read FComplexItems;
    property ArrayItems: TStubFieldList read FArrayItems;
  end;

  TStubClassList = class(TJsonNameList<TStubClass>)
  end;

implementation

uses
  System.Classes, System.StrUtils,

  Pkg.Json.ReservedWords;

class function TStubClass.Construct(aParentClass: TStubClass; aClassName: string; aStubClasses: TStubClassList; aArrayProperty: string): TStubClass;
var
  StubClass: TJsonName;
begin
  StubClass := aStubClasses.ItemByName(aClassName);
  if StubClass = nil then
    Result := TStubClass.Create(aParentClass, aClassName, aStubClasses, aArrayProperty)
  else
    Result := StubClass as TStubClass;
end;

constructor TStubClass.Create(aParentClass: TStubClass; aClassName: string; aStubClasses: TStubClassList; aArrayProperty: string);
begin
  inherited Create(aClassName);
  FStubClasses := aStubClasses;
  SetName(DelphiName);

  FItems := TStubFieldObjectList.Create;
  FComplexItems := TStubFieldList.Create;
  FArrayItems := TStubFieldList.Create;
  FStubClasses.Add(Self);
  FArrayProperty := aArrayProperty;

  FParentClass := aParentClass;

  FComparison := function(const Left, Right: TStubField): Integer
    begin
      Result := CompareStr(Left.Name, Right.Name);
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
  if FComplexItems.Count + FArrayItems.Count = 0 then
    exit('');

  Lines := TStringList.Create;
  try
    Lines.Add('');
    Lines.AddFormat('{ %s }', [Name]);
    Lines.Add('');

    Lines.AddFormat('constructor %s.Create;', [Name]);
    Lines.Add('begin');
    Lines.Add('  inherited;');

    for StubField in FComplexItems do
      Lines.AddFormat('  %s := %s.Create;', [StubField.FieldName, StubField.TypeAsString]);

    Lines.Add('end;');
    Lines.Add('');

    Lines.Add(Format('destructor %s.Destroy;', [Name]));
    Lines.Add('begin');

    for StubField in FComplexItems do
      Lines.AddFormat('  %s.Free;', [StubField.FieldName]);

    for StubField in FArrayItems do
      Lines.AddFormat('  %s.Free;', [StubField.FieldName]);

    Lines.Add('  inherited;');
    Lines.Add('end;');

    for StubField in FItems do
      if StubField.IsObjectArrayField then
      begin
        StubArrayField := StubField as TStubArrayField;
        Lines.Add('');
        Lines.AddFormat('function %s.Get%s: TObjectList<%s>;', [Name, StubField.PropertyName, StubArrayField.TypeAsString]);
        Lines.Add('begin');
        Lines.AddFormat('  Result := ObjectList<%s>(%s, %sArray);', [StubArrayField.TypeAsString, StubField.FieldName,  StubField.FieldName]);
        Lines.Add('end;');
      end;

    Lines.TrailingLineBreak := False;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TStubClass.SortFields;
var
  StubFieldsNames: TStringList;
  Item: TStubField;
  i: Integer;
begin
  // remove dublicates

  FItems.Sort(FComparer);
  i := 0;
  StubFieldsNames := TStringList.Create;
  while i < Items.Count do
  begin
    Item := FItems[i];

    if (StubFieldsNames.IndexOf(Item.Name) >= 0) then
    begin
      FComplexItems.Remove(Item);
      FArrayItems.Remove(Item);
      FItems.Delete(i);
    end
    else
    begin
      StubFieldsNames.Add(Item.Name);
      inc(i);
    end;
  end;

  StubFieldsNames.Free;
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
        Lines.Add('  [JSONMarshalled(False)]');
        Lines.AddFormat('  %s: TObjectList<%s>;', [StubField.FieldName, StubArrayField.TypeAsString]);
      end
      else
      begin
        if StubField.NeedsAttribute then
          Lines.Add('  ' + StubField.NameAttribute);

        Lines.AddFormat('  %s: %s;', [StubField.FieldName, StubField.TypeAsString]);
      end;
    end;

    for StubField in FItems do
      if StubField.IsObjectArrayField then
      begin
        StubArrayField := StubField as TStubArrayField;
        Lines.AddFormat('  function Get%s: TObjectList<%s>;', [StubField.PropertyName, StubArrayField.TypeAsString]);
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
      else if StubField.FieldType = jtObject then
        Lines.AddFormat('  property %s: %s read %s;', [StubField.PropertyName, StubField.TypeAsString, StubField.FieldName])
      else
        Lines.AddFormat('  property %s: %s read %s write %s;', [StubField.PropertyName, StubField.TypeAsString, StubField.FieldName, StubField.FieldName]);
    end;

    if (FComplexItems.Count > 0) or (FArrayItems.Count > 0) then
    begin
      Lines.Add('public');
      Lines.Add('  constructor Create;' + IfThen(BaseClass = '', '', ' override;'));
      Lines.Add('  destructor Destroy; override;');
    end;

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

end.
