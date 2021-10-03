unit Pkg.Json.DTO;

interface

uses System.Classes, System.Json, Rest.Json, System.Generics.Collections, Rest.JsonReflect;

type
  TArrayMapper = class
  protected
    procedure RefreshArray<T>(aSource: TList<T>; var aDestination: TArray<T>);
    function List<T>(var aList: TList<T>; aSource: TArray<T>): TList<T>;
    function ObjectList<T: class>(var aList: TObjectList<T>; aSource: TArray<T>): TObjectList<T>;
  public
    constructor Create; virtual;
  end;

  TJsonDTO = class(TArrayMapper)
  private
    FOptions: TJsonOptions;
    class procedure PrettyPrintPair(aJSONValue: TJSONPair; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
    class procedure PrettyPrintJSON(aJSONValue: TJsonValue; aOutputStrings: TStrings; Indent: Integer = 0); overload;
    class procedure PrettyPrintArray(aJSONValue: TJSONArray; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
  protected
    function GetAsJson: string; virtual;
    procedure SetAsJson(aValue: string); virtual;
  public
    constructor Create; override;
    class function PrettyPrintJSON(aJson: string): string; overload;
    function ToString: string; override;
    property AsJson: string read GetAsJson write SetAsJson;
  end;

  GenericListReflectAttribute = class(JsonReflectAttribute)
  public
    constructor Create;
  end;

  SuppressZeroAttribute = class(JsonReflectAttribute)
  public
    constructor Create;
  end;

implementation

uses System.Sysutils, System.JSONConsts, System.Rtti, System.DateUtils;

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

procedure TJsonDTO.SetAsJson(aValue: string);
var
  JSONValue: TJsonValue;
  JSONObject: TJSONObject;
begin
  JSONValue := TJSONObject.ParseJSONValue(aValue);
  try
    if not Assigned(JSONValue) then
      Exit;

    if (JSONValue is TJSONArray) then
    begin
      with TJSONUnMarshal.Create do
        try
          SetFieldArray(Self, 'Items', (JSONValue as TJSONArray));
        finally
          Free;
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

function TJsonDTO.ToString: string;
begin
  Result := AsJson;
end;

{ TArrayMapper }

constructor TArrayMapper.Create;
begin
  inherited;
end;

function TArrayMapper.List<T>(var aList: TList<T>; aSource: TArray<T>): TList<T>;
begin
  if aList = nil then
  begin
    aList := TList<T>.Create;
    aList.AddRange(aSource);
  end;

  Exit(aList);
end;

function TArrayMapper.ObjectList<T>(var aList: TObjectList<T>; aSource: TArray<T>): TObjectList<T>;
var
  Element: T;
begin
  if aList = nil then
  begin
    aList := TObjectList<T>.Create;
    for Element in aSource do
      aList.Add(Element);
  end;

  Exit(aList);
end;

procedure TArrayMapper.RefreshArray<T>(aSource: TList<T>; var aDestination: TArray<T>);
begin
  if aSource <> nil then
    aDestination := aSource.ToArray;
end;

type
  TGenericListFieldInterceptor = class(TJSONInterceptor)
  public
    function ObjectsConverter(Data: TObject; Field: string): TListOfObjects; override;
  end;

  { TListFieldInterceptor }

function TGenericListFieldInterceptor.ObjectsConverter(Data: TObject; Field: string): TListOfObjects;
var
  ctx: TRttiContext;
  List: TList<TObject>;
  RttiProperty: TRttiProperty;
begin
  RttiProperty := ctx.GetType(Data.ClassInfo).GetProperty(Copy(Field, 2, MAXINT));
  List := TList<TObject>(RttiProperty.GetValue(Data).AsObject);
  Result := TListOfObjects(List.List);
  SetLength(Result, List.Count);
end;

constructor GenericListReflectAttribute.Create;
begin
  inherited Create(ctObjects, rtObjects, TGenericListFieldInterceptor, nil, false);
end;

type
  TSuppressZeroDateInterceptor = class(TJSONInterceptor)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

function TSuppressZeroDateInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  RttiContext: TRttiContext;
  Date: TDateTime;
begin
  Date := RttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TDateTime>;
  if Date = 0 then
    Result := string.Empty
  else
    Result := DateToISO8601(Date, True);
end;

procedure TSuppressZeroDateInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  RttiContext: TRttiContext;
  Date: TDateTime;
begin
  if Arg.IsEmpty then
    Date := 0
  else
    Date := ISO8601ToDate(Arg, True);

  RttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data, Date);
end;

{ SuppressZeroAttribute }

constructor SuppressZeroAttribute.Create;
begin
  inherited Create(ctString, rtString, TSuppressZeroDateInterceptor);
end;

end.
