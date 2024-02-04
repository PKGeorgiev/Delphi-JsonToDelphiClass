unit Json.Interceptors;

interface

uses
  REST.JsonReflect, System.Rtti, System.Types, System.UITypes;

{$M+}


type
  TArrayHelper = record helper for TStringDynArray
  public
    function IndexOf(aString: string): Integer;
  end;

  EnumMemberAttribute = class(TCustomAttribute)
  strict private
    FNames: TStringDynArray;
  public
    constructor Create(const aNames: string; aSeparator: Char = ','); overload;
  published
    property Names: TStringDynArray read FNames;
  end;

  SetMembersAttribute = class(TCustomAttribute)
  strict private
    FName: string;
    FSeparator: Char;
  public
    constructor Create(const aName: string; const aSeparator: Char = ','); overload;
    function ToString: string; override;
    property &Name: string read FName;
    property Separator: Char read FSeparator;
  end;

  TBaseInterceptor<T> = class abstract(TJSONInterceptor)
  strict private
    FRttiContext: TRttiContext;
  strict protected
    function GetDataField(Data: TObject; Field: string): TRttiField; {$IFNDEF DEBUG} inline; {$ENDIF}
    function GetValue(Data: TObject; Field: string): TValue; {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure SetValue(Data: TObject; Field: string; const AValue: TValue); {$IFNDEF DEBUG} inline; {$ENDIF}
    function SplitString(const S, Delimiters: string): TStringDynArray;
    function GetAttributes<U: TCustomAttribute>: TArray<U>; {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure ExpectTypeKind(ATypeKind: TTypeKind); {$IFNDEF DEBUG} inline; {$ENDIF}
  end;

  TColorInterceptor = class abstract(TBaseInterceptor<TColor>)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
    class function TryStrToColor(const AValue: string; out aColor: TColor): Boolean;
    class function StrToColor(const AValue: string): TColor;
  end;

  TEnumInterceptor<T: record > = class abstract(TBaseInterceptor<T>)
  strict private
    function GetNames: TStringDynArray; {$IFNDEF DEBUG} inline; {$ENDIF}
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TSetInterceptor<T> = class abstract(TBaseInterceptor<T>)
  strict private
    function GetSetStrings: TStringDynArray;
  public
    function StringsConverter(Data: TObject; Field: string): TListOfStrings; override;
    procedure StringsReverter(Data: TObject; Field: string; Args: TListOfStrings); override;
  end;

  TJSONInterceptorClass = class of TJSONInterceptor;

  StringHandlerAttribute = class(JsonReflectAttribute)
  public
    constructor Create(aClass: TJSONInterceptorClass);
  end;

  StringsHandler = class(JsonReflectAttribute)
  public
    constructor Create(aClass: TJSONInterceptorClass);
  end;

implementation

uses
  System.TypInfo, System.SysUtils, System.StrUtils,

  System.Generics.Collections, System.UIConsts;

function TArrayHelper.IndexOf(aString: string): Integer;
begin
  for Result := low(Self) to high(Self) do
    if Self[Result] = aString then
      Exit;
  Result := -1;
end;

{ TBaseEnumInterceptor<T> }

procedure TBaseInterceptor<T>.ExpectTypeKind(ATypeKind: TTypeKind);
begin
  Assert(TypeInfo(T) <> nil, 'Type has no typeinfo!');
  Assert(PTypeInfo(TypeInfo(T)).Kind = ATypeKind, 'Type is not expected type!');
end;

function TBaseInterceptor<T>.GetAttributes<U>: TArray<U>;
var
  Attribute: TCustomAttribute;
  Attributes: TArray<TCustomAttribute>;
  ResultArray: TArray<U>;
begin
  ResultArray := nil;
  for Attribute in FRttiContext.GetType(ClassType).GetAttributes do
    if Attribute is U then
    begin
      SetLength(ResultArray, Length(ResultArray) + 1);
      ResultArray[Length(ResultArray) - 1] := Attribute as U;
    end;
  Result := ResultArray;
end;

function TBaseInterceptor<T>.GetDataField(Data: TObject; Field: string): TRttiField;
begin
  Result := FRttiContext.GetType(Data.ClassType).GetField(Field);
end;

function TBaseInterceptor<T>.GetValue(Data: TObject; Field: string): TValue;
begin
  Result := GetDataField(Data, Field).GetValue(Data);
end;

procedure TBaseInterceptor<T>.SetValue(Data: TObject; Field: string; const AValue: TValue);
begin
  GetDataField(Data, Field).SetValue(Data, AValue);
end;

function TBaseInterceptor<T>.SplitString(const S, Delimiters: string): TStringDynArray;
var
  Buffer: TStringDynArray;
  I: Integer;
begin
  Buffer := System.StrUtils.SplitString(S, Delimiters);
  SetLength(Result, Length(Buffer));
  for I := low(Result) to high(Result) do
    Result[I] := Buffer[I].Trim;
end;

{ EnumMemberAttribute }

constructor EnumMemberAttribute.Create(const aNames: string; aSeparator: Char);
begin
  inherited Create;
  FNames := SplitString(aNames, aSeparator);
end;

{ TSmartInterceptor<T> }

function TEnumInterceptor<T>.GetNames: TStringDynArray;
var
  Attribute: EnumMemberAttribute;
  Name: string;
  ResultArray: TStringDynArray;
begin
  for Attribute in GetAttributes<EnumMemberAttribute> do
    for name in Attribute.Names do
    begin
      SetLength(ResultArray, Length(ResultArray) + 1);
      ResultArray[Length(ResultArray) - 1] := name.Trim;
    end;

  Result := ResultArray;
end;

function TEnumInterceptor<T>.StringConverter(Data: TObject; Field: string): string;
begin
  ExpectTypeKind(tkEnumeration);
  Result := GetNames[GetValue(Data, Field).AsOrdinal];
end;

procedure TEnumInterceptor<T>.StringReverter(Data: TObject; Field, Arg: string);
var
  Enum: T;
  AbsValue: Byte absolute Enum;
begin
  ExpectTypeKind(tkEnumeration);
  AbsValue := GetNames.IndexOf(Arg);
  SetValue(Data, Field, TValue.From<T>(Enum));
end;

{ EnumHandlerAttribute }

constructor StringHandlerAttribute.Create(aClass: TJSONInterceptorClass);
begin
  inherited Create(ctString, rtString, aClass, nil, True);
end;

{ TSmartSetInterceptor<T> }

function TSetInterceptor<T>.GetSetStrings: TStringDynArray;
var
  AttributeArray: TArray<SetMembersAttribute>;
  Attr: TCustomAttribute;
  SetAttr: SetMembersAttribute absolute Attr;
begin
  Result := nil;

  AttributeArray := GetAttributes<SetMembersAttribute>;
  if AttributeArray = nil then
    Exit(nil);

  Attr := AttributeArray[0];
  Result := SplitString(SetAttr.Name, SetAttr.Separator);
end;

function TSetInterceptor<T>.StringsConverter(Data: TObject; Field: string): TListOfStrings;
var
  SetStrings: TStringDynArray;
  RttiField: TRttiField;
  EnumType: TRttiEnumerationType;
  SetValues: TIntegerSet;
  EnumSet: T absolute SetValues;
  SetType: TRttiSetType;
  I: Integer;
begin
  Result := nil;
  ExpectTypeKind(tkSet);

  SetStrings := GetSetStrings;
  Assert(Length(SetStrings) > 0, Format('EnumSet attribute needs string parameters on %s!', [ClassName]));

  RttiField := GetDataField(Data, Field);
  if not(RttiField.FieldType is TRttiSetType) then
    Exit;

  SetType := TRttiSetType(RttiField.FieldType);

  if not(SetType.ElementType is TRttiEnumerationType) then
    Exit;

  EnumSet := RttiField.GetValue(Data).AsType<T>;
  EnumType := TRttiEnumerationType(SetType.ElementType);

  (*
    Common error scenarios
    * Your Enum contains 3 elements, but you only provided 2 names
    * You separated the strings with a pipe (|), but the separator is comma (,)
  *)
  Assert(EnumType.MaxValue <= high(SetStrings), 'Number of elements in set does not match, or separator mismatch!');

  for I := EnumType.MinValue to EnumType.MaxValue do
    if I in SetValues then
      Result := Result + [SetStrings[I]];
end;

procedure TSetInterceptor<T>.StringsReverter(Data: TObject; Field: string; Args: TListOfStrings);
var
  I: Integer;
  RttiField: TRttiField;
  SetValues: TIntegerSet;
  EnumSet: T absolute SetValues;
  Names: TStringDynArray;
  Argument: string;
begin
  ExpectTypeKind(tkSet);

  Names := GetSetStrings;
  SetValues := [];

  for I := low(Names) to high(Names) do
    for Argument in TArray<string>(Args) do
      if Names[I] = Argument then
        Include(SetValues, I);

  SetValue(Data, Field, TValue.From<T>(EnumSet));
end;

{ SetMemberAttribute }

constructor SetMembersAttribute.Create(const aName: string; const aSeparator: Char);
begin
  FName := aName;
  FSeparator := aSeparator;
end;

function SetMembersAttribute.ToString: string;
begin
  Result := FName;
end;

{ StringsHandler }

constructor StringsHandler.Create(aClass: TJSONInterceptorClass);
begin
  inherited Create(ctStrings, rtStrings, aClass, nil, True)
end;

{ TColorInterceptor<T> }

function TColorInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  Color: TColor;
begin
  Color := GetValue(Data, Field).AsInteger;
  Result := '0x' + IntToHex(Color);
end;

procedure TColorInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  SetValue(Data, Field, StrToColor(Arg));
end;

class function TColorInterceptor.StrToColor(const AValue: string): TColor;
begin
  if not TryStrToColor(AValue, Result) then
    Result := 0;
end;

class function TColorInterceptor.TryStrToColor(const AValue: string; out aColor: TColor): Boolean;
var
  Int: Integer;
begin
  Result := False;

  if TryStrToInt(AValue, Int) or IdentToColor(AValue, Int) or IdentToColor('cl' + AValue, Int) then
  begin
    aColor := Int;
    Result := True;
  end;
end;

end.
