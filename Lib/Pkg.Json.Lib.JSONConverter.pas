unit Pkg.Json.Lib.JSONConverter;

interface

uses
  System.SysUtils, System.Json.Readers, System.Json.Writers, System.Classes;

{$M+}

type
  TJSONConverter = class
  private
    FJSON: string;
    function GetBson: string;
    procedure SetBson(const Value: string);

    // Byte <-> String convert utils
    class function Bytes2String(const ABytes: TBytes): string;
    class function String2Bytes(const AString: string): TBytes;

  published
    property Json: string read FJSON write FJSON;
    property Bson: string read GetBson write SetBson;
  public type
    TConverterOption = (ExtendedMode, Indented);
    TConverterOptions = set of TConverterOption;
  public
    constructor Create(aJSON: string); reintroduce;

    // Json <-> Bson converter
    class function Bson2Json(const ABytes: TBytes; AOptions: TConverterOptions = []): string;
    class function Json2Bson(const aJSON: string; AOptions: TConverterOptions = []): TBytes;
    class function BsonString2Json(const ABsonString: string; AOptions: TConverterOptions = []): string;
    class function Json2BsonString(const aJSON: string; AOptions: TConverterOptions = []): string;

    // Format Json
    class function JsonReformat(const aJSON: string; Indented: Boolean): string;
    class function MinifyJson(const aJSON: string): string;
    class function PrettyPrintJson(const aJSON: string): string;

    function Minify: string;
    function PrettyPrint: string;
  end;

  TJsonStringWriter = class(TJsonTextWriter)
  private
    FStrinBuilder: TStringBuilder;
    FStringWriter: TStringWriter;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
  end;

  TJsonStringReader = class(TJsonTextReader)
  private
    FStrinReader: TStringReader;
  public
    constructor Create(const aJSON: string);
    destructor Destroy; override;
  end;

implementation

uses
  System.Json.Bson, System.Json.Types;

{ TJsonStringWriter }

constructor TJsonStringWriter.Create;
begin
  FStrinBuilder := TStringBuilder.Create;
  FStringWriter := TStringWriter.Create(FStrinBuilder);
  inherited Create(FStringWriter);
end;

destructor TJsonStringWriter.Destroy;
begin
  FStringWriter.Free;
  FStrinBuilder.Free;
  inherited Destroy;
end;

function TJsonStringWriter.ToString: string;
begin
  Result := FStrinBuilder.ToString;
end;

{ TJsonStringReader }

constructor TJsonStringReader.Create(const aJSON: string);
begin
  FStrinReader := TStringReader.Create(aJSON);
  inherited Create(FStrinReader);
end;

destructor TJsonStringReader.Destroy;
begin
  FStrinReader.Free;
  inherited Destroy;
end;

{ TJSONConverter }

class function TJSONConverter.Bson2Json(const ABytes: TBytes; AOptions: TConverterOptions): string;
var
  JsonWriter: TJsonStringWriter;
  BsonReader: TBsonReader;
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create(ABytes);
  BsonReader := TBsonReader.Create(Stream);
  JsonWriter := TJsonStringWriter.Create;

  if Indented in AOptions then
    JsonWriter.Formatting := TJsonFormatting.Indented;

  try
    JsonWriter.WriteToken(BsonReader);
    Result := JsonWriter.ToString;
  finally
    JsonWriter.Free;
    BsonReader.Free;
    Stream.Free;
  end;
end;

class function TJSONConverter.BsonString2Json(const ABsonString: string; AOptions: TConverterOptions): string;
begin
  Result := Bson2Json(String2Bytes(ABsonString), AOptions)
end;

class function TJSONConverter.Bytes2String(const ABytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(ABytes) to High(ABytes) do
    if I = 0 then
      Result := IntToHex(ABytes[I], 2)
    else
      Result := Result + '-' + IntToHex(ABytes[I], 2);
end;

constructor TJSONConverter.Create(aJSON: string);
begin
  inherited Create;
  FJSON := aJSON;
end;

function TJSONConverter.GetBson: string;
begin
  Result := Json2BsonString(FJSON);
end;

class function TJSONConverter.Json2Bson(const aJSON: string; AOptions: TConverterOptions): TBytes;
var
  JsonReader: TJsonStringReader;
  BsonWriter: TBsonWriter;
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create;
  BsonWriter := TBsonWriter.Create(Stream);
  JsonReader := TJsonStringReader.Create(aJSON);
  try
    BsonWriter.WriteToken(JsonReader);
    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.Read(Result, Stream.Size);
  finally
    JsonReader.Free;
    BsonWriter.Free;
    Stream.Free;
  end;
end;

class function TJSONConverter.Json2BsonString(const aJSON: string; AOptions: TConverterOptions): string;
begin
  Result := Bytes2String(Json2Bson(aJSON, AOptions));
end;

class function TJSONConverter.JsonReformat(const aJSON: string; Indented: Boolean): string;
var
  JsonWriter: TJsonStringWriter;
  JsonReader: TJsonStringReader;
begin
  JsonReader := TJsonStringReader.Create(aJSON);
  JsonWriter := TJsonStringWriter.Create;

  if Indented then
    JsonWriter.Formatting := TJsonFormatting.Indented;

  try
    JsonWriter.WriteToken(JsonReader);
    Result := JsonWriter.ToString;
  finally
    JsonWriter.Free;
    JsonReader.Free;
  end;
end;

function TJSONConverter.Minify: string;
begin
  Result := MinifyJson(FJSON);
end;

class function TJSONConverter.MinifyJson(const aJSON: string): string;
begin
  Result := JsonReformat(aJSON, false);
end;

function TJSONConverter.PrettyPrint: string;
begin
  Result := PrettyPrintJson(FJSON);
end;

class function TJSONConverter.PrettyPrintJson(const aJSON: string): string;
begin
  Result := JsonReformat(aJSON, True);
end;

procedure TJSONConverter.SetBson(const Value: string);
begin
  FJSON := BsonString2Json(Value);
end;

class function TJSONConverter.String2Bytes(const AString: string): TBytes;
var
  CleanStr: string;
begin
  CleanStr := AString.Replace('-', '');
  SetLength(Result, Round(Length(CleanStr) / 2));
  HexToBin(PChar(CleanStr), 0, Result, 0, Length(Result));
end;

end.
