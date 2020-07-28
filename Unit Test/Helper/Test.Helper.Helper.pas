unit Test.Helper.Helper;

interface

uses
  Pkg.Json.DTO;

const
  DemodataDir = '..\..\..\Demo Data\';

type
  TDemoData = (AnonymousArray, ArrayTest1, ArrayTest2, ArrayTest3, EmptyAnonymousArray, MailChimp, NonObjectArray, StringTest, UnicodeTest, WebAPP);

const
  sDemoData: array [TDemoData] of string = ('Anonymous Array', 'ArrayTest1', 'ArrayTest2', 'ArrayTest3', 'Empty Anonymous Array', 'MailChimp', 'NonObjectArray', 'StringTest', 'UnicodeTest', 'WebAPP');

type

  TJsonDTOHelper = class helper for TJsonDTO
  public
    procedure LoadFromfile(aDemoData: TDemoData);
  end;

  TTestHelper = class
  public
    class function PropertyCount(aObject: TObject): Integer;
    class function GetProperties(aObject: TObject): TArray<string>;
  end;

implementation

uses
  System.Classes, System.IOUtils, System.RTTI;
{ TJsonDTOHelper }

procedure TJsonDTOHelper.LoadFromfile(aDemoData: TDemoData);
var
  JsonFile: string;
begin
  JsonFile := DemodataDir + sDemoData[aDemoData] + '.json';
  with TStringlist.Create do
    try
      LoadFromfile(JsonFile);
      AsJson := Text;
    finally
      free;
    end;
end;

{ TTestHelper }

class function TTestHelper.GetProperties(aObject: TObject): TArray<string>;
var
  Ctx: TRttiContext;
  Properties: TArray<TRttiProperty>;
  i: Integer;
  T: TRttiType;
  Prop: TRttiProperty;
begin
  if aObject = nil then
    exit(nil);

  Properties := Ctx.GetType(aObject.ClassInfo).GetProperties;
  SetLength(Result, length(Properties));
  for i := 0 to length(Properties) - 1 do
  begin
    Prop := Properties[i];
    Result[i] := Prop.Name;
  end;

end;

class function TTestHelper.PropertyCount(aObject: TObject): Integer;
begin
  if aObject = nil then
    exit(-1);
  Result := length(GetProperties(aObject));
end;

end.
