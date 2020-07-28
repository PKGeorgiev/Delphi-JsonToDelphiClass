unit Test.Helper.Helper;

interface
uses
  Pkg.Json.DTO;

const
   DemodataDir = '..\..\..\Demo Data\';

type
  TDemoData = (AnonymousArray, ArrayTest1, ArrayTest2, ArrayTest3,  EmptyAnonymousArray, MailChimp, NonObjectArray, StringTest, UnicodeTest,  WebAPP);
const
  sDemoData : array[TDemoData] of string =
  ('Anonymous Array', 'ArrayTest1', 'ArrayTest2', 'ArrayTest3',  'Empty Anonymous Array', 'MailChimp', 'NonObjectArray', 'StringTest', 'UnicodeTest',  'WebAPP');
type

  TJsonDTOHelper = class helper for TJsonDTO
  public
    procedure LoadFromfile(aDemoData : TDemoData);
  end;

  TTestHelper = class

  end;
implementation

uses
  System.Classes, System.IOUtils;
{ TJsonDTOHelper }

procedure TJsonDTOHelper.LoadFromfile(aDemoData: TDemoData);
var
  JsonFile : string;
begin
  JsonFile := DemodataDir + sDemoData[aDemoData] + '.json';
  with TStringlist.Create do
  try
    LoadFromFile(JsonFile);
    AsJson := Text;
  finally
    free;
  end;
end;

end.
