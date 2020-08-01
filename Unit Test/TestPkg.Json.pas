unit TestPkg.Json;

interface

uses
  System.Sysutils,
  DUnitX.TestFramework, DUnitX.Assert.Ex,

  Pkg.Json.JSONName;

type

// Test methods for class TJSONName
  [TestFixture]
  TestTJSONName = class
  strict private
    FJsonName: TJSONName;
    function JSONName(const Value: string): TJSONName;
  public
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestEmptyName;
    [Test]
    procedure TestCapitalizeFirst_teststring;
    [Test]
    procedure TestCapitalizeFirst_NameTest;
    [Test]
    procedure TestCapitalizeFirst_TestTest;
    [Test]
    procedure TestCapitalizeFirst_SpaceTest;
    [Test]
    procedure TestCapitalizeFirst_UnderscoreTest;
    [Test]
    procedure TestNameProperties;
  end;

implementation

function TestTJSONName.JSONName(const Value: string): TJSONName;
begin
  if (FJsonName = nil) or (FJsonName.Name <> Value) then
  begin
    FreeAndNil(FJsonName);
    FJsonName := TJSONName.Create(Value);
  end;

  Result := FJsonName;
end;

procedure TestTJSONName.TearDown;
begin
  FreeAndNil(FJsonName);
end;

procedure TestTJSONName.TestCapitalizeFirst_NameTest;
const
  TestResult = 'FirstName';
begin
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('firstname'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('firstName'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('FirstnaMe'));
end;

procedure TestTJSONName.TestCapitalizeFirst_SpaceTest;
const
  TestResult = '"Test string"';
begin
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('Test String'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('Test StriNg'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('test string'));
end;

procedure TestTJSONName.TestCapitalizeFirst_teststring;
const
  TestResult = 'Teststring';
begin
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('teststring'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('teststrinG'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('TestStrinG'));
end;

procedure TestTJSONName.TestCapitalizeFirst_TestTest;
const
  TestResult = 'MyTest';
begin
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('Mytest'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('myteSt'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('MyTest'));
end;

procedure TestTJSONName.TestCapitalizeFirst_UnderscoreTest;
const
  TestResult = 'Test_String';
begin
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('Test_string'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('Test_striNg'));
  Assert.AreEqual(TestResult, TJSONName.CapitalizeFirst('test_sTring'));
end;

procedure TestTJSONName.TestEmptyName;
const
  TestValue = '';
begin
  Assert.WillRaise(
      procedure
    begin
      JSONName(TestValue)
    end, Exception);
end;

procedure TestTJSONName.TestNameProperties;
const
  TestValue = 'Test';
begin
  JSONName(TestValue);
  Assert.AreEqual(TestValue, FJsonName.JSONName);
  Assert.AreEqual(TestValue, FJsonName.DelphiName);
end;

{ AssertHelper }

initialization

TDUnitX.RegisterTestFixture(TestTJSONName);

end.
