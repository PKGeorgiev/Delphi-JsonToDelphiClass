unit TestPkg.TestTJSONName;

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
    [Test]
    procedure TestNameProperties_StartsWithDigtig;
    [Test]
    procedure TestNameProperties_SpaceInName;
    [Test]
    procedure TestNameProperties_ColonInName;
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
  TestResult = 'Test string';
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
  TestResult = 'TestString';
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
  Assert.IsFalse(FJsonName.NeedsAttribute)
end;
procedure TestTJSONName.TestNameProperties_ColonInName;
const
  TestValue = 'configGlossary:adminEmail';
begin
  JSONName(TestValue);
  Assert.AreEqual(TestValue, FJsonName.JSONName);
  Assert.AreEqual('ConfigGlossaryAdminEmail', FJsonName.DelphiName);
  Assert.IsTrue(FJsonName.NeedsAttribute)
end;

procedure TestTJSONName.TestNameProperties_SpaceInName;
const
  TestValue = '1 Test';
begin
  JSONName(TestValue);
  Assert.AreEqual(TestValue, FJsonName.JSONName);
  Assert.AreEqual('_1Test', FJsonName.DelphiName);
  Assert.IsTrue(FJsonName.NeedsAttribute)
end;

procedure TestTJSONName.TestNameProperties_StartsWithDigtig;
const
  TestValue = '1Test';
begin
  JSONName(TestValue);
  Assert.AreEqual(TestValue, FJsonName.JSONName);
  Assert.AreEqual('_' + TestValue, FJsonName.DelphiName);
  Assert.IsTrue(FJsonName.NeedsAttribute)
end;

{ AssertHelper }

initialization

TDUnitX.RegisterTestFixture(TestTJSONName);

end.
