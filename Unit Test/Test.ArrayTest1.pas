unit Test.ArrayTest1;

interface

uses
  DUnitX.TestFramework, DUnitX.Assert.Ex,
  Test.Helper.Helper, Test.TestBase, Test.ArrayTestBase, ArrayTest1;
{$SCOPEDENUMS ON}

type

  [TestFixture]
  TAnonymousArrayTest = class(TArrayTestBase<TArrayTest1DTO, TArrayTestDTO>)
  strict protected
    function DemoData: TDemoData; override;
  public
    [Test]
    procedure ItemsAssigned;

    [Test]
    procedure PropertyCheck;

    [Test]
    procedure ElementPropertyCheck;

    [Test]
    procedure ItemsElements;
  end;

implementation

{ TAnonymousArrayTest }

function TAnonymousArrayTest.DemoData: TDemoData;
begin
  Result := TDemoData.ArrayTest1;
end;

procedure TAnonymousArrayTest.ElementPropertyCheck;
begin
  TestElementPropertyCheck(DTO.ArrayTest.First);
end;

procedure TAnonymousArrayTest.ItemsAssigned;
begin
  TestItemsAssigned(DTO.ArrayTest);
end;

procedure TAnonymousArrayTest.ItemsElements;
var
  Element: TArrayTestDTO;
begin
  TestItemsElements(DTO.ArrayTest);
  Element := DTO.ArrayTest[0];
  Assert.AreEqual('5102', Element.S1);
  Assert.AreEqual(default (boolean), Element.S2);

  Element := DTO.ArrayTest[1];
  Assert.AreEqual('5102', Element.S1);
  Assert.AreEqual(True, Element.S2);
end;

procedure TAnonymousArrayTest.PropertyCheck;
var
  Properties: TArray<string>;
begin
  Properties := TTestHelper.GetProperties(DTO);
  Assert.AreEqual(2, Length(Properties));
  Assert.AreEqual('ArrayTest', Properties[0]);
  Assert.AreEqual('AsJson', Properties[1]);
end;

initialization

TDUnitX.RegisterTestFixture(TAnonymousArrayTest);

end.
