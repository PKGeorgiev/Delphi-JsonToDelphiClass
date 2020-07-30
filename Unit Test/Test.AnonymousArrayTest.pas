unit Test.AnonymousArrayTest;

interface

uses
  DUnitX.TestFramework, DUnitX.Assert.Ex,

  Test.Helper.Helper, AnonymousArray;
{$SCOPEDENUMS ON}

type
  [TestFixture]
  TAnonymousArrayTest = class
  strict private
    DTO: TAnonymousarrayDTO;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

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

procedure TAnonymousArrayTest.ElementPropertyCheck;
var
  Properties: TArray<string>;
begin
  Properties := TTestHelper.GetProperties(DTO.Items.First);
  Assert.AreEqual(2, Length(Properties));
  Assert.AreEqual('S1', Properties[0]);
  Assert.AreEqual('S2', Properties[1]);
end;

procedure TAnonymousArrayTest.ItemsAssigned;
begin
  Assert.IsNotNull(DTO.Items);
  Assert.AreEqual(2, DTO.Items.Count);
end;

procedure TAnonymousArrayTest.ItemsElements;
var
  Element: TItemsDTO;
begin
  Element := DTO.Items[0];
  Assert.AreEqual('5102', Element.S1);
  Assert.AreEqual(default (boolean), Element.S2);

  Element := DTO.Items[1];
  Assert.AreEqual('5102', Element.S1);
  Assert.AreEqual(True, Element.S2);
end;

procedure TAnonymousArrayTest.PropertyCheck;
var
  Properties: TArray<string>;
begin
  Properties := TTestHelper.GetProperties(DTO);
  Assert.AreEqual(2, Length(Properties));
  Assert.AreEqual('Items', Properties[0]);
  Assert.AreEqual('AsJson', Properties[1]);
end;

procedure TAnonymousArrayTest.Setup;
begin
  DTO := TAnonymousarrayDTO.Create;
  DTO.LoadFromfile(TDemoData.AnonymousArray);
end;

procedure TAnonymousArrayTest.TearDown;
begin
  DTO.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TAnonymousArrayTest);

end.
