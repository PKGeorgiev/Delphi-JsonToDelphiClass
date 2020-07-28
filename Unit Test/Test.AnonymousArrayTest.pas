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
    procedure ItemAssigned;

    [Test]
    procedure ItemElements;
  end;

implementation

{ TAnonymousArrayTest }

procedure TAnonymousArrayTest.ItemAssigned;
begin
  Assert.IsNotNull(DTO.Items);
  Assert.AreEqual(2, DTO.Items.Count);
end;

procedure TAnonymousArrayTest.ItemElements;
var
  Element : TItemsDTO;
begin
  Element = DTO.Items[0];
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
