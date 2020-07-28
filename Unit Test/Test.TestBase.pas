unit Test.TestBase;

interface

uses
  DUnitX.TestFramework, DUnitX.Assert.Ex,
  Test.Helper.Helper, Pkg.Json.DTO;

type
  [TestFixture]
  TTestBase<T: TJsonDTO, constructor> = class abstract
  strict protected
    DTO: T;
    function DemoData: TDemoData; virtual; abstract;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;
  end;

implementation

{ TTestBase<T> }

procedure TTestBase<T>.Setup;
begin
  DTO := T.Create;
  DTO.LoadFromfile(DemoData);
end;

procedure TTestBase<T>.TearDown;
begin
  DTO.Free;
end;

end.
