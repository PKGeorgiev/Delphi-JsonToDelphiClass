unit Test.ArrayTestBase;

interface

uses
  DUnitX.TestFramework, DUnitX.Assert.Ex,
  System.Generics.Collections,
  Test.Helper.Helper, Test.TestBase, Pkg.Json.DTO;

type
  TArrayTestBase<T: TJsonDTO , constructor; U: class> = class abstract(TTestBase<T>)
  protected
    procedure TestItemsAssigned(aItems: TObjectList<U>);
    procedure TestElementPropertyCheck(aDTO : U);
  end;

implementation


{ TArrayTestBase<T, U> }

procedure TArrayTestBase<T, U>.TestElementPropertyCheck(aDTO: U);
var
  Properties: TArray<string>;
begin
  Properties := TTestHelper.GetProperties(aDTO);
  Assert.AreEqual(2, Length(Properties));
  Assert.AreEqual('S1', Properties[0]);
  Assert.AreEqual('S2', Properties[1]);
end;

procedure TArrayTestBase<T, U>.TestItemsAssigned(aItems: TObjectList<U>);
begin
 Assert.IsNotNull(aItems);
 Assert.AreEqual(2, aItems.Count);
end;

end.
