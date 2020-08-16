unit Pkg.JSON.SubTypes;

interface

uses
  System.Classes,

  Pkg.JSON.StubField;

type
  SubTypes = class abstract
  public
    class procedure AddSubTypes(const aStubClass: TStubClass; const aSubClasslist: TStringlist);
  end;

implementation

{ SubTypes }

class procedure SubTypes.AddSubTypes(const aStubClass: TStubClass; const aSubClasslist: TStringlist);
var
  StubField: TStubField;
  StubArrayField: TStubArrayField;
begin
  for StubField in aStubClass.ComplexItems do
    aSubClasslist.Add(StubField.TypeAsString);

  for StubField in aStubClass.Items do
    if StubField.IsObjectArrayField then
    begin
      StubArrayField := StubField as TStubArrayField;
      aSubClasslist.Add(StubArrayField.TypeAsString);
    end;
end;

end.
