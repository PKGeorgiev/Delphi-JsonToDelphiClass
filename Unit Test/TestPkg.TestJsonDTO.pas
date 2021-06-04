unit TestPkg.TestJsonDTO;

interface

uses
  System.Sysutils,
  DUnitX.TestFramework, DUnitX.Assert.Ex,

  TestPkg.PersonDTO, TestPkg.SimpleDTO;

Type

  [TestFixture]
  TTestJsonDTO = class
  private
    FPersons: TPersons;
    FSimpleDTO: TSimpleDTO;

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test_ToJson_UpdateSmpleList;
    [Test]
    procedure Test_ToJson_UpdateObjectList;
  end;

implementation

{ TPersons }

{ TTestJsonDTO }

procedure TTestJsonDTO.Setup;
begin
  FPersons := GetPersons;
  FSimpleDTO := GetSimpleDTO;
end;

procedure TTestJsonDTO.TearDown;
begin
  FreeAndNil(FPersons);
  FreeAndNil(FSimpleDTO);
end;

procedure TTestJsonDTO.Test_ToJson_UpdateObjectList;
var
  Person: TPerson;
  Json, OldName: string;
const
  NewName = 'New Name';
begin
  Assert.AreEqual(1, FPersons.Persons.Count);
  Person := FPersons.Persons.First;
  OldName := Person.Name;
  Assert.AreNotEqual(NewName, OldName);
  Json := FPersons.AsJson;
  Assert.IsTrue(Json.Contains(OldName));
  Person.Name := NewName;
  Json := FPersons.AsJson;
  Assert.IsTrue(Json.Contains(NewName));
  Assert.IsFalse(Json.Contains(OldName));
end;

procedure TTestJsonDTO.Test_ToJson_UpdateSmpleList;
var
  OldValue: Integer;
  Json: string;
const
  NewValue = 7;
begin
  Assert.AreEqual(1, FSimpleDTO.Options.Count);
  OldValue := FSimpleDTO.Options.First;
  Assert.AreNotEqual(NewValue, OldValue);
  Json := FSimpleDTO.AsJson;
  Assert.IsTrue(Json.Contains(OldValue.ToString));
  Assert.IsFalse(Json.Contains(NewValue.ToString));
  FSimpleDTO.Options[0] := NewValue;
  Json := FSimpleDTO.AsJson;
  Assert.IsFalse(Json.Contains(OldValue.ToString));
  Assert.IsTrue(Json.Contains(NewValue.ToString));
  FSimpleDTO.AsJson := Json;
  Assert.AreEqual(NewValue, FSimpleDTO.Options.First);
  Assert.AreNotEqual(OldValue, FSimpleDTO.Options.First);
end;

initialization

TDUnitX.RegisterTestFixture(TTestJsonDTO);

end.
