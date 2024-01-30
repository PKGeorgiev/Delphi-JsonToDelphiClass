unit TestPkg.TestSuppressZero;

interface

uses System.Sysutils,
  DUnitX.TestFramework, DUnitX.Assert.Ex,

  Pkg.Json.JSONName, Pkg.Json.DTO;

type
  TDateTimeDTO = class(TJsonDTO)
  private
    [SuppressZero]
    FSuppressDate: TDateTime;
    FNoSuppressDate: TDateTime;
  public
    property DateSuppress: TDateTime read FSuppressDate write FSuppressDate;
    property NoDateSuppress: TDateTime read FNoSuppressDate write FNoSuppressDate;
  end;

  // Test methods for class TJSONName
  [TestFixture]
  TestSuppressZero = class
  private
    FDateTimeDTO: TDateTimeDTO;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestDateTime;
  end;

implementation

{ TestSuppressZero }

procedure TestSuppressZero.Setup;
begin
  FDateTimeDTO := TDateTimeDTO.Create;
end;

procedure TestSuppressZero.TearDown;
begin
  FDateTimeDTO.Free;
end;

procedure TestSuppressZero.TestDateTime;
begin
  FDateTimeDTO.DateSuppress := 0;
  FDateTimeDTO.NoDateSuppress := 0;
{$IF CompilerVersion >= 36} //Delphi 12
  Assert.AreEqual(FDateTimeDTO.AsJson, '{"suppressDate":"","noSuppressDate":"1899-12-30T00:00:00.000Z","options":["joDateIsUTC","joDateFormatISO8601"]}');
{$ELSE}
  Assert.AreEqual(FDateTimeDTO.AsJson, '{"suppressDate":"","noSuppressDate":"1899-12-30T00:00:00.000Z"}');
{$IFEND}
end;

end.
