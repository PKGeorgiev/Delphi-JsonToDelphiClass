unit TestPkg.PersonDTO;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;
{$M+}

type
  TPerson = class
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  TPersons = class(TJsonDTO)
  private
    [JSONName('Items'), JSONMarshalled(False)]
    FPersonsArray: TArray<TPerson>;
    [GenericListReflect]
    FPersons: TObjectList<TPerson>;
    function GetPersons: TObjectList<TPerson>;
  protected
    procedure SetAsJson(aValue: string); override;
  published
    property Persons: TObjectList<TPerson> read GetPersons;
  public
    destructor Destroy; override;
  end;

function GetPersons: TPersons;

implementation

{ TPersons }
function GetPersons: TPersons;
const
  Json = '[{"Name": "Test Person"}]';
begin
  Result := TPersons.Create;
  Result.AsJson := Json;
end;

destructor TPersons.Destroy;
begin
  GetPersons.Free;
  inherited;
end;

function TPersons.GetPersons: TObjectList<TPerson>;
begin
  Result := ObjectList<TPerson>(FPersons, FPersonsArray);
end;

procedure TPersons.SetAsJson(aValue: string);
begin
  RefreshArray<TPerson>(FPersons, FPersonsArray);
  inherited;
end;

end.
