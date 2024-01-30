unit TestPkg.SimpleDTO;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  TSimpleDTO = class(TJsonDTO)
  private
    [JSONName('options')]
    FOptionsArray: TArray<Integer>;
    [JSONMarshalled(False)]
    FOptions: TList<Integer>;
    function GetOptions: TList<Integer>;
  protected
    function GetAsJson: string; override;
  published
    property Options: TList<Integer> read GetOptions;
  public
    destructor Destroy; override;
  end;

function GetSimpleDTO : TSimpleDTO;
implementation

{ TSimpleDTO }

function GetSimpleDTO : TSimpleDTO;
const
  Json = '{"options": [5]}';
begin
  Result := TSimpleDTO.Create;
  Result.AsJson := Json;
end;

destructor TSimpleDTO.Destroy;
begin
  GetOptions.Free;
  inherited;
end;

function TSimpleDTO.GetOptions: TList<Integer>;
begin
  Result := List<Integer>(FOptions, FOptionsArray);
end;

function TSimpleDTO.GetAsJson: string;
begin
  RefreshArray<Integer>(FOptions, FOptionsArray);
  Result := inherited;
end;

end.
