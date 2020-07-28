unit ArrayTest1;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  TArrayTestDTO = class
  private
    FS1: string;
    FS2: Boolean;
  published
    property S1: string read FS1 write FS1;
    property S2: Boolean read FS2 write FS2;
  end;

  TArraytest1DTO = class(TJsonDTO)
  private
    [JSONName('ArrayTest')]
    FArrayTestArray: TArray<TArrayTestDTO>;
    [JSONMarshalled(False)]
    FArrayTest: TObjectList<TArrayTestDTO>;
    function GetArrayTest: TObjectList<TArrayTestDTO>;
  published
    property ArrayTest: TObjectList<TArrayTestDTO> read GetArrayTest;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TArraytest1DTO }

constructor TArraytest1DTO.Create;
begin
  inherited;
end;

destructor TArraytest1DTO.Destroy;
begin
  FArrayTest.Free;
  inherited;
end;

function TArraytest1DTO.GetArrayTest: TObjectList<TArrayTestDTO>;
begin
  Result := ObjectList<TArrayTestDTO>(FArrayTest, FArrayTestArray);
end;

end.
