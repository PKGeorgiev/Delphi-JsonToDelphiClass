unit AnonymousArray;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  TItemsDTO = class
  private
    FS1: Boolean;
    FS2: Boolean;
  published
    property S1: Boolean read FS1 write FS1;
    property S2: Boolean read FS2 write FS2;
  end;
  
  TAnonymousarrayDTO = class(TJsonDTO)
  private
    [JSONName('Items')]
    FItemsArray: TArray<TItemsDTO>;
    [JSONMarshalled(False)]
    FItems: TObjectList<TItemsDTO>;
    function GetItems: TObjectList<TItemsDTO>;
  published
    property Items: TObjectList<TItemsDTO> read GetItems;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
  
implementation

{ TAnonymousarrayDTO }

constructor TAnonymousarrayDTO.Create;
begin
  inherited;
end;

destructor TAnonymousarrayDTO.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TAnonymousarrayDTO.GetItems: TObjectList<TItemsDTO>;
begin
  Result := ObjectList<TItemsDTO>(FItems, FItemsArray);
end;

end.
