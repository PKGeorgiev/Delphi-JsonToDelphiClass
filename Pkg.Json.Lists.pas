unit Pkg.Json.Lists;

interface

uses
  System.SysUtils, System.Generics.Defaults, System.Generics.Collections,
  Pkg.Json.JSONName;

type
  TJsonNameList<T: TJsonName> = class abstract(TObjectList<T>)
  strict protected
    constructor Create(AOwnsObjects: Boolean); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    function ItemByName(aJsonName: string): T;
  end;

  TJsonNameObjectList<T: TJsonName> = class abstract(TJsonNameList<T>)
  public
    constructor Create; reintroduce;
  end;

implementation

constructor TJsonNameList<T>.Create;
begin
  inherited Create(false);
end;

constructor TJsonNameList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited;
end;

function TJsonNameList<T>.ItemByName(aJsonName: string): T;
var
  JSONName: TJsonName;
begin
  for JSONName in Self do
    if SameText(JSONName.JSONName, aJsonName) then
      exit(JSONName as T);

  Result := nil;
end;

{ TJsonNameObjectList<T> }

constructor TJsonNameObjectList<T>.Create;
begin
  inherited Create(True);
end;

end.
