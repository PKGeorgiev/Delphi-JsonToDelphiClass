unit DummyJson.Lib.Enumerator;

interface

uses
  System.Classes, System.Generics.Collections,

  Pkg.Json.DTO;

{$M+}

Type
  TListeEumerator<TElement: class> = class(TComponent)
  strict private
    FDto: TJsonDTO;
    FElements: TList<TElement>;
    FCurrentIndex: Integer;
    procedure DoChange;
    function IndexInrange: Boolean;
    function GetCurrent: TElement;
  private
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
  published
    property Current: TElement read GetCurrent;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  public
    constructor Create(aOwner: TComponent; aDTO: TJsonDTO; aElements: TList<TElement>); reintroduce;
    destructor Destroy; override;
    Function Next: TElement;
    Function Previous: TElement;
  end;

implementation

{ TListeEumerator<TDto, TElements> }

constructor TListeEumerator<TElement>.Create(aOwner: TComponent; aDTO: TJsonDTO; aElements: TList<TElement>);
begin
  inherited Create(aOwner);
  FDto := aDTO;
  FElements := aElements;
  FCurrentIndex := 0;
  DoChange;
end;

destructor TListeEumerator<TElement>.Destroy;
begin
  FDto.Free;
  inherited;
end;

procedure TListeEumerator<TElement>.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TListeEumerator<TElement>.GetCurrent: TElement;
begin
  if not IndexInrange then
    Exit(nil);

  Result := FElements[FCurrentIndex];
end;

function TListeEumerator<TElement>.IndexInrange: Boolean;
begin
  if FElements.Count = 0 then
    Exit(False);

  Result := (FCurrentIndex >= 0) and (FCurrentIndex < FElements.Count);
end;

function TListeEumerator<TElement>.Next: TElement;
begin
  FCurrentIndex := (FCurrentIndex + 1) mod FElements.Count;
  Result := Current;
  DoChange;
end;

function TListeEumerator<TElement>.Previous: TElement;
begin
  FCurrentIndex := FCurrentIndex - 1;
  if FCurrentIndex < 0 then
    FCurrentIndex := FElements.Count - 1;

  Result := Current;
  DoChange;
end;

procedure TListeEumerator<TElement>.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  DoChange;
end;

end.
