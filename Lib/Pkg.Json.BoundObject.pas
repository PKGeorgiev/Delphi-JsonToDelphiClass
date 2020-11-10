unit Pkg.Json.BoundObject;

interface

uses
  Generics.Collections, System.Bindings.Expression, System.Bindings.Helper;

type
  TBoundObject = class
  protected type
    TExpressionList = TObjectList<TBindingExpression>;
  private
    FBindings: TExpressionList;
  protected
    procedure Notify(const APropertyName: string = '');
    property Bindings: TExpressionList read FBindings;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string; const ACreateOptions: TBindings.TCreateOptions = [coNotifyOutput, coEvaluate]);
    procedure ClearBindings;
  end;

implementation

constructor TBoundObject.Create;
begin
  inherited;
  FBindings := TExpressionList.Create(False { AOwnsObjects } );
end;

destructor TBoundObject.Destroy;
begin
  ClearBindings;
  FBindings.Free;
  inherited;
end;

procedure TBoundObject.ClearBindings;
var
  i: TBindingExpression;
begin
  for i in FBindings do
    TBindings.RemoveBinding(i);
  FBindings.Clear;
end;

procedure TBoundObject.Notify(const APropertyName: string);
begin
  TBindings.Notify(Self, APropertyName);
end;

procedure TBoundObject.Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string; const ACreateOptions: TBindings.TCreateOptions);
begin
  // From source to dest
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(Self, 'src')])], 'src.' + AProperty,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(ABindToObject, 'dst')])], 'dst.' + ABindToProperty, nil, nil, ACreateOptions));
  // From dest to source
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(ABindToObject, 'src')])], 'src.' + ABindToProperty,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(Self, 'dst')])], 'dst.' + AProperty, nil, nil, ACreateOptions));
end;

end.
