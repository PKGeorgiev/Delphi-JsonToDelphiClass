unit FMX.ConstrainedForm;

//  http://stackoverflow.com/a/8044518

interface

uses
  System.Classes, System.Types, System.UITypes, FMX.Forms, FMX.Platform, FMX.Types;

type
  TFormConstraints = class(TPersistent)
  private
    FMaxHeight: Integer;
    FMaxLeft: Integer;
    FMaxWidth: Integer;
    FMaxTop: Integer;
    FMinHeight: Integer;
    FMinLeft: Integer;
    FMinWidth: Integer;
    FMinTop: Integer;
  public
    constructor Create;
  published
    property MaxHeight: Integer read FMaxHeight write FMaxHeight default 0;
    property MaxLeft: Integer read FMaxLeft write FMaxLeft default 0;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 0;
    property MaxTop: Integer read FMaxTop write FMaxTop default 0;
    property MinHeight: Integer read FMinHeight write FMinHeight default 0;
    property MinLeft: Integer read FMinLeft write FMinLeft default 0;
    property MinWidth: Integer read FMinWidth write FMinWidth default 0;
    property MinTop: Integer read FMinTop write FMinTop default 0;
  end;

  TConstrainedForm = class(TCustomForm)
  private
    FConstraints: TFormConstraints;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure StartWindowResize; override;
    procedure StartWindowDrag; override;
  published
    property Constraints: TFormConstraints read FConstraints write FConstraints;
    property BiDiMode;
    property Caption;
    property Cursor default crDefault;
    property BorderStyle default TFmxFormBorderStyle.Sizeable;
    property BorderIcons default [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
    property ClientHeight;
    property ClientWidth;
    property Left;
    property Top;
    property Position default TFormPosition.DefaultPosOnly;
    property Width;
    property Height;
    property Transparency;
    property Visible;
    property WindowState default TWindowState.wsNormal;
    property OnCreate;
    property OnDestroy;
    property OnClose;
    property OnCloseQuery;
    property OnActivate;
    property OnDeactivate;
    property OnResize;
    property Fill;
    property StyleBook;
    property ActiveControl;
    property StyleLookup;
    property OnPaint;
    //  XE7
    property Padding;
    property FormFactor;
    property OnKeyDown;
  end;

procedure Register;

implementation

{ TFormConstraints }

constructor TFormConstraints.Create;
begin
  inherited;
  FMaxHeight := 0;
  FMaxLeft := 0;
  FMaxWidth := 0;
  FMaxTop := 0;
  FMinHeight := 0;
  FMinLeft := 0;
  FMinWidth := 0;
  FMinTop := 0;
end;

{ TConstrainedForm }

constructor TConstrainedForm.Create(AOwner: TComponent);
begin
  FConstraints := TFormConstraints.Create;
  inherited;
end;

destructor TConstrainedForm.Destroy;
begin
  FConstraints.Free;
  inherited;
end;

procedure TConstrainedForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (FConstraints.FMinWidth > 0) and (AWidth < FConstraints.FMinWidth) then
    AWidth := FConstraints.FMinWidth;

  if (FConstraints.FMaxWidth > 0) and (AWidth > FConstraints.FMaxWidth) then
    AWidth := FConstraints.FMaxWidth;

  if (FConstraints.FMinHeight > 0) and (AHeight < FConstraints.FMinHeight) then
    AHeight := FConstraints.FMinHeight;

  if (FConstraints.FMaxHeight > 0) and (AHeight > FConstraints.FMaxHeight) then
    AHeight := FConstraints.FMaxHeight;

  if (FConstraints.FMinLeft > 0) and (ALeft < FConstraints.FMinLeft) then
    ALeft := FConstraints.FMinLeft;

  if (FConstraints.FMaxLeft > 0) and (ALeft > FConstraints.FMaxLeft) then
    ALeft := FConstraints.FMaxLeft;

  if (FConstraints.FMinTop > 0) and (ATop < FConstraints.FMinTop) then
    ATop := FConstraints.FMinTop;

  if (FConstraints.FMaxTop > 0) and (ATop > FConstraints.FMaxTop) then
    ATop := FConstraints.FMaxTop;

  FWinService.SetWindowRect(Self, RectF(ALeft, ATop, ALeft + AWidth, ATop + AHeight));
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TConstrainedForm.StartWindowDrag;
begin
  inherited;
end;

procedure TConstrainedForm.StartWindowResize;
begin
  inherited;
end;

procedure Register;
begin
  RegisterClass(TConstrainedForm);
end;

end.
