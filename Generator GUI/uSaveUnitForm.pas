unit uSaveUnitForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo.Types;

type
  TSaveUnitForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    btnClose: TButton;
    btnSave: TButton;
    sd: TSaveDialog;
    Label1: TLabel;
    StyleBook1: TStyleBook;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    FJson: string;
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    procedure SetJson(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    property FileName: string read GetFileName write SetFileName;
    property Json: string read FJson write SetJson;
  end;

implementation

{$R *.fmx}

uses
  uMainForm, System.IoUtils;

procedure TSaveUnitForm.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSaveUnitForm.btnSaveClick(Sender: TObject);
var
  Buffer: TStringList;
  ResourceStream: TResourceStream;
begin
  if not sd.Execute then
    exit;

  Memo1.Lines.SaveToFile(FileName);

  Buffer := TStringList.Create;
  ResourceStream := TResourceStream.Create(HInstance, 'JsonDTO', 'PAS');
  try
    ResourceStream.Position := 0;
    Buffer.LoadFromStream(ResourceStream);
    Buffer.SaveToFile(TPath.GetDirectoryName(FileName) + TPath.DirectorySeparatorChar + 'Pkg.Json.DTO.pas');
  finally
    ResourceStream.Free;
    Buffer.Free;
  end;
end;

procedure TSaveUnitForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    ModalResult := mrCancel;
end;

function TSaveUnitForm.GetFileName: string;
begin
  Result := sd.FileName;
end;

procedure TSaveUnitForm.Memo1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    ModalResult := mrCancel;
end;

procedure TSaveUnitForm.SetFileName(const Value: string);
begin
  sd.FileName := Value;
  Caption := 'Preview Delphi Unit - ' + Value;
end;

procedure TSaveUnitForm.SetJson(const Value: string);
begin
  FJson := Value;
  Memo1.Lines.Text := FJson;
end;

end.
