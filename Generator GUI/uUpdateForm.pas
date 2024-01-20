unit uUpdateForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Objects, FMX.Memo.Types, FMX.ScrollBox, FMX.Controls.Presentation,

  DTO.GitHUB.ReleaseDTO;

type
  TUpdateForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    lblVersion: TLabel;
    Label3: TLabel;
    lblReleasesLink: TLabel;
    Label5: TLabel;
    Panel3: TPanel;
    StyleBook1: TStyleBook;
    Label6: TLabel;
    lblDownloadLink: TLabel;
    procedure FormShow(Sender: TObject);
    procedure lblReleasesLinkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    FRelease: TRelease;
    { Private declarations }
  public
    { Public declarations }
    property NewRelease: TRelease read FRelease write FRelease;
  end;

implementation

uses
  System.UIConsts, Pkg.Json.Utils;

{$R *.fmx}

procedure TUpdateForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    ModalResult := mrCancel;
end;

procedure TUpdateForm.FormShow(Sender: TObject);
begin
  lblVersion.Text := FRelease.TagName;
  lblReleasesLink.Text := FRelease.HtmlUrl;

  lblDownloadLink.Text := FRelease.AssetsUrl;

  Memo1.Text := FRelease.Body;
  (lblReleasesLink.FindStyleResource('text') as TText).OnClick := lblReleasesLinkClick;
  (lblDownloadLink.FindStyleResource('text') as TText).OnClick := lblReleasesLinkClick;
end;

procedure TUpdateForm.lblReleasesLinkClick(Sender: TObject);
begin
  ShellExecute((Sender as TText).Text);
  ModalResult := mrOk;
end;

end.
