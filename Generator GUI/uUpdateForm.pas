unit uUpdateForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Objects, FMX.Memo.Types, FMX.ScrollBox, FMX.Controls.Presentation,

  DTO.GitHUB.Release;

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
    Label2: TLabel;
    lblDownloadCount: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblReleasesLinkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    FRelease: TRelease;
    procedure SetRelease(const Value: TRelease);
    { Private declarations }
  public
    { Public declarations }
    property NewRelease: TRelease read FRelease write SetRelease;
  end;

implementation

uses
  System.UIConsts, Pkg.Json.Utils;
{$R *.fmx}

procedure TUpdateForm.Button1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TUpdateForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    ModalResult := mrCancel;
end;

procedure TUpdateForm.FormShow(Sender: TObject);
begin
  lblVersion.Text := FRelease.Tag_Name;
  lblReleasesLink.Text := FRelease.Html_Url;

  if FRelease.Assets.Count > 0 then
  begin
    lblDownloadLink.Text := FRelease.Assets.First.Browser_Download_Url;
    lblDownloadCount.Text := FRelease.Assets.First.Download_Count.ToString();
  end
  else
  begin
    lblDownloadLink.Text := lblReleasesLink.Text;
    lblDownloadCount.Text := '0';
  end;

  Memo1.Text := FRelease.body;
  (lblReleasesLink.FindStyleResource('text') as TText).OnClick := lblReleasesLinkClick;
  (lblDownloadLink.FindStyleResource('text') as TText).OnClick := lblReleasesLinkClick;
end;

procedure TUpdateForm.lblReleasesLinkClick(Sender: TObject);
var
  LUrl: string;
begin
  LUrl := (Sender as TText).Text;
  ShellExecute(LUrl);
  ModalResult := mrOk;
end;

procedure TUpdateForm.SetRelease(const Value: TRelease);
begin
  FRelease := Value;
end;

end.
