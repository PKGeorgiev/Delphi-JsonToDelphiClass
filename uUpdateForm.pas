unit uUpdateForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, uGitHub, FMX.Objects;

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
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    FRelease: TReleaseClass;
  end;

var
  UpdateForm: TUpdateForm;

implementation

uses System.UIConsts,
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib;
{$ENDIF POSIX}

{$R *.fmx}

procedure TUpdateForm.Button1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TUpdateForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if key = 27 then
    ModalResult := mrCancel;
end;

procedure TUpdateForm.FormShow(Sender: TObject);
begin
  lblVersion.Text := FRelease.tag_name;
  lblReleasesLink.Text := FRelease.html_url; //FRelease.assets[0].browser_download_url;

  if length(FRelease.assets) > 0 then
  begin
    lblDownloadLink.Text := FRelease.assets[0].browser_download_url;
    lblDownloadCount.text := FRelease.assets[0].download_count.ToString();
  end
  else
  begin
    lblDownloadLink.Text := lblReleasesLink.Text;
    lblDownloadCount.text := '0';
  end;

  memo1.Text := FRelease.body;
  (lblReleasesLink.FindStyleResource('text') as TText).OnClick := lblReleasesLinkClick;
  (lblDownloadLink.FindStyleResource('text') as TText).OnClick := lblReleasesLinkClick;
end;

procedure TUpdateForm.lblReleasesLinkClick(Sender: TObject);
var
  LUrl: string;
begin
  //  http://monkeystyler.com/blog/entry/a-clickable-hotlink-urllabel-for-firemonkey
  LUrl := (Sender as TText).Text;
  {$IFDEF MSWINDOWS}
    ShellExecute(0, 'OPEN', PChar(LUrl), '', '', SW_SHOWNORMAL);
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
    _system(PAnsiChar('open ' + AnsiString(LUrl)));
  {$ENDIF POSIX}

  ModalResult := mrOk;
end;

end.
