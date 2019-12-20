unit Unit38;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm38 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form38: TForm38;

implementation

{$R *.dfm}

uses
  Rest.Json,
  Root;

procedure TForm38.Button1Click(Sender: TObject);

begin

  with TStringList.Create do
    try
      LoadFromFile('C:\aa\Text.json');

      with TJson.JsonToObject<TRootDTO>(Text) do
//      with   TRootClass.FromJsonString(Text) do
        try
          Caption := Items[0].Assets[0].Browser_download_url;
        finally
          Free;
        end;
    finally
      Free;
    end;

end;

end.
