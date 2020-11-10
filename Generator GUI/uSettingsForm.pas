unit uSettingsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Pkg.Json.Settings, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TSettingsForm = class(TForm)
    chbAddJsonPropertyAttributes: TCheckBox;
    Label1: TLabel;
    chbUsePascalCase: TCheckBox;
    btnOk: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
  private
    { Private declarations }
    FSettings: TSettings;
  public
    { Public declarations }
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
  System.Bindings.Helper;

{$R *.fmx}

procedure TSettingsForm.CheckBoxChange(Sender: TObject);
begin
  TBindings.Notify(Sender, 'IsChecked');
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  FSettings := TSettings.Instance;
  FSettings.Bind('AddJsonPropertyAttributes', chbAddJsonPropertyAttributes, 'IsChecked');
  FSettings.Bind('UsePascalCase', chbUsePascalCase, 'IsChecked');
end;

end.
