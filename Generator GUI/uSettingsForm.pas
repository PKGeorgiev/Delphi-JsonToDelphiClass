unit uSettingsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Pkg.Json.Settings, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  Data.Bind.Components, Data.Bind.ObjectScope, Data.Bind.GenData, System.Rtti,
  System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.EngExt, FMX.Bind.DBEngExt;

type
  TSettingsForm = class(TForm)
    chbAddJsonPropertyAttributes: TCheckBox;
    Label1: TLabel;
    chbUsePascalCase: TCheckBox;
    btnOk: TButton;
    chbPostfixClassNames: TCheckBox;
    edPostFix: TEdit;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkPropertyToFieldEnabled: TLinkPropertyToField;
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
  System.Bindings.Helper;

{$R *.fmx}

procedure TSettingsForm.PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
begin
  ABindSourceAdapter := TObjectBindSourceAdapter<TSettings>.Create(Self, TSettings.Instance, false);
  ABindSourceAdapter.AutoPost := True;
end;

end.