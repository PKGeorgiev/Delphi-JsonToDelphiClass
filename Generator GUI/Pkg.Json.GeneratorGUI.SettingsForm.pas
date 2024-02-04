unit Pkg.Json.GeneratorGUI.SettingsForm;

interface

uses
  System.SysUtils, System.Types, System.Rtti, System.UITypes, System.Classes, System.Variants, System.Bindings.Outputs,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Bind.Editors,

  Data.Bind.Components, Data.Bind.ObjectScope, Data.Bind.GenData, Data.Bind.EngExt, FMX.Bind.DBEngExt,

  Pkg.Json.Settings;

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
    CheckBoxSuppressZeroDate: TCheckBox;
    LinkControlToField5: TLinkControlToField;
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TSettingsForm.PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
begin
  ABindSourceAdapter := TObjectBindSourceAdapter<TSettings>.Create(Self, TSettings.Instance, false);
  ABindSourceAdapter.AutoPost := True;
end;

end.
