unit MainFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Threading,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.Menus, System.Actions, FMX.ActnList, FMX.Memo.Types, FMX.Objects,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.StdCtrls,
  FMX.ListView, FMX.Layouts, FMX.TreeView, FMX.Edit,

  Pkg.Json.Mapper, DTO.GitHUB.Release, Pkg.Json.OutputFormat;

type
  TMainForm = class(TForm)
    MenuBar1: TMenuBar;
    ActionList1: TActionList;
    actOpen: TAction;
    actSaveAs: TAction;
    actExit: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    TabControl1: TTabControl;
    TabItemJSON: TTabItem;
    MenuItemView: TMenuItem;
    actDelphiUnit: TAction;
    actOnlineValidation: TAction;
    MenuItem5: TMenuItem;
    MemoJSON: TMemo;
    actSettings: TAction;
    MenuItem6: TMenuItem;
    actDemoData: TAction;
    ListView1: TListView;
    Splitter1: TSplitter;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    actClassVisualizer: TAction;
    TreeView: TTreeView;
    Panel3: TPanel;
    Panel4: TPanel;
    Label5: TLabel;
    EditUnitName: TEdit;
    Label2: TLabel;
    EditClassName: TEdit;
    Splitter2: TSplitter;
    actBSON: TAction;
    Formats: TMenuItem;
    Button1: TButton;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    actMinifyJson: TAction;
    actConvert: TAction;
    OpenDialog1: TOpenDialog;
    actDemoProject: TAction;
    MenuItem12: TMenuItem;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    sd: TSaveDialog;
    StyleBook1: TStyleBook;
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actOnlineValidationExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actDemoDataExecute(Sender: TObject);
    procedure ListView1Change(Sender: TObject);
    procedure actClassVisualizerExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actConvertExecute(Sender: TObject);
    procedure EmptyExecute(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure EditClassNameChange(Sender: TObject);
    procedure MemoJSONExit(Sender: TObject);
  private type
    TValidationTypes = (vtUnchecked, vtValid, vtInvalid);
  private
    { Private declarations }
    FCheckVersionResponse: TRelease;
    FJsonMapper: TPkgJsonMapper;
    FIsValid: TValidationTypes;
    FJson: string;
    procedure SetJSON(const Value: string);
  public
    { Public declarations }
    property Json: string read FJson write SetJSON;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils, System.Json, IdURI,

  FMX.DialogService,

  uSettingsForm, uUpdate, uUpdateForm,
  Pkg.Json.Utils, Pkg.Json.ThreadingEx, Pkg.Json.Lib.JSONConverter, Pkg.Json.Visualizer, Pkg.Json.DemoGenerator;

{$R *.fmx}

const
  DemoDataRoot = '../../../Demo Data/';

procedure TMainForm.actClassVisualizerExecute(Sender: TObject);
begin
  TreeView.Visible := actClassVisualizer.Checked;
  Splitter2.Visible := actClassVisualizer.Checked;
  if not actClassVisualizer.Checked then
    exit;

  FJsonMapper.DestinationUnitName := EditUnitName.Text;
  FJsonMapper.Parse(MemoJSON.Text);

  JsonVisualizer.Visualize(TreeView, 'TreeViewItem1Style1', FJsonMapper);
end;

procedure TMainForm.actConvertExecute(Sender: TObject);
var
  Destination: string;
begin
  FJson := MemoJSON.Text;

  while TabControl1.TabCount > 1 do
    TabControl1.Delete(TabControl1.TabCount - 1);

  OutputFormatDict.Clear;

  if actBSON.Checked then
    with TOutputFormat.Create(TabControl1, 'BJSON', TJSONConverter.Json2BsonString(FJson), 'bson') do
      Execute;

  if actMinifyJson.Checked then
    with TOutputFormat.Create(TabControl1, 'Minify Json', TJSONConverter.MinifyJson(FJson), 'json') do
      Execute;

  if actDelphiUnit.Checked then
  begin
    FJsonMapper.DestinationClassName := EditClassName.Text;
    FJsonMapper.DestinationUnitName := EditUnitName.Text;
    FJsonMapper.Parse(FJson);

    with TOutputFormat.Create(TabControl1, 'Delphi Unit', FJsonMapper.GenerateUnit, 'pas') do
      Execute;
  end;

  if actDemoProject.Checked then
  begin
    if not SelectDirectory('Select a directory', Destination, Destination) then
      exit;

    with TDemoGenerator.Create do
      try
        DestinationClassName := EditClassName.Text;
        DestinationUnitName := EditUnitName.Text;
        DestinationDirectory := Destination;
        DestinationFrameWork := TDestinationFrameWork.dfBoth;
        Json := MemoJSON.Text;
        Execute;
      finally
        Free;
      end;

    TDialogService.MessageDialog('Demo project sucessfull genereted. Do you want to open the destination folder?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult <> mrYes then
          exit;

        ShellExecute(Destination);
      end)
  end;

  TabControl1.Last;
end;

procedure TMainForm.actDemoDataExecute(Sender: TObject);
var
  FileName: string;
begin
  ListView1.Visible := actDemoData.Checked;
  Splitter1.Visible := actDemoData.Checked;

  if not TDirectory.Exists(DemoDataRoot) then
    exit;

  ListView1.BeginUpdate;
  try
    for FileName in TDirectory.GetFiles(DemoDataRoot, '*.json') do
      ListView1.Items.Add.Text := TPath.GetFileName(FileName);
  finally
    ListView1.EndUpdate;
  end;

  TTaskEx.QueueMainThread(50,
    procedure
    begin
      ListView1.ItemIndex := 0;
      ListView1.OnChange(nil);
    end);
end;

procedure TMainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
var
  OutputFormat: TOutputFormat;
begin
  if not OutputFormatDict.TryGetValue(TabControl1.ActiveTab, OutputFormat) then
    OutputFormat := nil;

  actConvert.Enabled := FIsValid = vtValid;
  actSaveAs.Enabled := (OutputFormat <> nil) and (actConvert.Enabled);
end;

procedure TMainForm.MemoJSONExit(Sender: TObject);
begin
  Json := MemoJSON.Text;
end;

procedure TMainForm.SetJSON(const Value: string);
begin
  if FJson = Value then
    exit;

  FJson := Value;

  if FJsonMapper.IsValid(FJson) then
    FIsValid := vtValid
  else
  begin
    FIsValid := vtInvalid;
    FJson := string.empty;
  end;
end;

procedure TMainForm.actOnlineValidationExecute(Sender: TObject);
const
  JsonValidatorUrl = 'https://jsonformatter.curiousconcept.com/?data=%s&process=true';
begin
  ShellExecute(TIdURI.URLEncode(Format(JsonValidatorUrl, [MinifyJson(MemoJSON.Text)])));
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    exit;

  while TabControl1.TabCount > 1 do
    TabControl1.Delete(TabControl1.TabCount - 1);

  MemoJSON.BeginUpdate;
  Json := TFile.ReadAllText(OpenDialog1.FileName);

  MemoJSON.Lines.Text := PrettyPrint(FJson);
  MemoJSON.EndUpdate;
end;

procedure TMainForm.actSaveAsExecute(Sender: TObject);
var
  Buffer: TStringList;
  ResourceStream: TResourceStream;
  OutputFormat: TOutputFormat;
  FileExtention, Output: string;
begin
  if not OutputFormatDict.TryGetValue(TabControl1.ActiveTab, OutputFormat) then
    OutputFormat := nil;

  if OutputFormat = nil then
  begin
    FileExtention := 'json';
    Output := MemoJSON.Text;
  end
  else
  begin
    FileExtention := OutputFormat.FileExtention;
    Output := OutputFormat.Output;
  end;

  sd.FileName := EditUnitName.Text + '.' + FileExtention;

  if not sd.Execute then
    exit;

  Buffer := TStringList.Create;
  Buffer.Text := Output;
  Buffer.SaveToFile(sd.FileName);

  try
    if not SameText(FileExtention, 'pas') then
      exit;

    ResourceStream := TResourceStream.Create(HInstance, 'JsonDTO', 'PAS');
    try
      ResourceStream.Position := 0;
      Buffer.LoadFromStream(ResourceStream);
      Buffer.SaveToFile(ExtractFilePath(sd.FileName) + 'Pkg.Json.DTO.pas');
    finally
      ResourceStream.Free;
    end;

  finally
    Buffer.Free;
  end;
end;

procedure TMainForm.actSettingsExecute(Sender: TObject);
begin
  with TSettingsForm.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.EditClassNameChange(Sender: TObject);
begin
  EditUnitName.Text := EditClassName.Text + 'U';
end;

procedure TMainForm.EmptyExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FJsonMapper := TPkgJsonMapper.Create;
  FJson := '';
  Caption := 'JsonToDelphiClass - ' + FloatToJson(ProgramVersion) + '.0 | By Jens Borrisholt';

  CheckForUpdate(
    procedure(aRelease: TRelease; aErrorMessage: string)
    begin
      if aRelease <> nil then
        FCheckVersionResponse := aRelease.Clone<TRelease>
      else
        FCheckVersionResponse := nil;

      if (aRelease = nil) and (aErrorMessage = '') then
      begin
        Label1.StyleLookup := 'LabelGreenStyle';
        Label1.Text := 'Your version ' + FloatToJson(uUpdate.ProgramVersion) + ' is up to date! For more information about JsonToDelphiClass click here!';
        (Label1.FindStyleResource('text') as TText).OnClick := Label1Click;
      end
      else if aErrorMessage = '' then
      begin
        Label1.StyleLookup := 'LabelLinkStyle';
        Label1.Text := 'Version ' + aRelease.Tag_Name + ' is available! Click here to download!';
        (Label1.FindStyleResource('text') as TText).OnClick := Label1Click;
        Label1.HitTest := True;
      end
      else
      begin
        Label1.StyleLookup := 'LabelErrorStyle';
        Label1.Text := 'Error checking for new version: ' + aErrorMessage;
      end;
    end);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FJsonMapper);
  FreeAndNil(FCheckVersionResponse);
end;

procedure TMainForm.Label1Click(Sender: TObject);
begin
  if FCheckVersionResponse = nil then
    ShellExecute(ProgramUrl)
  else
    with TUpdateForm.Create(nil) do
      try
        NewRelease := FCheckVersionResponse;
        ShowModal;
      finally
        Free;
      end;
end;

procedure TMainForm.ListView1Change(Sender: TObject);
var
  Item: TListViewItem;
begin
  Item := ListView1.Selected as TListViewItem;
  if Item = nil then
  begin
    MemoJSON.Lines.Clear;
    exit;
  end;

  with TStringList.Create do
    try
      LoadFromFile(DemoDataRoot + Item.Text);
      MemoJSON.Lines.Text := TJSONConverter.PrettyPrintJSON(Text);
    finally
      Free;
    end;

  TreeView.Clear;
  actClassVisualizer.Checked := False;
  actClassVisualizer.Execute;
end;

end.
