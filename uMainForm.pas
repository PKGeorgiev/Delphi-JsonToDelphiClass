unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.DialogService, FMX.Dialogs,
  FMX.Layouts, FMX.TreeView, FMX.Edit, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  FMX.Menus, FMX.Controls.Presentation,
  System.Json, Rest.Json, TypInfo, RTTI,
  regularexpressions, generics.collections, Pkg.Json.Mapper, NetEncoding,
  FMX.ConstrainedForm, Rest.Client,
  uUpdate, System.Threading, uGitHub, FMX.Objects, uUpdateForm, SyncObjs,
  System.Actions, FMX.ActnList;

const
  JsonValidatorUrl = 'http://jsonlint.com';

type

  TMainForm = class(TConstrainedForm)
    Memo1: TMemo;
    StyleBook1: TStyleBook;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    MainPopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Label4: TLabel;
    MemoPopupMenu: TPopupMenu;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Panel3: TPanel;
    btnVisualize: TButton;
    btnOnlineJsonValidator: TButton;
    btnExit: TButton;
    Panel4: TPanel;
    Label5: TLabel;
    Edit2: TEdit;
    btnGenerateUnit: TButton;
    Panel1: TPanel;
    TreeView: TTreeView;
    Label3: TLabel;
    Button1: TButton;
    Label2: TLabel;
    Edit1: TEdit;
    ActionList1: TActionList;
    actPrettyPrintJSON: TAction;
    actValidateJSON: TAction;
    MenuItem4: TMenuItem;
    actRenameProperty: TAction;
    procedure btnVisualizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewUnitClick(Sender: TObject);
    procedure MainPopupMenuPopup(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Label1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure Panel1Resize(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure actPrettyPrintJSONExecute(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure actValidateJSONExecute(Sender: TObject);
    procedure actRenamePropertyExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure btnExitClick(Sender: TObject);
  private
    { Private declarations }
    FJsonMapper: TPkgJsonMapper;
    FCheckVersionResponse: TObject;
    FChanged: Boolean;
    // 0: Active
    // 1: Terminating
    // >=2: Terminated
    FApplicationStatus: Integer;
    FUpdateCheckEvent: TEvent;
    procedure VisualizeClass;
    procedure PrepareMenu;
    procedure DisableGuiElements;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  uSaveUnitForm, Pkg.Json.Visualizer, Pkg.Json.DTO,
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
Posix.Stdlib;
{$ENDIF POSIX}

procedure TMainForm.btnExitClick(Sender: TObject);
begin
  CLose;
end;

procedure TMainForm.btnVisualizeClick(Sender: TObject);
begin
  if FChanged then
    TDialogService.MessageDialog('You made changes to the structure. Do you want to load original class?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrYes then
          VisualizeClass;
      end)
  else
    VisualizeClass;
end;

procedure TMainForm.DisableGuiElements;
begin
  Edit2.Enabled := false;
  Memo1.Enabled := false;
  TreeView.Enabled := false;
  TreeView.PopupMenu := nil;
  btnExit.Enabled := false;
  btnVisualize.Enabled := false;
  btnGenerateUnit.Enabled := false;
end;

procedure TMainForm.Edit1Change(Sender: TObject);
begin
  Edit2.Text := Edit1.Text + 'U';
end;

procedure TMainForm.PreviewUnitClick(Sender: TObject);
begin
  if TreeView.Count = 0 then
    VisualizeClass;

  FJsonMapper.DestinationClassName := Edit1.Text;
  FJsonMapper.DestinationUnitName := Edit2.Text;
  FJsonMapper.Parse(Memo1.Lines.Text);
  SaveUnitForm.sd.FileName := FJsonMapper.DestinationUnitName + '.pas';

  SaveUnitForm.Memo1.Text := FJsonMapper.GenerateUnit;
  SaveUnitForm.Caption := 'Preview Delphi Unit - ' + SaveUnitForm.sd.FileName;

  // ShowModal bug - QC129552
  // The same is declared in the SaveUnitForm's OnShow event
  SaveUnitForm.Width := MainForm.Width - 50;
  SaveUnitForm.Height := MainForm.Height - 50;
  SaveUnitForm.Left := MainForm.Left + 25;
  SaveUnitForm.Top := MainForm.Top + 25;

  SaveUnitForm.ShowModal;
end;

procedure TMainForm.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
var
  StubContainerField: TStubContainerField;
begin
  actRenameProperty.Enabled := false;

  if TreeView.Selected = nil then
    exit;

  if TreeView.Selected = TreeView.Items[0] then
    exit;

  if not(TreeView.Selected.TagObject is TStubField) then
    exit;

  if TreeView.Selected.TagObject is TStubContainerField then
    StubContainerField := TStubField(TreeView.Selected.TagObject) as TStubContainerField
  else
    StubContainerField := nil;

  actRenameProperty.Enabled := (StubContainerField <> nil) and (StubContainerField.FieldType = jtObject);
end;

procedure TMainForm.actPrettyPrintJSONExecute(Sender: TObject);
begin
  Memo1.Text := TJsonDTO.PrettyPrintJSON(Memo1.Text);
end;

procedure TMainForm.actRenamePropertyExecute(Sender: TObject);
var
  s: string;
  StubField: TStubField;
begin
  StubField := (Sender as TFmxObject).TagObject as TStubField;
  s := InputBox('Rename Property ' + StubField.Name, 'Enter new Property name', StubField.Name);
  if (s <> '') and (s.ToLower <> StubField.Name.ToLower) then
  begin
    FChanged := True;
    // StubField.Name := s;
    JsonVisualizer.Visualize(TreeView, 'TreeViewItem1Style1', FJsonMapper);
  end;
end;

procedure TMainForm.actValidateJSONExecute(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(JsonValidatorUrl), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(JsonValidatorUrl)));
{$ENDIF POSIX}
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FUpdateCheckEvent.WaitFor(0) = wrSignaled then
    CanClose := True
  else
  begin
    CanClose := false;

    case FApplicationStatus of
      0:
        begin
          TInterlocked.Increment(FApplicationStatus);
          DisableGuiElements;

          Label1.Text := 'Terminating application, please wait...';

          // We start a termination task.
          // This way the main thread will not freeze
          TTask.Run(
            procedure
            begin
              FUpdateCheckEvent.WaitFor();

              // Indicate next stage
              TInterlocked.Increment(FApplicationStatus);

              // We enqueue the handler
              TThread.Queue(nil,
                procedure
                begin
                  Close;
                end);
            end);

        end;
      1:
        ;
    else
      CanClose := True;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FApplicationStatus := 0;
  FUpdateCheckEvent := TEvent.Create(nil, True, false, '');

  self.Constraints.MinWidth := 1024;
  self.Constraints.MinHeight := 560;

  Caption := 'JsonToDelphiClass - ' + FloatToStr(ProgramVersion, PointDsFormatSettings) + ' | By Jens Borrisholt';

  FJsonMapper := TPkgJsonMapper.Create;

  Label1.Text := 'Checking for update...';

  NewCheckForUpdateTask(
    procedure(ARelease: TObject)
    begin
      FCheckVersionResponse := ARelease;
      if FCheckVersionResponse is TReleaseClass then
      begin
        Label1.StyleLookup := 'LabelLinkStyle';
        Label1.Text := 'Version ' + (FCheckVersionResponse as TReleaseClass).tag_name + ' is available! Click here to download!';
        (Label1.FindStyleResource('text') as TText).OnClick := Label1Click;
        Label1.HitTest := True;
      end
      else if FCheckVersionResponse is TErrorClass then
      begin
        Label1.StyleLookup := 'LabelErrorStyle';
        Label1.Text := 'Error checking for new version: ' + (FCheckVersionResponse as TErrorClass).message;
      end
      else
      begin
        Label1.StyleLookup := 'LabelGreenStyle';
        Label1.Text := 'Your version ' + FloatToStr(uUpdate.ProgramVersion, PointDsFormatSettings) + ' is up to date! For more information about JsonToDelphiClass click here!';
        (Label1.FindStyleResource('text') as TText).OnClick := Label1Click;
      end;
      FUpdateCheckEvent.SetEvent;
    end);

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FUpdateCheckEvent);
  FreeAndNil(FJsonMapper);
  FreeAndNil(FCheckVersionResponse);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    Close;
end;

procedure TMainForm.Label1Click(Sender: TObject);
begin
  if FCheckVersionResponse <> nil then
  begin
    UpdateForm.FRelease := FCheckVersionResponse as TReleaseClass;
    UpdateForm.ShowModal;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    ShellExecute(0, 'OPEN', PChar(ProgramUrl), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
    _system(PAnsiChar('open ' + AnsiString(ProgramUrl)));
{$ENDIF POSIX}
  end;
end;

procedure TMainForm.Memo1Change(Sender: TObject);
begin
  actPrettyPrintJSON.Execute;
end;

procedure TMainForm.MenuItem5Click(Sender: TObject);
var
  s: string;
  StubClass: TStubClass;
begin
  StubClass := (Sender as TFmxObject).TagObject as TStubClass;
  s := InputBox('Rename Class ' + StubClass.Name, 'Enter new Class name', StubClass.PureClassName);
  if (s <> '') and (s.ToLower <> StubClass.PureClassName.ToLower) then
  begin
    FChanged := True;
    StubClass.Name := s;
    JsonVisualizer.Visualize(TreeView, 'TreeViewItem1Style1', FJsonMapper);
  end;
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
begin
  if Panel1.Width < 200 then
    Panel1.Width := 200
  else if Panel1.Width > (MainForm.Width - 20) div 2 then
    Panel1.Width := (MainForm.Width - 20) div 2;
end;

procedure TMainForm.MainPopupMenuPopup(Sender: TObject);
var
  Item: TTreeViewItem;
  Point: TPointF;
begin
  // DisableMenuItems;
  MainPopupMenu.Items[0].Text := '---';
  Point := TreeView.AbsoluteToLocal(ScreenToClient(MainPopupMenu.PopupPoint));
  Item := TreeView.ItemByPoint(Point.X, Point.Y);
  if Item <> nil then
    Item.Select;
  //
  PrepareMenu;
end;

procedure TMainForm.PrepareMenu;
var
  StubField: TStubField;
begin
  if TreeView.Selected = nil then
    exit;

  MainPopupMenu.Items[0].Text := TreeView.Selected.Text;
  exit;
  if TreeView.Selected <> TreeView.Items[0] then
  begin
    StubField := TreeView.Selected.TagObject as TStubField;

    MainPopupMenu.Items[2].Enabled := True;
    MainPopupMenu.Items[2].TagObject := StubField;

    if (StubField is TStubContainerField) and ((StubField as TStubContainerField).ContainedType = TJsonType.jtObject) then
    begin
      MainPopupMenu.Items[3].Enabled := True;
      MainPopupMenu.Items[3].TagObject := (StubField as TStubContainerField).FieldClass;
    end;
  end
  else
  begin
    MainPopupMenu.Items[3].Enabled := True;
    MainPopupMenu.Items[3].TagObject := TreeView.Selected.TagObject;
  end;
end;

procedure TMainForm.TreeViewDblClick(Sender: TObject);
begin
  if TreeView.Selected <> nil then
    TreeView.Selected.IsExpanded := not TreeView.Selected.IsExpanded;
end;

procedure TMainForm.TreeViewKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if ((KeyChar = #0) and (Key = 113)) and (TreeView.Selected <> nil) then
  begin
    PrepareMenu;

    if TreeView.Selected = TreeView.Items[0] then
      MenuItem5Click(MenuItem5)
    else
      actRenameProperty.Execute;
    // MenuItem3Click(MenuItem3);
  end;
end;

procedure TMainForm.VisualizeClass;
begin
  FChanged := false;

  FJsonMapper.DestinationUnitName := Edit1.Text;
  FJsonMapper.Parse(Memo1.Text);

  JsonVisualizer.Visualize(TreeView, 'TreeViewItem1Style1', FJsonMapper);

  Panel1.Width := Panel1.Width + 1;
  Panel1.Width := Panel1.Width - 1;
end;

end.
