unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, System.Json, Rest.Json, FMX.TreeView, TypInfo, RTTI,
  regularexpressions, generics.collections, Pkg.Json.Mapper, NetEncoding,
  FMX.Menus, FMX.Controls.Presentation, FMX.Edit, FMX.ConstrainedForm, REST.Client,
  uUpdate, System.Threading, uGitHub, FMX.Objects, uUpdateForm;

type
  TMainForm = class(TConstrainedForm)
    Memo1: TMemo;
    tv: TTreeView;
    StyleBook1: TStyleBook;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    MainPopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    btnVisualize: TButton;
    btnGenerateUnit: TButton;
    btnExit: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    Label5: TLabel;
    MemoPopupMenu: TPopupMenu;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    Panel4: TPanel;
    procedure btnVisualizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGenerateUnitClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure MainPopupMenuPopup(Sender: TObject);
    procedure tvDblClick(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Label1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure tvKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
    procedure DisableMenuItems;
    procedure VisualizeClass;
    procedure PrepareMenu;
  public
    { Public declarations }
    jm: TPkgJsonMapper;
    FTerminationTask: ITask;
    FCheckForUpdateTask: ITask;
    FCheckVersionResponse: TObject;
    FChanged: boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses uSaveUnitForm,
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib;
{$ENDIF POSIX}

procedure TMainForm.btnVisualizeClick(Sender: TObject);
begin
  if FChanged then
    MessageDlg('You made changes to the structure. Do you want to load original class?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrYes then
          VisualizeClass;
      end
    )
  else
    VisualizeClass;
end;

procedure TMainForm.DisableMenuItems;
var
  k: integer;
begin
  for k := 0 to MainPopupMenu.ItemsCount - 1 do
  begin
    MainPopupMenu.Items[k].Enabled := false;
  end;
end;

procedure TMainForm.btnGenerateUnitClick(Sender: TObject);
begin
  if tv.Count = 0 then
    btnVisualizeClick(self);

  jm.DestinationUnitName := edit2.Text;
  SaveUnitForm.sd.FileName := jm.DestinationUnitName + '.pas';

  SaveUnitForm.Memo1.DeleteSelection;
  SaveUnitForm.Memo1.Text := jm.GenerateUnit;
  SaveUnitForm.Caption := 'Preview Delphi Unit - ' + SaveUnitForm.sd.FileName;

  //  ShowModal bug - QC129552
  //  The same is declared in the SaveUnitForm's OnShow event
  SaveUnitForm.width := MainForm.Width - 50;
  SaveUnitForm.height := MainForm.Height - 50;
  SaveUnitForm.left := MainForm.Left + 25;
  SaveUnitForm.top := MainForm.Top + 25;

  SaveUnitForm.ShowModal;
end;

procedure TMainForm.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FCheckForUpdateTask <> nil then
    if FTerminationTask = nil then
    begin

      //  Disable GUI elements
      CanClose := false;
      edit2.Enabled := false;
      Memo1.Enabled := false;
      tv.Enabled := false;
      tv.PopupMenu := nil;
      btnExit.Enabled := false;
      btnVisualize.Enabled := false;
      btnGenerateUnit.Enabled := false;

      label1.Text := 'Terminating application, please wait...';
      //  We start a termination task.
      //  This task will wait for the CheckForUpdateTask to complete.
      FTerminationTask := TTask.Run(
        procedure
        var
          LTask: ITask;
        begin

          LTask := FCheckForUpdateTask;
          try

            if LTask <> nil then
            begin
              //  Since TTask.Wait is buggy we use simple polling...
              while FCheckForUpdateTask <> nil do
                Sleep(100);
            end;

          finally

            LTask := nil;
            FTerminationTask := nil;

            TThread.Synchronize(nil,
              procedure
              begin
                Close;
              end
            );

          end;

        end
      );
    end
    else
  else
  begin
    CanClose := true;
    FCheckVersionResponse.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
//var
//  LRelease: TReleaseClass;
//  s: string;
//  tsl: TStringList;
begin
  self.Constraints.MinWidth := 1024;
  self.Constraints.MinHeight := 560;


  Caption := 'JsonToDelphiClass - ' + FloatToStr(ProgramVersion, PointDsFormatSettings) + ' | By Petar Georgiev';

  jm := TPkgJsonMapper.Create(tv);

  //  Test for MadExcept Report Leaks issue
  //  tsl := TStringList.Create;
  //  tsl.LoadFromFile('d:\json.txt');
  //  LRelease := TReleaseClass.FromJsonString(tsl.Text);
  ////  tsl.LoadFromFile('d:\json2.txt');
  //  LRelease := TReleaseClass.FromJsonString(tsl.Text);

//  btnVisualizeClick(self);

  //exit;

  label1.Text := 'Checking for update...';
  FCheckForUpdateTask := CheckForUpdate(
    procedure(ARelease: TObject)
    begin
      FCheckVersionResponse := ARelease;
      if FTerminationTask = nil then
      begin
        if ARelease is TReleaseClass then
        begin
          label1.StyleLookup := 'LabelLinkStyle';
          label1.Text := 'Version ' + (FCheckVersionResponse as TReleaseClass).tag_name + ' is available! Click here to download!';
          (label1.FindStyleResource('text') as TText).OnClick := label1Click;
          label1.HitTest := true;
        end
        else
          if ARelease is TErrorClass then
          begin
            label1.StyleLookup := 'LabelErrorStyle';
            label1.Text := 'Error checking for new version: ' + (ARelease as TErrorClass).message;
          end
          else
          begin
            label1.StyleLookup := 'LabelGreenStyle';
            label1.Text := 'Your version ' + FloatToStr(uUpdate.ProgramVersion, PointDsFormatSettings) + ' is up to date! For more information about JsonToDelphiClass click here!';
            (label1.FindStyleResource('text') as TText).OnClick := label1Click;
          end;
      end;

      FCheckForUpdateTask := nil;
    end
  );

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  jm.Free;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then
    close;
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

procedure TMainForm.Memo1DblClick(Sender: TObject);
var
  LTsl: TStringList;
  LJsonValue: TJSONValue;
begin
  LTsl := TStringList.Create;
  try
    LJsonValue := TJSONObject.ParseJSONValue(memo1.Text);
    try
      if LJsonValue <> nil then
        PrettyPrintJSON(LJsonValue, LTsl);
    finally
      LJsonValue.Free;
    end;
    memo1.Text := LTsl.Text;
  finally
    LTsl.Free;
  end;
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
var
  LString: string;
  LField: TStubField;
begin
  LField := (Sender as TFmxObject).TagObject as TStubField;
  LString := InputBox('Rename Property ' + LField.Name, 'Enter new Property name', LField.Name);
  if (LString <> '') AND (LString.ToLower <> LField.Name.ToLower) then
  begin
    FChanged := true;
    LField.Name := LString;
    jm.Visualize(tv, 'TreeViewItem1Style1');
  end;
end;

procedure TMainForm.MenuItem5Click(Sender: TObject);
var
  LString: string;
  LClass: TStubClass;
begin
  LClass := (Sender as TFmxObject).TagObject as TStubClass;
  LString := InputBox('Rename Class ' + LClass.Name, 'Enter new Class name', LClass.PureClassName);
  if (LString <> '') AND (LString.ToLower <> LClass.PureClassName.ToLower) then
  begin
    FChanged := true;
    LClass.Name := LString;
    jm.Visualize(tv, 'TreeViewItem1Style1');
  end;
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
begin
  if Panel1.Width < 200 then
    Panel1.Width := 200
  else
    if Panel1.Width > (MainForm.Width - 20) div 2 then
      Panel1.Width := (MainForm.Width - 20) div 2;
end;

procedure TMainForm.MainPopupMenuPopup(Sender: TObject);
var
  LItem: TTreeViewItem;
  LPoint: TPointF;
begin
  DisableMenuItems;
  MainPopupMenu.Items[0].Text := '---';
  LPoint :=  tv.AbsoluteToLocal(ScreenToClient(MainPopupMenu.PopupPoint));
  LItem := tv.ItemByPoint(LPoint.X, LPoint.Y);
  if LItem <> nil then
    LItem.Select;

  PrepareMenu;
end;

procedure TMainForm.PrepareMenu;
var
  LField: TStubField;
begin
  if tv.Selected <> nil then
  begin
    MainPopupMenu.Items[0].Text := tv.Selected.Text;

    if tv.Selected <> tv.Items[0] then
    begin
      LField := tv.Selected.TagObject as TStubField;

      MainPopupMenu.Items[2].Enabled := true;
      MainPopupMenu.Items[2].TagObject := LField;

      if (LField is TStubContainerField) AND ((LField as TStubContainerField).ContainedType = TJsonType.jtObject) then
      begin
        MainPopupMenu.Items[3].Enabled := true;
        MainPopupMenu.Items[3].TagObject := (LField as TStubContainerField).FieldClass;
      end;
    end
    else
    begin
      MainPopupMenu.Items[3].Enabled := true;
      MainPopupMenu.Items[3].TagObject := tv.Selected.TagObject;
    end;
  end;
end;

procedure TMainForm.tvDblClick(Sender: TObject);
begin
  if tv.Selected <> nil then
    tv.Selected.IsExpanded := not tv.Selected.IsExpanded;
end;

procedure TMainForm.tvKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if ((KeyChar = #0) AND (Key = 113)) AND (tv.Selected <> nil) then
  begin
    PrepareMenu;

    if tv.Selected = tv.Items[0] then
      MenuItem5Click(MenuItem5)
    else
      MenuItem3Click(MenuItem3);
  end;

end;

procedure TMainForm.VisualizeClass;
begin
  FChanged := false;

  jm.Parse(memo1.Text, 'Root');
  jm.Visualize(tv, 'TreeViewItem1Style1');

  //  Workarround for QC129540
  Panel1.Width := Panel1.Width + 1;
  Panel1.Width := Panel1.Width - 1;
end;

end.
