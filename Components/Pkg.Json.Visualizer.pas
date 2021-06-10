unit Pkg.Json.Visualizer;

interface

uses
  FMX.TreeView,

  Pkg.Json.Mapper, Pkg.Json.StubField;

Type
  JsonVisualizer = class
  private
    class procedure InternalFormatTreeViewFields(AItem: TTreeViewItem);
    class procedure FormatFields(aTreeView: TTreeView);
    class procedure InternalVisualize(aTreeViewItem: TTreeViewItem; aClass: TStubClass; AItemStyleLookup: string);

  public
    // Visualizes stub class structure in a treeview
    class procedure Visualize(aTreeView: TTreeView; AItemStyleLookup: string; aJsonString: string); overload;
    class procedure Visualize(aTreeView: TTreeView; AItemStyleLookup: string; aMapper: TPkgJsonMapper); overload;
  end;

implementation

uses
  System.Sysutils, Pkg.Json.JsonValueHelper;

{ TJsonVisualizer }

class procedure JsonVisualizer.Visualize(aTreeView: TTreeView; AItemStyleLookup: string; aJsonString: string);
var
  JsonMapper: TPkgJsonMapper;
begin
  JsonMapper := TPkgJsonMapper.Create;
  try
    JsonMapper.Parse(aJsonString);
    Visualize(aTreeView, AItemStyleLookup, JsonMapper);
  finally
    JsonMapper.Free;
  end;
end;

class procedure JsonVisualizer.FormatFields(aTreeView: TTreeView);
begin
  if aTreeView.Count = 1 then
    InternalFormatTreeViewFields(aTreeView.Items[0]);
end;

class procedure JsonVisualizer.InternalFormatTreeViewFields(AItem: TTreeViewItem);
var
  LItem: TTreeViewItem;
  k: Integer;
  LSize, LPos: Integer;
begin
  LSize := 0;

  // Find max len
  for k := 0 to AItem.Count - 1 do
  begin
    LItem := AItem.Items[k];
    LPos := Pos(':', LItem.Text);
    if (LPos > 0) AND (LPos > LSize) then
      LSize := LPos;
  end;

  for k := 0 to AItem.Count - 1 do
  begin
    LItem := AItem.Items[k];
    LPos := LSize - Pos(':', LItem.Text);
    if (LPos > 0) then
      LItem.Text := LItem.Text.Replace(':', StringOfChar(' ', LPos) + ':');

    InternalFormatTreeViewFields(LItem);
  end;

end;

class procedure JsonVisualizer.InternalVisualize(aTreeViewItem: TTreeViewItem; aClass: TStubClass; AItemStyleLookup: string);
var
  StubField: TStubField;
  TreeViewItem: TTreeViewItem;
begin
  for StubField in aClass.Items do
  begin
    TreeViewItem := TTreeViewItem.Create(aTreeViewItem);
    TreeViewItem.StyleLookup := AItemStyleLookup;
    TreeViewItem.TagObject := StubField;
    TreeViewItem.WordWrap := false;

    case StubField.FieldType of
      jtObject:
        begin
          TreeViewItem.Text := StubField.Name + ': {} ' + StubField.TypeAsString;
          InternalVisualize(TreeViewItem, (StubField as TStubObjectField).FieldClass, AItemStyleLookup);
        end;

      jtArray:
        begin
          TreeViewItem.Text := StubField.Name + ': [] ' + StubField.TypeAsString;
          if (StubField as TStubArrayField).ContainedType = jtObject then
            InternalVisualize(TreeViewItem, (StubField as TStubArrayField).FieldClass, AItemStyleLookup);
        end;

    else
      TreeViewItem.Text := StubField.Name + ': ' + StubField.TypeAsString;
    end;

    aTreeViewItem.AddObject(TreeViewItem);
  end;
end;

class procedure JsonVisualizer.Visualize(aTreeView: TTreeView; AItemStyleLookup: string; aMapper: TPkgJsonMapper);
var
  TreeViewItem: TTreeViewItem;
  RootClass: TStubClass;
begin
  aTreeView.Clear;
  try
    RootClass := aMapper.RootClass;
    if RootClass = nil then
      exit;

    aTreeView.BeginUpdate;
    TreeViewItem := TTreeViewItem.Create(aTreeView);
    TreeViewItem.Text := RootClass.Name;
    TreeViewItem.TagObject := RootClass;
    TreeViewItem.WordWrap := false;
    aTreeView.AddObject(TreeViewItem);
    InternalVisualize(TreeViewItem, RootClass, AItemStyleLookup);
    FormatFields(aTreeView);
  finally
    aTreeView.ExpandAll;
    aTreeView.EndUpdate;
  end;

end;

end.

