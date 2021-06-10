unit Pkg.Json.OutputFormat;

interface

uses
  System.Classes, System.Generics.Collections,
  FMX.Types,
  FMX.Memo, FMX.TabControl;

type
  TOutputFormat = class
  strict private
    FOwner: TTabControl;
    FTabCaption: string;
    FOutput: string;
    FFileExtention: string;
  public
    constructor Create(aOwner: TTabControl; aTabCaption: string; aOutput: string; aFileExtention: string); reintroduce;
    procedure Execute;
    property Output: string read FOutput;
    property FileExtention: string read FFileExtention;
  end;

  TOutputFormatDict = class(TObjectDictionary<TTabItem, TOutputFormat>)
  private
    class var Instance: TOutputFormatDict;
  public
    constructor Create; reintroduce;
  end;

function OutputFormatDict: TOutputFormatDict;

implementation

{ TOutputFormat }
function OutputFormatDict: TOutputFormatDict;
begin
  if TOutputFormatDict.Instance = nil then
    TOutputFormatDict.Instance := TOutputFormatDict.Create;
  Result := TOutputFormatDict.Instance;
end;

constructor TOutputFormat.Create(aOwner: TTabControl; aTabCaption: string; aOutput: string; aFileExtention: string);
begin
  inherited Create;
  FOwner := aOwner;
  FTabCaption := aTabCaption;
  FOutput := aOutput;
  FFileExtention := aFileExtention;
end;

procedure TOutputFormat.Execute;
var
  TabItem: TTabItem;
begin
  TabItem := FOwner.Add as TTabItem;
  TabItem.Text := FTabCaption;
  with TMemo.Create(TabItem) do
  begin
    Parent := TabItem;
    Align := TAlignLayout.Client;
    Text := FOutput;
    WordWrap := True;
    ReadOnly := True;

    TextSettings.Font.Family := 'Consolas';
    TextSettings.Font.Size := 14;
    StyledSettings := StyledSettings - [TStyledSetting.Size];
    Margins.Bottom := 4;
  end;

  OutputFormatDict.Add(TabItem, Self);

end;

{ TOutputFormatDict }

constructor TOutputFormatDict.Create;
begin
  inherited Create([doOwnsValues]);
end;

initialization

finalization

OutputFormatDict.Free;

end.
