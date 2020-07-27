unit Pkg.Json.DemoGenerator;

interface

uses
  Pkg.Json.Mapper;

{$M+}

type
  TDestinationFrameWork = (dfBoth, dfVCL, dfFMX);

  TDemoGenerator = class
  private
    FDestinationDirectory: string;
    FJsonMapper: TPkgJsonMapper;
    FTDestinationFrameWork: TDestinationFrameWork;
    procedure Validate;
    procedure ExtractZipFile;
    procedure GenerateFrameWorkINC;
    procedure UpdateDemoProject;
    procedure ModifyDemoHelper;
    procedure SetDestinationDirectory(const Value: string);
  published
    property DestinationDirectory: string read FDestinationDirectory write SetDestinationDirectory;
    property DestinationFrameWork: TDestinationFrameWork read FTDestinationFrameWork write FTDestinationFrameWork;
  public
    constructor Create(aJsonMapper: TPkgJsonMapper); reintroduce;
    procedure Execute;
  end;

implementation

uses
  System.Classes, System.Zip, System.SysUtils, System.IOUtils, System.RTLConsts;

{ TDemoGenerator }

constructor TDemoGenerator.Create(aJsonMapper: TPkgJsonMapper);
begin
  inherited Create;
  FJsonMapper := aJsonMapper;
  FDestinationDirectory := '';
  FTDestinationFrameWork := TDestinationFrameWork.dfBoth;
end;

procedure TDemoGenerator.Execute;
begin
  Validate;
  ExtractZipFile;
  UpdateDemoProject;
end;

procedure TDemoGenerator.ExtractZipFile;
var
  ResourceStream: TResourceStream;
  ZipFile: TZipFile;
begin
  ResourceStream := TResourceStream.Create(HInstance, 'DemoTemplate', 'ZipFile');
  ZipFile := TZipFile.Create;
  ZipFile.Open(ResourceStream, TZipMode.zmRead);
  ZipFile.ExtractAll(FDestinationDirectory);
  ZipFile.Free;
  ResourceStream.Free;
end;

procedure TDemoGenerator.GenerateFrameWorkINC;
begin
  with TStringList.Create do
    try
      case FTDestinationFrameWork of
        dfBoth:
          begin
            Add('{.$DEFINE FMX}');
            Add('{.$DEFINE VCL}');
          end;
        dfVCL:
          begin
            Add('{.$DEFINE FMX}');
            Add('{$DEFINE VCL}');
          end;
        dfFMX:
          begin
            Add('{$DEFINE FMX}');
            Add('{.$DEFINE VCL}');
          end;
      end;
      Add('');
      Add('{$IF not Defined(VCL) and not Defined(FMX)}');
      Add('  Please define framework, above.');
      Add('{$ENDIF}');

      SaveToFile(FDestinationDirectory + 'FrameWork.inc');
    finally
      Free;
    end;
end;

procedure TDemoGenerator.ModifyDemoHelper;
var
  FileName, s: String;

  procedure ReplaceAll(aToken, aText: string);
  begin
    s := StringReplace(s, aToken, aText, [rfReplaceAll]);
  end;

  procedure SaveText;
  begin
    with TStringList.Create do
      try
        Text := s;
        SaveToFile(FileName);
      finally
        Free
      end;
  end;
  procedure LoadText;
  begin
    with TStringList.Create do
      try
        LoadFromFile(FileName);
        s := Text;
      finally
        Free;
      end;
  end;

begin
  FileName := FDestinationDirectory + TPath.DirectorySeparatorChar;
  FileName := FileName + 'Demo Helper' + TPath.DirectorySeparatorChar;
  FileName := FileName + 'Demo.DemoHelper.pas';
  LoadText;
  ReplaceAll('@@UnitName@@', FJsonMapper.DestinationUnitName);
  ReplaceAll('@@ClassName@@', FJsonMapper.DestinationClassName + 'DTO');
  SaveText;
end;

procedure TDemoGenerator.SetDestinationDirectory(const Value: string);
begin
  FDestinationDirectory := IncludeTrailingPathDelimiter(Value);
end;

procedure TDemoGenerator.UpdateDemoProject;
begin
  GenerateFrameWorkINC;
  with TStringList.Create do
    try
      Text := FJsonMapper.JsonString;
      SaveToFile(FDestinationDirectory + 'DemoData.json');
    finally
      Free;
    end;

  FJsonMapper.SaveToFile(FDestinationDirectory + FJsonMapper.DestinationUnitName + '.pas');
  ModifyDemoHelper;
end;

procedure TDemoGenerator.Validate;
begin
  if FDestinationDirectory.Trim = string.empty then
    raise EIntError.Create('DestinationDirectory can not be empty');

  if not TDirectory.Exists(FDestinationDirectory) then
    raise EInOutError.CreateRes(@SDirectoryInvalid);
end;

end.
