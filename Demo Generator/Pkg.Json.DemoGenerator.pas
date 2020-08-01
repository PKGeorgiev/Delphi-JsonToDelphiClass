unit Pkg.Json.DemoGenerator;

interface

uses
  System.SysUtils,
  Pkg.Json.Mapper;

{$M+}

type
  TDestinationFrameWork = (dfBoth, dfVCL, dfFMX);

  TDemoGenerator = class
  private
    FRootDirectory: string;
    FJsonMapper: TPkgJsonMapper;
    FTDestinationFrameWork: TDestinationFrameWork;
    FDestinationClassName: string;
    FDestinationUnitName: string;
    FJson: string;
    FFileName: TFilename;
    FDestinationDirectory: string;
    procedure Validate;
    procedure ExtractZipFile;
    procedure GenerateFrameWorkINC;
    procedure UpdateDemoProject;
    procedure ModifyDemoHelper;
    procedure SetRootDirectory(const Value: string);
  published
    property RootDirectory: string read FRootDirectory write SetRootDirectory;
    property DestinationDirectory : string read FDestinationDirectory;
    property DestinationFrameWork: TDestinationFrameWork read FTDestinationFrameWork write FTDestinationFrameWork;
    property DestinationClassName: string read FDestinationClassName write FDestinationClassName;
    property DestinationUnitName: string read FDestinationUnitName write FDestinationUnitName;
    property Json: string read FJson write FJson;
  public
    constructor Create; overload;
    constructor Create(aFileName: TFilename); overload;
    destructor Destroy; override;
    procedure Execute;
  end;

implementation

uses
  System.Classes, System.Zip, System.IOUtils, System.RTLConsts;

{ TDemoGenerator }

constructor TDemoGenerator.Create;
begin
  inherited Create;

  FJsonMapper := TPkgJsonMapper.Create;;
  FDestinationClassName := '';
  FDestinationUnitName := '';
  FJson := '';
  FRootDirectory := '';
  FTDestinationFrameWork := TDestinationFrameWork.dfBoth;
  FFileName := '';
end;

constructor TDemoGenerator.Create(aFileName: TFilename);
begin
  inherited Create;

  FJsonMapper := TPkgJsonMapper.Create;;
  FFileName := aFileName;
  FDestinationClassName := '';
  FDestinationUnitName := '';
  FJson := '';
  FRootDirectory := '';
  FTDestinationFrameWork := TDestinationFrameWork.dfBoth;
end;

destructor TDemoGenerator.Destroy;
begin
  FreeAndNil(FJsonMapper);
  inherited;
end;

procedure TDemoGenerator.Execute;
begin
  Validate;

  FJsonMapper.DestinationClassName := FDestinationClassName;
  FJsonMapper.DestinationUnitName := FDestinationUnitName;
  FDestinationDirectory := FRootDirectory + FJsonMapper.DestinationClassName + TPath.DirectorySeparatorChar;
  TDirectory.CreateDirectory(FDestinationDirectory);

  if FFileName = '' then
    FJsonMapper.Parse(FJson)
  else
    FJsonMapper.LoadFormFile(FFileName);

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

procedure TDemoGenerator.SetRootDirectory(const Value: string);
begin
  FRootDirectory := IncludeTrailingPathDelimiter(Value);
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
  if FRootDirectory.Trim = string.empty then
    raise EPathNotFoundException.Create('RootDirectory can not be empty');

  if not TDirectory.Exists(FRootDirectory) then
    raise EPathNotFoundException.CreateRes(@SDirectoryInvalid);

  if FDestinationClassName = '' then
    raise EArgumentException.Create('DestinationClassName must be provided');

  if FDestinationUnitName = '' then
    raise EArgumentException.Create('DestinationUnitName must be provided');

  if FFileName + FJson = '' then
    raise EArgumentException.Create('Json must be provided');

end;

end.
