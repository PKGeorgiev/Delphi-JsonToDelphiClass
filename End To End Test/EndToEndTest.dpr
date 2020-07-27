program EndToEndTest;

{$APPTYPE CONSOLE}
{$R *.res}
{$R 'DemoProject.res' '..\Demo Generator\DemoProject.rc'}
{$R 'JSON_PAS.res' '..\Lib\JSON_PAS.rc'}

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.Console in 'System.Console.pas',
  Pkg.Json.Mapper,
  Pkg.Json.DemoGenerator,
  DelphiBuilderU in 'DelphiBuilderU.pas';

const
  DemoDataRoot = '../../../Demo Data/';

var
  FullFileName, FileName: TFileName;
  s, OutputDirectory: String;
  JsonMapper: TPkgJsonMapper;
  OutputBuffer: TStringlist;
  Sucess: boolean;

begin
  Console.ForegroundColor := TConsoleColor.White;

  try
    s := FormatDateTime('yyyymmdd-hhnnns', now);
    JsonMapper := TPkgJsonMapper.Create;
    OutputBuffer := TStringlist.Create;

    for FullFileName in TDirectory.GetFiles(DemoDataRoot, '*.json') do
    begin
      OutputDirectory := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + 'JsonToDelphiClass E2E Test\' + 'Test Run ' + s + TPath.DirectorySeparatorChar;
      TDirectory.CreateDirectory(OutputDirectory);

      FileName := TPath.GetFileName(FullFileName);
      Console.Write('** Building E2E Test for %s ... ', [FileName]);
      JsonMapper.DestinationClassName := ChangeFileExt(FileName, '').Replace(#32, '');
      JsonMapper.DestinationUnitName := JsonMapper.DestinationClassName;
      JsonMapper.LoadFormFile(FullFileName);

      OutputDirectory := OutputDirectory + JsonMapper.DestinationClassName + TPath.DirectorySeparatorChar;
      TDirectory.CreateDirectory(OutputDirectory);

      with TDemoGenerator.Create(JsonMapper) do
        try
          DestinationDirectory := OutputDirectory;
          DestinationFrameWork := TDestinationFrameWork.dfVCL;
          Execute;
        finally
          Free;
        end;

      OutputBuffer.Clear;
      DelphiBuilder.CompileProject(OutputBuffer, OutputDirectory + 'VCL\Demo.dproj');

      FileName := OutputDirectory + 'Win32\Release\Demo.exe';
      Sucess := TFile.Exists(FileName);
      if Sucess then
      begin
        Console.ForegroundColor := TConsoleColor.Green;
        Console.Write('Sucess!');
      end
      else
      begin
        Console.ForegroundColor := TConsoleColor.Red;
        Console.Write('Failed!');
      end;

      Console.ForegroundColor := TConsoleColor.White;
      Console.WriteLine(' **');
      Console.WriteLine('');
    end;

    Console.ForegroundColor := TConsoleColor.White;
    Console.WriteLine('Press anykey to continue ...');
    Console.ReadLine;
    OutputDirectory := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + 'JsonToDelphiClass E2E Test\' + 'Test Run ' + s + TPath.DirectorySeparatorChar;

    ShellExecute(0, 'OPEN', Pchar(OutputDirectory), '', '', SW_SHOWNORMAL);
    OutputBuffer.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
