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

      FileName := TPath.GetFileName(FullFileName).Replace('.json', '');
      Console.Write('* Building E2E Test for %s ... ', [FileName]);

      with TDemoGenerator.Create(FullFileName) do
        try
          DestinationClassName := string(FileName).Replace(#32, '');
          DestinationUnitName := JsonMapper.DestinationClassName;

          RootDirectory := OutputDirectory;
          DestinationFrameWork := TDestinationFrameWork.dfVCL;
          Execute;
          OutputDirectory := DestinationDirectory;
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
        Console.WriteLine('Sucess!');
      end
      else
      begin
        Console.ForegroundColor := TConsoleColor.Red;
        Console.WriteLine('Failed!');
      end;

      if Sucess then
      begin
        Console.ForegroundColor := TConsoleColor.Blue;
        Console.Write('   Launching [%s] demo ... ', [TPath.GetFileName(FullFileName).Replace('.json', '')]);

        Sucess := ShellExecute(0, 'OPEN', Pchar(FileName), '', Pchar(TPath.GetDirectoryName(FileName)), SW_SHOWNORMAL) > 32;
        if Sucess then
        begin
          Console.ForegroundColor := TConsoleColor.Green;
          Console.WriteLine('Sucess!');
        end
        else
        begin
          Console.ForegroundColor := TConsoleColor.Red;
          Console.WriteLine('Failed!');
        end;
      end;

      Console.ForegroundColor := TConsoleColor.White;
      Console.WriteLine('');
    end;

    Console.ForegroundColor := TConsoleColor.White;
    Console.WriteLine('Press any key to continue ...');
    Console.ReadLine;
    OutputDirectory := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + 'JsonToDelphiClass E2E Test\' + 'Test Run ' + s + TPath.DirectorySeparatorChar;

    ShellExecute(0, 'OPEN', Pchar(OutputDirectory), '', '', SW_SHOWNORMAL);
    OutputBuffer.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
