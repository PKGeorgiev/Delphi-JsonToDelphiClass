program JsonToDelphiClassTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}
{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  TestPkg.TestTJSONName in 'TestPkg.TestTJSONName.pas',
  Pkg.Json.JSONName in '..\Lib\Pkg.Json.JSONName.pas',
  Pkg.Json.JsonValueHelper in '..\Lib\Pkg.Json.JsonValueHelper.pas',
  TeskPkg.TestJsonValueHelper in 'TeskPkg.TestJsonValueHelper.pas',
  TestPkg.PersonDTO in 'TestPkg.PersonDTO.pas',
  TestPkg.TestJsonDTO in 'TestPkg.TestJsonDTO.pas',
  TestPkg.SimpleDTO in 'TestPkg.SimpleDTO.pas',
  TestPkg.TestSuppressZero in 'TestPkg.TestSuppressZero.pas';

{$R *.RES}

var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
  ConsoleLogger: TDUnitXConsoleLogger;

begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    // Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    // Create the test runner
    runner := TDUnitX.CreateRunner;
    // Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    // tell the runner how we will log things
    // Log to the console window
    logger := TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);
    // Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    ConsoleLogger := TDUnitXConsoleLogger.Create(false);
    runner.AddLogger(ConsoleLogger);
    runner.FailsOnNoAsserts := false; // When true, Assertions must be made during tests;

    // Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

{$IFNDEF CI}
    // We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
{$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;

end.
