program AuthenticationDemo;

uses
  Vcl.Forms,
  DummyJson.Authentication.MainForm in 'DummyJson.Authentication.MainForm.pas' {MainForm},
  DummyJson.UsersDTO in '..\DTO\DummyJson.UsersDTO.pas',
  Pkg.Json.DTO in '..\Lib\Pkg.Json.DTO.pas',
  DummyJson.Lib.AuthWebService in '..\Lib\DummyJson.Lib.AuthWebService.pas',
  DummyJson.AuthenticaticedUsers in 'DummyJson.AuthenticaticedUsers.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
