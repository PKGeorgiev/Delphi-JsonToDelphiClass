program JsonToDelphiClass;

{$R 'JSON_PAS.res' 'JSON_PAS.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  Pkg.Json.Mapper in 'Mapper\Pkg.Json.Mapper.pas',
  uSaveUnitForm in 'uSaveUnitForm.pas' {SaveUnitForm},
  uGitHub in 'uGitHub.pas',
  FMX.ConstrainedForm in 'FMX.ConstrainedForm.pas' {/  IdSSLOpenSSLHeaders,},
  uUpdate in 'uUpdate.pas',
  uUpdateForm in 'uUpdateForm.pas' {UpdateForm},
  Pkg.Json.Visualizer in 'Pkg.Json.Visualizer.pas',
  Pkg.Json.DTO in 'Pkg.Json.DTO.pas',
  Pkg.Json.ReservedWords in 'Pkg.Json.ReservedWords.pas',
  Pkg.Json.JSONName in 'Mapper\Pkg.Json.JSONName.pas',
  Pkg.Json.StubField in 'Mapper\Pkg.Json.StubField.pas',
  Pkg.Json.Lists in 'Pkg.Json.Lists.pas';

{$R *.res}

{$WEAKLINKRTTI OFF}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSaveUnitForm, SaveUnitForm);
  Application.CreateForm(TUpdateForm, UpdateForm);
  Application.Run;
end.
