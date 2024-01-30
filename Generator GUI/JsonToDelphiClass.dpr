program JsonToDelphiClass;

{$R 'DemoProject.res' '..\Demo Generator\DemoProject.rc'}
{$R 'JSON_PAS.res' '..\Lib\JSON_PAS.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  Pkg.Json.Components.Update in '..\Components\Pkg.Json.Components.Update.pas',
  Pkg.Json.Visualizer in '..\Components\Pkg.Json.Visualizer.pas',
  Pkg.Json.DemoGenerator in '..\Demo Generator\Pkg.Json.DemoGenerator.pas',
  Pkg.Json.DTO in '..\Lib\Pkg.Json.DTO.pas',
  Pkg.Json.JSONName in '..\Lib\Pkg.Json.JSONName.pas',
  Pkg.Json.Lists in '..\Lib\Pkg.Json.Lists.pas',
  Pkg.Json.Mapper in '..\Lib\Pkg.Json.Mapper.pas',
  Pkg.Json.ReservedWords in '..\Lib\Pkg.Json.ReservedWords.pas',
  Pkg.Json.StubField in '..\Lib\Pkg.Json.StubField.pas',
  Pkg.Json.JsonValueHelper in '..\Lib\Pkg.Json.JsonValueHelper.pas',
  DTO.GitHUB.ReleaseDTO in '..\DTO\GitHUB\DTO.GitHUB.ReleaseDTO.pas',
  Pkg.Json.Utils in '..\Lib\Pkg.Json.Utils.pas',
  Pkg.JSON.SubTypes in '..\Lib\Pkg.JSON.SubTypes.pas',
  Pkg.Json.Settings in '..\Lib\Pkg.Json.Settings.pas',
  Pkg.Json.ThreadingEx in '..\Lib\Pkg.Json.ThreadingEx.pas',
  MainFormU in 'MainFormU.pas' {MainForm},
  Pkg.Json.Lib.JSONConverter in '..\Lib\Pkg.Json.Lib.JSONConverter.pas',
  Pkg.Json.OutputFormat in '..\Lib\Pkg.Json.OutputFormat.pas',
  Pkg.Json.GeneratorGUI.SettingsForm in 'Pkg.Json.GeneratorGUI.SettingsForm.pas' {SettingsForm},
  Pkg.Json.GeneratorGUI.UpdateForm in 'Pkg.Json.GeneratorGUI.UpdateForm.pas' {UpdateForm};

{$R *.res}

{$WEAKLINKRTTI OFF}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
