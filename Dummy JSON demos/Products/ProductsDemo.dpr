program ProductsDemo;

uses
  Vcl.Forms,
  DummyJson.Products.MainForm in 'DummyJson.Products.MainForm.pas' {MainForm},
  DummyJson.ProductsDTO in '..\DTO\DummyJson.ProductsDTO.pas',
  Pkg.Json.DTO in '..\Lib\Pkg.Json.DTO.pas',
  DummyJson.Lib.Enumerator in '..\Lib\DummyJson.Lib.Enumerator.pas',
  DummyJson.Lib.DTODownloader in '..\Lib\DummyJson.Lib.DTODownloader.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
