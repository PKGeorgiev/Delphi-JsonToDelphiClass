program Project38;

uses
  Vcl.Forms,
  Unit38 in 'Unit38.pas' {Form38},
  Root in 'Root.pas',
  Pkg.Json.DTO in 'Pkg.Json.DTO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm38, Form38);
  Application.Run;
end.
