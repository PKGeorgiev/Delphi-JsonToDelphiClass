library GeneratorLIB;

uses
  System.SysUtils, System.Classes, System.IOUtils,

  Pkg.Json.Mapper, Pkg.Json.Settings;

{$R *.res}


function GenerateUnit(Settings: WideString; Json: WideString; out SouceFile: WideString): WordBool; stdcall;
begin
  TSettings.Instance.AsJson := string(Settings);

  with TPkgJsonMapper.Create do
    try
      DestinationClassName := 'Root';
      DestinationUnitName := 'Root.pas';
      Parse(string(Json));
      SouceFile := GenerateUnit;
    finally
      Free;
    end;

  Result := True;
end;

exports
  GenerateUnit;

end.
