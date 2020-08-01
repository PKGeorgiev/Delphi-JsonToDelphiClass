unit uUpdate;

interface

uses
  System.JSON, System.SysUtils, System.Threading, System.Classes, IPPeerClient,

  DTO.GitHUB.Release;

const
  ProgramVersion: double = 2.2;
  UpdateUrl = 'https://api.github.com/repos/JensBorrisholt/Delphi-JsonToDelphiClass/releases';
  ProgramUrl = 'https://github.com/JensBorrisholt/Delphi-JsonToDelphiClass';

procedure CheckForUpdate(AOnFinish: TProc<TRelease, string>);

implementation

uses
  System.Generics.Collections,

  Pkg.JSON.SerializableObject;

procedure CheckForUpdate(AOnFinish: TProc<TRelease, string>);
begin
  TTask.Run(
      procedure
    var
      LResult: TRelease;
      LErrorMessage: string;
      Releases: TObjectList<TRelease>;
    begin
      try
        Releases := TUGitHubSerializableObject.RestRequest<TReleasesDTO>(UpdateUrl).Releases;
        if Releases.Count >= 0 then
          LResult := Releases.Last;

        if JsonToFloat(LResult.Tag_Name) <= ProgramVersion then
          FreeAndNil(LResult);

        LErrorMessage := '';
      except
        on e: Exception do
          LErrorMessage := e.message;
      end;

      try
        // Execute AOnFinish in the context of the Main Thread
        TThread.Synchronize(nil,
            procedure
          begin
            AOnFinish(LResult, LErrorMessage);
          end);
      except
      end;
    end);

end;

initialization

end.
