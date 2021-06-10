unit uUpdate;

interface

uses
  System.JSON, System.SysUtils, System.Classes, IPPeerClient,

  DTO.GitHUB.Release, Pkg.JSON.ThreadingEx;

const
  ProgramVersion: double = 3.0;
  UpdateUrl = 'https://api.github.com/repos/JensBorrisholt/Delphi-JsonToDelphiClass/releases';
  ProgramUrl = 'https://github.com/JensBorrisholt/Delphi-JsonToDelphiClass';

function CheckForUpdate(AOnFinish: TProc<TRelease, string>): ITaskEx;

implementation

uses
  System.Generics.Collections,

  Pkg.JSON.SerializableObject;

function CheckForUpdate(AOnFinish: TProc<TRelease, string>): ITaskEx;
var
  Releases: TObjectList<TRelease>;
  LResult: TRelease;
  LErrorMessage: string;
begin
  Result := TTaskEx.Run(
      procedure
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
    end).ContinueWithInMainThread(
    procedure(const aTask: ITaskEx)
    begin
      AOnFinish(LResult, LErrorMessage);
    end, TTaskContinuationOptions.OnlyOnCompleted);
end;

initialization

end.
