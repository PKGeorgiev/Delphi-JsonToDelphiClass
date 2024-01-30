unit uUpdate;

interface

uses
  System.JSON, System.SysUtils, System.Classes, IPPeerClient,

  DTO.GitHUB.Release, Pkg.JSON.ThreadingEx;

const
  ProgramVersion: double = 3.1;
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
  Release: TRelease;
  ErrorMessage: string;
begin
  Result := TTaskEx.Run(
    procedure
    begin
      try
        Releases := TUGitHubSerializableObject.RestRequest<TReleasesDTO>(UpdateUrl).Releases;
        if Releases.Count >= 0 then
          Release := Releases.Last;

        if JsonToFloat(Release.Tag_Name) <= ProgramVersion then
          FreeAndNil(Release);

        ErrorMessage := '';
      except
        on e: Exception do
          ErrorMessage := e.message;
      end;
    end).ContinueWithInMainThread(
    procedure(const aTask: ITaskEx)
    begin
      AOnFinish(Release, ErrorMessage);
      FreeAndNil(Releases);
    end, TTaskContinuationOptions.OnlyOnCompleted);
end;

initialization

end.
