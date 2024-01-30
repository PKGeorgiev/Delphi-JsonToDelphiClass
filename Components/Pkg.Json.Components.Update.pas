﻿unit Pkg.Json.Components.Update;

interface

uses
  System.JSON, System.SysUtils,

  Pkg.JSON.ThreadingEx, DTO.GitHUB.ReleaseDTO;

const
  ProgramVersion: double = 3.2;
  ProgramUrl = 'https://github.com/JensBorrisholt/Delphi-JsonToDelphiClass';
  HTTP_OK = 200;

function CheckForUpdate(AOnFinish: TProc<TRelease, string>): ITaskEx;

implementation

uses
  System.Net.HttpClient, System.Net.HttpClientComponent;

function CheckForUpdate(AOnFinish: TProc<TRelease, string>): ITaskEx;
const
  UpdateUrl = 'https://api.github.com/repos/JensBorrisholt/Delphi-JsonToDelphiClass/releases';
  HTTP_OK = 200;
var
  ErrorMessage: string;
  Releases: TReleases;
  Release: TRelease;
  Respons: IHTTPResponse;
begin
  Result := TTaskEx.Run(
    procedure

    begin
      Releases := TReleases.Create;
      with TNetHTTPClient.Create(nil) do
        try
          try
            Respons := Get(UpdateUrl);

            if Respons.StatusCode = HTTP_OK then
              Releases.AsJson := Respons.ContentAsString
            else
            begin
              Release := nil;
              Exit;


            end;

            if Releases.Items.Count >= 0 then
            begin
              Release := Releases.Items.First;
              if JsonToFloat(Release.TagName) <= ProgramVersion then
                Release := nil;
            end
            else
              Release := nil;

            ErrorMessage := '';
          except
            on e: Exception do
              ErrorMessage := e.message;
          end;
        finally
          Free;
        end;
    end
    ).ContinueWithInMainThread(
    procedure(const aTask: ITaskEx)
    begin
      AOnFinish(Release, ErrorMessage);
      FreeAndNil(Releases);
    end, TTaskContinuationOptions.OnlyOnCompleted);
end;

end.
