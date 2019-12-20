unit uUpdate;

interface

uses REST.Client, uGitHub, REST.JSON, JSON,
  IPPeerClient, SysUtils, System.Threading, Classes, Pkg.JSON.Mapper;

const
  ProgramVersion: double = 0.65;
  UpdateUrl = 'https://api.github.com/repos/PKGeorgiev/Delphi-JsonToDelphiClass/releases';
  ProgramUrl = 'https://github.com/PKGeorgiev/Delphi-JsonToDelphiClass';

function InternalCheckForUpdate: TObject;
procedure NewCheckForUpdateTask(AOnFinish: TProc<TObject>);

var
  PointDsFormatSettings: TFormatSettings;

implementation

uses
  Math;

function InternalCheckForUpdate: TObject;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LRelease, LResult: TObject;
  LJsonArray: TJsonArray;
  LJsonValue: TJsonValue;
  LTag: double;
begin
  LResult := nil;
  try
    LRestClient := TRESTClient.Create('');
    try
      LRestClient.BaseURL := UpdateUrl;
      LRestResponse := TRESTResponse.Create(nil);
      try
        LRestRequest := TRESTRequest.Create(nil);
        try
          LRestRequest.Client := LRestClient;
          LRestRequest.Response := LRestResponse;
          LRestRequest.Timeout := 10000;

          LRestRequest.Execute;

          if LRestResponse.StatusCode = 200 then
          begin
            LJsonArray := TJSONObject.ParseJSONValue(LRestResponse.Content) as TJsonArray;
            try
              for LJsonValue in LJsonArray do
              begin
                LRelease := TReleaseClass.FromJsonString(LJsonValue.ToJSON);
                LTag := StrToFloat((LRelease as TReleaseClass).tag_name, PointDsFormatSettings);
                if Math.CompareValue(LTag, ProgramVersion) = 1 then
                begin
                  LResult := LRelease;
                  break;
                end
                else
                  LRelease.Free;
              end;
            finally
              LJsonArray.Free;
            end;
          end
          else
            LResult := TErrorClass.FromJsonString(LRestResponse.Content);

        finally
          LRestRequest.Free;
        end;

      finally
        LRestResponse.Free;
      end;

    finally
      LRestClient.Free;
    end;

  except
    on e: Exception do
    begin
      LResult := TErrorClass.Create;
      (LResult as TErrorClass).message := e.message;
    end;
  end;

  result := LResult;
end;

procedure NewCheckForUpdateTask(AOnFinish: TProc<TObject>);
begin
  TTask.Run(
    procedure
    var
      LResult: TObject;
    begin
      // Asynchronously check for update
      LResult := InternalCheckForUpdate();
      try
        // Execute AOnFinish in the context of the Main Thread
        TThread.Synchronize(nil,
          procedure
          begin
            AOnFinish(LResult);
          end);
      except
      end;
    end);
end;

initialization

PointDsFormatSettings := TFormatSettings.Create();
PointDsFormatSettings.DecimalSeparator := '.';

end.
