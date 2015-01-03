unit uUpdate;

interface
uses REST.Client, uGitHub, REST.JSON, JSON,
  IPPeerClient, SysUtils, System.Threading, Classes, Pkg.Json.Mapper;

const
  ProgramVersion : double = 0.64;
  UpdateUrl = 'https://api.github.com/repos/PKGeorgiev/Delphi-JsonToDelphiClass/releases';
  ProgramUrl = 'https://github.com/PKGeorgiev/Delphi-JsonToDelphiClass';

function CheckForUpdate(AOnFinish: TProc<TObject>): ITask;

implementation
uses Math;

function CheckForUpdate(AOnFinish: TProc<TObject>): ITask;
begin

  result := TTask.Run(
    procedure
    var
      LRestClient: TRESTClient;
      LRestRequest: TRESTRequest;
      LRestResponse: TRESTResponse;
      LRelease,
      LResult: TObject;
      LJsonArray: TJsonArray;
      LJsonValue: TJsonValue;
      LTag: double;
    begin
      LResult := nil;
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
            try
              try

                LRestRequest.Execute;

                if LRestResponse.StatusCode = 200 then
                begin
                  LJsonArray := TJSONObject.ParseJSONValue(LRestResponse.Content) as TJSONArray;
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

              except
                on e: Exception do
                begin
                  LResult := TErrorClass.Create;
                  (LResult as TErrorClass).message := e.Message;
                end;
              end;

            finally
              TThread.Synchronize(nil,
                procedure
                begin
                  AOnFinish(LResult);
                end
              );
            end;

          finally
            LRestRequest.Free;
          end;

        finally
          LRestResponse.Free;
        end;

      finally
        LRestClient.Free;
      end;

    end);

end;

end.
