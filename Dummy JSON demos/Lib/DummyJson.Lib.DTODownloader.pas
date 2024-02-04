unit DummyJson.Lib.DTODownloader;

interface

uses
  System.SysUtils,
  Pkg.Json.DTO;

type
  DTODownloader = class abstract
  public
    class function GetDTO<T: TJsonDTO, constructor>(const aUrl: string; var aErrorText: string): T; overload;
    class function GetDTO<T: TJsonDTO, constructor>(const aUrl: string): T; overload;
  end;

const
  HTTP_OK = 200;

implementation

uses
  System.Net.HttpClient, System.Net.HttpClientComponent;

{ DTODownloader }

class function DTODownloader.GetDTO<T>(const aUrl: string; var aErrorText: string): T;
var
  DTO: T;
  Respons: IHTTPResponse;
begin
  DTO := nil;
  with TNetHTTPClient.Create(nil) do
    try
      try
        Respons := Get(aUrl);

        if Respons.StatusCode = HTTP_OK then
        begin
          DTO := T.Create;
          DTO.AsJson := Respons.ContentAsString;
        end
        else
          Exit;

        aErrorText := '';
      except
        on e: Exception do
          aErrorText := e.message;
      end;
    finally
      Result := DTO;
      Free;
    end;
end;

class function DTODownloader.GetDTO<T>(const aUrl: string): T;
var
  ErrorText: string;
begin
  Result := GetDTO<T>(aUrl, ErrorText);
end;

end.
