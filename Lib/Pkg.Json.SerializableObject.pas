unit Pkg.Json.SerializableObject;

interface

uses Generics.Collections, Rest.Json, IdUri, IdHttp,
  IdSSLOpenSSL, System.Json, SysUtils, Classes, Pkg.Json.DTO;

type
  /// Represents a serializable object with HTTP/REST capabilities (via Indy)
  /// HTTPS connections require OpenSSL binaries!
  /// Use the "AOnBeforeRequest" event to setup HTTP client's parameters like timeout, encoding etc.
  TUGitHubSerializableObject = class
  protected
    // As per http://www.restapitutorial.com/lessons/httpmethods.html
    class procedure EnsureHttpResponseCode(AHttpResponseCode: integer; aUrl: string; AValidValues: array of integer);
    class procedure EnsureHttpContentType(AHttp: TIdHttp);
    class procedure DoOnError<T: TJsonDTO>(var aObject: T; aErrorProc: TProc<string>; aMessage: string);
  public
    // Generic Web Request method
    class function WebRequest(aUrl: string; AOnRequest: TProc<TIdHttp>): integer;
    // Returns an instance of T from a JSON string via GET request. AArrayProperty is intended for internal use only!
    // HttpGet is reintroduced in descendant classes to return concrete instance
    class function HttpGet<T: TJsonDTO, constructor>(aUrl: string; AOnBeforeRequest: TProc<TIdHttp> = nil): T;
    class function RestRequest<T: TJsonDTO, constructor>(aUrl: string; aErrorProc: TProc<string> = nil): T;
    // Performs POST request, sends the current object as JSON string and returns server's response as text.
    function HttpPost(aUrl: string; AOnBeforeRequest: TProc<TIdHttp> = nil): string;
    // Performs PUT request, sends the current object as JSON string and returns server's response as text.
    function HttpPut(aUrl: string; AOnBeforeRequest: TProc<TIdHttp> = nil): string;
    // Performs DELETE request and returns server's response as text. This method exists just REST compliance.
    function HttpDelete(aUrl: string; AOnBeforeRequest: TProc<TIdHttp> = nil): string;
  end;

implementation

uses
  Rest.Client;
{ TUGitHubSerializableObject }

class procedure TUGitHubSerializableObject.DoOnError<T>(var aObject: T; aErrorProc: TProc<string>; aMessage: string);
begin
  FreeAndNil(aObject);
  if Assigned(aErrorProc) then
    aErrorProc(aMessage);
end;

class procedure TUGitHubSerializableObject.EnsureHttpContentType(AHttp: TIdHttp);
begin
  if AHttp.Response.ContentType <> 'application/json' then
    raise Exception.CreateFmt('Invalid content type %s!', [AHttp.Response.ContentType]);
end;

class procedure TUGitHubSerializableObject.EnsureHttpResponseCode(AHttpResponseCode: integer; aUrl: string; AValidValues: array of integer);
var
  LValue: integer;
begin
  for LValue in AValidValues do
    if LValue = AHttpResponseCode then
      exit;

  raise Exception.CreateFmt('The request to %s has failed with code %d', [aUrl, AHttpResponseCode]);
end;

function TUGitHubSerializableObject.HttpDelete(aUrl: string; AOnBeforeRequest: TProc<TIdHttp>): string;
var
  LResult: string;
begin
  WebRequest(aUrl,
      procedure(LHttp: TIdHttp)
    begin

      // Allow HTTP client pre-configuration
      if Assigned(AOnBeforeRequest) then
        AOnBeforeRequest(LHttp);

      LResult := LHttp.Delete(aUrl);
      EnsureHttpResponseCode(LHttp.ResponseCode, aUrl, [200, 204]);
    end);

  Result := LResult;
end;

class function TUGitHubSerializableObject.RestRequest<T>(aUrl: string; aErrorProc: TProc<string> = nil): T;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LResult: T;
begin
  LResult := nil;

  LRestClient := TRESTClient.Create('');
  LRestResponse := TRESTResponse.Create(nil);
  LRestRequest := TRESTRequest.Create(nil);
  try
    LRestClient.BaseURL := aUrl;

    LRestRequest.Client := LRestClient;
    LRestRequest.Response := LRestResponse;
    LRestRequest.Timeout := 10000;

    try
      LRestRequest.Execute;

      if LRestResponse.StatusCode = 200 then
      begin
        try
          LResult := T.Create;
          LResult.AsJson := LRestResponse.Content;
        except
          on e: Exception do
            DoOnError(LResult, aErrorProc, e.message);
        end;
      end
      else
        DoOnError(LResult, aErrorProc, LRestResponse.Content);
    except
      on e: Exception do
        DoOnError(LResult, aErrorProc, e.message);
    end;

  finally
    LRestResponse.Free;
    LRestRequest.Free;
    LRestClient.Free;
  end;

  Result := LResult;
end;

class function TUGitHubSerializableObject.HttpGet<T>(aUrl: string; AOnBeforeRequest: TProc<TIdHttp>): T;
var
  LResult: T;
begin
  WebRequest(aUrl,
    procedure(LHttp: TIdHttp)
    var
      LString: string;
      LJsonValue: TJsonValue;
      LJsonObject: TJSONObject;
    begin

      try // Allow HTTP client pre-configuration
        if Assigned(AOnBeforeRequest) then
          AOnBeforeRequest(LHttp);

        LString := LHttp.Get(aUrl);

        EnsureHttpResponseCode(LHttp.ResponseCode, aUrl, [200, 304]);
        EnsureHttpContentType(LHttp);
        LResult := T.Create;
        LResult.AsJson := LString;
      except
        FreeAndNil(LResult);
      end;

    end);

  Result := LResult;
end;

function TUGitHubSerializableObject.HttpPost(aUrl: string; AOnBeforeRequest: TProc<TIdHttp>): string;
var
  LResult: string;
begin

  WebRequest(aUrl,
    procedure(LHttp: TIdHttp)
    var
      LStringStream: TStringStream;
    begin

      // Allow HTTP client pre-configuration
      if Assigned(AOnBeforeRequest) then
        AOnBeforeRequest(LHttp);

      LResult := TJson.ObjectToJsonString(Self);

      LStringStream := TStringStream.Create(LResult, TEncoding.GetEncoding(LHttp.Request.ContentEncoding));
      try
        LResult := LHttp.Post(aUrl, LStringStream);
        EnsureHttpResponseCode(LHttp.ResponseCode, aUrl, [200, 201, 202, 204]);
        EnsureHttpContentType(LHttp);
      finally
        LStringStream.Free;
      end;

    end);

  Result := LResult;
end;

function TUGitHubSerializableObject.HttpPut(aUrl: string; AOnBeforeRequest: TProc<TIdHttp>): string;
var
  LResult: string;
begin

  WebRequest(aUrl,
    procedure(LHttp: TIdHttp)
    var
      LStringStream: TStringStream;
    begin

      // Allow HTTP client pre-configuration
      if Assigned(AOnBeforeRequest) then
        AOnBeforeRequest(LHttp);

      LResult := TJson.ObjectToJsonString(Self);

      LStringStream := TStringStream.Create(LResult, TEncoding.GetEncoding(LHttp.Request.ContentEncoding));
      try
        LResult := LHttp.Put(aUrl, LStringStream);
        EnsureHttpResponseCode(LHttp.ResponseCode, aUrl, [200, 204]);
        EnsureHttpContentType(LHttp);
      finally
        LStringStream.Free;
      end;

    end);

  Result := LResult;
end;

class function TUGitHubSerializableObject.WebRequest(aUrl: string; AOnRequest: TProc<TIdHttp>): integer;
var
  LUri: TIdUri;
  LHttp: TIdHttp;
  LSslIoHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  LUri := TIdUri.Create(aUrl);
  try
    LHttp := TIdHttp.Create;
    try
      LHttp.HandleRedirects := true;
      // Default encoding
      LHttp.Request.ContentEncoding := 'utf-8';
      // Specify Content-Type header
      LHttp.Request.ContentType := 'application/json';

      // Replace default IOHandler with TIdSSLIOHandlerSocketOpenSSL if the connection is SSL based
      if LUri.Protocol.ToLower = 'https' then
      begin
        LSslIoHandler := TIdSSLIOHandlerSocketOpenSSL.Create(LHttp);
        LHttp.IOHandler := LSslIoHandler;
      end;

      try
        AOnRequest(LHttp);
      finally
        Result := LHttp.ResponseCode;
      end;

    finally
      LHttp.Free;
    end;
  finally
    LUri.Free;
  end;
end;

end.
