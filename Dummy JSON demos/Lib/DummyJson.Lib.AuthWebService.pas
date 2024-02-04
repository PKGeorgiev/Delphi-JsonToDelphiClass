unit DummyJson.Lib.AuthWebService;

interface

uses
  Pkg.Json.DTO, Rest.Types, Rest.Client;

{$M+}

const
  HTTP_OK = 200;
  CONTENT_TYPE = 'Content-Type';
  HEADER_PARAM_ACCEPT = 'Accept';
  HEADER_PARAM_AUTHORIZATION = 'Authorization';
  HEADER_PARAM_BEARER = 'Bearer ';

type
  TWebService<T: TJsonDTO, constructor> = class abstract
  private
    FClient: TRESTClient;
    FRequest: TRESTRequest;
    FStatusText: string;
    FStatusCode: Integer;
    procedure SetBaseURL(const Value: string);
    function GetBaseURL: string;
  protected
    procedure AddHeaderParam(const aKey, aValue: string);
    procedure AddBody(aBody: string; aAddCoontentLength: Boolean = True);
    function Execute: T; virtual;
  published
    property StatusCode: Integer read FStatusCode;
    property StatusText: string read FStatusText;
    property BaseURL: string read GetBaseURL write SetBaseURL;
  public
    constructor Create(aBaseUrl: string = ''; aAuthenticationToken: string = ''); reintroduce;
    destructor Destroy; override;
    function Post: T; virtual;
    function Get: T; virtual;
    class function GetDTO(aBaseUrl: string = ''): T;
  end;

  TUserAuthWebService<T: TJsonDTO, constructor> = class sealed(TWebService<T>)
  strict private
  Type
    TInternalUser = class(TJsonDTO)
    private
      FPassword: string;
      FUsername: string;
    public
      property Username: string read FUsername write FUsername;
      property Password: string read FPassword write FPassword;
      constructor Create(aUserName: string; aPassword: string); reintroduce;
    end;

  public
    constructor Create(aUrl: string; aUserName: string; aPassword: string); reintroduce;
  end;

implementation

uses
  System.Sysutils;

{ TWebService<T> }

procedure TWebService<T>.AddBody(aBody: string; aAddCoontentLength: Boolean);
begin
  if aAddCoontentLength then
    FRequest.Params.AddItem('Content-Length', (aBody.Length).ToString, TRESTRequestParameterKind.pkHTTPHEADER);

  FRequest.AddBody(aBody, TRESTContentType.ctAPPLICATION_JSON);
end;

procedure TWebService<T>.AddHeaderParam(const aKey, aValue: string);
begin
  FRequest.AddParameter(aKey, aValue, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

(*
  Documentation from dummyjson:
  fetch('https://dummyjson.com/auth/products',
  {
  method: 'GET',
  headers:
  {
  'Authorization': 'Bearer /* YOUR_TOKEN_HERE */',
  },
  })
  .then(res => res.json())
  .then(console.log);
*)

constructor TWebService<T>.Create(aBaseUrl: string; aAuthenticationToken: string);
begin
  inherited Create;
  FClient := TRESTClient.Create(aBaseUrl);
  FRequest := TRESTRequest.Create(FClient);
  FRequest.Timeout := 10 * MSecsPerSec * SecsPerMin;
  AddHeaderParam(CONTENT_TYPE, CONTENTTYPE_APPLICATION_JSON);
  AddHeaderParam(HEADER_PARAM_ACCEPT, CONTENTTYPE_APPLICATION_JSON);
  if aAuthenticationToken <> '' then
    AddHeaderParam(HEADER_PARAM_AUTHORIZATION, HEADER_PARAM_BEARER  + aAuthenticationToken);
end;

destructor TWebService<T>.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TWebService<T>.Execute: T;
begin
  Result := nil;
  FStatusCode := -1;
  FStatusText := '';

  try
    FRequest.Execute;
    FStatusCode := FRequest.Response.StatusCode;
    FStatusText := FRequest.Response.StatusText;

    if FStatusCode = HTTP_OK then
    begin
      Result := T.Create;
      Result.AsJson := FRequest.Response.JSONText;
    end;

  except
    on e: Exception do
    begin
      FStatusCode := FRequest.Response.StatusCode;
      FStatusText := e.Message;
    end;
  end;
end;

function TWebService<T>.Get: T;
begin
  FRequest.Method := TRESTRequestMethod.rmGET;
  Result := Execute;
end;

function TWebService<T>.GetBaseURL: string;
begin
  Result := FClient.BaseURL;
end;

class function TWebService<T>.GetDTO(aBaseUrl: string): T;
begin
  with TWebService<T>.Create(aBaseUrl) do
    try
      Result := Get;
    finally
      Free;
    end;
end;

function TWebService<T>.Post: T;
begin
  FRequest.Method := TRESTRequestMethod.rmPOST;
  Result := Execute;
end;

procedure TWebService<T>.SetBaseURL(const Value: string);
begin
  FClient.BaseURL := Value;
end;

{ TUserAuthWebService<T> }

constructor TUserAuthWebService<T>.Create(aUrl, aUserName, aPassword: string);
begin
  inherited Create(aUrl);
  with TInternalUser.Create(aUserName, aPassword) do
    try
      AddBody(AsJson);
    finally
      Free;
    end;
end;

{ TUserAuthWebService<T>.TInternalUser }

constructor TUserAuthWebService<T>.TInternalUser.Create(aUserName, aPassword: string);
begin
  inherited Create;
  FPassword := aPassword;
  FUsername := aUserName;
end;

end.
