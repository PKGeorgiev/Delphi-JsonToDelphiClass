unit DummyJson.AuthenticaticedUsers;

interface

uses
  DummyJson.UsersDTO, System.Generics.Collections;

Type
  TAuthenticaticedUsers = class
  private
    FAutenticatedUsers: TObjectDictionary<TUser, TUserAuthenticationDTO>;
  public
    constructor Create;
    Destructor Destroy; override;
    function AuthenticateUser(aUser: TUser): TUserAuthenticationDTO;
    procedure Clear;
  end;

var
  AuthenticaticedUsers: TAuthenticaticedUsers;

implementation

uses
  DummyJson.Lib.AuthWebService;

{ TAuthenticaticedUsers }

function TAuthenticaticedUsers.AuthenticateUser(aUser: TUser): TUserAuthenticationDTO;
begin
  if FAutenticatedUsers.TryGetValue(aUser, Result) then
    exit;

  with TUserAuthWebService<TUserAuthenticationDTO>.Create('https://dummyjson.com/auth/login', aUser.Username, aUser.Password) do
    try
      Result := Post;
      FAutenticatedUsers.Add(aUser, Result);
    finally
      Free;
    end;
end;

procedure TAuthenticaticedUsers.Clear;
begin
  FAutenticatedUsers.Clear;
end;

constructor TAuthenticaticedUsers.Create;
begin
  inherited;
  FAutenticatedUsers := TObjectDictionary<TUser, TUserAuthenticationDTO>.Create([doOwnsValues]);
end;

destructor TAuthenticaticedUsers.Destroy;
begin
  FAutenticatedUsers.Free;
  inherited;
end;

initialization

AuthenticaticedUsers := TAuthenticaticedUsers.Create;

finalization

AuthenticaticedUsers.Free;

end.
