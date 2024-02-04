unit DummyJson.UsersDTO;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  TAddress = class;
  TBank = class;
  TCompany = class;
  TCoordinates = class;
  TCrypto = class;
  THair = class;

  TCrypto = class
  private
    FCoin: string;
    FNetwork: string;
    FWallet: string;
  published
    property Coin: string read FCoin write FCoin;
    property Network: string read FNetwork write FNetwork;
    property Wallet: string read FWallet write FWallet;
  end;

  TCoordinates = class
  private
    FLat: Double;
    FLng: Double;
  published
    property Lat: Double read FLat write FLat;
    property Lng: Double read FLng write FLng;
  end;

  TAddress = class
  private
    FAddress: string;
    FCity: string;
    FCoordinates: TCoordinates;
    FPostalCode: string;
    FState: string;
  published
    property Address: string read FAddress write FAddress;
    property City: string read FCity write FCity;
    property Coordinates: TCoordinates read FCoordinates;
    property PostalCode: string read FPostalCode write FPostalCode;
    property State: string read FState write FState;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCompany = class
  private
    FAddress: TAddress;
    FDepartment: string;
    FName: string;
    FTitle: string;
  published
    property Address: TAddress read FAddress;
    property Department: string read FDepartment write FDepartment;
    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TBank = class
  private
    FCardExpire: string;
    FCardNumber: string;
    FCardType: string;
    FCurrency: string;
    FIban: string;
  published
    property CardExpire: string read FCardExpire write FCardExpire;
    property CardNumber: string read FCardNumber write FCardNumber;
    property CardType: string read FCardType write FCardType;
    property Currency: string read FCurrency write FCurrency;
    property Iban: string read FIban write FIban;
  end;

  THair = class
  private
    FColor: string;
    FType: string;
  published
    property Color: string read FColor write FColor;
    property &Type: string read FType write FType;
  end;

  TUser = class
  private
    FAddress: TAddress;
    FAge: Integer;
    FBank: TBank;
    [SuppressZero]
    FBirthDate: TDateTime;
    FBloodGroup: string;
    FCompany: TCompany;
    FCrypto: TCrypto;
    FDomain: string;
    FEin: string;
    FEmail: string;
    FEyeColor: string;
    FFirstName: string;
    FGender: string;
    FHair: THair;
    FHeight: Integer;
    FId: Integer;
    FImage: string;
    FIp: string;
    FLastName: string;
    FMacAddress: string;
    FMaidenName: string;
    FPassword: string;
    FPhone: string;
    FSsn: string;
    FUniversity: string;
    FUserAgent: string;
    FUsername: string;
    FWeight: Integer;
  published
    property Address: TAddress read FAddress;
    property Age: Integer read FAge write FAge;
    property Bank: TBank read FBank;
    property BirthDate: TDateTime read FBirthDate write FBirthDate;
    property BloodGroup: string read FBloodGroup write FBloodGroup;
    property Company: TCompany read FCompany;
    property Crypto: TCrypto read FCrypto;
    property Domain: string read FDomain write FDomain;
    property Ein: string read FEin write FEin;
    property Email: string read FEmail write FEmail;
    property EyeColor: string read FEyeColor write FEyeColor;
    property FirstName: string read FFirstName write FFirstName;
    property Gender: string read FGender write FGender;
    property Hair: THair read FHair;
    property Height: Integer read FHeight write FHeight;
    property Id: Integer read FId write FId;
    property Image: string read FImage write FImage;
    property Ip: string read FIp write FIp;
    property LastName: string read FLastName write FLastName;
    property MacAddress: string read FMacAddress write FMacAddress;
    property MaidenName: string read FMaidenName write FMaidenName;
    property Password: string read FPassword write FPassword;
    property Phone: string read FPhone write FPhone;
    property Ssn: string read FSsn write FSsn;
    property University: string read FUniversity write FUniversity;
    property UserAgent: string read FUserAgent write FUserAgent;
    property Username: string read FUsername write FUsername;
    property Weight: Integer read FWeight write FWeight;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TUsersDTO = class(TJsonDTO)
  private
    FLimit: Integer;
    FSkip: Integer;
    FTotal: Integer;
    [JSONName('users'), JSONMarshalled(False)]
    FUsersArray: TArray<TUser>;
    [GenericListReflect]
    FUsers: TObjectList<TUser>;
    function GetUsers: TObjectList<TUser>;
  protected
    function GetAsJson: string; override;
  published
    property Limit: Integer read FLimit write FLimit;
    property Skip: Integer read FSkip write FSkip;
    property Total: Integer read FTotal write FTotal;
    property Users: TObjectList<TUser> read GetUsers;
  public
    destructor Destroy; override;
  end;

  TUserAuthenticationDTO = class(TJsonDTO)
  private
    FEmail: string;
    FFirstName: string;
    FGender: string;
    FId: Integer;
    FImage: string;
    FLastName: string;
    FToken: string;
    FUsername: string;
  published
    property Email: string read FEmail write FEmail;
    property FirstName: string read FFirstName write FFirstName;
    property Gender: string read FGender write FGender;
    property Id: Integer read FId write FId;
    property Image: string read FImage write FImage;
    property LastName: string read FLastName write FLastName;
    property Token: string read FToken write FToken;
    property Username: string read FUsername write FUsername;
  end;


implementation

{ TAddress }

constructor TAddress.Create;
begin
  inherited;
  FCoordinates := TCoordinates.Create;
end;

destructor TAddress.Destroy;
begin
  FCoordinates.Free;
  inherited;
end;

{ TCompany }

constructor TCompany.Create;
begin
  inherited;
  FAddress := TAddress.Create;
end;

destructor TCompany.Destroy;
begin
  FAddress.Free;
  inherited;
end;

{ TUsers }

constructor TUser.Create;
begin
  inherited;
  FAddress := TAddress.Create;
  FHair := THair.Create;
  FBank := TBank.Create;
  FCompany := TCompany.Create;
  FCrypto := TCrypto.Create;
end;

destructor TUser.Destroy;
begin
  FAddress.Free;
  FHair.Free;
  FBank.Free;
  FCompany.Free;
  FCrypto.Free;
  inherited;
end;

{ TRoot }

destructor TUsersDTO.Destroy;
begin
  GetUsers.Free;
  inherited;
end;

function TUsersDTO.GetUsers: TObjectList<TUser>;
begin
  Result := ObjectList<TUser>(FUsers, FUsersArray);
end;

function TUsersDTO.GetAsJson: string;
begin
  RefreshArray<TUser>(FUsers, FUsersArray);
  Result := inherited;
end;

end.
