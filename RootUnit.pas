unit RootUnit;

interface

uses
  Generics.Collections, Rest.Json;

{$M+}

type

  TProductsDTO = class
  strict private
    FActivationDate: String;
    FCampaignPrice: Extended;
    FDiscountCode: String;
    FFrame: String;
    FGlassType: String;
    FInsurance: String;
    FLeftGlass: String;
    FOpusNr: String;
    FReason: String;
    FRightGlass: String;
    FSalesPrice: Extended;
    FState: String;
    FSubscriptionPrice: Extended;
  published
    property ActivationDate: String read FActivationDate write FActivationDate;
    property CampaignPrice: Extended read FCampaignPrice write FCampaignPrice;
    property DiscountCode: String read FDiscountCode write FDiscountCode;
    property Frame: String read FFrame write FFrame;
    property GlassType: String read FGlassType write FGlassType;
    property Insurance: String read FInsurance write FInsurance;
    property LeftGlass: String read FLeftGlass write FLeftGlass;
    property OpusNr: String read FOpusNr write FOpusNr;
    property Reason: String read FReason write FReason;
    property RightGlass: String read FRightGlass write FRightGlass;
    property SalesPrice: Extended read FSalesPrice write FSalesPrice;
    property State: String read FState write FState;
    property SubscriptionPrice: Extended read FSubscriptionPrice write FSubscriptionPrice;
  public
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TProductsDTO;
  end;

  TSubscriptionsDTO = class
  strict private
    FCountryCode: String;
    FCreatedDate: String;
    FCustomerId: String;
    FEarliestSubscriptionEndDate: String;
    FPOSUserId: String;
    FProducts: TArray<TProductsDTO>;
    FShopNumber: String;
    FSubscriptionId: String;
    FSubscriptionStartDate: String;
    FSubscriptionState: String;
  published
    property CountryCode: String read FCountryCode write FCountryCode;
    property CreatedDate: String read FCreatedDate write FCreatedDate;
    property CustomerId: String read FCustomerId write FCustomerId;
    property EarliestSubscriptionEndDate: String read FEarliestSubscriptionEndDate write FEarliestSubscriptionEndDate;
    property POSUserId: String read FPOSUserId write FPOSUserId;
    property Products: TArray<TProductsDTO> read FProducts write FProducts;
    property ShopNumber: String read FShopNumber write FShopNumber;
    property SubscriptionId: String read FSubscriptionId write FSubscriptionId;
    property SubscriptionStartDate: String read FSubscriptionStartDate write FSubscriptionStartDate;
    property SubscriptionState: String read FSubscriptionState write FSubscriptionState;
  public
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TSubscriptionsDTO;
  end;

  TRootDTO = class
  strict private
    FCode: Extended;
    FMessage: String;
    FSubscriptions: TArray<TSubscriptionsDTO>;
  private
    function GetAsJson: string;
    procedure SetAsJson(const Value: string);
  published
    property Code: Extended read FCode write FCode;
    property Message: String read FMessage write FMessage;
    property Subscriptions: TArray<TSubscriptionsDTO> read FSubscriptions write FSubscriptions;
  public
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TRootDTO;

    property AsJson: string read GetAsJson write SetAsJson;
  end;

implementation

uses
  System.Json;
{ TProductsDTO }

function TProductsDTO.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

class function TProductsDTO.FromJsonString(AJsonString: string): TProductsDTO;
begin
  Result := TJson.JsonToObject<TProductsDTO>(AJsonString)
end;

{ TSubscriptionsDTO }

destructor TSubscriptionsDTO.Destroy;
var
  LProductsItem: TProductsDTO;
begin

  for LProductsItem in FProducts do
    LProductsItem.Free;

  inherited;
end;

function TSubscriptionsDTO.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

class function TSubscriptionsDTO.FromJsonString(AJsonString: string): TSubscriptionsDTO;
begin
  Result := TJson.JsonToObject<TSubscriptionsDTO>(AJsonString)
end;

{ TRootDTO }

destructor TRootDTO.Destroy;
var
  LSubscriptionsItem: TSubscriptionsDTO;
begin

  for LSubscriptionsItem in FSubscriptions do
    LSubscriptionsItem.Free;

  inherited;
end;

function TRootDTO.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

class function TRootDTO.FromJsonString(AJsonString: string): TRootDTO;
begin
  Result := TJson.JsonToObject<TRootDTO>(AJsonString)
end;

function TRootDTO.GetAsJson: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

procedure TRootDTO.SetAsJson(const Value: string);
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := TJSONObject.ParseJSONValue(Value);
  try
    TJson.JsonToObject(Self, LJSONValue);
  finally
    LJSONValue.Free;
  end;
end;

end.
