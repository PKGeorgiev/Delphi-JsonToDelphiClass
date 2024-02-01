unit DummyJson.ProductsDTO;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  TProduct = class(TJsonDTO)
  private
    FBrand: string;
    FCategory: string;
    FDescription: string;
    FDiscountPercentage: Double;
    FId: Integer;
    [JSONName('images')]
    FImagesArray: TArray<string>;
    [JSONMarshalled(False)]
    FImages: TList<string>;
    FPrice: Integer;
    FRating: Double;
    FStock: Integer;
    FThumbnail: string;
    FTitle: string;
    function GetImages: TList<string>;
  protected
    function GetAsJson: string; override;
  published
    property Brand: string read FBrand write FBrand;
    property Category: string read FCategory write FCategory;
    property Description: string read FDescription write FDescription;
    property DiscountPercentage: Double read FDiscountPercentage write FDiscountPercentage;
    property Id: Integer read FId write FId;
    property Images: TList<string> read GetImages;
    property Price: Integer read FPrice write FPrice;
    property Rating: Double read FRating write FRating;
    property Stock: Integer read FStock write FStock;
    property Thumbnail: string read FThumbnail write FThumbnail;
    property Title: string read FTitle write FTitle;
  public
    destructor Destroy; override;
  end;

  TProductsDTO = class(TJsonDTO)
  private
    FLimit: Integer;
    [JSONName('products'), JSONMarshalled(False)]
    FProductsArray: TArray<TProduct>;
    [GenericListReflect]
    FProducts: TObjectList<TProduct>;
    FSkip: Integer;
    FTotal: Integer;
    function GetProducts: TObjectList<TProduct>;
  protected
    function GetAsJson: string; override;
  published
    property Limit: Integer read FLimit write FLimit;
    property Products: TObjectList<TProduct> read GetProducts;
    property Skip: Integer read FSkip write FSkip;
    property Total: Integer read FTotal write FTotal;
  public
    destructor Destroy; override;
  end;

implementation

{ TProducts }

destructor TProduct.Destroy;
begin
  GetImages.Free;
  inherited;
end;

function TProduct.GetImages: TList<string>;
begin
  Result := List<string>(FImages, FImagesArray);
end;

function TProduct.GetAsJson: string;
begin
  RefreshArray<string>(FImages, FImagesArray);
  Result := inherited;
end;

{ TRoot }

destructor TProductsDTO.Destroy;
begin
  GetProducts.Free;
  inherited;
end;

function TProductsDTO.GetProducts: TObjectList<TProduct>;
begin
  Result := ObjectList<TProduct>(FProducts, FProductsArray);
end;

function TProductsDTO.GetAsJson: string;
begin
  RefreshArray<TProduct>(FProducts, FProductsArray);
  Result := inherited;
end;

end.
