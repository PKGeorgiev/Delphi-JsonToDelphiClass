unit DummyJson.Products.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ActnList,

  DummyJson.Lib.Enumerator, DummyJson.ProductsDTO;

type
  TMainForm = class(TForm)
    LabelBrand: TLabel;
    LabelCategory: TLabel;
    LabelDescription: TLabel;
    LabelDiscountPercentage: TLabel;
    LabelId: TLabel;
    LabelPrice: TLabel;
    LabelRating: TLabel;
    LabelStock: TLabel;
    LabelThumbnail: TLabel;
    LabelTitle: TLabel;
    EditBrand: TEdit;
    EditCategory: TEdit;
    EditDescription: TMemo;
    EditDiscountPercentage: TEdit;
    EditId: TEdit;
    EditPrice: TEdit;
    EditRating: TEdit;
    EditStock: TEdit;
    EditThumbnail: TEdit;
    EditTitle: TEdit;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    actNext: TAction;
    actPrevious: TAction;
    procedure actNextExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actPreviousExecute(Sender: TObject);
  private
    { Private declarations }
    FListeEumerator: TListeEumerator<TProduct>;
    procedure ListeEumeratorChanged(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  DummyJson.Lib.DTODownloader;

{$R *.dfm}

procedure TMainForm.actNextExecute(Sender: TObject);
begin
  FListeEumerator.Next;
end;

procedure TMainForm.actPreviousExecute(Sender: TObject);
begin
  FListeEumerator.Previous;
end;

procedure TMainForm.FormCreate(Sender: TObject);
const
  Url = 'https://dummyjson.com/products';
var
  ProductsDTO: TProductsDTO;
begin
  ProductsDTO := DTODownloader.GetDTO<TProductsDTO>(Url);
  FListeEumerator := TListeEumerator<TProduct>.Create(Self, ProductsDTO, ProductsDTO.Products);
  FListeEumerator.OnChange := ListeEumeratorChanged;
end;

procedure TMainForm.ListeEumeratorChanged(Sender: TObject);
var
  Product: TProduct;
begin
  Product := FListeEumerator.Current;
  if Product = nil then
    Exit;

  EditBrand.Text := Product.Brand;
  EditCategory.Text := Product.Category;
  EditDescription.Text := Product.Description;
  EditDiscountPercentage.Text := Product.DiscountPercentage.ToString;
  EditId.Text := Product.Id.ToString;
  EditPrice.Text := Product.Price.ToString;
  EditRating.Text := Product.Rating.ToString;
  EditStock.Text := Product.Stock.ToString;
  EditThumbnail.Text := Product.Thumbnail;
  EditTitle.Text := Product.Title;
end;

end.
