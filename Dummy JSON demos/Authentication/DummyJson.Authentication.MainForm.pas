unit DummyJson.Authentication.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.ComCtrls,

  DummyJson.UsersDTO, DummyJson.ProductsDTO, DummyJson.Lib.Enumerator;

type
  TMainForm = class(TForm)
    lblAge: TLabel;
    edtAge: TEdit;
    lblBirthDate: TLabel;
    edtBirthDate: TEdit;
    lblBloodGroup: TLabel;
    edtBloodGroup: TEdit;
    lblDomain: TLabel;
    edtDomain: TEdit;
    lblEin: TLabel;
    edtEin: TEdit;
    lblEmail: TLabel;
    edtEmail: TEdit;
    lblEyeColor: TLabel;
    edtEyeColor: TEdit;
    lblFirstName: TLabel;
    edtFirstName: TEdit;
    lblGender: TLabel;
    edtGender: TEdit;
    lblHair: TLabel;
    edtHair: TEdit;
    lblHeight: TLabel;
    edtHeight: TEdit;
    lblId: TLabel;
    edtId: TEdit;
    lblImage: TLabel;
    edtImage: TEdit;
    lblIp: TLabel;
    edtIp: TEdit;
    lblLastName: TLabel;
    edtLastName: TEdit;
    lblMacAddress: TLabel;
    edtMacAddress: TEdit;
    lblMaidenName: TLabel;
    edtMaidenName: TEdit;
    lblPhone: TLabel;
    edtPhone: TEdit;
    lblSsn: TLabel;
    edtSsn: TEdit;
    lblUniversity: TLabel;
    edtUniversity: TEdit;
    lblUserAgent: TLabel;
    edtUserAgent: TEdit;
    lblWeight: TLabel;
    edtWeight: TEdit;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    acUsertNext: TAction;
    actUserPrevious: TAction;
    LabelEin: TLabel;
    LabelHair: TLabel;
    LabelId: TLabel;
    LabelIp: TLabel;
    LabelSsn: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    EditStock: TEdit;
    Button3: TButton;
    Button4: TButton;
    EditTitle: TEdit;
    EditThumbnail: TEdit;
    EditRating: TEdit;
    EditPrice: TEdit;
    EditId: TEdit;
    EditDiscountPercentage: TEdit;
    EditDescription: TMemo;
    EditCategory: TEdit;
    EditBrand: TEdit;
    LabelTitle: TLabel;
    LabelThumbnail: TLabel;
    LabelStock: TLabel;
    LabelRating: TLabel;
    LabelPrice: TLabel;
    Label1: TLabel;
    LabelDiscountPercentage: TLabel;
    LabelDescription: TLabel;
    LabelCategory: TLabel;
    LabelBrand: TLabel;
    actProductNext: TAction;
    actProductPrevious: TAction;
    procedure FormCreate(Sender: TObject);
    procedure acUsertNextExecute(Sender: TObject);
    procedure actUserPreviousExecute(Sender: TObject);
    procedure actProductNextExecute(Sender: TObject);
    procedure actProductPreviousExecute(Sender: TObject);
  private
    { Private declarations }
    FUserEumerator: TListeEumerator<TUser>;
    FProductsEumerator: TListeEumerator<TProduct>;

    procedure UserEumeratorChanged(Sender: TObject);
    procedure ProductsEumeratorChanged(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  DummyJson.AuthenticaticedUsers, DummyJson.Lib.AuthWebService;

{$R *.dfm}

procedure TMainForm.acUsertNextExecute(Sender: TObject);
begin
  FUserEumerator.Next;
end;

procedure TMainForm.actProductNextExecute(Sender: TObject);
begin
  FProductsEumerator.Next;
end;

procedure TMainForm.actProductPreviousExecute(Sender: TObject);
begin
  FProductsEumerator.Previous;
end;

procedure TMainForm.actUserPreviousExecute(Sender: TObject);
begin
  FUserEumerator.Previous;
end;

procedure TMainForm.FormCreate(Sender: TObject);
const
  Url = 'https://dummyjson.com/users';
var
  UsersDTO: TUsersDTO;
begin
  UsersDTO := TWebService<TUsersDTO>.GetDTO(Url);
  FUserEumerator := TListeEumerator<TUser>.Create(Self, UsersDTO, UsersDTO.Users);
  FUserEumerator.OnChange := UserEumeratorChanged;
end;

procedure TMainForm.ProductsEumeratorChanged(Sender: TObject);
var
  Product: TProduct;
begin
  Product := FProductsEumerator.Current;
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

procedure TMainForm.UserEumeratorChanged(Sender: TObject);
const
  Url = 'https://dummyjson.com/auth/products';
var
  User: TUser;
  ProductsDTO: TProductsDTO;
  Token: string;
begin
  User := FUserEumerator.Current;
  if User = nil then
    Exit;

  edtAge.Text := User.Age.ToString;
  edtBirthDate.Text := DateToStr(User.BirthDate);
  edtBloodGroup.Text := User.BloodGroup;
  edtDomain.Text := User.Domain;
  edtEin.Text := User.Ein;
  edtEmail.Text := User.Email;
  edtEyeColor.Text := User.EyeColor;
  edtFirstName.Text := User.FirstName;
  edtGender.Text := User.Gender;
  edtHair.Text := User.Hair.Color;
  edtHeight.Text := User.Height.ToString;
  edtId.Text := User.Id.ToString;;
  edtImage.Text := User.Image;
  edtIp.Text := User.Ip;
  edtLastName.Text := User.LastName;
  edtMacAddress.Text := User.MacAddress;
  edtMaidenName.Text := User.MaidenName;
  edtPhone.Text := User.Phone;
  edtSsn.Text := User.Ssn;
  edtUniversity.Text := User.University;
  edtUserAgent.Text := User.UserAgent;
  edtWeight.Text := User.Weight.ToString;;

  FreeAndNil(FProductsEumerator);

  Token := AuthenticaticedUsers.AuthenticateUser(User).Token;
  ProductsDTO := TWebService<TProductsDTO>.GetDTO(Url, Token);

  FProductsEumerator := TListeEumerator<TProduct>.Create(Self, ProductsDTO, ProductsDTO.Products);
  FProductsEumerator.OnChange := ProductsEumeratorChanged;
end;

end.
