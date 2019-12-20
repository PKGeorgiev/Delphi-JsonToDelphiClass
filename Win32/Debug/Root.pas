unit Root;

interface

uses
  Pkg.Json.DTO;

{$M+}

type
  TUploaderDTO = class
  strict private
    FAvatar_Url: String;
    FEvents_Url: String;
    FFollowers_Url: String;
    FFollowing_Url: String;
    FGists_Url: String;
    FGravatar_Id: String;
    FHtml_Url: String;
    FId: Integer;
    FLogin: String;
    FOrganizations_Url: String;
    FReceived_Events_Url: String;
    FRepos_Url: String;
    FSite_Admin: Boolean;
    FStarred_Url: String;
    FSubscriptions_Url: String;
    FType: String;
    FUrl: String;
  published
    property Avatar_Url: String read FAvatar_Url write FAvatar_Url;
    property Events_Url: String read FEvents_Url write FEvents_Url;
    property Followers_Url: String read FFollowers_Url write FFollowers_Url;
    property Following_Url: String read FFollowing_Url write FFollowing_Url;
    property Gists_Url: String read FGists_Url write FGists_Url;
    property Gravatar_Id: String read FGravatar_Id write FGravatar_Id;
    property Html_Url: String read FHtml_Url write FHtml_Url;
    property Id: Integer read FId write FId;
    property Login: String read FLogin write FLogin;
    property Organizations_Url: String read FOrganizations_Url write FOrganizations_Url;
    property Received_Events_Url: String read FReceived_Events_Url write FReceived_Events_Url;
    property Repos_Url: String read FRepos_Url write FRepos_Url;
    property Site_Admin: Boolean read FSite_Admin write FSite_Admin;
    property Starred_Url: String read FStarred_Url write FStarred_Url;
    property Subscriptions_Url: String read FSubscriptions_Url write FSubscriptions_Url;
    property &Type: String read FType write FType;
    property Url: String read FUrl write FUrl;
  end;

  TAssetsDTO = class
  strict private
    FBrowser_Download_Url: String;
    FContent_Type: String;
    FCreated_At: String;
    FDownload_Count: Integer;
    FId: Integer;
    FName: String;
    FSize: Integer;
    FState: String;
    FUpdated_At: String;
    FUploader: TUploaderDTO;
    FUrl: String;
  published
    property Browser_Download_Url: String read FBrowser_Download_Url write FBrowser_Download_Url;
    property Content_Type: String read FContent_Type write FContent_Type;
    property Created_At: String read FCreated_At write FCreated_At;
    property Download_Count: Integer read FDownload_Count write FDownload_Count;
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property Size: Integer read FSize write FSize;
    property State: String read FState write FState;
    property Updated_At: String read FUpdated_At write FUpdated_At;
    property Uploader: TUploaderDTO read FUploader write FUploader;
    property Url: String read FUrl write FUrl;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAuthorDTO = class
  strict private
    FAvatar_Url: String;
    FEvents_Url: String;
    FFollowers_Url: String;
    FFollowing_Url: String;
    FGists_Url: String;
    FGravatar_Id: String;
    FHtml_Url: String;
    FId: Integer;
    FLogin: String;
    FOrganizations_Url: String;
    FReceived_Events_Url: String;
    FRepos_Url: String;
    FSite_Admin: Boolean;
    FStarred_Url: String;
    FSubscriptions_Url: String;
    FType: String;
    FUrl: String;
  published
    property Avatar_Url: String read FAvatar_Url write FAvatar_Url;
    property Events_Url: String read FEvents_Url write FEvents_Url;
    property Followers_Url: String read FFollowers_Url write FFollowers_Url;
    property Following_Url: String read FFollowing_Url write FFollowing_Url;
    property Gists_Url: String read FGists_Url write FGists_Url;
    property Gravatar_Id: String read FGravatar_Id write FGravatar_Id;
    property Html_Url: String read FHtml_Url write FHtml_Url;
    property Id: Integer read FId write FId;
    property Login: String read FLogin write FLogin;
    property Organizations_Url: String read FOrganizations_Url write FOrganizations_Url;
    property Received_Events_Url: String read FReceived_Events_Url write FReceived_Events_Url;
    property Repos_Url: String read FRepos_Url write FRepos_Url;
    property Site_Admin: Boolean read FSite_Admin write FSite_Admin;
    property Starred_Url: String read FStarred_Url write FStarred_Url;
    property Subscriptions_Url: String read FSubscriptions_Url write FSubscriptions_Url;
    property &Type: String read FType write FType;
    property Url: String read FUrl write FUrl;
  end;

  TItemDTO = class
  strict private
    FAssets: TArray<TAssetsDTO>;
    FAssets_Url: String;
    FAuthor: TAuthorDTO;
    FBody: String;
    FCreated_At: String;
    FDraft: Boolean;
    FHtml_Url: String;
    FId: Integer;
    FName: String;
    FPrerelease: Boolean;
    FPublished_At: String;
    FTag_Name: String;
    FTarball_Url: String;
    FTarget_Commitish: String;
    FUpload_Url: String;
    FUrl: String;
    FZipball_Url: String;
  published
    property Assets: TArray<TAssetsDTO> read FAssets write FAssets;
    property Assets_Url: String read FAssets_Url write FAssets_Url;
    property Author: TAuthorDTO read FAuthor write FAuthor;
    property Body: String read FBody write FBody;
    property Created_At: String read FCreated_At write FCreated_At;
    property Draft: Boolean read FDraft write FDraft;
    property Html_Url: String read FHtml_Url write FHtml_Url;
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property Prerelease: Boolean read FPrerelease write FPrerelease;
    property Published_At: String read FPublished_At write FPublished_At;
    property Tag_Name: String read FTag_Name write FTag_Name;
    property Tarball_Url: String read FTarball_Url write FTarball_Url;
    property Target_Commitish: String read FTarget_Commitish write FTarget_Commitish;
    property Upload_Url: String read FUpload_Url write FUpload_Url;
    property Url: String read FUrl write FUrl;
    property Zipball_Url: String read FZipball_Url write FZipball_Url;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRootDTO = class(TJsonDTO)
  strict private
    FItems: TArray<TItemDTO>;
  published
    property Items: TArray<TItemDTO> read FItems write FItems;
  public
    destructor Destroy; override;
  end;

implementation

{ TAssetsDTO }

constructor TAssetsDTO.Create;
begin
  inherited;
  FUploader := TUploaderDTO.Create;
end;

destructor TAssetsDTO.Destroy;
begin
  FUploader.Free;
  inherited;
end;

{ TItemDTO }

constructor TItemDTO.Create;
begin
  inherited;
  FAuthor := TAuthorDTO.Create;
end;

destructor TItemDTO.Destroy;
var
  AssetsItem: TAssetsDTO;
begin
  for AssetsItem in FAssets do
    AssetsItem.Free;

  FAuthor.Free;
  inherited;
end;

{ TRootDTO }

destructor TRootDTO.Destroy;
var
  ItemsItem: TItemDTO;
begin
  for ItemsItem in FItems do
    ItemsItem.Free;

  inherited;
end;

end.

