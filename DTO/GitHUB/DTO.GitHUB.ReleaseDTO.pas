unit DTO.GitHUB.ReleaseDTO;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  TUploaderDTO = class
  private
    FAvatar_Url: string;
    FEvents_Url: string;
    FFollowers_Url: string;
    FFollowing_Url: string;
    FGists_Url: string;
    FGravatar_Id: string;
    FHtml_Url: string;
    FId: Integer;
    FLogin: string;
    FNode_Id: string;
    FOrganizations_Url: string;
    FReceived_Events_Url: string;
    FRepos_Url: string;
    FSite_Admin: Boolean;
    FStarred_Url: string;
    FSubscriptions_Url: string;
    FType: string;
    FUrl: string;
  published
    property Avatar_Url: string read FAvatar_Url write FAvatar_Url;
    property Events_Url: string read FEvents_Url write FEvents_Url;
    property Followers_Url: string read FFollowers_Url write FFollowers_Url;
    property Following_Url: string read FFollowing_Url write FFollowing_Url;
    property Gists_Url: string read FGists_Url write FGists_Url;
    property Gravatar_Id: string read FGravatar_Id write FGravatar_Id;
    property Html_Url: string read FHtml_Url write FHtml_Url;
    property Id: Integer read FId write FId;
    property Login: string read FLogin write FLogin;
    property Node_Id: string read FNode_Id write FNode_Id;
    property Organizations_Url: string read FOrganizations_Url write FOrganizations_Url;
    property Received_Events_Url: string read FReceived_Events_Url write FReceived_Events_Url;
    property Repos_Url: string read FRepos_Url write FRepos_Url;
    property Site_Admin: Boolean read FSite_Admin write FSite_Admin;
    property Starred_Url: string read FStarred_Url write FStarred_Url;
    property Subscriptions_Url: string read FSubscriptions_Url write FSubscriptions_Url;
    property &Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
  end;
  
  TAssetsDTO = class
  private
    FBrowser_Download_Url: string;
    FContent_Type: string;
    FCreated_At: TDateTime;
    FDownload_Count: Integer;
    FId: Integer;
    FLabel: string;
    FName: string;
    FNode_Id: string;
    FSize: Integer;
    FState: string;
    FUpdated_At: TDateTime;
    FUploader: TUploaderDTO;
    FUrl: string;
  published
    property Browser_Download_Url: string read FBrowser_Download_Url write FBrowser_Download_Url;
    property Content_Type: string read FContent_Type write FContent_Type;
    property Created_At: TDateTime read FCreated_At write FCreated_At;
    property Download_Count: Integer read FDownload_Count write FDownload_Count;
    property Id: Integer read FId write FId;
    property &Label: string read FLabel write FLabel;
    property Name: string read FName write FName;
    property Node_Id: string read FNode_Id write FNode_Id;
    property Size: Integer read FSize write FSize;
    property State: string read FState write FState;
    property Updated_At: TDateTime read FUpdated_At write FUpdated_At;
    property Uploader: TUploaderDTO read FUploader;
    property Url: string read FUrl write FUrl;
  public
    constructor Create;
    destructor Destroy; override;
  end;
  
  TAuthorDTO = class
  private
    FAvatar_Url: string;
    FEvents_Url: string;
    FFollowers_Url: string;
    FFollowing_Url: string;
    FGists_Url: string;
    FGravatar_Id: string;
    FHtml_Url: string;
    FId: Integer;
    FLogin: string;
    FNode_Id: string;
    FOrganizations_Url: string;
    FReceived_Events_Url: string;
    FRepos_Url: string;
    FSite_Admin: Boolean;
    FStarred_Url: string;
    FSubscriptions_Url: string;
    FType: string;
    FUrl: string;
  published
    property Avatar_Url: string read FAvatar_Url write FAvatar_Url;
    property Events_Url: string read FEvents_Url write FEvents_Url;
    property Followers_Url: string read FFollowers_Url write FFollowers_Url;
    property Following_Url: string read FFollowing_Url write FFollowing_Url;
    property Gists_Url: string read FGists_Url write FGists_Url;
    property Gravatar_Id: string read FGravatar_Id write FGravatar_Id;
    property Html_Url: string read FHtml_Url write FHtml_Url;
    property Id: Integer read FId write FId;
    property Login: string read FLogin write FLogin;
    property Node_Id: string read FNode_Id write FNode_Id;
    property Organizations_Url: string read FOrganizations_Url write FOrganizations_Url;
    property Received_Events_Url: string read FReceived_Events_Url write FReceived_Events_Url;
    property Repos_Url: string read FRepos_Url write FRepos_Url;
    property Site_Admin: Boolean read FSite_Admin write FSite_Admin;
    property Starred_Url: string read FStarred_Url write FStarred_Url;
    property Subscriptions_Url: string read FSubscriptions_Url write FSubscriptions_Url;
    property &Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
  end;
  
  TItemsDTO = class(TArrayMapper)
  private
    [JSONName('assets')]
    [JSONMarshalled(False)]
    FAssetsArray: TArray<TAssetsDTO>;
    [GenericListReflect]
    FAssets: TObjectList<TAssetsDTO>;
    FAssets_Url: string;
    FAuthor: TAuthorDTO;
    FBody: string;
    FCreated_At: TDateTime;
    FDraft: Boolean;
    FHtml_Url: string;
    FId: Integer;
    FName: string;
    FNode_Id: string;
    FPrerelease: Boolean;
    FPublished_At: TDateTime;
    FTag_Name: string;
    FTarball_Url: string;
    FTarget_Commitish: string;
    FUpload_Url: string;
    FUrl: string;
    FZipball_Url: string;
    function GetAssets: TObjectList<TAssetsDTO>;
  published
    property Assets: TObjectList<TAssetsDTO> read GetAssets;
    property Assets_Url: string read FAssets_Url write FAssets_Url;
    property Author: TAuthorDTO read FAuthor;
    property Body: string read FBody write FBody;
    property Created_At: TDateTime read FCreated_At write FCreated_At;
    property Draft: Boolean read FDraft write FDraft;
    property Html_Url: string read FHtml_Url write FHtml_Url;
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Node_Id: string read FNode_Id write FNode_Id;
    property Prerelease: Boolean read FPrerelease write FPrerelease;
    property Published_At: TDateTime read FPublished_At write FPublished_At;
    property Tag_Name: string read FTag_Name write FTag_Name;
    property Tarball_Url: string read FTarball_Url write FTarball_Url;
    property Target_Commitish: string read FTarget_Commitish write FTarget_Commitish;
    property Upload_Url: string read FUpload_Url write FUpload_Url;
    property Url: string read FUrl write FUrl;
    property Zipball_Url: string read FZipball_Url write FZipball_Url;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
  
  TReleaseDTO = class(TJsonDTO)
  protected
    [JSONName('Items')]
    [JSONMarshalled(False)]
    FItemsArray: TArray<TItemsDTO>;
    [GenericListReflect]
    FItems: TObjectList<TItemsDTO>;
    function GetItems: TObjectList<TItemsDTO>;
  published
    property Items: TObjectList<TItemsDTO> read GetItems;
  public
    constructor Create; override;
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

{ TItemsDTO }

constructor TItemsDTO.Create;
begin
  inherited;
  FAuthor := TAuthorDTO.Create;
end;

destructor TItemsDTO.Destroy;
begin
  FAuthor.Free;
  FAssets.Free;
  inherited;
end;

function TItemsDTO.GetAssets: TObjectList<TAssetsDTO>;
begin
  Result := ObjectList<TAssetsDTO>(FAssets, FAssetsArray);
end;

{ TReleaseDTO }

constructor TReleaseDTO.Create;
begin
  inherited;
end;

destructor TReleaseDTO.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TReleaseDTO.GetItems: TObjectList<TItemsDTO>;
begin
  Result := ObjectList<TItemsDTO>(FItems, FItemsArray);
end;

end.