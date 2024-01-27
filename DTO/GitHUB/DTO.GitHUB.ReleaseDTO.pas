unit DTO.GitHUB.ReleaseDTO;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  TAssets = class;
  TAuthor = class;

  TAssets = class
  end;

  TAuthor = class
  private
    [JSONName('avatar_url')]
    FAvatarUrl: string;
    [JSONName('events_url')]
    FEventsUrl: string;
    [JSONName('followers_url')]
    FFollowersUrl: string;
    [JSONName('following_url')]
    FFollowingUrl: string;
    [JSONName('gists_url')]
    FGistsUrl: string;
    [JSONName('gravatar_id')]
    FGravatarId: string;
    [JSONName('html_url')]
    FHtmlUrl: string;
    FId: Integer;
    FLogin: string;
    [JSONName('node_id')]
    FNodeId: string;
    [JSONName('organizations_url')]
    FOrganizationsUrl: string;
    [JSONName('received_events_url')]
    FReceivedEventsUrl: string;
    [JSONName('repos_url')]
    FReposUrl: string;
    [JSONName('site_admin')]
    FSiteAdmin: Boolean;
    [JSONName('starred_url')]
    FStarredUrl: string;
    [JSONName('subscriptions_url')]
    FSubscriptionsUrl: string;
    FType: string;
    FUrl: string;
  published
    property AvatarUrl: string read FAvatarUrl write FAvatarUrl;
    property EventsUrl: string read FEventsUrl write FEventsUrl;
    property FollowersUrl: string read FFollowersUrl write FFollowersUrl;
    property FollowingUrl: string read FFollowingUrl write FFollowingUrl;
    property GistsUrl: string read FGistsUrl write FGistsUrl;
    property GravatarId: string read FGravatarId write FGravatarId;
    property HtmlUrl: string read FHtmlUrl write FHtmlUrl;
    property Id: Integer read FId write FId;
    property Login: string read FLogin write FLogin;
    property NodeId: string read FNodeId write FNodeId;
    property OrganizationsUrl: string read FOrganizationsUrl write FOrganizationsUrl;
    property ReceivedEventsUrl: string read FReceivedEventsUrl write FReceivedEventsUrl;
    property ReposUrl: string read FReposUrl write FReposUrl;
    property SiteAdmin: Boolean read FSiteAdmin write FSiteAdmin;
    property StarredUrl: string read FStarredUrl write FStarredUrl;
    property SubscriptionsUrl: string read FSubscriptionsUrl write FSubscriptionsUrl;
    property &Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
  end;

  TItems = class(TJsonDTO)
  private
    [JSONName('assets'), JSONMarshalled(False)]
    FAssetsArray: TArray<TAssets>;
    [GenericListReflect]
    FAssets: TObjectList<TAssets>;
    [JSONName('assets_url')]
    FAssetsUrl: string;
    FAuthor: TAuthor;
    FBody: string;
    [SuppressZero, JSONName('created_at')]
    FCreatedAt: TDateTime;
    FDraft: Boolean;
    [JSONName('html_url')]
    FHtmlUrl: string;
    FId: Integer;
    FName: string;
    [JSONName('node_id')]
    FNodeId: string;
    FPrerelease: Boolean;
    [SuppressZero, JSONName('published_at')]
    FPublishedAt: TDateTime;
    [JSONName('tag_name')]
    FTagName: string;
    [JSONName('tarball_url')]
    FTarballUrl: string;
    [JSONName('target_commitish')]
    FTargetCommitish: string;
    [JSONName('upload_url')]
    FUploadUrl: string;
    FUrl: string;
    [JSONName('zipball_url')]
    FZipballUrl: string;
    function GetAssets: TObjectList<TAssets>;
  protected
    function GetAsJson: string; override;
  published
    property Assets: TObjectList<TAssets> read GetAssets;
    property AssetsUrl: string read FAssetsUrl write FAssetsUrl;
    property Author: TAuthor read FAuthor;
    property Body: string read FBody write FBody;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property Draft: Boolean read FDraft write FDraft;
    property HtmlUrl: string read FHtmlUrl write FHtmlUrl;
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property NodeId: string read FNodeId write FNodeId;
    property Prerelease: Boolean read FPrerelease write FPrerelease;
    property PublishedAt: TDateTime read FPublishedAt write FPublishedAt;
    property TagName: string read FTagName write FTagName;
    property TarballUrl: string read FTarballUrl write FTarballUrl;
    property TargetCommitish: string read FTargetCommitish write FTargetCommitish;
    property UploadUrl: string read FUploadUrl write FUploadUrl;
    property Url: string read FUrl write FUrl;
    property ZipballUrl: string read FZipballUrl write FZipballUrl;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TReleases = class(TJsonDTO)
  private
    [JSONName('Items'), JSONMarshalled(False)]
    FItemsArray: TArray<TItems>;
    [GenericListReflect]
    FItems: TObjectList<TItems>;
    function GetItems: TObjectList<TItems>;
  protected
    function GetAsJson: string; override;
  published
    property Items: TObjectList<TItems> read GetItems;
  public
    destructor Destroy; override;
  end;

  TRelease = DTO.GitHUB.ReleaseDTO.TItems;

implementation

{ TItems }

constructor TItems.Create;
begin
  inherited;
  FAuthor := TAuthor.Create;
end;

destructor TItems.Destroy;
begin
  FAuthor.Free;
  GetAssets.Free;
  inherited;
end;

function TItems.GetAssets: TObjectList<TAssets>;
begin
  Result := ObjectList<TAssets>(FAssets, FAssetsArray);
end;

function TItems.GetAsJson: string;
begin
  RefreshArray<TAssets>(FAssets, FAssetsArray);
  Result := inherited;
end;

{ TReleases }

destructor TReleases.Destroy;
begin
  GetItems.Free;
  inherited;
end;

function TReleases.GetItems: TObjectList<TItems>;
begin
  Result := ObjectList<TItems>(FItems, FItemsArray);
end;

function TReleases.GetAsJson: string;
begin
  RefreshArray<TItems>(FItems, FItemsArray);
  Result := inherited;
end;

end.
