unit Pkg.Json.ReservedWords;

interface

uses
  System.Classes;

type
  TStringsHelper = class helper for TStrings
  public
    procedure AddFormat(const aFormat: string; const Args: array of const);
    procedure AddIfNotEmpty(const s: string);
    function Contains(const s: string): boolean;
  end;

var
  ReservedWords: TStringlist;

  { TStringsHelper }
implementation

uses
  System.SysUtils;

procedure TStringsHelper.AddIfNotEmpty(const s: string);
begin
  if s <> '' then
    inherited Add(s);
end;

function TStringsHelper.Contains(const s: string): boolean;
begin
  Result := IndexOf(s) >= 0;
end;

procedure TStringsHelper.AddFormat(const aFormat: string; const Args: array of const);
begin
  Add(Format(aFormat, Args));
end;

initialization

ReservedWords := TStringlist.Create;
ReservedWords.Add('and');
ReservedWords.Add('array');
ReservedWords.Add('as');
ReservedWords.Add('asm');
ReservedWords.Add('begin');
ReservedWords.Add('case');
ReservedWords.Add('class');
ReservedWords.Add('const');
ReservedWords.Add('constructor');
ReservedWords.Add('destructor');
ReservedWords.Add('dispinterface');
ReservedWords.Add('div');
ReservedWords.Add('do');
ReservedWords.Add('downto');
ReservedWords.Add('else');
ReservedWords.Add('end');
ReservedWords.Add('except');
ReservedWords.Add('exports');
ReservedWords.Add('file');
ReservedWords.Add('finalization');
ReservedWords.Add('finally');
ReservedWords.Add('for');
ReservedWords.Add('function');
ReservedWords.Add('goto');
ReservedWords.Add('if');
ReservedWords.Add('implementation');
ReservedWords.Add('in');
ReservedWords.Add('inherited');
ReservedWords.Add('initialization');
ReservedWords.Add('inline');
ReservedWords.Add('interface');
ReservedWords.Add('is');
ReservedWords.Add('label');
ReservedWords.Add('library');
ReservedWords.Add('mod');
ReservedWords.Add('nil');
ReservedWords.Add('not');
ReservedWords.Add('object');
ReservedWords.Add('of');
ReservedWords.Add('or');
ReservedWords.Add('out');
ReservedWords.Add('packed');
ReservedWords.Add('procedure');
ReservedWords.Add('program');
ReservedWords.Add('property');
ReservedWords.Add('raise');
ReservedWords.Add('record');
ReservedWords.Add('repeat');
ReservedWords.Add('resourcestring');
ReservedWords.Add('set');
ReservedWords.Add('shl');
ReservedWords.Add('shr');
ReservedWords.Add('string');
ReservedWords.Add('then');
ReservedWords.Add('threadvar');
ReservedWords.Add('to');
ReservedWords.Add('try');
ReservedWords.Add('type');
ReservedWords.Add('unit');
ReservedWords.Add('until');
ReservedWords.Add('uses');
ReservedWords.Add('var');
ReservedWords.Add('while');
ReservedWords.Add('with');
ReservedWords.Add('xor');
ReservedWords.Add('threadvar');
ReservedWords.Add('raise');
ReservedWords.Add('interface');

ReservedWords.Sorted := True;

finalization

FreeAndNil(ReservedWords);

end.
