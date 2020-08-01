unit Pkg.Json.Utils;

interface
uses
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
Posix.Stdlib;
{$ENDIF POSIX}

procedure ShellExecute(aFileName : string);

implementation
procedure ShellExecute(aFileName : string);
begin
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI.ShellExecute(0, 'OPEN', PChar(aFileName), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(aFileName)));
{$ENDIF POSIX}

end;

end.
