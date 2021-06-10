unit DelphiBuilderU;

interface

uses
  Classes;

Type
  DelphiBuilder = class
  protected
    class function GetDelphiInstallPath: string;
    class procedure CaptureConsoleOutput(const lpCommandLine: string; OutPutList: TStrings);
  public
    class procedure CompileProject(Console: TStrings; const ProjectFile: string);
  end;

implementation

uses
  System.SysUtils, WinApi.ShellAPI, WinApi.Windows, System.Win.Registry, RegistryHelperU;
{ TDelphiBuilder }

class procedure DelphiBuilder.CaptureConsoleOutput(const lpCommandLine: string; OutPutList: TStrings);
const
  ReadBuffer = 1024 * 1024;
var
  lpPipeAttributes: TSecurityAttributes;
  ReadPipe: THandle;
  WritePipe: THandle;
  lpStartupInfo: TStartUpInfo;
  lpProcessInformation: TProcessInformation;
  Buffer: PAnsiChar;
  TotalBytesRead: DWORD;
  BytesRead: DWORD;
  Apprunning: integer;
  n: integer;
  BytesLeftThisMessage: integer;
  TotalBytesAvail: integer;
begin
  with lpPipeAttributes do
  begin
    nlength := SizeOf(TSecurityAttributes);
    binherithandle := True;
    lpsecuritydescriptor := nil;
  end;

  if not CreatePipe(ReadPipe, WritePipe, @lpPipeAttributes, 0) then
    exit;

  try
    Buffer := AllocMem(ReadBuffer + 1);
    try
      ZeroMemory(@lpStartupInfo, SizeOf(lpStartupInfo));
      lpStartupInfo.cb := SizeOf(lpStartupInfo);
      lpStartupInfo.hStdOutput := WritePipe;
      lpStartupInfo.hStdInput := ReadPipe;
      lpStartupInfo.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
      lpStartupInfo.wShowWindow := SW_HIDE;

      OutPutList.Add(lpCommandLine);
      if CreateProcess(nil, PChar(lpCommandLine), @lpPipeAttributes, @lpPipeAttributes, True, CREATE_NO_WINDOW or NORMAL_PRIORITY_CLASS, nil, nil, lpStartupInfo, lpProcessInformation) then
      begin
        try
          n := 0;
          TotalBytesRead := 0;
          repeat
            Inc(n);
            Apprunning := WaitForSingleObject(lpProcessInformation.hProcess, 100);

            if not PeekNamedPipe(ReadPipe, @Buffer[TotalBytesRead], ReadBuffer, @BytesRead, @TotalBytesAvail, @BytesLeftThisMessage) then
              break
            else if BytesRead > 0 then
              ReadFile(ReadPipe, Buffer[TotalBytesRead], BytesRead, BytesRead, nil);

            // Inc(TotalBytesRead, BytesRead);

            Buffer[BytesRead] := #0;
            OemToAnsi(Buffer, Buffer);
            OutPutList.Text := OutPutList.Text + string(Buffer);

          until (Apprunning <> WAIT_TIMEOUT) or (n > 150);

          {
            Buffer[TotalBytesRead] := #0;
            OemToAnsi(Buffer, Buffer);
            OutPutList.Text := OutPutList.Text + String(Buffer);
          }
        finally
          CloseHandle(lpProcessInformation.hProcess);
          CloseHandle(lpProcessInformation.hThread);
        end;
      end;
    finally
      FreeMem(Buffer);
    end;
  finally
    CloseHandle(ReadPipe);
    CloseHandle(WritePipe);
  end;
end;

class procedure DelphiBuilder.CompileProject(Console: TStrings; const ProjectFile: string);
var
  Path: string;
begin
  Console.Add('');
  with TStringList.Create do
    try
      LoadFromFile(GetDelphiInstallPath + 'rsvars.bat');
      Path := ExtractFilePath(ProjectFile);
      Add(Format('msbuild.exe "%s"', [ProjectFile]));
      SaveToFile(Path + 'Build.bat');
    finally
      Free;
    end;

  CaptureConsoleOutput(Path + 'Build.bat', Console);
  Console.Add('');
  DeleteFile(PChar(Path + 'Build.bat'));
end;

class function DelphiBuilder.GetDelphiInstallPath: string;
var
  RegKey: String;
  Filename: string;
  Found: boolean;
begin
  RegKey := '\Software\Embarcadero\BDS\' + FloatToStr(CompilerVersion - 13) + '.0';
  Found := RegKeyExists(RegKey, HKEY_CURRENT_USER);
  if Found then
    Found := RegReadStr(RegKey, 'App', Filename, HKEY_CURRENT_USER) and FileExists(Filename);

  if not Found then
    if RegKeyExists(RegKey, HKEY_LOCAL_MACHINE) then
      RegReadStr(RegKey, 'App', Filename, HKEY_LOCAL_MACHINE);

  Result := ExtractFilePath(Filename);
end;

end.
