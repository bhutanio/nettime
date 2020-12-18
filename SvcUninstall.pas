unit SvcUninstall;

interface

procedure Uninstall;

implementation

uses Dialogs, Windows, WinSvc, SysUtils, Registry, IsWinNt, NetTimeCommon,
  WinsockUtil, mutex, NetTimeIPC;

procedure Uninstall;

var
  reg: TRegistry;
  sch: THandle;
  svh: THandle;
  InstDir: string;
  ni: TNetTimeIPC;

begin
  InstDir := ParamStr(1);
  if InstDir[length(InstDir)] <> '\' then
    InstDir := InstDir + '\';
  if IsWindowsNT then
    begin
      sch := OpenSCManager(nil,nil,SC_MANAGER_CONNECT);
      if sch <> 0 then
        begin
          svh := OpenService(sch,'NetTimeSvc',SERVICE_ALL_ACCESS);
          if sch <> 0 then
            DeleteService(svh);
        end;
    end;
  reg := TRegistry.Create;
  try
    reg.LazyWrite := false;
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if not reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\RunServices',true) then
      raise exception.create('Could not uninstall: RunServices key does not exist');
    reg.DeleteValue('NetTime Service');
    reg.CloseKey;
    if not reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run',true) then
      raise exception.create('Could not uninstall: Run key does not exist');
    reg.DeleteValue('NetTime');
    reg.CloseKey;
  finally
    reg.Free;
  end;
  ni := TNetTimeIPC.Create(nil);
  ni.InitResources;
  ni.KillEverything;
  ni.Free;
  Sleep(PollSleepTime);
  if GetExclusivity(ExNameUI) then
    ReleaseExclusivity(ExNameUI)
  else
     ShowMessage('An instance of the NetTime user interface is still running. '+
      'Please shut it down and then click OK.');
  if GetExclusivity(ExNameServer) then
    ReleaseExclusivity(ExNameServer)
  else
    ShowMessage('An instance of the NetTime server is still running. '+
      'Please shut it down and then click OK.');
end;

end.
