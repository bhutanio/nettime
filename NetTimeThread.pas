{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit NetTimeThread;

interface

uses
  Windows, SysUtils, Classes, timewrap, unixtime, ntptime, ComObj,
    NetTimeCommon, NetTimeIPC, Winsock;

type

  TTimeThread = class;
  TTimeWatcher = class;

  TNetTimeServer = class(TNetTimeServerBase)
  private

    ttime: TTimeThread;
    ttwatch: TTimeWatcher;

    FActive: boolean;
    FServerCount: integer;
    FServers: TServerDefArray;
    FSyncFreq: integer;
    FLostSync: integer;
    FRetry: integer;
    FWarnAdj: integer;
    FServer: boolean;
    FOnStateChange: TNotifyEvent;
    FOnWarnAdj: TWarnAdjEvent;
    FOnExit: TNotifyEvent;
    FServerTime, FStationTime: TDateTime;
    FWarnAdjResult: boolean;
    FWantUpdateNow: boolean;
    FLastUpdateStatus: TSyncStatus;

    ServerIPC: TNetTimeIPCServer;

    RFC868_TCP_Thread: TRFC868_TCPServerThread;
    RFC868_UDP_Thread: TRFC868_UDPServerThread;
    NTP_Thread: TNTPServerThread;

    function GetServerStatus: TServerStatusBlock;
    procedure ExitNow;

  protected
    procedure DoStateChange;
    procedure DoWarnAdj;

  public
    function GetActive: boolean; override;
    function GetStatus: TSyncStatus; override;
    function GetLastUpdateTime: TDateTime; override;
    function GetStateChange: TNotifyEvent; override;
    procedure SetStateChange(const sc: TNotifyEvent); override;
    function GetWarnAdj: TWarnAdjEvent; override;
    procedure SetWarnAdj(const wa: TWarnAdjEvent); override;
    function GetOnExit: TNotifyEvent; override;
    procedure SetOnExit(const ex: TNotifyEvent); override;
    function GetServer: boolean; override;
    procedure SetServer(const sv: boolean); override;
    procedure SetConfig(const cfg: TServerConfigBlock); override;
    function GetConfig: TServerConfigBlock; override;
    procedure ForceUpdate; override;
    function UpdateNow: boolean; override;
    procedure KillEverything; override;
    procedure Start;
    procedure Stop;
    constructor Create;
  end;

  TTimeThread = class(TThread)
  public
    dttime: TDateTime;
    NowOfLastUpdate: TDateTime;
    StatusOfLastUpdate: TSyncStatus;
    MyOwner: TNetTimeServer;
    procedure SetDateTime(dDateTime: TDateTime);
    constructor Create(const Suspended: boolean);
    destructor Destroy; override;
  private
    update_rejected: boolean;
    time_retrieval_time: TDateTime;
  protected
    procedure Execute; override;
  end;

  TTimeWatcher = class(TThread)
  public
    synchronized: boolean;
    MyOwner: TNetTimeServer;
  protected
    procedure Execute; override;
  end;

implementation

uses iswinnt, timeconv, Registry, winerr, WinSockUtil;

constructor TNetTimeServer.Create;
begin
  inherited Create;
  FServerCount := 0;
  FSyncFreq := DefaultSyncFreq;
  FLostSync := DefaultLostSync;
  FRetry := DefaultRetry;
  FWarnAdj := DefaultWarnAdj;
  FServer := false;
  FOnStateChange := nil;
  FOnWarnAdj := nil;
end;

procedure TNetTimeServer.ForceUpdate;
begin
  // do nothing
end;

function TNetTimeServer.GetActive: boolean;
begin
  result := FActive;
end;

function TNetTimeServer.GetStateChange: TNotifyEvent;
begin
  result := FOnStateChange;
end;

procedure TNetTimeServer.SetStateChange(const sc: TNotifyEvent);
begin
  FOnStateChange := sc;
end;

function TNetTimeServer.GetWarnAdj: TWarnAdjEvent;
begin
  result := FOnWarnAdj;
end;

procedure TNetTimeServer.SetWarnAdj(const wa: TWarnAdjEvent);
begin
  FOnWarnAdj := wa;
end;

function TNetTimeServer.GetOnExit: TNotifyEvent;
begin
  result := FOnExit;
end;

procedure TNetTimeServer.SetOnExit(const ex: TNotifyEvent);
begin
  FOnExit := ex;
end;

procedure TNetTimeServer.SetConfig(const cfg: TServerConfigBlock);
begin
  FServerCount := cfg.ServerCount;
  FServers := cfg.Servers;
  FSyncFreq := cfg.SyncFreq;
  FLostSync := cfg.LostSync;
  FWarnAdj := cfg.WarnAdj;
  FRetry := cfg.Retry;
end;

function TNetTimeServer.GetConfig: TServerConfigBlock;
begin
  result.ServerCount := FServerCount;
  result.Servers := FServers;
  result.SyncFreq := FSyncFreq;
  result.LostSync := FLostSync;
  result.WarnAdj := FWarnAdj;
  result.Retry := FRetry;
end;

function TNetTimeServer.GetLastUpdateTime: TDateTime;
begin
  if FActive then
    result := (ttime as TTimeThread).NowOfLastUpdate
  else
    result := 0;
end;

function TNetTimeServer.GetStatus: TSyncStatus;

var
  i: integer;

begin
  if not FActive then
    begin
      result.Synchronized := false;
      for i := 0 to MaxServers-1 do
        result.ss[i] := ssUnconfigured;
    end
  else
    begin
      result.Synchronized := (ttwatch as TTimeWatcher).synchronized;
      result.ss := FLastUpdateStatus.ss;
    end;
end;

function TNetTimeServer.GetServer: boolean;
begin
  result := FServer;
end;

procedure TNetTimeServer.SetServer(const sv: boolean);
begin
  if (FServer = sv) then
    exit;
  FServer := sv;
  if FServer then
    begin
      RFC868_TCP_Thread := TRFC868_TCPServerThread.Create(false,RFC868_Port);
      RFC868_UDP_Thread := TRFC868_UDPServerThread.Create(false,RFC868_Port);
      NTP_Thread := TNTPServerThread.Create(false,NTP_Port);
    end
  else
    begin
      RFC868_TCP_Thread.Terminate;
      RFC868_UDP_Thread.Terminate;
      NTP_Thread.Terminate;
    end;
end;

function TNetTimeServer.GetServerStatus: TServerStatusBlock;
begin
  result.Config := GetConfig;
  result.Server := GetServer;
  result.Active := FActive;
  result.Status := GetStatus;
  result.LastUpdateTime := GetLastUpdateTime;
end;

procedure TNetTimeServer.ExitNow;
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

function TNetTimeServer.UpdateNow: boolean;
begin
  FWantUpdateNow := true;
  while FWantUpdateNow do
    sleep(GUISleepTime);
  result := FLastUpdateStatus.Synchronized;
end;

procedure TNetTimeServer.KillEverything;
begin
  ServerIPC.KillEverything;
end;

procedure TNetTimeServer.DoWarnAdj;
begin
  if Assigned(FOnWarnAdj) then
    FWarnAdjResult := FOnWarnAdj(Self,FServerTime,FStationTime)
  else
    FWarnAdjResult := ServerIPC.LargeAdjustWarn(FServerTime,FStationTime);
end;

procedure TNetTimeServer.DoStateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
  ServerIPC.AdviseStatus;
end;

procedure TNetTimeServer.Start;

var
  tt: TTimeThread;
  tw: TTimeWatcher;

begin
  if FActive then exit;
  ServerIPC := TNetTimeIPCServer.Create(GetServerStatus, SetConfig, SetServer,
    ExitNow, UpdateNow);
  ServerIPC.InitResources;
  ttime := TTimeThread.Create(true);
  tt := (ttime as TTimeThread);
  tt.MyOwner := self;
  tt.Resume;
  ttwatch := TTimeWatcher.Create(true);
  tw := (ttwatch as TTimeWatcher);
  tw.MyOwner := self;
  tw.Resume;
  ntptime.TimeSyncGoodFunc := GetSynchronized;
  ntptime.TimeLastUpdatedFunc := GetLastUpdateTime;
  FActive := true;
end;

procedure TNetTimeServer.Stop;
begin
  if not FActive then Exit;
  Sleep(IPCSleepTime);
  ntptime.TimeSyncGoodFunc := nil;
  ntptime.TimeLastUpdatedFunc := nil;
  if assigned(ttwatch) then
    begin
      ttwatch.Terminate;
      ttwatch.WaitFor;
      ttwatch.Free;
    end;
  if assigned(ttime) then
    begin
      ttime.Terminate;
      ttime.WaitFor;
      ttime.Free;
    end;
  ServerIPC.Free;
  FActive := false;
end;

{ TTimeThread }

constructor TTimeThread.Create(const Suspended: boolean);
begin
  inherited Create(true);
  // nothing to initialize yet
  if not Suspended then
    Resume;
end;

destructor TTimeThread.Destroy;
begin
  inherited Destroy;
end;

procedure TTimeThread.SetDateTime(dDateTime: TDateTime);
var
  dSysTime: TSystemTime;
  buffer: DWord;
  tkp, tpko: TTokenPrivileges;
  hToken: THandle;

begin
  if IsWindowsNT then
    begin
      if not OpenProcessToken(GetCurrentProcess(),
        TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then exit;
      LookupPrivilegeValue(nil,'SE_SYSTEMTIME_NAME',tkp.Privileges[0].Luid);
      tkp.PrivilegeCount := 1;
      tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      if not AdjustTokenPrivileges(hToken, FALSE, tkp, sizeof(tkp),
        tpko, buffer) then exit;
      CloseHandle(hToken);
    end;
  DateTimeToSystemTime(dDateTime, dSysTime);
  SetLocalTime(dSysTime);
end;

procedure TTimeThread.Execute;

var
  time_status: TSyncStatus;
  haveaddr, oldhaveaddr: boolean;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or (not Assigned(MyOwner));
  end;

begin
  ReturnValue := 0;
  dttime := 0;
  NowOfLastUpdate := 0;
  StatusOfLastUpdate.Synchronized := false;
  time_retrieval_time := 0;
  haveaddr := HaveLocalAddress;
  repeat
    if not ThreadClosed then
      begin
        try
          FigureBestKnownTime(MyOwner.FServerCount, MyOwner.FServers,
            time_status, dttime);
          time_retrieval_time := Now;
        except
          time_status.Synchronized := false;
        end;
        if time_status.Synchronized then
          begin
            update_rejected := false;
            if (MyOwner.FWarnAdj > 0) and
              (SecondsApart(Now,dttime) >= MyOwner.FWarnAdj) then
              begin
                MyOwner.FServerTime := dttime;
                MyOwner.FStationTime := time_retrieval_time;
                Synchronize(MyOwner.DoWarnAdj);
                update_rejected := not MyOwner.FWarnAdjResult;
              end;
            if update_rejected then
              begin
                Synchronize(MyOwner.ServerIPC.KillEverything);
                Terminate;
              end
            else
              begin
                SetDateTime(dttime + (Now - time_retrieval_time));
                NowOfLastUpdate := Now;
                StatusOfLastUpdate := time_status;
              end;
          end;
      end;
    MyOwner.FLastUpdateStatus := time_status;
    MyOwner.FWantUpdateNow := false;
    if IsWindowsNT then
      SetProcessWorkingSetSize(GetCurrentProcess,$ffffffff,$ffffffff);
    repeat
      Sleep(PollSleepTime);
      oldhaveaddr := haveaddr;
      haveaddr := HaveLocalAddress;
      if (not oldhaveaddr) and (haveaddr) then
        MyOwner.FWantUpdateNow := true;
    until self.terminated or
      (haveaddr and (
        MyOwner.FWantUpdateNow or
        (time_status.Synchronized and (SecondsApart(Now,NowOfLastUpdate) >= MyOwner.FSyncFreq)) or
        ((not time_status.Synchronized) and (SecondsApart(Now,NowOfLastUpdate) >= MyOwner.FRetry))
      ));
  until (self.terminated);
  ReturnValue := 1;
end;

{ TTimeWatcher }

procedure TTimeWatcher.Execute;

var
  oldUpdate: TDateTime;
  old_sync: boolean;

begin
  ReturnValue := 0;
  oldUpdate := 0;
  Synchronized := false;
  repeat
    Sleep(PollSleepTime);
    old_sync := Synchronized;
    if SecondsApart(Now,(MyOwner.ttime as TTimeThread).NowOfLastUpdate)
      <= int64(MyOwner.FLostSync) then
      Synchronized := (MyOwner.ttime as TTimeThread).StatusOfLastUpdate.Synchronized
    else
      Synchronized := false;
    if (Synchronized <> old_sync) or
     (oldUpdate <> (MyOwner.ttime as TTimeThread).NowOfLastUpdate) then
      begin
        Synchronize(MyOwner.DoStateChange);
        oldUpdate := (MyOwner.ttime as TTimeThread).NowOfLastUpdate;
      end;
  until (Self.Terminated);
  ReturnValue := 1;
end;

end.
