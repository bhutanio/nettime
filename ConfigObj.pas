{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit ConfigObj;

interface

uses SysUtils, timewrap, NetTimeCommon, IsWinNT, WinSvc, Math;

type
  TConfigObj = class
  private
    FServerCount: integer;
    FServers: TServerDefArray;
    Fsyncfreq: integer;
    Flostsync: integer;
    Fwarnadj: integer;
    Fretry: integer;
    Fserver: boolean;
    Floadonlogin: boolean;
    Fserviceonboot: boolean;
    function GetServer(idx: integer): TServerDef;
  public
    constructor Create;
    procedure ReadFromRegistry;
    procedure ReadFromRunning(tt: TNetTimeServerBase);
    procedure WriteToRegistry;
    procedure WriteToRunning(tt: TNetTimeServerBase);
    property ServerCount: integer read FServerCount;
    property Servers[idx: integer]: TServerDef read GetServer;
    property SyncFreq: integer read Fsyncfreq write Fsyncfreq;
    property LostSync: integer read Flostsync write Flostsync;
    property WarnAdj: integer read Fwarnadj write Fwarnadj;
    property Retry: integer read Fretry write Fretry;
    property Server: boolean read Fserver write Fserver;
    property LoadOnLogin: boolean read Floadonlogin write Floadonlogin;
    property ServiceOnBoot: boolean read Fserviceonboot write Fserviceonboot;
    procedure ClearServerList;
    procedure AddServer(const Srv: TServerDef);
  end;

implementation

uses windows, registry;

function FindExe(const exefn: string): string;

var
  dir: string;
  di: TSearchRec;
  found: boolean;

begin
  dir := ExtractFilePath(ExpandFileName(ParamStr(0)));
  found := (FindFirst(dir+exefn,faAnyFile,di) = 0);
  {$WARNINGS OFF} FindClose(di.FindHandle); {$WARNINGS ON}
  if found then
    result := dir + di.Name
  else
    begin
      result := '';
      raise exception.create('Could not locate '+exefn);
    end;
end;

constructor TConfigObj.Create;
begin
  inherited Create;
  FServerCount := 0;
  Fsyncfreq := DefaultSyncFreq;
  Flostsync := DefaultLostSync;
  Fretry := DefaultRetry;
  Fwarnadj := DefaultWarnAdj;
  Fserver := false;
  Floadonlogin := false;
  Fserviceonboot := false;
end;

procedure TConfigObj.ReadFromRegistry;

var
  reg: TRegistry;
  i: integer;
  s: string;
  sch, svh: THandle;
  qsc: QUERY_SERVICE_CONFIG;
  qbn: cardinal;

  procedure GetServerInfo(const n: integer);

  var
    sd: TServerDef;
    hns: string;

  begin
    if n = 0 then
      hns := ''
    else
      hns := inttostr(n);
    sd.Hostname := '';
    try
      sd.Hostname := reg.ReadString('Hostname'+hns);
    except
    end;
    if sd.Hostname <> '' then
      begin
        sd.protocol := ttpRFC868_TCP;
        try
          sd.protocol := TTimeProto(reg.ReadInteger('Protocol'+hns));
        except
        end;
        sd.Port := DefaultPortForProtocol(sd.Protocol);
        try
          sd.Port := reg.ReadInteger('Port'+hns);
        except
        end;
        FServers[FServerCount] := sd;
        inc(FServerCount);
      end;
  end;

begin
  reg := TRegistry.Create;
  reg.RootKey := HKEY_LOCAL_MACHINE;
  if reg.OpenKey('Software\Subjective Software\NetTime',false) then
    begin
      for i := 0 to MaxServers-1 do
        GetServerInfo(i);
      try
        Fsyncfreq := reg.ReadInteger('SyncFreq');
      except
      end;
      try
        Flostsync := reg.ReadInteger('LostSync');
      except
      end;
      try
        Fwarnadj := reg.ReadInteger('WarnAdj');
      except
      end;
      try
        Fretry := reg.ReadInteger('Retry');
      except
      end;
      Fserver := false;
      try
        Fserver := reg.ReadBool('Server');
      except
      end;
    end;
  reg.CloseKey;
  if reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run',false) then
    begin
      try
        s := reg.ReadString('NetTime')
      except
        s := '';
      end;
      Floadonlogin := (s <> '');
    end;
  reg.CloseKey;
  if IsWindowsNT then
    begin
      sch := OpenSCManager(nil,nil,SC_MANAGER_CONNECT);
      if sch = 0 then
        Fserviceonboot := false;
      svh := OpenService(sch,'NetTimeSvc',SERVICE_ALL_ACCESS);
      if svh = 0 then
        Fserviceonboot := false
      else
        begin
          svh := OpenService(sch,'NetTimeSvc',SERVICE_ALL_ACCESS);
          if svh = 0 then
            Fserviceonboot := false
          else
            begin
              QueryServiceConfig(svh,@qsc,sizeof(qsc),qbn);
              if (qsc.dwStartType = SERVICE_AUTO_START) then
                Fserviceonboot := true
              else
                Fserviceonboot := false;
            end;
          CloseServiceHandle(svh);
        end;
      CloseServiceHandle(sch);
    end
  else
    begin
      if reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\RunServices',false) then
        begin
          try
            s := reg.ReadString('NetTime Service')
          except
            s := '';
          end;
          Fserviceonboot := (s <> '');
        end;
      reg.CloseKey;
    end;
  reg.Free;
end;

procedure TConfigObj.ReadFromRunning(tt: TNetTimeServerBase);

var
  cfg: TServerConfigBlock;

begin
  cfg := tt.GetConfig;
  FServerCount := cfg.ServerCount;
  FServers := cfg.Servers;
  Fsyncfreq := cfg.SyncFreq;
  Flostsync := cfg.LostSync;
  Fwarnadj := cfg.WarnAdj;
  Fretry := cfg.Retry;
  Fserver := tt.Server;
end;

procedure TConfigObj.WriteToRegistry;

var
  reg: TRegistry;
  i: integer;
  sch, svh: THandle;
  s: string;

  procedure WriteServer(const n: integer);

  var
    hns: string;

  begin
    if n = 0 then
      hns := ''
    else
      hns := inttostr(n);
    if (n < ServerCount) then
      begin
        reg.WriteString('Hostname'+hns,Servers[n].hostname);
        reg.WriteInteger('Protocol'+hns,integer(servers[n].protocol));
        reg.WriteInteger('Port'+hns,servers[n].port);
      end
    else
      begin
        reg.DeleteValue('Hostname'+hns);
        reg.DeleteValue('Protocol'+hns);
        reg.DeleteValue('Port'+hns);
      end;
  end;

begin
  reg := TRegistry.Create;
  reg.RootKey := HKEY_LOCAL_MACHINE;
  if reg.OpenKey('Software\Subjective Software\NetTime',true) then
    begin
      for i := 0 to MaxServers-1 do
        WriteServer(i);
      reg.WriteInteger('SyncFreq',syncfreq);
      reg.WriteInteger('LostSync',lostsync);
      reg.WriteInteger('WarnAdj',warnadj);
      reg.WriteInteger('Retry',retry);
      reg.WriteBool('Server',server);
    end;
  reg.CloseKey;
  if reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run',true) then
    begin
      if LoadOnLogin then
        reg.WriteString('NetTime',FindExe('NetTime.exe'))
      else
        reg.DeleteValue('NetTime');
    end;
  reg.CloseKey;
  if IsWindowsNT then
    begin
      sch := OpenSCManager(nil,nil,SC_MANAGER_CONNECT);
      if sch = 0 then
        raise exception.create('Could not open service control manager');
      svh := OpenService(sch,'NetTimeSvc',SERVICE_ALL_ACCESS);
      if svh = 0 then
        begin
          s := FindExe('NeTmSvNT.exe') + ' /install /silent';
          WinExecAndWait(pchar(s),SW_SHOW);
          svh := OpenService(sch,'NetTimeSvc',SERVICE_ALL_ACCESS);
        end;
      if svh = 0 then
        raise exception.create('Could open NetTime service');
      if not ChangeServiceConfig(svh, SERVICE_NO_CHANGE,
        IfThen(Fserviceonboot, SERVICE_AUTO_START, SERVICE_DEMAND_START),
        SERVICE_NO_CHANGE, nil, nil, nil, nil, nil, nil, nil) then
        raise exception.create('Could not update service configuration. '+
          'Error: ' + SysErrorMessage(GetLastError));
      CloseServiceHandle(svh);
      CloseServiceHandle(sch);
    end
  else
    begin
      if reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\RunServices',true) then
        begin
          if ServiceOnBoot then
            reg.WriteString('NetTime Service',FindExe('NeTmSv95.exe'))
          else
            reg.DeleteValue('NetTime Service');
        end;
      reg.CloseKey;
    end;
  reg.Free;
end;

procedure TConfigObj.WriteToRunning(tt: TNetTimeServerBase);

var
  cfg: TServerConfigBlock;

begin
  cfg.ServerCount := FServerCount;
  cfg.Servers := FServers;
  cfg.SyncFreq := syncfreq;
  cfg.LostSync := lostsync;
  cfg.WarnAdj := warnadj;
  cfg.Retry := retry;
  tt.SetConfig(cfg);
  tt.Server := server;
end;

function TConfigObj.GetServer(idx: integer): TServerDef;
begin
  result := FServers[idx];
end;

procedure TConfigObj.ClearServerList;
begin
  FServerCount := 0;
end;

procedure TConfigObj.AddServer(const Srv: TServerDef);
begin
  FServers[FServerCount] := Srv;
  inc(FServerCount);
end;

end.

