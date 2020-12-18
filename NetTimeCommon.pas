unit NetTimeCommon;

interface

uses Windows, Messages, Classes, SysUtils;

const
  RFC868_Port = 37;
  NTP_Port = 123;
  MaxServers = 5;
  MaxServerList = 1000;
  MagicCookie = $1A34450B;
  ProtocolVersion = 4;
  ms = 1.0 / (24 * 60 * 60 * 1000);
  IPCSleepTime = 10;
  GUISleepTime = 100;
  PollSleepTime = 1000;

type

  TTimeProto = (ttpNTP, ttpRFC868_TCP, ttpRFC868_UDP);

  TServerDef = record
    hostname: Shortstring;
    protocol: TTimeProto;
    Port: integer;
  end;

  TServerDefArray = array[0..MaxServers-1] of TServerDef;

  TServerConfigBlock = record
    ServerCount: integer;
    Servers: TServerDefArray;
    SyncFreq: integer;
    LostSync: integer;
    WarnAdj: integer;
    Retry: integer;
    Protocol: TTimeProto;
  end;

  TWarnAdjEvent = function(const Sender: TObject;
    const ServerTime, StationTime: TDateTime): boolean of object;

  TSyncServerStatus = (ssGood, ssFailed, ssWrong, ssUnconfigured);
  TSyncStatus = record
    Synchronized: boolean;
    ss: array[0..MaxServers-1] of TSyncServerStatus;
  end;

  TNetTimeServerBase = class
  public
    function GetActive: boolean; virtual; abstract;
    function GetStatus: TSyncStatus; virtual; abstract;
    function GetSynchronized: boolean; virtual;
    function GetLastUpdateTime: TDateTime; virtual; abstract;
    function GetStateChange: TNotifyEvent; virtual; abstract;
    procedure SetStateChange(const sc: TNotifyEvent); virtual; abstract;
    function GetWarnAdj: TWarnAdjEvent; virtual; abstract;
    procedure SetWarnAdj(const wa: TWarnAdjEvent); virtual; abstract;
    function GetOnExit: TNotifyEvent; virtual; abstract;
    procedure SetOnExit(const ex: TNotifyEvent); virtual; abstract;
    function GetServer: boolean; virtual; abstract;
    procedure SetServer(const sv: boolean); virtual; abstract;
    procedure SetConfig(const cfg: TServerConfigBlock); virtual; abstract;
    function GetConfig: TServerConfigBlock; virtual; abstract;
    procedure ForceUpdate; virtual; abstract; // forces a CONFIGURATION update
    function UpdateNow: boolean; virtual; abstract; // forces a TIME update
    procedure KillEverything; virtual; abstract;
    property Active: boolean read GetActive;
    property Status: TSyncStatus read GetStatus;
    property LastUpdateTime: TDateTime read GetLastUpdateTime;
    property OnStateChange: TNotifyEvent read GetStateChange write SetStateChange;
    property OnWarnAdj: TWarnAdjEvent read GetWarnAdj write SetWarnAdj;
    property OnExitNow: TNotifyEvent read GetOnExit write SetOnExit;
    property Server: boolean read GetServer write SetServer;
    property Config: TServerConfigBlock read GetConfig write SetConfig;
  end;

  EServerRunning = class(Exception)
  end;

const
  DefaultSyncFreq = 600;
  DefaultLostSync = 7500;
  DefaultRetry = 600;
  DefaultWarnAdj = 120;
  DefaultProtocol = ttpNTP;

  ExNameUI = 'NetTimeGHJM_UI';
  ExNameServer = 'NetTimeGHJM_Server';

type
  TServerData = record
    Host: ShortString;
    Time: TDateTime;
    NetLag: TDateTime;
    RetrievalTime: TDateTime;
    Status: TSyncServerStatus;
    Done: boolean;
  end;
  TServerDataArray = array[0..MaxServerList-1] of TServerData;
  PServerDataArray = ^TServerDataArray;
  TServerDataSort = (sdsByTime, sdsByNetlag);

procedure SortServerData(const Arr: PServerDataArray; const Count: integer;
  const WhichSort: TServerDataSort; const Ascending: boolean);
procedure NormalizeTimes(const Arr: PServerDataArray; const Count: integer);

function DefaultPortForProtocol(const Proto: TTimeProto): integer;
function WinExecAndWait(Path: PChar; Visibility: Word): integer;

implementation

function DefaultPortForProtocol(const Proto: TTimeProto): integer;
begin
  case Proto of
    ttpRFC868_UDP, ttpRFC868_TCP: result := RFC868_Port;
    ttpNTP: result := NTP_Port;
  else
    result := 0;
  end;
end;

function WinExecAndWait(Path: PChar; Visibility: Word): integer;

var
  Msg: TMsg;
  lpExitCode: cardinal;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;

begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    wShowWindow := visibility;
  end;
  if CreateProcess(nil, path, nil, nil, False, NORMAL_PRIORITY_CLASS, nil,
    nil, StartupInfo, ProcessInfo) then
    begin
      repeat
        while PeekMessage(Msg, 0, 0, 0, pm_Remove) do
          begin
            if Msg.Message = wm_Quit then Halt(Msg.WParam);
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
        GetExitCodeProcess(ProcessInfo.hProcess,lpExitCode);
      until lpExitCode <> Still_Active;
      with ProcessInfo do
        begin
          CloseHandle(hThread);
          CloseHandle(hProcess);
        end;
      Result := 0;
    end
  else
    Result := GetLastError;
end;

procedure NormalizeTimes(const Arr: PServerDataArray; const Count: integer);

var
  CalcNow: TDateTime;
  i: integer;

begin
  if Count = 0 then
    raise exception.create('Cannot normalize a list of zero length');
  CalcNow := Now;
  for i := 0 to Count-1 do
    Arr[i].Time := Arr[i].Time + (CalcNow - Arr[i].RetrievalTime);
end;

procedure SortServerData(const Arr: PServerDataArray; const Count: integer;
  const WhichSort: TServerDataSort; const Ascending: boolean);

var
  done: boolean;
  i: integer;
  OutOfOrder: boolean;
  TmpData: TServerData;

begin
  repeat
    done := true;
    for i := 0 to Count-2 do
      begin
        if Ascending then
          if WhichSort = sdsByTime then
            OutOfOrder := Arr[i].Time > Arr[i+1].Time
          else
            OutOfOrder := Arr[i].NetLag > Arr[i+1].NetLag
        else
          if WhichSort = sdsByTime then
            OutOfOrder := Arr[i+1].Time > Arr[i].Time
          else
            OutOfOrder := Arr[i+1].NetLag > Arr[i].NetLag;
        if OutOfOrder then
          begin
            TmpData := Arr[i];
            Arr[i] := Arr[i+1];
            Arr[i+1] := TmpData;
            done := false;
          end;
      end;
  until done;
end;

function TNetTimeServerBase.GetSynchronized: boolean;
begin
  result := GetStatus.Synchronized;
end;

end.
