{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit timewrap;

interface

uses Windows, Classes, SysUtils, NetTimeCommon, WinSockUtil;

procedure GetTimeFromServer(const h: string; const protocol: TTimeProto;
  const Port: integer; var status: TSyncServerStatus; var Time: TDateTime;
  var NetLag: TDateTime);

procedure GetTimeFromServerAsync(const h: string; const protocol: TTimeProto;
  const Port: integer; var status: TSyncServerStatus; var Time: TDateTime;
  var NetLag: TDateTime; var Done: boolean);

procedure FigureBestKnownTime(const ServerCount: integer;
  const Servers: TServerDefArray; var Status: TSyncStatus; var Time: TDateTime);

implementation

uses unixtime, ntptime;

procedure GetTimeFromServer(const h: string; const protocol: TTimeProto;
  const Port: integer; var Status: TSyncServerStatus; var Time: TDateTime;
  var NetLag: TDateTime);
begin
  case protocol of
    ttpRFC868_UDP: GetTimeFromHost(h,port,true,status,time,netlag);
    ttpRFC868_TCP: GetTimeFromHost(h,port,false,status,time,netlag);
    ttpNTP:        GetTimeFromNTP(h,port,status,time,netlag);
  else
    status := ssUnconfigured;
  end;
end;

type
  PBoolean = ^boolean;
  PStatus = ^TSyncServerStatus;
  PDateTime = ^TDateTime;
  TRetrieverThread = class(TThread)
  protected
    FHost: string;
    FProtocol: TTimeProto;
    FPort: integer;
    FStatusPtr: PStatus;
    FTimePtr: PDateTime;
    FNetLagPtr: PDateTime;
    FDonePtr: PBoolean;
    procedure Execute; override;
  public
    constructor Create(const h: string; const protocol: TTimeProto;
      const Port: integer; const StatusPtr: PStatus; const TimePtr: PDateTime;
      const NetLagPtr: PDateTime; const DonePtr: PBoolean);
  end;

constructor TRetrieverThread.Create(const h: string; const protocol: TTimeProto;
  const Port: integer; const StatusPtr: PStatus; const TimePtr: PDateTime;
  const NetLagPtr: PDateTime; const DonePtr: PBoolean);
begin
  inherited Create(true);
  FreeOnTerminate := true;
  FHost := h;
  FProtocol := protocol;
  FPort := Port;
  FStatusPtr := StatusPtr;
  FTimePtr := TimePtr;
  FNetLagPtr := NetLagPtr;
  FDonePtr := DonePtr;
  DonePtr^ := false;
  Resume;
end;

procedure TRetrieverThread.Execute;

var
  SStatus: TSyncServerStatus;
  STime, SNetLag: TDateTime;

begin
  try
    try
      GetTimeFromServer(FHost, FProtocol, FPort, SStatus, STime, SNetLag);
      FStatusPtr^ := SStatus;
      FTimePtr^ := STime;
      FNetLagPtr^ := SNetLag;
    except
      FStatusPtr^ := ssFailed;
    end;
  finally
    FDonePtr^ := true;
  end;
end;

procedure GetTimeFromServerAsync(const h: string; const protocol: TTimeProto;
  const Port: integer; var Status: TSyncServerStatus; var Time: TDateTime;
  var NetLag: TDateTime; var Done: boolean);
begin
  TRetrieverThread.Create(h,protocol,port,@status,@time,@netlag,@done);
end;

procedure FigureBestKnownTime(const ServerCount: integer;
  const Servers: TServerDefArray; var Status: TSyncStatus; var Time: TDateTime);

var
  i: integer;
  GotCount: integer;
  ThrdData, CalcData: array[0..MaxServers-1] of TServerData;
  AllDone: boolean;

begin

  for i := ServerCount to MaxServers-1 do
    Status.ss[i] := ssUnconfigured;

  if not HaveLocalAddress then
    begin
      Status.Synchronized := false;
      for i := 0 to ServerCount-1 do
        Status.ss[i] := ssFailed;
      Time := Now;
      exit;
    end;

  // Retrieve all server times
  for i := 0 to ServerCount-1 do
    begin
      ThrdData[i].RetrievalTime := 0;
      Status.ss[i] := ssFailed;
    end;
  for i := 0 to ServerCount-1 do
    GetTimeFromServerAsync(Servers[i].Hostname, Servers[i].Protocol,
      Servers[i].Port, Status.ss[i], ThrdData[i].Time, ThrdData[i].NetLag,
      ThrdData[i].Done);
  repeat
    Sleep(GUISleepTime);
    AllDone := true;
    for i := 0 to ServerCount-1 do
      if not ThrdData[i].Done then
        AllDone := false
      else if ThrdData[i].RetrievalTime = 0 then
        ThrdData[i].RetrievalTime := Now;
  until AllDone;

  // Extract only those times that were good
  GotCount := 0;
  for i := 0 to ServerCount-1 do
    if (Status.ss[i] = ssGood) then
      begin
        CalcData[GotCount] := ThrdData[i];
        inc(GotCount);
      end;

  // If no good times, overall result is false
  if GotCount = 0 then
    begin
      Status.Synchronized := false;
      exit;
    end
  else
    Status.Synchronized := true;

  //TODO: find a better strategy here.
  NormalizeTimes(@CalcData,GotCount);
  SortServerData(@CalcData,GotCount,sdsByTime,true);
  Time := CalcData[GotCount div 2].Time;

end;

end.
