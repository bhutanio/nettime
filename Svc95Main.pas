unit Svc95Main;

interface

procedure ServiceMain;

implementation

uses Windows, Messages, SysUtils, ActiveX, ComObj, ComServ, ConfigObj,
  NetTimeCommon, NetTimeThread, winerr, WinSvc;

const
  RPC_C_AUTHN_LEVEL_NONE = 1;
  RPC_C_IMP_LEVEL_IMPERSONATE = 3;
  EOAC_NONE = 0;

var
  DoneExit: boolean;

type
  TSetDoneObj = class
    class procedure DoExitNow(Sender: TObject);
  end;

class procedure TSetDoneObj.DoExitNow(Sender: TObject);
begin
  DoneExit := true;
end;

procedure ServiceMain;

var
  co: TConfigObj;
  tt: TNetTimeServer;
  Msg: TMsg;
  result: LongBool;

begin
  tt := TNetTimeServer.Create;
  co := TConfigObj.Create;
  try
    co.ReadFromRegistry;
    co.WriteToRunning(tt);
  finally
    co.Free;
  end;
  if (tt.Config.ServerCount = 0) then
    raise exception.create('NetTime has not been configured');
  tt.OnExitNow := TSetDoneObj.DoExitNow;
  tt.Start;
  DoneExit := false;
  repeat
    result := PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
    if result then
      begin
        if (Msg.Message = WM_QUIT) or (Msg.Message = WM_ENDSESSION) then
          DoneExit := true;
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end
    else
      Sleep(GUISleepTime);
  until DoneExit;
  tt.Stop;
  CoUninitialize;
end;

end.
