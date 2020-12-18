program nettime;

uses
  Forms,
  Dialogs,
  SysUtils,
  tclfrm in 'tclfrm.pas' {frmMain},
  timeconv in 'timeconv.pas',
  TrayIcon in 'Trayicon.pas',
  Options in 'Options.pas' {frmOptions},
  About in 'About.pas' {frmAbout},
  warning in 'warning.pas' {frmWarning},
  findhost in 'findhost.pas' {frmFindServer},
  ntptime in 'ntptime.pas',
  unixtime in 'unixtime.pas',
  timewrap in 'timewrap.pas',
  ConfigObj in 'ConfigObj.pas',
  NetTimeThread in 'NetTimeThread.pas',
  IsWinNT in 'IsWinNT.pas',
  mutex in 'mutex.pas',
  winerr in 'winerr.pas',
  NetTimeClient in 'NetTimeClient.pas',
  NetTimeCommon in 'NetTimeCommon.pas',
  WinsockUtil in 'WinsockUtil.pas',
  NetTimeIPC in 'NetTimeIPC.pas',
  autoconfig in 'autoconfig.pas' {frmAutoConfigure},
  serverlist in 'serverlist.pas';

{$R *.RES}

begin
  if not GetExclusivity(ExNameUI) then
    exit;
  Application.Initialize;
  Application.ShowMainForm := false;
  Application.Title := 'Network Time Synchronizer';
  Application.HelpFile := 'NETTIME.HLP';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAutoConfigure, frmAutoConfigure);
  if not ((ParamCount = 1) and (uppercase(ParamStr(1)) = '/NOSPLASH')) then
    DoSplash;
  try
    frmMain.DoAppStartup;
  except
    ShowMessage('Initialization failure.');
    halt;
  end;
  
  Application.Run;
end.
