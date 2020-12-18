program NeTmSv95;

uses
  Windows,
  SysUtils,
  IsWinNt,
  Dialogs,
  NetTimeThread in 'NetTimeThread.pas',
  ConfigObj in 'ConfigObj.pas',
  Svc95Main in 'Svc95Main.pas',
  mutex in 'mutex.pas',
  NetTimeCommon in 'NetTimeCommon.pas';

{$R *.RES}

type
  TRegSvcFunc = function(dwProcessID, dwType: DWord) : DWord; stdcall;

var
  RegisterServiceProcess: TRegSvcFunc;

procedure MapRSPFunc;

var
  lib: THandle;

begin
  lib := LoadLibrary('kernel32.dll'); // if this fails, we're done for
  if lib = 0 then
    raise exception.create('Cannot load kernel32.dll!!! Man the lifeboats');
  RegisterServiceProcess := GetProcAddress(lib,'RegisterServiceProcess');
  if @RegisterServiceProcess = nil then
    raise exception.create('Cannot map RegisterServiceProcess from kernel32.dll - are you sure this is Windows 95/98/ME?');
end;

begin
  if not GetExclusivity(ExNameServer) then
    begin
      ShowMessage('Cannot load NetTime server: Another server is already running');
      exit;
    end;
  if IsWindowsNT then
    begin
      if MessageDlg('This program is intended as a Windows 95/98 service. On '+
        'Windows NT or 2000, you should install the NT service instead. '+
        'Running this should not cause any problems, but it will run as an '+
        'application, not a service. Do you want to run this program anyway?',
        mtWarning,[mbYes,mbNo],0) = idNo then
        abort;
    end
  else if not ((ParamCount = 1) and (uppercase(ParamStr(1)) = '/NOSERVICE')) then
    begin
      MapRSPFunc;
      RegisterServiceProcess(0,1);
    end;
  ServiceMain;
end.
