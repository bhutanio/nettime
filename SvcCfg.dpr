program SvcCfg;

uses
  Forms,
  SysUtils,
  ServiceConfigFrm in 'ServiceConfigFrm.pas' {frmSvcConfig},
  SvcConfigYesNo in 'SvcConfigYesNo.pas' {frmServiceYN},
  SvcConfigEssential in 'SvcConfigEssential.pas' {frmEssentialConfig},
  Options in 'Options.pas' {frmOptions},
  ConfigObj in 'ConfigObj.pas',
  SvcUninstall in 'SvcUninstall.pas',
  mutex in 'mutex.pas';

{$R *.RES}

begin
  if (ParamCount=2) and (uppercase(ParamStr(2))='/UNINSTALL') then
    begin
      Uninstall;
      exit;
    end;
  Application.Initialize;
  Application.HelpFile := 'NETTIME.HLP';
  Application.Title := 'NetTime Service Installer';
  Application.CreateForm(TfrmServiceYN, frmServiceYN);
  Application.CreateForm(TfrmEssentialConfig, frmEssentialConfig);
  Application.CreateForm(TfrmSvcConfig, frmSvcConfig);
  Application.Run;
end.
