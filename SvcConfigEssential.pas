{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit SvcConfigEssential;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ServiceConfigFrm, StdCtrls;

type
  TfrmEssentialConfig = class(TfrmSvcConfig)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEssentialConfig: TfrmEssentialConfig;

implementation

uses SvcConfigYesNo, Options;

{$R *.DFM}

procedure TfrmEssentialConfig.Button1Click(Sender: TObject);

var
  opt: TfrmOptions;

begin
  inherited;
  opt := TfrmOptions.Create(Application);
  opt.ReadFromRegistry;
  opt.cbxServiceAutoStart.Checked := true;
  opt.cbxShowInTray.Checked := true;
  opt.ShowModal;
  opt.Release;
  if opt.cbxShowInTray.Checked then
    WinExec(pchar(InstDir+'NetTime.exe /NOSPLASH'),SW_SHOW);
  Application.Terminate;
end;

procedure TfrmEssentialConfig.Button2Click(Sender: TObject);
begin
  inherited;
  Application.Terminate;
end;

end.
