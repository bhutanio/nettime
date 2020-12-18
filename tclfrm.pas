{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit tclfrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, trayicon, Menus, Buttons, ExtCtrls, NetTimeCommon,
  About, Options, NetTimeThread, NetTimeClient;

type
  TfrmMain = class(TForm)
    lblTime: TLabel;
    lblGoodness: TLabel;
    lblLastSync: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    mnuTray: TPopupMenu;
    Properties1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    Exit1: TMenuItem;
    imgBad: TImage;
    imgGood: TImage;
    btnSettings: TButton;
    Timer1: TTimer;
    Button1: TButton;
    lblSource: TLabel;
    btnUpdateNow: TButton;
    UpdateNow1: TMenuItem;
    imgWarn: TImage;
    lblServer1: TLabel;
    lblServer2: TLabel;
    lblServer3: TLabel;
    lblServer4: TLabel;
    lblServer5: TLabel;
    Label8: TLabel;
    btnStop: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Exit1Click(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnUpdateNowClick(Sender: TObject);
    procedure UpdateNow1Click(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    StopOnTimer: boolean;
    ServerLabels: array[0..MaxServers-1] of TLabel;
    procedure WMEndSession(var Msg: TWmEndSession); message WM_ENDSESSION;
  public
    tt: TNetTimeServerBase;
    ti: TTrayIcon;
    procedure DoAppStartup;
    procedure TimeStateChange(Sender: TObject);
    function WarnAdjust(const Sender: TObject;
      const ServerTime, StationTime: TDateTime): boolean;
    procedure DoExitNow(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses Warning, timewrap, iswinnt, mutex;

procedure TfrmMain.DoAppStartup;

var
  frmOpt: TfrmOptions;

begin
  ti := TTrayIcon.Create(Application);
  ti.ToolTip := 'Network Time Synchronization';
  ti.PopupMenu := mnuTray;
  ti.Icon := imgBad.Picture.Icon;
  ti.OnDblClick := Properties1Click;
  ti.Active := true;

  if GetExclusivity(ExNameServer) then
    begin
      lblSource.Caption := 'Server Type: In-Process Thread';
      lblSource.Visible := true;
      btnStop.Visible := false;
      tt := TNetTimeServer.Create;
      frmOpt := TfrmOptions.Create(Application);
      frmOpt.ReadFromRegistry;
      frmOpt.tt := tt;
      if (frmOpt.edHostname.Text = '') then
        frmOpt.ShowModal;
      if (frmOpt.edHostname.Text = '') then
        raise Exception.Create('Hostname not specified. Application cannot start.');
      frmOpt.WriteToRunning(tt);
      frmOpt.Release;
      (tt as TNetTimeServer).Start;
    end
  else
    begin
      lblSource.Caption := 'Server Type: Out-Of-Process Service';
      lblSource.Visible := true;
      btnStop.Left := lblSource.Left + lblSource.Width + 10;
      btnStop.Visible := true;
      tt := TNetTimeProxy.Create;
    end;
  tt.OnWarnAdj := WarnAdjust;
  tt.OnStateChange := TimeStateChange;
  tt.OnExitNow := DoExitNow;
  tt.ForceUpdate;
  TimeStateChange(Self);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  Self.Hide;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.Properties1Click(Sender: TObject);
begin
  Self.Show;
end;

procedure TfrmMain.BitBtn1Click(Sender: TObject);
begin
  Self.Hide;
end;

procedure TfrmMain.About1Click(Sender: TObject);

var
  fA: TfrmAbout;

begin
  fA := TfrmAbout.Create(Application);
  fA.ShowModal;
  fA.Release;
end;

procedure TfrmMain.btnSettingsClick(Sender: TObject);

var
  fO: TfrmOptions;

begin
  fO := TfrmOptions.Create(Application);
  fO.tt := tt;
  fO.ShowModal;
  fO.Release;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
  TimeStateChange(Sender);
end;

procedure TfrmMain.TimeStateChange(Sender: TObject);

var
  tip: string;
  i: integer;
  AllGood: boolean;

begin
  try
    tip := 'Network Time Synchronization';
    if tt.LastUpdateTime = 0 then
      lblLastSync.Caption := 'No synchronization yet'
    else
      begin
        lblLastSync.Caption := datetimetostr(tt.LastUpdateTime);
        tip := tip + #13#10 + 'Last Sync: '+datetimetostr(tt.LastUpdateTime);
      end;
    if (tt.Status.Synchronized) then
      begin
        lblGoodness.Caption := 'Time is synchronized.';
        AllGood := true;
        for i := 0 to MaxServers-1 do
          if not (tt.Status.ss[i] in [ssGood, ssUnconfigured]) then
            AllGood := false;
        if AllGood then
          ti.Icon := imgGood.Picture.Icon
        else
          ti.Icon := imgWarn.Picture.Icon;
      end
    else
      begin
        lblGoodness.Caption := 'CLOCK SYNC LOST!';
        tip := tip + #13#10 + lblGoodness.Caption;
        ti.Icon := imgBad.Picture.Icon;
      end;
    for i := 0 to MaxServers-1 do
      begin
        if tt.Status.ss[i] = ssGood then
          ServerLabels[i].Caption := 'Good'
        else if tt.Status.ss[i] = ssFailed then
          ServerLabels[i].Caption := 'Failed'
        else if tt.Status.ss[i] = ssWrong then
          ServerLabels[i].Caption := 'Wrong'
        else
          ServerLabels[i].Caption := '';
        if ServerLabels[i].Caption <> '' then
          ServerLabels[i].Caption := tt.Config.Servers[i].Hostname + ': ' +
            ServerLabels[i].Caption;
      end;
    ti.ToolTip := tip;
    if IsWindowsNT then
      SetProcessWorkingSetSize(GetCurrentProcess,$ffffffff,$ffffffff);
  except
  end;
end;

function TfrmMain.WarnAdjust(const Sender: TObject;
  const ServerTime, StationTime: TDateTime): boolean;

var
  fW: TfrmWarning;

begin
  fW := TfrmWarning.Create(Application);
  fW.lblServerTime.Caption := datetimetostr(ServerTime);
  fW.lblStationTime.Caption := datetimetostr(StationTime);
  fW.ShowModal;
  result := not fW.rbnShutdown.Checked;
  fw.Free;
end;

procedure TfrmMain.DoExitNow(Sender: TObject);
begin
  StopOnTimer := true;
  Timer1.Interval := 50;
  Timer1.Enabled := true;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  if StopOnTimer then
    begin
      Timer1.Enabled := false;
      if tt is TNetTimeServer then
        (tt as TNetTimeServer).Stop;
      tt.Free;
      ti.Free;
      Application.Terminate;
    end
  else
    lblTime.Caption := datetimetostr(Now);
end;

procedure TfrmMain.FormHide(Sender: TObject);
begin
  Timer1.Enabled := false;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Self.Hide;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ServerLabels[0] := lblServer1;
  ServerLabels[1] := lblServer2;
  ServerLabels[2] := lblServer3;
  ServerLabels[3] := lblServer4;
  ServerLabels[4] := lblServer5;
  StopOnTimer := false;
end;

procedure TfrmMain.btnUpdateNowClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    if not tt.UpdateNow then
      ShowMessage('Error: Could not update.');
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.UpdateNow1Click(Sender: TObject);
begin
  if tt.UpdateNow then
    ShowMessage('Update successful. The time is now '+DateTimeToStr(Now))
  else
    ShowMessage('Error: Could not update.');
end;

procedure TfrmMain.WMEndSession(var Msg: TWmEndSession);
begin
  Application.Terminate;
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  if MessageDlg('This shut down the currently running service and exit '+
    'NetTime. Are you sure you want to do this?', mtInformation,
    [mbYes,mbNo], 0) = mrYes then
    tt.KillEverything;
end;

end.
