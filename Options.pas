{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit Options;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, configobj, NetTimeCommon;

type

  TfrmOptions = class;

  TfrmOptions = class(TForm)
    Label1: TLabel;
    edHostname: TEdit;
    Label2: TLabel;
    edTimeFreq: TEdit;
    Label3: TLabel;
    edLostSync: TEdit;
    Label4: TLabel;
    edWarnAdj: TEdit;
    btnFindServer: TButton;
    Label5: TLabel;
    edRetry: TEdit;
    cbxProtocol: TComboBox;
    CbxNeverWarn: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbxServer: TCheckBox;
    Label7: TLabel;
    edPort: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    edHostname1: TEdit;
    btnFindServer1: TButton;
    cbxProtocol1: TComboBox;
    edPort1: TEdit;
    edHostname2: TEdit;
    btnFindServer2: TButton;
    cbxProtocol2: TComboBox;
    edPort2: TEdit;
    edHostname3: TEdit;
    btnFindServer3: TButton;
    cbxProtocol3: TComboBox;
    edPort3: TEdit;
    edHostname4: TEdit;
    btnFindServer4: TButton;
    cbxProtocol4: TComboBox;
    edPort4: TEdit;
    btnHelp: TButton;
    btnAutoConfig: TButton;
    cbxShowInTray: TCheckBox;
    cbxServiceAutoStart: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnFindServerClick(Sender: TObject);
    procedure CbxNeverWarnClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxProtocolChange(Sender: TObject);
    procedure btnAutoConfigClick(Sender: TObject);
  private
    co: TConfigObj;
    Hostnames: array[0..MaxServers-1] of TEdit;
    Protocols: array[0..MaxServers-1] of TComboBox;
    Ports: array[0..MaxServers-1] of TEdit;
    FindButtons: array[0..MaxServers-1] of TButton;
  public
    tt: TNetTimeServerBase;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadFromRegistry;
    procedure ReadFromRunning(tt: TNetTimeServerBase);
    procedure ReadFromObject;
    procedure WriteToRegistry;
    procedure WriteToRunning(tt: TNetTimeServerBase);
    procedure WriteToObject;
  end;

implementation

{$R *.DFM}

uses registry, findhost, timewrap, autoconfig;

constructor TfrmOptions.Create(AOwner: TComponent);
begin
  inherited;
  co := TConfigObj.Create;
  ReadFromObject;
end;

destructor TfrmOptions.Destroy;
begin
  co.Free;
  inherited;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  if Assigned(tt) then
    ReadFromRunning(tt);
end;

procedure TfrmOptions.btnFindServerClick(Sender: TObject);

var
  fFH: TfrmFindServer;
  s: string;
  p: integer;
  i: integer;
  n: integer;

begin
  n := -1;
  for i := 0 to MaxServers-1 do
    if Sender = FindButtons[i] then
      n := i;
  if n = -1 then
    exit;
  fFH := TfrmFindServer.Create(nil);
  if fFH.ShowModal = mrOK then
    begin
      if (fFH.lstSuccess.ItemIndex >= 0) then
        begin
          s := fFH.lstSuccess.Items[fFH.lstSuccess.ItemIndex];
          p := pos(' ',s);
          if p > 0 then
            s := copy(s,1,p-1);
          Hostnames[n].Text := s;
          Protocols[n].ItemIndex := 0;
          Ports[n].Text := inttostr(NTP_Port);
        end;
    end;
  fFH.Release;
end;

procedure TfrmOptions.ReadFromRegistry;
begin
  co.ReadFromRegistry;
  ReadFromObject;
end;

procedure TfrmOptions.ReadFromRunning(tt: TNetTimeServerBase);
begin
  co.ReadFromRegistry;
  co.ReadFromRunning(tt);
  ReadFromObject;
end;

procedure TfrmOptions.ReadFromObject;

var
  i: integer;

  procedure ReadServer(const n: integer);
  begin
    Hostnames[n].Text := co.Servers[n].Hostname;
    Protocols[n].ItemIndex := integer(co.Servers[n].Protocol);
    Ports[n].Text := inttostr(co.Servers[n].Port);
  end;

begin
  for i := 0 to co.ServerCount-1 do
    ReadServer(i);
  edTimeFreq.Text := inttostr(co.SyncFreq);
  edLostSync.Text := inttostr(co.LostSync);
  edWarnAdj.Text := inttostr(co.WarnAdj);
  cbxNeverWarn.Checked := (edWarnAdj.Text = '0');
  edWarnAdj.Enabled := not cbxNeverWarn.Checked;
  edRetry.Text := inttostr(co.Retry);
  cbxServer.Checked := co.Server;
  cbxShowInTray.Checked := co.LoadOnLogin;
  cbxServiceAutoStart.Checked := co.ServiceOnBoot;
end;

procedure TfrmOptions.WriteToRegistry;
begin
  WriteToObject;
  co.WriteToRegistry;
end;

procedure TfrmOptions.WriteToRunning(tt: TNetTimeServerBase);
begin
  WriteToObject;
  co.WriteToRunning(tt);
end;

procedure TfrmOptions.WriteToObject;

var
  i: integer;

  procedure WriteServer(const n: integer);

  var
    Srv: TServerDef;

  begin
    if Hostnames[n].Text = '' then
      exit;
    Srv.Hostname := Hostnames[n].Text;
    Srv.Protocol := TTimeProto(Protocols[n].ItemIndex);
    Srv.Port := strtoint(Ports[n].Text);
    co.AddServer(Srv);
  end;

begin
  co.ClearServerList;
  for i := 0 to MaxServers-1 do
    WriteServer(i);
  co.SyncFreq := strtoint(edTimeFreq.Text);
  co.LostSync := strtoint(edLostSync.Text);
  co.WarnAdj := strtoint(edWarnAdj.Text);
  co.Retry := strtoint(edRetry.Text);
  co.Server := cbxServer.Checked;
  co.LoadOnLogin := cbxShowInTray.Checked;
  co.ServiceOnBoot := cbxServiceAutoStart.Checked;
end;

procedure TfrmOptions.CbxNeverWarnClick(Sender: TObject);
begin
  if CbxNeverWarn.Checked then
    begin
      EdWarnAdj.Text := '0';
      EdWarnAdj.Enabled := false;
    end
  else
    begin
      if EdWarnAdj.Text = '0' then
        EdWarnAdj.Text := inttostr(DefaultRetry);
      EdWarnAdj.Enabled := true;
    end;
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);

var
  Problems: string;

begin
  WriteToObject;
  Problems := '';
  if (co.SyncFreq < 600) or (co.Retry < 600) then
    Problems := Problems + '* Update intervals lower than ten minutes are strongly '+
      'discouraged when synchronizing to public servers, in order to avoid excessive '+
      'bandwidth costs for the server operators.' + #13#10 + #13#10;
  if co.Retry < co.SyncFreq then
    Problems := Problems + '* The retry interval is the time to wait when a server is '+
      'down. This should be higher than the normal sync interval, to avoid '+
      'creating heavy traffic to a server that can''t handle it.' + #13#10 + #13#10;
  if co.LostSync < (co.SyncFreq + (co.Retry * 2)) then
    Problems := Problems + '* The Max Free Run interval is the maximum amount of time '+
      'that can elapse before we consider the local clock to be out of sync. It is '+
      'recommended that this be long enough to allow at least two retries.' + #13#10 + #13#10;
  if Problems <> '' then
    if MessageDlg('The following problems were found with your configuration:' + #13#10 + #13#10 +
      Problems + 'Do you want to correct these problems automatically?',
      mtWarning, [mbYes,mbNo], 0) = mrYes then
      begin
        if co.SyncFreq < 600 then
          co.SyncFreq := 600;
        if co.Retry < 600 then
          co.Retry := 600;
        if co.Retry < co.SyncFreq then
          co.Retry := co.SyncFreq;
        if co.LostSync < (co.SyncFreq + (co.Retry * 2)) then
          co.LostSync := (co.SyncFreq + (co.Retry * 2));
        ReadFromObject;
      end;
  WriteToRegistry;
  if Assigned(tt) then
    WriteToRunning(tt);
end;

procedure TfrmOptions.btnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT,Self.HelpContext);
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  Hostnames[0] := edHostname;
  Hostnames[1] := edHostname1;
  Hostnames[2] := edHostname2;
  Hostnames[3] := edHostname3;
  Hostnames[4] := edHostname4;
  Protocols[0] := cbxProtocol;
  Protocols[1] := cbxProtocol1;
  Protocols[2] := cbxProtocol2;
  Protocols[3] := cbxProtocol3;
  Protocols[4] := cbxProtocol4;
  Ports[0] := edPort;
  Ports[1] := edPort1;
  Ports[2] := edPort2;
  Ports[3] := edPort3;
  Ports[4] := edPort4;
  FindButtons[0] := btnFindServer;
  FindButtons[1] := btnFindServer1;
  FindButtons[2] := btnFindServer2;
  FindButtons[3] := btnFindServer3;
  FindButtons[4] := btnFindServer4;
end;

procedure TfrmOptions.cbxProtocolChange(Sender: TObject);

var
  i: integer;

begin
  for i := 0 to MaxServers-1 do
    if Sender = Protocols[i] then
      Ports[i].Text := inttostr(
        DefaultPortForProtocol(TTimeProto(Protocols[i].ItemIndex)));
end;

procedure TfrmOptions.btnAutoConfigClick(Sender: TObject);

var
  fAC: TfrmAutoConfigure;

  function UpToComma(const s: string): string;

  var
    p: integer;

  begin
    p := pos(',',s);
    if p = 0 then
      result := s
    else
      result := copy(s,1,p-1);
  end;

  procedure SetItUp(const idx: integer; const hn: string);
  begin
    if hn <> '' then
      begin
        Hostnames[idx].Text := hn;
        Protocols[idx].ItemIndex := 0;
        Ports[idx].Text := inttostr(NTP_Port);
      end
    else
      begin
        Hostnames[idx].Text := '';
        Protocols[idx].ItemIndex := -1;
        Ports[idx].Text := '';
      end;
  end;

begin
  fAC := TfrmAutoConfigure.Create(Application);
  try
    if fAC.ShowModal = mrOK then
      begin
        SetItUp(0,UpToComma(fAC.lblServer1.Caption));
        SetItUp(1,UpToComma(fAC.lblServer2.Caption));
        SetItUp(2,UpToComma(fAC.lblServer3.Caption));
        SetItUp(3,UpToComma(fAC.lblServer4.Caption));
        SetItUp(4,UpToComma(fAC.lblServer5.Caption));
      end;
  finally
    fAC.Free;
  end;
end;

end.
