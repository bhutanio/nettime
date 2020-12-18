{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit autoconfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Math, INIFiles, NetTimeCommon;

type
  TfrmAutoConfigure = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Shape1: TShape;
    lblTotal: TLabel;
    lblChecked: TLabel;
    Label3: TLabel;
    lblServer1: TLabel;
    lblServer2: TLabel;
    lblServer3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    Label4: TLabel;
    Label5: TLabel;
    lblGood: TLabel;
    lblBad: TLabel;
    lblServer5: TLabel;
    lblServer4: TLabel;
    btnMore: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnMoreClick(Sender: TObject);
  private
    AllServers: TStringList;
    ThrdData: PServerDataArray;
    DataCount: integer;
    procedure GetListOfServers;
    procedure ThrowOut(const n: integer);
    procedure SetLabel;
    procedure SelectServers;
  public
    { Public declarations }
  end;

var
  frmAutoConfigure: TfrmAutoConfigure;

implementation

{$R *.DFM}

uses timewrap, findhost, ntptime;

procedure TfrmAutoConfigure.FormCreate(Sender: TObject);
begin
  Self.Height := 89;
  lblTotal.Caption := '0';
  lblChecked.Caption := '';
  lblGood.Caption := '';
  lblBad.Caption := '';
  lblServer1.Caption := '';
  lblServer2.Caption := '';
  lblServer3.Caption := '';
  lblServer4.Caption := '';
  lblServer5.Caption := '';
  AllServers := TStringList.Create;
  ThrdData := nil;
end;

procedure TfrmAutoConfigure.GetListOfServers;

var
  ServerINI: TMemINIFile;
  SectionList: TStringList;
  ServerList: TStringList;
  i: integer;

begin
  AllServers.Clear;
  SectionList := TStringList.Create;
  ServerList := TStringList.Create;
  ServerINI := TMemIniFile.Create(ExtractFilePath(ParamStr(0))+'SERVERS.INI');
  try
    ServerINI.ReadSections(SectionList);
    for i := 0 to SectionList.Count-1 do
      begin
        ServerList.Clear;
        ServerINI.ReadSectionValues(SectionList[i],ServerList);
        AllServers.AddStrings(ServerList);
      end;
    lblTotal.Caption := inttostr(AllServers.Count);
    Application.ProcessMessages;
    FindServersViaBroadcast(ServerList);
    AllServers.AddStrings(ServerList);
    lblTotal.Caption := inttostr(AllServers.Count);
    Application.ProcessMessages;
  finally
    SectionList.Free;
    ServerList.Free;
    ServerINI.Free;
  end;
end;

procedure TfrmAutoConfigure.FormDestroy(Sender: TObject);
begin
  if ThrdData <> nil then
    GlobalFree(cardinal(ThrdData));
  AllServers.Free;
end;

procedure TfrmAutoConfigure.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TfrmAutoConfigure.ThrowOut(const n: integer);

var
  i: integer;

begin
  for i := n+1 to DataCount-1 do
    ThrdData^[i-1] := ThrdData^[i];
  dec(DataCount);
end;

procedure TfrmAutoConfigure.SetLabel;

var
  LagStamp: TTimeStamp;
  lbl: TLabel;

  function GetNextLabel: TLabel;
  begin
    if lblServer1.Tag = 0 then
      result := lblServer1
    else if lblServer2.Tag = 0 then
      result := lblServer2
    else if lblServer3.Tag = 0 then
      result := lblServer3
    else if lblServer4.Tag = 0 then
      result := lblServer4
    else if lblServer5.Tag = 0 then
      result := lblServer5
    else
      result := nil;
  end;

begin
  lbl := GetNextLabel;
  if lbl = nil then
    exit;
  lbl.Tag := 1;
  LagStamp := DateTimeToTimeStamp(ThrdData^[0].NetLag);
  Lbl.Caption := ThrdData^[0].Host + ', netlag = ' +
    inttostr(LagStamp.Time) + ' ms.';
  ThrowOut(0);
  btnMore.Visible := (DataCount >= 0) and (lbl <> lblServer5);
end;

procedure TfrmAutoConfigure.SelectServers;

var
  p: integer;
  TimeRef: TDateTime;
  TmpDataCount: integer;

begin
  TmpDataCount := DataCount;
  // Throw out the top and bottom range, to get rid of inaccurate
  // data. However, don't throw things out if they look okay.
  if TmpDataCount >= 3 then
    begin
      // throw out the top
      SortServerData(ThrdData, TmpDataCount, sdsByTime, true);
      p := (9 * TmpDataCount) div 10;
      TimeRef := ThrdData^[p].Time;
      while (p < TmpDataCount) and ((ThrdData^[p].Time - TimeRef) < (10*ms)) do
        inc(p);
      TmpDataCount := p;
      // throw out the bottom
      SortServerData(ThrdData, TmpDataCount, sdsByTime, false);
      p := (9 * TmpDataCount) div 10;
      TimeRef := ThrdData^[p].Time;
      while (p < TmpDataCount) and ((TimeRef - ThrdData^[p].Time) < (10*ms)) do
        inc(p);
      TmpDataCount := p;
    end;

  // Throw out all results with lag times more than double the best
  SortServerData(ThrdData, TmpDataCount, sdsByNetlag, true);
  p := 1;
  while (p < TmpDataCount) and (ThrdData^[p].NetLag <= min(5,ThrdData^[0].NetLag*2)) do
    inc(p);
  TmpDataCount := p;

  // Of the remaining, find the best by netlag
  SortServerData(ThrdData, TmpDataCount, sdsByNetlag, true);

  for p := 0 to min(TmpDataCount-1,4) do
    SetLabel;

end;

procedure TfrmAutoConfigure.Timer1Timer(Sender: TObject);

var
  i: integer;
  AllDone: boolean;
  DoneCount: integer;
  GoodCount, BadCount: integer;

begin
  Timer1.Enabled := false;

  GetListOfServers;

  ThrdData := pointer(
    GlobalAlloc(GMEM_FIXED,AllServers.Count * sizeof(TServerData)));
  if ThrdData = nil then
    raise exception.create('Could not allocate memory');

  // Retrieve all server times
  for i := 0 to AllServers.Count-1 do
    begin
      ThrdData^[i].RetrievalTime := 0;
      ThrdData^[i].Status := ssUnconfigured;
      ThrdData^[i].Host := AllServers[i];
      GetTimeFromServerAsync(AllServers[i], ttpNTP, NTP_Port,
        ThrdData^[i].Status, ThrdData^[i].Time, ThrdData^[i].NetLag,
        ThrdData^[i].Done);
    end;
  GoodCount := 0;
  BadCount := 0;
  repeat
    Sleep(GUISleepTime);
    AllDone := true;
    DoneCount := 0;
    for i := 0 to AllServers.Count-1 do
      if ThrdData^[i].Done then
        begin
          if ThrdData^[i].RetrievalTime = 0 then
            begin
              ThrdData^[i].RetrievalTime := Now;
              if ThrdData^[i].Status = ssGood then
                inc(GoodCount)
              else
                inc(BadCount);
            end;
          inc(DoneCount);
        end
      else
        AllDone := false;
    lblChecked.Caption := inttostr(DoneCount);
    lblGood.Caption := inttostr(GoodCount);
    lblBad.Caption := inttostr(BadCount);
    Application.ProcessMessages;
  until AllDone;

  // Throw out all the times that weren't good
  DataCount := AllServers.Count;
  repeat
    AllDone := true;
    for i := 0 to DataCount-1 do
      if ThrdData^[i].Status <> ssGood then
        begin
          AllDone := false;
          break;
        end;
    if not AllDone then
      ThrowOut(i);
  until AllDone;

  // Normalize all server times to now
  NormalizeTimes(ThrdData, DataCount);

  // Make sure we got at least one server
  if DataCount < 1 then
    begin
      ShowMessage('Could not connect to any servers. Either you are not '+
        'connected to the Internet, or you are behind a firewall that does '+
        'not allow NTP traffic. Contact your network administrator or ISP.');
      ModalResult := mrCancel;
    end
  else
    SelectServers;
  Self.Height := 232;
end;

procedure TfrmAutoConfigure.btnMoreClick(Sender: TObject);
begin
  SelectServers;
end;

end.
