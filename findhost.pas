{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit findhost;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, IniFiles;

type
  TfrmFindServer = class(TForm)
    Button1: TButton;
    Label5: TLabel;
    lstSuccess: TListBox;
    Button3: TButton;
    Button4: TButton;
    tvCountries: TTreeView;
    lstServers: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure tvCountriesChange(Sender: TObject; Node: TTreeNode);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ServerINI: TMemIniFile;
  public
    { Public declarations }
  end;

var
  frmFindServer: TfrmFindServer;

implementation

{$R *.DFM}

uses timewrap, ntptime, NetTimeCommon;

procedure TfrmFindServer.Button1Click(Sender: TObject);

var
  status: TSyncServerStatus;
  time: TDateTime;
  netlag: TDateTime;
  stamp: TTimeStamp;

begin
  if lstServers.ItemIndex = -1 then
    begin
      ShowMessage('Please select a server.');
      exit;
    end;
  Screen.Cursor := crHourglass;
  try
    try
      GetTimeFromServer(lstServers.Items[lstServers.ItemIndex],
        ttpNTP, NTP_Port, status, time, netlag);
    except
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  if status <> ssGood then
    ShowMessage('Could not get the time from this host.')
  else
    begin
      stamp := DateTimeToTimeStamp(netlag);
      ShowMessage('Host reports the time to be '+datetimetostr(time)+
        '. Net lag is '+inttostr(stamp.time)+' ms.');
      lstSuccess.Items.Add(lstServers.Items[lstServers.ItemIndex] + ' (' +
        inttostr(stamp.time) + ' ms)');
    end;
end;

procedure TfrmFindServer.Button3Click(Sender: TObject);
begin
  if (lstSuccess.ItemIndex < 0) then
    begin
      ShowMessage('Please choose a server from the successes list prior to '+
        'clicking OK, or click Cancel to exit without selecting a server.');
      ModalResult := mrNone;
    end
  else
    ModalResult := mrOK;
end;

procedure TfrmFindServer.tvCountriesChange(Sender: TObject;
  Node: TTreeNode);

var
  SecName: string;

begin
  if (Node.HasChildren) or (Node.Parent = nil) then
    exit;
  SecName := Node.Text;
  if Node.Parent.Text = 'North America' then
    SecName := 'NA-' + SecName
  else if Node.Parent.Text = 'United States' then
    SecName := 'US-' + SecName
  else if Node.Parent.Text = 'South America' then
    SecName := 'SA-' + SecName
  else if Node.Parent.Text = 'Europe' then
    SecName := 'EU-' + SecName
  else if Node.Parent.Text = 'Asia' then
    SecName := 'AS-' + SecName
  else if Node.Parent.Text = 'Africa' then
    SecName := 'AF-' + SecName
  else if Node.Parent.Text = 'Oceania' then
    SecName := 'OC-' + SecName;
  lstServers.Clear;
  ServerINI.ReadSectionValues(SecName,lstServers.Items);
end;

procedure TfrmFindServer.Button2Click(Sender: TObject);
begin
  lstServers.Clear;
  Screen.Cursor := crHourglass;
  try
    FindServersViaBroadcast(lstServers.Items);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmFindServer.FormCreate(Sender: TObject);

var
  Sections: TStringList;
  cNA, cSA, cEU, cAS, cAF, cOC, cUS: TTreeNode;
  i: integer;
  Continent, Country: string;

begin
  ServerINI := TMemIniFile.Create(ExtractFilePath(ParamStr(0))+'SERVERS.INI');
  cNA := tvCountries.Items.Add(nil,'North America');
  cUS := tvCountries.Items.AddChild(cNA,'United States');
  cSA := tvCountries.Items.Add(nil,'South America');
  cEU := tvCountries.Items.Add(nil,'Europe');
  cAS := tvCountries.Items.Add(nil,'Asia');
  cAF := tvCountries.Items.Add(nil,'Africa');
  cOC := tvCountries.Items.Add(nil,'Oceania');
  Sections := TStringList.Create;
  try
    ServerINI.ReadSections(Sections);
    for i := 0 to Sections.Count-1 do
      begin
        Continent := uppercase(copy(Sections[i],1,2));
        Country := copy(Sections[i],4,length(Sections[i])-3);
        if Continent = 'NA' then
          tvCountries.Items.AddChild(cNA,Country)
        else if Continent = 'US' then
          tvCountries.Items.AddChild(cUS,Country)
        else if Continent = 'SA' then
          tvCountries.Items.AddChild(cSA,Country)
        else if Continent = 'EU' then
          tvCountries.Items.AddChild(cEU,Country)
        else if Continent = 'AS' then
          tvCountries.Items.AddChild(cAS,Country)
        else if Continent = 'AF' then
          tvCountries.Items.AddChild(cAF,Country)
        else if Continent = 'OC' then
          tvCountries.Items.AddChild(cOC,Country);
      end;
  finally
    Sections.Free;
  end;
end;

procedure TfrmFindServer.FormDestroy(Sender: TObject);
begin
  ServerINI.Free;
end;

end.
