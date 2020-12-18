unit WinsockUtil;

interface

uses Classes, Dialogs, SysUtils, Winsock;

var
  WSData: WSAData;

function StrToAddr(const s: string): longword;
procedure QuickSendUDP(const h: string; const port: integer;
  var Data; const DataLen: integer);
procedure GetLocalAddresses(const s: TStrings);
function HaveLocalAddress: boolean;

type
  TQuickUDPServerThread = class(TThread)
  private
    FListenPort: Word;
    listener: sockaddr_in;
    arg: integer;
    ExceptMsg: string;
    procedure ShowError;
  protected
    Sock: TSocket;
    Remote: sockaddr_in;
    Req: array[1..1024] of byte;
    Req_Len: integer;
    procedure Execute; override;
    procedure DoRequest; virtual; abstract;
  public
    constructor Create(const Suspended: boolean; const ListenPort: Word);
  end;

implementation

type
  PLongWord = ^LongWord;

function StrToAddr(const s: string): longword;

var
  pHE: PHostEnt;

begin
  result := inet_addr(pchar(s));
  if result = longword(INADDR_NONE) then
    begin
      pHE := GetHostByName(pchar(s));
      if pHE = nil then
        result := longword(INADDR_NONE)
      else
        result := LongWord(PLongWord(pHE^.h_addr_list^)^);
    end;
end;

function HaveLocalAddress: boolean;

var
  sl: TStringList;

begin
  sl := TStringList.Create;
  try
    GetLocalAddresses(sl);
    result := (sl.Count <> 0) and ((sl.Count > 1) or (sl[0] <> '127.0.0.1'));
  finally
    sl.Free;
  end;
end;

procedure GetLocalAddresses(const s: TStrings);

type
  TaPInAddr = array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;

var
  i: integer;
  HostAddr: PHostEnt;
  PAdrPtr: PaPInAddr;
  HostName: string;

begin
  s.Clear;
  setlength(HostName,255);
  GetHostName(pchar(HostName),255);
  setlength(HostName,strlen(pchar(HostName)));
  HostAddr := GetHostByName(PChar(HostName));
  if HostAddr = nil then
    exit;
  PAdrPtr := PAPInAddr(HostAddr^.h_addr_list);
  i := 0;
  while PAdrPtr^[i] <> nil do
  begin
    s.Add(inet_ntoa(PAdrPtr^[I]^));
    Inc(I);
  end;
end;


procedure QuickSendUDP(const h: string; const port: integer;
  var Data; const DataLen: integer);

var
  addr: sockaddr_in;
  sock: TSocket;

begin
  addr.sin_family := AF_INET;
  addr.sin_addr.S_addr := StrToAddr(h);
  addr.sin_port := htons(port);
  sock := Socket(AF_INET, SOCK_DGRAM, 0);
  if sock = INVALID_SOCKET then
    raise exception.create('Could not create socket');
  try
    if sendto(sock,Data,DataLen,0,addr,sizeof(addr)) = SOCKET_ERROR then
      raise exception.create('Could not send packet');
  finally
    closesocket(sock);
  end;
end;

constructor TQuickUDPServerThread.Create(const Suspended: boolean; const ListenPort: Word);
begin
  inherited Create(true);
  FListenPort := ListenPort;
  if not Suspended then
    Resume;
end;

procedure TQuickUDPServerThread.ShowError;
begin
  ShowMessage(ExceptMsg);
end;

procedure TQuickUDPServerThread.Execute;
begin
  try
    FreeOnTerminate := true;
    sock := Socket(AF_INET, SOCK_DGRAM, 0);
    if sock = INVALID_SOCKET then
      raise exception.create('Could not allocate a socket: Winsock error '+inttostr(WSAGetLastError));
    arg := 10000; // 10 seconds
    if setsockopt(sock,SOL_SOCKET,SO_RCVTIMEO,@arg,sizeof(arg)) = SOCKET_ERROR then
      raise exception.create('Could set socket timeout: Winsock error '+inttostr(WSAGetLastError));
    fillchar(listener,sizeof(listener),0);
    listener.sin_family := AF_INET;
    listener.sin_addr.S_addr := INADDR_ANY;
    listener.sin_port := htons(FListenPort);
    if bind(sock,listener,sizeof(sockaddr_in)) = SOCKET_ERROR then
      raise exception.create('Cannot bind to address: Winsock error '+inttostr(WSAGetLastError));
    while not Terminated do
      begin
        arg := sizeof(sockaddr_in);
        Req_Len := recvfrom(sock,Req,sizeof(Req),0,remote,arg);
        if Req_Len <> SOCKET_ERROR then
          DoRequest;
      end;
    closesocket(sock);
  except
    on e: Exception do
      begin
        ExceptMsg := e.Message;
        Synchronize(ShowError);
      end;
  end;
end;

initialization
  WSAStartup($0101,WSData);
finalization
  WSACleanup;
end.
