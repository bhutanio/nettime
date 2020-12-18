{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit timeconv;

interface

type
  TNTPTimestamp = record
    Seconds: longword;
    SubSeconds: longword;
  end;

function UnixTimeToDateTime(ut: longword): TDateTime;
function DateTimeToUnixTime(dt: TDateTime): longword;
function RFC868TimeToDateTime(rt: longint): TDateTime;
function DateTimeToRFC868Time(dt: TDateTime): longword;
function NTPToDateTime(const ntp: TNTPTimestamp): TDateTime;
function DateTimeToNTP(const dt: TDateTime): TNTPTimestamp;
function SecondsApart(const t1, t2: TDateTime): int64;

implementation

uses Windows, WinSock, SysUtils;

procedure SwapBytes(var b1, b2: byte);

var
  tmp: byte;

begin
  tmp := b1;
  b1 := b2;
  b2 := tmp;
end;

function SecondsApart(const t1, t2: TDateTime): int64;
var
  st: TSystemTime;
  ft1, ft2: TFileTime;
begin
  DateTimeToSystemTime(t1,st);
  SystemTimeToFileTime(st,ft1);
  DateTimeToSystemTime(t2,st);
  SystemTimeToFileTime(st,ft2);
  result := abs((int64(ft2) - int64(ft1)) div int64(10000000));
end;

const
  BaseDate1970 = 11644473600;
  BaseDate1900 = 9435484800;

function BaseDate(rfc: boolean): int64;
begin
  if rfc then
    result := BaseDate1900
  else
    result := BaseDate1970;
end;

function ConvertToDateTime(rfc: boolean; ut: longword): TDateTime;

var
  utctime: int64;
  localtime: int64;
  systemtime: TSystemTime;

begin
  utctime := (int64(ut) + BaseDate(rfc));
  utctime := utctime * int64(10000000);
  FileTimeToLocalFileTime(TFileTime(utctime),TFileTime(localtime));
  FileTimeToSystemTime(TFileTime(localtime),systemtime);
  result := SystemTimeToDateTime(systemtime);
end;

function ConvertFromDateTime(rfc: boolean; dt: TDateTime): longword;

var
  utctime: int64;
  localtime: int64;
  systemtime: TSystemTime;

begin
  DateTimeToSystemTime(dt,systemtime);
  SystemTimeToFileTime(systemtime,TFileTime(localtime));
  LocalFileTimeToFileTime(TFileTime(localtime),TFileTime(utctime));
  utctime := utctime div int64(10000000);
  result := utctime - BaseDate(rfc);
end;

function UnixTimeToDateTime(ut: longword): TDateTime;
begin
  result := ConvertToDateTime(false,ut);
end;

function DateTimeToUnixTime(dt: TDateTime): longword;
begin
  result := ConvertFromDateTime(false,dt);
end;

function RFC868TimeToDateTime(rt: longint): TDateTime;

var
  nt: longword;

begin
  nt := rt;
  nt := ntohl(nt);
  result := ConvertToDateTime(true,nt);
end;

function DateTimeToRFC868Time(dt: TDateTime): longword;
begin
  result := ConvertFromDateTime(true,dt);
  result := htonl(result);
end;

const
  OneMil = $100000000 div 1000; // ratio of milliseconds to secs/2^32

function NTPToDateTime(const ntp: TNTPTimestamp): TDateTime;

var
  timepart: longword;
  time: TSystemTime;

begin
  // first, figure out the "rough" time in seconds
  timepart := ntohl(ntp.Seconds);
  DateTimeToSystemTime(ConvertToDateTime(true,timepart),time);
  // now, add the "fine" time in 2^32nds of a second
  timepart := ntohl(ntp.SubSeconds);
  timepart := timepart div OneMil;
  time.wMilliseconds := time.wMilliseconds + timepart;
  time.wSecond := time.wSecond + (time.wMilliseconds div 1000);
  time.wMilliseconds := time.wMilliseconds mod 1000;
  result := SystemTimeToDateTime(time);
end;

function DateTimeToNTP(const dt: TDateTime): TNTPTimestamp;

var
  time: TSystemTime;

begin
  result.Seconds := htonl(ConvertFromDateTime(true,dt));
  DateTimeToSystemTime(dt,time);
  result.SubSeconds := htonl(time.wMilliseconds * OneMil);
end;

end.
