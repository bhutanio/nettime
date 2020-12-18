unit mutex;

interface

uses Windows, SysUtils;

function GetExclusivity(const Name: string): boolean;
procedure ReleaseExclusivity(const Name: string);

implementation

uses Classes;

var
  HeldHandles: TStringList;

function GetExclusivity(const Name: string): boolean;

var
  res: HResult;

begin
  res := CreateMutex(nil,true,pchar(Name));
  if (res = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
    begin
      result := false;
      exit;
    end
  else
    begin
      HeldHandles.AddObject(Name,TObject(res));
      result := true;
    end;
end;

procedure ReleaseExclusivity(const Name: string);

var
  idx: integer;

begin
  idx := HeldHandles.IndexOf(Name);
  if idx = -1 then
    raise exception.create('We do not hold this handle');
  CloseHandle(integer(HeldHandles.Objects[idx]));
  HeldHandles.Delete(idx);
end;

initialization
  HeldHandles := TStringList.Create;
finalization
  HeldHandles.Free;
end.
