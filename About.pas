{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, jpeg;

type
  TfrmAbout = class(TForm)
    Image1: TImage;
    btnOk: TButton;
    lblVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  end;

procedure DoSplash;

implementation

{$R *.DFM}

procedure DoSplash;

var
  fA: TfrmAbout;
  start: longword;

begin
  fA := TfrmAbout.Create(Application);
  fA.BorderStyle := bsNone;
  fA.Caption := '';
  fA.btnOK.Visible := false;
  fA.Width := fA.Image1.Width;
  fA.Height := fA.Image1.Height;
  fA.Show;
  start := GetTickCount;
  while (GetTickCount - start) < 2500 do
    Application.ProcessMessages;
  fA.Release;
  Application.ProcessMessages;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);

var
  VerSize: cardinal;
  VerHandle: cardinal;
  VerPtr: pointer;
  VerPchar: pchar;
  VerLen: cardinal;

begin
  VerSize := GetFileVersionInfoSize(pchar(ParamStr(0)),VerHandle);
  if VerSize = 0 then
    exit;
  VerPtr := pointer(GlobalAlloc(GPTR,VerSize));
  try
    GetFileVersionInfo(pchar(ParamStr(0)),VerHandle,VerSize,VerPtr);
    if not VerQueryValue(VerPtr,'\StringFileInfo\040904E4\FileVersion',
      pointer(VerPchar),VerLen) then
      exit;
    lblVersion.Caption := 'Version '+VerPchar;
  finally
    GlobalFree(cardinal(VerPtr));
  end;
end;

end.
