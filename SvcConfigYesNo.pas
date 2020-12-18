{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit SvcConfigYesNo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ServiceConfigFrm, StdCtrls;

type
  TfrmServiceYN = class(TfrmSvcConfig)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmServiceYN: TfrmServiceYN;
  InstDir: string;

implementation

uses SvcConfigEssential;

{$R *.DFM}

procedure TfrmServiceYN.Button2Click(Sender: TObject);
begin
  inherited;
  Application.Terminate;
end;

procedure TfrmServiceYN.Button1Click(Sender: TObject);
begin
  inherited;
  Self.Hide;
  frmEssentialConfig.Show;
end;

procedure TfrmServiceYN.FormCreate(Sender: TObject);
begin
  inherited;
  if ParamCount <> 1 then
    begin
      ShowMessage('Please supply an installation directory.');
      Application.Terminate;
      exit;
    end;
  InstDir := ParamStr(1);
  if InstDir[length(InstDir)] <> '\' then
    InstDir := InstDir + '\';
end;

end.
