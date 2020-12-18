{ ************************************************************************

   NetTime is copyrighted by Graham Mainwaring. Permission is hereby
   granted to use, modify, redistribute and create derivative works
   provided this attribution is not removed. I also request that if you
   make any useful changes, please e-mail the diffs to graham@mhn.org
   so that I can include them in an 'official' release.

  ************************************************************************ }

unit ServiceConfigFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmSvcConfig = class(TForm)
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSvcConfig: TfrmSvcConfig;

implementation

{$R *.DFM}

procedure TfrmSvcConfig.Button2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmSvcConfig.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Application.Terminate;
end;

end.
