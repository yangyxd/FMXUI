unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, UI.Standard, UI.Toast;

type
  TForm1 = class(TForm)
    ToastManager1: TToastManager;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  uFrameLogin;

procedure TForm1.FormShow(Sender: TObject);
begin
  TFrmaeLogin.ShowFrame(Self, 'Frame Demo');
end;

end.
