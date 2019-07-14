unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, UI.Standard, UI.Toast, FMX.Layouts,
  UI.ListView, UI.Dialog, FMX.DateTimeCtrls, UI.VKhelper;

type
  TForm1 = class(TForm)
    ToastManager1: TToastManager;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  UI.Async,
  UI.Frame,
  uFrame1;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TFrameView.SetDefaultStatusColor($ff800080);
  //TFrameView.SetDefaultBackColor($fff1f2f3);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TFrame1.ShowFrame(Self, 'FMXUI Base Frame Demo');
end;

end.
