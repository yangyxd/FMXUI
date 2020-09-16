unit Unit1;

interface

uses
  System.Generics.Collections,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  UI.Standard, UI.ListView, UI.Toast;

type
  TForm1 = class(TForm)
    ToastManager1: TToastManager;
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
  CustomList;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TfrmCustomList.ShowFrame(Self);
end;

end.
