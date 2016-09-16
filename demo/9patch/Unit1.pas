unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, UI.Standard;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ButtonView1: TButtonView;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  //View1.Background.ItemDefault.Kind := TViewBrushKind.Bitmap;
end;

end.
