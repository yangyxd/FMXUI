unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ExtCtrls,
  System.ImageList, FMX.ImgList, FMX.Objects, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.ListBox;

type
  TForm1 = class(TForm)
    LinearLayout2: TLinearLayout;
    RelativeLayout1: TRelativeLayout;
    View3: TView;
    Label2: TLabel;
    View4: TView;
    LinearLayout3: TLinearLayout;
    RelativeLayout3: TRelativeLayout;
    RelativeLayout4: TRelativeLayout;
    RelativeLayout5: TRelativeLayout;
    View5: TView;
    View6: TView;
    View7: TView;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure View5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ///
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Self.Fill.Color;
end;

procedure TForm1.View5Click(Sender: TObject);
begin
  Caption := Sender.ClassName;
end;

end.
