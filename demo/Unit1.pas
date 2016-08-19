unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ExtCtrls,
  FMX.ImgList, FMX.Objects, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView,
  FMX.ListBox, UI.Standard, System.ImageList;

type
  TForm1 = class(TForm)
    LinearLayout2: TLinearLayout;
    RelativeLayout1: TRelativeLayout;
    View3: TView;
    View4: TView;
    LinearLayout3: TLinearLayout;
    RelativeLayout3: TRelativeLayout;
    RelativeLayout4: TRelativeLayout;
    RelativeLayout5: TRelativeLayout;
    View5: TView;
    View6: TView;
    View7: TView;
    RelativeLayout2: TRelativeLayout;
    View1: TView;
    View2: TView;
    View8: TView;
    View10: TView;
    TextView1: TTextView;
    TextView3: TTextView;
    TextView4: TTextView;
    TextView5: TTextView;
    Label1: TLabel;
    TextView2: TTextView;
    TextView6: TTextView;
    LinearLayout1: TLinearLayout;
    TextView7: TTextView;
    TextView8: TTextView;
    ImageList1: TImageList;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure View5Click(Sender: TObject);
    procedure View8Click(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure View3Click(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Self.Fill.Color;
end;

procedure TForm1.ButtonView1Click(Sender: TObject);
begin
  ShowMessage('Clieck Event');
end;

procedure TForm1.View1Click(Sender: TObject);
begin
  Label1.Trimming := TTextTrimming.Word;
end;

procedure TForm1.View3Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.View5Click(Sender: TObject);
begin
  Caption := Sender.ClassName;
end;

var
  I: Integer = 0;

procedure TForm1.View8Click(Sender: TObject);
begin
  Inc(I);
  if TextView2.Tag = 0 then begin
    TextView2.Tag := TextView1.Tag + 1;
    TextView2.Text := IntToStr(I) + ', ÖÐÎÄ²âÊÔ abc 123';
  end else begin
    TextView2.Text := IntToStr(I) + ', ÄúºÃ';
    TextView2.Tag := 0;
  end;
end;

end.
