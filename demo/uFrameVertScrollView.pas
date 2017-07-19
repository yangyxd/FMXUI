unit uFrameVertScrollView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Standard, UI.Base, UI.Edit, FMX.Controls.Presentation;

type
  TFrameVertScrollView = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    VertScrollView1: TVertScrollView;
    EditView1: TEditView;
    TextView1: TTextView;
    EditView2: TEditView;
    EditView3: TEditView;
    TextView2: TTextView;
    Button1: TButton;
    ButtonView1: TButtonView;
    procedure btnBackClick(Sender: TObject);
    procedure VertScrollView1PullRefresh(Sender: TObject);
    procedure VertScrollView1PullLoad(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameVertScrollView.btnBackClick(Sender: TObject);
begin
  Finish;
end;

procedure TFrameVertScrollView.VertScrollView1PullLoad(Sender: TObject);
begin
  Hint('PullLoad');
end;

procedure TFrameVertScrollView.VertScrollView1PullRefresh(Sender: TObject);
begin
  Hint('PullRefresh');
end;

end.
