unit uFrameProgressView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Base, UI.Standard, UI.ListView, System.ImageList, FMX.ImgList,
  FMX.Objects, FMX.Controls.Presentation;

type
  TFrameProgressView = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    TextView2: TTextView;
    ImageList1: TImageList;
    ProgressView1: TProgressView;
    Timer1: TTimer;
    ProgressView2: TProgressView;
    ProgressView3: TProgressView;
    ProgressView4: TProgressView;
    TextView3: TTextView;
    Timer2: TTimer;
    ProgressView5: TProgressView;
    btnBack: TTextView;
    procedure Timer1Timer(Sender: TObject);
    procedure ProgressView1Click(Sender: TObject);
    procedure ProgressView2Click(Sender: TObject);
    procedure ProgressView5Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure RingView1Click(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure DoShow(); override;
    procedure DoFree(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameProgressView.btnBackClick(Sender: TObject);
begin
  Finish();
end;

procedure TFrameProgressView.DoFree;
begin
  inherited DoFree;
end;

procedure TFrameProgressView.DoShow;
begin
  inherited;
end;

procedure TFrameProgressView.Image1Click(Sender: TObject);
begin
  Hint('click');
end;

procedure TFrameProgressView.ProgressView1Click(Sender: TObject);
begin
  Timer1Timer(Timer1);
end;

procedure TFrameProgressView.ProgressView2Click(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
end;

procedure TFrameProgressView.ProgressView5Click(Sender: TObject);
begin
  Hint(TView(Sender).Name);
end;

procedure TFrameProgressView.RingView1Click(Sender: TObject);
begin
  Hint('click');
end;

procedure TFrameProgressView.Timer1Timer(Sender: TObject);
begin
  ProgressView1.Value := ProgressView1.Value + 1;
  if ProgressView1.Value > ProgressView1.Max then
    ProgressView1.Value := ProgressView1.Min;
  ProgressView2.Value := ProgressView2.Value + 1;
  if ProgressView2.Value > ProgressView2.Max then
    ProgressView2.Value := ProgressView2.Min;
  ProgressView3.Value := ProgressView3.Value + 1;
  if ProgressView3.Value > ProgressView3.Max then
    ProgressView3.Value := ProgressView3.Min;
  ProgressView4.Value := ProgressView4.Value + 1;
  if ProgressView4.Value > ProgressView4.Max then
    ProgressView4.Value := ProgressView4.Min;
  ProgressView5.Value := ProgressView5.Value + 1;
  if ProgressView5.Value > ProgressView5.Max then
    ProgressView5.Value := ProgressView5.Min;
end;

procedure TFrameProgressView.Timer2Timer(Sender: TObject);
begin
  TextView3.Text := ViewStateToString(ProgressView2.ViewState);
end;

end.
