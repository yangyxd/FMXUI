unit uFrameRingView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Standard, UI.Base, FMX.Objects;

type
  TFrameRingView = class(TFrame)
    LinearLayout1: TLinearLayout;
    TextView17: TTextView;
    tvTitle: TTextView;
    RelativeLayout1: TRelativeLayout;
    RingView1: TRingView;
    RingView2: TRingView;
    RingView3: TRingView;
    RingView4: TRingView;
    RingView5: TRingView;
    RingView6: TRingView;
    TextView1: TTextView;
    procedure TextView17Click(Sender: TObject);
    procedure RingView5Click(Sender: TObject);
    procedure RingView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  Math;

{$R *.fmx}

procedure TFrameRingView.RingView1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  TextView1.Text := Format('X: %.1f, Y: %.1f, æ‡¿Î: %.1f', [
    X, Y, Sqr(X - 160) + Sqr(Y - 160)
  ]);
end;

procedure TFrameRingView.RingView5Click(Sender: TObject);
begin
  tvTitle.Text := (TView(Sender).Name);
end;

procedure TFrameRingView.TextView17Click(Sender: TObject);
begin
  Finish;
end;

end.
