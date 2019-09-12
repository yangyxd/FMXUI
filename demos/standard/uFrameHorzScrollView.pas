unit uFrameHorzScrollView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.Frame;

type
  TFrameHorzScrollView = class(TFrame)
    LinearLayout1: TLinearLayout;
    btnBack: TTextView;
    tvTitle: TTextView;
    HorzScrollView1: THorzScrollView;
    TextView1: TTextView;
    HorzScrollView2: THorzScrollView;
    LinearLayout3: TLinearLayout;
    TextView2: TTextView;
    TextView3: TTextView;
    TextView5: TTextView;
    TextView6: TTextView;
    TextView7: TTextView;
    TextView8: TTextView;
    TextView9: TTextView;
    TextView10: TTextView;
    procedure btnBackClick(Sender: TObject);
    procedure HorzScrollView2PullLoad(Sender: TObject);
    procedure HorzScrollView2PullRefresh(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameHorzScrollView.btnBackClick(Sender: TObject);
begin
  Finish;
end;

procedure TFrameHorzScrollView.HorzScrollView2PullLoad(Sender: TObject);
begin
  TFrameAnimator.DelayExecute(Self,
    procedure (Sender: TObject)
    begin
      HorzScrollView2.PullLoadComplete;
      Hint('加载完成');
    end
  , 2);
end;

procedure TFrameHorzScrollView.HorzScrollView2PullRefresh(Sender: TObject);
begin
  TFrameAnimator.DelayExecute(Self,
    procedure (Sender: TObject)
    begin
      HorzScrollView2.PullRefreshComplete;
      Hint('刷新完成');
    end
  , 3.5);
end;

end.
