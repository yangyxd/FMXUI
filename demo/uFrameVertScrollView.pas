unit uFrameVertScrollView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Standard, UI.Base, UI.Edit, FMX.Controls.Presentation,
  UI.Calendar;

type
  TFrameVertScrollView = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    VertScrollView2: TVertScrollView;
    LinearLayout2: TLinearLayout;
    ButtonView2: TButtonView;
    ButtonView3: TButtonView;
    ButtonView4: TButtonView;
    ButtonView7: TButtonView;
    ButtonView9: TButtonView;
    ButtonView10: TButtonView;
    ButtonView11: TButtonView;
    ButtonView12: TButtonView;
    ButtonView13: TButtonView;
    TextView3: TTextView;
    EditView1: TEditView;
    TextView4: TTextView;
    EditView2: TEditView;
    EditView3: TEditView;
    EditView4: TEditView;
    EditView5: TEditView;
    EditView6: TEditView;
    EditView7: TEditView;
    EditView8: TEditView;
    EditView9: TEditView;
    HorzScrollView1: THorzScrollView;
    CalendarView1: TCalendarView;
    LinearLayout3: TLinearLayout;
    TextView1: TTextView;
    TextView2: TTextView;
    TextView5: TTextView;
    TextView6: TTextView;
    TextView7: TTextView;
    TextView8: TTextView;
    TextView9: TTextView;
    TextView10: TTextView;
    BadgeView1: TBadgeView;
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
  TFrameAnimator.DelayExecute(Self,
    procedure (Sender: TObject)
    begin
      VertScrollView2.PullLoadComplete;
      Hint('加载完成');
    end
  , 2);
end;

procedure TFrameVertScrollView.VertScrollView1PullRefresh(Sender: TObject);
begin
  TFrameAnimator.DelayExecute(Self,
    procedure (Sender: TObject)
    begin
      VertScrollView2.PullRefreshComplete;
      Hint('刷新完成');
    end
  , 3.5);
end;

end.
