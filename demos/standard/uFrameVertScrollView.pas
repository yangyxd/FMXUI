unit uFrameVertScrollView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.InertialMovement,
  UI.Frame, UI.Standard, UI.Base, UI.Edit, UI.Calendar;

type
  TFrameVertScrollView = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    RelativeLayout1: TRelativeLayout;
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
    TextView4: TTextView;
    EditView1: TEditView;
    ButtonView13: TButtonView;
    CalendarView1: TCalendarView;
    TextView3: TTextView;
    HorzScrollView1: THorzScrollView;
    LinearLayout3: TLinearLayout;
    TextView1: TTextView;
    TextView2: TTextView;
    TextView5: TTextView;
    TextView6: TTextView;
    TextView7: TTextView;
    TextView8: TTextView;
    TextView9: TTextView;
    TextView10: TTextView;
    EditView2: TEditView;
    EditView3: TEditView;
    EditView4: TEditView;
    EditView5: TEditView;
    EditView6: TEditView;
    EditView7: TEditView;
    EditView8: TEditView;
    EditView9: TEditView;
    BadgeView1: TBadgeView;
    TextView11: TTextView;
    procedure btnBackClick(Sender: TObject);
    procedure VertScrollView1PullRefresh(Sender: TObject);
    procedure VertScrollView1PullLoad(Sender: TObject);
    procedure VertScrollView2ViewportPositionChange(Sender: TObject;
      const OldViewportPosition, NewViewportPosition: TPointD;
      const ContentSizeChanged: Boolean);
  private
    { Private declarations }
  protected
    procedure DoCreate; override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameVertScrollView.btnBackClick(Sender: TObject);
begin
  Finish;
end;

procedure TFrameVertScrollView.DoCreate;
begin
  inherited;

  TextView11.Text := Format(' %d / %d ', [
    1,
    Trunc(VertScrollView2.ContentBounds.Height / VertScrollView2.Height)]);
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

procedure TFrameVertScrollView.VertScrollView2ViewportPositionChange(
  Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointD;
  const ContentSizeChanged: Boolean);
begin
  TextView11.Text := Format(' %d / %d ', [
    Trunc(NewViewportPosition.Y / VertScrollView2.Height) + 1,
    Trunc(VertScrollView2.ContentBounds.Height / VertScrollView2.Height)]);
end;

end.
