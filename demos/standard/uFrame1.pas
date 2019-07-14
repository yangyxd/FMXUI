unit uFrame1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.Frame, UI.Dialog, FMX.Layouts;

type
  TFrame1 = class(TFrame)
    LinearLayout1: TLinearLayout;
    TextView17: TTextView;
    tvTitle: TTextView;
    VertScrollBox1: TVertScrollView;
    TextView1: TTextView;
    LinearLayout2: TLinearLayout;
    ButtonView1: TButtonView;
    ButtonView10: TButtonView;
    ButtonView11: TButtonView;
    ButtonView12: TButtonView;
    ButtonView13: TButtonView;
    ButtonView14: TButtonView;
    ButtonView15: TButtonView;
    ButtonView2: TButtonView;
    ButtonView3: TButtonView;
    ButtonView4: TButtonView;
    ButtonView5: TButtonView;
    ButtonView6: TButtonView;
    ButtonView7: TButtonView;
    ButtonView8: TButtonView;
    ButtonView9: TButtonView;
    ButtonView16: TButtonView;
    ButtonView17: TButtonView;
    procedure TextView17Click(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
    procedure ButtonView3Click(Sender: TObject);
    procedure TextView1Click(Sender: TObject);
    procedure ButtonView4Click(Sender: TObject);
    procedure ButtonView6Click(Sender: TObject);
    procedure ButtonView7Click(Sender: TObject);
    procedure ButtonView8Click(Sender: TObject);
    procedure ButtonView9Click(Sender: TObject);
    procedure ButtonView10Click(Sender: TObject);
    procedure ButtonView11Click(Sender: TObject);
    procedure ButtonView12Click(Sender: TObject);
    procedure ButtonView13Click(Sender: TObject);
    procedure ButtonView14Click(Sender: TObject);
    procedure ButtonView15Click(Sender: TObject);
    procedure ButtonView5Click(Sender: TObject);
    procedure ButtonView16Click(Sender: TObject);
    procedure ButtonView17Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure DoHide(); override;
    procedure DoReStart(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  uFrameDateView,
  uFrameHorzScrollView,
  uFrameVertScrollView,
  uFrameCameraViewer,
  uFrameGridView,
  uFrameImageViewer,
  uFrameListViewGroup,
  uFrameImageView,
  uFramePopMenu,
  uFrameMultiPathView,
  uFrameRingView,
  ui_CustomListView,
  uFrameProgressView,
  uFrameMore,
  uFrameDialog,
  uFrame4,
  uFrame3,
  uFrame2;

procedure TFrame1.ButtonView10Click(Sender: TObject);
begin
  case TView(Sender).Tag of
    0, 4: begin
      TDialog.ShowView(Self, TView(Sender), TFramePopMenu, 0, 0,
        TDialogViewPosition.LeftFill, True,
        TFrameAniType.LeftSlideMenu);
      TView(Sender).Tag := 0;
    end;
    1: begin
      TDialog.ShowView(Self, TView(Sender), TFramePopMenu, 0, 0,
        TDialogViewPosition.RightFill, True,
        TFrameAniType.RightSlideMenu);
    end;
    2: begin
      TDialog.ShowView(Self, TView(Sender), TFramePopMenu, 0, 0,
        TDialogViewPosition.Top, True,
        TFrameAniType.TopMoveInOut).RootView.Controls[0].Align := TAlignLayout.Top;
    end;
    3: begin
      TDialog.ShowView(Self, TView(Sender), TFramePopMenu, 0, 0,
        TDialogViewPosition.Bottom, True,
        TFrameAniType.BottomMoveInOut).RootView.Controls[0].Align := TAlignLayout.Bottom;
    end;
  end;
  TView(Sender).Tag := TView(Sender).Tag + 1;
end;

procedure TFrame1.ButtonView11Click(Sender: TObject);
begin
  StartFrame(TFrameImageView, 'ImageView Demo')
end;

procedure TFrame1.ButtonView12Click(Sender: TObject);
begin
  StartFrame(TFrameListViewGroup, TButtonView(Sender).Text);
end;

procedure TFrame1.ButtonView13Click(Sender: TObject);
begin
  StartFrame(TFrameImageViewer);
end;

procedure TFrame1.ButtonView14Click(Sender: TObject);
begin
  StartFrame(TFrameGridView)
end;

procedure TFrame1.ButtonView15Click(Sender: TObject);
begin
  StartFrame(TFrameCameraViewer);
end;

procedure TFrame1.ButtonView16Click(Sender: TObject);
begin
  StartFrame(TFrameHorzScrollView)
end;

procedure TFrame1.ButtonView17Click(Sender: TObject);
begin
  StartFrame(TFrameDateView)
end;

procedure TFrame1.ButtonView1Click(Sender: TObject);
begin
  StartFrame(TFrame2, TButtonView(Sender).Text, TFrameAniType.TopMoveInOut);
end;

procedure TFrame1.ButtonView2Click(Sender: TObject);
begin
  StartFrame(TFrame3);
end;

procedure TFrame1.ButtonView3Click(Sender: TObject);
begin
  StartFrame(TFrmaeDialog, TButtonView(Sender).Text, TFrameAniType.FadeInOut)
end;

procedure TFrame1.ButtonView4Click(Sender: TObject);
begin
  StartFrame(TCustomListview);
end;

procedure TFrame1.ButtonView5Click(Sender: TObject);
begin
  StartFrame(TFrameVertScrollView)
end;

procedure TFrame1.ButtonView6Click(Sender: TObject);
begin
  StartFrame(TFrameProgressView);
end;

procedure TFrame1.ButtonView7Click(Sender: TObject);
begin
  StartFrame(TFrame4);
end;

procedure TFrame1.ButtonView8Click(Sender: TObject);
begin
  StartFrame(TFrameRingView);
end;

procedure TFrame1.ButtonView9Click(Sender: TObject);
begin
  StartFrame(TFrameMultiPathView);
end;

procedure TFrame1.DoHide;
begin
  inherited DoHide;
end;

procedure TFrame1.DoReStart;
begin
  inherited DoReStart;
end;

procedure TFrame1.TextView17Click(Sender: TObject);
begin
  Finish();
end;

procedure TFrame1.TextView1Click(Sender: TObject);
begin
  StartFrame(TFrameMore, '¸ü¶àÄÚÈÝ')
end;

end.
