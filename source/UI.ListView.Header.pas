unit UI.ListView.Header;

interface

uses
  UI.ListView,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Base, UI.Standard;

type
  /// <summary>
  /// ListView 默认头部
  /// </summary>
  TListViewDefaultHeader = class(TFrame, IListViewHeader)
    RelativeLayout1: TRelativeLayout;
    tvText: TTextView;
    AniView: TAniIndicator;
    vImg: TView;
    View2: TView;
  private
    { Private declarations }
    FOrientation: TOrientation;
    FStatePullDownStart, FStatePullDownOK, FStatePullDownFinish, FStatePullDownComplete: string;
    FStatePullRightStart, FStatePullRightOK, FStatePullRightFinish, FStatePullRightComplete: string;
  protected
    function GetOrientation: TOrientation;
    procedure SetOrientation(AOrientation: TOrientation);
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
    procedure DoUpdateState(const State: TListViewState; const ScrollValue: Double);
    procedure SetStateHint(const State: TListViewState; const Msg: string);
    property Orientation: TOrientation read GetOrientation write SetOrientation;
  end;

implementation

{$R *.fmx}

uses
  UI.Frame;

{ TListViewDefaultHeader }

constructor TListViewDefaultHeader.Create(AOwner: TComponent);
begin
  inherited;
  FOrientation := TOrientation.Vertical;

  FStatePullDownStart := '下拉刷新';
  FStatePullDownOK := '松开立即刷新';
  FStatePullDownFinish := '正在刷新...';
  FStatePullDownComplete := '刷新完成';

  FStatePullRightStart := '右拉刷新';
  FStatePullRightOK := '释放刷新';
  FStatePullRightFinish := '正在刷新';
  FStatePullRightComplete := '刷新完成';
end;

procedure TListViewDefaultHeader.DoUpdateState(const State: TListViewState;
  const ScrollValue: Double);
begin
  case Orientation of
    TOrientation.Horizontal: begin
      case State of
        TListViewState.None, TListViewState.PullRightStart:
          begin
            AniView.Visible := False;
            AniView.Enabled := False;
            tvText.Text := FStatePullRightStart;
            tvText.Checked := False;
            vImg.Visible := True;
            vImg.Checked := False;
            Visible := State <> TListViewState.None;

            tvText.TextSettings.WordWrap := True;
            tvText.Layout.CenterHorizontal := False;
            tvText.Layout.AlignParentLeft := True;
            tvText.Width := 25;
            Width := tvText.Width + 5;
          end;
        TListViewState.PullRightOK:
          begin
            AniView.Visible := False;
            AniView.Enabled := False;
            tvText.Text := FStatePullRightOK;
            tvText.Checked := False;
            vImg.Visible := True;
            vImg.Checked := True;
          end;
        TListViewState.PullRightFinish:
          begin
            vImg.Visible := False;
            AniView.Enabled := True;
            AniView.Visible := True;
            tvText.Visible := False;
            View2.Layout.CenterInParent := True;
          end;
        TListViewState.PullRightComplete:
          begin
            vImg.Visible := False;
            AniView.Enabled := False;
            AniView.Visible := False;
            tvText.Text := FStatePullRightComplete;

            tvText.Visible := True;
          end;
      end;
    end;
    TOrientation.Vertical: begin
      case State of
        TListViewState.None, TListViewState.PullDownStart:
          begin
            AniView.Visible := False;
            AniView.Enabled := False;
            tvText.Text := FStatePullDownStart;
            tvText.Checked := False;
            vImg.Visible := True;
            vImg.Checked := False;
            Visible := State <> TListViewState.None;
          end;
        TListViewState.PullDownOK:
          begin
            AniView.Visible := False;
            AniView.Enabled := False;
            tvText.Text := FStatePullDownOK;
            tvText.Checked := False;
            vImg.Visible := True;
            vImg.Checked := True;
          end;
        TListViewState.PullDownFinish:
          begin
            vImg.Visible := False;
            AniView.Enabled := True;
            AniView.Visible := True;
            tvText.Text := FStatePullDownFinish;
            tvText.Checked := False;
          end;
        TListViewState.PullDownComplete:
          begin
            vImg.Visible := False;
            AniView.Enabled := False;
            AniView.Visible := False;
            tvText.Text := FStatePullDownComplete;
            tvText.Checked := True;
          end;
      end;
    end;
  end;
end;

function TListViewDefaultHeader.GetOrientation: TOrientation;
begin
  Result := FOrientation;
end;

procedure TListViewDefaultHeader.SetOrientation(AOrientation: TOrientation);
begin
  FOrientation := AOrientation;
end;

procedure TListViewDefaultHeader.SetStateHint(const State: TListViewState;
  const Msg: string);
begin
  case Orientation of
    TOrientation.Horizontal: begin
      case State of
        TListViewState.None, TListViewState.PullRightStart:
          FStatePullRightStart := Msg;
        TListViewState.PullRightOK:
          FStatePullRightOK := Msg;
        TListViewState.PullRightFinish:
          FStatePullRightFinish := Msg;
        TListViewState.PullRightComplete:
          FStatePullRightComplete := Msg;
      end;
    end;
    TOrientation.Vertical: begin
      case State of
        TListViewState.None, TListViewState.PullDownStart:
          FStatePullDownStart := Msg;
        TListViewState.PullDownOK:
          FStatePullDownOK := Msg;
        TListViewState.PullDownFinish:
          FStatePullDownFinish := Msg;
        TListViewState.PullDownComplete:
          FStatePullDownComplete := Msg;
      end;
    end;
  end;
end;

end.
