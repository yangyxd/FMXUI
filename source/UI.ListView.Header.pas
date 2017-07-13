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
    FStatePullDownStart, FStatePullDownOK, FStatePullDownFinish, FStatePullDownComplete: string;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
    procedure DoUpdateState(const State: TListViewState; const ScrollValue: Double);
    procedure SetStateHint(const State: TListViewState; const Msg: string);
  end;

implementation

{$R *.fmx}

uses
  UI.Frame;

{ TListViewDefaultHeader }

constructor TListViewDefaultHeader.Create(AOwner: TComponent);
begin
  inherited;
  FStatePullDownStart := '下拉刷新';
  FStatePullDownOK := '松开立即刷新';
  FStatePullDownFinish := '正在刷新...';
  FStatePullDownComplete := '刷新完成';
end;

procedure TListViewDefaultHeader.DoUpdateState(const State: TListViewState;
  const ScrollValue: Double);
begin
  case State of
    TListViewState.None, TListViewState.PullDownStart:
      begin
        AniView.Visible := False;
        AniView.Enabled := False;
        tvText.Text := FStatePullDownStart;
        tvText.Checked := False;
        vImg.Visible := True;
        vImg.Checked := False;
        Visible := True;
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

procedure TListViewDefaultHeader.SetStateHint(const State: TListViewState;
  const Msg: string);
begin
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

end.
