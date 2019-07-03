unit UI.ListView.Footer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.ListView;

type
  TListViewDefaultFooter = class(TFrame, IListViewHeader)
    RelativeLayout1: TRelativeLayout;
    tvText: TTextView;
    AniView: TAniIndicator;
  private
    { Private declarations }
    FStatePullUpStart, FStatePullUpOK, FStatePullUpFinish, FStatePullUpComplete: string;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
    procedure DoUpdateState(const State: TListViewState; const ScrollValue: Double);
    procedure SetStateHint(const State: TListViewState; const Msg: string);
  end;

implementation

{$R *.fmx}

{ TListViewDefaultFooter }

constructor TListViewDefaultFooter.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF MSWINDOWs}
  FStatePullUpStart := '点击加载更多';
  {$ELSE}
  FStatePullUpStart := '上拉或点击加载更多';
  {$ENDIF}
  FStatePullUpOK := '松开加载更多';
  FStatePullUpFinish := '正在加载...';
  FStatePullUpComplete := '加载完成';
end;

procedure TListViewDefaultFooter.DoUpdateState(const State: TListViewState;
  const ScrollValue: Double);
begin
  case State of
    TListViewState.None, TListViewState.PullUpStart:
      begin
        tvText.Checked := False;
        tvText.Text := FStatePullUpStart;
        AniView.Visible := False;
        AniView.Enabled := False;
        Visible := True;
      end;
    TListViewState.PullUpOK:
      begin
        tvText.Checked := False;
        tvText.Text := FStatePullUpOK;
        AniView.Visible := False;
        AniView.Enabled := False;
      end;
    TListViewState.PullUpFinish:
      begin
        tvText.Checked := False;
        tvText.Text := FStatePullUpFinish;
        AniView.Enabled := True;
        AniView.Visible := True;
      end;
    TListViewState.PullUpComplete:
      begin
        tvText.Checked := True;
        tvText.Text := FStatePullUpComplete;
        AniView.Enabled := False;
        AniView.Visible := False;
      end;
  end;
end;

procedure TListViewDefaultFooter.SetStateHint(const State: TListViewState;
  const Msg: string);
begin
  case State of
    TListViewState.None, TListViewState.PullUpStart:
      FStatePullUpStart := Msg;
    TListViewState.PullUpOK:
      FStatePullUpOK := Msg;
    TListViewState.PullUpFinish:
      FStatePullUpFinish := Msg;
    TListViewState.PullUpComplete:
      FStatePullUpComplete := Msg;
  end;
end;

end.
