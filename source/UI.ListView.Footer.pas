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
    FOrientation: TOrientation;
    FStatePullUpStart, FStatePullUpOK, FStatePullUpFinish, FStatePullUpComplete: string;
    FStatePullLeftStart, FStatePullLeftOK, FStatePullLeftFinish, FStatePullLeftComplete: string;
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

{ TListViewDefaultFooter }

constructor TListViewDefaultFooter.Create(AOwner: TComponent);
begin
  inherited;
  FOrientation := TOrientation.Vertical;

  {$IFDEF MSWINDOWS}
  FStatePullUpStart := '点击加载更多';
  {$ELSE}
  FStatePullUpStart := '上拉或点击加载更多';
  {$ENDIF}
  FStatePullUpOK := '松开加载更多';
  FStatePullUpFinish := '正在加载...';
  FStatePullUpComplete := '加载完成';

  FStatePullLeftStart := '查看更多';
  FStatePullLeftOK := '释放查看';
  FStatePullLeftFinish := '正在加载';
  FStatePullLeftComplete := '加载完成';
end;

procedure TListViewDefaultFooter.DoUpdateState(const State: TListViewState;
  const ScrollValue: Double);
begin
  case Orientation of
    TOrientation.Horizontal: begin
      case State of
        TListViewState.None, TListViewState.PullLeftStart:
          begin
            tvText.Checked := False;
            tvText.Text := FStatePullLeftStart;
            AniView.Visible := False;
            AniView.Enabled := False;
            Visible := State <> TListViewState.None;

            tvText.Paddings := '0';
            tvText.Margin := '0';
            tvText.TextSettings.WordWrap := True;
            tvText.Layout.CenterHorizontal := False;
            tvText.Layout.AlignParentLeft := True;
            tvText.Width := 25;
            Width := tvText.Width + 5;
          end;
        TListViewState.PullLeftOK:
          begin
            tvText.Checked := False;
            tvText.Text := FStatePullLeftOK;
            AniView.Visible := False;
            AniView.Enabled := False;
          end;
        TListViewState.PullLeftFinish:
          begin
            tvText.Checked := False;
            tvText.Text := FStatePullLeftFinish;
            AniView.Enabled := True;
            AniView.Visible := True;
            tvText.Visible := False;
            AniView.Position.X := (Width - AniView.Width) / 2;
            AniView.Position.Y := (Height - AniView.Height) / 2;
          end;
        TListViewState.PullLeftComplete:
          begin
            tvText.Text := FStatePullLeftComplete;
            AniView.Enabled := False;
            AniView.Visible := False;
            tvText.Visible := True;
          end;
      end;
    end;
    TOrientation.Vertical: begin
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
  end;
end;

function TListViewDefaultFooter.GetOrientation: TOrientation;
begin
  Result := FOrientation;
end;

procedure TListViewDefaultFooter.SetOrientation(AOrientation: TOrientation);
begin
  FOrientation := AOrientation;
end;

procedure TListViewDefaultFooter.SetStateHint(const State: TListViewState;
  const Msg: string);
begin
  case Orientation of
    TOrientation.Horizontal: begin
      case State of
        TListViewState.None, TListViewState.PullLeftStart:
          FStatePullLeftStart := Msg;
        TListViewState.PullLeftOK:
          FStatePullLeftOK := Msg;
        TListViewState.PullLeftFinish:
          FStatePullLeftFinish := Msg;
        TListViewState.PullLeftComplete:
          FStatePullLeftComplete := Msg;
      end;
    end;
    TOrientation.Vertical: begin
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
  end;
end;

end.
