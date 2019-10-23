unit UI.Toast.AndroidLike;

interface

{$IFNDEF ANDROID}
uses
  UI.Base,
  UI.Utils,
  UI.Standard,
  System.SysUtils,
  System.Classes,
  System.Types,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Objects,
  System.UITypes,
  FMX.Graphics,
  System.Actions,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
  FMX.Styles,
  FMX.TextLayout,
  FMX.Effects,
  FMX.Layouts;
{$ENDIF}

{$IFNDEF ANDROID}
type
  TToast = class(TComponent)
  private
    FTimer: TTimer;
    FStartTime: Int64;
    FText: TTextView;
    FPH, FPW: Single;
    FQueue: TQueue<string>;
  protected
    procedure DoToastTimer(Sender: TObject);
    procedure DoSetText(const Text: string);
    procedure InitTimer();
    procedure InitText();
    procedure AdjustTextPosition();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowToast(const AMsg: string);

    property Text: TTextView read FText;
    property Queue: TQueue<string> read FQueue;
  end;
{$ENDIF}

implementation

{$IFNDEF ANDROID}
uses FMX.Forms;

const
  FViewTime = 1500;
  FMinViewTime = 500;

procedure TToast.AdjustTextPosition;
begin
  if (FPW > 0) and (FPH > 0) then begin
    FText.Position.Y := FPH - FText.Size.Height - FText.Margins.Bottom;
    FText.Position.X := (FPW - FText.Size.Width) / 2;
  end;
end;

constructor TToast.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (AOwner is TFmxObject) then
    raise Exception.Create('The Owner must be TFmxObject.');
  FStartTime := 0;
  FQueue := TQueue<string>.Create;
  InitTimer();
end;

destructor TToast.Destroy;
begin
  FText := nil;
  if Assigned(FTimer) then
    FTimer := nil;
  FreeAndNil(FQueue);
  inherited Destroy;
end;

procedure TToast.DoSetText(const Text: string);
begin
  if not Assigned(FText) then
    InitText;

  if FText.Parent is TControl then begin
    FPW := TControl(FText.Parent).LocalRect.Width;
    FPH := TControl(FText.Parent).LocalRect.Height;
  end else if FText.Parent is TCommonCustomForm then begin
    FPW := TCommonCustomForm(FText.Parent).ClientRect.Width;
    FPH := TCommonCustomForm(FText.Parent).ClientRect.Height;
  end else begin
    FPW := 0;
    FPH := 0;
  end;
  if FPW > 0 then
    FText.MaxWidth := FPW - FText.Margins.Width;
  FText.Text := Text;
  FText.Index := FText.Parent.ChildrenCount - 1;

  AdjustTextPosition;
end;

procedure TToast.DoToastTimer(Sender: TObject);
var
  LTime: Int64;
begin
  if (csDestroying in ComponentState) then
    Exit;
  LTime := GetTimestamp - FStartTime;
  if (LTime >= FMinViewTime) then begin
    if FQueue.Count > 0 then begin
      DoSetText(FQueue.Dequeue);
      FStartTime := GetTimestamp;
    end else if (LTime > FViewTime) then begin
      FStartTime := 0;
      FTimer.Enabled := False;
      if Assigned(FText) then begin
        FText.Parent.RemoveObject(FText);
        FreeAndNil(FText);
      end;
    end;
  end;
end;

procedure TToast.InitText;
var
  P: TFmxObject;
begin
  FText := TTextView.Create(Owner);
  {$IFDEF MSWINDOWS}
  FText.Name := 'ToastText';
  {$ENDIF}
  P := TFmxObject(Owner);
  FText.BeginUpdate;
  FText.Padding.Rect := RectF(16, 6, 16, 6);
  FText.Margins.Rect := RectF(24, 0, 24, 48);
  FText.Parent := P;
  FText.HitTest := False;
  FText.MinHeight := 32;
  FText.Opacity := 1.0;
  FText.Gravity := TLayoutGravity.Center;
  FText.Background.ItemDefault.Color := $7f000000;
  FText.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FText.Background.XRadius := 15;
  FText.Background.YRadius := 15;
  FText.TextSettings.Color.Default := $fff0f0f0;
  FText.TextSettings.WordWrap := True;
  FText.WidthSize := TViewSize.WrapContent;
  FText.HeightSize := TViewSize.WrapContent;
  FText.AutoSize := True;
  FText.Index := P.ChildrenCount - 1;
  FText.EndUpdate;
end;

procedure TToast.InitTimer;
begin
  FTimer := TTimer.Create(Owner);
  FTimer.Enabled := False;
  FTimer.Interval := 200;
  FTimer.OnTimer := DoToastTimer;
end;

procedure TToast.ShowToast(const AMsg: string);
begin
  if AMsg = '' then Exit;
  FQueue.Enqueue(AMsg);
  FTimer.Enabled := True;
end;

{$ENDIF}

end.
