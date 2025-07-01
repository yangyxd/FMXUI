unit UI.Toast.AndroidLike;

interface

uses
  UI.Base,
  UI.Utils,
  UI.Standard,
  System.SysUtils,
  System.Classes,
  System.Types,
  {$IFDEF FMX}
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Graphics,
  FMX.Styles,
  FMX.TextLayout,
  FMX.Effects,
  FMX.Layouts,
  {$ELSE}
  Windows,
  Messages,
  VCL.Controls,
  VCL.Graphics,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Forms,
  {$ENDIF}
  System.UITypes,
  System.Actions,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TToast = class(TComponent)
  private
    FTimer: TTimer;
    FStartTime: Int64;
    {$IFDEF FMX}
    FText: TTextView;
    FPH, FPW: Single;
    {$ELSE}
    FText: TLabel;
    FPanel: TPanel;
    FPH, FPW: Integer;
    {$ENDIF}
    FQueue: TQueue<string>;
  protected
    procedure DoToastTimer(Sender: TObject);
    procedure DoSetText(const Text: string);
    procedure InitTimer();
    procedure InitText();
    procedure AdjustTextPosition();
    procedure DoDbClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowToast(const AMsg: string);

    property Text: {$IFDEF FMX}TTextView{$ELSE}TLabel{$ENDIF} read FText;
    property Queue: TQueue<string> read FQueue;
  end;

implementation

uses FMX.Forms;

const
  FViewTime = 1500;
  FMinViewTime = 500;

procedure TToast.AdjustTextPosition;
begin
  if (FPW > 0) and (FPH > 0) then begin
    {$IFDEF FMX}
    FText.Position.Y := FPH - FText.Size.Height - FText.Margins.Bottom;
    FText.Position.X := (FPW - FText.Size.Width) / 2;
    {$ELSE}
    FPanel.Top := FPH - FPanel.Height - FPanel.Margins.Bottom;
    FPanel.Left := Round((FPW - FPanel.Width) / 2);
    {$ENDIF}
  end;
end;

constructor TToast.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //  if not (AOwner is TFmxObject) then
  //    raise Exception.Create('The Owner must be TFmxObject.');
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

procedure TToast.DoDbClick(Sender: TObject);
begin
  {$IFNDEF FMX}
  if not Assigned(FPanel) then Exit;
  FPanel.Parent.RemoveControl(FPanel);
  FreeAndNil(FPanel);
  FText := nil;
  {$ENDIF}
end;

procedure TToast.DoSetText(const Text: string);
{$IFNDEF FMX}
var
  R: TRect;
  rgn: Thandle;
{$ENDIF}
begin
  if not Assigned(FText) then
    InitText;
  {$IFDEF FMX}
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
  {$ELSE}
  if FPanel.Parent is TControl then begin
    FPW := TControl(FPanel.Parent).ClientWidth;
    FPH := TControl(FPanel.Parent).ClientHeight;
  end else if FPanel.Parent is VCL.Forms.TCustomForm then begin
    FPW := TCustomForm(FPanel.Parent).ClientWidth;
    FPH := TCustomForm(FPanel.Parent).ClientHeight;
  end else begin
    FPW := 0;
    FPH := 0;
  end;
  FPanel.Top := -9000;
  FText.Caption := Text;
  FPanel.Visible := True;
  FPanel.Realign;
  R := FPanel.ClientRect;
  rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, 5, 5);
  try
    TWinControl(Owner).Perform(EM_GETRECT, 0, lParam(@r));
    InflateRect(r, -5, -5);
    TWinControl(Owner).Perform(EM_SETRECTNP, 0, lParam(@r));
    SetWindowRgn(FPanel.handle, rgn, true);
    R := TWinControl(Owner).ClientRect;
    AdjustTextPosition;
    FPanel.BringToFront;
    FPanel.Invalidate;
  finally
    DeleteObject(rgn);
  end;
  {$ENDIF}
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
        {$IFDEF FMX}
        FText.Parent.RemoveObject(FText);
        FreeAndNil(FText);
        {$ELSE}
        FPanel.Parent.RemoveControl(FPanel);
        FreeAndNil(FPanel);
        FText := nil;
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TToast.InitText;
var
  P: {$IFDEF FMX}TFmxObject{$ELSE}TWinControl{$ENDIF};
begin
  {$IFDEF FMX}
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
  {$ELSE}
  P := TWinControl(Owner);
  FPanel := TPanel.Create(Owner);
  FPanel.Visible := False;
  FPanel.Parent := P;
  FPanel.ParentCtl3D := False;
  FPanel.ParentColor := False;
  FPanel.ParentBackground := False;
  FPanel.Ctl3D := False;
  FPanel.Caption := '';
  FPanel.BorderStyle := bsSingle;
  FPanel.Font.Size := 10;
  FPanel.Font.Color := $00f0f0f0;
  FPanel.BevelOuter := bvNone;
  FPanel.Padding.SetBounds(16, 6, 16, 6);
  FPanel.Margins.SetBounds(24, 0, 24, 32);
  FPanel.AutoSize := True;
  FPanel.DoubleBuffered := True;
  FPanel.FullRepaint := True;
  FPanel.Color := $00666666;
  FPanel.Anchors := [akLeft, akBottom];
  FText := TLabel.Create(FPanel);
  FText.Parent := FPanel;
  FText.Caption := '';
  FText.Transparent := True;
  FText.AutoSize := True;
  FText.ShowAccelChar := False;
  FPanel.Top := -1000;
  FPanel.OnDblClick := DoDbClick;
  FText.OnDblClick := DoDbClick;
  {$ENDIF}
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
  if TThread.CurrentThread.ThreadID = MainThreadID then begin
    FQueue.Enqueue(AMsg);
    FTimer.Enabled := True;
  end
  else
    TThread.Queue(nil, procedure begin
      FQueue.Enqueue(AMsg);
      FTimer.Enabled := True;
    end);
end;

end.
