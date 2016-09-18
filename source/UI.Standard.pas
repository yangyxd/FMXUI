{*******************************************************}
{                                                       }
{       FMX UI 标准组件单元                             }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Standard;

interface

uses
  UI.Base,
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
  FMX.BehaviorManager, FMX.Forms, System.Messaging, FMX.Styles,
  FMX.ActnList, FMX.Objects, System.Math, System.Actions, System.Rtti, FMX.Consts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, FMX.TextLayout,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement;

type
  TOnDrawText = procedure (Sender: TObject; Canvas: TCanvas;
    Text: UI.Base.TTextSettings; R: TRectF) of object;

  TOnDrawViewBackgroud = procedure (Sender: TObject; Canvas: TCanvas;
    const R: TRectF; State: TViewState) of object;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TTextView = class(TView, ICaption)
  private
    FText: UI.Base.TTextSettings;
    FTextHint: string;
    FDrawable: TDrawableIcon;
    FOnDrawText: TOnDrawText;
    FOnTextChange: TNotifyEvent;
    FOnDrawViewBackgroud: TOnDrawViewBackgroud;
    FInFitSize: Boolean;

    function GetAutoSize: Boolean;
    function GetText: string;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetTextSettings(const Value: UI.Base.TTextSettings);
    function GetDrawable: TDrawableIcon;
    procedure SetDrawable(const Value: TDrawableIcon);
    function GetDrawableWidth(): Integer;
    function GetDrawableHeight(): Integer;
    procedure SetTextHint(const Value: string);
  protected
    procedure Loaded; override;
    procedure DblClick; override;
    procedure ImagesChanged; override;
    procedure PaintBackground; override;
    procedure DoDrawBackground(var R: TRectF); virtual;
    procedure DoPaintBackground(var R: TRectF); virtual;
    procedure DoPaintText(var R: TRectF); virtual;
    procedure SetGravity(const Value: TLayoutGravity); override;
    procedure DoLayoutChanged(Sender: TObject); override;
    procedure ActionChange(Sender: TBasicAction; CheckDefaults: Boolean); override;
    function GetDefaultSize: TSizeF; override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure SetName(const Value: TComponentName); override;
  protected
    function TextStored: Boolean;
    function IsAutoSize: Boolean; override;
    procedure DoChanged(Sender: TObject); virtual;
    procedure DoDrawableChanged(Sender: TObject);
    procedure PaddingChanged; override;
    procedure Resize; override;
    procedure DoRecalcSize(var AWidth, AHeight: Single); override;
    procedure DoChangeSize(var ANewWidth, ANewHeight: Single); override;
    procedure DoAutoSize;
    procedure DoUpdateContentBounds;
  protected
    {Scrollbar}
    FScrollBarObj: TScrollBar;
    FContentBounds: TRectF;
    FDownPoint: TPointF;
    procedure AniMouseDown(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseMove(const Touch: Boolean; const X, Y: Single); virtual;
    procedure InitScrollbar; override;
    procedure FreeScrollbar; override;
    procedure HScrollChange(Sender: TObject);
    procedure VScrollChange(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    function GetVScrollBar: TScrollBar; override;
    function GetHScrollBar: TScrollBar; override;
    function GetContentBounds: TRectF; override;
    function CreateBackground: TDrawable; override;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ToString: string; override;
    procedure SetNewScene(AScene: IScene); override;
    procedure AfterConstruction; override;
    procedure Change;
  published
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default True;
    property Text: string read GetText write SetText stored TextStored;
    property TextHint: string read FTextHint write SetTextHint;
    property TextSettings: UI.Base.TTextSettings read FText write SetTextSettings;
    property Drawable: TDrawableIcon read GetDrawable write SetDrawable;
    property ScrollBars;
    property DisableMouseWheel;
    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property OnDrawText: TOnDrawText read FOnDrawText write FOnDrawText;
    property OnDrawBackgroud: TOnDrawViewBackgroud read FOnDrawViewBackgroud
      write FOnDrawViewBackgroud;
  end;

type
  TStyleView = class(TTextView)
  protected
    procedure PaintBackground; override;
    procedure DoDrawStyleControl(var R: TRectF); virtual;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TButtonView = class(TStyleView)
  private
    FDefault: Boolean;
    FCancel: Boolean;
    FModalResult: TModalResult;
  protected
    function GetDefaultSize: TSizeF; override;
    procedure Click; override;
    function CreateBackground: TDrawable; override;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure AfterDialogKey(var Key: Word; Shift: TShiftState); override;
  published
    property CanFocus default True;
    property CanParentFocus;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Default: Boolean read FDefault write FDefault default False;
    property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;
    property Clickable default True;
    property Gravity default TLayoutGravity.Center;
    property OnCanFocus;
  end;

implementation

{ TTextView }

procedure TTextView.ActionChange(Sender: TBasicAction; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then begin
    if not CheckDefaults or Text.IsEmpty or (Text = Name) then
     Text := TCustomAction(Sender).Text;
  end;
  inherited ActionChange(Sender, CheckDefaults);
end;

procedure TTextView.AfterConstruction;
begin
  inherited AfterConstruction;
  FText.OnChanged := DoChanged;
  FText.OnTextChanged := FOnTextChange;
end;

procedure TTextView.AniMouseDown(const Touch: Boolean; const X, Y: Single);
begin
  FDownPoint.X := X;
  FDownPoint.Y := Y;
end;

procedure TTextView.AniMouseMove(const Touch: Boolean; const X, Y: Single);
var
  Offset: Single;
begin
  if Assigned(FScrollBarObj) then begin
    if FScrollbar = TViewScroll.Vertical then begin
      if (FContentBounds.Height > Height) then begin
        Offset := Y - FDownPoint.Y;
        FScrollBarObj.Value := FScrollBarObj.Value - Offset / 100;
        Exit;
      end; 
    end else if FScrollbar = TViewScroll.Horizontal then begin
      if (FContentBounds.Width > Width) then begin
        Offset := X - FDownPoint.X;
        FScrollBarObj.Value := FScrollBarObj.Value - Offset / 100;
        Exit;
      end;
    end;
  end; 
end;

procedure TTextView.DoAutoSize;
var
  W, H: Single;
begin
  if FInFitSize then
    Exit;
  W := GetParentMaxWidth;
  H := GetParentMaxHeight;
  if (MaxHeight > 0) and (W > MaxWidth) then
    W := MaxWidth;
  if (MaxHeight > 0) and (H > MaxHeight) then
    H := MaxHeight;
  if W <= 0 then
    W := FSize.Width;
  if H <= 0 then
    H := FSize.Height;
  DoChangeSize(W, H);
  if (W <> FSize.Width) or (H <> FSize.Height) then begin
    FInFitSize := True;
    SetSize(W, H, False);
    FInFitSize := False;
  end;
end;

function TTextView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
var
  Border: TViewBorder;
begin
  Result := inherited CanRePaintBk(View, State);
  if (Assigned(FDrawable)) then
    Result := not FDrawable.IsEmpty;
  if (not Result) and (Assigned(FBackground)) then begin
    Border := TDrawableBorder(FBackground).Border;
    Result := Assigned(Border) and (Border.Style <> TViewBorderStyle.None) and
      (Border.Width > 0) and (Border.Color.GetColor(State) <> TAlphaColorRec.Null);
  end;
end;

procedure TTextView.Change;
begin
  DoChanged(FText);
end;

constructor TTextView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText := UI.Base.TTextSettings.Create(Self);
  if csDesigning in ComponentState then begin
    FDrawable := TDrawableIcon.Create(Self);
    FDrawable.SizeWidth := 16;
    FDrawable.SizeHeight := 16;
    FDrawable.OnChanged := DoDrawableChanged;
  end;
  SetAcceptsControls(False);
end;

function TTextView.CreateBackground: TDrawable;
begin
  Result := TDrawableBorder.Create(Self);
  with TDrawableBorder(Result).Border do begin
    Width := 1;
    Color.Default := $BFC0C0C0;
    Color.Pressed := $FFC0C0C0;
  end;
  Result.OnChanged := DoBackgroundChanged;
end;

procedure TTextView.DblClick;
begin
  inherited DblClick;
  Click;
end;

destructor TTextView.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FDrawable);
  inherited Destroy;
end;

procedure TTextView.DoChanged(Sender: TObject);
begin
  FGravity := FText.Gravity;
  if FText.IsSizeChange then begin
    if AutoSize then
      DoAutoSize
    else
      DoUpdateContentBounds;
  end else
    Repaint;
  if FText.IsEffectsChange then
    UpdateEffects;
end;

procedure TTextView.DoChangeSize(var ANewWidth, ANewHeight: Single);
begin
  inherited DoChangeSize(ANewWidth, ANewHeight);
  if (Assigned(FScrollBarObj)) then begin
    case FScrollbar of
      TViewScroll.Horizontal: 
        begin
          FScrollBarObj.Visible := FContentBounds.Right > ANewWidth; 
        end;
      TViewScroll.Vertical: 
        begin
          FScrollBarObj.Visible := FContentBounds.Bottom > ANewHeight;
          FContentBounds.Bottom := FContentBounds.Bottom + 4;
        end;
    end;
  end;
end;

procedure TTextView.DoDrawableChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTextView.DoDrawBackground(var R: TRectF);
begin
  if Assigned(FOnDrawViewBackgroud) then
    FOnDrawViewBackgroud(Self, Canvas, R, DrawState);
end;

procedure TTextView.DoLayoutChanged(Sender: TObject);
begin
  inherited DoLayoutChanged(Sender);
  DoAutoSize;
end;

procedure TTextView.DoPaintBackground(var R: TRectF);
begin
  R := RectF(R.Left + Padding.Left, R.Top + Padding.Top,
    R.Right - Padding.Right, R.Bottom - Padding.Bottom);
  if Assigned(FDrawable) and (not FDrawable.IsEmpty) then
    FDrawable.AdjustDraw(Canvas, R, True);
  if (Assigned(FText)) then
    DoPaintText(R);
end;

procedure TTextView.DoPaintText(var R: TRectF);
var
  SR: TRectF;
begin
  if InVisible then
    Exit;
  if FText.Text = '' then
    FText.Draw(Canvas, FTextHint, R, GetAbsoluteOpacity, TViewState(8))
  else begin
    if Assigned(FScrollBarObj) and (FScrollBarObj.Visible) then begin    
      case FScrollbar of
        TViewScroll.None: SR := R;
        TViewScroll.Horizontal: 
          begin
            SR := FContentBounds;
            SR.Top := R.Top;
            SR.Bottom := R.Bottom - FScrollBarObj.Height;
            OffsetRect(SR, -(FScrollBarObj.Value / 100 * (SR.Width - R.Width)), 0);  
          end;
        TViewScroll.Vertical: 
          begin
            SR := FContentBounds;
            SR.Left := R.Left;
            SR.Right := R.Right - FScrollBarObj.Width;
            OffsetRect(SR, 0, -(FScrollBarObj.Value / 100 * (SR.Height - R.Height)));  
          end;
      end;
    end else 
      SR := R;

    if Assigned(FOnDrawText) then
      FOnDrawText(Self, Canvas, FText, SR)
    else
      FText.Draw(Canvas, SR, GetAbsoluteOpacity, DrawState);
  end;
end;

procedure TTextView.DoRecalcSize(var AWidth, AHeight: Single);
var
  ASize: TSizeF;
  V, IconS, VW, VH: Single;
begin
  if FInFitSize or (Scene = nil) or (not Assigned(FText)) then
    Exit;
  FInFitSize := True;
  try
    // 计算出文本区域的最大宽度，如果为0，则不自动换行
    if FText.WordWrap then begin
      V := AWidth - Padding.Left - Padding.Right;
      // 如果有icon，并且是在左边或右边，则还需要减去icon大小
      IconS := GetDrawableWidth;
      if (IconS > 0) and (FDrawable.Position in [TDrawablePosition.Left, TDrawablePosition.Right]) then
        V := V - IconS - FDrawable.Padding;
      if (FScrollbar = TViewScroll.Vertical) and (FScrollBarObj <> nil) then
        V := V - FScrollBarObj.Width - 1;
    end else
      V := 0;

    // 计算文本区域大小
    if not FText.CalcTextObjectSize(V, Scene.GetSceneScale, nil, ASize) then Exit;

    if ASize.Width < GetDrawableWidth then
      ASize.Width := GetDrawableWidth;
    if ASize.Height < GetDrawableHeight then
      ASize.Height := GetDrawableHeight;

    if (WidthSize = TViewSize.WrapContent) and (HeightSize = TViewSize.WrapContent) then begin
      V := GetDrawableWidth;
      AWidth := ASize.Width + Padding.Left + Padding.Right;
      AHeight := ASize.Height + Padding.Top + Padding.Bottom;
      if (V > 0) and (FDrawable.Position in [TDrawablePosition.Left, TDrawablePosition.Right]) then
        AWidth := AWidth + V + FDrawable.Padding;
      V := GetDrawableHeight;
      if (V > 0) and (FDrawable.Position in [TDrawablePosition.Top, TDrawablePosition.Bottom]) then
        AHeight := AHeight + V + FDrawable.Padding;
    end else if WidthSize = TViewSize.WrapContent then begin
      V := GetDrawableWidth;
      if (V > 0) and (FDrawable.Position in [TDrawablePosition.Left, TDrawablePosition.Right]) then
        AWidth := ASize.Width + Padding.Left + Padding.Right + V + FDrawable.Padding
      else
        AWidth := ASize.Width + Padding.Left + Padding.Right;
    end else if HeightSize = TViewSize.WrapContent then begin
      V := GetDrawableHeight;
      if (V > 0) and (FDrawable.Position in [TDrawablePosition.Top, TDrawablePosition.Bottom]) then
        AHeight := ASize.Height + Padding.Top + Padding.Bottom + V + FDrawable.Padding
      else
        AHeight := ASize.Height + Padding.Top + Padding.Bottom;
    end;

    if FScrollbar <> TViewScroll.None then begin
      V := GetDrawableWidth;
      VW := ASize.Width + Padding.Left + Padding.Right;
      VH := ASize.Height + Padding.Top + Padding.Bottom;
      if (V > 0) and (FDrawable.Position in [TDrawablePosition.Left, TDrawablePosition.Right]) then
        VW := VW + V + FDrawable.Padding;
      V := GetDrawableHeight;
      if (V > 0) and (FDrawable.Position in [TDrawablePosition.Top, TDrawablePosition.Bottom]) then
        VH := VH + V + FDrawable.Padding;

      if (FScrollbar = TViewScroll.Horizontal) and (FScrollBarObj <> nil) and (FScrollBarObj.Visible) then
        VH := VH + FScrollBarObj.Height;

      FContentBounds := RectF(Padding.Left, Padding.Top, VW, VH);
    end;

  finally
    FInFitSize := False;
  end;
end;

procedure TTextView.DoUpdateContentBounds;
var
  W, H: Single;
begin
  if (FScrollbar <> TViewScroll.None) and (not (csDestroying in ComponentState)) then begin
    W := FSize.Width;
    H := FSize.Height;
    DoRecalcSize(W, H);
  end;
end;

procedure TTextView.FreeScrollbar;
begin
  AutoCapture := False;
  if Assigned(FScrollBarObj) then begin
    RemoveComponent(FScrollBarObj);
    FreeAndNil(FScrollBarObj);
  end;
end;

function TTextView.GetAutoSize: Boolean;
begin
  Result := FText.AutoSize;
end;

function TTextView.GetContentBounds: TRectF;
begin

end;

function TTextView.GetData: TValue;
begin
  Result := Text;
end;

function TTextView.GetDefaultSize: TSizeF;
var
  DeviceInfo: IDeviceBehavior;
begin
  if TBehaviorServices.Current.SupportsBehaviorService(IDeviceBehavior, DeviceInfo, Self) then
    case DeviceInfo.GetOSPlatform(Self) of
      TOSPlatform.Windows:
        Result := TSizeF.Create(120, 17);
      TOSPlatform.OSX:
        Result := TSizeF.Create(120, 17);
      TOSPlatform.iOS:
        Result := TSize.Create(82, 21);
      TOSPlatform.Android:
        Result := TSize.Create(82, 23);
    end
  else
    Result := TSizeF.Create(120, 17);
end;

function TTextView.GetDrawable: TDrawableIcon;
begin
  if not Assigned(FDrawable) then begin
    FDrawable := TDrawableIcon.Create(Self);
    FDrawable.SizeWidth := 16;
    FDrawable.SizeHeight := 16;
    FDrawable.OnChanged := DoDrawableChanged;
  end;
  Result := FDrawable;
end;

function TTextView.GetDrawableHeight: Integer;
begin
  if Assigned(FDrawable) and (FDrawable.IsEmpty = False) then
    Result := FDrawable.SizeHeight
  else
    Result := 0;
end;

function TTextView.GetDrawableWidth: Integer;
begin
  if Assigned(FDrawable) and (FDrawable.IsEmpty = False) then
    Result := FDrawable.SizeWidth
  else
    Result := 0;
end;

function TTextView.GetHScrollBar: TScrollBar;
begin
  if FScrollbar = TViewScroll.Horizontal then   
    Result := FScrollBarObj
  else
    Result := nil;
end;

function TTextView.GetText: string;
begin
  Result := FText.Text;
end;

function TTextView.GetVScrollBar: TScrollBar;
begin
  if FScrollbar = TViewScroll.Vertical then   
    Result := FScrollBarObj
  else
    Result := nil;
end;

procedure TTextView.HScrollChange(Sender: TObject);
begin
  Repaint;
end;

procedure TTextView.ImagesChanged;
begin
  if Assigned(FDrawable) then
    FDrawable.Change;
  inherited ImagesChanged;
end;

procedure TTextView.InitScrollbar;
begin
  if (csDesigning in ComponentState) then 
    Exit;
  case FScrollbar of
    TViewScroll.Vertical: 
      begin
        if Assigned(FScrollBarObj) and (FScrollBarObj.Orientation = TOrientation.Horizontal) then
          Exit;
        FScrollBarObj := TSmallScrollBar.Create(Self);
        FScrollBarObj.Orientation := TOrientation.Vertical;
        FScrollBarObj.OnChange := VScrollChange;
        FScrollBarObj.Locked := True;
        FScrollBarObj.Align := TAlignLayout.Right;
        FScrollBarObj.SmallChange := SmallChangeFraction;
        FScrollBarObj.Parent := Self;
        FScrollBarObj.Visible := True;
        FScrollBarObj.Stored := False;
      end;
    TViewScroll.Horizontal:
      begin
        if Assigned(FScrollBarObj) and (FScrollBarObj.Orientation = TOrientation.Vertical) then
          Exit;
        FScrollBarObj := TSmallScrollBar.Create(Self);
        FScrollBarObj.Orientation := TOrientation.Horizontal;
        FScrollBarObj.OnChange := HScrollChange;
        FScrollBarObj.Locked := True;
        FScrollBarObj.Align := TAlignLayout.Bottom;
        FScrollBarObj.SmallChange := SmallChangeFraction;
        FScrollBarObj.Parent := Self;
        FScrollBarObj.Visible := True;  
        FScrollBarObj.Stored := False;
      end;
  end;
  if (FScrollbar <> TViewScroll.None) then begin
    DisableDisappear := True;
    AutoCapture := True;
    Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Pan];
    Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.Pan];
  end;
end;

function TTextView.IsAutoSize: Boolean;
begin
  Result := AutoSize;
end;

procedure TTextView.Loaded;
begin
  inherited;
  FText.OnChanged := DoChanged;
  if AutoSize then
    DoAutoSize;
  Change;
end;

procedure TTextView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  if (Button = TMouseButton.mbLeft) and (FScrollbar <> TViewScroll.None) then
    AniMouseDown(ssTouch in Shift, X, Y);
end;

procedure TTextView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (FScrollbar <> TViewScroll.None) and (ssLeft in Shift) then
    AniMouseMove(ssTouch in Shift, X, Y);
end;

procedure TTextView.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  Offset: Single;
begin
  inherited;
  if (not (Handled or FDisableMouseWheel)) and (not FContentBounds.IsEmpty) then begin
    if not Assigned(FScrollBarObj) then Exit;    
    if ssHorizontal in Shift then begin
      if (FContentBounds.Width > Width) and (FScrollbar = TViewScroll.Horizontal) then begin
        Offset := FScrollBarObj.SmallChange;
        Offset := Offset * -1 * (WheelDelta / 120);
        FScrollBarObj.Value := FScrollBarObj.Value + Offset;
        Handled := True;
      end;
    end else if (FContentBounds.Height > Height) and (FScrollbar = TViewScroll.Vertical) then begin
      Offset := FScrollBarObj.SmallChange;
      Offset := Offset * -1 * (WheelDelta / 120);
      FScrollBarObj.Value := FScrollBarObj.Value + Offset;
      Handled := True;
    end else if (FContentBounds.Width > Width) and (FScrollbar = TViewScroll.Horizontal) then begin
      Offset := FScrollBarObj.SmallChange;
      Offset := Offset * -1 * (WheelDelta / 120);
      FScrollBarObj.Value := FScrollBarObj.Value + Offset;
      Handled := True;
    end;
  end;
end;

procedure TTextView.PaddingChanged;
begin
  HandleSizeChanged;
end;

procedure TTextView.PaintBackground;
var
  R: TRectF;
begin
  R := RectF(0, 0, Width, Height);
  if Assigned(FOnDrawViewBackgroud) then
    DoDrawBackground(R)
  else
    inherited PaintBackground;
  DoPaintBackground(R);
end;

procedure TTextView.Resize;
begin
  inherited Resize;
  if AutoSize then
    DoAutoSize
  else if FScrollbar <> TViewScroll.None then
    DoUpdateContentBounds;
end;

procedure TTextView.SetAutoSize(const Value: Boolean);
begin
  FText.AutoSize := Value;
end;

procedure TTextView.SetData(const Value: TValue);
begin
  if Value.IsEmpty then
    Text := ''
  else
    Text := Value.ToString;
end;

procedure TTextView.SetDrawable(const Value: TDrawableIcon);
begin
  Drawable.Assign(Value);
end;

procedure TTextView.SetGravity(const Value: TLayoutGravity);
begin
  FGravity := Value;
  FText.Gravity := Value;
end;

procedure TTextView.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (csLoading in TComponent(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then
    Text := Value;
end;

procedure TTextView.SetNewScene(AScene: IScene);
begin
  inherited SetNewScene(AScene);
  if not (csLoading in ComponentState) then
    DoUpdateContentBounds;
end;

procedure TTextView.SetText(const Value: string);
begin
  FText.Text := Value;
end;

procedure TTextView.SetTextHint(const Value: string);
begin
  if FTextHint <> Value then begin
    FTextHint := Value;
    if FText.Text = '' then
      DoChanged(FText);
  end;
end;

procedure TTextView.SetTextSettings(const Value: UI.Base.TTextSettings);
begin
  FText.Assign(Value);
end;

function TTextView.TextStored: Boolean;
begin
  Result := (not Text.IsEmpty and not ActionClient) or (not (ActionClient and (ActionLink <> nil) and
    ActionLink.CaptionLinked and (Action is TContainedAction)));
end;

function TTextView.ToString: string;
begin
  Result := Format('%s ''%s''', [inherited ToString, FText]);
end;

procedure TTextView.VScrollChange(Sender: TObject);
begin
  Repaint;
end;

{ TStyleView }

function TStyleView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
begin
  if Assigned(FOnDrawViewBackgroud) then
    Result := True
  else
    Result := inherited CanRePaintBk(View, State);
end;

constructor TStyleView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TStyleView.Destroy;
begin
  inherited Destroy;
end;

procedure TStyleView.DoDrawStyleControl(var R: TRectF);
begin
  if Assigned(FOnDrawViewBackgroud) then
    FOnDrawViewBackgroud(Self, Canvas, R, DrawState);
end;

procedure TStyleView.PaintBackground;
begin
  inherited PaintBackground;
end;

{ TButtonView }

procedure TButtonView.AfterDialogKey(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Default and (Key = vkReturn)) or (Cancel and (Key = vkEscape)) then
  begin
    Click;
    Key := 0;
  end;
end;

procedure TButtonView.Click;
var
  O: TComponent;
begin
  inherited Click;
  if (Self <> nil) and (ModalResult <> mrNone) then
  begin
    O := Scene.GetObject;
    while O <> nil do
    begin
      if (O is TCommonCustomForm) then
      begin
        TCommonCustomForm(O).ModalResult := FModalResult;
        Break;
      end;
      O := O.Owner;
    end;
  end;
end;

constructor TButtonView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Clickable := True;
  CanFocus := True;
  Padding.Rect := RectF(4, 4, 4, 4);
  Gravity := TLayoutGravity.Center;
end;

function TButtonView.CreateBackground: TDrawable;
begin
  Result := TDrawableBorder.Create(Self, TViewBrushKind.Solid, $FFF0F1F2);
  Result.ItemPressed.Color := $FFE0E0E0;
  Result.ItemPressed.DefaultColor := Result.ItemPressed.Color;
  Result.ItemPressed.Kind := TViewBrushKind.Solid;
  Result.ItemEnabled.Color := $FFD1D2D3;
  Result.ItemEnabled.DefaultColor := Result.ItemEnabled.Color;
  Result.ItemEnabled.Kind := TViewBrushKind.Solid;
  with TDrawableBorder(Result).Border do begin
    Width := 1;
    DefaultStyle := TViewBorderStyle.RectBorder;
    Style := DefaultStyle;
    Color.Default := $AFCCCCCC;
    Color.Pressed := $FFC0C0C0;
    Color.Focused := $EFCCCCCC;
    Color.Hovered := $EFCCCCCC;
    Color.Checked := $EFCCCCCC;
    Color.Activated := $EFCCCCCC;
  end;
  Result.OnChanged := DoBackgroundChanged;
  Result.OnChanged := DoBackgroundChanged;
end;

function TButtonView.GetDefaultSize: TSizeF;
var
  DeviceInfo: IDeviceBehavior;
begin
  if TBehaviorServices.Current.SupportsBehaviorService(IDeviceBehavior, DeviceInfo, Self) then
    case DeviceInfo.GetOSPlatform(Self) of
      TOSPlatform.Windows:
        Result := TSizeF.Create(80, 22);
      TOSPlatform.OSX:
        Result := TSizeF.Create(80, 22);
      TOSPlatform.iOS:
        Result := TSizeF.Create(73, 44);
      TOSPlatform.Android:
        Result := TSizeF.Create(73, 44);
    end
  else
    Result := TSizeF.Create(80, 22);
end;

initialization

finalization

end.
