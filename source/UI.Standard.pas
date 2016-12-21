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
  UI.Base, UI.Utils,
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
  {$IF CompilerVersion > 30.0}
  FMX.AcceleratorKey,
  {$ENDIF}
  FMX.BehaviorManager, FMX.Forms, System.Messaging, FMX.Styles,
  FMX.ActnList, FMX.Objects, System.Math, System.Actions, System.Rtti, FMX.Consts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, FMX.TextLayout,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement,
  FMX.Ani, FMX.StdActns;

type
  TOnDrawText = procedure (Sender: TObject; Canvas: TCanvas;
    Text: UI.Base.TTextSettings; R: TRectF) of object;

  TOnDrawViewBackgroud = procedure (Sender: TObject; Canvas: TCanvas;
    const R: TRectF; State: TViewState) of object;

type
  /// <summary>
  /// 进度视图类型
  /// </summary>
  TProgressKind = (Horizontal {水平}, Vertical {垂直}, CircleRing {圆环});
type
  /// <summary>
  /// 进度视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TProgressView = class(TView)
  private
    FMin: Int64;
    FMax: Int64;
    FValue: Int64;
    FStartAngle: Single;
    FKind: TProgressKind;
    FOnValueChange: TNotifyEvent;
    FForeGround: TDrawable;
    FShapePath: TPathData;
    procedure SetForeGround(const Value: TDrawable);
    procedure SetMaxValue(const Value: Int64);
    procedure SetMinValue(const Value: Int64);
    procedure SetProValue(const Value: Int64);
    procedure SetKind(const Value: TProgressKind);
    procedure SetStartAngle(const Value: Single);
  protected
    procedure DoForegroundChanged(Sender: TObject); virtual;
    procedure DoValueChanged(Sender: TObject); virtual;
  protected
    procedure PaintBackground; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Min: Int64 read FMin write SetMinValue default 0;
    property Max: Int64 read FMax write SetMaxValue default 100;
    property Value: Int64 read FValue write SetProValue default 50;
    property ForeGround: TDrawable read FForeGround write SetForeGround;
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property Kind: TProgressKind read FKind write SetKind default TProgressKind.Horizontal;
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  end;

type
  TScrollView = class;
  TOnCalcContentBoundsEvent = procedure (Sender: TObject; var ContentBounds: TRectF) of object;
  PRectD = ^TRectD;

  /// <summary>
  /// 滚动视图
  /// </summary>
  TScrollView = class(TView)
  private const
    ChangeRepaintedIncidentDelay = 0.1; // seconds
    PhysicsProcessingInterval = 8; // 8 ms for ~120 frames per second
    DefaultScrollingStretchGlowColor: TAlphaColor = $FFC0C0C0;
  private
    FOnScrollChange: TNotifyEvent;
    FCanScroll: Boolean;
    FInInternalAlign: Boolean;
    FShowScrollBars: Boolean;
    FCachedAutoShowing: Boolean;
    function GetViewportPosition: TPointD;
    procedure SetViewportPosition(const Value: TPointD);
    procedure SetShowScrollBars(const Value: Boolean);
    function GetScrollValue: Single;
    function IsStoredScrollStretchGlowColor: Boolean; virtual;
  protected
    FScrolling: Boolean;
    FSystemInfoSrv: IFMXSystemInformationService;
    FListingService: IFMXListingService;
    //Animation mouse events
    procedure AniMouseDown(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseMove(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseUp(const Touch: Boolean; const X, Y: Single); virtual;

    procedure AniCalcChange(Sender: TObject);

    procedure CMGesture(var EventInfo: TGestureEventInfo); override;

    function IsRunningOnDesktop: Boolean;
    function HasTouchTracking: Boolean;
    function HasStretchyScrolling: Boolean;
    function HasScrollingStretchGlow: Boolean;
    function HasPhysicsStretchyScrolling: Boolean;
    function GetMaxScrollViewPos: Integer;
    procedure ScrollStretchChanged; virtual;
    procedure UpdateScrollStretchStrength(const NewValue: Single);
  protected
    FScroll: TScrollBar;
    FContentBounds: PRectD;
    FAniCalculations: TScrollCalculations;
    FLastViewportPosition: TPointD;
    FMouseEvents: Boolean;
    FScrollStretchStrength: Single;
    FScrollTrackPressed: Boolean;
    FScrollingStretchGlowColor: TAlphaColor;

    procedure Resize; override;
    procedure DoRealign; override;
    procedure DoInVisibleChange; override;
    function IsOpaque: Boolean; virtual;

    function CreateScroll: TScrollBar; virtual;
    procedure InitScrollbar; override;
    procedure FreeScrollbar; override;
    procedure HScrollChange(Sender: TObject); virtual;
    procedure VScrollChange(Sender: TObject); virtual;
    function GetVScrollBar: TScrollBar; override;
    function GetHScrollBar: TScrollBar; override;
    function GetContentBounds: TRectD; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure InvalidateContentSize(); virtual;
    procedure RealignContent;
    procedure InternalAlign; override;

    procedure Adjust(var ContentLayoutRect: TRectD); virtual;

    function GetColorFromStyle(const ObjectName: string; const DefaultColor: TAlphaColor): TAlphaColor;
    function NeedPaintScrollingStretchGlow: Boolean;
    procedure PaintScrollingStretchGlow(const ACanvas: TCanvas;
      const AWidth, AHeight, AIntensity, AOpacity: Single);

    function GetAniCalculations: TScrollCalculations; override;
    function GetScrollingBehaviours: TScrollingBehaviours;
    function CreateAniCalculations: TScrollCalculations; virtual;
    procedure DoScrollVisibleChange; virtual;
    procedure DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations); virtual;
    procedure UpdateAniCalculations;
    procedure DoUpdateScrollingLimits(NeedUpdateScroll: Boolean = False); virtual;
    procedure UpdateScrollBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollBy(const Dx, Dy: Single);
    procedure ScrollTo(const Dx, Dy: Single);
    function VScrollBarValue: Double;
    function HScrollBarValue: Double;
    // 获取滚动条所在位置的百分比
    property ScrollValue: Single read GetScrollValue;
    // 是否可以滚动
    property CanScroll: Boolean read FCanScroll;
    // 是否显示滚动条
    property ShowScrollBars: Boolean read FShowScrollBars write SetShowScrollBars default True;
    // 视口位置
    property ViewportPosition: TPointD read GetViewportPosition write SetViewportPosition;
    // 滚动伸展区颜色
    property ScrollStretchGlowColor: TAlphaColor read FScrollingStretchGlowColor write FScrollingStretchGlowColor stored IsStoredScrollStretchGlowColor;
    property OnScrollChange: TNotifyEvent read FOnScrollChange write FOnScrollChange;
  published
  end;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TTextView = class(TScrollView, ICaption{$IF CompilerVersion > 30.0}, IAcceleratorKeyReceiver{$ENDIF})
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
    function GetTextLength: Integer;
  protected
    procedure Loaded; override;
    procedure DblClick; override;
    procedure ImagesChanged; override;
    procedure PaintBackground; override;
    procedure DoDrawBackground(var R: TRectF); virtual;
    procedure DoPaintBackground(var R: TRectF); virtual;
    procedure DoPaintText(var R: TRectF); virtual;
    function DoGetUpdateRect: TRectF; override;
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
    procedure DoScrollVisibleChange; override;
    procedure DoAutoSize;
    procedure DoUpdateContentBounds;
  protected
    {Scrollbar}
    function CreateBackground: TDrawable; override;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
    { IAcceleratorKeyReceiver }
    procedure TriggerAcceleratorKey; virtual;
    function CanTriggerAcceleratorKey: Boolean; virtual;
    function GetAcceleratorChar: Char;
    function GetAcceleratorCharIndex: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ToString: string; override;
    procedure SetNewScene(AScene: IScene); override;
    procedure AfterConstruction; override;
    procedure Change;
    property Length: Integer read GetTextLength;
  published
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
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
    procedure DoDrawStyleControl(var R: TRectF); virtual;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
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
    procedure SetScrollbar(const Value: TViewScroll); override;
    property ScrollBars;
  published
    property CanFocus default True;
    property CanParentFocus;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Default: Boolean read FDefault write FDefault default False;
    property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;
    property HitTest default True;
    property Clickable default True;
    property Gravity default TLayoutGravity.Center;
    property OnCanFocus;
  end;

implementation

procedure DisableHitTestForControl(const AControl: TControl);
var
  LChild: TFmxObject;
begin
  AControl.HitTest := False;
  if AControl.Children <> nil then
    for LChild in AControl.Children do
      if LChild is TControl then
        DisableHitTestForControl(TControl(LChild));
end;

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

procedure TTextView.DoAutoSize;
var
  W, H: Single;
begin
  if FInFitSize or (not FAdjustViewBounds) then
    Exit;
  if TextSettings.WordWrap then begin // 只有需要自动换行时，才需要判断父级组件的宽度
    W := GetParentMaxWidth;
    H := GetParentMaxHeight;
  end else begin
    W := 0;
    H := 0;
  end;
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
  if (not Result) then begin
    if (Assigned(FDrawable)) then
      Result := not FDrawable.IsEmpty;
    if (not Result) and (Assigned(FBackground)) then begin
      Border := TDrawableBorder(FBackground).Border;
      Result := Assigned(Border) and (Border.Style <> TViewBorderStyle.None) and
        (Border.Width > 0) and (Border.Color.GetColor(State) <> TAlphaColorRec.Null);
      if (not Result) and (FText.TextLength > 0) then
        Result := (HitTest) or (TViewState.Pressed in FViewState);
    end;
  end;
end;

function TTextView.CanTriggerAcceleratorKey: Boolean;
begin
  Result := ParentedVisible;
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
  if FText.IsSizeChange or FText.IsTextChange then begin
    if IsAutoSize then begin
      DoAutoSize;
    end else begin
      DoUpdateContentBounds;
    end;
  end;
  Repaint;
  if FText.IsEffectsChange then
    UpdateEffects;
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

function TTextView.DoGetUpdateRect: TRectF;
begin
  Result := inherited DoGetUpdateRect;
//  if Assigned(FScroll) and (FScroll.Value > Padding.Top) then begin
//    Result.Top := Result.Top + Padding.Top;
//    if not FText.WordWrap then
//      Result.Right := Result.Right - Padding.Right;
//  end;
end;

procedure TTextView.DoLayoutChanged(Sender: TObject);
begin
  inherited DoLayoutChanged(Sender);
  if (HeightSize = TViewSize.WrapContent) or IsAutoSize then
    DoAutoSize
  else if FScrollbar <> TViewScroll.None then
    DoUpdateContentBounds;
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
    if Assigned(FScroll) and (FScroll.Visible) then begin
      case FScrollbar of
        TViewScroll.None: SR := R;
        TViewScroll.Horizontal: 
          begin
            SR := GetRectF(FContentBounds^);
            SR.Top := R.Top;
            // windows平台显示滚动条，其它平台会自动隐藏
            SR.Bottom := R.Bottom{$IFDEF MSWINDOWS} - FScroll.Height{$ENDIF};
            OffsetRect(SR, -(ScrollValue * (SR.Width - R.Width)), 0);
          end;
        TViewScroll.Vertical: 
          begin
            SR := GetRectF(FContentBounds^);
            SR.Left := R.Left;
            SR.Right := R.Right{$IFDEF MSWINDOWS} - FScroll.Width{$ENDIF};
            OffsetRect(SR, 0, -(ScrollValue * (SR.Height - R.Height)));
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
      V := AWidth - Padding.Left - Padding.Right - Margins.Left - Margins.Right;
      // 如果有icon，并且是在左边或右边，则还需要减去icon大小
      IconS := GetDrawableWidth;
      if (IconS > 0) and (FDrawable.Position in [TDrawablePosition.Left, TDrawablePosition.Right]) then
        V := V - IconS - FDrawable.Padding;
      {$IFDEF MSWINDOWS}
      if (FScrollbar = TViewScroll.Vertical) and (FScroll <> nil) and (FScroll.Visible) then
        V := V - FScroll.Width;
      {$ENDIF}
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

      if (FScrollbar = TViewScroll.Horizontal) and (FScroll <> nil) and (FScroll.Visible) then
        VH := VH + FScroll.Height;

      if FContentBounds = nil then
        New(FContentBounds);
      FContentBounds^ := RectD(Padding.Left, Padding.Top, VW - Padding.Right, VH - Padding.Bottom);
      RealignContent;
    end;

  finally
    FInFitSize := False;
  end;
end;

procedure TTextView.DoScrollVisibleChange;
begin
  Resize;
end;

procedure TTextView.DoUpdateContentBounds;
var
  W, H: Single;
begin
  if csDestroying in ComponentState then
    Exit;
  if IsAutoSize or (FScrollbar <> TViewScroll.None) then begin
    W := FSize.Width;
    H := FSize.Height;
    DoRecalcSize(W, H);
    if (W <> FSize.Width) or (H <> FSize.Height) then
      FSize.Size := TSizeF.Create(W, H);
  end;
end;

function TTextView.GetAcceleratorChar: Char;
begin
  Result := #0;
end;

function TTextView.GetAcceleratorCharIndex: Integer;
begin
  Result := -1;
end;

function TTextView.GetAutoSize: Boolean;
begin
  Result := FText.AutoSize;
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

function TTextView.GetText: string;
begin
  Result := FText.Text;
end;

function TTextView.GetTextLength: Integer;
begin
  Result := FText.TextLength;
end;

procedure TTextView.ImagesChanged;
begin
  if Assigned(FDrawable) then
    FDrawable.Change;
  inherited ImagesChanged;
end;

function TTextView.IsAutoSize: Boolean;
begin
  Result := AutoSize or (FText.WordWrap and (HeightSize = TViewSize.WrapContent)) or (WidthSize = TViewSize.WrapContent);
end;

procedure TTextView.Loaded;
begin
  inherited;
  FText.OnChanged := DoChanged;
  Change;
end;

procedure TTextView.PaddingChanged;
begin
  HandleSizeChanged;
end;

procedure TTextView.PaintBackground;
var
  R: TRectF;
begin
  if AbsoluteInVisible then
    Exit;
  R := RectF(0, 0, Width, Height);
  if Assigned(FOnDrawViewBackgroud) then
    DoDrawBackground(R)
  else
    inherited PaintBackground;
  DoPaintBackground(R);
end;

procedure TTextView.Resize;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  inherited Resize;
  if IsAutoSize then
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

procedure TTextView.TriggerAcceleratorKey;
begin
  SetFocus;
end;

{ TStyleView }

function TStyleView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
begin
  if Assigned(FOnDrawViewBackgroud) then
    Result := True
  else
    Result := inherited CanRePaintBk(View, State);
end;

procedure TStyleView.DoDrawStyleControl(var R: TRectF);
begin
  if Assigned(FOnDrawViewBackgroud) then
    FOnDrawViewBackgroud(Self, Canvas, R, DrawState);
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
  with TDrawableBorder(Result).Border do begin
    Width := 1;
    DefaultStyle := TViewBorderStyle.RectBorder;
    Style := DefaultStyle;
    Color.Default := $AFCCCCCC;
    Color.DefaultChange := False;
    Color.Pressed := $FFC0C0C0;
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

procedure TButtonView.SetScrollbar(const Value: TViewScroll);
begin
  FScrollbar := Value; // 不初始化滚动条
end;

{ TScrollView }

procedure TScrollView.Adjust(var ContentLayoutRect: TRectD);
begin
  if FContentBounds <> nil then
    ContentLayoutRect := FContentBounds^
  else
    ContentLayoutRect := TRectD.Empty;
end;

procedure TScrollView.AniCalcChange(Sender: TObject);
var
  NewViewPos, MaxScrollViewPos: Single;
begin
  NewViewPos := FAniCalculations.ViewportPosition.Y;
  MaxScrollViewPos := GetMaxScrollViewPos;

  if NewViewPos < 0 then
    UpdateScrollStretchStrength(NewViewPos)
  else if NewViewPos > MaxScrollViewPos then
    UpdateScrollStretchStrength(NewViewPos - MaxScrollViewPos)
  else
    UpdateScrollStretchStrength(0);
end;

procedure TScrollView.AniMouseDown(const Touch: Boolean; const X, Y: Single);

  function GetScrollPressed: Boolean;
  var
    FTrack: TCustomTrack;
    FThumb: TThumb;
  begin
    Result := False;
    FTrack := GetRttiObject(FScroll, 'FTrack') as TCustomTrack;
    if not Assigned(FTrack) then Exit;
    FThumb := GetRttiObject(FTrack, 'FThumb') as TThumb;
    Result := Assigned(FThumb) and (FThumb.Pressed);
  end;

begin
  FScrollTrackPressed := FScroll.Pressed or GetScrollPressed;
  FAniCalculations.Averaging := Touch;
  FAniCalculations.MouseDown(X, Y);
end;

procedure TScrollView.AniMouseMove(const Touch: Boolean; const X, Y: Single);
begin
  FAniCalculations.MouseMove(X, Y);
  if FAniCalculations.Moved then
    FAniCalculations.Shown := True;
end;

procedure TScrollView.AniMouseUp(const Touch: Boolean; const X, Y: Single);
begin
  if FScrollTrackPressed then  // 更新一下按钮时的位置，不然会回弹
    SetRttiValue<TPointD>(FAniCalculations, 'FDownPosition', ViewportPosition);
  FAniCalculations.MouseUp(X, Y);
  if (FAniCalculations.LowVelocity) or
     (not FAniCalculations.Animation) then
    FAniCalculations.Shown := False;
end;

procedure TScrollView.CMGesture(var EventInfo: TGestureEventInfo);
var
  LP: TPointF;
begin
  if (FCanScroll) and (EventInfo.GestureID = igiPan) then
  begin
    if FInVisible or (FAniCalculations = nil) then
      Exit;
    FMouseEvents := False;
    LP := AbsoluteToLocal(EventInfo.Location);
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then begin
      AniMouseDown(True, LP.X, LP.Y)
    end else
      if EventInfo.Flags = [] then begin
        AniMouseMove(True, LP.X, LP.Y);
      end else if AniCalculations.Down then begin
        AniMouseUp(True, LP.X, LP.Y);
      end;
  end
  else
    inherited CMGesture(EventInfo);
end;

constructor TScrollView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContentBounds := nil;
  FShowScrollBars := True;
  FScrollingStretchGlowColor := GetColorFromStyle('glow', DefaultScrollingStretchGlowColor);
  SupportsPlatformService(IFMXSystemInformationService, FSystemInfoSrv);
  SupportsPlatformService(IFMXListingService, FListingService);
end;

function TScrollView.CreateAniCalculations: TScrollCalculations;
begin
  Result := TScrollCalculations.Create(Self);
  Result.Interval := PhysicsProcessingInterval;
  Result.OnChanged := AniCalcChange;
end;

function TScrollView.CreateScroll: TScrollBar;
begin
  Result := TSmallScrollBar.Create(Self);
end;

destructor TScrollView.Destroy;
begin
  FreeAndNil(FAniCalculations);
  inherited Destroy;
  if FContentBounds <> nil then begin
    Dispose(FContentBounds);
    FContentBounds := nil;    
  end;
end;

procedure TScrollView.DoInVisibleChange;
var
  V: Boolean;
begin
  inherited DoInVisibleChange;
  if Assigned(FScroll) then begin
    if FInVisible then
      V := False
    else
      V := FCanScroll and FShowScrollBars;
    if FScroll.Visible <> V then
      FScroll.Visible := V;
  end;
end;

procedure TScrollView.DoMouseLeave;
begin
  inherited;
  if FMouseEvents and Assigned(FAniCalculations) and FAniCalculations.Down then
  begin
    FAniCalculations.MouseLeave;
    if (FAniCalculations.LowVelocity) or
       (not FAniCalculations.Animation) then
      FAniCalculations.Shown := False;
  end;
end;

procedure TScrollView.DoRealign;
var
  LDisablePaint, LDisableInternalAlign: Boolean;
begin
  LDisableInternalAlign := (not FCanScroll) or FDisableAlign or
    (csDestroying in ComponentState) or (FUpdating > 0) or
    (csLoading in ComponentState);
  LDisablePaint := FDisablePaint;
  try
    FDisablePaint := True;
    inherited;
    if not LDisableInternalAlign then
    begin
      InternalAlign;
    end;
  finally
    FDisablePaint := LDisablePaint;
  end;
end;

procedure TScrollView.DoScrollVisibleChange;
begin
  Realign;
end;

procedure TScrollView.DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations);
begin
  AAniCalculations.Animation := TScrollingBehaviour.Animation in GetScrollingBehaviours;
  if TScrollingBehaviour.TouchTracking in GetScrollingBehaviours then
    AAniCalculations.TouchTracking := [ttVertical, ttHorizontal]
  else
    AAniCalculations.TouchTracking := [];
  AAniCalculations.BoundsAnimation := True; //TScrollingBehaviour.BoundsAnimation in GetScrollingBehaviours;
  AAniCalculations.AutoShowing := TScrollingBehaviour.AutoShowing in GetScrollingBehaviours;
  if FScrollbar = TViewScroll.Vertical then
    AAniCalculations.TouchTracking := AAniCalculations.TouchTracking - [ttHorizontal]
  else if FScrollbar = TViewScroll.Horizontal then
    AAniCalculations.TouchTracking := AAniCalculations.TouchTracking - [ttVertical];
end;

procedure TScrollView.DoUpdateScrollingLimits(NeedUpdateScroll: Boolean);
var
  Targets: array [0..1] of TAniCalculations.TTarget;
begin
  if FAniCalculations <> nil then
  begin
    Targets[0].TargetType := TAniCalculations.TTargetType.Min;
    Targets[0].Point := TPointD.Create(0, 0);
    Targets[1].TargetType := TAniCalculations.TTargetType.Max;
    Targets[1].Point := TPointD.Create(0, Padding.Top + Max(FContentBounds.Height - ViewRect.Height, 0));

    FAniCalculations.SetTargets(Targets);

    if NeedUpdateScroll or (not HasTouchTracking) then
      UpdateScrollBar;
  end;
end;

procedure TScrollView.FreeScrollbar;
begin
  AutoCapture := False;
  if Assigned(FScroll) then begin
    RemoveComponent(FScroll);
    FreeAndNil(FScroll);     
  end;
end;

function TScrollView.GetAniCalculations: TScrollCalculations;
begin
  Result := FAniCalculations;
end;

function TScrollView.GetColorFromStyle(const ObjectName: string;
  const DefaultColor: TAlphaColor): TAlphaColor;
var
  StyleObject: TFmxObject;
begin
  StyleObject := FindStyleResource(ObjectName);
  if StyleObject is TColorObject then
    Result := TColorObject(StyleObject).Color
  else
    Result := DefaultColor;
end;

function TScrollView.GetContentBounds: TRectD;
begin
  if FContentBounds = nil then
    Result := TRectD.Empty
  else
    Result := FContentBounds^;
end;

function TScrollView.GetHScrollBar: TScrollBar;
begin
  if FScrollbar = TViewScroll.Horizontal then
    Result := FScroll
  else
    Result := nil;
end;

function TScrollView.GetMaxScrollViewPos: Integer;
begin
  Result := Max(Round(FAniCalculations.MaxTarget.Point.Y), 0);
end;

function TScrollView.GetScrollingBehaviours: TScrollingBehaviours;
var
  StyleDescriptor: TStyleDescription;
begin
  if Scene <> nil then
    StyleDescriptor := TStyleManager.GetStyleDescriptionForControl(Self)
  else
    StyleDescriptor := nil;
  if (StyleDescriptor <> nil) and StyleDescriptor.PlatformTarget.Contains('[METROPOLISUI]') then
    Result := [TScrollingBehaviour.AutoShowing]
  else
  begin
    if FSystemInfoSrv <> nil then
      Result := FSystemInfoSrv.GetScrollingBehaviour
    else
      Result := [];
  end;
end;

function TScrollView.GetScrollValue: Single;
begin
  Result := (FScroll.Value - FScroll.Min) / (FScroll.Max - FScroll.Min - FScroll.ViewportSize);
end;

function TScrollView.GetViewportPosition: TPointD;
var
  LScale, X, Y: Double;
begin
  LScale := GetSceneScale;
  X := Round(FAniCalculations.ViewportPosition.X * LScale) / LScale;
  Y := Round(FAniCalculations.ViewportPosition.Y * LScale) / LScale;
  Result := TPointD.Create(X, Y);
end;

function TScrollView.GetVScrollBar: TScrollBar;
begin
  if FScrollbar = TViewScroll.Vertical then
    Result := FScroll
  else
    Result := nil;
end;

function TScrollView.HasPhysicsStretchyScrolling: Boolean;
begin
  Result := HasTouchTracking and HasStretchyScrolling;
end;

function TScrollView.HasScrollingStretchGlow: Boolean;
begin
  Result := (FListingService <> nil) and (TListingTransitionFeature.ScrollGlow in FListingService.GetTransitionFeatures);
end;

function TScrollView.HasStretchyScrolling: Boolean;
begin
  Result := HasTouchTracking and (FSystemInfoSrv <> nil) and
    (TScrollingBehaviour.BoundsAnimation in FSystemInfoSrv.GetScrollingBehaviour);
end;

function TScrollView.HasTouchTracking: Boolean;
begin
  Result := (FAniCalculations <> nil) or ((FSystemInfoSrv <> nil) and
    (TScrollingBehaviour.TouchTracking in FSystemInfoSrv.GetScrollingBehaviour));
end;

function TScrollView.HScrollBarValue: Double;
begin
  if FAniCalculations <> nil then
    Result := ViewportPosition.X
  else
    Result := 0;
end;

procedure TScrollView.HScrollChange(Sender: TObject);
begin
  if FScrolling or (FAniCalculations = nil) then Exit;
  FScrolling := True;
  ViewportPosition := PointF(HScrollBar.Value, ViewportPosition.Y);
  if not IsOpaque then
    UpdateEffects;
  if Assigned(FOnScrollChange) then
    FOnScrollChange(self);
  FAniCalculations.Shown := True;
  FScrolling := False;
end;

procedure TScrollView.InitScrollbar;
begin
  if (csDesigning in ComponentState) then
    Exit;
  if (FContentBounds = nil) and (FScrollbar <> TViewScroll.None) then begin
    New(FContentBounds);
    FContentBounds^ := TRectD.Empty;
  end;
  case FScrollbar of
    TViewScroll.Vertical:
      begin
        if Assigned(FScroll) and (FScroll.Orientation = TOrientation.Horizontal) then
          Exit;
        FScroll := CreateScroll();
        FScroll.Orientation := TOrientation.Vertical;
        FScroll.OnChange := VScrollChange;
        FScroll.Locked := True;
        FScroll.Align := TAlignLayout.Right;
        FScroll.SmallChange := SmallChangeFraction;
        FScroll.Parent := Self;
        FScroll.Visible := False;
        FScroll.Stored := False;
        FScroll.Margins.Rect := TRectF.Create(0, 0, 0, 0);
      end;
    TViewScroll.Horizontal:
      begin
        if Assigned(FScroll) and (FScroll.Orientation = TOrientation.Vertical) then
          Exit;
        FScroll := CreateScroll();
        FScroll.Orientation := TOrientation.Horizontal;
        FScroll.OnChange := HScrollChange;
        FScroll.Locked := True;
        FScroll.Align := TAlignLayout.Bottom;
        FScroll.SmallChange := SmallChangeFraction;
        FScroll.Parent := Self;
        FScroll.Visible := False;
        FScroll.Stored := False;
        FScroll.Margins.Rect := TRectF.Create(0, 0, 0, 0);
      end;
  end;
  UpdateAniCalculations;
  if (FScrollbar <> TViewScroll.None) then begin
    DisableDisappear := True;
    AutoCapture := True;
    Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Pan];
    Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.Pan];
  end;
  RealignContent;
end;

procedure TScrollView.InternalAlign;
var
  LViewportPosition: TPointD;
  ContentLayoutRect: TRectD;
  VR: TRectF;
  NewTargets: array of TAniCalculations.TTarget;
begin
  if (not FInInternalAlign) and (FAniCalculations <> nil) then
  begin
    FInInternalAlign := True;
    try
      if (FAniCalculations <> nil) and not Released then
      begin
        if (FCachedAutoShowing <> FAniCalculations.AutoShowing) and not FAniCalculations.AutoShowing then
          InvalidateContentSize;
        FCachedAutoShowing := FAniCalculations.AutoShowing;
      end;
      if (not FAniCalculations.Down) and FAniCalculations.LowVelocity then
        FAniCalculations.Shown := False;
      Adjust(ContentLayoutRect);
      FScroll.Opacity := AniCalculations.Opacity{$IFNDEF MSWINDOWS} - 0.1{$ENDIF};
      LViewportPosition := ViewportPosition;
      if FLastViewportPosition = LViewportPosition then
        Exit;
      FLastViewportPosition := LViewportPosition;
      VR := ViewRect;
      UpdateVScrollBar(LViewportPosition.Y, VR.Height);
      UpdateHScrollBar(LViewportPosition.X, VR.Width);
      if not (csDesigning in ComponentState) then begin
        if (not FAniCalculations.Animation) then
          FAniCalculations.UpdatePosImmediately(True)
        else if FMouseEvents then begin
          SetLength(NewTargets, 1);
          NewTargets[0].Point := LViewportPosition;
          FAniCalculations.MouseTarget := NewTargets[0];
        end;
      end;
    finally
      Repaint;
      FInInternalAlign := False;
    end;
  end;
end;

procedure TScrollView.InvalidateContentSize;
begin
end;

function TScrollView.IsOpaque: Boolean;
begin
  Result := False;
end;

function TScrollView.IsRunningOnDesktop: Boolean;
begin
  Result := TOSVersion.Platform in [pfWindows, pfMacOS, pfLinux];
end;

function TScrollView.IsStoredScrollStretchGlowColor: Boolean;
begin
  Result := FScrollingStretchGlowColor <> DefaultScrollingStretchGlowColor;
end;

procedure TScrollView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if Assigned(FAniCalculations) and (Button = TMouseButton.mbLeft) then
  begin
    AniMouseDown(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if Assigned(FAniCalculations) and FAniCalculations.Down then
  begin
    AniMouseMove(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if Assigned(FAniCalculations) and (Button = TMouseButton.mbLeft) then
  begin
    AniMouseUp(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollView.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  Offset: Single;
begin
  inherited;
  if (not (Handled or FDisableMouseWheel)) and (FCanScroll) and (not FInVisible) then
  begin
    if FScrollbar = TViewScroll.Vertical then begin
      FAniCalculations.Shown := True;
      if VScrollBar <> nil then begin
        Offset := VScrollBar.SmallChange
      end else
        Offset := FContentBounds.Height / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      FAniCalculations.MouseWheel(0, Offset);
      Handled := True;
    end else begin
      FAniCalculations.Shown := True;
      if HScrollBar <> nil then
        Offset := HScrollBar.SmallChange
      else
        Offset := FContentBounds.Width / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      FAniCalculations.MouseWheel(Offset, 0);
      Handled := True;
    end;
  end;
end;

function TScrollView.NeedPaintScrollingStretchGlow: Boolean;
begin
  Result := HasScrollingStretchGlow and (Abs(FScrollStretchStrength) > 0);
end;

procedure TScrollView.PaintScrollingStretchGlow(const ACanvas: TCanvas;
  const AWidth, AHeight, AIntensity, AOpacity: Single);
var
  TempPoint: TGradientPoint;
  TempColor: TAlphaColor;
  GlowBrush: TBrush;
  GlowDepth: Single;
  GlowRect: TRectF;
begin
  GlowBrush := TBrush.Create(TBrushKind.Gradient, FScrollingStretchGlowColor);
  try
    GlowBrush.Gradient.Style := TGradientStyle.Radial;
    GlowBrush.Gradient.Points.Clear;
    TempColor := FScrollingStretchGlowColor;

    TempPoint := TGradientPoint.Create(GlowBrush.Gradient.Points);
    TAlphaColorRec(TempColor).A := 0;
    TempPoint.Color := TempColor;
    TempPoint.Offset := 0;

    TempPoint := TGradientPoint.Create(GlowBrush.Gradient.Points);
    TAlphaColorRec(TempColor).A := 255;
    TempPoint.Color := TempColor;
    TempPoint.Offset := 1;

    GlowDepth := Max((Sqrt(Abs(AIntensity)) - 3) * 3, 0);
    if GlowDepth > TEpsilon.Position then
    begin
      if AIntensity < 0 then
      begin
        GlowRect := TRectF.Create(-AWidth / 8, -GlowDepth, AWidth + AWidth / 8, GlowDepth);
      end else
        GlowRect := TRectF.Create(-AWidth / 8, Height - GlowDepth, AWidth + AWidth / 8, AHeight + GlowDepth);

      ACanvas.FillEllipse(GlowRect, AOpacity, GlowBrush);
    end;
  finally
    GlowBrush.Free;
  end;
end;

procedure TScrollView.RealignContent;
begin
  case FScrollbar of
    TViewScroll.None: FCanScroll := False;
    TViewScroll.Horizontal: FCanScroll := FContentBounds.Width > ViewRect.Width;
    TViewScroll.Vertical: FCanScroll := FContentBounds.Height > ViewRect.Height;
  end;
  if Assigned(FScroll) then begin
    InvalidateContentSize;
    UpdateScrollBar;
    DoUpdateScrollingLimits;
  end;
end;

procedure TScrollView.Resize;
begin
  inherited Resize;
  if (FScroll <> nil) and (FContentBounds <> nil) then
    DoUpdateScrollingLimits;
end;

procedure TScrollView.ScrollBy(const Dx, Dy: Single);
begin
  if VScrollBar <> nil then
    VScrollBar.Value := VScrollBar.Value - Dy;
  if HScrollBar <> nil then
    HScrollBar.Value := HScrollBar.Value - Dx;
end;

procedure TScrollView.ScrollStretchChanged;
begin
  if HasScrollingStretchGlow then
    Invalidate;
end;

procedure TScrollView.ScrollTo(const Dx, Dy: Single);
begin
  if VScrollBar <> nil then
    VScrollBar.Value := Dy;
  if HScrollBar <> nil then
    HScrollBar.Value := Dx;
end;

procedure TScrollView.SetShowScrollBars(const Value: Boolean);
begin
  if FShowScrollBars <> Value then
  begin
    FShowScrollBars := Value;
    if FShowScrollBars then
      InvalidateContentSize;
    Realign;
  end;
end;

procedure TScrollView.SetViewportPosition(const Value: TPointD);
var
  LScale, X, Y: Double;
begin
  LScale := GetSceneScale;
  X := Value.X;
  Y := Value.Y;
  FAniCalculations.ViewportPosition := TPointD.Create(Round(X * LScale) / LScale, Round(Y * LScale) / LScale);
end;

procedure TScrollView.UpdateAniCalculations;
begin
  if not (csDestroying in ComponentState) then
  begin
    if FAniCalculations = nil then
      FAniCalculations := CreateAniCalculations;
    FAniCalculations.BeginUpdate;
    try
      DoUpdateAniCalculations(FAniCalculations);
    finally
      FAniCalculations.EndUpdate;
    end;
  end;
end;

procedure TScrollView.UpdateScrollBar;
var
  LViewportPosition: TPointD;
  R: TRectF;
begin
  LViewportPosition := ViewportPosition;
  R := ViewRect;
  if FScrollbar = TViewScroll.Vertical then begin
    FCanScroll := FContentBounds.Height > R.Height;
    if (LViewportPosition.Y > FContentBounds.Height - FScroll.ViewportSize) and
      (LViewportPosition.Y > FAniCalculations.MaxTarget.Point.Y) then
      LViewportPosition.Y := FAniCalculations.MaxTarget.Point.Y;
    UpdateVScrollBar(LViewportPosition.Y, R.Height);
  end else if FScrollbar = TViewScroll.Horizontal then begin
    FCanScroll := FContentBounds.Width > R.Width;
    if (LViewportPosition.X > FContentBounds.Width - FScroll.ViewportSize) and
      (LViewportPosition.X > FAniCalculations.MaxTarget.Point.X) then
      LViewportPosition.X := FAniCalculations.MaxTarget.Point.X;
    UpdateHScrollBar(LViewportPosition.X, R.Width);
  end else begin
    FCanScroll := False;
  end;
  if FScroll.Visible <> FCanScroll then begin
    FScroll.Visible := FCanScroll and FShowScrollBars and (not FInVisible);
    DoScrollVisibleChange;
  end;
end;

procedure TScrollView.UpdateScrollStretchStrength(const NewValue: Single);
begin
  if not SameValue(FScrollStretchStrength, NewValue, TEpsilon.Position) then
  begin
    FScrollStretchStrength := NewValue;
    ScrollStretchChanged;
  end;
end;

function TScrollView.VScrollBarValue: Double;
begin
  if FAniCalculations <> nil then begin
    Result := ViewportPosition.Y; // / (FScroll.Max - FScroll.ViewportSize) * FScroll.Max
  end else
    Result := 0;
end;

procedure TScrollView.VScrollChange(Sender: TObject);
begin
  if FScrolling or (FAniCalculations = nil) then Exit;
  FScrolling := True;
  ViewportPosition := TPointF.Create(ViewportPosition.X, VScrollBar.Value);
  if not IsOpaque then
    UpdateEffects;
  if Assigned(FOnScrollChange) then
    FOnScrollChange(self);
  FAniCalculations.Shown := True;
  FScrolling := False;
end;


{ TProgressView }

constructor TProgressView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMax := 100;
  FValue := 50;
  FKind := TProgressKind.Horizontal;
  FStartAngle := 0;

  FForeGround := TDrawableBorder.Create(Self);
  FForeGround.ItemDefault.Color := TAlphaColorRec.Blue;
  FForeGround.ItemDefault.DefaultColor := TAlphaColorRec.Blue;
  FForeGround.ItemDefault.Kind := TViewBrushKind.Solid;
  FForeGround.ItemDefault.DefaultKind := TBrushKind.Solid;
  FForeGround.OnChanged := DoForegroundChanged;
end;

destructor TProgressView.Destroy;
begin
  FreeAndNil(FForeGround);
  FreeAndNil(FShapePath);
  inherited Destroy;
end;

procedure TProgressView.DoForegroundChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TProgressView.DoValueChanged(Sender: TObject);
begin
  if Assigned(FOnValueChange) then
    FOnValueChange(Self);
  Invalidate;
end;

procedure TProgressView.PaintBackground;

  procedure DoDrawHorizontal;
  var
    R: TRectF;
    W: Single;
  begin
    inherited PaintBackground;
    if ((FMax - FMin) <= 0) then
      Exit;
    W := (FValue - FMin) / (FMax - FMin) * Width;
    R := RectF(0, 0, W, Height);
    FForeGround.DrawTo(Canvas, R);
  end;

  procedure DoDrawVertical;
  var
    R: TRectF;
    H, V: Single;
  begin
    inherited PaintBackground;
    if ((FMax - FMin) <= 0) then
      Exit;
    V := Height;
    H := V - (FValue - FMin) / (FMax - FMin) * V;
    R := RectF(0, V, Width, H);
    FForeGround.DrawTo(Canvas, R);
  end;

  procedure DoDrawCircleRing();
  var
    W, H, SA, EA: Single;
    R: TRectF;
    LCenter, LRadius: TPointF;
    V: TBrush;
    LOpacity: Single;
    LBorder: TViewBorder;
  begin
    W := Width;
    H := Height;
    LOpacity := GetAbsoluteOpacity;

    // 画背景
    if Assigned(FBackground) then begin
      R := RectF(FBackground.Padding.Left, FBackground.Padding.Top,
        W - FBackground.Padding.Right, H - FBackground.Padding.Bottom);
      LCenter := PointF(R.Width / 2 + R.Left, R.Height / 2 + R.Top);
      if R.Width > R.Height then
        LRadius := PointF(R.Height / 2, R.Height / 2)
      else
        LRadius := PointF(R.Width / 2, R.Width / 2);
      V := FBackground.GetStateItem(DrawState);
      if V <> nil then
        Canvas.FillArc(LCenter, LRadius, 0, 360, LOpacity, V);

      LBorder := TDrawableBorder(FBackground)._Border;
      if Assigned(LBorder) and (LBorder.Style = TViewBorderStyle.RectBorder) and (LBorder.Width > 0) then begin
        LBorder.Brush.Color :=  LBorder.Color.GetStateColor(DrawState);
        if LBorder.Width > 0.1 then begin
          LRadius.X := LRadius.X - LBorder.Width * 0.5;
          LRadius.Y := LRadius.X;
        end;
        Canvas.DrawArc(LCenter, LRadius, 0, 360, LOpacity, LBorder.Brush);
      end;
    end;

    // 画前景
    if Assigned(FForeGround) then begin
      SA := FStartAngle;
      EA := SA + (FValue - FMin) / (FMax - FMin) * 360;

      R := RectF(FForeGround.Padding.Left, FForeGround.Padding.Top,
        W - FForeGround.Padding.Right, H - FForeGround.Padding.Bottom);

      LCenter := PointF(R.Width / 2 + R.Left, R.Height / 2 + R.Top);
      if R.Width > R.Height then
        LRadius := PointF(R.Height / 2, R.Height / 2)
      else
        LRadius := PointF(R.Width / 2, R.Width / 2);

      if not Assigned(FShapePath) then
        FShapePath := TPathData.Create
      else
        FShapePath.Clear;
      FShapePath.MoveTo(LCenter);
      FShapePath.AddArc(LCenter, LRadius, SA, EA - SA);
      FShapePath.MoveTo(LCenter);

      V := FForeGround.GetStateItem(DrawState);
      if V <> nil then
        Canvas.FillPath(FShapePath, LOpacity, V);

      LBorder := TDrawableBorder(FForeGround)._Border;
      if Assigned(LBorder) and (LBorder.Style = TViewBorderStyle.RectBorder) and (LBorder.Width > 0) then begin
        LBorder.Brush.Color :=  LBorder.Color.GetStateColor(DrawState);

        if LBorder.Width > 0.1 then begin
          LRadius.X := LRadius.X - LBorder.Width * 0.5;
          LRadius.Y := LRadius.X;
        end;

        FShapePath.Clear;
        FShapePath.AddArc(LCenter, LRadius, SA, EA - SA);
        Canvas.DrawPath(FShapePath, LOpacity, LBorder.Brush);
      end;
    end;
  end;

begin
  if AbsoluteInVisible then
    Exit;

  case FKind of
    Horizontal: // 水平
      begin
        DoDrawHorizontal();
      end;
    Vertical: // 垂直
      begin
        DoDrawVertical();
      end;
    CircleRing: // 圆环
      begin
        DoDrawCircleRing();
      end;
  end;
end;

procedure TProgressView.SetForeGround(const Value: TDrawable);
begin
  FForeGround.SetDrawable(Value);
end;

procedure TProgressView.SetKind(const Value: TProgressKind);
begin
  if FKind <> Value then begin
    FKind := Value;
    FreeAndNil(FShapePath);
    Repaint;
  end;
end;

procedure TProgressView.SetMaxValue(const Value: Int64);
begin
  if FMax <> Value then begin
    FMax := Value;
    DoValueChanged(Self);
  end;
end;

procedure TProgressView.SetMinValue(const Value: Int64);
begin
  if FMin <> Value then begin
    FMin := Value;
    DoValueChanged(Self);
  end;
end;

procedure TProgressView.SetProValue(const Value: Int64);
begin
  if FValue <> Value then begin
    FValue := Value;
    DoValueChanged(Self);
  end;
end;

procedure TProgressView.SetStartAngle(const Value: Single);
begin
  if FStartAngle <> Value then begin
    FStartAngle := Value;
    DoValueChanged(Self);
  end;
end;

initialization

finalization

end.
