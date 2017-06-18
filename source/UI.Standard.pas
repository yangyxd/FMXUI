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
  UI.Base, UI.Utils, UI.Ani,
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
  TPathDataHelper = class helper for TPathData
    procedure AddRing(const ACenter: TPointF;
      const R1, R2, AStartAngle, ASweepAngle: Single);
  end;

type
  TRingViewStyle = (Rectangle {矩形}, Circle {圆形}, Ellipse {椭圆});

type
  /// <summary>
  /// 空心图形视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TRingView = class(TView)
  private
    FOuter: TRingViewStyle;
    FInner: TRingViewStyle;
    FDistance: Single;
    FStartAngle: Single;
    FAngle: Single;
    FCenter: TPointF;
    FRadius: Single;
    FPathChanged: Boolean;
    FClickInPath: Boolean;
    procedure SetDistance(const Value: Single);
    procedure SetInner(const Value: TRingViewStyle);
    procedure SetOuter(const Value: TRingViewStyle);
    function IsStoredDistance: Boolean;
    procedure SetEndAngle(const Value: Single);
    procedure SetStartAngle(const Value: Single);
  protected
    FPath: TPathData;
    procedure Resize; override;
    procedure DoBackgroundChanged(Sender: TObject); override;
    procedure PaintBackground; override;
    procedure RecreatePath; virtual;
    procedure PathChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PointInObject(X, Y: Single): Boolean; override;
  published
    /// <summary>
    /// 外层样式
    /// </summary>
    property KindOuter: TRingViewStyle read FOuter write SetOuter default TRingViewStyle.Rectangle;
    /// <summary>
    /// 内层样式
    /// </summary>
    property KindInner: TRingViewStyle read FInner write SetInner default TRingViewStyle.Ellipse;
    /// <summary>
    /// 内层与外层之间的距离
    /// </summary>
    property Distance: Single read FDistance write SetDistance stored IsStoredDistance;
    /// <summary>
    /// 开始角度，当 Style 不是 Rectangle 时有效
    /// </summary>
    property AngleStart: Single read FStartAngle write SetStartAngle;
    /// <summary>
    /// 角度，当 Style 不是 Rectangle 时有效
    /// </summary>
    property AngleEnd: Single read FAngle write SetEndAngle;
    /// <summary>
    /// 是否是能点击到路径上
    /// </summary>
    property ClickInPath: Boolean read FClickInPath write FClickInPath default True;
  end;

type
  /// <summary>
  /// 进度视图类型
  /// </summary>
  TProgressKind = (Horizontal {水平}, Vertical {垂直}, CircleRing {圆环});

type
  TDrawableProgress = class(TDrawable)
  private
    FWidth: Single;
    function IsStoredWidth: Boolean;
    procedure SetWidth(const Value: Single);
  protected
    procedure InitDrawable; override;
  published
    property RingWidth: Single read FWidth write SetWidth stored IsStoredWidth;
  end;

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
    FForeGround: TDrawableProgress;
    FShapePath: TPathData;
    FPaddingBorder: Boolean;
    FPathChanged: Boolean;
    procedure SetForeGround(const Value: TDrawableProgress);
    procedure SetMaxValue(const Value: Int64);
    procedure SetMinValue(const Value: Int64);
    procedure SetProValue(const Value: Int64);
    procedure SetKind(const Value: TProgressKind);
    procedure SetStartAngle(const Value: Single);
    procedure SetPaddingBorder(const Value: Boolean);
  protected
    procedure DoForegroundChanged(Sender: TObject); virtual;
    procedure DoValueChanged(Sender: TObject); virtual;
  protected
    procedure Resize; override;
    procedure PaintBackground; override;
    procedure RecreatePath; virtual;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
    function DoGetUpdateRect: TRectF; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    /// <summary>
    /// 最小值
    /// </summary>
    property Min: Int64 read FMin write SetMinValue default 0;
    /// <summary>
    /// 最大值
    /// </summary>
    property Max: Int64 read FMax write SetMaxValue default 100;
    /// <summary>
    /// 当前值
    /// </summary>
    property Value: Int64 read FValue write SetProValue default 50;
    /// <summary>
    /// 前景层样式 (显示值的绘制)
    /// </summary>
    property ForeGround: TDrawableProgress read FForeGround write SetForeGround;
    /// <summary>
    /// 开始角度
    /// </summary>
    property StartAngle: Single read FStartAngle write SetStartAngle;
    /// <summary>
    /// 进度条类型
    /// </summary>
    property Kind: TProgressKind read FKind write SetKind default TProgressKind.Horizontal;
    /// <summary>
    /// 前存在边框时，空出边框位置
    /// </summary>
    property PaddingBorder: Boolean read FPaddingBorder write SetPaddingBorder default False;
    /// <summary>
    /// 当前值变更事件
    /// </summary>
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  end;

type
  TImageScaleType = (
    None, {无}
    Matrix, {矩阵}
    Center, {在视图中心显示图片，并且不缩放图片}
    CenterCrop, {按比例缩放图片，使得图片长 (宽)的大于等于视图的相应维度}
    CenterInside, {按比例缩放图片，使得图片长 (宽)的小于等于视图的相应维度}
    FitCenter, {按比例缩放图片到视图的最小边，居中显示}
    FitStart, { 把图片按比例扩大/缩小到视图的最小边，显示在视图的上部分位置}
    FitEnd  {按比例缩放图片到视图的最小边，显示在视图的下部分位置}
  );

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TImageView = class(TView)
  private
    FScaleType: TImageScaleType;
    FImage: TDrawable;
    procedure SetScaleType(const Value: TImageScaleType);
    function GetImage: TDrawable;
    procedure SetImage(const Value: TDrawable);
  protected
    procedure PaintBackground; override;
    procedure DoDrawImage(); virtual;
    procedure CreateImage; virtual;
    procedure DoImageChange(Sender: TObject); virtual;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    /// <summary>
    /// 可绘制图像
    /// </summary>
    property Image: TDrawable read GetImage write SetImage;
    /// <summary>
    /// 缩放类型
    /// </summary>
    property ScaleType: TImageScaleType read FScaleType write SetScaleType default TImageScaleType.None;
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
    FCanAnimation: Boolean;
    FInInternalAlign: Boolean;
    FCachedAutoShowing: Boolean;
    FDragScroll: Boolean;
    FScrollbarWidth: Single;
    function GetViewportPosition: TPointD;
    procedure SetViewportPosition(const Value: TPointD);
    procedure SetShowScrollBars(const Value: Boolean);
    function GetScrollValueV: Single;
    function GetScrollValueH: Single;
    function IsStoredScrollStretchGlowColor: Boolean; virtual;
    procedure SetScrollSmallChangeFraction(const Value: Single);
    procedure SetDragScroll(const Value: Boolean);
    function GetHScrollBarValue: Double;
    function GetVScrollBarValue: Double;
    procedure SetHScrollBarValue(const Value: Double);
    procedure SetVScrollBarValue(const Value: Double);
    function IsStoredScrollbarWidth: Boolean;
    procedure SetScrollbarWidth(const Value: Single);
  protected
    FCanScrollV: Boolean;
    FCanScrollH: Boolean;
    FShowScrollBars: Boolean;
    FScrolling: Boolean;
    FSystemInfoSrv: IFMXSystemInformationService;
    FListingService: IFMXListingService;
    //Animation mouse events
    procedure AniMouseDown(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseMove(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseUp(const Touch: Boolean; const X, Y: Single); virtual;

    procedure AniCalcChange(Sender: TObject);

    procedure CMGesture(var EventInfo: TGestureEventInfo); override;

    function CanAnimation: Boolean; override;

    function IsRunningOnDesktop: Boolean;
    function HasTouchTracking: Boolean;
    function HasStretchyScrolling: Boolean;
    function HasScrollingStretchGlow: Boolean;
    function HasPhysicsStretchyScrolling: Boolean;
    function GetMaxScrollViewPos: Integer;
    procedure ScrollStretchChanged; virtual;
    procedure UpdateScrollStretchStrength(const NewValue: Single);

  protected
    FScrollV: TScrollBar;
    FScrollH: TScrollBar;
    FContentBounds: PRectD;
    FAniCalculations: TScrollCalculations;
    FLastViewportPosition: TPointD;
    FMouseEvents: Boolean;
    FScrollStretchStrength: Single;
    FScrollTrackPressed: Boolean;
    FScrollingStretchGlowColor: TAlphaColor;
    FScrollSmallChangeFraction: Single;

    procedure Resize; override;
    procedure DoRealign; override;
    procedure DoInVisibleChange; override;
    function IsOpaque: Boolean; virtual;

    function AllowInitScrollbar: Boolean; virtual;

    function CreateScroll: TScrollBar; virtual;
    procedure MakScrollBar(SType: TViewScroll; var Obj: TScrollBar);
    procedure UpdateScrollWidth(const AScroll: TScrollBar); virtual;
    procedure InitScrollbar; override;
    procedure FreeScrollbar; override;
    procedure HScrollChange(Sender: TObject); virtual;
    procedure VScrollChange(Sender: TObject); virtual;
    function GetVScrollBar: TScrollBar; override;
    function GetHScrollBar: TScrollBar; override;
    function GetContentBounds: TRectD; override;
    function GetScrollSmallChangeFraction: Single;  override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure InvalidateContentSize(); virtual;
    procedure RealignContent; virtual;
    procedure InternalAlign; override;

    procedure AniVScrollTo(const AOffset: Single; AFinish: TNotifyEventA = nil);

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
    procedure DoUpdateScrollingLimits(NeedUpdateScroll: Boolean = False; const ValueOffset: Double = 0); virtual;
    procedure UpdateScrollBar(AScroll: TScrollBar; AScrollBar: TViewScroll; const ValueOffset: Double = 0); virtual;

    function IsStoredScrollSmallChangeFraction: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollBy(const Dx, Dy: Double);
    procedure ScrollTo(const Dx, Dy: Double);
    property VScrollBarValue: Double read GetVScrollBarValue write SetVScrollBarValue;
    property HScrollBarValue: Double read GetHScrollBarValue write SetHScrollBarValue;
    // 获取滚动条所在位置的百分比
    property ScrollValueV: Single read GetScrollValueV;
    // 获取滚动条所在位置的百分比
    property ScrollValueH: Single read GetScrollValueH;
    // 是否可以垂直滚动
    property CanVScroll: Boolean read FCanScrollV;
    // 是否可以水平滚动
    property CanHScroll: Boolean read FCanScrollH;
    /// <summary>
    /// 是否启用鼠标拖动滚动功能 （在非移动平台上设置有效）
    /// </summary>
    property DragScroll: Boolean read FDragScroll write SetDragScroll default False;
    // 是否显示滚动条
    property ShowScrollBars: Boolean read FShowScrollBars write SetShowScrollBars default True;
    // 视口位置
    property ViewportPosition: TPointD read GetViewportPosition write SetViewportPosition;
    // 滚动条宽度
    property ScrollbarWidth: Single read FScrollbarWidth write SetScrollbarWidth stored IsStoredScrollbarWidth;
    // 滚动伸展区颜色
    property ScrollStretchGlowColor: TAlphaColor read FScrollingStretchGlowColor write FScrollingStretchGlowColor stored IsStoredScrollStretchGlowColor;
    // 滚动条最小改变值
    property ScrollSmallChangeFraction: Single read GetScrollSmallChangeFraction write SetScrollSmallChangeFraction stored IsStoredScrollSmallChangeFraction;
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
    FInFitSize: Boolean;
    FGroupIndex: Integer;

    function GetAutoSize: Boolean;
    function GetText: string;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetTextSettings(const Value: UI.Base.TTextSettings);
    function GetDrawable: TDrawableIcon;
    procedure SetDrawable(const Value: TDrawableIcon);
    procedure SetTextHint(const Value: string);
    function GetTextLength: Integer;
    procedure SetGroupIndex(const Value: Integer);
    function GetNeedSize: TSizeF;
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
    function GetDrawableWidth(): Integer;
    function GetDrawableHeight(): Integer;
    function GetDefaultSize: TSizeF; override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure SetName(const Value: TComponentName); override;
    procedure DoGroupSelected(); virtual;
    procedure DoCheckedChange(); override;
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
    property NeedSize: TSizeF read GetNeedSize;
  published
    property Gravity stored False;
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    property EnableExecuteAction default True;
    property Hint;
    property ShowHint;
    property Text: string read GetText write SetText stored TextStored;
    property TextHint: string read FTextHint write SetTextHint;
    property TextSettings: UI.Base.TTextSettings read FText write SetTextSettings;
    property Drawable: TDrawableIcon read GetDrawable write SetDrawable;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ScrollBars;
    property DisableMouseWheel;
    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property OnDrawText: TOnDrawText read FOnDrawText write FOnDrawText;
  end;

type
  TStyleView = class(TTextView)
  protected
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

type
  TSimpleTextSettings = class(TTextSettingsBase)
  private
    FColor: TAlphaColor;
    FColorChange: Boolean;
    procedure SetColor(const Value: TAlphaColor);
    function IsColorStored: Boolean;
  protected
    function GetStateColor(const State: TViewState): TAlphaColor; override;
  public
    constructor Create(AOwner: TComponent);
    property ColorChange: Boolean read FColorChange write FColorChange;
  published
    property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
    property Font;
    property PrefixStyle;
    property Trimming;
    property Gravity default TLayoutGravity.Center;
  end;

  TBadgeBackground = class(TPersistent)
  private
    FColor: TAlphaColor;
    FXRadius, FYRadius: Single;
    FCorners: TCorners;
    FOnChanged: TNotifyEvent;
    function IsStoredCorners: Boolean;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetCorners(const Value: TCorners);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
  protected
  public
    constructor Create();
    procedure Assign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Color: TAlphaColor read FColor write SetColor default TAlphaColorRec.Red;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property Corners: TCorners read FCorners write SetCorners stored IsStoredCorners;
  end;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TBadgeView = class(TControl, IViewBadge)
  private
    [Weak] FTargetView: IView;
    FMaxValue: Integer;
    FValue: Integer;
    FBackground: TBadgeBackground;
    FText: TSimpleTextSettings;
    FStyle: TBadgeStyle;
    FIcon: TBrush;
    FAutoSize: Boolean;
    FAdjustSizeing: Boolean;
    FValueOutTail: string;
    procedure SetValue(const Value: Integer);
    procedure SetMaxValue(const Value: Integer);
    procedure SetTargetView(const Value: IView);
    procedure SetBackground(const Value: TBadgeBackground);
    function GetText: string;
    procedure SetTextSettings(const Value: TSimpleTextSettings);
    function IsVisibleView: Boolean;
    procedure SetStyle(const Value: TBadgeStyle);
    procedure SetValueOutTail(const Value: string);
    function GetIcon: TBrush;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetIcon(const Value: TBrush);
    function GetValue: Integer;
    function GetMaxValue: Integer;
    function GetStyle: TBadgeStyle;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DoRealign; override;
    procedure DoAdjustSize; virtual;
    function GetDefaultSize: TSizeF; override;
    function GetFirstParent: TFmxObject;
    procedure DoChanged(Sender: TObject); virtual;
    procedure DoTextChanged(Sender: TObject); virtual;
    procedure DoMatrixChanged(Sender: TObject); override;
    procedure AncestorParentChanged; override;
    procedure PaddingChanged; override;
    function GetViewText: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    property Text: string read GetText;
  published
    /// <summary>
    /// 是否自动大小
    /// </summary>
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    /// <summary>
    /// 目标View
    /// </summary>
    property TargetView: IView read FTargetView write SetTargetView;
    /// <summary>
    /// 效果样式
    /// </summary>
    property Style: TBadgeStyle read FStyle write SetStyle default TBadgeStyle.NumberText;
    /// <summary>
    /// 当 Style 为 NumberText 时，显示 Value 的最大值
    /// </summary>
    property MaxValue: Integer read FMaxValue write SetMaxValue default 99;
    /// <summary>
    /// 当 Style 为 NumberText 时，要显示的数值
    /// </summary>
    property Value: Integer read FValue write SetValue default 0;
    /// <summary>
    /// 当 Style 为 NumberText 时，数值大于MaxValue时，要在属部加入的内容，如 "+"
    /// </summary>
    property ValueOutTail: string read FValueOutTail write SetValueOutTail;
    /// <summary>
    /// 背景颜色
    /// </summary>
    property Background: TBadgeBackground read FBackground write SetBackground;
    /// <summary>
    /// 字体设置
    /// </summary>
    property TextSettings: TSimpleTextSettings read FText write SetTextSettings;
    /// <summary>
    /// 当 Style 为 Icon 时，要显示的图标
    /// </summary>
    property Icon: TBrush read GetIcon write SetIcon;

    property Width;
    property Height;
    property Scale;
    property Size;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Margins;
    property Padding;
    property Visible;
    property OnPaint;
  end;

type
  TPathViewStyle = class(TPersistent)
  private
    FFill: TBrush;
    FStroke: TStrokeBrush;
    FOnChange: TNotifyEvent;
    function GetFill: TBrush;
    function GetStroke: TStrokeBrush;
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
  protected
    procedure DoChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsEmpty: Boolean;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    /// <summary>
    /// 填充刷子
    /// </summary>
    property Fill: TBrush read GetFill write SetFill;
    /// <summary>
    /// 边框画笔
    /// </summary>
    property Border: TStrokeBrush read GetStroke write SetStroke;
  end;

  TPathViewStyles = class(TPersistent)
  private
    FItemDefault: TPathViewStyle;
    FItemActive: TPathViewStyle;
    FItemHover: TPathViewStyle;
    FItemPressed: TPathViewStyle;
    FOnChanged: TNotifyEvent;
    function GetActiveStyle: TPathViewStyle;
    function GetHoverStyle: TPathViewStyle;
    function GetPressedStyle: TPathViewStyle;
    procedure SetActiveStyle(const Value: TPathViewStyle);
    procedure SetHoverStyle(const Value: TPathViewStyle);
    procedure SetPressedStyle(const Value: TPathViewStyle);
    function GetDefaultStyle: TPathViewStyle;
    procedure SetDefaultStyle(const Value: TPathViewStyle);
  protected
    procedure DoChanged(Sender: TObject);
    procedure CreateStyle(var Value: TPathViewStyle);
    procedure UpdateStyle(var Source: TPathViewStyle; const Value: TPathViewStyle);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChanged write FOnChanged;
  published
    /// <summary>
    /// 激活时
    /// </summary>
    property ItemDefault: TPathViewStyle read GetDefaultStyle write SetDefaultStyle;
    /// <summary>
    /// 激活时
    /// </summary>
    property ItemActivated: TPathViewStyle read GetActiveStyle write SetActiveStyle;
    /// <summary>
    /// 鼠标悬停时
    /// </summary>
    property ItemHovered: TPathViewStyle read GetHoverStyle write SetHoverStyle;
    /// <summary>
    /// 按下时
    /// </summary>
    property ItemPressed: TPathViewStyle read GetPressedStyle write SetPressedStyle;
  end;

  /// <summary>
  /// 单个路径信息
  /// </summary>
  TPathViewItem = class(TCollectionItem)
  private
    [Weak] FOwner: TControl;
    FOnChange: TNotifyEvent;
    FPath: TPathData;
    FStyle: TPathViewStyles;
    FVisible: Boolean;
    FScaleX: Single;
    FScaleY: Single;
    FWidth: Single;
    FHeight: Single;
    FGravity: TLayoutGravity;
    FDisplayName: string;
    FPathData: string;
    function GetPath: TPathData;
    procedure SetPath(const Value: TPathData);
    procedure SetVisible(const Value: Boolean);
    function GetStyle: TPathViewStyles;
    procedure SetStyle(const Value: TPathViewStyles);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);
    function IsStoreScaleX: Boolean;
    function IsStoreScaleY: Boolean;
    procedure SetPathData(const Value: string);
    procedure SetGravity(const Value: TLayoutGravity);
    procedure SetOwner(const Value: TControl);
  protected
    procedure DoChanged(Sender: TObject);
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    procedure UpdateSize();
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure ApplayScale;
    /// <summary>
    /// 路径
    /// </summary>
    property Path: TPathData read GetPath write SetPath;
    property Owner: TControl read FOwner write SetOwner;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property PathData: string read FPathData write SetPathData;
    /// <summary>
    /// 是否可视
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>
    /// 样式
    /// </summary>
    property Style: TPathViewStyles read GetStyle write SetStyle;
    /// <summary>
    /// 缩放
    /// </summary>
    property ScaleX: Single read FScaleX write SetScaleX stored IsStoreScaleX;
    property ScaleY: Single read FScaleY write SetScaleY stored IsStoreScaleY;

    property SizeWidth: Single read FWidth;
    property SizeHeight: Single read FHeight;
    /// <summary>
    /// 位置
    /// </summary>
    property Gravity: TLayoutGravity read FGravity write SetGravity default TLayoutGravity.None;
  end;

  /// <summary>
  /// 路径集合
  /// </summary>
  TPathViewCollection = class(TCollection)
  protected
    [Weak] FOwner: TControl;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TPathViewItem;
    procedure SetItem(Index: Integer; const Value: TPathViewItem);
    procedure DoItemChanged(ASender: TObject);
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
    procedure DoChange(Sender: TObject);
    procedure DoParentSizeChange();
  public
    constructor Create(AOwner: TControl; ItemClass: TCollectionItemClass);
    function Add: TPathViewItem; reintroduce;
    function FindItemID(ID: Integer): TPathViewItem; reintroduce;
    property Items[Index: Integer]: TPathViewItem read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TOnPathViewItemClick = procedure (Sender: TObject; Index: Integer) of object;

  TCustomMultiPathView = class(TView)
  private
    FActiveIndex: Integer;
    FHoverIndex: Integer;
    FPressedIndex: Integer;
    FClickInPath: Boolean;
    FOnItemHover: TNotifyEvent;
    FOnItemClick: TOnPathViewItemClick;
    procedure SetPaths(const Value: TPathViewCollection);
    procedure SetActiveIndex(const Value: Integer);
    function GetPathCount: Integer;
  protected
    FPaths: TPathViewCollection;
    procedure PaintBackground; override;
    procedure Click; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
    procedure DoPathChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PointInObject(X, Y: Single): Boolean; override;

    /// <summary>查找指定位置对应的路径项目</summary>
    /// <param name="LocalPoint">要查找的路径内的一个点坐标</param>
    /// <returns>返回找到的路径，如果没找到，返回空</returns>
    function ItemAt(const LocalPoint: TPointF): TPathViewItem;
    /// <summary>查找指定位置对应的路径项目</summary>
    /// <param name="LocalPoint">要查找的路径内的一个点坐标</param>
    /// <returns>返回找到的路径的索引，如果没找到，返回-1</returns>
    function IndexAt(const LocalPoint: TPointF): Integer;

    /// <summary>
    /// 路径集合
    /// </summary>
    property Paths: TPathViewCollection read FPaths write SetPaths;
    /// <summary>
    /// 激活的路径索引号
    /// </summary>
    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex default -1;
    /// <summary>
    /// 当前鼠标指向的路径索引号
    /// </summary>
    property HoverIndex: Integer read FHoverIndex;
    /// <summary>
    /// 当前按下的路径索引号
    /// </summary>
    property PressedIndex: Integer read FPressedIndex;
    /// <summary>
    /// 返回 Paths 数量
    /// </summary>
    property PathCount: Integer read GetPathCount;
    /// <summary>
    /// 是否是能点击到路径上
    /// </summary>
    property ClickInPath: Boolean read FClickInPath write FClickInPath default False;

    property OnItemHover: TNotifyEvent read FOnItemHover write FOnItemHover;
    property OnItemClick: TOnPathViewItemClick read FOnItemClick write FOnItemClick;
  end;

type
  /// <summary>
  /// 多重路径可视组件
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TMultiPathView = class(TCustomMultiPathView)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ActiveIndex;
    property Paths;
    property ClickInPath;
    property OnItemHover;
    property OnItemClick;
    property Clickable default True;
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
  EnableExecuteAction := True;
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

procedure TTextView.DoCheckedChange;
begin
  inherited;
  DoGroupSelected;
end;

procedure TTextView.DoDrawableChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTextView.DoDrawBackground(var R: TRectF);
begin
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

procedure TTextView.DoGroupSelected;
var
  I: Integer;
  Control: TControl;
  Item: TFmxObject;
begin
  if (FGroupIndex <> 0) and (Checked) then begin
    if Assigned(ParentControl) then begin
      for I := 0 to ParentControl.ControlsCount - 1 do begin
        Control := ParentControl.Controls[I];
        if Control = Self then Continue;
        if (Control is TTextView) and (TTextView(Control).FGroupIndex = FGroupIndex) then
          TTextView(Control).Checked := False;
      end;
    end else if Parent is TCommonCustomForm then begin
      for I := 0 to TCommonCustomForm(Parent).ChildrenCount - 1 do begin
        Item := TCommonCustomForm(Parent).Children.Items[I];
        if Item = Self then Continue;
        if (Item is TTextView) and (TTextView(Item).FGroupIndex = FGroupIndex) then
          TTextView(Item).Checked := False;
      end;
    end;
  end;
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
  if Assigned(FDrawable) and (not FDrawable.IsEmpty) then begin
    if TViewState.Checked in FViewState then
      FDrawable.AdjustDraw(Canvas, R, True, TViewState.Checked)
    else
      FDrawable.AdjustDraw(Canvas, R, True, DrawState);
  end;
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
    case FScrollbar of
      TViewScroll.Horizontal:
        begin
          if Assigned(FScrollH) and (FScrollH.Visible) then begin
            SR := GetRectF(FContentBounds^);
            SR.Top := R.Top;
            // windows平台显示滚动条，其它平台会自动隐藏
            SR.Bottom := R.Bottom{$IFDEF MSWINDOWS} - FScrollH.Height{$ENDIF};
            OffsetRect(SR, -(ScrollValueH * (SR.Width - R.Width)), 0);
          end else
            SR := R;
        end;
      TViewScroll.Vertical:
        begin
          if Assigned(FScrollV) and (FScrollV.Visible) then begin
            SR := GetRectF(FContentBounds^);
            SR.Left := R.Left;
            SR.Right := R.Right{$IFDEF MSWINDOWS} - FScrollV.Width{$ENDIF};
            OffsetRect(SR, 0, -(ScrollValueV * (SR.Height - R.Height)));
          end else
            SR := R;
        end;
      else
        SR := R;
    end;
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
      if (FScrollbar = TViewScroll.Vertical) and (FScrollV <> nil) and (FScrollV.Visible) then
        V := V - FScrollV.Width;
      {$ENDIF}
    end else
      V := 0;

    // 计算文本区域大小
    if not FText.CalcTextObjectSize(FText.Text, V, Scene.GetSceneScale, nil, ASize) then Exit;
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

      if (FScrollbar = TViewScroll.Horizontal) and (FScrollV <> nil) and (FScrollV.Visible) then
        VH := VH + FScrollV.Height;

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

function TTextView.GetNeedSize: TSizeF;
begin
  if not (csDestroying in ComponentState) and Assigned(Scene) then begin
    if not TextSettings.CalcTextObjectSize(TextSettings.Text, $FFFF, Scene.GetSceneScale, nil, Result) then
      Result := TSizeF.Create(0, 0)
  end else begin
    Result := TSizeF.Create(0, 0);
  end;
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

procedure TTextView.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then begin
    FGroupIndex := Value;
    DoGroupSelected();
  end;
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
  Result := Format('%s ''%s''', [inherited ToString, FText.Text]);
end;

procedure TTextView.TriggerAcceleratorKey;
begin
  SetFocus;
end;

{ TStyleView }

function TStyleView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
begin
  Result := inherited CanRePaintBk(View, State);
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
  if not Assigned(FBackground) then
    FBackground := CreateBackground;
end;

function TButtonView.CreateBackground: TDrawable;
begin
  Result := TDrawableBorder.Create(Self, TViewBrushKind.Solid, $FFF0F1F2);
  Result.ItemPressed.Color := $FFE0E0E0;
  Result.ItemPressed.DefaultColor := Result.ItemPressed.Color;
  Result.ItemPressed.Kind := TViewBrushKind.Solid;
  Result.ItemPressed.DefaultKind := TBrushKind.Solid;
  with TDrawableBorder(Result).Border do begin
    DefaultStyle := TViewBorderStyle.RectBorder;
    Style := DefaultStyle;
    Color.Default := $AFCCCCCC;
    Color.DefaultChange := False;
    Color.Pressed := $EF33ccff;
    Color.PressedChange := False;
  end;
  Result.OnChanged := DoBackgroundChanged;
end;

function TButtonView.GetDefaultSize: TSizeF;
begin
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

function TScrollView.AllowInitScrollbar: Boolean;
begin
  Result := not (csDesigning in ComponentState);
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
    FTrack := GetRttiObject(FScrollV, 'FTrack') as TCustomTrack;
    if not Assigned(FTrack) then Exit;
    FThumb := GetRttiObject(FTrack, 'FThumb') as TThumb;
    Result := Assigned(FThumb) and (FThumb.Pressed);
  end;

begin
  if Assigned(FScrollV) then
    FScrollTrackPressed := FScrollV.Pressed or GetScrollPressed
  else
    FScrollTrackPressed := False;
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

procedure TScrollView.AniVScrollTo(const AOffset: Single;
  AFinish: TNotifyEventA);
begin
  if (AOffset = 0) or (not Assigned(FAniCalculations)) then
    Exit;
  TFrameAnimator.AnimateFloat(Self, 'VScrollBarValue', VScrollBarValue - AOffset, AFinish);
  //TFrameAnimator.AnimateFloat(FAniCalculations, ViewportPosition.Y);
end;

function TScrollView.CanAnimation: Boolean;
begin
  Result := FCanAnimation;
end;

procedure TScrollView.CMGesture(var EventInfo: TGestureEventInfo);
var
  LP: TPointF;
begin
  if (FCanScrollV or FCanScrollH) and ((EventInfo.GestureID = igiPan)) then
  begin
    if FInVisible or (FAniCalculations = nil) then
      Exit;
    FMouseEvents := False;
    LP := AbsoluteToLocal(EventInfo.Location);
    if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then begin
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
  FScrollbarWidth := 0;
  FContentBounds := nil;
  FShowScrollBars := True;
  FDragScroll := False;
  FScrollSmallChangeFraction := TView.SmallChangeFraction;
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
  if Assigned(FScrollV) then begin
    if FInVisible then
      V := False
    else
      V := FCanScrollV and FShowScrollBars;
    if FScrollV.Visible <> V then
      FScrollV.Visible := V;
  end;
end;

procedure TScrollView.DoMouseLeave;
begin
  inherited DoMouseLeave;
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
  LDisableInternalAlign := (not FCanScrollV) or FDisableAlign or
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
  if FDragScroll then begin
    FCanAnimation := True;
    AAniCalculations.Animation := FCanAnimation;
    AAniCalculations.TouchTracking := [ttVertical, ttHorizontal];
  end else begin
    FCanAnimation := (TScrollingBehaviour.Animation in GetScrollingBehaviours);
    AAniCalculations.Animation := FCanAnimation;
    if TScrollingBehaviour.TouchTracking in GetScrollingBehaviours then
      AAniCalculations.TouchTracking := [ttVertical, ttHorizontal]
    else
      AAniCalculations.TouchTracking := [];
  end;
  AAniCalculations.BoundsAnimation := True; //TScrollingBehaviour.BoundsAnimation in GetScrollingBehaviours;
  AAniCalculations.AutoShowing := TScrollingBehaviour.AutoShowing in GetScrollingBehaviours;
  if FScrollbar = TViewScroll.Vertical then
    AAniCalculations.TouchTracking := AAniCalculations.TouchTracking - [ttHorizontal]
  else if FScrollbar = TViewScroll.Horizontal then
    AAniCalculations.TouchTracking := AAniCalculations.TouchTracking - [ttVertical];
end;

procedure TScrollView.DoUpdateScrollingLimits(NeedUpdateScroll: Boolean; const ValueOffset: Double);

  function GetScrollBar(): TScrollBar;
  begin
    case ScrollBars of
      TViewScroll.Horizontal: Result := HScrollBar;
      TViewScroll.Vertical: Result := VScrollBar;
    else
      Result := nil;
    end;
  end;

var
  Targets: array [0..1] of TAniCalculations.TTarget;
  LScroll: TScrollBar;
  {$IFNDEF NEXTGEN}
  FTrackChanging: Boolean;
  {$ENDIF}
begin
  if FAniCalculations <> nil then
  begin
    Targets[0].TargetType := TAniCalculations.TTargetType.Min;
    Targets[0].Point := TPointD.Create(0, 0);
    Targets[1].TargetType := TAniCalculations.TTargetType.Max;
    case FScrollbar of
      TViewScroll.None:
        Targets[1].Point := TPointD.Create(0, 0);
      TViewScroll.Horizontal:
        Targets[1].Point := TPointD.Create(Padding.Left + Max(FContentBounds.Width - ViewRect.Width, 0), 0);
      TViewScroll.Vertical:
        Targets[1].Point := TPointD.Create(0, Padding.Top + Max(FContentBounds.Height - ViewRect.Height, 0));
    else
      Targets[1].Point := TPointD.Create(
        Padding.Left + Max(FContentBounds.Width - ViewRect.Width, 0),
        Padding.Top + Max(FContentBounds.Height - ViewRect.Height, 0)
      );
    end;

    FAniCalculations.SetTargets(Targets);

    if NeedUpdateScroll or (not HasTouchTracking) then begin
      LScroll := GetScrollBar;
      if not Assigned(LScroll) then
        Exit;
      {$IFNDEF NEXTGEN}
      // 非移动平台，在拖动滚动条后，动态更新滚动条的值
      FTrackChanging := GetRttiValue<Boolean>(LScroll, 'FTrackChanging');
      SetRttiValue<Boolean>(LScroll, 'FTrackChanging', False); // 临时将此变量设为False，否则为忽略本次调整
      try
        UpdateScrollBar(LScroll, FScrollbar, ValueOffset);
      finally
        SetRttiValue<Boolean>(LScroll, 'FTrackChanging', FTrackChanging);
      end;
      {$ELSE}
      UpdateScrollBar(LScroll, FScrollbar, ValueOffset);
      {$ENDIF}
    end;
  end;
end;

procedure TScrollView.FreeScrollbar;
begin
  AutoCapture := False;
  if Assigned(FScrollV) then begin
    RemoveComponent(FScrollV);
    FreeAndNil(FScrollV);
  end;
  if Assigned(FScrollH) then begin
    RemoveComponent(FScrollH);
    FreeAndNil(FScrollH);
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
  Result := FScrollH;
end;

function TScrollView.GetHScrollBarValue: Double;
begin
  if (FAniCalculations <> nil) and Assigned(FScrollH) and (FCanScrollH) then
    Result := ViewportPosition.X
  else
    Result := 0;
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

function TScrollView.GetScrollSmallChangeFraction: Single;
begin
  Result := FScrollSmallChangeFraction;
end;

function TScrollView.GetScrollValueH: Single;
begin
  Result := (FScrollH.ValueD - FScrollH.MinD) / (FScrollH.MaxD - FScrollH.MinD - FScrollH.ViewportSizeD);
end;

function TScrollView.GetScrollValueV: Single;
begin
  Result := (FScrollV.ValueD - FScrollV.MinD) / (FScrollV.MaxD - FScrollV.MinD - FScrollV.ViewportSizeD);
end;

function TScrollView.GetViewportPosition: TPointD;
var
  LScale: Double;
begin
  LScale := GetSceneScale;
  if LScale = 1 then begin
    Result.X := FAniCalculations.ViewportPosition.X;
    Result.Y := FAniCalculations.ViewportPosition.Y;
  end else begin
    Result.X := Round(FAniCalculations.ViewportPosition.X * LScale) / LScale;
    Result.Y := Round(FAniCalculations.ViewportPosition.Y * LScale) / LScale;
  end;
end;

function TScrollView.GetVScrollBar: TScrollBar;
begin
  Result := FScrollV;
end;

function TScrollView.GetVScrollBarValue: Double;
begin
  if (FAniCalculations <> nil) and Assigned(FScrollV) and (FCanScrollV) then begin
    Result := ViewportPosition.Y; // / (FScroll.Max - FScroll.ViewportSize) * FScroll.Max
  end else
    Result := 0;
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

procedure TScrollView.HScrollChange(Sender: TObject);
begin
  if FScrolling or (FAniCalculations = nil) then Exit;
  FScrolling := True;
  ViewportPosition := PointF(HScrollBar.ValueD, ViewportPosition.Y);
  if not IsOpaque then
    UpdateEffects;
  if Assigned(FOnScrollChange) then
    FOnScrollChange(self);
  FAniCalculations.Shown := True;
  FScrolling := False;
end;

procedure TScrollView.InitScrollbar;
begin
  if not AllowInitScrollbar() then
    Exit;
  if (FContentBounds = nil) and (FScrollbar <> TViewScroll.None) then begin
    New(FContentBounds);
    FContentBounds^ := TRectD.Empty;
  end;
  case FScrollbar of
    TViewScroll.Vertical: MakScrollBar(FScrollbar, FScrollV);
    TViewScroll.Horizontal: MakScrollBar(FScrollbar, FScrollH);
    TViewScroll.Both:
      begin
        MakScrollBar(TViewScroll.Vertical, FScrollV);
        MakScrollBar(TViewScroll.Horizontal, FScrollH);
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
      if not Released then begin
        if (FCachedAutoShowing <> FAniCalculations.AutoShowing) and not FAniCalculations.AutoShowing then
          InvalidateContentSize;
        FCachedAutoShowing := FAniCalculations.AutoShowing;
      end;
      if (not FAniCalculations.Down) and FAniCalculations.LowVelocity then
        FAniCalculations.Shown := False;
      Adjust(ContentLayoutRect);
      if Assigned(FScrollV) and (FScrollV.Visible) then
        FScrollV.Opacity := AniCalculations.Opacity{$IFNDEF MSWINDOWS} - 0.1{$ENDIF};
      if Assigned(FScrollH) and (FScrollH.Visible) then
        FScrollH.Opacity := AniCalculations.Opacity{$IFNDEF MSWINDOWS} - 0.1{$ENDIF};
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

function TScrollView.IsStoredScrollbarWidth: Boolean;
begin
  Result := FScrollbarWidth > 0;
end;

function TScrollView.IsStoredScrollSmallChangeFraction: Boolean;
begin
  Result := FScrollSmallChangeFraction <> TView.SmallChangeFraction;
end;

function TScrollView.IsStoredScrollStretchGlowColor: Boolean;
begin
  Result := FScrollingStretchGlowColor <> DefaultScrollingStretchGlowColor;
end;

procedure TScrollView.MakScrollBar(SType: TViewScroll; var Obj: TScrollBar);
var
  O: TOrientation;
begin
  if SType = TViewScroll.Vertical then
    O := TOrientation.Vertical
  else
    O := TOrientation.Horizontal;
  if Assigned(Obj) and (Obj.Orientation = O) then
    Exit;
  Obj := CreateScroll();
  UpdateScrollWidth(Obj);
  Obj.Orientation := O;
  Obj.Locked := True;
  if SType = TViewScroll.Vertical then begin
    Obj.Align := TAlignLayout.Right;
    Obj.OnChange := VScrollChange;
  end else begin
    Obj.Align := TAlignLayout.Bottom;
    Obj.OnChange := HScrollChange;
  end;
  Obj.SmallChange := GetScrollSmallChangeFraction;
  Obj.Parent := Self;
  Obj.Visible := False;
  Obj.Stored := False;
  Obj.Margins.Rect := TRectF.Create(0, 0, 0, 0);
end;

procedure TScrollView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if Assigned(FAniCalculations) and (Button = TMouseButton.mbLeft) and (FDragScroll) then
  begin
    AniMouseDown(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if Assigned(FAniCalculations) and FAniCalculations.Down and (FDragScroll) then
  begin
    AniMouseMove(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if Assigned(FAniCalculations) and (Button = TMouseButton.mbLeft) and (FDragScroll) then
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
  if (not (Handled or FDisableMouseWheel)) and (not FInVisible) then begin
    if (FCanScrollV and (not (ssShift in Shift))) then begin
      FAniCalculations.Shown := True;
      if VScrollBar <> nil then begin
        Offset := VScrollBar.SmallChange
      end else
        Offset := FContentBounds.Height / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      FAniCalculations.MouseWheel(0, Offset);
      Handled := True;
    end else if FCanScrollH then begin
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
    TViewScroll.None:
      begin
        FCanScrollV := False;
        FCanScrollH := False;
      end;
    {$IFDEF NEXTGEN}
    TViewScroll.Horizontal: FCanScrollH := True;
    TViewScroll.Vertical: FCanScrollV := True;
    {$ELSE}
    TViewScroll.Horizontal: FCanScrollH := DragScroll or (FContentBounds.Width > ViewRect.Width);
    TViewScroll.Vertical: FCanScrollV := DragScroll or (FContentBounds.Height > ViewRect.Height);
    {$ENDIF}
    TViewScroll.Both:
      begin
        {$IFDEF NEXTGEN}
          FCanScrollV := True;
          FCanScrollH := True;
        {$ELSE}
          FCanScrollH := DragScroll or (FContentBounds.Width > ViewRect.Width);
          FCanScrollV := DragScroll or (FContentBounds.Height > ViewRect.Height);
        {$ENDIF}
      end;
  end;
  if Assigned(FScrollV) or Assigned(FScrollH) then begin
    InvalidateContentSize;
    if Assigned(FScrollV) then
      UpdateScrollBar(FScrollV, TViewScroll.Vertical);
    if Assigned(FScrollH) then
      UpdateScrollBar(FScrollH, TViewScroll.Horizontal);
    DoUpdateScrollingLimits;
  end;
end;

procedure TScrollView.Resize;
begin
  inherited Resize;
  if (FScrollV <> nil) and (FContentBounds <> nil) then
    DoUpdateScrollingLimits;
end;

procedure TScrollView.ScrollBy(const Dx, Dy: Double);
begin
  if VScrollBar <> nil then
    VScrollBar.ValueD := VScrollBar.ValueD - Dy;
  if HScrollBar <> nil then
    HScrollBar.ValueD := HScrollBar.ValueD - Dx;
end;

procedure TScrollView.ScrollStretchChanged;
begin
  if HasScrollingStretchGlow then
    Invalidate;
end;

procedure TScrollView.ScrollTo(const Dx, Dy: Double);
begin
  if VScrollBar <> nil then
    VScrollBar.ValueD := Dy;
  if HScrollBar <> nil then
    HScrollBar.ValueD := Dx;
end;

procedure TScrollView.SetDragScroll(const Value: Boolean);
begin
  if FDragScroll <> Value then begin
    FDragScroll := Value;
    if not (csDesigning in ComponentState) then begin
      if FScrollbar = TViewScroll.None then
        FreeScrollbar
      else
        InitScrollbar;
      RealignContent;
    end;
  end;
end;

procedure TScrollView.SetHScrollBarValue(const Value: Double);
var
  V: TPointD;
begin
  if (FAniCalculations <> nil) and Assigned(FScrollH) and (FCanScrollH) then begin
    V := ViewportPosition;
    V.X := Value;
    ViewportPosition := V;
  end;
end;

procedure TScrollView.SetScrollbarWidth(const Value: Single);
begin
  if FScrollbarWidth <> Value then begin
    FScrollbarWidth := Value;
    FreeScrollbar;
    InitScrollbar;
    if Assigned(FScrollV) then
      UpdateScrollBar(FScrollV, FScrollbar);
    if Assigned(FScrollH) then
      UpdateScrollBar(FScrollH, FScrollbar);
  end;
end;

procedure TScrollView.SetScrollSmallChangeFraction(const Value: Single);
begin
  FScrollSmallChangeFraction := Value;
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

procedure TScrollView.SetVScrollBarValue(const Value: Double);
var
  V: TPointD;
begin
  if (FAniCalculations <> nil) and Assigned(FScrollV) and (FCanScrollV) then begin
    V := ViewportPosition;
    V.Y := Value;
    ViewportPosition := V;
  end;
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

procedure TScrollView.UpdateScrollBar(AScroll: TScrollBar; AScrollBar: TViewScroll; const ValueOffset: Double);
var
  LViewportPosition: TPointD;
  R: TRectF;
begin
  if not Assigned(AScroll) then Exit;
  LViewportPosition := ViewportPosition;
  R := ViewRect;
  if AScrollBar = TViewScroll.Vertical then begin
    {$IFDEF NEXTGEN}
    FCanScrollV := True; // 移动平台始终能滚动
    {$ELSE}
    FCanScrollV := FContentBounds.Height > R.Height;
    {$ENDIF}
    LViewportPosition.Y := LViewportPosition.Y + ValueOffset;
    if (LViewportPosition.Y > FContentBounds.Height - AScroll.ViewportSizeD) and
      (LViewportPosition.Y > FAniCalculations.MaxTarget.Point.Y) then
      LViewportPosition.Y := FAniCalculations.MaxTarget.Point.Y;
    UpdateVScrollBar(LViewportPosition.Y, R.Height);
    if (Assigned(AScroll)) and (AScroll.Visible <> FCanScrollV) then begin
      AScroll.Visible := FCanScrollV and FShowScrollBars and (not FInVisible);
      DoScrollVisibleChange;
    end;
  end else if AScrollBar = TViewScroll.Horizontal then begin
    {$IFDEF NEXTGEN}
    FCanScrollH := True; // 移动平台始终能滚动
    {$ELSE}
    FCanScrollH := FContentBounds.Width > R.Width;
    {$ENDIF}
    LViewportPosition.X := LViewportPosition.X + ValueOffset;
    if (LViewportPosition.X > FContentBounds.Width - AScroll.ViewportSizeD) and
      (LViewportPosition.X > FAniCalculations.MaxTarget.Point.X) then
      LViewportPosition.X := FAniCalculations.MaxTarget.Point.X;
    UpdateHScrollBar(LViewportPosition.X, R.Width);
    if (Assigned(AScroll)) and (AScroll.Visible <> FCanScrollH) then begin
      AScroll.Visible := FCanScrollH and FShowScrollBars and (not FInVisible);
      DoScrollVisibleChange;
    end;
  end else begin
    FCanScrollV := False;
    FCanScrollH := False;
    if Assigned(AScroll) then AScroll.Visible := False;
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

procedure TScrollView.UpdateScrollWidth(const AScroll: TScrollBar);
begin
  if ScrollbarWidth > 0 then begin
    AScroll.Width := ScrollbarWidth;
    AScroll.Height := ScrollbarWidth;
  end else begin
    {$IFDEF MSWINDOWS}
    if not DragScroll then begin
      AScroll.Width := 16;
      AScroll.Height := 16;
    end;
    {$ENDIF}
  end;
end;

procedure TScrollView.VScrollChange(Sender: TObject);
begin
  if FScrolling or (FAniCalculations = nil) then Exit;
  FScrolling := True;
  ViewportPosition := TPointF.Create(ViewportPosition.X, VScrollBar.ValueD);
  if not IsOpaque then
    UpdateEffects;
  if Assigned(FOnScrollChange) then
    FOnScrollChange(self);
  FAniCalculations.Shown := True;
  FScrolling := False;
end;

{ TProgressView }

function TProgressView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
begin
  Result := inherited CanRePaintBk(View, State);
  if (not Result) and Assigned(FForeGround) then
    Result := Assigned(FForeGround.GetStateBrush(State));
end;

constructor TProgressView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMax := 100;
  FValue := 50;
  FKind := TProgressKind.Horizontal;
  FStartAngle := 0;
  FPathChanged := True;

  FForeGround := TDrawableProgress.Create(Self);
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
  FPathChanged := True;
  Repaint;
end;

function TProgressView.DoGetUpdateRect: TRectF;
begin
  Result := inherited DoGetUpdateRect;
end;

procedure TProgressView.DoValueChanged(Sender: TObject);
begin
  if Assigned(FOnValueChange) then
    FOnValueChange(Self);
  FPathChanged := True;
  Invalidate;
end;

procedure TProgressView.PaintBackground;

  procedure DoDrawHorizontal;
  var
    R: TRectF;
    W, PW: Single;
  begin
    inherited PaintBackground;
    if (FMax - FMin <= 0) or (FValue - FMin <= 0) then
      Exit;
    PW := FForeGround.Padding.Left + FForeGround.Padding.Right;
    if FValue > FMax then
      W := FMax
    else if FValue < FMin then
      W := FMin
    else
      W := FValue;
    W := (W - FMin) / (FMax - FMin) * (Width - PW) + PW;
    R := RectF(0, 0, W, Height);
    FForeGround.DrawTo(Canvas, R);
  end;

  procedure DoDrawVertical;
  var
    R: TRectF;
    H, V, PH: Single;
  begin
    inherited PaintBackground;
    if (FMax - FMin <= 0) or (FValue - FMin <= 0) then
      Exit;
    PH := FForeGround.Padding.Top + FForeGround.Padding.Bottom;
    V := Height - PH;
    if FValue > FMax then
      H := FMax
    else if FValue < FMin then
      H := FMin
    else
      H := FValue;
    H := V - (H - FMin) / (FMax - FMin) * V;
    R := RectF(0, H, Width, Height);
    FForeGround.DrawTo(Canvas, R);
  end;

  procedure DoDrawCircleRing();
  var
    W, H: Single;
    R: TRectF;
    LCenter, LRadius: TPointF;
    V: TBrush;
    LOpacity: Single;
    LBorder: TViewBorder;
  begin
    if FPathChanged then begin
      FPathChanged := False;
      RecreatePath;
    end;

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
        if LBorder.Kind = TBrushKind.Solid then
          LBorder.Brush.Color :=  LBorder.Color.GetStateColor(DrawState);
        if LBorder.Width > 0.1 then begin
          LRadius.X := LRadius.X - LBorder.Width * 0.5;
          LRadius.Y := LRadius.X;
        end;
        R := RectF(LCenter.X - LRadius.X, LCenter.Y - LRadius.Y,
          LCenter.X + LRadius.X, LCenter.Y + LRadius.Y);
        Canvas.DrawEllipse(R, LOpacity, LBorder.Brush);
      end;
    end;

    // 画前景
    if Assigned(FForeGround) then begin
      V := FForeGround.GetStateItem(DrawState);
      if V <> nil then
        Canvas.FillPath(FShapePath, LOpacity, V);
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

procedure TProgressView.RecreatePath;
var
  SA, EA, LRadius: Single;
  R: TRectF;
  LCenter: TPointF;
begin
  if FShapePath = nil then
    FShapePath := TPathData.Create
  else
    FShapePath.Clear;

  SA := FStartAngle;
  EA := FValue;
  if EA < FMin then EA := FMin;
  if EA > FMax then EA := FMax;
  EA := (EA - FMin) / (FMax - FMin) * 360;

  R := RectF(FForeGround.Padding.Left, FForeGround.Padding.Top,
    Width - FForeGround.Padding.Right, Height - FForeGround.Padding.Bottom);

  LCenter := PointF(R.Width * 0.5 + R.Left, R.Height * 0.5 + R.Top);
  if R.Width > R.Height then
    LRadius := R.Height * 0.5
  else
    LRadius := R.Width * 0.5;

  if FPaddingBorder and Assigned(FBackground) and Assigned(TDrawableBorder(FBackground)._Border) then
    LRadius := LRadius - TDrawableBorder(FBackground)._Border.Width;

  FShapePath.AddRing(LCenter, LRadius, LRadius - FForeGround.RingWidth, SA, EA);
end;

procedure TProgressView.Resize;
begin
  inherited Resize;
  FPathChanged := True;
end;

procedure TProgressView.SetForeGround(const Value: TDrawableProgress);
begin
  FForeGround.SetDrawable(Value);
end;

procedure TProgressView.SetKind(const Value: TProgressKind);
begin
  if FKind <> Value then begin
    FKind := Value;
    FPathChanged := True;
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

procedure TProgressView.SetPaddingBorder(const Value: Boolean);
begin
  if FPaddingBorder <> Value then begin
    FPaddingBorder := Value;
    FPathChanged := True;
    Repaint;
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
    FPathChanged := True;
    Repaint;
  end;
end;

{ TImageView }

function TImageView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
begin
  Result := inherited CanRePaintBk(View, State);
  if (not Result) and Assigned(FImage) then
    Result := EmptyBackground(FImage, State);
end;

constructor TImageView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := nil;
end;

procedure TImageView.CreateImage;
begin
  FImage := TDrawable.Create(Self);
  FImage.OnChanged := DoImageChange;
end;

destructor TImageView.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TImageView.DoDrawImage;
var
  Img: TBrush;
  IW, IH, W, H, SW, SH: Single;
  R, VR: TRectF;
  LWrapMode: TWrapMode;
  LBitmapChange: TNotifyEvent;
begin
  Img := FImage.GetStateItem(FDrawState);
  if Img = nil then Exit;
  R := RectF(
    Padding.Left + FImage.Padding.Left,
    Padding.Top + FImage.Padding.Top,
    Width - Padding.Right - FImage.Padding.Right,
    Height - Padding.Bottom - FImage.Padding.Bottom);

  // 如果不是图像，直接缓制，不管 ScaleType
  if (Img.Kind <> TBrushKind.Bitmap) or (FScaleType = TImageScaleType.None) then begin
    FImage.DrawBrushTo(Canvas, Img, R);
    Exit;
  end;

  if not Assigned(Img.Bitmap) then
    Exit;
  if not Assigned(Img.Bitmap.Image) then
    Exit;
  IW := Img.Bitmap.Image.Width;
  IH := Img.Bitmap.Image.Height;
  if (IW = 0) or (IH = 0) then
    Exit;

  LWrapMode := Img.Bitmap.WrapMode;
  LBitmapChange := Img.Bitmap.OnChanged;
  Img.Bitmap.OnChanged := nil;
  W := R.Width;
  H := R.Height;

  case FScaleType of
    TImageScaleType.Matrix:
      begin
        Img.Bitmap.WrapMode := TWrapMode.TileOriginal;
        if IW < W then SW := IW else SW := W;
        if IH < H then SH := IH else SH := H;
        VR := RectSF(R.Left, R.Top, SW, SH);
        Canvas.DrawBitmap(Img.Bitmap.Bitmap, RectF(0, 0, SW, SH), VR, AbsoluteOpacity);
      end;

    TImageScaleType.Center:
      begin
        Img.Bitmap.WrapMode := TWrapMode.Tile;
        VR := RectSF(R.Left + (W - IW) * 0.5, R.Top + (H - IH) * 0.5, IW, IH);
        Canvas.DrawBitmap(Img.Bitmap.Bitmap, RectF(0, 0, IW, IH), VR, AbsoluteOpacity);
      end;

    TImageScaleType.CenterCrop:
      begin
        Img.Bitmap.WrapMode := TWrapMode.TileStretch;
        SW := W / IW;
        SH := H / IH;
        if SW > SH then begin
          SH := IH * SW;
          SW := W;
        end else begin
          SW := IW * SH;
          SH := H;
        end;
        VR := RectSF(R.Left + (W - SW) * 0.5, R.Top + (H - SH) * 0.5, SW, SH);
        Canvas.DrawBitmap(Img.Bitmap.Bitmap, RectF(0, 0, IW, IH), VR, AbsoluteOpacity);
      end;

    TImageScaleType.CenterInside:
      begin
        if (W >= IW) and (H >= IH) then begin
          Img.Bitmap.WrapMode := TWrapMode.Tile;
          VR := RectSF(R.Left + (W - IW) * 0.5, R.Top + (H - IH) * 0.5, IW, IH);
        end else begin
          Img.Bitmap.WrapMode := TWrapMode.TileStretch;
          SW := W / IW;
          SH := H / IH;
          if SW < SH then begin
            SH := IH * SW;
            SW := W;
          end else begin
            SW := IW * SH;
            SH := H;
          end;
          VR := RectSF(R.Left + (W - SW) * 0.5, R.Top + (H - SH) * 0.5, SW, SH);
        end;
        Canvas.DrawBitmap(Img.Bitmap.Bitmap, RectF(0, 0, IW, IH), VR, AbsoluteOpacity);
      end;

    TImageScaleType.FitCenter:
      begin
        Img.Bitmap.WrapMode := TWrapMode.TileStretch;
        SW := W / IW;
        SH := H / IH;
        if SW < SH then begin
          SH := IH * SW;
          SW := W;
        end else begin
          SW := IW * SH;
          SH := H;
        end;
        VR := RectSF(R.Left + (W - SW) * 0.5, R.Top + (H - SH) * 0.5, SW, SH);
        Canvas.DrawBitmap(Img.Bitmap.Bitmap, RectF(0, 0, IW, IH), VR, AbsoluteOpacity);
      end;

    TImageScaleType.FitStart:
      begin
        Img.Bitmap.WrapMode := TWrapMode.TileStretch;
        SW := W / IW;
        SH := H / IH;
        if SW < SH then begin
          SH := IH * SW;
          SW := W;
        end else begin
          SW := IW * SH;
          SH := H;
        end;
        VR := RectSF(R.Left, R.Top, SW, SH);
        Canvas.DrawBitmap(Img.Bitmap.Bitmap, RectF(0, 0, IW, IH), VR, AbsoluteOpacity);
      end;

    TImageScaleType.FitEnd:
      begin
        Img.Bitmap.WrapMode := TWrapMode.TileStretch;
        SW := W / IW;
        SH := H / IH;
        if SW < SH then begin
          SH := IH * SW;
          SW := W;
        end else begin
          SW := IW * SH;
          SH := H;
        end;
        VR := RectSF(R.Right - SW, R.Bottom - SH, SW, SH);
        Canvas.DrawBitmap(Img.Bitmap.Bitmap, RectF(0, 0, IW, IH), VR, AbsoluteOpacity);
      end;
  end;

  Img.Bitmap.WrapMode := LWrapMode;
  Img.Bitmap.OnChanged := LBitmapChange;

  if FImage is TDrawableBorder then
    TDrawableBorder(FImage).DrawBorder(Canvas, R, DrawState);
end;

procedure TImageView.DoImageChange(Sender: TObject);
begin
  Repaint;
end;

function TImageView.GetImage: TDrawable;
begin
  if not Assigned(FImage) then
    CreateImage;
  Result := FImage;
end;

procedure TImageView.PaintBackground;
begin
  inherited PaintBackground;
  if Assigned(FImage) and (AbsoluteInVisible = False) then
    DoDrawImage;
end;

procedure TImageView.SetImage(const Value: TDrawable);
begin
  if Value = nil then begin
    FreeAndNil(FImage);
    Exit;
  end;
  if (FImage = nil) then
    CreateImage;
  FImage.Assign(Value);
end;

procedure TImageView.SetScaleType(const Value: TImageScaleType);
begin
  if FScaleType <> Value then begin
    FScaleType := Value;
    Repaint;
  end;
end;

{ TBadgeView }

procedure TBadgeView.AfterConstruction;
begin
  inherited AfterConstruction;
  if Assigned(FBackground) then
    FBackground.OnChanged := DoChanged;
  FText.OnChanged := DoTextChanged;
end;

constructor TBadgeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxValue := 99;
  FStyle := TBadgeStyle.NumberText;
  FBackground := TBadgeBackground.Create;
  FBackground.FXRadius := 8;
  FBackground.FYRadius := 8;
  FText := TSimpleTextSettings.Create(Self);
  FText.Color := TAlphaColorRec.White;
  FText.ColorChange := False;
  HitTest := False;
  SetAcceptsControls(False);
  if csDesigning in ComponentState then
    GetIcon;
end;

destructor TBadgeView.Destroy;
begin
  if Assigned(FTargetView) then begin
    FTargetView.SetBadgeView(nil);
    FTargetView := nil;
  end;
  FreeAndNil(FText);
  FreeAndNil(FBackground);
  FreeAndNil(FIcon);
  inherited;
end;

procedure TBadgeView.DoAdjustSize;
var
  P: TSizeF;
begin
  if (Scene = nil) or (not Assigned(FText)) then
    Exit;
  FAdjustSizeing := True;
  try
    P.Width := Width;
    P.Height := Height;
    case FStyle of
      TBadgeStyle.NumberText, TBadgeStyle.NewText, TBadgeStyle.HotText:
        begin
          FText.CalcTextObjectSize(FText.Text, 0, Scene.GetSceneScale, nil, P);
          P.Width := P.Width + Padding.Left + Padding.Right;
          P.Height := P.Height + Padding.Top + Padding.Bottom;
          if Assigned(FBackground) then begin
            if P.Width < FBackground.FXRadius * 2 then P.Width := FBackground.FXRadius * 2;
            if P.Height < FBackground.FYRadius * 2 then P.Height := FBackground.FYRadius * 2;
          end;
        end;
      TBadgeStyle.Icon:
        begin
          if not Assigned(FIcon) or (FIcon.Kind <> TBrushKind.Bitmap) then
            Exit;
          if not Assigned(FIcon.Bitmap) then
            Exit;
          if not Assigned(FIcon.Bitmap.Bitmap) then
            Exit;
          P.Width := FIcon.Bitmap.Bitmap.Width + Padding.Left + Padding.Right;
          P.Height := FIcon.Bitmap.Bitmap.Height + Padding.Top + Padding.Bottom;
          if P.Width < 1 then P.Width := 1;
          if P.Height < 1 then P.Height := 1;
        end
    else
      Exit;
    end;
    if (P.Width <> Width) or (P.Height <> Height) then
      SetSize(P.Width, P.Height);
  finally
    FAdjustSizeing := False;
  end;
end;

procedure TBadgeView.DoChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TBadgeView.DoMatrixChanged(Sender: TObject);
begin
  inherited DoMatrixChanged(Sender);
  DoRealign;
end;

procedure TBadgeView.DoRealign;
var
  P: TPointF;
begin
  if FDisableAlign or (not Assigned(FTargetView)) then
    Exit;
  if (csDestroying in ComponentState) then
    Exit;
  FDisableAlign := True;
  P := FTargetView.LocalToAbsolute(TPointF.Zero);
  Position.Point := PointF(
    P.X + FTargetView.Width - Width * 0.65 + Margins.Left,
    P.Y - Height * 0.35 + Margins.Top{$IFDEF ANDROID} - TView.GetStatusHeight{$ENDIF});
  FDisableAlign := False;
end;

procedure TBadgeView.DoTextChanged(Sender: TObject);
begin
  if FAutoSize then begin
    DoAdjustSize;
    DoRealign;
  end;
  Repaint;
  if FText.IsEffectsChange then
    UpdateEffects;
end;

function TBadgeView.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(16, 16);
end;

function TBadgeView.GetFirstParent: TFmxObject;
begin
  Result := Self;
  while Result.Parent <> nil do begin
    if csDesigning in ComponentState then begin
      if Result.Parent.ClassName = 'TControlForm' then
        Break;
    end;
    Result := Result.Parent;
    if Result is TCustomForm then
      Break;
  end;
end;

function TBadgeView.GetIcon: TBrush;
begin
  if not Assigned(FIcon) then begin
    FIcon := TBrush.Create(TBrushKind.Bitmap, 0);
    FIcon.OnChanged := DoChanged;
  end;
  Result := FIcon;
end;

function TBadgeView.GetMaxValue: Integer;
begin
  Result := FMaxValue;
end;

function TBadgeView.GetStyle: TBadgeStyle;
begin
  Result := FStyle;
end;

function TBadgeView.GetText: string;
begin
  Result := FText.Text;
end;

function TBadgeView.GetValue: Integer;
begin
  Result := FValue;
end;

function TBadgeView.GetViewText: string;
begin
  case FStyle of
    TBadgeStyle.EmptyText: Result := '';
    TBadgeStyle.NumberText:
      begin
        if Value < 1 then
          Result := ''
        else if (MaxValue > 0) and (Value < MaxValue) then
          Result := IntToStr(Value)
        else
          Result := IntToStr(MaxValue) + FValueOutTail;
      end;
    TBadgeStyle.NewText: Result := 'NEW';
    TBadgeStyle.HotText: Result := 'HOT';
  end;
end;

function TBadgeView.IsVisibleView: Boolean;
begin
  if FStyle = TBadgeStyle.Icon then
    Result := (FValue > 0) and Assigned(FIcon) and Assigned(FIcon.Bitmap) and Assigned(FIcon.Bitmap.Bitmap) and
      (FIcon.Bitmap.Bitmap.Width > 0) and (FIcon.Bitmap.Bitmap.Height > 0)
  else
    Result := (FValue > 0)
end;

procedure TBadgeView.PaddingChanged;
begin
  inherited PaddingChanged;
  if FAutoSize then
    DoAdjustSize;
end;

procedure TBadgeView.Paint;
var
  R: TRectF;
begin
  if IsVisibleView then begin
    R := RectF(0, 0, Width, Height);
    if FStyle = TBadgeStyle.Icon then begin
      if Assigned(FIcon) then begin
        with FBackground do begin
          Canvas.FillRect(R, FXRadius, FYRadius, FCorners, FTargetView.Opacity, FIcon);
        end;
      end;
    end else begin
      if Assigned(FBackground) then
        with FBackground do begin
          Canvas.Fill.Color := FColor;
          Canvas.FillRect(R, FXRadius, FYRadius, FCorners, FTargetView.Opacity);
        end;
      if Assigned(FText) and (FText.TextLength > 0) then
        FText.Draw(Canvas, R, FTargetView.Opacity, TViewState.None);
    end;
  end;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

procedure TBadgeView.Resize;
begin
  inherited;
  if FAutoSize and (not FAdjustSizeing) then
    DoAdjustSize;
  DoRealign;
end;

procedure TBadgeView.AncestorParentChanged;
var
  LParent: TFmxObject;
begin
  inherited AncestorParentChanged;
  if csDesigning in ComponentState then begin
    LParent := GetFirstParent;
    if (Parent <> LParent) and (LParent <> nil) and (LParent <> Self) then
      Parent := LParent;
  end;
end;

procedure TBadgeView.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then begin
    FAutoSize := Value;
    if FAutoSize then
      DoAdjustSize;
    DoRealign;
    Repaint;
  end;
end;

procedure TBadgeView.SetBackground(const Value: TBadgeBackground);
begin
  FBackground.Assign(Value);
end;

procedure TBadgeView.SetIcon(const Value: TBrush);
begin
  if Value = nil then begin
    FreeAndNil(FIcon);
    Exit;
  end;
  Icon.Assign(Value);
end;

procedure TBadgeView.SetValue(const Value: Integer);
begin
  if FValue <> Value then begin
    FValue := Value;
    FText.Text := GetViewText;
    Repaint;
  end;
end;

procedure TBadgeView.SetValueOutTail(const Value: string);
begin
  if FValueOutTail <> Value then begin
    FValueOutTail := Value;
    DoRealign;
    Repaint;
  end;
end;

procedure TBadgeView.SetMaxValue(const Value: Integer);
begin
  if FMaxValue <> Value then begin
    FMaxValue := Value;
    DoRealign;
    Repaint;
  end;
end;

procedure TBadgeView.SetStyle(const Value: TBadgeStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    FText.Text := GetViewText;
    if FAutoSize then
      DoAdjustSize;
    Repaint;
  end;
end;

procedure TBadgeView.SetTargetView(const Value: IView);
begin
  if FTargetView <> Value then begin
    if Assigned(FTargetView) and (FTargetView.BadgeView = Self as IViewBadge) then
      FTargetView.SetBadgeView(nil);
    FTargetView := Value;
    if Assigned(FTargetView) then begin
      FTargetView.SetBadgeView(Self);
      DoRealign;
    end;
    Repaint;
  end;
end;

procedure TBadgeView.SetTextSettings(const Value: TSimpleTextSettings);
begin
  FText.Assign(Value);
end;

{ TSimpleTextSettings }

constructor TSimpleTextSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Gravity := TLayoutGravity.Center;
  FColor := TAlphaColorRec.Black;
  FColorChange := False;
end;

function TSimpleTextSettings.GetStateColor(const State: TViewState): TAlphaColor;
begin
  Result := FColor;
end;

function TSimpleTextSettings.IsColorStored: Boolean;
begin
  Result := FColorChange;
end;

procedure TSimpleTextSettings.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

{ TBadgeBackground }

procedure TBadgeBackground.Assign(Source: TPersistent);
var
  SaveChange: TNotifyEvent;
begin
  if Source is TBadgeBackground then begin
    SaveChange := FOnChanged;
    FOnChanged := nil;
    FCorners := TBadgeBackground(Source).Corners;
    FXRadius := TBadgeBackground(Source).FXRadius;
    FYRadius := TBadgeBackground(Source).FYRadius;
    FColor := TBadgeBackground(Source).FColor;
    FOnChanged := SaveChange;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end else
    inherited;
end;

constructor TBadgeBackground.Create;
begin
  FCorners := AllCorners;
  FColor := TAlphaColorRec.Red;
end;

function TBadgeBackground.IsStoredCorners: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

procedure TBadgeBackground.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TBadgeBackground.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then begin
    FCorners := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TBadgeBackground.SetXRadius(const Value: Single);
begin
  if FXRadius <> Value then begin
    FXRadius := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TBadgeBackground.SetYRadius(const Value: Single);
begin
  if FYRadius <> Value then begin
    FYRadius := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

{ TRingView }

constructor TRingView.Create(AOwner: TComponent);
begin
  inherited;
  FPath := TPathData.Create;
  FDistance := 10;
  FInner := TRingViewStyle.Ellipse;
  FPathChanged := True;
  FClickInPath := True;
end;

destructor TRingView.Destroy;
begin
  FreeAndNil(FPath);
  inherited Destroy;
end;

procedure TRingView.DoBackgroundChanged(Sender: TObject);
begin
  FPathChanged := True;
  inherited;
end;

function TRingView.IsStoredDistance: Boolean;
begin
  Result := FDistance <> 10;
end;

procedure TRingView.PaintBackground;
var
  V: TBrush;
  LBorder: TViewBorder;
  LOnChange: TNotifyEvent;
begin
  if FPathChanged then begin
    RecreatePath;
    FPathChanged := False;
  end;

  if AbsoluteInVisible then
    Exit;

  V := FBackground.GetStateItem(DrawState);
  if V <> nil then
    Canvas.FillPath(FPath, AbsoluteOpacity, V);

  LBorder := TDrawableBorder(FBackground)._Border;
  if Assigned(LBorder) and (LBorder.Width > 0) and (LBorder.Style = TViewBorderStyle.RectBorder) then begin
    if LBorder.Kind = TBrushKind.Solid then begin
      LOnChange := LBorder.Brush.OnChanged;
      LBorder.Brush.OnChanged := nil;
      LBorder.Brush.Color :=  LBorder.Color.GetStateColor(DrawState);
      Canvas.DrawPath(FPath, AbsoluteOpacity, LBorder.Brush);
      LBorder.Brush.OnChanged := LOnChange;
    end else
      Canvas.DrawPath(FPath, AbsoluteOpacity, LBorder.Brush);
  end;
end;

procedure TRingView.PathChanged;
begin
  FPathChanged := True;
  Invalidate;
end;

function TRingView.PointInObject(X, Y: Single): Boolean;
var
  LP: TPointF;
  LR, LE, LS: Single;
begin
  if FClickInPath and (not AbsoluteInVisible) and (not FInPaintTo) and (FPath.Count > 0) then begin
    if (FOuter = TRingViewStyle.Circle) and (FInner = TRingViewStyle.Circle) then begin
      // 圆环
      LP := AbsoluteToLocal(PointF(X, Y));
      if FStartAngle = FAngle then
        Result := False  // 夹角为0时不显示
      else begin
        // 判断是否在圆环内
        Result := Sqr(LP.X - FCenter.X) + Sqr(LP.Y - FCenter.Y) <= Sqr(FRadius);
        if Result then
          Result := Sqr(LP.X - FCenter.X) + Sqr(LP.Y - FCenter.Y) >= Sqr(FRadius - FDistance);
        // 在圆环内时，判断是否在指定夹角内
        if Result and (FAngle - FStartAngle < 360) then begin
          LS := FStartAngle;
          LE := FAngle;
          if LS < 0 then LS := 360 - LS;
          if LE < 0 then LE := 360 - LE;
          if LS > LE then begin
            LR := LS;
            LS := LE;
            LE := LR;
          end;
          LR := GetAngle(FCenter.X, FCenter.Y, LP.X, LP.Y);
          Result := (LR >= LS) and (LR <= LE);
        end;
      end;
    end else
      Result := Canvas.PtInPath(AbsoluteToLocal(PointF(X, Y)), FPath);
  end else
    Result := inherited PointInObject(X, Y);
end;

procedure TRingView.RecreatePath;
var
  R: TRectF;
  LC, LR: TPointF;
begin
  FPath.Clear;
  R := RectF(FBackground.Padding.Left, FBackground.Padding.Top,
    Width - FBackground.Padding.Right, Height - FBackground.Padding.Bottom);;

  if (FOuter = TRingViewStyle.Circle) and (FInner = TRingViewStyle.Circle) then
  begin
    if FStartAngle = FAngle then
      Exit;
    LC := PointF(R.Width / 2 + R.Left, R.Height / 2 + R.Top);
    if R.Width > R.Height then
      FRadius := R.Height / 2
    else
      FRadius := R.Width / 2;
    FCenter := LC;
    if (FAngle - FStartAngle < 360) then begin
      FPath.AddRing(LC, FRadius, FRadius - FDistance, FStartAngle, FAngle - FStartAngle);
    end else begin
      FPath.AddEllipse(RectF(LC.X - FRadius, LC.Y - FRadius, LC.X + FRadius, LC.Y + FRadius));
      LR.X := FRadius - FDistance;
      FPath.AddEllipse(RectF(LC.X - LR.X, LC.Y - LR.X, LC.X + LR.X, LC.Y + LR.X));
      FPath.ClosePath;
    end;
    Exit;
  end;

  case FOuter of
    TRingViewStyle.Rectangle:
      FPath.AddRectangle(R, FBackground.XRadius, FBackground.YRadius,
        FBackground.Corners, FBackground.CornerType);
    TRingViewStyle.Circle:
      begin
        LC := PointF(R.Width / 2 + R.Left, R.Height / 2 + R.Top);
        if R.Width > R.Height then
          LR := PointF(R.Height / 2, R.Height / 2)
        else
          LR := PointF(R.Width / 2, R.Width / 2);
        FPath.AddEllipse(RectF(LC.X - LR.X, LC.Y - LR.Y, LC.X + LR.X, LC.Y + LR.Y));
      end;
    TRingViewStyle.Ellipse:
      FPath.AddEllipse(R);
  end;

  case FInner of
    TRingViewStyle.Rectangle:
      begin
        R.Inflate(-FDistance, -FDistance);
        FPath.AddRectangle(R, FBackground.XRadius, FBackground.YRadius,
          FBackground.Corners, FBackground.CornerType);
      end;
    TRingViewStyle.Circle:
      begin
        R.Inflate(-FDistance, -FDistance);
        LC := PointF(R.Width / 2 + R.Left, R.Height / 2 + R.Top);
        if R.Width > R.Height then
          LR := PointF(R.Height / 2, R.Height / 2)
        else
          LR := PointF(R.Width / 2, R.Width / 2);
        FPath.AddEllipse(RectF(LC.X - LR.X, LC.Y - LR.Y, LC.X + LR.X, LC.Y + LR.Y));
        FPath.ClosePath;
      end;
    TRingViewStyle.Ellipse:
      begin
        R.Inflate(-FDistance, -FDistance);
        FPath.AddEllipse(R);
        FPath.ClosePath;
      end;
  end;

end;

procedure TRingView.Resize;
begin
  inherited Resize;
  PathChanged;
end;

procedure TRingView.SetDistance(const Value: Single);
begin
  if FDistance <> Value then begin
    FDistance := Value;
    PathChanged;
  end;
end;

procedure TRingView.SetEndAngle(const Value: Single);
begin
  if FAngle <> Value then begin
    FAngle := Value;
    PathChanged;
  end;
end;

procedure TRingView.SetInner(const Value: TRingViewStyle);
begin
  if FInner <> Value then begin
    FInner := Value;
    PathChanged;
  end;
end;

procedure TRingView.SetOuter(const Value: TRingViewStyle);
begin
  if FOuter <> Value then begin
    FOuter := Value;
    PathChanged;
  end;
end;

procedure TRingView.SetStartAngle(const Value: Single);
begin
  if FStartAngle <> Value then begin
    FStartAngle := Value;
    PathChanged;
  end;
end;

{ TPathDataHelper }

procedure TPathDataHelper.AddRing(const ACenter: TPointF;
  const R1, R2, AStartAngle, ASweepAngle: Single);
var
  A, CA, SA: Single;
begin
  AddArc(ACenter, PointF(R1, R1), AStartAngle, ASweepAngle);
  A := AStartAngle * PI / 180;
  SA := sin(A);
  CA := cos(A);
  MoveTo(PointF(ACenter.X + R1 * CA, ACenter.Y + R1 * SA));
  LineTo(PointF(ACenter.X + R2 * CA, ACenter.Y + R2 * SA));
  AddArc(ACenter, PointF(R2, R2), AStartAngle, ASweepAngle);
  A := (AStartAngle + ASweepAngle) * PI / 180;
  SA := sin(A);
  CA := cos(A);
  LineTo(PointF(ACenter.X + R1 * CA, ACenter.Y + R1 * SA));
end;

{ TDrawableProgress }

procedure TDrawableProgress.InitDrawable;
begin
  FWidth := 8;
end;

function TDrawableProgress.IsStoredWidth: Boolean;
begin
  Result := FWidth <> 8;
end;

procedure TDrawableProgress.SetWidth(const Value: Single);
begin
  if FWidth <> Value then begin
    FWidth := Value;
    DoChange(Self);
  end;
end;

{ TPathViewItem }

procedure TPathViewItem.ApplayScale;
var
  XOffset: Single;
  YOffset: Single;
  W, H: Single;
begin
  FPath.Data := FPathData;
  FPath.Scale(FScaleX, FScaleY);
  UpdateSize();
  if not Assigned(FOwner) then
    Exit;

  XOffset := 0;
  YOffset := 0;
  W := FOwner.Width;
  H := FOwner.Height;

  case FGravity of
    TLayoutGravity.LeftBottom:
      begin
        YOffset := H - FHeight;
      end;
    TLayoutGravity.RightTop:
      begin
        XOffset := W - FWidth;
      end;
    TLayoutGravity.RightBottom:
      begin
        XOffset := W - FWidth;
        YOffset := H - FHeight;
      end;
    TLayoutGravity.CenterVertical:
      begin
        YOffset := (H - FHeight) * 0.5;
      end;
    TLayoutGravity.CenterHorizontal:
      begin
        XOffset := (W - FWidth) * 0.5;
      end;
    TLayoutGravity.CenterHBottom:
      begin
        XOffset := (W - FWidth) * 0.5;
        YOffset := H - FHeight;
      end;
    TLayoutGravity.CenterVRight:
      begin
        XOffset := W - FWidth;
        YOffset := (H - FHeight) * 0.5;
      end;
    TLayoutGravity.Center:
      begin
        XOffset := (W - FWidth) * 0.5;
        YOffset := (H - FHeight) * 0.5;
      end;
  end;

  if (XOffset <> 0) or (YOffset <> 0) then
    FPath.Translate(XOffset, YOffset);
end;

constructor TPathViewItem.Create(Collection: TCollection);
begin
  inherited;
  FVisible := True;
  FScaleX := 1;
  FScaleY := 1;
  FPath := TPathData.Create;
end;

destructor TPathViewItem.Destroy;
begin
  FreeAndNil(FStyle);
  FreeAndNil(FPath);
  inherited;
end;

procedure TPathViewItem.DoChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TPathViewItem.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TPathViewItem.GetPath: TPathData;
begin
  Result := FPath;
end;

function TPathViewItem.GetStyle: TPathViewStyles;
begin
  if not Assigned(FStyle) then begin
    FStyle := TPathViewStyles.Create();
    FStyle.OnChange := DoChanged;
  end;
  Result := FStyle;
end;

function TPathViewItem.IsStoreScaleX: Boolean;
begin
  Result := FScaleX <> 1;
end;

function TPathViewItem.IsStoreScaleY: Boolean;
begin
  Result := FScaleY <> 1;
end;

procedure TPathViewItem.SetDisplayName(const Value: string);
begin
  if FDisplayName <> Value then begin
    FDisplayName := Value;
    inherited;
  end;
end;

procedure TPathViewItem.SetGravity(const Value: TLayoutGravity);
begin
  if FGravity <> Value then begin
    FGravity := Value;
    ApplayScale;
    DoChanged(Self);
  end;
end;

procedure TPathViewItem.SetOwner(const Value: TControl);
begin
  if FOwner <> Value then begin
    FOwner := Value;
    ApplayScale;
  end;
end;

procedure TPathViewItem.SetPath(const Value: TPathData);
begin
  FPath.Assign(Value);
  DoChanged(Self);
end;

procedure TPathViewItem.SetPathData(const Value: string);
begin
  if FPathData <> Value then begin
    FPathData := Value;
    FPath.Data := FPathData;
    ApplayScale();
    DoChanged(Self);
  end;
end;

procedure TPathViewItem.SetScaleX(const Value: Single);
begin
  if FScaleX <> Value then begin
    FScaleX := Value;
    ApplayScale;
    DoChanged(Self);
  end;
end;

procedure TPathViewItem.SetScaleY(const Value: Single);
begin
  if FScaleY <> Value then begin
    FScaleY := Value;
    ApplayScale;
    DoChanged(Self);
  end;
end;

procedure TPathViewItem.SetStyle(const Value: TPathViewStyles);
begin
  if (Value = nil) then begin
    FreeAndNil(FStyle);
  end else
    Style.Assign(Value);
end;

procedure TPathViewItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then begin
    FVisible := Value;
    DoChanged(Self);
  end;
end;

procedure TPathViewItem.UpdateSize;
var
  I: Integer;
  MW, MH: Single;
begin
  MW := 0;
  MH := 0;
  for I := 0 to FPath.Count - 1 do begin
    with FPath.Points[I] do begin
      if Kind = TPathPointKind.Close then
        Continue;
      MW := Max(Point.X, MW);
      MH := Max(Point.Y, MH);
    end;
  end;
  FWidth := MW;
  FHeight := MH;
end;

{ TPathViewCollection }

function TPathViewCollection.Add: TPathViewItem;
begin
  Result := inherited Add as TPathViewItem;
  Result.Owner := FOwner;
  Result.OnChange := DoChange;
end;

constructor TPathViewCollection.Create(AOwner: TControl;
  ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

procedure TPathViewCollection.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TPathViewCollection.DoItemChanged(ASender: TObject);
begin
  Changed;
  DoChange(Self);
end;

procedure TPathViewCollection.DoParentSizeChange;
var
  I: Integer;
  Item: TPathViewItem;
begin
  for I := 0 to Count - 1 do begin
    Item := Items[I];
    if Assigned(Item) and (Item.Gravity <> TLayoutGravity.None) then
      Item.ApplayScale;
  end;
end;

function TPathViewCollection.FindItemID(ID: Integer): TPathViewItem;
begin
  Result := inherited FindItemID(ID) as TPathViewItem;
end;

function TPathViewCollection.GetItem(Index: Integer): TPathViewItem;
begin
  Result := inherited GetItem(Index) as TPathViewItem;
  if Assigned(Result) then begin
    Result.Owner := FOwner;
    Result.OnChange := DoChange;
  end;
end;

function TPathViewCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TPathViewCollection.SetItem(Index: Integer;
  const Value: TPathViewItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TPathViewCollection.Update(Item: TCollectionItem);
begin
  inherited;
  DoChange(Self);
end;

{ TCustomMultiPathView }

function TCustomMultiPathView.CanRePaintBk(const View: IView;
  State: TViewState): Boolean;
begin
  Result := inherited CanRePaintBk(View, State);
  if (not Result) then begin
    {$IFDEF NEXTGEN}
    Result := (FPaths.Count > 0) and (FPressedIndex <> -1);
    {$ELSE}
    Result := (FPaths.Count > 0) and ((FHoverIndex <> -1) or (FPressedIndex <> -1));
    {$ENDIF}
  end;
end;

procedure TCustomMultiPathView.Click;
begin
  inherited Click;
  if Assigned(FOnItemClick) and (FPressedIndex <> -1) then
    FOnItemClick(Self, FPressedIndex);
end;

constructor TCustomMultiPathView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPaths := TPathViewCollection.Create(Self, TPathViewItem);
  FPaths.OnChange := DoPathChange;
  FHoverIndex := -1;
  FActiveIndex := -1;
  FPressedIndex := -1;
  FClickInPath := False;
end;

destructor TCustomMultiPathView.Destroy;
begin
  FreeAndNil(FPaths);
  inherited;
end;

procedure TCustomMultiPathView.DoPathChange(Sender: TObject);
begin
  Invalidate;
end;

function TCustomMultiPathView.GetPathCount: Integer;
begin
  Result := FPaths.Count;
end;

function TCustomMultiPathView.IndexAt(const LocalPoint: TPointF): Integer;
var
  I: Integer;
  LCanvas: TCanvas;
begin
  LCanvas := Canvas;
  for I := 0 to FPaths.Count - 1 do begin
    if Assigned(FPaths[I].FPath) and FPaths[I].Visible and
      LCanvas.PtInPath(LocalPoint, FPaths[I].FPath) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TCustomMultiPathView.ItemAt(const LocalPoint: TPointF): TPathViewItem;
var
  I: Integer;
begin
  I := IndexAt(LocalPoint);
  if I < 0 then
    Result := nil
  else
    Result := FPaths[I];
end;

procedure TCustomMultiPathView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if (TMouseButton.mbLeft = Button) and (Clickable) then
    FPressedIndex := IndexAt(PointF(X, Y));
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomMultiPathView.MouseMove(Shift: TShiftState; X, Y: Single);
{$IFNDEF NEXTGEN}
var
  I: Integer;
{$ENDIF}
begin
  inherited MouseMove(Shift, X, Y);
  {$IFNDEF NEXTGEN}
  I := IndexAt(PointF(X, Y));
  if I <> FHoverIndex then begin
    FHoverIndex := I;
    if Assigned(FOnItemHover) then
      FOnItemHover(Self);
    Invalidate;
  end;
  {$ENDIF}
end;

procedure TCustomMultiPathView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  if FPressedIndex <> -1 then
    FActiveIndex := FPressedIndex;
  inherited MouseUp(Button, Shift, X, Y);
  FHoverIndex := -1;
  FPressedIndex := -1;
end;

procedure TCustomMultiPathView.PaintBackground;

  procedure DrawStyle(const Canvas: TCanvas; const APath: TPathViewItem;
    const AStyle: TPathViewStyle; const AOpacity: Single);
  begin
    if Assigned(AStyle) then begin
      if Assigned(AStyle.FFill) and (AStyle.FFill.Kind <> TBrushKind.None) then
        Canvas.FillPath(APath.FPath, AOpacity, AStyle.FFill);
      if Assigned(AStyle.FStroke) and (AStyle.FStroke.Kind <> TBrushKind.None) then
        Canvas.DrawPath(APath.FPath, AOpacity, AStyle.FStroke);
    end;
  end;

var
  I: Integer;
  APath: TPathViewItem;
  LOpacity: Single;
  LCanvas: TCanvas;
  LStyle: TPathViewStyle;
begin
  inherited PaintBackground;
  if AbsoluteInVisible then
    Exit;
  LCanvas := Canvas;
  LOpacity := AbsoluteOpacity;
  for I := 0 to FPaths.Count - 1 do begin
    APath := FPaths[I];
    if (not Assigned(APath.FPath)) or (APath.FVisible = False) then
      Continue;
    if APath.FPath.Count = 0 then
      Continue;
    if not Assigned(APath.FStyle) then
      Continue;

    if I = FPressedIndex then
      LStyle := APath.FStyle.FItemPressed
    else if I = FActiveIndex then begin
      LStyle := APath.FStyle.FItemActive
    end else if I = HoverIndex then begin
      LStyle := APath.FStyle.FItemHover;
    end else
      LStyle := nil;

    if (LStyle = nil) or (LStyle.IsEmpty) then
      LStyle := APath.FStyle.FItemDefault;

    DrawStyle(LCanvas, APath, LStyle, LOpacity)
  end;
end;

function TCustomMultiPathView.PointInObject(X, Y: Single): Boolean;
begin
  if FClickInPath and (FPaths.Count > 0) and (not AbsoluteInVisible) and (not FInPaintTo) then begin
    Result := IndexAt(AbsoluteToLocal(PointF(X, Y))) <> -1;
    {$IFNDEF NEXTGEN}
    if (not Result) and (FHoverIndex <> -1) then begin
      FHoverIndex := -1;
      if not (csDesigning in ComponentState) then
        Invalidate;
    end;
    {$ENDIF}
  end else
    Result := inherited PointInObject(X, Y);
end;

procedure TCustomMultiPathView.Resize;
begin
  if Assigned(FPaths) and (not (csDestroying in ComponentState)) then
    FPaths.DoParentSizeChange;
  inherited Resize;
end;

procedure TCustomMultiPathView.SetActiveIndex(const Value: Integer);
begin
  if FActiveIndex <> Value then begin
    FActiveIndex := Value;
    Invalidate;
  end;
end;

procedure TCustomMultiPathView.SetPaths(const Value: TPathViewCollection);
begin
  FPaths.Assign(Value);
end;

{ TPathViewStyle }

procedure TPathViewStyle.Assign(Source: TPersistent);
var
  LastOnChange: TNotifyEvent;
begin
  if Source is TPathViewStyle then begin
    LastOnChange := FOnChange;
    Self.Fill := TPathViewStyle(Source).FFill;
    Self.FStroke := TPathViewStyle(Source).FStroke;
    FOnChange := LastOnChange;
    DoChanged(Self);
  end else
    inherited;
end;

constructor TPathViewStyle.Create;
begin
  inherited Create;
end;

destructor TPathViewStyle.Destroy;
begin
  FreeAndNil(FFill);
  FreeAndNil(FStroke);
  inherited;
end;

procedure TPathViewStyle.DoChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TPathViewStyle.GetFill: TBrush;
begin
  if FFill = nil then begin
    FFill := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Null);
    FFill.OnChanged := DoChanged;
  end;
  Result := FFill;
end;

function TPathViewStyle.GetStroke: TStrokeBrush;
begin
  if FStroke = nil then begin
    FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Null);
    FStroke.OnChanged := DoChanged;
  end;
  Result := FStroke;
end;

function TPathViewStyle.IsEmpty: Boolean;
begin
  Result := ((FFill = nil) or (FFill.Kind = TBrushKind.None)) and
    ((FStroke = nil) or (FStroke.Kind = TBrushKind.None));
end;

procedure TPathViewStyle.SetFill(const Value: TBrush);
begin
  if (Value = nil) then begin
    FreeAndNil(FFill);
    DoChanged(Self);
  end else
    Fill.Assign(Value);
end;

procedure TPathViewStyle.SetStroke(const Value: TStrokeBrush);
begin
  if (Value = nil) then begin
    FreeAndNil(FStroke);
    DoChanged(Self);
  end else
    FStroke.Assign(Value);
end;

{ TPathViewStyles }

procedure TPathViewStyles.Assign(Source: TPersistent);
var
  LastOnChange: TNotifyEvent;
begin
  if Source is TPathViewStyles then begin
    LastOnChange := FOnChanged;
    Self.ItemDefault := TPathViewStyles(Source).FItemDefault;
    Self.ItemActivated := TPathViewStyles(Source).FItemActive;
    Self.ItemHovered := TPathViewStyles(Source).FItemHover;
    Self.ItemPressed := TPathViewStyles(Source).FItemPressed;
    FOnChanged := LastOnChange;
    DoChanged(Self);
  end else
    inherited;
end;

procedure TPathViewStyles.CreateStyle(var Value: TPathViewStyle);
begin
  Value := TPathViewStyle.Create;
  Value.FOnChange := DoChanged;
end;

destructor TPathViewStyles.Destroy;
begin
  FreeAndNil(FItemDefault);
  FreeAndNil(FItemActive);
  FreeAndNil(FItemHover);
  FreeAndNil(FItemPressed);
  inherited;
end;

procedure TPathViewStyles.DoChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

function TPathViewStyles.GetActiveStyle: TPathViewStyle;
begin
  if not Assigned(FItemActive) then
    CreateStyle(FItemActive);
  Result := FItemActive;
end;

function TPathViewStyles.GetDefaultStyle: TPathViewStyle;
begin
  if not Assigned(FItemDefault) then
    CreateStyle(FItemDefault);
  Result := FItemDefault;
end;

function TPathViewStyles.GetHoverStyle: TPathViewStyle;
begin
  if not Assigned(FItemHover) then
    CreateStyle(FItemHover);
  Result := FItemHover;
end;

function TPathViewStyles.GetPressedStyle: TPathViewStyle;
begin
  if not Assigned(FItemPressed) then
    CreateStyle(FItemPressed);
  Result := FItemPressed;
end;

procedure TPathViewStyles.SetActiveStyle(const Value: TPathViewStyle);
begin
  UpdateStyle(FItemActive, Value);
end;

procedure TPathViewStyles.SetDefaultStyle(const Value: TPathViewStyle);
begin
  UpdateStyle(FItemDefault, Value);
end;

procedure TPathViewStyles.SetHoverStyle(const Value: TPathViewStyle);
begin
  UpdateStyle(FItemHover, Value);
end;

procedure TPathViewStyles.SetPressedStyle(const Value: TPathViewStyle);
begin
  UpdateStyle(FItemPressed, Value);
end;

procedure TPathViewStyles.UpdateStyle(var Source: TPathViewStyle;
  const Value: TPathViewStyle);
begin
  if (Value = nil) then begin
    if (Source <> nil) then begin
      FreeAndNil(Source);
      DoChanged(Self);
    end;
    Exit;
  end;
  if Source = nil then
    CreateStyle(Source);
  Source.Assign(Value);
end;

{ TMultiPathView }

constructor TMultiPathView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Clickable := True;
end;



initialization

finalization

end.

