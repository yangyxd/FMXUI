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
  UI.Base, UI.Utils, UI.Ani, FMX.Effects, FMX.Text,
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
  {$IF CompilerVersion > 30.0}
  FMX.AcceleratorKey,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows, FMX.Platform.Win,
  {$ENDIF}
  FMX.BehaviorManager, FMX.Forms, System.Messaging, FMX.Styles,
  FMX.Media,
  FMX.ActnList, FMX.Objects, System.Math, System.Actions, System.Rtti, FMX.Consts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, FMX.TextLayout,
  System.SyncObjs,
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
    FValueOffset: Single;
    function IsStoredWidth: Boolean;
    procedure SetWidth(const Value: Single);
    function IsStoreValueOffset: Boolean;
    procedure SetValueOffset(const Value: Single);
  protected
    procedure InitDrawable; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property RingWidth: Single read FWidth write SetWidth stored IsStoredWidth;
    property ValueOffset: Single read FValueOffset write SetValueOffset stored IsStoreValueOffset;
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
  /// <summary>
  /// 图像浏览器  {由 TksImageViewer 改写}
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TImageViewerEx = class(TView)
  private
    FAniCalc: TAniCalculations;
    FBitmap: TBitmap;
    FZoom: integer;

    FStartZoom: integer;
    FStartDistance: Integer;
    FMaxXPos: single;
    FMaxYPos: single;

    FStretch: Boolean;

    FOnZoom: TNotifyEvent;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetStretch(const Value: Boolean);
  protected
    procedure AniCalcChange(Sender: TObject);
    procedure AniCalcStart(Sender: TObject);
    procedure AniCalcStop(Sender: TObject);
    procedure UpdateScrollLimits;
    procedure SetZoom(const Value: integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      x, y: single); override;
    procedure MouseMove(Shift: TShiftState; x, y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      x, y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
    procedure DoMouseLeave; override;
    procedure Resize; override;
    procedure PaintBackground; override;
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateLabel(ADistance: integer);
  published
    property Image: TBitmap read FBitmap write SetBitmap;
    property Zoom: integer read FZoom write SetZoom default 100;
    /// <summary>
    /// 是否拉伸
    /// </summary>
    property Stretch: Boolean read FStretch write SetStretch default False;

    property CanFocus default True;
    property HitTest default True;
    property Clickable default True;

    property OnGesture;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
  end;

type
  TScrollView = class;
  TOnCalcContentBoundsEvent = procedure (Sender: TObject; var ContentBounds: TRectF) of object;
  TPositionChangeEvent = procedure(Sender: TObject; const OldViewportPosition,
    NewViewportPosition: TPointD; const ContentSizeChanged: Boolean) of object;
  PRectD = ^TRectD;

  /// <summary>
  /// 滚动视图
  /// </summary>
  TScrollView = class(TView, IViewTouch)
  private const
    ChangeRepaintedIncidentDelay = 0.1; // seconds
    PhysicsProcessingInterval = 8; // 8 ms for ~120 frames per second
    DefaultScrollingStretchGlowColor: TAlphaColor = $FFC0C0C0;
  private
    FCanAnimation: Boolean;
    FInInternalAlign: Boolean;
    FCachedAutoShowing: Boolean;
    FDragScroll: Boolean;

    FDragOneWay: Boolean;
    FLastTouchTracking: TTouchTracking;

    FScrollbarWidth: Single;

    FOnScrollChange: TNotifyEvent;
    FOnViewportPositionChange: TPositionChangeEvent;

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
    procedure SetDragOneWay(const Value: Boolean);
  protected
    FCanScrollV: Boolean;
    FCanScrollH: Boolean;
    FShowScrollBars: Boolean;
    FScrolling: Boolean;
    FSystemInfoSrv: IFMXSystemInformationService;
    FListingService: IFMXListingService;

    FScrollV: TScrollBar;
    FScrollH: TScrollBar;
    FAniCalculations: TScrollCalculations;

    FContentBounds: PRectD;
    FLastViewportPosition: TPointD;
    FMouseEvents: Boolean;
    FScrollStretchStrength: Single;
    //FScrollTrackPressed: Boolean;
    FScrollStart: Boolean;
    FScrollingStretchGlowColor: TAlphaColor;
    FScrollSmallChangeFraction: Single;    //Animation mouse events

    procedure AniMouseDown(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseMove(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseUp(const Touch: Boolean; const X, Y: Single); virtual;

    procedure AniCalcChange(Sender: TObject);

    procedure CMGesture(var EventInfo: TGestureEventInfo); override;

    function CanAnimation: Boolean; override;
    function CanInheritedCMGesture(const EventInfo: TGestureEventInfo): Boolean; virtual;

    function IsRunningOnDesktop: Boolean;
    function HasTouchTracking: Boolean;
    function HasStretchyScrolling: Boolean;
    function HasScrollingStretchGlow: Boolean;
    function HasPhysicsStretchyScrolling: Boolean;
    function GetMaxScrollViewPos: Integer;
    procedure ScrollStretchChanged; virtual;
    procedure UpdateScrollStretchStrength(const NewValue: Single);

    function IsCanTouch: Boolean; virtual;
    function GetScrollOffset: TPointF; virtual;
  protected
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
    procedure ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointD; const ContentSizeChanged: boolean); virtual;

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

    procedure ContentAddObject(const AObject: TFmxObject); virtual;
    procedure ContentInsertObject(Index: Integer; const AObject: TFmxObject); virtual;
    procedure ContentBeforeRemoveObject(AObject: TFmxObject); virtual;
    procedure ContentRemoveObject(const AObject: TFmxObject); virtual;

    function IsStoredScrollSmallChangeFraction: Boolean; virtual;

    property InInternalAlign: Boolean read FInInternalAlign;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollBy(const Dx, Dy: Double);
    procedure ScrollTo(const Dx, Dy: Double);

    // 刷新开始
    procedure PullRefreshStart(); virtual;

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
    /// <summary>
    /// 拖动时是否只能单向
    /// </summary>
    property DragOneWay: Boolean read FDragOneWay write SetDragOneWay default False;
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
    property OnViewportPositionChange: TPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
  published
  end;

type
  TPullScrollView = class;

  /// <summary>
  /// 滚动视图内容
  /// </summary>
  TViewScrollContent = class(TLinearLayout, IContent)
  private
    [weak] FScrollBox: TScrollView;
    FIsContentChanged: Boolean;
  protected
    function GetClipRect: TRectF; override;
    function GetChildrenRect: TRectF; override;
    function ObjectAtPoint(P: TPointF): IControl; override;
    function DoGetUpdateRect: TRectF; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    procedure DoRealign; override;
    procedure ContentChanged; virtual;
    procedure IContent.Changed = ContentChanged;
    /// <summary> This flag is set in the method ContentChanged. Used to optimize ScrollBox </summary>
    property IsContentChanged: Boolean read FIsContentChanged write FIsContentChanged;
  public
    constructor Create(AOwner: TComponent); override;
    function PointInObjectLocal(X, Y: Single): Boolean; override;
    property ScrollBox: TScrollView read FScrollBox;
  end;

  /// <summary>
  /// 拉动更新滚动视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TPullScrollView = class(TScrollView)
  private const
    CSContentName = 'ContentLayout';
  private
    FContent: TViewScrollContent;
    FEnablePullRefresh: Boolean;
    FEnablePullLoad: Boolean;

    // 下拉刷新，上拉加载更多
    FState: TListViewState;      // 列表视图状态
    FHeader: IListViewHeader;    // 头部下拉刷新列表视图
    FFooter: IListViewHeader;    // 尾部上拉加载更多视图

    FLastScrollValue: Double; // 上次滚动条位置
    FOffsetScroll: Single;

    FOnInitFooter: TOnInitHeader;
    FOnInitHeader: TOnInitHeader;
    FOnPullRefresh: TNotifyEvent;
    FOnPullLoad: TNotifyEvent;
    procedure SetEnablePullLoad(const Value: Boolean);
    procedure SetEnablePullRefresh(const Value: Boolean);
    function GetContentChildCount: Integer;
    function GetContentControlItem(const Index: Integer): TControl;
  protected
    function CreateScroll: TScrollBar; override;
    procedure InvalidateContentSize(); override; // 计算内容区大小
    procedure VScrollChange(Sender: TObject); override;
    procedure HScrollChange(Sender: TObject); override;
    function AllowInitScrollbar: Boolean; override;
    procedure DoPullLoad(Sender: TObject);
    procedure AniMouseUp(const Touch: Boolean; const X, Y: Single); override;
  protected
    {$IFNDEF NEXTGEN}
    FDownPos, FMovePos: TPointF;
    {$ENDIF}
    {$IFNDEF NEXTGEN}
    [Weak] FPointTarget: IControl;
    FMouseEnter, FMouseDown: Boolean;
    {$ENDIF}
    function ObjectAtPoint(AScreenPoint: TPointF): IControl; override;
    function GetScrollOffset: TPointF; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure CheckMouseLeftState;

  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure DoRealign; override;
    procedure DoInVisibleChange; override;

    procedure DoRealignContent; virtual;
    procedure DoSetDefaulatScrollBars; virtual;
    procedure DoUpdateHeaderFooter(const V: Single); virtual;
    procedure CreateContentView(); virtual;
    function IsAddToContent(const AObject: TFmxObject): Boolean; virtual;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure MousePosToAni(var X, Y: Single);

    procedure InitFooter(); virtual;
    procedure InitHeader(); virtual;
    procedure FreeHeader(); virtual;
    procedure FreeFooter(); virtual;
    procedure DoPullLoadComplete; virtual;
    procedure DoPullRefreshComplete; virtual;
    function CheckState(AState: TListViewState): Boolean;
    procedure DoUpdateState(AObject: IListViewHeader;
      const State: TListViewState; const ScrollValue: Double = 0);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // 刷新开始
    procedure PullRefreshStart(); override;
    // 刷新完成
    procedure PullRefreshComplete();
    // 加载更多完成
    procedure PullLoadComplete();

    // 内部控件数量
    property ContentControlsCount: Integer read GetContentChildCount;
    property ContentControls[const Index: Integer]: TControl read GetContentControlItem;
  published
    property HitTest default True;
    property Clickable default True;
    property CanFocus default True;
    property DragScroll;
    property ShowScrollBars;
    property ScrollbarWidth;
    property ScrollStretchGlowColor;
    property ScrollSmallChangeFraction;

    /// <summary>
    /// 是否启用拉动刷新
    /// </summary>
    property EnablePullRefresh: Boolean read FEnablePullRefresh write SetEnablePullRefresh default False;
    /// <summary>
    /// 是否启用拉动加载更多
    /// </summary>
    property EnablePullLoad: Boolean read FEnablePullLoad write SetEnablePullLoad default False;

    /// <summary>
    /// 加载 Footer 事件, 如果不设置，将在需要时加载默认的 Footer
    /// </summary>
    property OnInitFooter: TOnInitHeader read FOnInitFooter write FOnInitFooter;
    /// <summary>
    /// 加载 Header 事件, 如果不设置，将在需要时加载默认的 Header
    /// </summary>
    property OnInitHeader: TOnInitHeader read FOnInitHeader write FOnInitHeader;
    /// <summary>
    /// 下拉刷新事件
    /// </summary>
    property OnPullRefresh: TNotifyEvent read FOnPullRefresh write FOnPullRefresh;
    /// <summary>
    /// 上拉加载更多事件
    /// </summary>
    property OnPullLoad: TNotifyEvent read FOnPullLoad write FOnPullLoad;

    property OnScrollChange;
    property OnViewportPositionChange;
  end;

  /// <summary>
  /// 垂直滚动视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TVertScrollView = class(TPullScrollView)
  protected
    procedure DoSetDefaulatScrollBars; override;
  end;

type
  /// <summary>
  /// 水平滚动视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  THorzScrollView = class(TPullScrollView)
  protected
    procedure DoSetDefaulatScrollBars; override;
  end;

type
  /// <summary>
  /// 文本视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TTextView = class(TScrollView, ICaption{$IF CompilerVersion > 30.0}, IAcceleratorKeyReceiver{$ENDIF})
  private
    FText: UI.Base.TTextSettings;
    FHtmlText: TViewHtmlText;
    FTextHint: string;
    FDrawable: TDrawableIcon;
    FInFitSize: Boolean;
    FGroupIndex: Integer;

    FOnDrawText: TOnDrawText;
    FOnTextChange: TNotifyEvent;
    FOnLinkClick: TViewLinkClickEvent;

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
    function GetHtmlText: string;
    procedure SetHtmlText(const Value: string);
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
    procedure DoLinkClick(const Text, URL: string); override;
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

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure DoMouseLeave; override;
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
    property DragScroll;
    property Text: string read GetText write SetText stored TextStored;
    property TextHint: string read FTextHint write SetTextHint;
    property TextSettings: UI.Base.TTextSettings read FText write SetTextSettings;
    property HtmlText: string read GetHtmlText write SetHtmlText;
    property Drawable: TDrawableIcon read GetDrawable write SetDrawable;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ScrollBars;
    property ShowScrollBars;
    property DisableMouseWheel;
    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property OnDrawText: TOnDrawText read FOnDrawText write FOnDrawText;
    property OnLinkClick: TViewLinkClickEvent read FOnLinkClick write FOnLinkClick;
  end;

type
  TStyleView = class(TTextView)
  protected
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
  end;

type
  /// <summary>
  /// 按钮视图
  /// </summary>
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
    procedure SetColor(const Value: TAlphaColor);
  protected
    FColor: TAlphaColor;
    FColorChange: Boolean;
    function IsColorStored: Boolean;
  public
    constructor Create(AOwner: TComponent);
    function GetStateColor(const State: TViewState): TAlphaColor; override;
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
    FGravity: TLayoutGravity;
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
    procedure MarginsChanged(Sender: TObject);
    procedure SetGravity(const Value: TLayoutGravity);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DoRealign; override;
    procedure DoAdjustSize; virtual;
    function GetDefaultSize: TSizeF; override;
    procedure DoChanged(Sender: TObject); virtual;
    procedure DoTextChanged(Sender: TObject); virtual;
    procedure DoMatrixChanged(Sender: TObject); override;
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
    /// <summary>
    /// 重力。也就是组件的位于容器的位置。
    /// </summary>
    property Gravity: TLayoutGravity read FGravity write SetGravity default TLayoutGravity.RightTop;

    property Width;
    property Height;
    property Scale;
    property Size;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Margins;
    property Padding;
    property Enabled;
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

type
  TScanBufferEvent = procedure (Sender: TObject; ABitmap: TBitmap) of object;

  /// <summary>
  /// 摄像头图像显示器
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TCameraViewer = class(TView)
  private
    FActive: Boolean;
    FLocker: TCriticalSection;
    FBuffer: TBitmap;
    FViewportBuffer: TBitmap;

    FDrawBmp, FViewBmp: TBitmap;
    FTimer: TTimer;

    FVideoCamera: TVideoCaptureDevice;
    FOnScanBuffer: TScanBufferEvent;
    procedure DoScanBuffer(Sender: TObject; const ATime: TMediaTime);
    procedure SyncroniseBuffer;
    procedure InitDrawBmp();
    procedure SetActive(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure DoStop(); virtual;
    procedure DoRepaint(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartCapture;
    procedure StopCapture;
  published
    property Active: Boolean read FActive write SetActive;
    property OnScanBuffer: TScanBufferEvent read FOnScanBuffer write FOnScanBuffer;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  UI.Dialog,
{$ENDIF}
  UI.ListView.Header, UI.ListView.Footer;

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
  FreeAndNil(FHtmlText);
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

procedure TTextView.DoLinkClick(const Text, URL: string);
begin
  if Assigned(FOnLinkClick) then begin
    try
      FOnLinkClick(Self, Text, URL);
    except
    end;
  end;
end;

procedure TTextView.DoMouseLeave;
begin
  if Assigned(FHtmlText) then
    FHtmlText.MouseLeave(Self);
  inherited;
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
  LState: TViewState;
begin
  if InVisible then
    Exit;
  if Text = '' then
    FText.Draw(Canvas, FTextHint, R, GetAbsoluteOpacity, TViewState(8))
  else begin
    case FScrollbar of
      TViewScroll.Horizontal:
        begin
          if Assigned(FScrollH) and (FCanScrollH) then begin
            SR := GetRectF(FContentBounds^);
            SR.Top := R.Top;
            // windows平台显示滚动条，其它平台会自动隐藏
            if FShowScrollBars then
              SR.Bottom := R.Bottom{$IFDEF MSWINDOWS} - FScrollH.Height{$ENDIF}
            else
              SR.Bottom := R.Bottom;
            OffsetRect(SR, -(ScrollValueH * (SR.Width - R.Width)), 0);
          end else
            SR := R;
        end;
      TViewScroll.Vertical:
        begin
          if Assigned(FScrollV) and (FCanScrollV) then begin
            SR := GetRectF(FContentBounds^);
            SR.Left := R.Left;
            if FShowScrollBars then
              SR.Right := R.Right{$IFDEF MSWINDOWS} - FScrollV.Width{$ENDIF}
            else
              SR.Right := R.Right;
            OffsetRect(SR, 0, -(ScrollValueV * (SR.Height - R.Height)));
          end else
            SR := R;
        end;
      else
        SR := R;
    end;
    if Assigned(FOnDrawText) then
      FOnDrawText(Self, Canvas, FText, SR)
    else begin
      LState := DrawState;
      if (LState <> TViewState.None) and Checked and (FText.Color.GetColor(LState) = 0) then
        LState := TViewState.Checked;
      if Assigned(FHtmlText) then
        FHtmlText.Draw(Canvas, FText, SR, GetAbsoluteOpacity, LState)
      else
        FText.Draw(Canvas, SR, GetAbsoluteOpacity, LState);
    end;
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
    if Assigned(FHtmlText) and FText.WordWrap then begin
      if Scene.GetSceneScale >= 0 then
        FHtmlText.CalcTextSize(Canvas, TextSettings, RectF(0, 0, V, $FFFFFF), ASize)
      else
        Exit;
    end else
      if not FText.CalcTextObjectSize(Text, V, Scene.GetSceneScale, nil, ASize) then Exit;

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

function TTextView.GetHtmlText: string;
begin
  if not Assigned(FHtmlText) then
    Result := ''
  else
    Result := FHtmlText.HtmlText;
end;

function TTextView.GetNeedSize: TSizeF;
begin
  if not (csDestroying in ComponentState) and Assigned(Scene) then begin
    if not TextSettings.CalcTextObjectSize(Text, $FFFF, Scene.GetSceneScale, nil, Result) then
      Result := TSizeF.Create(0, 0)
  end else begin
    Result := TSizeF.Create(0, 0);
  end;
end;

function TTextView.GetText: string;
begin
  if Assigned(FHtmlText) then
    Result := FHtmlText.Text
  else
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

procedure TTextView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if Assigned(FHtmlText) then
    FHtmlText.MouseDown(Self, Button, Shift, X, Y);
  inherited;
end;

procedure TTextView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Assigned(FHtmlText) and (FHtmlText.HtmlText <> '') then
    FHtmlText.MouseMove(Self, X, Y);
end;

procedure TTextView.MouseUp(Button: TMouseButton; Shift: TShiftState; x,
  y: single);
begin
  inherited;
  if Assigned(FHtmlText) then
    FHtmlText.MouseUp(Self, Button, Shift, X, Y);
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

procedure TTextView.SetHtmlText(const Value: string);
begin
  if HtmlText <> Value then begin
    if not Assigned(FHtmlText) then begin
      FHtmlText := TViewHtmlText.Create(Value);
      FHtmlText.DefaultCursor := Cursor;
    end else
      FHtmlText.HtmlText := Value;
    FText.IsTextChange := True;
    DoChanged(FText);
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
  if Assigned(FHtmlText) then
    FreeAndNil(FHtmlText);
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
  if Assigned(FHtmlText) and (FHtmlText.HtmlText <> '') then
    Result := False
  else
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
  Padding.DefaultValue := RectF(4, 4, 4, 4);
  Padding.Rect := Padding.DefaultValue;
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
  NeedUpdatePosImmediately: Boolean;
begin
  NeedUpdatePosImmediately := False;

  if FCanScrollV or FDragScroll then begin
    NewViewPos := FAniCalculations.ViewportPosition.Y;
    MaxScrollViewPos := GetMaxScrollViewPos;

    if NewViewPos < 0 then
      UpdateScrollStretchStrength(NewViewPos)
    else if NewViewPos > MaxScrollViewPos then
      UpdateScrollStretchStrength(NewViewPos - MaxScrollViewPos)
    else
      UpdateScrollStretchStrength(0);

    if FLastViewportPosition.Y <> NewViewPos then
      NeedUpdatePosImmediately := (Round(NewViewPos) = 0) or
        ((MaxScrollViewPos > 0) and (Round(NewViewPos) = MaxScrollViewPos));
  end;

  if FCanScrollH and (not NeedUpdatePosImmediately) then begin
    NewViewPos := FAniCalculations.ViewportPosition.X;
    MaxScrollViewPos := Max(Round(FAniCalculations.MaxTarget.Point.X), 0);

    if FLastViewportPosition.X <> NewViewPos then
      NeedUpdatePosImmediately := (Round(NewViewPos) = 0) or
        ((MaxScrollViewPos > 0) and (Round(NewViewPos) = MaxScrollViewPos));
  end;

  if NeedUpdatePosImmediately then begin
    FAniCalculations.UpdatePosImmediately(True);
  end else begin
    if (not FCanScrollV) and (not FCanScrollH) then
      FAniCalculations.Shown := False;
  end;
end;

procedure TScrollView.AniMouseDown(const Touch: Boolean; const X, Y: Single);

//  function GetScrollPressed: Boolean;
//  var
//    FTrack: TCustomTrack;
//    FThumb: TThumb;
//  begin
//    Result := False;
//    FTrack := GetRttiObject(FScrollV, 'FTrack') as TCustomTrack;
//    if not Assigned(FTrack) then Exit;
//    FThumb := GetRttiObject(FTrack, 'FThumb') as TThumb;
//    Result := Assigned(FThumb) and (FThumb.Pressed);
//  end;

begin
//  if Assigned(FScrollV) then
//    FScrollTrackPressed := FScrollV.Pressed or GetScrollPressed
//  else
//    FScrollTrackPressed := False;
  FScrollStart := True;
  FAniCalculations.Averaging := Touch;
  FAniCalculations.MouseDown(X, Y);
end;

procedure TScrollView.AniMouseMove(const Touch: Boolean; const X, Y: Single);
var
  P: TPointD;
begin
  if FDragOneWay then begin
    // 只允许单向拖动时
    if FLastTouchTracking = [] then
      FLastTouchTracking := FAniCalculations.TouchTracking;
    // 开始滚动时，就决定好方向
    if FScrollStart then begin
      FScrollStart := False;
      P := FAniCalculations.DownPoint;
      if Abs(Y - P.Y)  > Abs(X - P.X) then
        FAniCalculations.TouchTracking := [ttVertical]
      else
        FAniCalculations.TouchTracking := [ttHorizontal];
      FAniCalculations.Shown := False;
    end;
  end;
  FAniCalculations.MouseMove(X, Y);
  if FAniCalculations.Moved then
    FAniCalculations.Shown := True;
end;

procedure TScrollView.AniMouseUp(const Touch: Boolean; const X, Y: Single);
begin
//  if FScrollTrackPressed then  // 更新一下按钮时的位置，不然会回弹
//    SetRttiValue<TPointD>(FAniCalculations, 'FDownPosition', ViewportPosition);
  FAniCalculations.MouseUp(X, Y);
  if (FAniCalculations.LowVelocity) or (not FAniCalculations.Animation) then
    FAniCalculations.Shown := False;
  if FDragOneWay then
    FAniCalculations.TouchTracking := FLastTouchTracking;
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

function TScrollView.CanInheritedCMGesture(const EventInfo: TGestureEventInfo): Boolean;
begin
  if CanVScroll and Assigned(FScrollV) then begin
    Result := (FScrollV.Value <= 0) or (ScrollValueV >= 1);
//    Text := Format('GestureID: %d'#13'Angle: %.2f'#13'Distance: %d'#13+
//      'FScrollV.Value: %.2f, Max: %.2f'#13'Location: %.2f, %.2f'#13'Result: %s',
//      [Ord(EventInfo.GestureID),
//        EventInfo.Angle, EventInfo.Distance,
//        FScrollV.Value, FScrollV.Max,
//        EventInfo.Location.X, EventInfo.Location.Y,
//        BoolToStr(Result)]);
  end else if CanHScroll and Assigned(FScrollH) then begin
    Result := (FScrollH.Value <= 0) or (ScrollValueH >= 0.999);
  end else
    Result := True;
end;

procedure TScrollView.CMGesture(var EventInfo: TGestureEventInfo);
var
  LP: TPointF;
begin
  if (FCanScrollV or FCanScrollH {$IFNDEF NEXTGEN} or FDragScroll{$ENDIF}) and ((EventInfo.GestureID = igiPan)) then
  begin
    if FInVisible or (FAniCalculations = nil) then
      Exit;
    FMouseEvents := False;
    LP := AbsoluteToLocal(EventInfo.Location);
    if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then begin
      AniMouseDown(True, LP.X, LP.Y)
    end else begin
      if EventInfo.Flags = [] then begin
        AniMouseMove(True, LP.X, LP.Y);
      end else if AniCalculations.Down then begin
        AniMouseUp(True, LP.X, LP.Y);
      end;
    end;
    if CanInheritedCMGesture(EventInfo) then
      inherited CMGesture(EventInfo); // 向上级传递
  end else
    inherited CMGesture(EventInfo); // 向上级传递
end;

procedure TScrollView.ContentAddObject(const AObject: TFmxObject);
begin
  if csDestroying in ComponentState then
    Exit;
  if FDisableAlign then
    Exit;
  RealignContent;
end;

procedure TScrollView.ContentBeforeRemoveObject(AObject: TFmxObject);
begin
end;

procedure TScrollView.ContentInsertObject(Index: Integer;
  const AObject: TFmxObject);
begin
  if csDestroying in ComponentState then
    Exit;
  if FDisableAlign then
    Exit;
  RealignContent;
end;

procedure TScrollView.ContentRemoveObject(const AObject: TFmxObject);
begin
  if csDestroying in ComponentState then
    Exit;
  if FDisableAlign then
    Exit;
  if IsUpdating then
    Exit;
  RealignContent;
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
  Result.OnStart := AniCalcChange;
  Result.OnStop := AniCalcChange;
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
  if Assigned(FAniCalculations) then
  begin
    FAniCalculations.MouseLeave;
    if FMouseEvents and FAniCalculations.Down and ((FAniCalculations.LowVelocity) or (not FAniCalculations.Animation)) then
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
  if not FDisableAlign then
    Realign
  else begin
    if Assigned(FScrollV) and FScrollV.Visible then
      FScrollV.SetBounds(Width - FScrollV.Width - Padding.Left - Padding.Right,
        0, FScrollV.Width, Height - Padding.Top - Padding.Bottom);
    if Assigned(FScrollH) and FScrollH.Visible then
      FScrollH.SetBounds(0, Height - FScrollH.Height - Padding.Top - Padding.Bottom,
        Width - Padding.Left - Padding.Right, FScrollH.Height);
  end;
end;

procedure TScrollView.DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations);
begin
  {$IFNDEF NEXTGEN}
  if FDragScroll then begin
    FCanAnimation := True;
    AAniCalculations.Animation := FCanAnimation;
    AAniCalculations.TouchTracking := [ttVertical, ttHorizontal];
  end else
  {$ENDIF}
  begin
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
        Targets[1].Point := TPointD.Create(Padding.Left + Padding.Right + Max(FContentBounds.Width - ViewRect.Width, 0), 0);
      TViewScroll.Vertical:
        Targets[1].Point := TPointD.Create(0, Padding.Top + Padding.Bottom + Max(FContentBounds.Height - ViewRect.Height, 0));
    else
      Targets[1].Point := TPointD.Create(
        Padding.Left + Padding.Right + Max(FContentBounds.Width - ViewRect.Width, 0),
        Padding.Top + Padding.Bottom + Max(FContentBounds.Height - ViewRect.Height, 0)
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
    FScrollV.OnChange := nil;
    RemoveComponent(FScrollV);
    FreeAndNil(FScrollV);
  end;
  if Assigned(FScrollH) then begin
    FScrollH.OnChange := nil;
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
  if (FAniCalculations <> nil) and Assigned(FScrollH) and (FCanScrollH{$IFNDEF NEXTGEN} or FDragScroll{$ENDIF}) then
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

function TScrollView.GetScrollOffset: TPointF;
begin
  Result := PointF(0, 0);
end;

function TScrollView.GetScrollSmallChangeFraction: Single;
begin
  Result := FScrollSmallChangeFraction;
end;

function TScrollView.GetScrollValueH: Single;
var
  V: Double;
begin
  V := FScrollH.MaxD - FScrollH.MinD - FScrollH.ViewportSizeD;
  if V <> 0 then
    Result := (FScrollH.ValueD - FScrollH.MinD) / V
  else
    Result := 0;
end;

function TScrollView.GetScrollValueV: Single;
var
  V: Double;
begin
  V := FScrollV.MaxD - FScrollV.MinD - FScrollV.ViewportSizeD;
  if V <> 0 then
    Result := (FScrollV.ValueD - FScrollV.MinD) / V
  else
    Result := 0;
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
  if (FAniCalculations <> nil) and Assigned(FScrollV) and (FCanScrollV{$IFNDEF NEXTGEN} or FDragScroll{$ENDIF}) then begin
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
  NeedRePaint, UV, UH: Boolean;
  LViewportPosition: TPointD;
  ContentLayoutRect: TRectD;
  NewTargets: array of TAniCalculations.TTarget;
begin
  if (not FInInternalAlign) and (FAniCalculations <> nil) then
  begin
    NeedRePaint := True;
    FInInternalAlign := True;
    try
      {$IF CompilerVersion < 32}
      if not Released then begin
      {$ELSE}
      if Assigned(Self) then begin  // Tokyo 弃用 Released ，永远返回 False
      {$ENDIF}
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
      UV := FLastViewportPosition.Y <> LViewportPosition.Y;
      UH := FLastViewportPosition.X <> LViewportPosition.X;
      if (UV = False) and (UH = False) then begin
        NeedRePaint := False;
        Exit;
      end;
      if UV and Assigned(FScrollV) then begin
        UpdateVScrollBar(LViewportPosition.Y, Height - Padding.Top - Padding.Bottom);
        if (LViewportPosition.Y = 0) or (LViewportPosition.Y >= GetMaxScrollViewPos) then
          VScrollChange(FScrollV);
      end;
      if UH then
        UpdateHScrollBar(LViewportPosition.X, Width - Padding.Left - Padding.Right);

      if FLastViewportPosition <> LViewportPosition then
        try
          ViewportPositionChange(FLastViewportPosition, LViewportPosition, UV or UH);
        finally
          FLastViewportPosition := LViewportPosition;
        end;

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
      if NeedRePaint then
        Repaint;
      FInInternalAlign := False;
    end;
  end;
end;

procedure TScrollView.InvalidateContentSize;
begin
end;

function TScrollView.IsCanTouch: Boolean;
begin
  Result := HitTest and (FCanScrollV or FCanScrollH or FDragScroll);
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
  inherited MouseDown(Button, Shift, X, Y);
  if Assigned(FAniCalculations) and (Button = TMouseButton.mbLeft){$IFNDEF NEXTGEN} and (FDragScroll){$ENDIF} then
  begin
    AniMouseDown(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if Assigned(FAniCalculations) and FAniCalculations.Down{$IFNDEF NEXTGEN} and (FDragScroll){$ENDIF} then
  begin
    AniMouseMove(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if Assigned(FAniCalculations) and (Button = TMouseButton.mbLeft){$IFNDEF NEXTGEN} and (FDragScroll){$ENDIF} then
  begin
    AniMouseUp(ssTouch in Shift, X, Y);
  end;
end;

procedure TScrollView.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  Offset, ANewPos: Single;
begin
  inherited;
  if (not (Handled or FDisableMouseWheel)) and (not FInVisible) and (Assigned(FAniCalculations)) then begin
    if (FCanScrollV and (not (ssShift in Shift))) then begin
      FAniCalculations.UpdatePosImmediately(True);
      if VScrollBar <> nil then
        Offset := VScrollBar.SmallChange
      else
        Offset := FContentBounds.Height / 14;
      Offset := Offset * -1 * (WheelDelta / 120);
      ANewPos := Max(VScrollBarValue + Offset, 0);
      ANewPos := Min(ANewPos, (FAniCalculations.MaxTarget.Point.Y));
      if VScrollBar <> nil then
        VScrollBar.ValueD := Floor(ANewPos);
      Handled := True;
    end else if FCanScrollH then begin
      FAniCalculations.UpdatePosImmediately(True);
      if HScrollBar <> nil then
        Offset := HScrollBar.SmallChange
      else
        Offset := FContentBounds.Width / 14;
      Offset := Offset * -1 * (WheelDelta / 120);
      ANewPos := Max(HScrollBarValue + Offset, 0);
      ANewPos := Min(ANewPos, (FAniCalculations.MaxTarget.Point.X));
      if HScrollBar <> nil then
        HScrollBar.ValueD := Floor(ANewPos);
      Handled := True;
    end;
    FAniCalculations.Shown := False;
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

procedure TScrollView.PullRefreshStart;
begin
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

procedure TScrollView.SetDragOneWay(const Value: Boolean);
begin
  FDragOneWay := Value;
end;

procedure TScrollView.SetDragScroll(const Value: Boolean);
begin
  if FDragScroll <> Value then begin
    FDragScroll := Value;
    {$IFNDEF NEXTGEN}
    if not (csDesigning in ComponentState) then begin
      if FScrollbar = TViewScroll.None then
        FreeScrollbar
      else
        InitScrollbar;
      RealignContent;
    end;
    {$ENDIF}
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
  if (FAniCalculations <> nil) and Assigned(FScrollV) and (FCanScrollV{$IFNDEF NEXTGEN} or FDragScroll{$ENDIF}) then begin
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
    if (Assigned(AScroll)) then begin
      if (AScroll.Visible <> FCanScrollV) or (AScroll.Visible and (FShowScrollBars = False)) then begin
        AScroll.Visible := FCanScrollV and FShowScrollBars and (not FInVisible);
        DoScrollVisibleChange;
      end;
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
    if (Assigned(AScroll)) then begin
      if (AScroll.Visible <> FCanScrollH) or (AScroll.Visible and (FShowScrollBars = False)) then begin
        AScroll.Visible := FCanScrollH and FShowScrollBars and (not FInVisible);
        DoScrollVisibleChange;
      end;
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
    {$IFNDEF NEXTGEN}
    if not DragScroll then begin
      AScroll.Width := 16;
      AScroll.Height := 16;
    end;
    {$ELSE}
    AScroll.Width := 6;
    AScroll.Height := 6;
    {$ENDIF}
  end;
end;

procedure TScrollView.ViewportPositionChange(const OldViewportPosition,
  NewViewportPosition: TPointD; const ContentSizeChanged: boolean);
begin
  if Assigned(FOnViewportPositionChange) then
    FOnViewportPositionChange(Self, OldViewportPosition, NewViewportPosition, ContentSizeChanged);
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
    W, PW: Single;
  begin
    inherited PaintBackground;
    if (FMax - FMin <= 0) or (FValue - FMin < 0) then
      Exit;
    PW := FForeGround.Padding.Left + FForeGround.Padding.Right + FForeGround.FValueOffset;
    if FValue > FMax then
      W := FMax
    else if FValue < FMin then
      W := FMin
    else
      W := FValue;
    W := (W - FMin) / (FMax - FMin) * (Width - PW) + PW;
    FForeGround.DrawTo(Canvas, RectF(0, 0, W, Height));
  end;

  procedure DoDrawVertical;
  var
    H, V, PH: Single;
  begin
    inherited PaintBackground;
    if (FMax - FMin <= 0) or (FValue - FMin < 0) then
      Exit;
    PH := FForeGround.Padding.Top + FForeGround.Padding.Bottom + FForeGround.FValueOffset;
    V := Height - PH;
    if FValue > FMax then
      H := FMax
    else if FValue < FMin then
      H := FMin
    else
      H := FValue;
    H := V - (H - FMin) / (FMax - FMin) * V;
    FForeGround.DrawTo(Canvas, RectF(0, H, Width, Height));
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
  if IsChecked then
    Img := FImage.GetStateItem(TViewState.Checked)
  else
    Img := nil;
  if FImage.BrushIsEmpty(Img) then
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
  Margins.OnChange := MarginsChanged;
  FGravity := TLayoutGravity.RightTop;
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
  if Parent is TControl then
    P := P - TControl(Parent).LocalToAbsolute(TPointF.Zero);
  case FGravity of
    TLayoutGravity.None: ;
    TLayoutGravity.LeftTop:
      Position.Point := PointF(
        P.X - Width * 0.35 + Margins.Left,
        P.Y - Height * 0.35 + Margins.Top);
    TLayoutGravity.LeftBottom:
      Position.Point := PointF(
        P.X - Width * 0.35 + Margins.Left,
        P.Y + FTargetView.Height - Height * 0.65 + Margins.Top);
    TLayoutGravity.RightTop:
      Position.Point := PointF(
        P.X + FTargetView.Width - Width * 0.65 + Margins.Left,
        P.Y - Height * 0.35 + Margins.Top);
    TLayoutGravity.RightBottom:
      Position.Point := PointF(
        P.X + FTargetView.Width - Width * 0.65 + Margins.Left,
        P.Y + FTargetView.Height - Height * 0.65 + Margins.Top);
    TLayoutGravity.CenterVertical:
      Position.Point := PointF(
        P.X - Width * 0.35 + Margins.Left,
        P.Y + (FTargetView.Height - Height) / 2 + Margins.Top);
    TLayoutGravity.CenterHorizontal:
      Position.Point := PointF(
        P.X + (FTargetView.Width - Width) / 2 + Margins.Left,
        P.Y - Height * 0.35 + Margins.Top);
    TLayoutGravity.CenterHBottom:
      Position.Point := PointF(
        P.X + (FTargetView.Width - Width) / 2 + Margins.Left,
        P.Y + FTargetView.Height - Height * 0.65 + Margins.Top);
    TLayoutGravity.CenterVRight:
      Position.Point := PointF(
        P.X + FTargetView.Width - Width * 0.65 + Margins.Left,
        P.Y + (FTargetView.Height - Height) / 2 + Margins.Top);
    TLayoutGravity.Center:
      Position.Point := PointF(
        P.X + (FTargetView.Width - Width) / 2 + Margins.Left,
        P.Y + (FTargetView.Height - Height) / 2 + Margins.Top);
  end;
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

procedure TBadgeView.MarginsChanged(Sender: TObject);
begin
  DoRealign;
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
  LOpacity: Single;
begin
  if IsVisibleView then begin
    if Assigned(FTargetView) then
      LOpacity := FTargetView.Opacity
    else
      LOpacity := Opacity;
    R := RectF(0, 0, Width, Height);
    if FStyle = TBadgeStyle.Icon then begin
      if Assigned(FIcon) then begin
        with FBackground do begin
          Canvas.FillRect(R, FXRadius, FYRadius, FCorners, LOpacity, FIcon);
        end;
      end;
    end else begin
      if Assigned(FBackground) then
        with FBackground do begin
          Canvas.Fill.Color := FColor;
          Canvas.FillRect(R, FXRadius, FYRadius, FCorners, LOpacity);
        end;
      if Assigned(FText) and (FText.TextLength > 0) then
        FText.Draw(Canvas, R, LOpacity, TViewState.None);
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

procedure TBadgeView.SetGravity(const Value: TLayoutGravity);
begin
  if FGravity <> Value then begin
    FGravity := Value;
    DoRealign;
    Repaint;
  end;
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
  IsColorChange := False;
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
  if FColor <> Value then begin
    FColor := Value;
    FColorChange := True;
    DoColorChanged(Self);
  end;
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

  if Assigned(FBackground) then begin
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
  if Assigned(FBackground) then
    R := RectF(FBackground.Padding.Left, FBackground.Padding.Top,
      Width - FBackground.Padding.Right, Height - FBackground.Padding.Bottom)
  else
    R := RectF(0, 0, Width, Height);

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
      if Assigned(FBackground) then
        FPath.AddRectangle(R, FBackground.XRadius, FBackground.YRadius,
          FBackground.Corners, FBackground.CornerType)
      else
        FPath.AddRectangle(R, 0, 0, AllCorners, TCornerType.Round);
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
        if Assigned(FBackground) then
          FPath.AddRectangle(R, FBackground.XRadius, FBackground.YRadius,
            FBackground.Corners, FBackground.CornerType)
        else
          FPath.AddRectangle(R, 0, 0, AllCorners, TCornerType.Round);
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

procedure TDrawableProgress.Assign(Source: TPersistent);
begin
  if Source is TDrawableProgress then begin
    FWidth := TDrawableProgress(Source).FWidth;
    FValueOffset := TDrawableProgress(Source).FValueOffset;
  end;
  inherited Assign(Source);
end;

procedure TDrawableProgress.InitDrawable;
begin
  FWidth := 8;
  FValueOffset := 0;
end;

function TDrawableProgress.IsStoredWidth: Boolean;
begin
  Result := FWidth <> 8;
end;

function TDrawableProgress.IsStoreValueOffset: Boolean;
begin
  Result := FValueOffset <> 0;
end;

procedure TDrawableProgress.SetValueOffset(const Value: Single);
begin
  if FValueOffset <> Value then begin
    FValueOffset := Value;
    DoChange(Self);
  end;
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

{ TImageViewerEx }

procedure TImageViewerEx.AniCalcChange(Sender: TObject);
begin
  InvalidateRect(ClipRect);
end;

procedure TImageViewerEx.AniCalcStart(Sender: TObject);
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(Self, True);
end;

procedure TImageViewerEx.AniCalcStop(Sender: TObject);
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(nil, False);
end;

procedure TImageViewerEx.CMGesture(var EventInfo: TGestureEventInfo);
var
  APercent: integer;
  ANewZoom: integer;
begin
  inherited;

  if EventInfo.GestureID = igiZoom then begin
    if FStartDistance = 0 then
      APercent := 100
    else
      APercent := Round((EventInfo.Distance / FStartDistance) * 100);

    ANewZoom := Round(FStartZoom * (APercent / 100));

    if Max(FZoom, ANewZoom) - Min(FZoom, ANewZoom) > 10 then
    begin
      FStartZoom := FZoom;
      FStartDistance := 0;
      Exit;
    end;

    Zoom := ANewZoom;
    FStartZoom := Zoom;
    FStartDistance := EventInfo.Distance;
  end;
end;

constructor TImageViewerEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;

  FAniCalc := TAniCalculations.Create(nil);
  FAniCalc.ViewportPositionF := PointF(0, 0);
  FAniCalc.Animation := True;
  FAniCalc.Averaging := True;
  FAniCalc.Interval := 8;
  FAniCalc.BoundsAnimation := True;
  FAniCalc.TouchTracking := [ttHorizontal, ttVertical];
  FAniCalc.OnChanged := AniCalcChange;
  FAniCalc.OnStart := AniCalcStart;
  FAniCalc.OnStop := AniCalcStop;
  FZoom := 100;
  FMaxXPos := 0;
  FMaxYPos := 0;
  Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan];

  Clickable := True;
  CanFocus := True;
end;

destructor TImageViewerEx.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FAniCalc);
  inherited Destroy;
end;

procedure TImageViewerEx.DoMouseLeave;
begin
  inherited;
  if (FAniCalc <> nil) then
    FAniCalc.MouseLeave;

  FStartDistance := 0;
  FStartZoom := 0;
end;

procedure TImageViewerEx.MouseDown(Button: TMouseButton; Shift: TShiftState; x,
  y: single);
begin
  inherited;
  FAniCalc.MouseDown(x, y);
end;

procedure TImageViewerEx.MouseMove(Shift: TShiftState; x, y: single);
begin
  inherited;
  FAniCalc.MouseMove(x, y);
end;

procedure TImageViewerEx.MouseUp(Button: TMouseButton; Shift: TShiftState; x,
  y: single);
begin
  inherited;
  FAniCalc.MouseUp(x, y);
  FStartZoom := 0;
  FStartDistance := 0;
  UpdateScrollLimits;
end;

procedure TImageViewerEx.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var
  Handled: Boolean);
begin
  if not (DisableMouseWheel or Handled or FStretch) then begin
    Zoom := Zoom + Trunc((WheelDelta / 120) * 4);
    Handled := True;
  end;
  inherited;
end;

procedure TImageViewerEx.PaintBackground;
var
  ASourceRect: TRectF;
  ADestRect: TRectF;
begin
  inherited PaintBackground;

  if Assigned(FBitmap) and (AbsoluteInVisible = False) then begin
    ASourceRect := RectF(0, 0, FBitmap.Width, FBitmap.Height);

    if FStretch then begin
      ADestRect := RectF(Padding.Left, Padding.Top, Width - Padding.Right, Height - Padding.Bottom);

      OffsetRect(ADestRect, 0 - FAniCalc.ViewportPosition.X, 0 - FAniCalc.ViewportPosition.Y);

    end else begin
      ADestRect := ASourceRect;
      ADestRect.Width := (FBitmap.Width / 100) * FZoom;
      ADestRect.Height := (FBitmap.Height / 100) * FZoom;

      OffsetRect(ADestRect, 0 - FAniCalc.ViewportPosition.X, 0 - FAniCalc.ViewportPosition.Y);

      if ADestRect.Width < Width then
        OffsetRect(ADestRect, (Width - ADestRect.Width) * 0.5, 0);

      if ADestRect.Height < Height then
        OffsetRect(ADestRect, 0, (Height - ADestRect.Height)  * 0.5);
    end;

    Canvas.DrawBitmap(FBitmap, ASourceRect, ADestRect, 1, True);
  end;
end;

procedure TImageViewerEx.Resize;
begin
  inherited;
  UpdateScrollLimits;
end;

procedure TImageViewerEx.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  UpdateScrollLimits;
end;

procedure TImageViewerEx.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then begin
    FStretch := Value;
    UpdateScrollLimits;
  end;
end;

procedure TImageViewerEx.SetZoom(const Value: integer);
var
  xpercent, ypercent: single;
begin
  if (Value > 10) and (Value < 200) then begin
    if FZoom <> Value then begin
      FZoom := Value;

      FAniCalc.UpdatePosImmediately;
      FAniCalc.MouseLeave;

      if FMaxXPos = 0 then
        XPercent := 0
      else
        xpercent := (FAniCalc.ViewportPositionF.X / FMaxXPos) * 100;

      if FMaxYPos = 0 then
        ypercent := 0
      else
        ypercent := (FAniCalc.ViewportPositionF.Y / FMaxYPos) * 100;

      UpdateScrollLimits;

      FAniCalc.ViewportPositionF := PointF((FMaxXPos / 100) * xpercent, (FMaxYPos / 100) * ypercent);

      InvalidateRect(ClipRect);
      if Assigned(FOnZoom) then
        FOnZoom(Self);
    end;
  end;
end;

procedure TImageViewerEx.UpdateLabel(ADistance: integer);
begin
  //
end;

procedure TImageViewerEx.UpdateScrollLimits;
var
  Targets: array of TAniCalculations.TTarget;
  w, h: single;
begin
  if FAniCalc <> nil then begin

    if FStretch then begin
      W := 0;
      H := 0;
    end else begin
      w := (FBitmap.Width / 100) * FZoom;
      h := (FBitmap.Height / 100) * FZoom;
      w := w - Width;
      h := h - Height;
    end;

    SetLength(Targets, 2);
    Targets[0].TargetType := TAniCalculations.TTargetType.Min;
    Targets[0].Point := TPointD.Create(0, 0);

    Targets[1].TargetType := TAniCalculations.TTargetType.Max;
    Targets[1].Point := TPointD.Create(Max(0,w), Max(0, h));
    FAniCalc.SetTargets(Targets);

    FMaxXPos := Targets[1].Point.X;
    FMaxYPos := Targets[1].Point.Y;
  end;
end;

{ TCameraViewer }

constructor TCameraViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  ClipChildren := True;
  FBuffer := TBitmap.Create;
  FViewportBuffer := TBitmap.Create;

  FLocker := TCriticalSection.Create;
  FDrawBmp := TBitmap.Create;
  FViewBmp := TBitmap.Create;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 40;
  FTimer.OnTimer := DoRepaint;
end;

destructor TCameraViewer.Destroy;
begin
  FActive := False;
  SyncroniseBuffer;
  FTimer.OnTimer := nil;
  FTimer := nil;
  FreeAndNil(FDrawBmp);
  FreeAndNil(FBuffer);
  FreeAndNil(FViewBmp);
  FreeAndNil(FViewportBuffer);
  FreeAndNil(FLocker);
  inherited;
end;

procedure TCameraViewer.DoRepaint(Sender: TObject);
begin
  if Assigned(FOnScanBuffer) then
    FOnScanBuffer(Self, FBuffer);
  Repaint;
end;

procedure TCameraViewer.DoScanBuffer(Sender: TObject; const ATime: TMediaTime);
begin
  SyncroniseBuffer;
end;

procedure TCameraViewer.DoStop;
begin
  if FVideoCamera <> nil then begin
    FVideoCamera.StopCapture;
    FVideoCamera.OnSampleBufferReady := nil;
    FVideoCamera := nil;
  end;
end;

procedure TCameraViewer.InitDrawBmp;
var
  F: TCustomForm;
begin
  F := ParentForm;
  if Assigned(F) then
    FDrawBmp.SetSize(F.Width, F.Height);
end;

procedure TCameraViewer.Paint;
var
  R, LR: TRectF;
  NeedDraw: Boolean;
begin
  inherited Paint;
  LR := RectF(0, 0, Width, Height);
  R := LR;
  FViewBmp.SetSize(Round(Width), Round(Height));

  FLocker.Enter;
  NeedDraw := FDrawBmp.Width > 0;
  if NeedDraw then begin
    OffsetRect(r, (FDrawBmp.Width - r.Width) / 2, (FDrawBmp.Height - r.Height) / 2);
    FViewBmp.CopyFromBitmap(FDrawBmp, r.Truncate, 0, 0);
  end;
  FLocker.Leave;

  if NeedDraw then
    Canvas.DrawBitmap(FViewBmp, RectF(0, 0, FViewBmp.Width, FViewBmp.Height), LR, 1, True);
end;

procedure TCameraViewer.SetActive(const Value: Boolean);
begin
  FActive := Value;
  case FActive of
    True: StartCapture;
    False: StopCapture;
  end;
end;

procedure TCameraViewer.StartCapture;
begin
  if not Assigned(FVideoCamera) then
    FVideoCamera := TVideoCaptureDevice(TCaptureDeviceManager.Current.GetDefaultDeviceByMediaType(TMediaType.Video));
  if Assigned(FVideoCamera) then begin
    FVideoCamera.Quality := TVideoCaptureQuality.MediumQuality;
    {$IFDEF ANDROID}
    FVideoCamera.FocusMode := TFocusMode.ContinuousAutoFocus;
    {$ENDIF}
    FVideoCamera.OnSampleBufferReady := DoScanBuffer;

    FBuffer.Clear(TAlphaColors.Null);
    FActive := True;

    FVideoCamera.StartCapture;
    {$IFDEF ANDROID}
    FVideoCamera.FocusMode := TFocusMode.ContinuousAutoFocus;
    {$ENDIF}
  end else begin
    if csDesigning in ComponentState then
      FActive := True;
  end;
end;

procedure TCameraViewer.StopCapture;
begin
  FActive := False;
end;

procedure TCameraViewer.SyncroniseBuffer;
begin
  if FActive = False then begin
    TThread.Synchronize(TThread.CurrentThread, DoStop);
  end else begin
    FVideoCamera.SampleBufferToBitmap(FBuffer, True);

    if FBuffer.Width > 0 then begin

      FLocker.Enter;
      try
        if (FDrawBmp.Width = 0) then begin
          InitDrawBmp;
          if (FDrawBmp.Width = 0) then
            Exit;
        end;
        FDrawBmp.Canvas.BeginScene;
        FDrawBmp.Canvas.DrawBitmap(FBuffer, RectF(0, 0, FBuffer.Width, FBuffer.Height),
          RectF(0, 0, FDrawBmp.Width, FDrawBmp.Height), 1, True);
        FDrawBmp.Canvas.EndScene;
      finally
        FLocker.Leave;
      end;

    end;
  end;
end;

{ TPullScrollView }

function TPullScrollView.AllowInitScrollbar: Boolean;
begin
  Result := True;
end;

procedure TPullScrollView.AniMouseUp(const Touch: Boolean; const X, Y: Single);
begin
  inherited AniMouseUp(Touch, X, Y);

  // 下拉刷新处理
  if FEnablePullRefresh and (CheckState(TListViewState.PullDownOK)
    or CheckState(TListViewState.PullRightOK)) then begin
    if Assigned(FHeader) and Assigned(FOnPullRefresh) then begin
      case FContent.Orientation of
        TOrientation.Horizontal: begin
          DoUpdateState(FHeader, TListViewState.PullRightFinish, 0);
          InvalidateContentSize;
          DoUpdateScrollingLimits(True, 0);

          FOffsetScroll := (FHeader as TControl).Width;
        end;
        TOrientation.Vertical: begin
          DoUpdateState(FHeader, TListViewState.PullDownFinish, 0);
          InvalidateContentSize;
          DoUpdateScrollingLimits(True, 0);

          FOffsetScroll := (FHeader as TControl).Height;
        end;
      end;

      FOnPullRefresh(Self);
      Exit;
    end else
      DoPullRefreshComplete;
  end;

  // 上拉加载更多
  if FEnablePullLoad then begin
    if CheckState(TListViewState.PullUpOK) or CheckState(TListViewState.PullLeftOK) then
      DoPullLoad(Self);
  end;
end;

procedure TPullScrollView.CheckMouseLeftState;
begin
  {$IFNDEF NEXTGEN}
  // 检查鼠标左键是否松开
  if Assigned(Self) and DragScroll and (not FMouseEnter) then begin
    {$IFDEF MSWINDOWS}
    if GetAsyncKeyState(VK_LBUTTON) = 0 then
      MouseUp(TMouseButton.mbLeft, [], FMovePos.X, FMovePos.Y)
    else if (not (csDestroying in ComponentState)) then
      TFrameAnimator.DelayExecute(Self,
        procedure(Sender: TObject)
        begin
          if (not (csDestroying in ComponentState)) then
            CheckMouseLeftState;
        end,
      0.05);
    {$ENDIF}
  end;
  {$ENDIF}
end;

function TPullScrollView.CheckState(AState: TListViewState): Boolean;
begin
  Result := FState = AState;
  if not Result then
    Exit;

  case FState of
    TListViewState.PullDownStart, TListViewState.PullDownOK,
    TListViewState.PullDownFinish, TListViewState.PullDownComplete,
    TListViewState.PullUpStart, TListViewState.PullUpOK,
    TListViewState.PullUpFinish, TListViewState.PullUpComplete:
      Result := Assigned(FContent) and (FContent.Orientation = TOrientation.Vertical);
    TListViewState.PullLeftStart, TListViewState.PullLeftOK,
    TListViewState.PullLeftFinish, TListViewState.PullLeftComplete,
    TListViewState.PullRightStart, TListViewState.PullRightOK,
    TListViewState.PullRightFinish, TListViewState.PullRightComplete:
      Result := Assigned(FContent) and (FContent.Orientation = TOrientation.Horizontal);
  end;
end;

procedure TPullScrollView.Click;
begin
  inherited Click;
  {$IFNDEF NEXTGEN}
  if DragScroll and Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then
    TPullScrollView(FPointTarget as TControl).Click;
  {$ENDIF}
end;

constructor TPullScrollView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  New(FContentBounds);
  DoSetDefaulatScrollBars;
  CreateContentView();
  DisableFocusEffect := True;
  AutoCapture := True;
  ClipChildren := True;
  CanFocus := True;
  HitTest := True;
  SetAcceptsControls(True);
end;

procedure TPullScrollView.CreateContentView;
var
  Item: TComponent;
begin
  Item := Self.FindComponent(CSContentName);
  if Assigned(Item) then
    FContent := Item as TViewScrollContent
  else begin
    FContent := TViewScrollContent.Create(Self);
    FContent.Visible := True;
    FContent.Stored := False;
    FContent.Locked := True;
    FContent.Name := CSContentName;
    case ScrollBars of
      TViewScroll.Horizontal: begin
        FContent.WidthSize := TViewSize.WrapContent;
        FContent.HeightSize := TViewSize.FillParent;
        FContent.Orientation := TOrientation.Horizontal;
      end;
      TViewScroll.Vertical: begin
        FContent.WidthSize := TViewSize.FillParent;
        FContent.HeightSize := TViewSize.WrapContent;
        FContent.Orientation := TOrientation.Vertical;
      end;
    end;
    FContent.DisableDisappear := True;
    FContent.Parent := Self;
  end;
  SetAcceptsControls(True);
  RealignContent;
end;

function TPullScrollView.CreateScroll: TScrollBar;
begin
  {$IFNDEF NEXTGEN}
  if DragScroll then
    Result := TSmallScrollBar.Create(Self)
  else
    Result := TScrollBar.Create(Self);
  {$ELSE}
  Result := TSmallScrollBar.Create(Self);
  {$ENDIF}
end;

destructor TPullScrollView.Destroy;
begin
  FContent := nil;
  FreeHeader;
  FreeFooter;
  inherited Destroy;
end;

procedure TPullScrollView.DoAddObject(const AObject: TFmxObject);
begin
  if IsAddToContent(AObject) then begin
    FContent.AddObject(AObject);
  end else
    inherited;
end;

procedure TPullScrollView.DoInVisibleChange;
begin
  inherited DoInVisibleChange;
  FContent.InVisible := InVisible;
end;

procedure TPullScrollView.DoMouseEnter;
begin
  inherited;
  {$IFNDEF NEXTGEN}
  FMouseEnter := True;
  if DragScroll and Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then
    FPointTarget.DoMouseEnter;
  {$ENDIF}
end;

procedure TPullScrollView.DoMouseLeave;
begin
  inherited;
  {$IFNDEF NEXTGEN}
  FMouseEnter := False;
  if DragScroll and Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then
    FPointTarget.DoMouseLeave;
  if not IsPressed then
    CheckMouseLeftState;
  {$ENDIF}
end;

procedure TPullScrollView.DoPullLoad(Sender: TObject);
begin
  if Assigned(FFooter) and Assigned(FOnPullLoad) then begin
    case FContent.Orientation of
      TOrientation.Horizontal: FState := TListViewState.PullLeftFinish;
      TOrientation.Vertical: FState := TListViewState.PullUpFinish;
    end;
    DoUpdateState(FFooter, FState, 0);
    FOnPullLoad(Self);
    InvalidateContentSize;
    DoUpdateScrollingLimits(True, 0);
  end else
    DoPullLoadComplete;
end;

procedure TPullScrollView.DoPullLoadComplete;
begin
  if Assigned(FFooter) then begin
    case FContent.Orientation of
      TOrientation.Horizontal: begin
        if FState = TListViewState.PullLeftComplete then
          Exit;
        FFooter.DoUpdateState(TListViewState.PullLeftComplete, 0);
      end;
      TOrientation.Vertical: begin
        if FState = TListViewState.PullUpComplete then
          Exit;
        FFooter.DoUpdateState(TListViewState.PullUpComplete, 0);
      end;
    end;

    // 加载完成，回弹
    TFrameAnimator.DelayExecute(Self,
      procedure (Sender: TObject)
      var
        W, H: Single;
      begin
        try
          if CheckState(TListViewState.PullUpFinish) or CheckState(TListViewState.PullLeftFinish) then begin
            case FContent.Orientation of
              TOrientation.Horizontal: begin
                W := 0;
                if Assigned(FFooter) then
                  W := (FFooter as TControl).Width;
                FContentBounds.Right := FContentBounds.Right - W;
              end;
              TOrientation.Vertical: begin
                H := 0;
                if Assigned(FFooter) then
                  H := (FFooter as TControl).Height;
                FContentBounds.Bottom := FContentBounds.Bottom - H;
              end;
            end;
            DoUpdateScrollingLimits(True, 0);
            DoUpdateState(FFooter, TListViewState.None, 0);
          end;
        except
        end;
      end
    , 0.3);
  end;
end;

procedure TPullScrollView.DoPullRefreshComplete;
var
  W, H: Single;
begin
  if Assigned(FHeader) then begin
    case FContent.Orientation of
      TOrientation.Horizontal: begin
        if FState = TListViewState.PullRightComplete then
          Exit;
        FHeader.DoUpdateState(TListViewState.PullRightComplete, 0);
      end;
      TOrientation.Vertical: begin
        if FState = TListViewState.PullDownComplete then
          Exit;
        FHeader.DoUpdateState(TListViewState.PullDownComplete, 0);
      end;
    end;

    FOffsetScroll := 0;
    case FContent.Orientation of
      TOrientation.Horizontal: begin
        W := 0;
        if Assigned(FHeader) then
          W := (FHeader as TControl).Width;
        if W > 0 then begin
          FContentBounds.Right := FContentBounds.Right - W;
          DoUpdateScrollingLimits(True, 0);
          FLastScrollValue := FLastScrollValue - W;
          HScrollBarValue := HScrollBarValue - W;
        end;
      end;
      TOrientation.Vertical: begin
        H := 0;
        if Assigned(FHeader) then
          H := (FHeader as TControl).Height;
        if H > 0 then begin
          FContentBounds.Bottom := FContentBounds.Bottom - H;
          DoUpdateScrollingLimits(True, 0);
          FLastScrollValue := FLastScrollValue - H;
          VScrollBarValue := VScrollBarValue - H;
        end;
      end;
    end;

    // 刷新完成，回弹
    TFrameAnimator.DelayExecute(Self,
      procedure (Sender: TObject)
      begin
        try
          if CheckState(TListViewState.PullDownFinish) or CheckState(TListViewState.PullRightFinish) then begin
            DoUpdateState(FHeader, TListViewState.None, 0);
          end;
        except
        end;
      end
    , 0.5);
  end;
end;

procedure TPullScrollView.DoRealign;
var
  LDisablePaint: Boolean;
  LDisableAlign: Boolean;
begin
  if FDisableAlign or IsUpdating or (not Assigned(FContent)) then
    Exit;
  if (csDestroying in ComponentState) then
    Exit;
  LDisablePaint := FDisablePaint;
  try
    FDisablePaint := True;

    inherited DoRealign;
    DoRealignContent();
    if Assigned(FAniCalculations) then begin
      LDisableAlign := FDisableAlign;
      FDisableAlign := True;
      RealignContent;
      FDisableAlign := LDisableAlign;
    end;
  finally
    FDisablePaint := LDisablePaint;
    FContent.Invalidate;
  end;
end;

procedure TPullScrollView.DoRealignContent;
var
  W, H: Single;
begin
  if not Assigned(FContent) then
    Exit;

  {$IFDEF MSWINDOWS}
  if Assigned(FScrollH) and (FScrollH.Visible) then
    H := Height - Padding.Bottom - Padding.Top - FScrollH.Height
  else
  {$ENDIF}
  H := Height - Padding.Bottom - Padding.Top;

  {$IFDEF MSWINDOWS}
  if Assigned(FScrollV) and (FScrollV.Visible) then
    W := Width - Padding.Right - Padding.Left - FScrollV.Width
  else
  {$ENDIF}
  W := Width - Padding.Right - Padding.Left;

  case ScrollBars of
    TViewScroll.Horizontal:
      FContent.SetBounds(Padding.Left - HScrollBarValue, Padding.Top, W, H);
    TViewScroll.Vertical:
      FContent.SetBounds(Padding.Left, Padding.Top - VScrollBarValue, W, H);
  end;
end;

procedure TPullScrollView.DoSetDefaulatScrollBars;
begin
  ScrollBars := TViewScroll.None;
end;

procedure TPullScrollView.DoUpdateHeaderFooter(const V: Single);
var
  View: TControl;
  ScrollValue, LV: Single;
begin
  case FContent.Orientation of
    TOrientation.Horizontal: ScrollValue := HScrollBarValue;
    TOrientation.Vertical: ScrollValue := VScrollBarValue;
  end;

  // 下拉刷新
  if Assigned(FHeader) and FEnablePullRefresh and (FContent.Orientation = TOrientation.Vertical) then begin
    View := FHeader as TControl;
    if (V > 0) or (FState = TListViewState.PullDownFinish) then begin
      View.SetBounds(0, V - View.Height + FOffsetScroll, FContent.Width, View.Height);
      View.HitTest := True;
      View.Visible := True;

      if FState = TListViewState.PullDownFinish then begin
        LV := - ScrollValue;
      end else
        LV := - View.Height - ScrollValue;

      case FState of
        TListViewState.None:
          begin
            DoUpdateState(FHeader, TListViewState.PullDownStart, ScrollValue);
            FOffsetScroll := 0;
          end;
        TListViewState.PullDownStart:
          begin
            if (LV >= 0) then begin
              DoUpdateState(FHeader, TListViewState.PullDownOK, ScrollValue);
              FOffsetScroll := 0;
            end;
          end;
        TListViewState.PullDownOK:
          begin
            if (LV < 0) then begin
              DoUpdateState(FHeader, TListViewState.PullDownStart, ScrollValue);
              FOffsetScroll := 0;
            end;
          end;
      end
    end else begin
      View.Visible := False;
      FOffsetScroll := 0;
      if FState = TListViewState.PullDownStart then
        FState := TListViewState.None;
    end;
  end;

  // 上拉加载更多
  if Assigned(FFooter) and FEnablePullLoad and (FContent.Orientation = TOrientation.Vertical) then begin
    View := FFooter as TControl;
    if V + FContent.Height < Height - Padding.Bottom - FOffsetScroll then begin
      View.SetBounds(0, V + FContent.Height + FOffsetScroll, FContent.Width, View.Height);
      View.Visible := True;
      View.HitTest := True;

      case FState of
        TListViewState.None:
          begin
            DoUpdateState(FFooter, TListViewState.PullUpStart, ScrollValue);
          end;
        TListViewState.PullUpStart:
          begin
            if (View.Position.Y + View.Height + 8 <= Height) then begin
              DoUpdateState(FFooter, TListViewState.PullUpOK, ScrollValue);
            end;
          end;
        TListViewState.PullUpOK:
          begin
            if (View.Position.Y + View.Height > Height - 6) then begin
              DoUpdateState(FFooter, TListViewState.PullUpStart, ScrollValue);
            end;
          end;
      end;
    end else begin
      View.Visible := False;
      if FState in [TListViewState.PullUpOK, TListViewState.PullUpStart] then
        FState := TListViewState.None;
    end;
  end;

  // 右拉刷新
  if Assigned(FHeader) and FEnablePullRefresh and (FContent.Orientation = TOrientation.Horizontal) then begin
    View := FHeader as TControl;
    if (V > 0) or (FState = TListViewState.PullRightFinish) then begin
      View.SetBounds(V - View.Width + FOffsetScroll, 0, View.Width, FContent.Height);
      View.HitTest := True;
      View.Visible := True;

      if FState = TListViewState.PullRightFinish then begin
        LV := - ScrollValue;
      end else
        LV := - View.Width - ScrollValue;

      case FState of
        TListViewState.None:
          begin
            DoUpdateState(FHeader, TListViewState.PullRightStart, ScrollValue);
            FOffsetScroll := 0;
          end;
        TListViewState.PullRightStart:
          begin
            if (LV >= 0) then begin
              DoUpdateState(FHeader, TListViewState.PullRightOK, ScrollValue);
              FOffsetScroll := 0;
            end;
          end;
        TListViewState.PullRightOK:
          begin
            if (LV < 0) then begin
              DoUpdateState(FHeader, TListViewState.PullRightStart, ScrollValue);
              FOffsetScroll := 0;
            end;
          end;
      end
    end else begin
      View.Visible := False;
      FOffsetScroll := 0;
      if FState = TListViewState.PullRightStart then
        FState := TListViewState.None;
    end;
  end;

  // 左拉加载更多
  if Assigned(FFooter) and FEnablePullLoad and (FContent.Orientation = TOrientation.Horizontal) then begin
    View := FFooter as TControl;
    if V + FContent.Width < Width - Padding.Right - FOffsetScroll then begin
      View.SetBounds(V + FContent.Width + FOffsetScroll, 0, View.Width, FContent.Height);
      View.Visible := True;
      View.HitTest := True;

      case FState of
        TListViewState.None:
          begin
            DoUpdateState(FFooter, TListViewState.PullLeftStart, ScrollValue);
          end;
        TListViewState.PullLeftStart:
          begin
            if (View.Position.X + View.Width + 8 <= Width) then begin
              DoUpdateState(FFooter, TListViewState.PullLeftOK, ScrollValue);
            end;
          end;
        TListViewState.PullLeftOK:
          begin
            if (View.Position.X + View.Width > Width - 6) then begin
              DoUpdateState(FFooter, TListViewState.PullLeftStart, ScrollValue);
            end;
          end;
      end;
    end else begin
      View.Visible := False;
      if FState in [TListViewState.PullLeftOK, TListViewState.PullLeftStart] then
        FState := TListViewState.None;
    end;
  end;
end;

procedure TPullScrollView.DoUpdateState(AObject: IListViewHeader;
  const State: TListViewState; const ScrollValue: Double);
begin
  AObject.DoUpdateState(State, ScrollValue);
  if FState <> State then
    FState := State;
end;

procedure TPullScrollView.FreeFooter;
begin
  if Assigned(FFooter) then begin
    RemoveObject(FFooter as TControl);
    FFooter := nil;
  end;
end;

procedure TPullScrollView.FreeHeader;
begin
  if Assigned(FHeader) then begin
    RemoveObject(FHeader as TControl);
    FHeader := nil;
  end;
end;

function TPullScrollView.GetContentChildCount: Integer;
begin
  if Assigned(FContent) then
    Result := FContent.ControlsCount
  else
    Result := 0;
end;

function TPullScrollView.GetContentControlItem(const Index: Integer): TControl;
begin
  Result := FContent.Controls[Index];
end;

function TPullScrollView.GetScrollOffset: TPointF;
begin
  case FContent.Orientation of
    TOrientation.Horizontal: begin
      Result.X := FOffsetScroll;
      Result.Y := 0;
    end;
    TOrientation.Vertical: begin
      Result.X := 0;
      Result.Y := FOffsetScroll;
    end;
  end;
end;

procedure TPullScrollView.HScrollChange(Sender: TObject);
var
  H: Single;
begin
  if FScrolling then Exit;
  inherited HScrollChange(Sender);
  if Assigned(FContent) then begin
    H := Padding.Left - HScrollBarValue;
    DoUpdateHeaderFooter(H);
    FContent.Position.X := H + FOffsetScroll;
  end;
end;

procedure TPullScrollView.InitFooter;
begin
  if csDesigning in ComponentState then
    Exit;
  if not Assigned(FFooter) then begin
    if Assigned(FOnInitFooter) then
      FOnInitFooter(Self, FFooter);
    if not Assigned(FFooter) then
      FFooter := TListViewDefaultFooter.Create(Self);
    case FContent.Orientation of
      TOrientation.Horizontal: FFooter.SetStateHint(TListViewState.None, '左拉加载更多');
      TOrientation.Vertical: FFooter.SetStateHint(TListViewState.None, '上拉加载更多');
    end;
    (FFooter as TControl).Parent := Self;
    (FFooter as TControl).Stored := False;
    (FFooter as TControl).Visible := False;
    FFooter.Orientation := FContent.Orientation;
  end;
end;

procedure TPullScrollView.InitHeader;
begin
  if csDesigning in ComponentState then
    Exit;
  if not Assigned(FHeader) then begin
    if Assigned(FOnInitHeader) then
      FOnInitHeader(Self, FHeader);
    if not Assigned(FHeader) then
      FHeader := TListViewDefaultHeader.Create(Self);
    (FHeader as TControl).Parent := Self;
    (FHeader as TControl).Stored := False;
    (FHeader as TControl).Index := 0;
    (FHeader as TControl).Visible := False;
    FHeader.Orientation := FContent.Orientation;
  end;
end;

procedure TPullScrollView.InvalidateContentSize;
begin
  if not Assigned(FContent) then begin
    FContentBounds.Left := 0;
    FContentBounds.Top := 0;
    FContentBounds.Right := 0;
    FContentBounds.Bottom := 0;
    Exit;
  end;
  FContentBounds.Left := 0;
  FContentBounds.Right := FContent.Width;
  FContentBounds.Top := 0;
  FContentBounds.Bottom := FContent.Height;

  case FContent.Orientation of
    TOrientation.Horizontal: begin
      if Assigned(FHeader) and FHeader.Visible then
        FContentBounds.Right := FContentBounds.Right + (FHeader as TControl).Width;
      if Assigned(FFooter) and FFooter.Visible then
        FContentBounds.Right := FContentBounds.Right + (FFooter as TControl).Width;
    end;
    TOrientation.Vertical: begin
      if Assigned(FHeader) and FHeader.Visible then
        FContentBounds.Bottom := FContentBounds.Bottom + (FHeader as TControl).Height;
      if Assigned(FFooter) and FFooter.Visible then
        FContentBounds.Bottom := FContentBounds.Bottom + (FFooter as TControl).Height;
    end;
  end;
end;

function TPullScrollView.IsAddToContent(const AObject: TFmxObject): Boolean;
begin
  Result := (FContent <> nil)
    and (AObject <> FContent)
    and (AObject <> FHeader as TObject)
    and (AObject <> FFooter as TObject)
    and not (AObject is TEffect)
    and not (AObject is TAnimation)
    and not ((AObject = FScrollV) or
             (AObject = FScrollH));
end;

procedure TPullScrollView.Loaded;
begin
  inherited Loaded;
  RealignContent;
end;

procedure TPullScrollView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  //LogD('MouseDown');
  {$IFDEF NEXTGEN}
  inherited;
  {$ELSE}
  if DragScroll then begin
    FDownPos.X := X;
    FDownPos.Y := Y;
    FMovePos := FDownPos;
    AniMouseDown(True, X, Y);
    if Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then begin
      FMouseDown := False;
      TFrameAnimator.DelayExecute(Self,
        procedure (Sender: TObject)
        var
          P: TPointF;
        begin
          try
            if (FMovePos <> FDownPos) or (FMouseDown) then Exit;
            if Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then begin
              P := (FPointTarget as TControl).AbsoluteToLocal(LocalToAbsolute(PointF(X, Y)));
              FPointTarget.MouseDown(Button, Shift, P.X, P.Y);
            end;
          except
          end;
        end,
      0.05);
    end else
      FMouseDown := True;

  end else
    inherited;
  {$ENDIF}
end;

procedure TPullScrollView.MouseMove(Shift: TShiftState; X, Y: Single);
{$IFNDEF NEXTGEN}
var
  P: TPointF;
{$ENDIF}
begin
  {$IFDEF NEXTGEN}
  inherited;
  {$ELSE}
  if DragScroll then begin
    if ssLeft in Shift then begin
      FMovePos.X := X;
      FMovePos.Y := Y;
      AniMouseMove(True, X, Y);
    end else
      if Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then begin
        P := (FPointTarget as TControl).AbsoluteToLocal(LocalToAbsolute(PointF(X, Y)));
        FPointTarget.MouseMove(Shift, P.X, P.Y);
      end;
  end else
    inherited;
  {$ENDIF}
end;

procedure TPullScrollView.MousePosToAni(var X, Y: Single);
var
  LPoint: TPointF;
begin
  LPoint := PointF(X, Y);
  if FContent <> nil then
  begin
    LPoint := FContent.AbsoluteToLocal(LocalToAbsolute(LPoint));
    X := LPoint.X;
    Y := LPoint.Y;
  end;
end;

procedure TPullScrollView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
{$IFNDEF NEXTGEN}
var
  P: TPointF;
{$ENDIF}
begin
  //LogD('MouseUp');
  {$IFDEF NEXTGEN}
  inherited;
  {$ELSE}
  FMouseDown := False;
  if DragScroll then begin
    if Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then begin
      if FMovePos = FDownPos then begin
        Sleep(30);
        P := (FPointTarget as TControl).AbsoluteToLocal(LocalToAbsolute(PointF(X, Y)));
        FPointTarget.MouseUp(Button, Shift, P.X, P.Y);
      end;
    end;
    if (Button = TMouseButton.mbLeft) then begin
      FMovePos := TPointF.Zero;
      AniMouseUp(True, X, Y);
    end;
  end else
    inherited;
  {$ENDIF}
end;

function TPullScrollView.ObjectAtPoint(AScreenPoint: TPointF): IControl;
{$IFNDEF NEXTGEN}var P: TPointF; IT: IViewTouch;{$ENDIF}
begin
  Result := inherited;
  {$IFNDEF NEXTGEN}
  if DragScroll and (not (csDesigning in ComponentState)) then begin // 如果允许拖动
    if FMouseDown then
      Exit;
    //LogD('FMouseDown: ' + BoolToStr(FMouseDown));
    if Assigned(Result) then begin
      if Supports(Result, ITextInput) then begin
        FPointTarget := nil;
        Exit;
      end;
      if Supports(Result, IViewTouch, IT) and (IT.IsCanTouch) then begin
        FPointTarget := nil;
        Exit;
      end;
    end;
    P := ScreenToLocal(AScreenPoint);
    if Assigned(Result) and (P.X < Width - 10) then begin
      FPointTarget := Result;
      Result := Self;
    end else
      FPointTarget := nil;  // win10 bug
  end;
  {$ENDIF}
end;

procedure TPullScrollView.PullLoadComplete;
begin
  if Assigned(FContent) and ((FState = TListViewState.PullUpFinish) or (FState = TListViewState.PullLeftFinish)) then
    DoPullLoadComplete;
end;

procedure TPullScrollView.PullRefreshComplete;
begin
  if Assigned(FContent) and ((FState = TListViewState.PullDownFinish) or (FState = TListViewState.PullRightFinish)) then
    DoPullRefreshComplete;
end;

procedure TPullScrollView.PullRefreshStart;
begin
  if not Assigned(FHeader) then
    Exit;
  case FContent.Orientation of
    TOrientation.Horizontal: begin
      if FState = TListViewState.PullRightFinish then
        Exit;
      FState := TListViewState.PullRightFinish;
      FOffsetScroll := (FHeader as TControl).Width;
      HScrollBarValue := HScrollBarValue + FOffsetScroll;
    end;
    TOrientation.Vertical: begin
      if FState = TListViewState.PullDownFinish then
        Exit;
      FState := TListViewState.PullDownFinish;
      FOffsetScroll := (FHeader as TControl).Height;
      VScrollBarValue := VScrollBarValue + FOffsetScroll;
    end;
  end;

  DoUpdateState(FHeader, FState, 0);
  InvalidateContentSize;
  DoUpdateScrollingLimits(True, 0);
  if Assigned(FOnPullRefresh) then
    FOnPullRefresh(Self);
  DoRealign;
  (FHeader as TControl).UpdateEffects;
end;

procedure TPullScrollView.Resize;
begin
  if (csLoading in ComponentState) or
    (csDestroying in ComponentState) then
    Exit;
  inherited Resize;
  UpdateScrollBar(FScrollV, FScrollbar);
  RealignContent;
end;

procedure TPullScrollView.SetEnablePullLoad(const Value: Boolean);
begin
  if FEnablePullLoad <> Value then begin
    FEnablePullLoad := Value;
    if (not Value) then begin
      if csDesigning in ComponentState then
        FreeFooter
      else
        DoPullLoadComplete;
    end else begin
      InitFooter;
      if Assigned(FFooter) then begin
        case FContent.Orientation of
          TOrientation.Horizontal: begin
            FContentBounds.Right := FContentBounds.Right + (FFooter as TControl).Width;
          end;
          TOrientation.Vertical: begin
            FContentBounds.Bottom := FContentBounds.Bottom + (FFooter as TControl).Height;
          end;
        end;
        DoUpdateScrollingLimits(True);
      end;
      DoRealign;
    end;
  end;
end;

procedure TPullScrollView.SetEnablePullRefresh(const Value: Boolean);
begin
  if FEnablePullRefresh <> Value then begin
    FEnablePullRefresh := Value;
    if (not Value) then begin
      if csDesigning in ComponentState then
        FreeHeader
      else
        DoPullRefreshComplete;
    end else begin
      InitHeader;
      if Assigned(FHeader) then begin
        case FContent.Orientation of
          TOrientation.Horizontal: begin
            FContentBounds.Right := FContentBounds.Right + (FHeader as TControl).Width;
          end;
          TOrientation.Vertical: begin
            FContentBounds.Bottom := FContentBounds.Bottom + (FHeader as TControl).Height;
          end;
        end;
        DoUpdateScrollingLimits(True);
      end;
      DoRealign;
    end;
  end;
end;

procedure TPullScrollView.VScrollChange(Sender: TObject);
var
  V: Single;
begin
  if FScrolling then Exit;
  inherited VScrollChange(Sender);
  if Assigned(FContent) then begin
    V := Padding.Top - VScrollBarValue;
    DoUpdateHeaderFooter(V);
    FContent.Position.Y := V + FOffsetScroll;
  end;
end;

{ TViewScrollContent }

procedure TViewScrollContent.ContentChanged;
begin
  {$IF CompilerVersion >= 32.0}
  if (FScrollBox <> nil) and ([csLoading, csDestroying] * FScrollBox.ComponentState = [])
    and not ScrollBox.InInternalAlign then
  {$ELSE}
  if (FScrollBox <> nil) and not FScrollBox.Released and ([csLoading, csDestroying] * FScrollBox.ComponentState = [])
    and not ScrollBox.InInternalAlign then
  {$ENDIF}
  begin
    FIsContentChanged := True;
    FScrollBox.InvalidateContentSize;
    if not IsUpdating then
      FScrollBox.Realign;
  end;
end;

constructor TViewScrollContent.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TScrollView then
    FScrollBox := TScrollView(AOwner);
  ClipChildren := True;
  FIsContentChanged := True;
  SetAcceptsControls(False);
end;

procedure TViewScrollContent.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if FScrollBox <> nil then
    FScrollBox.ContentAddObject(AObject);
end;

function TViewScrollContent.DoGetUpdateRect: TRectF;
begin
  if ParentControl is TScrollView then
    Result := TScrollView(ParentControl).UpdateRect
  else
    Result := inherited DoGetUpdateRect;
end;

procedure TViewScrollContent.DoInsertObject(Index: Integer;
  const AObject: TFmxObject);
begin
  inherited;
  if FScrollBox <> nil then
    FScrollBox.ContentInsertObject(Index, AObject);
end;

procedure TViewScrollContent.DoRealign;
begin
  if ScrollBox <> nil then
    inherited;
  FLastWidth := Width;
  FLastHeight := Height;
end;

procedure TViewScrollContent.DoRemoveObject(const AObject: TFmxObject);
begin
  if FScrollBox <> nil then
    FScrollBox.ContentBeforeRemoveObject(AObject);
  inherited;
  if FScrollBox <> nil then
    FScrollBox.ContentRemoveObject(AObject);
end;

function TViewScrollContent.GetChildrenRect: TRectF;
begin
  Result := GetUpdateRect;
end;

function TViewScrollContent.GetClipRect: TRectF;
var
  P: TPointD;
  OP: TPointF;
begin
  if FScrollBox <> nil then begin
    Result := RectF(0, 0, FScrollBox.Width, FScrollBox.Height);
    if Assigned(FScrollBox.FAniCalculations) then begin
      P := FScrollBox.ViewportPosition;
      OP := FScrollBox.GetScrollOffset;
      Result.Offset(P.X - OP.X, P.Y - OP.Y);
    end;
  end else
    Result := inherited GetClipRect;
end;

function TViewScrollContent.ObjectAtPoint(P: TPointF): IControl;
begin
  if Assigned(FScrollBox.FAniCalculations) and (FScrollBox.FAniCalculations.Shown) then
    Result := nil   // 手势滚动中，不允许点击子项
  else
    Result := inherited ObjectAtPoint(P);
//  if Result <> nil then
//  begin
//    if FScene <> nil then
//      P := FScene.ScreenToLocal(P);
//    P := AbsoluteToLocal(P);
//    if not ClipRect.Contains(P) then
//      Result := nil;
//  end;
end;

function TViewScrollContent.PointInObjectLocal(X, Y: Single): Boolean;
var
  ClipRect: TRectF;
begin
  ClipRect := GetClipRect;
  Result := (X >= (ClipRect.TopLeft.X - TouchTargetExpansion.Left)) and
    (X <= (ClipRect.TopLeft.X + ClipRect.Width + TouchTargetExpansion.Right)) and
    (Y >= (ClipRect.TopLeft.Y + TouchTargetExpansion.Top)) and
    (Y <= (ClipRect.TopLeft.Y + ClipRect.Height + TouchTargetExpansion.Bottom));
end;

{ TVertScrollView }

procedure TVertScrollView.DoSetDefaulatScrollBars;
begin
  ScrollBars := TViewScroll.Vertical;
end;

{ THorzScrollView }

procedure THorzScrollView.DoSetDefaulatScrollBars;
begin
  ScrollBars := TViewScroll.Horizontal;
end;

initialization

finalization

end.

