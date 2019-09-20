{*******************************************************}
{                                                       }
{       FMX UI 核心基础单元                             }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

// 注意：如果多行文本显示不完整，请将patch下的
// FMX.TextLayout.GPU.pas 放到你的项目目录下

unit UI.Base;

interface

{$R ViewIcon.res}
{$SCOPEDENUMS ON}

{$IF CompilerVersion >= 29.0}
  {$DEFINE XE8_OR_NEWER}
{$ENDIF}

uses
  UI.Debug, UI.Utils, UI.Utils.SVGImage,
  FMX.Forms,
  FMX.FontGlyphs,
  {$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.Jni,
  Androidapi.JNI.Media,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.JNI.Os,
  FMX.Helpers.Android,
  {$ENDIF}
  {$IFDEF IOS}
  IOSApi.Foundation,
  {$ENDIF}
  FMX.BehaviorManager, FMX.StdActns, FMX.Menus,
  FMX.Styles, FMX.Styles.Objects,
  FMX.Utils, FMX.ImgList, FMX.MultiResBitmap, FMX.ActnList, System.Rtti, FMX.Consts,
  FMX.TextLayout, FMX.Objects, System.ImageList, System.RTLConsts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, System.Math, System.UIConsts,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement, FMX.Ani;

const
  AllCurrentPlatforms =
    pidWin32 or pidWin64 or pidOSX32 or
    pidiOSSimulator or pidiOSDevice or pidAndroid;

type
  IView = interface;
  IViewGroup = interface;
  TView = class;
  TViewGroup = class;

  TDrawableBase = class;
  TDrawable = class;
  TDrawableIcon = class;
  TViewColor = class;
  TDrawableBrush = class;

  EViewError = class(Exception);
  EViewLayoutError = class(Exception);
  EDrawableError = class(Exception);

  TViewClass = class of TControl;

  /// <summary>
  /// 视图状态
  /// </summary>
  TViewState = (None {正常}, Pressed {按下}, Focused {取得焦点}, Hovered {悬停},
    Selected{选中}, Checked{复选}, Enabled{禁用}, Activated{激活}, Custom {自定义});
  TViewStates = set of TViewState;

  /// <summary>
  /// 视图大小
  /// </summary>
  TViewSize = (CustomSize {自定义大小}, WrapContent {随内容}, FillParent {填充父级});
  /// <summary>
  /// 滚动条
  /// </summary>
  TViewScroll = (None, Horizontal, Vertical, Both);

  TViewBrushKind = (None, Solid, Gradient, Bitmap, Resource, Patch9Bitmap, AccessoryBitmap, SVGImage);

  /// <summary>
  /// 附件样式
  /// </summary>
  TViewAccessoryStyle = (Accessory, Path);

  /// <summary>
  /// 附图类型
  /// </summary>
  TViewAccessoryType = (None, More, Checkmark, Detail, Ellipses, Flag, Back, Refresh,
    Action, Play, Rewind, Forwards, Pause, Stop, Add, Prior,
    Next, BackWard, ForwardGo, ArrowUp, ArrowDown, ArrowLeft, ArrowRight, Reply,
    Search, Bookmarks, Trash, Organize, Camera, Compose, Info,
    Pagecurl, Details, RadioButton, RadioButtonChecked, CheckBox,
    CheckBoxChecked, User, Password, Down, Exit, Finish, Calendar, Cross, Menu,
    About, Share, UserMsg, Cart, Setting, Edit, Home, Heart,
    Comment, Collection, Fabulous, Image, Help, VCode, Time, UserReg, Scan, Circle, Location,
    UserDefined1, UserDefined2, UserDefined3);

  TPatchBounds = class(TBounds);

  TRectFHelper = record Helper for TRectF
  public
    procedure Clear; inline;
  end;

  TControlHelper = class Helper for TControl
  public
    // 为指定控件设置焦点
    function SetFocusObject(V: TControl): Boolean;
    // 进入下一个焦点控件
    procedure FocusToNext();
  end;

  /// <summary>
  /// 列表视图状态
  /// </summary>
  TListViewState = (None {无},  PullChangeing,
    PullDownStart {下拉开始}, PullDownOK {下拉到位}, PullDownFinish {下拉松开}, PullDownComplete {下拉完成},
    PullUpStart {上拉开始}, PullUpOK {上拉到位}, PullUpFinish {上拉松开}, PullUpComplete {上拉完成},
    PullLeftStart {左拉开始}, PullLeftOK {左拉到位}, PullLeftFinish {左拉松开}, PullLeftComplete {左拉完成},
    PullRightStart {右拉开始}, PullRightOK {右拉到位}, PullRightFinish {右拉松开}, PullRightComplete {右拉完成}
  );

  /// <summary>
  /// 列表 Header 或 Footer 接口
  /// </summary>
  IListViewHeader = interface
    ['{44F6F649-D173-4BEC-A38D-F03436ED55BC}']
    /// <summary>
    /// 更新状态
    /// </summary>
    procedure DoUpdateState(const State: TListViewState;
      const ScrollValue: Double);
    /// <summary>
    /// 设置各种状态要显示的消息
    /// </summary>
    procedure SetStateHint(const State: TListViewState; const Msg: string);
    function GetVisible: Boolean;
    function GetOrientation: TOrientation;
    procedure SetOrientation(AOrientation: TOrientation);
    /// <summary>
    /// 可视状态
    /// </summary>
    property Visible: Boolean read GetVisible;
    /// <summary>
    /// 方向
    /// </summary>
    property Orientation: TOrientation read GetOrientation write SetOrientation;
  end;

  /// <summary>
  /// 加载 Header 或 Footer 事件
  /// </summary>
  TOnInitHeader = procedure (Sender: TObject; var NewFooter: IListViewHeader) of object;

  { 感谢  KernowSoftwareFMX }
  TViewAccessoryImageList = class(TObjectList<TBitmap>)
  private
    FImageScale: Single;
    FImageMap: TBitmap;
    FActiveStyle: TFmxObject;
  protected
    //procedure AddFlagAccessory;
    procedure AddBackAccessory;
    procedure AddAddAccessory;
    procedure AddRefreshAccessory;
    procedure CalculateImageScale;
    function GetAccessoryFromResource(const AStyleName: string; const AState: string = ''): TBitmap;
    function LoadFromResource(const AStyleName: string): TBitmap;
    procedure Initialize;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPath(const PathData: string; const SW, SH: Single);

    function GetAccessoryImage(AAccessory: TViewAccessoryType): TBitmap;
    procedure SetAccessoryImage(AAccessory: TViewAccessoryType; const Value: TBitmap);

    procedure Draw(ACanvas: TCanvas; const ARect: TRectF; AAccessory: TViewAccessoryType;
      const AOpacity: Single = 1; const AStretch: Boolean = True);

    property Images[AAccessory: TViewAccessoryType]: TBitmap read GetAccessoryImage write SetAccessoryImage; default;
    property ImageMap: TBitmap read FImageMap;
    property ImageScale: single read FImageScale;
  end;

  /// <summary>
  /// 9 宫格位图
  /// </summary>
  TPatch9Bitmap = class(TBrushBitmap)
  private
    FBounds: TPatchBounds;
    FRemoveBlackLine: Boolean;
    procedure SetBounds(const Value: TPatchBounds);
    procedure SetRemoveBlackLine(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Bounds: TPatchBounds read FBounds write SetBounds;
    // 是否移除黑线(.9.png一般会有一条黑线，移除时，等于是将原图截掉最外围的1像索)
    property BlackLine: Boolean read FRemoveBlackLine write SetRemoveBlackLine default False;
  end;

  TViewAccessory = class(TPersistent)
  private
    FStyle: TViewAccessoryStyle;
    FAccessoryType: TViewAccessoryType;
    FAccessoryColor: TAlphaColor;
    FAccessoryBmp: TBitmap;
    FPathData: TPathData;
    FOnChanged: TNotifyEvent;
    procedure SetAccessoryType(const Value: TViewAccessoryType);
    procedure SetAccessoryColor(const Value: TAlphaColor);
    function GetIsEmpty: Boolean;
    procedure SetStyle(const Value: TViewAccessoryStyle);
    function GetPathData: string;
    procedure SetPathData(const Value: string);
  protected
    procedure DoChanged();
    procedure DoPathChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property IsEmpty: Boolean read GetIsEmpty;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Accessory: TViewAccessoryType read FAccessoryType write SetAccessoryType default TViewAccessoryType.None;
    property Color: TAlphaColor read FAccessoryColor write SetAccessoryColor default TAlphaColorRec.White;
    property PathData: string read GetPathData write SetPathData;
    property Style: TViewAccessoryStyle read FStyle write SetStyle default TViewAccessoryStyle.Accessory;
  end;

  TViewBrushBase = class(TBrush)
  private
    FAccessory: TViewAccessory;
    FSvgImage: TSVGImage;
    function GetKind: TViewBrushKind;
    procedure SetKind(const Value: TViewBrushKind);
    function IsKindStored: Boolean;
    function GetAccessory: TViewAccessory;
    procedure SetAccessory(const Value: TViewAccessory);
    procedure SetSvgImage(const Value: TSVGImage);
    function GetSvgImage: TSVGImage;
  protected
    procedure DoAccessoryChange(Sender: TObject);
    procedure DoSvgImageChange(Sender: TObject);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ChangeToSolidColor(const AColor: TAlphaColor; IsDefault: Boolean = True);
  published
    property Accessory: TViewAccessory read GetAccessory write SetAccessory;
    property SVGImage: TSVGImage read GetSvgImage write SetSvgImage;
    property Kind: TViewBrushKind read GetKind write SetKind stored IsKindStored;
  end;

  TViewBrush = class(TViewBrushBase)
  private
    function IsPatch9BitmapStored: Boolean;
    function GetBitmap: TPatch9Bitmap;
    procedure SetBitmap(const Value: TPatch9Bitmap);
  protected
  public
    constructor Create(const ADefaultKind: TViewBrushKind; const ADefaultColor: TAlphaColor);
    procedure Assign(Source: TPersistent); override;
  published
    property Bitmap: TPatch9Bitmap read GetBitmap write SetBitmap stored IsPatch9BitmapStored;
  end;

  TCustomActionEx = class(FMX.Menus.TMenuItem);

  TViewImagesBrush = class(TViewBrushBase, IInterface, IGlyph, IInterfaceComponentReference)
  private
    FImageIndex: TImageIndex;
    [Weak] FOwner: TObject;
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IInterfaceComponentReference }
    function GetComponent: TComponent;
    {IGlyph}
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure ImagesChanged; virtual;
    function GetImageList: TBaseImageList; inline;
    procedure SetImageList(const Value: TBaseImageList);
    function IGlyph.GetImages = GetImageList;
    procedure IGlyph.SetImages = SetImageList;
  public
    constructor Create(const ADefaultKind: TBrushKind; const ADefaultColor: TAlphaColor);
    procedure Assign(Source: TPersistent); override;
    property Owner: TObject read FOwner write FOwner;
    property Images: TBaseImageList read GetImageList write SetImageList;
  published
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
  end;

  /// <summary>
  /// 绘制位置
  /// </summary>
  TDrawablePosition = (Left, Right, Top, Bottom, Center);

  /// <summary>
  /// 绘制样式
  /// </summary>
  TDrawableKind = (None, Circle, Ellipse);

  /// <summary>
  /// 可绘制对象
  /// </summary>
  TDrawableBase = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FKind: TDrawableKind;

    FXRadius, FYRadius: Single;
    FIsEmpty: Boolean;

    FCorners: TCorners;
    FCornerType: TCornerType;

    procedure GetStateBrush(const State: TViewState; var V: TBrush); overload;
    procedure SetStateBrush(const State: TViewState; const V: TBrush);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    procedure SetCorners(const Value: TCorners);
    function IsStoredCorners: Boolean;
    procedure SetCornerType(const Value: TCornerType);
    procedure SetKind(const Value: TDrawableKind);
  protected
    [Weak] FView: IView;
    FDefault: TBrush;  // 0
    FPressed: TBrush;  // 1
    FFocused: TBrush;  // 2
    FHovered: TBrush;  // 3
    FSelected: TBrush; // 4
    FChecked: TBrush;  // 5
    FEnabled: TBrush;  // 6
    FActivated: TBrush;// 7

    function GetEmpty: Boolean; virtual;
    function GetDrawRect(const ALeft, ATop, ARight, ABottom: Single): TRectF; virtual;
    function GetValue(const Index: Integer): TBrush;
    procedure SetValue(const Index: Integer; const Value: TBrush);

    procedure DoChange(Sender: TObject);

    class procedure FillRect9Patch(Canvas: TCanvas; const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TViewBrush; const ACornerType: TCornerType = TCornerType.Round);
    procedure FillRect(Canvas: TCanvas; const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.Round);
    procedure FillArc(Canvas: TCanvas; const Center, Radius: TPointF;
      const StartAngle, SweepAngle, AOpacity: Single; const ABrush: TBrush); inline;

    procedure DoDrawed(Canvas: TCanvas; var R: TRectF; AState: TViewState; const AOpacity: Single); virtual;
    procedure InitDrawable; virtual;
  public
    constructor Create(View: IView; const ADefaultKind: TViewBrushKind = TViewBrushKind.None;
      const ADefaultColor: TAlphaColor = TAlphaColors.Null);
    destructor Destroy; override;

    function BrushIsEmpty(V: TBrush): Boolean;

    function GetBrush(const State: TViewState; AutoCreate: Boolean): TBrush;
    function GetStateBrush(const State: TViewState): TBrush; overload;
    function GetStateItem(AState: TViewState): TBrush;
    function GetStateImagesItem(AState: TViewState): TBrush;

    procedure Assign(Source: TPersistent); override;
    procedure Change; virtual;
    procedure CreateBrush(var Value: TBrush;
      const ADefaultKind: TViewBrushKind = TViewBrushKind.None;
      const ADefaultColor: TAlphaColor = TAlphaColors.Null); overload; virtual;
    function CreateBrush(): TBrush; overload;

    procedure Draw(Canvas: TCanvas); virtual;
    procedure DrawTo(Canvas: TCanvas; const R: TRectF); inline;
    procedure DrawStateTo(Canvas: TCanvas; const R: TRectF; AState: TViewState); overload;
    procedure DrawStateTo(Canvas: TCanvas; const R: TRectF; AState: TViewState; const AOpacity: Single); overload; virtual;
    procedure DrawBrushTo(Canvas: TCanvas; ABrush: TBrush; const R: TRectF);

    procedure SetRadius(const X, Y: Single);
    procedure SetDrawable(const Value: TDrawableBase); overload;
    procedure SetBrush(State: TViewState; const Value: TBrush); overload;
    procedure SetBrush(State: TViewState; const Value: TDrawableBrush); overload;
    procedure SetColor(State: TViewState; const Value: TAlphaColor); overload;
    procedure SetGradient(State: TViewState; const Value: TGradient); overload;
    procedure SetBitmap(State: TViewState; const Value: TBitmap); overload;
    procedure SetBitmap(State: TViewState; const Value: TBrushBitmap); overload;

    // 是否为空
    property IsEmpty: Boolean read FIsEmpty;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    // 边框圆角
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property Corners: TCorners read FCorners write SetCorners stored IsStoredCorners;
    property CornerType: TCornerType read FCornerType write SetCornerType default TCornerType.Round;

    // 背景样式
    property Kind: TDrawableKind read FKind write SetKind default TDrawableKind.None;
  end;

  /// <summary>
  /// 可绘制对象
  /// </summary>
  TDrawable = class(TDrawableBase)
  private
    FPadding: TBounds;
    procedure SetPadding(const Value: TBounds);
    function GetPaddings: string;
    procedure SetPaddings(const Value: string);
    function GetValue(const Index: Integer): TViewBrush;
    procedure SetValue(const Index: Integer; const Value: TViewBrush);
  protected
    function GetDrawRect(const ALeft, ATop, ARight, ABottom: Single): TRectF; override;
  public
    constructor Create(View: IView; const ADefaultKind: TViewBrushKind = TViewBrushKind.None;
      const ADefaultColor: TAlphaColor = TAlphaColors.Null);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Padding: TBounds read FPadding write SetPadding;
    property Paddings: string read GetPaddings write SetPaddings stored False;

    property XRadius;
    property YRadius;
    property Corners;
    property CornerType;
    property Kind;
    property ItemDefault: TViewBrush index 0 read GetValue write SetValue;
    property ItemPressed: TViewBrush index 1 read GetValue write SetValue;
    property ItemFocused: TViewBrush index 2 read GetValue write SetValue;
    property ItemHovered: TViewBrush index 3 read GetValue write SetValue;
    property ItemSelected: TViewBrush index 4 read GetValue write SetValue;
    property ItemChecked: TViewBrush index 5 read GetValue write SetValue;
    property ItemEnabled: TViewBrush index 6 read GetValue write SetValue;
    property ItemActivated: TViewBrush index 7 read GetValue write SetValue;
  end;

  /// <summary>
  /// 边框样式
  /// </summary>
  TViewBorderStyle = (None {无边框},
    RectBorder {四周矩形边框,会使用圆角设置},
    RectBitmap {实心的矩形, 像框},
    CircleBorder {圆形边框},
    EllipseBorder {椭圆边框},
    LineEdit {底部边框（带两端凸出},
    LineTop {顶部边框},
    LineBottom {底部边框},
    LineLeft {左边边框},
    LineRight {右边边框} );

  TViewBorder = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FBrush: TStrokeBrush;
    FColor: TViewColor;
    FStyle: TViewBorderStyle;
    FDefaultStyle: TViewBorderStyle;
    procedure SetColor(const Value: TViewColor);
    procedure SetStyle(const Value: TViewBorderStyle);
    procedure SetWidth(const Value: Single);
    procedure SetOnChanged(const Value: TNotifyEvent);
    function GetDash: TStrokeDash;
    function GetWidth: Single;
    procedure SetDash(const Value: TStrokeDash);
    function GetCap: TStrokeCap;
    function GetJoin: TStrokeJoin;
    procedure SetCap(const Value: TStrokeCap);
    procedure SetJoin(const Value: TStrokeJoin);
    function WidthStored: Boolean;
    function StyleStored: Boolean;
    function GetGradient: TGradient;
    procedure SetGradient(const Value: TGradient);
    function GetBitmap: TBrushBitmap;
    function GetKind: TBrushKind;
    function IsBitmapStored: Boolean;
    function IsGradientStored: Boolean;
    procedure SetBitmap(const Value: TBrushBitmap);
    procedure SetKind(const Value: TBrushKind);
  protected
    procedure DoChanged();
    procedure DoGradientChanged(Sender: TObject);
  public
    constructor Create(ADefaultStyle: TViewBorderStyle = TViewBorderStyle.None);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property OnChanged: TNotifyEvent read FOnChanged write SetOnChanged;
    property Brush: TStrokeBrush read FBrush;
    property DefaultStyle: TViewBorderStyle read FDefaultStyle write FDefaultStyle;
  published
    property Color: TViewColor read FColor write SetColor;
    property Width: Single read GetWidth write SetWidth stored WidthStored;
    property Style: TViewBorderStyle read FStyle write SetStyle stored StyleStored;
    property Dash: TStrokeDash read GetDash write SetDash default TStrokeDash.Solid;
    property Cap: TStrokeCap read GetCap write SetCap default TStrokeCap.Flat;
    property Join: TStrokeJoin read GetJoin write SetJoin default TStrokeJoin.Miter;
    property Gradient: TGradient read GetGradient write SetGradient stored IsGradientStored;
    property Bitmap: TBrushBitmap read GetBitmap write SetBitmap stored IsBitmapStored;
    property Kind: TBrushKind read GetKind write SetKind default TBrushKind.Solid;
  end;

  TDrawableBorder = class(TDrawable)
  private
    FBorder: TViewBorder;
    procedure SetBorder(const Value: TViewBorder);
    function GetBorder: TViewBorder;
  protected
    function GetEmpty: Boolean; override;
    procedure CreateBorder(); virtual;
    procedure DoDrawed(Canvas: TCanvas; var R: TRectF; AState: TViewState; const AOpacity: Single); override;
  public
    constructor Create(View: IView; const ADefaultKind: TViewBrushKind = TViewBrushKind.None;
      const ADefaultColor: TAlphaColor = TAlphaColors.Null);
    destructor Destroy; override;
    procedure DrawBorder(Canvas: TCanvas; var R: TRectF; AState: TViewState);
    procedure Assign(Source: TPersistent); override;
    property _Border: TViewBorder read FBorder;
  published
    property Border: TViewBorder read GetBorder write SetBorder;
  end;

  TViewImageLink = class(TGlyphImageLink)
  public
    constructor Create(AOwner: TDrawableIcon); reintroduce;
    procedure Change; override;
  end;

  /// <summary>
  /// 可绘制刷子组件
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TDrawableBrush = class(TComponent, IGlyph, IInterfaceComponentReference)
  private
    FBrush: TBrush;
    FImageLink: TGlyphImageLink;
    FOnChanged: TNotifyEvent;
    function GetBrush: TBrush;
    function GetImages: TCustomImageList;
    function GetIsEmpty: Boolean;
    procedure SetBrush(const Value: TBrush);
    function GetImageIndexEx: TImageIndex;
    procedure SetImageIndexEx(const Value: TImageIndex);
  protected
    { IInterfaceComponentReference }
    function GetComponent: TComponent;
    {IGlyph}
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure ImagesChanged; virtual;
    function GetImageList: TBaseImageList; inline;
    procedure SetImageList(const Value: TBaseImageList);
    function IGlyph.GetImages = GetImageList;
    procedure IGlyph.SetImages = SetImageList;
  protected
    procedure CreateBrush(var Value: TBrush); virtual;
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; const R: TRectF;
      const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single = 1; const ACornerType: TCornerType = TCornerType.Round); virtual;
    property IsEmpty: Boolean read GetIsEmpty;
    property ImageIndex: TImageIndex read GetImageIndexEx write SetImageIndexEx;
  published
    property Images: TCustomImageList read GetImages write SetImages;
    property Brush: TBrush read GetBrush write SetBrush;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  /// <summary>
  /// 可绘制图标
  /// </summary>
  TDrawableIcon = class(TDrawableBase, IInterface, IGlyph, IInterfaceComponentReference)
  private
    FWidth: Integer;
    FHeight: Integer;
    FPadding: Integer;
    FPosition: TDrawablePosition;
    FImageLink: TGlyphImageLink;

    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetPadding(const Value: Integer);
    procedure SetPosition(const Value: TDrawablePosition);
    function GetImages: TCustomImageList;
  protected
    { IInterfaceComponentReference }
    function GetComponent: TComponent;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {IGlyph}
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure ImagesChanged; virtual;
    function GetImageList: TBaseImageList; inline;
    procedure SetImageList(const Value: TBaseImageList);
    function IGlyph.GetImages = GetImageList;
    procedure IGlyph.SetImages = SetImageList;
  protected
    function GetEmpty: Boolean; override;
    function GetStateImageIndex(): Integer; overload;
    function GetStateImageIndex(State: TViewState): Integer; overload; virtual;
  public
    constructor Create(View: IView; const ADefaultKind: TViewBrushKind = TViewBrushKind.None;
      const ADefaultColor: TAlphaColor = TAlphaColors.Null);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    /// <summary>
    /// 绘制，并调整原来的区域
    /// </summary>
    procedure AdjustDraw(Canvas: TCanvas; var R: TRectF; ExecDraw: Boolean; AState: TViewState);

    procedure CreateBrush(var Value: TBrush;
      const ADefaultKind: TViewBrushKind = TViewBrushKind.None;
      const ADefaultColor: TAlphaColor = TAlphaColors.Null); override;

    procedure Draw(Canvas: TCanvas); override;
    procedure DrawStateTo(Canvas: TCanvas; const R: TRectF; AState: TViewState; const AOpacity: Single); override;
    procedure DrawImage(Canvas: TCanvas; Index: Integer; const R: TRectF); overload;
    procedure DrawImage(Canvas: TCanvas; Index: Integer; const R: TRectF; const AOpacity: Single); overload; virtual;
  published
    property SizeWidth: Integer read FWidth write SetWidth default 16;
    property SizeHeight: Integer read FHeight write SetHeight default 16;
    property Padding: Integer read FPadding write SetPadding default 4;
    property Position: TDrawablePosition read FPosition write SetPosition default TDrawablePosition.Left;
    property Images: TCustomImageList read GetImages write SetImages;

    property XRadius;
    property YRadius;
    property Corners;
    property CornerType;
    property Kind;
    property ItemDefault: TBrush index 0 read GetValue write SetValue;
    property ItemPressed: TBrush index 1 read GetValue write SetValue;
    property ItemFocused: TBrush index 2 read GetValue write SetValue;
    property ItemHovered: TBrush index 3 read GetValue write SetValue;
    property ItemSelected: TBrush index 4 read GetValue write SetValue;
    property ItemChecked: TBrush index 5 read GetValue write SetValue;
    property ItemEnabled: TBrush index 6 read GetValue write SetValue;
    property ItemActivated: TBrush index 7 read GetValue write SetValue;
  end;

  /// <summary>
  /// 颜色属性
  /// </summary>
  TViewColor = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FDefault: TAlphaColor;
    FPressed: TAlphaColor;
    FFocused: TAlphaColor;
    FHovered: TAlphaColor;
    FSelected: TAlphaColor;
    FChecked: TAlphaColor;
    FEnabled: TAlphaColor;
    FActivated: TAlphaColor;
    FHintText: TAlphaColor;
    FColorStoreState: Cardinal;
    procedure SetDefault(const Value: TAlphaColor);
    procedure SetActivated(const Value: TAlphaColor);
    procedure SetChecked(const Value: TAlphaColor);
    procedure SetEnabled(const Value: TAlphaColor);
    procedure SetFocused(const Value: TAlphaColor);
    procedure SetHovered(const Value: TAlphaColor);
    procedure SetPressed(const Value: TAlphaColor);
    procedure SetSelected(const Value: TAlphaColor);
    function GetColorStoreState(const Index: Integer): Boolean;
    procedure SetColorStoreState(const Index: Integer; const Value: Boolean);
  private
    function ColorDefaultStored: Boolean;
    function ColorActivatedStored: Boolean;
    function ColorCheckedStored: Boolean;
    function ColorEnabledStored: Boolean;
    function ColorFocusedStored: Boolean;
    function ColorHoveredStored: Boolean;
    function ColorPressedStored: Boolean;
    function ColorSelectedStored: Boolean;
  protected
    procedure DoChange(Sender: TObject);
    function GetValue(const Index: Integer): TAlphaColor;
    procedure SetValue(const Index: Integer; const Value: TAlphaColor);
  public
    constructor Create(const ADefaultColor: TAlphaColor = TAlphaColorRec.Black);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    // 根据当前状态获取颜色，如果颜色为 Null 则返回上一次获取到的颜色
    function GetStateColor(State: TViewState): TAlphaColor;

    function GetColor(State: TViewState): TAlphaColor;
    procedure SetColor(State: TViewState; const Value: TAlphaColor);

    property DefaultChange: Boolean index 1 read GetColorStoreState write SetColorStoreState;
    property PressedChange: Boolean index 2 read GetColorStoreState write SetColorStoreState;
    property FocusedChange: Boolean index 3 read GetColorStoreState write SetColorStoreState;
    property HoveredChange: Boolean index 4 read GetColorStoreState write SetColorStoreState;
    property SelectedChange: Boolean index 5 read GetColorStoreState write SetColorStoreState;
    property CheckedChange: Boolean index 6 read GetColorStoreState write SetColorStoreState;
    property EnabledChange: Boolean index 7 read GetColorStoreState write SetColorStoreState;
    property ActivatedChange: Boolean index 8 read GetColorStoreState write SetColorStoreState;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Default: TAlphaColor read FDefault write SetDefault stored ColorDefaultStored;
    property Pressed: TAlphaColor read FPressed write SetPressed stored ColorPressedStored;
    property Focused: TAlphaColor read FFocused write SetFocused stored ColorFocusedStored;
    property Hovered: TAlphaColor read FHovered write SetHovered stored ColorHoveredStored;
    property Selected: TAlphaColor read FSelected write SetSelected stored ColorSelectedStored;
    property Checked: TAlphaColor read FChecked write SetChecked stored ColorCheckedStored;
    property Enabled: TAlphaColor read FEnabled write SetEnabled stored ColorEnabledStored;
    property Activated: TAlphaColor read FActivated write SetActivated stored ColorActivatedStored;
  end;

  TTextColor = class(TViewColor)
  private
    procedure SetHintText(const Value: TAlphaColor);
    function GetHintText: TAlphaColor;
  published
    property HintText: TAlphaColor read GetHintText write SetHintText default TAlphaColorRec.Gray;
  end;

  /// <summary>
  /// 视图布局属性
  /// </summary>
  TViewLayout = class(TPersistent)
  private
    [Weak] FView: IView;
    FOnChanged: TNotifyEvent;

    FToLeftOf: TControl;
    FToRightOf: TControl;
    FAbove: TControl;
    FBelow: TControl;
    FAlignBaseline: TControl;
    FAlignLeft: TControl;
    FAlignTop: TControl;
    FAlignRight: TControl;
    FAlignBottom: TControl;

    FAlignParentLeft: Boolean;
    FAlignParentTop: Boolean;
    FAlignParentRight: Boolean;
    FAlignParentBottom: Boolean;
    FCenterInParent: Boolean;
    FCenterHorizontal: Boolean;
    FCenterVertical: Boolean;

    procedure SetValue(var Dest: TControl; const Value: TControl); overload;
    procedure SetValue(var Dest: Boolean; const Value: Boolean); overload;
    procedure SetAbove(const Value: TControl);
    procedure SetAlignBaseline(const Value: TControl);
    procedure SetAlignBottom(const Value: TControl);
    procedure SetAlignLeft(const Value: TControl);
    procedure SetAlignRight(const Value: TControl);
    procedure SetAlignTop(const Value: TControl);
    procedure SetBelow(const Value: TControl);
    procedure SetToLeftOf(const Value: TControl);
    procedure SetToRightOf(const Value: TControl);
    procedure SetHeight(const Value: TViewSize);
    procedure SetWidth(const Value: TViewSize);
    procedure SetAlignParentBottom(const Value: Boolean);
    procedure SetAlignParentLeft(const Value: Boolean);
    procedure SetAlignParentRight(const Value: Boolean);
    procedure SetAlignParentTop(const Value: Boolean);
    procedure SetCenterHorizontal(const Value: Boolean);
    procedure SetCenterInParent(const Value: Boolean);
    procedure SetCenterVertical(const Value: Boolean);
    function GetHeight: TViewSize;
    function GetWidth: TViewSize;
  protected
    procedure DoChange(); virtual;
  public
    constructor Create(View: IView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsEmpty: Boolean;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property ToLeftOf: TControl read FToLeftOf write SetToLeftOf;
    property ToRightOf: TControl read FToRightOf write SetToRightOf;
    property Above: TControl read FAbove write SetAbove;
    property Below: TControl read FBelow write SetBelow;
    property AlignBaseline: TControl read FAlignBaseline write SetAlignBaseline;
    property AlignLeft: TControl read FAlignLeft write SetAlignLeft;
    property AlignTop: TControl read FAlignTop write SetAlignTop;
    property AlignRight: TControl read FAlignRight write SetAlignRight;
    property AlignBottom: TControl read FAlignBottom write SetAlignBottom;
    property WidthSize: TViewSize read GetWidth write SetWidth default TViewSize.CustomSize;
    property HeightSize: TViewSize read GetHeight write SetHeight default TViewSize.CustomSize;
    property AlignParentLeft: Boolean read FAlignParentLeft write SetAlignParentLeft default False;
    property AlignParentTop: Boolean read FAlignParentTop write SetAlignParentTop default False;
    property AlignParentRight: Boolean read FAlignParentRight write SetAlignParentRight default False;
    property AlignParentBottom: Boolean read FAlignParentBottom write SetAlignParentBottom default False;
    property CenterInParent: Boolean read FCenterInParent write SetCenterInParent default False;
    property CenterHorizontal: Boolean read FCenterHorizontal write SetCenterHorizontal default False;
    property CenterVertical: Boolean read FCenterVertical write SetCenterVertical default False;
  end;

  /// <summary>
  /// 内容重力
  /// </summary>
  TLayoutGravity = (None, LeftTop, LeftBottom, RightTop, RightBottom,
    CenterVertical, CenterHorizontal, CenterHBottom, CenterVRight, Center);

  /// <summary>
  /// 提示样式
  /// </summary>
  TBadgeStyle = (EmptyText {空白}, NumberText {数字值 (显示数字)},
    NewText {显示New文本}, HotText {显示Hot文本}, Icon {显示指定的图像});

  /// <summary>
  /// 标记提示接口
  /// </summary>
  IViewBadge = interface(IInterface)
    ['{493E5A10-0227-46AE-A17A-3B31D1B04D71}']
    function GetText: string;
    function GetIcon: TBrush;
    function GetValue: Integer;
    function GetMaxValue: Integer;
    function GetStyle: TBadgeStyle;
    procedure SetValue(const Value: Integer);
    procedure SetMaxValue(const Value: Integer);
    procedure SetStyle(const Value: TBadgeStyle);

    procedure Realign;
    procedure SetVisible(const Value: Boolean);
    function GetEnabled: Boolean;

    property Value: Integer read GetValue write SetValue;
    property MaxValue: Integer read GetMaxValue write SetMaxValue;
    property Style: TBadgeStyle read GetStyle write SetStyle;
  end;

  /// <summary>
  /// 视图布局属性接口
  /// </summary>
  IView = interface(IInterface)
    ['{9C2D9DB0-9D59-4A9D-BC47-53928194544E}']
    function GetBackground: TDrawable;
    function GetLayout: TViewLayout;
    function GetParentControl: TControl;
    function GetParentView: IViewGroup;
    function GetAdjustViewBounds: Boolean;
    function GetGravity: TLayoutGravity;
    function GetMaxHeight: Single;
    function GetMaxWidth: Single;
    function GetMinHeight: Single;
    function GetMinWidth: Single;
    function GetWeight: Single;
    function GetViewStates: TViewStates;
    function GetDrawState: TViewState;
    function GetHeightSize: TViewSize;
    function GetWidthSize: TViewSize;
    function GetOrientation: TOrientation;
    function GetComponent: TComponent;
    function GetComponentState: TComponentState;
    function GetInVisible: Boolean;
    function GetBadgeView: IViewBadge;

    function GetPosition: TPosition;
    function GetWidth: Single;
    function GetHeight: Single;
    function GetOpacity: Single;

    function IsAutoSize: Boolean;

    function LocalToAbsolute(const Point: TPointF): TPointF;

    procedure IncViewState(const State: TViewState);
    procedure DecViewState(const State: TViewState);

    procedure SetLayout(const Value: TViewLayout);
    procedure SetBackground(const Value: TDrawable);
    procedure SetWeight(const Value: Single);
    procedure SetGravity(const Value: TLayoutGravity);
    procedure SetOrientation(const Value: TOrientation);
    procedure SetMaxHeight(const Value: Single);
    procedure SetMaxWidth(const Value: Single);
    procedure SetMinHeight(const Value: Single);
    procedure SetMinWidth(const Value: Single);
    procedure SetAdjustViewBounds(const Value: Boolean);
    procedure SetHeightSize(const Value: TViewSize);
    procedure SetWidthSize(const Value: TViewSize);
    procedure SetBadgeView(const Value: IViewBadge);

    property Layout: TViewLayout read GetLayout write SetLayout;
    property Background: TDrawable read GetBackground write SetBackground;
    property Weight: Single read GetWeight write SetWeight;
    property Gravity: TLayoutGravity read GetGravity write SetGravity;
    property Orientation: TOrientation read GetOrientation write SetOrientation;
    property MaxHeight: Single read GetMaxHeight write SetMaxHeight;
    property MaxWidth: Single read GetMaxWidth write SetMaxWidth;
    property MinHeight: Single read GetMinHeight write SetMinHeight;
    property MinWidth: Single read GetMinWidth write SetMinWidth;
    property AdjustViewBounds: Boolean read GetAdjustViewBounds write SetAdjustViewBounds;
    property HeightSize: TViewSize read GetHeightSize write SetHeightSize;
    property WidthSize: TViewSize read GetWidthSize write SetWidthSize;
    property BadgeView: IViewBadge read GetBadgeView write SetBadgeView;

    property Opacity: Single read GetOpacity;
    property Width: Single read GetWidth;
    property Height: Single read GetHeight;
    property Position: TPosition read GetPosition;
    property ParentControl: TControl read GetParentControl;
    property ParentView: IViewGroup read GetParentView;
    property InVisible: Boolean read GetInVisible;
  end;

  /// <summary>
  /// 视图组接口
  /// </summary>
  IViewGroup = interface(IView)
    ['{73A1B9E5-D4AF-4956-A15F-73B0B8EDADF9}']
    function AddView(View: TView): Integer;
    function RemoveView(View: TView): Integer;
    function GetAbsoluteInVisible: Boolean;
  end;

  /// <summary>
  /// 触摸事件支持
  /// </summary>
  IViewTouch = interface(IInterface)
    ['{ADA36492-479A-468E-A813-59CC1940612A}']
    function IsCanTouch: Boolean;
  end;

  TTextSettingsBase = class(TPersistent)
  private
    [Weak] FOwner: TControl;
    FOnChanged: TNotifyEvent;
    FOnTextChanged: TNotifyEvent;
    FOnLastFontChanged: TNotifyEvent;
    FLayout: TTextLayout;
    FPrefixStyle: TPrefixStyle;
    FGravity: TLayoutGravity;
    FTrimming: TTextTrimming;
    FText: string;
    FAutoSize: Boolean;
    FIsSizeChange: Boolean;
    FIsTextChange: Boolean;
    FIsEffectsChange: Boolean;
    FIsColorChange: Boolean;
    function GetGravity: TLayoutGravity;
    function GetWordWrap: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetGravity(const Value: TLayoutGravity);
    procedure SetPrefixStyle(const Value: TPrefixStyle);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetAutoSize(const Value: Boolean);
    function GetFillTextFlags: TFillTextFlags;
    function GetHorzAlign: TTextAlign;
    function GetVertAlign: TTextAlign;
    procedure SetHorzVertValue(const H, V: TTextAlign);
    procedure SetHorzAlign(const Value: TTextAlign);
    procedure SetVertAlign(const Value: TTextAlign);
    function GetTextLength: Integer;
    function GetFont: TFont;
  protected
    procedure DoChange; virtual;
    procedure DoTextChanged;
    procedure DoFontChanged(Sender: TObject);
    procedure DoColorChanged(Sender: TObject);
    function IsStoredGravity: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Change;

    procedure Assign(Source: TPersistent); override;

    function GetStateColor(const State: TViewState): TAlphaColor; virtual; abstract;

    function CalcTextObjectSize(const AText: string; const MaxWidth, SceneScale: Single;
      const Margins: TBounds; var Size: TSizeF): Boolean;

    function CalcTextWidth(const AText: string; SceneScale: Single): Single;
    function CalcTextHeight(const AText: string; SceneScale: Single): Single;

    procedure FillText(const Canvas: TCanvas; const ARect: TRectF; const AText: string; const AOpacity: Single;
      const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; State: TViewState = TViewState.None); overload;

    procedure FillText(const Canvas: TCanvas; const ARect: TRectF; const AText: string; const AOpacity: Single;
      const AColor: TAlphaColor; const Flags: TFillTextFlags; ASize: PSizeF; const SceneScale: Single;
      const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.Center;
      State: TViewState = TViewState.None); overload;

    // 计算 Text 真实大小
    procedure TextSize(const AText: string; var ASize: TSizeF; const SceneScale: Single;
      const MaxWidth: Single = -1; AWordWrap: Boolean = False);

    procedure Draw(const Canvas: TCanvas; const R: TRectF;
        const Opacity: Single; State: TViewState); overload;
    procedure Draw(const Canvas: TCanvas; const AText: string; const R: TRectF;
        const Opacity: Single; State: TViewState); overload;
    procedure Draw(const Canvas: TCanvas; const AText: string; const R: TRectF;
        const Opacity: Single; State: TViewState; AGravity: TLayoutGravity); overload;

    property IsColorChange: Boolean read FIsColorChange write FIsColorChange;
    property IsSizeChange: Boolean read FIsSizeChange write FIsSizeChange;
    property IsTextChange: Boolean read FIsTextChange write FIsTextChange;
    property IsEffectsChange: Boolean read FIsEffectsChange;

    property Text: string read FText write SetText;
    property TextLength: Integer read GetTextLength;
    property FillTextFlags: TFillTextFlags read GetFillTextFlags;

    property HorzAlign: TTextAlign read GetHorzAlign write SetHorzAlign;
    property VertAlign: TTextAlign read GetVertAlign write SetVertAlign;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnTextChanged: TNotifyEvent read FOnTextChanged write FOnTextChanged;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Font: TFont read GetFont write SetFont;
    property PrefixStyle: TPrefixStyle read FPrefixStyle write SetPrefixStyle default TPrefixStyle.NoPrefix;
    property Trimming: TTextTrimming read FTrimming write SetTrimming default TTextTrimming.None;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property Gravity: TLayoutGravity read GetGravity write SetGravity stored IsStoredGravity;
  end;

  /// <summary>
  /// 字体设置
  /// </summary>
  TTextSettings = class(TTextSettingsBase)
  private
    FColor: TViewColor;
    FOpacity: Single;
    procedure SetColor(const Value: TViewColor);
    function IsStoreOpacity: Boolean;
    procedure SetOpacity(const Value: Single);
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetStateColor(const State: TViewState): TAlphaColor; override;
  published
    property AutoSize;
    property Color: TViewColor read FColor write SetColor;
    property Font;
    property PrefixStyle;
    property Trimming;
    property WordWrap;
    property Gravity;
    property Opacity: Single read FOpacity write SetOpacity stored IsStoreOpacity;
  end;

  /// <summary>
  /// Html解析中间结果 (仅支持字体颜色、样式部分)
  /// </summary>
  THtmlTextItem = record
    P: PChar;
    Len: Cardinal;
    Color: TAlphaColor;
    Style: TFontStyles;
    Link: SmallInt;  // 超链接索引号
    LinkURL: SmallInt; // 超链接地址
    NoneTag: Boolean;  // 是否存在标签
    function Text: string;
  end;

  THtmlDataList = TList<THtmlTextItem>;

  TViewLinkClickEvent = procedure (Sender: TObject; const Text, URL: string) of object;

  TViewHtmlText = class(TPersistent)
  private const
    PLineBreak: PChar = #13;
    PStyle = 'style';
  private
    FList: THtmlDataList;
    FText: string;
    FHtmlText: string;
    FRealHtmlText: string;
    FFont: TFont;
    FReplace: Boolean;

    FLinkHrefs: TStrings;
    FLinkRange: TArray<TRectF>;
    FLinkRangeCount: Integer;
    FLinkHot: Integer;

    FDefaultCursor: TCursor;

    procedure SetHtmlText(const Value: string);
  protected
    procedure ParseHtmlText(const Text: string); virtual;
    function GetHtmlText: string; virtual;
    function ReplaceValue(const Value: string): string;
    function PointInLink(const X, Y: Single): Integer;
  public
    constructor Create(const AHtmlText: string = '');
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    // 绘制到画布中
    procedure Draw(Canvas: TCanvas; TextSet: UI.Base.TTextSettings; const R: TRectF;
      const Opacity: Single; State: TViewState; ASize: PSizeF = nil);

    // 计算大小
    procedure CalcTextSize(Canvas: TCanvas; TextSet: UI.Base.TTextSettings; const R: TRectF;
      var ASize: TSizeF);

    procedure MouseDown(Sender: TView; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Sender: TView; X, Y: Single);
    procedure MouseUp(Sender: TView; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseLeave(Sender: TView);

    property Text: string read FText;
    property List: THtmlDataList read FList;
    property LinkHot: Integer read FLinkHot;
    property DefaultCursor: TCursor read FDefaultCursor write FDefaultCursor;
  published
    property HtmlText: string read FHtmlText write SetHtmlText;
  end;

  TAniCalculationsEx = class (TAniCalculations)
  private
    function GetDownPoint: TPointD;
  public
    property DownPoint: TPointD read GetDownPoint;
    property Shown;
    property MouseTarget;
    property MinTarget;
    property MaxTarget;
    property Target;
  end;

  /// <summary>
  /// 滚动动画控制器
  /// </summary>
  TScrollCalculations = class (TAniCalculationsEx)
  private
    [Weak] FScrollView: TView;
  protected
    procedure DoChanged; override;
    procedure DoStart; override;
    procedure DoStop; override;
  public
    constructor Create(AOwner: TPersistent); override;
    property ScrollView: TView read FScrollView;
  end;

  TScrollBarHelper = class Helper for TScrollBar
  private
    function GetMaxD: Double;
    function GetMinD: Double;
    function GetValueD: Double;
    procedure SetMaxD(const Value: Double);
    procedure SetMinD(const Value: Double);
    procedure SetValueD(const Value: Double);
    function GetViewportSizeD: Double;
    procedure SetViewportSizeD(const Value: Double);
  public
    property MinD: Double read GetMinD write SetMinD;
    property MaxD: Double read GetMaxD write SetMaxD;
    property ValueD: Double read GetValueD write SetValueD;
    property ViewportSizeD: Double read GetViewportSizeD write SetViewportSizeD;
  end;

  TCustomTrackHelper = class Helper for TCustomTrack
  private
    function GetMaxD: Double;
    function GetMinD: Double;
    function GetValueD: Double;
    procedure SetMaxD(const Value: Double);
    procedure SetMinD(const Value: Double);
    procedure SetValueD(const Value: Double);
    function GetViewportSizeD: Double;
    procedure SetViewportSizeD(const Value: Double);
  public
    property MinD: Double read GetMinD write SetMinD;
    property MaxD: Double read GetMaxD write SetMaxD;
    property ValueD: Double read GetValueD write SetValueD;
    property ViewportSizeD: Double read GetViewportSizeD write SetViewportSizeD;
  end;

  TViewBase = class(TControl)
  protected
    function GetBackground: TDrawable; virtual;
    function GetViewBackground: TDrawable; virtual;
    function GetMaxHeight: Single; virtual;
    function GetMaxWidth: Single; virtual;
    function GetMinHeight: Single; virtual;
    function GetMinWidth: Single; virtual;
    function GetViewStates: TViewStates; virtual;
    procedure SetBackgroundBase(const Value: TDrawable); virtual; abstract;
    procedure SetMaxHeight(const Value: Single); virtual; abstract;
    procedure SetMaxWidth(const Value: Single); virtual; abstract;
    procedure SetMinHeight(const Value: Single); virtual; abstract;
    procedure SetMinWidth(const Value: Single); virtual; abstract;
    procedure SetViewStates(const Value: TViewStates); virtual; abstract;
  public
    property Background: TDrawable read GetViewBackground write SetBackgroundBase;
    property MinWidth: Single read GetMinWidth write SetMinWidth;
    property MinHeight: Single read GetMinHeight write SetMinHeight;
    property MaxWidth: Single read GetMaxWidth write SetMaxWidth;
    property MaxHeight: Single read GetMaxHeight write SetMaxHeight;
    property ViewState: TViewStates read GetViewStates write SetViewStates;
  end;

  /// <summary>
  /// 基本视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TView = class(TViewBase, IView)
  const
    SmallChangeFraction = 5;
  private
    FInvaliding: Boolean;
    FRecalcInVisible: Boolean;
    FAbsoluteInVisible: Boolean;
    {$IFDEF MSWINDOWS}
    FCaptureDragForm: Boolean;
    {$ENDIF}
    function GetParentView: IViewGroup;
    function GetClickable: Boolean;
    procedure SetClickable(const Value: Boolean);
    function GetPaddings: string;
    procedure SetPaddings(const Value: string);
    function GetMargin: string;
    procedure SetMargin(const Value: string);
    procedure SetWeight(const Value: Single);
    procedure SetOrientation(const Value: TOrientation);
    procedure SetAdjustViewBounds(const Value: Boolean);
    function GetLayout: TViewLayout;
    procedure SetLayout(const Value: TViewLayout);
    procedure SetHeightSize(const Value: TViewSize);
    procedure SetWidthSize(const Value: TViewSize);
    function GetAdjustViewBounds: Boolean;
    function GetGravity: TLayoutGravity;
    function GetWeight: Single;
    function GetOrientation: TOrientation;
    function GetComponent: TComponent;
    function GetComponentState: TComponentState;
    function GetOpacity: Single;
    function GetParentControl: TControl;
    function GetPosition: TPosition;
    function GetInVisible: Boolean;
    procedure SetInVisible(const Value: Boolean);
    procedure SetTempMaxHeight(const Value: Single);
    procedure SetTempMaxWidth(const Value: Single);
    function GetIsChecked: Boolean;
    procedure SetIsChecked(const Value: Boolean);
    function GetHeightSize: TViewSize;
    function GetWidthSize: TViewSize;
    function GetCaptureDragForm: Boolean;
    procedure SetCaptureDragForm(const Value: Boolean);
    function GetParentForm: TCustomForm;
  protected
    function GetViewRect: TRectF;
    function GetViewRectD: TRectD;
    function GetMaxHeight: Single; override;
    function GetMaxWidth: Single; override;
    function GetMinHeight: Single; override;
    function GetMinWidth: Single; override;
    function GetViewStates: TViewStates; override;
    function GetBackground: TDrawable; override;
    function GetViewBackground: TDrawable; override;
    function DoGetUpdateRect: TRectF; override;
    procedure SetMaxHeight(const Value: Single); override;
    procedure SetMaxWidth(const Value: Single); override;
    procedure SetMinHeight(const Value: Single); override;
    procedure SetMinWidth(const Value: Single); override;
    procedure SetViewStates(const Value: TViewStates); override;
    procedure SetBackgroundBase(const Value: TDrawable); override;
  protected
    FWeight: Single;
    FInVisible: Boolean;
    FGravity: TLayoutGravity;
    FOrientation: TOrientation;
    FBackground: TDrawable;
    FDrawing: Boolean;
    FViewState: TViewStates;
    FDrawState: TViewState;
    FMinWidth: Single;
    FMinHeight: Single;
    FMaxWidth: Single;
    FMaxHeight: Single;
    FWidthSize: TViewSize;
    FHeightSize: TViewSize;
    FSaveMaxWidth: Single;
    FSaveMaxHeight: Single;
    {$IFNDEF MSWINDOWS}
    FDownUpOffset: Single;
    {$ENDIF}
    FLayout: TViewLayout;
    [Weak] FBadgeView: IViewBadge;
    function IsDrawing: Boolean;
    function IsDesignerControl(Control: TControl): Boolean;
    function IsAutoSize: Boolean; virtual;
    function IsAdjustLayout: Boolean; virtual;
    function GetBadgeView: IViewBadge;
    procedure SetBadgeView(const Value: IViewBadge);
    function EmptyBackground(const V: TDrawable; const State: TViewState): Boolean;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; virtual;
    procedure IncViewState(const State: TViewState); virtual;
    procedure DecViewState(const State: TViewState); virtual;
    procedure IncChildState(State: TViewState); virtual;  // 给所有子控件增加状态
    procedure DecChildState(State: TViewState); virtual;  // 给所有子控件减少状态
    procedure DoActivate; override;
    procedure DoDeactivate; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure EnabledChanged; override;
    procedure HitTestChanged; override;
    procedure VisibleChanged; override;
    function DoSetSize(const ASize: TControlSize; const NewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
      var ALastWidth, ALastHeight: Single): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  protected
    FAdjustViewBounds: Boolean;
    procedure Paint; override;
    procedure AfterPaint; override;
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    procedure DoOrientation; virtual;
    procedure DoGravity; virtual;
    procedure DoWeight; virtual;
    procedure DoMaxSizeChange; virtual;
    procedure DoMinSizeChange; virtual;
    procedure DoInVisibleChange; virtual;
    procedure DoBackgroundChanged(Sender: TObject); virtual;
    procedure DoCheckedChange(); virtual;
    procedure DoEndUpdate; override;
    procedure DoMatrixChanged(Sender: TObject); override;
    procedure DoLinkClick(const Text, URL: string); virtual;
    procedure HandleSizeChanged; override;
    procedure Click; override;
    procedure DoClickEvent; virtual;


    // 限制组件最大和最小大小
    procedure DoAdjustViewBounds(var ANewWidth, ANewHeight: Single); virtual;
    // 布局变化了
    procedure DoLayoutChanged(Sender: TObject); virtual;
    // 大小改变了
    procedure DoChangeSize(var ANewWidth, ANewHeight: Single); virtual;
    // 开始计算大小
    procedure DoRecalcSize(var AWidth, AHeight: Single); virtual;

    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure PaintBackground; virtual;
    procedure SetGravity(const Value: TLayoutGravity); virtual;
    function AllowUseLayout(): Boolean; virtual;
    procedure ImagesChanged; virtual;
    function CreateBackground: TDrawable; virtual;
    function GetParentMaxWidth: Single;
    function GetParentMaxHeight: Single;
    function GetRealDrawState: TViewState; virtual;
    function GetDrawState: TViewState;
    function GetAbsoluteInVisible: Boolean; virtual;
    procedure RecalcInVisible; virtual;
  protected
    FScrollbar: TViewScroll;
    FDisableMouseWheel: Boolean;
    procedure SetScrollbar(const Value: TViewScroll); virtual;
    function GetSceneScale: Single;
    function GetAniCalculations: TScrollCalculations; virtual;
    procedure StartScrolling;
    procedure StopScrolling;
    procedure InternalAlign; virtual;
    procedure FreeScrollbar; virtual;
    procedure InitScrollbar; virtual;
    procedure SetDisableMouseWheel(const Value: Boolean);
    procedure DoSetScrollBarValue(Scroll: TScrollBar; const Value, ViewportSize: Double); virtual;
    procedure UpdateVScrollBar(const Value: Double; const ViewportSize: Double);
    procedure UpdateHScrollBar(const Value: Double; const ViewportSize: Double);
    function GetVScrollBar: TScrollBar; virtual;
    function GetHScrollBar: TScrollBar; virtual;
    function GetContentBounds: TRectD; virtual;
    function CanAnimation: Boolean; virtual;
    function GetScrollSmallChangeFraction: Single; virtual;
  protected
    {$IFDEF ANDROID}
    class procedure InitAudioManager();
    class procedure InitFontGlyphs();
    {$ENDIF}
  public
    /// <summary>
    /// 滚动条样式
    /// </summary>
    property ScrollBars: TViewScroll read FScrollbar write SetScrollbar default TViewScroll.None;
    /// <summary>
    /// 禁止鼠标滚动
    /// </summary>
    property DisableMouseWheel: Boolean read FDisableMouseWheel write SetDisableMouseWheel default False;
    /// <summary>
    /// 滚动动画控制器
    /// </summary>
    property AniCalculations: TScrollCalculations read GetAniCalculations;

    property HScrollBar: TScrollBar read GetHScrollBar;
    property VScrollBar: TScrollBar read GetVScrollBar;
    property ContentBounds: TRectD read GetContentBounds;
    property AbsoluteInVisible: Boolean read GetAbsoluteInVisible;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PointInObject(X, Y: Single): Boolean; override;
    procedure PlaySoundEffect(ASoundConstant: Integer);
    procedure PlayClickEffect(); virtual;

    procedure Invalidate;
    procedure DoResize;

    {$IFDEF ANDROID}
    class function AppContext: JContext;
    {$ENDIF}

    procedure SetBackground(const Value: TDrawable); overload;
    procedure SetBackground(const Value: TAlphaColor); overload;
    procedure SetBackground(const Value: TGradient); overload;
    procedure SetBackground(const Value: TBitmap); overload;
    procedure SetBackground(const Value: TBrushBitmap); overload;

    function IsPressed: Boolean;
    function IsHovered: Boolean;
    function IsActivated: Boolean;

    function FindStyleResource<T: TFmxObject>(const AStyleLookup: string; var AResource: T): Boolean; overload;
    function FindAndCloneStyleResource<T: TFmxObject>(const AStyleLookup: string; var AResource: T): Boolean;

    { ITriggerEffect }
    procedure ApplyTriggerEffect(const AInstance: TFmxObject; const ATrigger: string); override;
    { ITriggerAnimation }
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
    procedure StartTriggerAnimationWait(const AInstance: TFmxObject; const ATrigger: string); override;

    /// <summary>
    /// 开始拖动窗口
    /// </summary>
    procedure StartWindowDrag;

    /// <summary>
    /// 获取状态栏高度
    /// </summary>
    class function GetStatusHeight: Single;
    /// <summary>
    /// 获取底部虚拟键高度
    /// </summary>
    class function GetNavigationBarHeight: Single;

    { Rtti Function }
    class function ExistRttiValue(Instance: TObject; const Name: string): Boolean;
    class function GetRttiValue(Instance: TObject; const Name: string): TValue; overload;
    class function GetRttiValue<T>(Instance: TObject; const Name: string): T; overload;
    class function GetRttiObject(Instance: TObject; const Name: string): TObject;
    class procedure SetRttiValue(Instance: TObject; const Name: string; const Value: TValue); overload;
    class procedure SetRttiValue<T>(Instance: TObject; const Name: string; const Value: T); overload;
    class procedure SetRttiObject(Instance: TObject; const Name: string; const Value: TObject); overload;
    class procedure InvokeMethod(Instance: TObject; const Name: string; const Args: array of TValue); overload;
    class function InvokeMethod<T>(Instance: TObject; const Name: string; const Args: array of TValue): T; overload;

    property ParentView: IViewGroup read GetParentView;
    /// <summary>
    /// 组件的布局方式。Horizontal，水平布局； Vertical，垂直布局。默认为Horizontal。
    /// </summary>
    property Orientation: TOrientation read GetOrientation write SetOrientation;
    /// <summary>
    /// 组件当前的状态
    /// </summary>
    property ViewState;
    /// <summary>
    /// 组件当前的绘制状态
    /// </summary>
    property DrawState: TViewState read FDrawState;
    /// <summary>
    /// 组件内容有效区域（返回减去Padding后的值）
    /// </summary>
    property ViewRect: TRectF read GetViewRect;
    property ViewRectD: TRectD read GetViewRectD;

    /// <summary>
    /// 临时最大高度, 设置为0时，恢复原始的MaxHeight
    /// </summary>
    property TempMaxHeight: Single read FMaxHeight write SetTempMaxHeight;
    /// <summary>
    /// 临时最大高度, 设置为0时，恢复原始的MaxWidth
    /// </summary>
    property TempMaxWidth: Single read FMaxWidth write SetTempMaxWidth;
    /// <summary>
    /// 提示标记视图
    /// </summary>
    property BadgeView: IViewBadge read FBadgeView;
    /// <summary>
    /// 获取父级Form
    /// </summary>
    property ParentForm: TCustomForm read GetParentForm;

    property IsChecked: Boolean read GetIsChecked write SetIsChecked;
  published
    /// <summary>
    /// 组件相对于容器的对齐方式。当容器为非布局组件时有效，在部分布局组件中有效，但不建议使用。
    /// </summary>
    property Align;
    /// <summary>
    /// 组件在容器中的缩放和定位方式。当容器为非布局组件时有效。
    /// </summary>
    property Anchors;
    /// <summary>
    /// 是否允许根据MaxWidth, MaxHeight, MinWidth, MinHeight属性来限制组件大小
    /// </summary>
    property AdjustViewBounds: Boolean read GetAdjustViewBounds write SetAdjustViewBounds default True;
    /// <summary>
    /// 视图背景。视图背景是一个TDrawable对象，可通过设置此属性的子项，来实现不同的显示效果，详见TDrawable的属性说明。
    /// </summary>
    property Background;
    /// <summary>
    /// 是否响应点击事件。同HitTest属性
    /// </summary>
    property Clickable: Boolean read GetClickable write SetClickable default False;
    /// <summary>
    /// 是否剪切超出组件可视区域的图形输出
    /// </summary>
    property ClipChildren default True;
    /// <summary>
    /// 是否选中
    /// </summary>
    property Checked: Boolean read GetIsChecked write SetIsChecked default False;
    /// <summary>
    /// 是否允许捕获拖动主窗口
    /// </summary>
    property CaptureDragForm: Boolean read GetCaptureDragForm write SetCaptureDragForm default False;
    /// <summary>
    /// 是否执行动作操作
    /// </summary>
    property EnableExecuteAction default False;
    /// <summary>
    /// 相对布局属性。当容器是TRelativeLayout相对布局时有效。Layout是一个TViewLayout对象，详请参考TViewLayout属性说明。
    /// </summary>
    property Layout: TViewLayout read GetLayout write SetLayout;
    /// <summary>
    /// 组件内容四周留白大小。会自动设置Padding的四边会相同的值。
    /// </summary>
    property Paddings: string read GetPaddings write SetPaddings stored False;
    /// <summary>
    /// 布局时与其它组件四周的距离。此属性是一个字符串形式的浮点数，用于一次设置Margins的四边为相同的大小。
    /// </summary>
    property Margin: string read GetMargin write SetMargin stored False;
    /// <summary>
    /// 组件是否可视。Visible 为 True 时有效，InVisible 为 True 时，只显位置不显示内容
    /// </summary>
    property InVisible: Boolean read FInVisible write SetInVisible default False;
    /// <summary>
    /// 组件宽度调节方式，CustomSize, 指定的固定大小; WrapContent 随内容决定； FillParent，填充容器。
    /// </summary>
    property WidthSize: TViewSize read FWidthSize write SetWidthSize default TViewSize.CustomSize;
    /// <summary>
    /// 组件高度调节方式，CustomSize, 指定的固定大小; WrapContent 随内容决定； FillParent，填充容器。
    /// </summary>
    property HeightSize: TViewSize read FHeightSize write SetHeightSize default TViewSize.CustomSize;
    /// <summary>
    /// 组件的最小宽度。当AdjustViewBounds为True时有效。
    /// </summary>
    property MinWidth;
    /// <summary>
    /// 组件的最小高度。当AdjustViewBounds为True时有效。
    /// </summary>
    property MinHeight;
    /// <summary>
    /// 组件的最大宽度。当AdjustViewBounds为True时有效。
    /// </summary>
    property MaxWidth;
    /// <summary>
    /// 组件的最大高度。当AdjustViewBounds为True时有效。
    /// </summary>
    property MaxHeight;
    /// <summary>
    /// 组件本身作为容器时，内部的重力。也就是子组件的位于容器的位置。
    ///    LeftTop, 左上角;
    ///    LeftBottom, 左下角;
    ///    RightTop, 右上角;
    ///    RightBottom, 右下角;
    ///    CenterVertical, 垂直居中（可左右移动）;
    ///    CenterHorizontal, 水平居中（可上下移动）;
    ///    CenterHBottom, 底部水平居中;
    ///    CenterVRight, 靠右垂直居中;
    ///    Center, 完全居中;
    /// </summary>
    property Gravity: TLayoutGravity read GetGravity write SetGravity;
    /// <summary>
    /// 视图在线性布局 TLinearLayout 时，其宽度或高度在容器中所占的大小比例。
    /// 设为>0时，布局组件会按比例自动调整组件大小。只有容器是TLinearLayout时有效。
    /// </summary>
    property Weight: Single read GetWeight write SetWeight;

    property Action;
    property Cursor;
    property ClipParent;
    property Enabled;
    property Locked;
    property Opacity;
    property RotationAngle;
    property RotationCenter;
    property Padding;
    property Margins;
    property PopupMenu;
    property Visible;
    property HitTest default False;
    property Width;
    property Height;
    property Scale;
    property Size;
    property Position;
    property TabOrder;
    property TabStop;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  /// <summary>
  /// 基本视图组
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TViewGroup = class(TView, IViewGroup)
  private
  protected
    /// <summary>
    /// 是否需要自动调整大小
    /// </summary>
    function IsAdjustSize(View: IView; Align: TAlignLayout;
      AParentOrientation: TOrientation): Boolean;

    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoLayoutChanged(Sender: TObject); override;
    procedure DoGravity(); override;
    procedure DoMaxSizeChange; override;
    procedure DoMinSizeChange; override;
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddView(View: TView): Integer;
    function RemoveView(View: TView): Integer;
  end;

  /// <summary>
  /// 线性布局
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TLinearLayout = class(TViewGroup)
  private
  protected
    /// <summary>
    /// 查找最后一个需要自动调整大小的组件
    /// <param name="AControl">输出需要自动调整大小的组件</param>
    /// <param name="AdjustSize">输出自动调整大小组件的可用空间</param>
    /// </summary>
    function AdjustAutoSizeControl(out AControl: TControl; out AdjustSize: Single): Boolean;

    function GetWeightSum(var FixSize: Single): Single;
    function GetLastWeightView(): TView;
    function IsUseWeight(): Boolean;
    procedure DoRealign; override;
    procedure DoOrientation; override;
    procedure DoRecalcSize(var AWidth, AHeight: Single); override;
  public
  published
    property Orientation;
  end;

  /// <summary>
  /// 相对布局
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TRelativeLayout = class(TViewGroup)
  private
    FViewList: TList<TControl>;
    procedure DoAlignControl(const X, Y, W, H: Single);
  protected
    function GetXY(const StackList: TList<TControl>; const Control: TControl;
      var X, Y, W, H: Single): Integer;
    procedure DoRealign; override;
    procedure DoRecalcSize(var AWidth, AHeight: Single); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  /// <summary>
  /// 格子布局时拉伸模式
  /// </summary>
  TViewStretchMode = (None {无},
    SpacingWidth {自动调整间距，使充满布局},
    ColumnWidth {自动调整宽度，使充满布局},
    SpacingWidthUniform {自动调整间距(平均间隔)，使充满布局});

  /// <summary>
  /// 格子布局
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TGridsLayout = class(TViewGroup)
  private const
    CDefaultColumnWidth = 50;
    CDefaultColumnHeight = 50;
    CDefaultDividerColor = $FFF2E9E6;
  private
    FNumColumns: Integer;
    FColumnWidth: Single;
    FColumnHeight: Single;

    FSpacingBorder: Boolean;
    FVerticalSpacing: Single;
    FHorizontalSpacing: Single;

    FStretchMode: TViewStretchMode;
    FForceColumnSize: Boolean;

    FLastRH, FLastCW, FLastPW: Single;
    FLastColumns, FLastRows: Integer;
    FLastStretchMode: TViewStretchMode;

    FDividerBrush: TBrush;

    procedure SetNumColumns(const Value: Integer);
    function IsStoredColumnWidth: Boolean;
    procedure SetColumnWidth(const Value: Single);
    procedure SetHorizontalSpacing(const Value: Single);
    procedure SetVerticalSpacing(const Value: Single);
    procedure SetStretchMode(const Value: TViewStretchMode);
    function IsStoredColumnHeight: Boolean;
    procedure SetColumnHeight(const Value: Single);
    procedure SetDivider(const Value: TAlphaColor);
    function GetAbsoluteColumnsNum: Integer;
    function GetCount: Integer;
    function GetDivider: TAlphaColor;
    procedure SetForceColumnSize(const Value: Boolean);
    procedure SetSpacingBorder(const Value: Boolean);
  protected
    procedure DoRealign; override;
    procedure PaintBackground; override;
    procedure DrawDivider(Canvas: TCanvas);   // 画分隔线
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// 格子数量
    /// </summary>
    property Count: Integer read GetCount;
    /// <summary>
    /// 决对的列数
    /// </summary>
    property AbsoluteColumnsNum: Integer read GetAbsoluteColumnsNum;
  published
    /// <summary>
    /// 列数, <= 0 时为自动
    /// </summary>
    property ColumnCount: Integer read FNumColumns write SetNumColumns default 0;
    /// <summary>
    /// 列宽度
    /// </summary>
    property ColumnWidth: Single read FColumnWidth write SetColumnWidth stored IsStoredColumnWidth;
    /// <summary>
    /// 列高度
    /// </summary>
    property ColumnHeight: Single read FColumnHeight write SetColumnHeight stored IsStoredColumnHeight;
    /// <summary>
    /// 分隔线颜色
    /// </summary>
    property Divider: TAlphaColor read GetDivider write SetDivider default CDefaultDividerColor;
    /// <summary>
    /// 两列之间的间距
    /// </summary>
    property SpacingHorizontal: Single read FHorizontalSpacing write SetHorizontalSpacing;
    /// <summary>
    /// 两行之间的间距
    /// </summary>
    property SpacingVertical: Single read FVerticalSpacing write SetVerticalSpacing;
    /// <summary>
    /// 间距从边框开始算 (为 False 时，左右上下四边的间距为0)
    /// </summary>
    property SpacingBorder: Boolean read FSpacingBorder write SetSpacingBorder default True;
    /// <summary>
    /// 缩放与列宽大小调整方式
    /// </summary>
    property StretchMode: TViewStretchMode read FStretchMode write SetStretchMode default TViewStretchMode.None;
    /// <summary>
    /// 强制使用列大小。此时不再检测每个格子的宽度高度是否需要自动大小
    /// </summary>
    property ForceColumnSize: Boolean read FForceColumnSize write SetForceColumnSize default False;
  end;


// 处理消息
procedure ProcessMessages;
// 模拟点击
procedure SimulateClick(AControl: TControl; const x, y: single);
// 替换不透明颜色
procedure ReplaceOpaqueColor(ABmp: TBitmap; const Color: TAlphaColor);
// 屏幕缩放
function GetScreenScale: single;
// 获取组件所属的窗口
function GetParentForm(AObj: TFmxObject): TCustomForm;

function ViewStateToString(const State: TViewStates): string;
function ComponentStateToString(const State: TComponentState): string;

var
  /// <summary>
  /// Accessory 图像列表
  /// </summary>
  FAccessoryImages: TViewAccessoryImageList;

implementation

uses
  {$IFDEF ANDROID}
  UI.FontGlyphs.Android,
  {$ENDIF}
  UI.Ani;

resourcestring
  SInvViewValue = '无效的视图状态值: %d';
  SNotAllowSelf = '不允许设定为自己';
  SMustSameParent = '必须指定一个与当前组件所属视图中的同级兄弟组件';
  SLocateFailed = '存在循环引用';
  SRefOutLimitMax = '组件引用层级超过上限值: 256';
  SUnsupportPropertyType = '不支持的属性类型.';

var
  /// <summary>
  /// APP 状态条高度 (Android 平台有效)
  /// </summary>
  StatusHeight: Single = 0;
  /// <summary>
  /// APP 底部虚拟键高度 (Android 平台有效)
  /// </summary>
  NavigationBarHeight: Single = 0;
  {$IFDEF ANDROID}
  FAudioManager: JAudioManager = nil;
  {$ENDIF}

function AlignToPixel(Canvas: TCanvas; const Rect: TRectF): TRectF;
begin
  Result.Left := Canvas.AlignToPixelHorizontally(Rect.Left);
  Result.Top := Canvas.AlignToPixelVertically(Rect.Top);
  Result.Right := Result.Left + Round(Rect.Width * Canvas.Scale) / Canvas.Scale; // keep ratio horizontally
  Result.Bottom := Result.Top + Round(Rect.Height * Canvas.Scale) / Canvas.Scale; // keep ratio vertically
end;

function ComponentStateToString(const State: TComponentState): string;

  procedure Write(var P: PChar; const V: string);
  var
    PV, PM: PChar;
  begin
    PV := PChar(V);
    PM := PV + Length(V);
    while PV < PM do begin
      P^ := PV^;
      Inc(P);
      Inc(PV);
    end;
  end;

var
  P, P1: PChar;
begin
  SetLength(Result, 256);
  P := PChar(Result);
  P1 := P;
  if csLoading in State then Write(P, 'csLoading,');
  if csReading in State then Write(P, 'csReading,');
  if csWriting in State then Write(P, 'csWriting,');
  if csDestroying in State then Write(P, 'csDestroying,');
  if csDesigning in State then Write(P, 'csDesigning,');
  if csAncestor in State then Write(P, 'csAncestor,');
  if csUpdating in State then Write(P, 'csUpdating,');
  if csFixups in State then Write(P, 'csFixups,');
  if csFreeNotification in State then Write(P, 'csFreeNotification,');
  if csInline in State then Write(P, 'csInline,');
  if csDesignInstance in State then Write(P, 'csDesignInstance,');
  if (P - P1) > 0 then
    SetLength(Result, P - P1 - 1)
  else
    Result := '';
end;

function ViewStateToString(const State: TViewStates): string;

  procedure Write(var P: PChar; const V: string);
  var
    PV, PM: PChar;
  begin
    PV := PChar(V);
    PM := PV + Length(V);
    while PV < PM do begin
      P^ := PV^;
      Inc(P);
      Inc(PV);
    end;
  end;

var
  P, P1: PChar;
begin
  SetLength(Result, 256);
  P := PChar(Result);
  P1 := P;
  if TViewState.Pressed in State then Write(P, 'Pressed,');
  if TViewState.Focused in State then Write(P, 'Focused,');
  if TViewState.Hovered in State then Write(P, 'Hovered,');
  if TViewState.Selected in State then Write(P, 'Selected,');
  if TViewState.Checked in State then Write(P, 'Checked,');
  if TViewState.Enabled in State then Write(P, 'Enabled,');
  if TViewState.Activated in State then Write(P, 'Activated,');
  if TViewState.Custom in State then Write(P, 'Custom,');
  if (P - P1) > 0 then
    SetLength(Result, P - P1 - 1)
  else
    Result := 'None';
end;

type TPrivateControl = class(TControl);

function GetBoundsFloat(const R: TBounds): string;
begin
  if Assigned(R) and (R.Left = R.Top) and (R.Left = R.Right) and (R.Left = R.Bottom) and (R.Left <> 0) then
    Result := Format('%.1f', [R.Left])
  else Result := '';
end;

function GetFloatValue(const Value: string; var OutData: Single): Boolean;
var
  V: Single;
begin
  Result := False;
  if Length(Value) = 0 then Exit;
  V := StrToFloatDef(Value, 0);
  if (V = 0) and (Value <> '0') then Exit;
  OutData := V;
  Result := True;
end;

procedure CheckView(const List: TInterfaceList; const View: IView);

  procedure Check(const Control: TControl);
  var
    View: IView;
  begin
    if Supports(Control, IView, View) then
      CheckView(List, View);
  end;

var
  Layout: TViewLayout;
begin
  if Assigned(View) then begin
    if Assigned(List) and (List.Count > 0) then begin
      if List.Count > 256 then
        raise EViewError.Create(SRefOutLimitMax);
      if (List.IndexOf(View) >= 0) then // 重复引用
        raise EViewError.Create(SLocateFailed);
    end;
    Layout := View.GetLayout;
        if not Assigned(Layout) then
      Exit;
    List.Add(View);
    try
      if Assigned(Layout.FAlignBaseline) then
        Check(Layout.FAlignBaseline);
      if Assigned(Layout.FAlignTop) then
        Check(Layout.FAlignTop);
      if Assigned(Layout.FAlignBottom) then
        Check(Layout.FAlignBottom);
      if Assigned(Layout.FAbove) then
        Check(Layout.FAbove);
      if Assigned(Layout.FBelow) then
        Check(Layout.FBelow);
      if Assigned(Layout.FAlignLeft) then
        Check(Layout.FAlignLeft);
      if Assigned(Layout.FAlignRight) then
        Check(Layout.FAlignRight);
      if Assigned(Layout.FToRightOf) then
        Check(Layout.FToRightOf);
      if Assigned(Layout.FToLeftOf) then
        Check(Layout.FToLeftOf);
    finally
      if List.Count > 0 then
        List.Delete(List.Count - 1);
    end;
  end;
end;

// 检查组件引用是否存在死循环。返回True表示不存在
function CheckRecursionState(const Control: IView): Boolean;
var
  List: TInterfaceList;
begin
  if not Assigned(Control) then
    Result := True
  else begin
    List := TInterfaceList.Create;
    try
      CheckView(List, Control);
    finally
      List.Free;
    end;
    Result := True;
  end;
end;

function GetParentForm(AObj: TFmxObject): TCustomForm;
var
  V: TFmxObject;
begin
  Result := nil;
  if not Assigned(AObj) then
    Exit;
  V := AObj.Parent;
  while Assigned(V) do begin
    if V is TCustomForm then begin
      Result := V as TCustomForm;
      Break;
    end;
    V := V.Parent;
  end;
end;

{$IFDEF ANDROID}
procedure DoInitFrameStatusHeight();
var
  resourceId: Integer;
begin
  if TJBuild_VERSION.JavaClass.SDK_INT < 19 then
    Exit;
  try
    resourceId := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}
      .getResources().getIdentifier(
        StringToJString('status_bar_height'),
        StringToJString('dimen'),
        StringToJString('android'));
    if resourceId <> 0 then begin
      StatusHeight := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}
        .getResources().getDimensionPixelSize(resourceId);
      if StatusHeight > 0 then
        StatusHeight := StatusHeight / {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}
          .getResources().getDisplayMetrics().scaledDensity;
    end else
      StatusHeight := 0;
  except
  end;
end;

// 感谢 Flying Wang
type
  JSystemPropertiesClass = interface(IJavaClass)
    ['{C14AB573-CC6F-4087-A1FB-047E92F8E718}']
    function get(name: JString): JString; cdecl;
  end;

  [JavaSignature('android/os/SystemProperties')]
  JSystemProperties = interface(IJavaInstance)
    ['{58A4A7BF-80D0-4FF8-9CF3-F94123C8EEB7}']
  end;
  TJSystemProperties = class(TJavaGenericImport<JSystemPropertiesClass, JSystemProperties>) end;

procedure DoInitNavigationBarHeight();
var
  resourceId: Integer;
  HasNavigationBar: Boolean;
  oStr: JString;
  AStr: string;
begin
  NavigationBarHeight := 0;
  if TJBuild_VERSION.JavaClass.SDK_INT < 21 then
    Exit;
  HasNavigationBar := False;
  try
    resourceId := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}
      .getResources.getIdentifier(
        StringToJString('config_showNavigationBar'),
        StringToJString('bool'),
        StringToJString('android'));
    if resourceId <> 0 then begin
      HasNavigationBar := TAndroidHelper.Context.getResources.getBoolean(resourceId);
      try
        // http://blog.csdn.net/lgaojiantong/article/details/42874529
        oStr := TJSystemProperties.JavaClass.get(StringToJString('qemu.hw.mainkeys'));
        if oStr = nil then Exit;
        AStr := JStringToString(oStr).Trim;
      except
        AStr := '';
      end;
      if AStr <> '' then begin
        if AStr = '0' then
          HasNavigationBar := True
        else if AStr = '1' then
          HasNavigationBar := False
        else begin
          if TryStrToBool(AStr, HasNavigationBar) then
            HasNavigationBar := not HasNavigationBar;
        end;
      end;
      if not HasNavigationBar then
        Exit;
      resourceId := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}
        .getResources.getIdentifier(
          StringToJString('navigation_bar_height'),
          StringToJString('dimen'),
          StringToJString('android'));
      if resourceId <> 0 then begin
        NavigationBarHeight := TAndroidHelper.Context.getResources.getDimensionPixelSize(resourceId);
        if NavigationBarHeight > 0 then
          NavigationBarHeight := NavigationBarHeight / {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}
            .getResources().getDisplayMetrics().scaledDensity;
      end;
    end;
  except
  end;
end;
{$ENDIF}

{ 来自 KernowSoftwareFMX }
procedure ProcessMessages;
{$IFDEF IOS}
var
  TimeoutDate: NSDate;
begin
  TimeoutDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(0.0));
  TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop).runMode(NSDefaultRunLoopMode, TimeoutDate);
end;
{$ELSE}
begin
  // FMX can occasionally raise an exception.
  try
    Application.ProcessMessages;
  except end;
end;
{$ENDIF}

{ 来自 KernowSoftwareFMX }
procedure ReplaceOpaqueColor(ABmp: TBitmap; const Color: TAlphaColor);
var
  x, y: Integer;
  AMap: TBitmapData;
  PixelColor: TAlphaColor;
  C: PAlphaColorRec;
  A: Single;
begin
  if (Assigned(ABmp)) then begin
    if ABmp.Map(TMapAccess.ReadWrite, AMap) then
    try
      AlphaColorToPixel(Color, @PixelColor, AMap.PixelFormat);
      A := TAlphaColorRec(PixelColor).A / 255;
      //AlphaColorToPixel(claWhite, @PixelWhiteColor, AMap.PixelFormat);
      for y := 0 to ABmp.Height - 1 do begin
        for x := 0 to ABmp.Width - 1 do begin
          C := @PAlphaColorArray(AMap.Data)[y * (AMap.Pitch div 4) + x];
          if (C^.Color <> claWhite) and (C^.A > 0) then begin
            TAlphaColorRec(PixelColor).A := Trunc(C^.A * A);
            C^.Color := PremultiplyAlpha(PixelColor);
          end;
            //C^.Color := PremultiplyAlpha(MakeColor(PixelColor, C^.A / $FF));
        end;
      end;
    finally
      ABmp.Unmap(AMap);
    end;
  end;
end;

procedure SimulateClick(AControl: TControl; const x, y: single);
var
  AForm: TCommonCustomForm;
  AFormPoint: TPointF;
begin
  AForm := nil;
  if (AControl.Root is TCustomForm) then
    AForm := (AControl.Root as TCustomForm);
  if AForm <> nil then
  begin
    AFormPoint := AControl.LocalToAbsolute(PointF(X,Y));
    AForm.MouseDown(TMouseButton.mbLeft, [], AFormPoint.X, AFormPoint.Y);
    AForm.MouseUp(TMouseButton.mbLeft, [], AFormPoint.X, AFormPoint.Y);
  end;
end;

var
  AScreenScale: Single;

function GetScreenScale: single;
var
  Service: IFMXScreenService;
begin
  if AScreenScale > 0 then begin
    Result := AScreenScale;
    Exit;
  end;
  Service := IFMXScreenService(TPlatformServices.Current.GetPlatformService(IFMXScreenService));
  Result := Service.GetScreenScale;
  {$IFDEF IOS}
  if Result < 2 then
    Result := 2;
  {$ENDIF}
  AScreenScale := Result;
end;

{ TDrawableBase }

procedure TDrawableBase.Assign(Source: TPersistent);

  procedure AssignItem(State: TViewState; const Src: TDrawableBase);
  var V: TBrush;
  begin
    Src.GetStateBrush(State, V);
    if Assigned(V) then
      GetBrush(State, True).Assign(V)
    else begin
      GetStateBrush(State, V);
      FreeAndNil(V);
    end;
  end;

var
  SaveChange: TNotifyEvent;
  Src: TDrawable;
begin
  if Source is TDrawableBase then begin
    SaveChange := FOnChanged;
    FOnChanged := nil;
    Src := TDrawable(Source);
    FCornerType := Src.FCornerType;
    FCorners := Src.Corners;
    FKind := Src.FKind;
    AssignItem(TViewState.None, Src);
    AssignItem(TViewState.Pressed, Src);
    AssignItem(TViewState.Focused, Src);
    AssignItem(TViewState.Hovered, Src);
    AssignItem(TViewState.Selected, Src);
    AssignItem(TViewState.Checked, Src);
    AssignItem(TViewState.Enabled, Src);
    AssignItem(TViewState.Activated, Src);
    FOnChanged := SaveChange;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end else
    inherited;
end;

function TDrawableBase.BrushIsEmpty(V: TBrush): Boolean;
begin
  if not Assigned(V) then
    Result := True
  else begin
    if (V.Kind = TBrushKind.None) or
      ((V.Color and $FF000000 = 0) and (V.Kind = TBrushKind.Solid)) or
      ((Ord(V.Kind) = Ord(TViewBrushKind.SVGImage)) and ((TViewBrushBase(V).FSvgImage = nil) or (TViewBrushBase(V).FSvgImage.Empty))) or
      ((Ord(V.Kind) = Ord(TViewBrushKind.AccessoryBitmap)) and ((TViewBrushBase(V).FAccessory = nil) or TViewBrushBase(V).FAccessory.IsEmpty))
    then begin
      if (V is TViewImagesBrush) and (TViewImagesBrush(V).FImageIndex >= 0) then
        Result := False
      else
        Result := True
    end else
      Result := False;
  end;
end;

procedure TDrawableBase.Change;
begin
  DoChange(Self);
end;

constructor TDrawableBase.Create(View: IView; const ADefaultKind: TViewBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  FView := View;
  FCorners := AllCorners;
  FCornerType := TCornerType.Round;

  if Assigned(FView) and (csDesigning in FView.GetComponentState) then begin
    CreateBrush(FDefault, ADefaultKind, ADefaultColor);
    CreateBrush(FPressed);
    CreateBrush(FFocused);
    CreateBrush(FHovered);
    CreateBrush(FSelected);
    CreateBrush(FChecked);
    CreateBrush(FEnabled);
    CreateBrush(FActivated);
    FIsEmpty := GetEmpty;
  end else begin
    FIsEmpty := True;
    if (ADefaultKind = TViewBrushKind.Solid) and (ADefaultColor <> TAlphaColorRec.Null) then begin
      CreateBrush(FDefault, ADefaultKind, ADefaultColor);
    end;
  end;
  InitDrawable;
end;

procedure TDrawableBase.CreateBrush(var Value: TBrush;
  const ADefaultKind: TViewBrushKind; const ADefaultColor: TAlphaColor);
begin
  if Assigned(Value) then
    FreeAndNil(Value);
  Value := TViewBrush.Create(ADefaultKind, ADefaultColor);
  Value.OnChanged := DoChange;
end;

function TDrawableBase.CreateBrush: TBrush;
begin
  CreateBrush(Result);
end;

destructor TDrawableBase.Destroy;
begin
  FOnChanged := nil;
  FreeAndNil(FDefault);
  FreeAndNil(FPressed);
  FreeAndNil(FFocused);
  FreeAndNil(FHovered);
  FreeAndNil(FSelected);
  FreeAndNil(FChecked);
  FreeAndNil(FEnabled);
  FreeAndNil(FActivated);
  inherited;
end;

procedure TDrawableBase.DoChange(Sender: TObject);
begin
  FIsEmpty := GetEmpty;
  if Assigned(FOnChanged) then
    FOnChanged(Sender);
end;

procedure TDrawableBase.DoDrawed(Canvas: TCanvas; var R: TRectF; AState: TViewState; const AOpacity: Single);
begin
end;

function TDrawableBase.GetValue(const Index: Integer): TBrush;
begin
  Result := GetBrush(TViewState(Index), not (csLoading in FView.GetComponentState));
end;

procedure TDrawableBase.InitDrawable;
begin
end;

function TDrawableBase.IsStoredCorners: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

function TDrawableBase.GetBrush(const State: TViewState; AutoCreate: Boolean): TBrush;
begin
  GetStateBrush(State, Result);
  if (Result = nil) and
    (AutoCreate or (csLoading in FView.GetComponentState)) then
  begin
    CreateBrush(Result);
    SetStateBrush(State, Result);
  end;
end;

function TDrawableBase.GetDrawRect(const ALeft, ATop, ARight, ABottom: Single): TRectF;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function TDrawableBase.GetEmpty: Boolean;
begin
  Result := ((FDefault = nil) or (FDefault.Kind = TBrushKind.None)) and
    ((FPressed = nil) or (FPressed.Kind = TBrushKind.None)) and
    ((FFocused = nil) or (FFocused.Kind = TBrushKind.None)) and
    ((FHovered = nil) or (FHovered.Kind = TBrushKind.None)) and
    ((FSelected = nil) or (FSelected.Kind = TBrushKind.None)) and
    ((FChecked = nil) or (FChecked.Kind = TBrushKind.None)) and
    ((FEnabled = nil) or (FEnabled.Kind = TBrushKind.None)) and
    ((FActivated = nil) or (FActivated.Kind = TBrushKind.None));
end;

function TDrawableBase.GetStateBrush(const State: TViewState): TBrush;
begin
  GetStateBrush(State, Result);
end;

procedure TDrawableBase.GetStateBrush(const State: TViewState; var V: TBrush);
begin
  case State of
    TViewState.None: V := FDefault;
    TViewState.Pressed: V := FPressed;
    TViewState.Focused: V := FFocused;
    TViewState.Hovered: V := FHovered;
    TViewState.Selected: V := FSelected;
    TViewState.Checked: V := FChecked;
    TViewState.Enabled: V := FEnabled;
    TViewState.Activated: V := FActivated;
  else
    raise EDrawableError.Create(Format(SInvViewValue, [Integer(State)]));
  end;
end;

function TDrawableBase.GetStateImagesItem(AState: TViewState): TBrush;
begin
  GetStateBrush(AState, Result);
  if Result <> FDefault then begin
    if BrushIsEmpty(Result) then
    begin
      if (AState = TViewState.Pressed) then begin
        Result := FFocused;
        if BrushIsEmpty(Result) then
          Result := FDefault
      end else
        Result := FDefault;
    end;
  end;
  if BrushIsEmpty(Result) then
    Result := nil;
end;

function TDrawableBase.GetStateItem(AState: TViewState): TBrush;
begin
  GetStateBrush(AState, Result);
  if Result <> FDefault then begin
    if BrushIsEmpty(Result) then
    begin
      if (AState = TViewState.Pressed) then begin
        Result := FFocused;
        if BrushIsEmpty(Result) then
          Result := FDefault
      end else
        Result := FDefault;
    end;
  end;
  if BrushIsEmpty(Result) then
    Result := nil;
end;

procedure TDrawableBase.Draw(Canvas: TCanvas);
var
  V: TBrush;
  R: TRectF;
  AState: TViewState;
begin
  if FIsEmpty or (not Assigned(FView)) then Exit;
  if FView.InVisible or (csDestroying in FView.GetComponentState) then Exit;
  AState := FView.GetDrawState;
  R := GetDrawRect(0, 0, FView.GetWidth, FView.GetHeight);
  V := GetStateItem(AState);
  if V <> nil then
    FillRect(Canvas, R, FXRadius, FYRadius, FCorners, FView.Opacity, V, FCornerType);
  DoDrawed(Canvas, R, AState, FView.Opacity);
end;

procedure TDrawableBase.DrawBrushTo(Canvas: TCanvas; ABrush: TBrush;
  const R: TRectF);
begin
  if ABrush <> nil then
    FillRect(Canvas, R, FXRadius, FYRadius, FCorners, FView.GetOpacity, ABrush, FCornerType);
end;

procedure TDrawableBase.DrawStateTo(Canvas: TCanvas; const R: TRectF;
  AState: TViewState);
begin
  DrawStateTo(Canvas, R, AState, FView.GetOpacity);
end;

procedure TDrawableBase.DrawStateTo(Canvas: TCanvas; const R: TRectF;
  AState: TViewState; const AOpacity: Single);
var
  V: TBrush;
  VR: TRectF;
begin
  if FIsEmpty or (not Assigned(FView)) then Exit;
  if FView.InVisible or (csDestroying in FView.GetComponentState) then Exit;
  V := GetStateItem(AState);
  VR := GetDrawRect(R.Left, R.Top, R.Right, R.Bottom);
  if V <> nil then
    FillRect(Canvas, VR, FXRadius, FYRadius, FCorners, AOpacity, V, FCornerType);
  DoDrawed(Canvas, VR, AState, AOpacity);
end;

procedure TDrawableBase.DrawTo(Canvas: TCanvas; const R: TRectF);
begin
  if FIsEmpty or (not Assigned(FView)) then Exit;
  DrawStateTo(Canvas, R, FView.GetDrawState);
end;

procedure TDrawableBase.FillArc(Canvas: TCanvas; const Center, Radius: TPointF;
  const StartAngle, SweepAngle, AOpacity: Single; const ABrush: TBrush);
begin
  Canvas.FillArc(Center, Radius, StartAngle, SweepAngle, AOpacity, ABrush);
end;

procedure TDrawableBase.FillRect(Canvas: TCanvas; const ARect: TRectF;
  const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ABrush: TBrush;
  const ACornerType: TCornerType = TCornerType.Round);
var
  Bmp: TBitmap;
  V: Single;
begin
  if (Ord(ABrush.Kind) = Ord(TViewBrushKind.Patch9Bitmap)) and (ABrush is TViewBrush) then begin
    FillRect9Patch(Canvas, ARect, XRadius, YRadius, ACorners, AOpacity, TViewBrush(ABrush), ACornerType);
  end else begin
    if Ord(ABrush.Kind) = Ord(TViewBrushKind.AccessoryBitmap) then begin
      if Assigned(TViewBrushBase(ABrush).FAccessory) then begin
        Bmp := TViewBrushBase(ABrush).FAccessory.FAccessoryBmp;
        if Assigned(Bmp) then
          Canvas.DrawBitmap(Bmp, RectF(0, 0, Bmp.Width, Bmp.Height), AlignToPixel(Canvas, ARect), AOpacity);
      end;
    end else if Ord(ABrush.Kind) = Ord(TViewBrushKind.SVGImage) then begin
      if Assigned(TViewBrushBase(ABrush).FSvgImage) then begin
        if TViewBrushBase(ABrush).FSvgImage.Loss then
          TViewBrushBase(ABrush).FSvgImage.SetSize(Round(ARect.Width * GetScreenScale), Round(ARect.Height * GetScreenScale));
        Bmp := TViewBrushBase(ABrush).FSvgImage.Bitmap;
        if Assigned(Bmp) then
          Canvas.DrawBitmap(Bmp, RectF(0, 0, Bmp.Width, Bmp.Height), ARect, AOpacity);
      end;
    end else begin
      case FKind of
        TDrawableKind.None:
          begin
            Canvas.FillRect(ARect, XRadius, YRadius, ACorners, AOpacity, ABrush, ACornerType);
          end;
        TDrawableKind.Circle:
          begin
            V := Min(ARect.Width, ARect.Height) * 0.5;
            Canvas.FillArc(
              PointF(ARect.Width * 0.5 + ARect.Left, ARect.Height * 0.5 + ARect.Top),
              PointF(V, V), 0, 360, AOpacity, ABrush);
          end;
        TDrawableKind.Ellipse:
          begin
            Canvas.FillEllipse(ARect, AOpacity, ABrush);
          end;
      end;

    end;
  end;
end;

class procedure TDrawableBase.FillRect9Patch(Canvas: TCanvas; const ARect: TRectF;
  const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ABrush: TViewBrush;
  const ACornerType: TCornerType);
var
  Bmp: TPatch9Bitmap;
  AOnChanged: TNotifyEvent;
  AO: Single;
  BL, BT, BR, BB: Single;
  BW, BH: Single;
begin
  if (ABrush.Bitmap = nil) or (ABrush.Bitmap.Bitmap = nil) or
    ABrush.Bitmap.Bitmap.IsEmpty then
    Exit;

  Bmp := TPatch9Bitmap(ABrush.Bitmap);
  AOnChanged := ABrush.OnChanged;
  ABrush.OnChanged := nil;
  ABrush.Kind := TViewBrushKind.Bitmap;

  if Bmp.FRemoveBlackLine then begin
    {$IFNDEF MSWINDOWS}
    // 移动平台，使用PPI计算出1个dp所占用的像索
    // 感觉直接设为2效果更好
    AO := 2; //1 * GetPPI(Self.FView as TFmxObject) / 160;
    //if AO < 0 then AO := 1;
    {$ELSE}
    AO := 1;
    {$ENDIF}
  end else
    AO := 0;

  if (Bmp.FBounds.Left = 0) and (Bmp.FBounds.Top = 0) and (Bmp.FBounds.Right = 0) and (Bmp.FBounds.Bottom = 0) then begin
    if AO = 0 then
      Canvas.FillRect(ARect, XRadius, YRadius, ACorners, AOpacity, ABrush, ACornerType)
    else
      Canvas.DrawBitmap(Bmp.Bitmap, RectF(AO, AO, Bmp.Bitmap.Width - AO, Bmp.Bitmap.Height - AO),
        ARect, AOpacity);
  end else begin
    // 九宫格绘图
    BW := Bmp.Bitmap.Width;
    BH := Bmp.Bitmap.Height;

    BL := Bmp.FBounds.Left;
    BT := Bmp.FBounds.Top;
    BR := Bmp.FBounds.Right;
    BB := Bmp.FBounds.Bottom;

    // 左上
    Canvas.DrawBitmap(Bmp.Bitmap,
      RectF(AO, AO, BL + AO, BT + AO),
      RectF(ARect.Left, ARect.Top, ARect.Left + BL, ARect.Top + BT),
      AOpacity);
    // 顶部
    Canvas.DrawBitmap(Bmp.Bitmap,
      RectF(BL + AO, AO, BW - BR - AO, BT + AO),
      RectF(ARect.Left + BL, ARect.Top, ARect.Right - BR, ARect.Top + BT),
      AOpacity);
    // 右上
    Canvas.DrawBitmap(Bmp.Bitmap,
      RectF(BW - BR - AO, AO, BW - AO, BT + AO),
      RectF(ARect.Right - BR, ARect.Top, ARect.Right, ARect.Top + BT),
      AOpacity);

    // 左中
    Canvas.DrawBitmap(Bmp.Bitmap,
      RectF(AO, BT + AO, BL + AO, BH - BB - AO),
      RectF(ARect.Left, ARect.Top + BT, ARect.Left + BL, ARect.Bottom - BB),
      AOpacity);
    // 中间
    Canvas.DrawBitmap(Bmp.Bitmap,
      RectF(BL + AO, BT + AO, BW - BR - AO, BH - BB - AO),
      RectF(ARect.Left + BL, ARect.Top + BT, ARect.Right - BR, ARect.Bottom - BB),
      AOpacity);
    // 右中
    Canvas.DrawBitmap(Bmp.Bitmap,
      RectF(BW - BR - AO, BT + AO, BW - AO, BH - BB - AO),
      RectF(ARect.Right - BR, ARect.Top + BT, ARect.Right, ARect.Bottom - BB),
      AOpacity);

    // 左下
    Canvas.DrawBitmap(Bmp.Bitmap,
      RectF(AO, BH - BB - AO, BL + AO, BH - AO),
      RectF(ARect.Left, ARect.Bottom - BB, ARect.Left + BL, ARect.Bottom),
      AOpacity);
    // 下中
    Canvas.DrawBitmap(Bmp.Bitmap,
      RectF(BL + AO, BH - BB - AO, BW - BR - AO, BH - AO),
      RectF(ARect.Left + BL, ARect.Bottom - BB, ARect.Right - BR, ARect.Bottom),
      AOpacity);
    // 右下
    Canvas.DrawBitmap(Bmp.Bitmap,
      RectF(BW - BR - AO, BH - BB - AO, BW - AO, BH - AO),
      RectF(ARect.Right - BR, ARect.Bottom - BB, ARect.Right, ARect.Bottom),
      AOpacity);
  end;

  ABrush.Kind := TViewBrushKind.Patch9Bitmap;
  ABrush.OnChanged := AOnChanged;
end;

procedure TDrawableBase.SetDrawable(const Value: TDrawableBase);
begin
  Assign(Value);
end;

procedure TDrawableBase.SetColor(State: TViewState; const Value: TAlphaColor);
var V: TBrush;
begin
  V := GetBrush(State, True);
  V.Kind := TBrushKind.Solid;
  V.Color := Value;
end;

procedure TDrawableBase.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then begin
    FCorners := Value;
    DoChange(Self);
  end;
end;

procedure TDrawableBase.SetCornerType(const Value: TCornerType);
begin
  if FCornerType <> Value then begin
    FCornerType := Value;
    DoChange(Self);
  end;
end;

procedure TDrawableBase.SetGradient(State: TViewState; const Value: TGradient);
var V: TBrush;
begin
  V := GetBrush(State, True);
  V.Gradient.Assign(Value);
  V.Kind := TBrushKind.Gradient;
end;

procedure TDrawableBase.SetKind(const Value: TDrawableKind);
begin
  if FKind <> Value then begin
    FKind := Value;
    DoChange(Self);
  end;
end;

procedure TDrawableBase.SetRadius(const X, Y: Single);
begin
  FYRadius := Y;
  FXRadius := X;
  DoChange(Self);
end;

procedure TDrawableBase.SetBitmap(State: TViewState; const Value: TBrushBitmap);
var V: TBrush;
begin
  V := GetBrush(State, True);
  V.Bitmap.Assign(Value);
  V.Kind := TBrushKind.Bitmap;
end;

procedure TDrawableBase.SetBrush(State: TViewState;
  const Value: TDrawableBrush);
var V: TBrush;
begin
  if not Assigned(Value) then Exit;
  V := GetBrush(State, True);
  if (Self is TDrawableIcon) and (Value.ImageIndex >= 0) then
    TDrawableIcon(Self).Images := Value.Images;
  V.Assign(Value.Brush);
end;

procedure TDrawableBase.SetBrush(State: TViewState; const Value: TBrush);
begin
  GetBrush(State, True).Assign(Value);
end;

procedure TDrawableBase.SetBitmap(State: TViewState; const Value: TBitmap);
var V: TBrush;
begin
  V := GetBrush(State, True);
  if Assigned(Value) then
    V.Bitmap.Bitmap.Assign(Value)
  else
    V.Bitmap.Bitmap.Clear(0);
  V.Kind := TBrushKind.Bitmap;
end;

procedure TDrawableBase.SetStateBrush(const State: TViewState; const V: TBrush);
begin
  case State of
    TViewState.None: FDefault := V;
    TViewState.Pressed: FPressed := V;
    TViewState.Focused: FFocused := V;
    TViewState.Hovered: FHovered := V;
    TViewState.Selected: FSelected := V;
    TViewState.Checked: FChecked := V;
    TViewState.Enabled: FEnabled := V;
    TViewState.Activated: FActivated := V;
  end;
end;

procedure TDrawableBase.SetValue(const Index: Integer; const Value: TBrush);
begin
  SetBrush(TViewState(Index), Value);
end;

procedure TDrawableBase.SetXRadius(const Value: Single);
begin
  if FXRadius <> Value then begin
    FXRadius := Value;
    DoChange(Self);
  end;
end;

procedure TDrawableBase.SetYRadius(const Value: Single);
begin
  if FYRadius <> Value then begin
    FYRadius := Value;
    DoChange(Self);
  end;
end;

{ TDrawable }

procedure TDrawable.Assign(Source: TPersistent);
var
  LastOnChange: TNotifyEvent;
begin
  if Source is TDrawable then begin
    LastOnChange := FPadding.OnChange;
    FPadding.OnChange := nil;
    FPadding.Assign(TDrawable(Source).FPadding);
    FPadding.OnChange := LastOnChange;
  end;
  inherited Assign(Source);
end;

constructor TDrawable.Create(View: IView; const ADefaultKind: TViewBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  FPadding := TBounds.Create(TRectF.Empty);
  FPadding.OnChange := DoChange;
  inherited Create(View, ADefaultKind, ADefaultColor);
end;

destructor TDrawable.Destroy;
begin
  FreeAndNil(FPadding);
  inherited Destroy;
end;

function TDrawable.GetDrawRect(const ALeft, ATop, ARight, ABottom: Single): TRectF;
begin
  Result.Left := ALeft + FPadding.Left;
  Result.Top := ATop + FPadding.Top;
  Result.Right := ARight - FPadding.Right;
  Result.Bottom := ABottom - FPadding.Bottom;
end;

function TDrawable.GetPaddings: string;
begin
  Result := GetBoundsFloat(FPadding);
end;

function TDrawable.GetValue(const Index: Integer): TViewBrush;
begin
  Result := inherited GetBrush(TViewState(Index),
    not (csLoading in FView.GetComponentState)) as TViewBrush;
end;

procedure TDrawable.SetPadding(const Value: TBounds);
begin
  FPadding.Assign(Value);
end;

procedure TDrawable.SetPaddings(const Value: string);
var
  V: Single;
begin
  if Assigned(Padding) and GetFloatValue(Value, V) then
    Padding.Rect := RectF(V, V, V, V);
end;

procedure TDrawable.SetValue(const Index: Integer; const Value: TViewBrush);
begin
  inherited SetValue(Index, Value);
end;

{ TDrawableIcon }

procedure TDrawableIcon.AdjustDraw(Canvas: TCanvas; var R: TRectF; ExecDraw: Boolean;
  AState: TViewState);
var
  DR: TRectF;
  SW, SH: Single;
begin
  SW := R.Right - R.Left;
  SH := R.Bottom - R.Top;
  case FPosition of
    TDrawablePosition.Left:
      begin
        if ExecDraw then begin
          DR.Left := R.Left;
          DR.Top := R.Top + (SH - FHeight) / 2;
          DR.Right := DR.Left + FWidth;
          DR.Bottom := DR.Top + FHeight;
          DrawStateTo(Canvas, DR, AState);
        end;
        R.Left := R.Left + FWidth + FPadding;
      end;
    TDrawablePosition.Right:
      begin
        if ExecDraw then begin
          DR.Left := R.Right - FWidth;
          DR.Top := R.Top + (SH - FHeight) / 2;
          DR.Right := R.Right;
          DR.Bottom := DR.Top + FHeight;
          DrawStateTo(Canvas, DR, AState);
        end;
        R.Right := R.Right - FWidth - FPadding;
      end;
    TDrawablePosition.Top:
      begin
        if ExecDraw then begin
          DR.Left := R.Left + (SW - FWidth) / 2;
          DR.Top := R.Top;
          DR.Right := DR.Left + FWidth;
          DR.Bottom := DR.Top + FHeight;
          DrawStateTo(Canvas, DR, AState);
        end;
        R.Top := R.Top + FHeight + FPadding;
      end;
    TDrawablePosition.Bottom:
      begin
        if ExecDraw then begin
          DR.Left := R.Left + (SW - FWidth) / 2;
          DR.Top := R.Bottom - FHeight;
          DR.Right := DR.Left + FWidth;
          DR.Bottom := R.Bottom;
          DrawStateTo(Canvas, DR, AState);
        end;
        R.Bottom := R.Bottom - FHeight - FPadding;
      end;
    TDrawablePosition.Center:
      begin
        if ExecDraw then begin
          DR.Left := R.Left + (SW - FWidth) / 2;
          DR.Top := R.Top + (SH - FHeight) / 2;
          DR.Right := DR.Left + FWidth;
          DR.Bottom := DR.Top + FHeight;
          DrawStateTo(Canvas, DR, AState);
        end;
      end;
  end;
end;

procedure TDrawableIcon.Assign(Source: TPersistent);
begin
  if Source is TDrawableIcon then begin
    FWidth := TDrawableIcon(Source).FWidth;
    FHeight := TDrawableIcon(Source).FHeight;
    FPadding := TDrawableIcon(Source).FPadding;
    FPosition := TDrawableIcon(Source).FPosition;
  end;
  inherited Assign(Source);
end;

constructor TDrawableIcon.Create(View: IView; const ADefaultKind: TViewBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  FView := View;
  FImageLink := TViewImageLink.Create(Self);
  FImageLink.OnChange := DoChange;
  inherited Create(View, ADefaultKind, ADefaultColor);
  FWidth := 16;
  FHeight := 16;
  FPosition := TDrawablePosition.Left;
  FPadding := 4;
end;

procedure TDrawableIcon.CreateBrush(var Value: TBrush;
  const ADefaultKind: TViewBrushKind; const ADefaultColor: TAlphaColor);
begin
  if Assigned(Value) then
    FreeAndNil(Value);
  Value := TViewImagesBrush.Create(TBrushKind(ADefaultKind), ADefaultColor);
  TViewImagesBrush(Value).FOwner := Self;
  Value.OnChanged := DoChange;
end;

destructor TDrawableIcon.Destroy;
begin
  FImageLink.DisposeOf;
  inherited;
end;

procedure TDrawableIcon.Draw(Canvas: TCanvas);
var
  ImageIndex: Integer;
begin
  inherited Draw(Canvas);
  ImageIndex := GetStateImageIndex();
  if (ImageIndex >= 0) and Assigned(FImageLink.Images) then
    DrawImage(Canvas, ImageIndex, GetDrawRect(0, 0, FView.GetWidth, FView.GetHeight), FView.GetOpacity);
end;

procedure TDrawableIcon.DrawImage(Canvas: TCanvas; Index: Integer;
  const R: TRectF);
begin
  DrawImage(Canvas, Index, R, FView.GetOpacity);
end;

procedure TDrawableIcon.DrawImage(Canvas: TCanvas; Index: Integer;
  const R: TRectF; const AOpacity: Single);
var
  Images: TCustomImageList;
  Bitmap: TBitmap;
  BitmapSize: TSize;
  BitmapRect: TRectF;
begin
  if FView.InVisible then
    Exit;
  Images := GetImages;
  if Assigned(Images) and (Index >= 0) and (Index < Images.Count) then begin
    BitmapSize := TSize.Create(FWidth * 2, FHeight * 2);
    if BitmapSize.IsZero then
      Exit;
    Bitmap := Images.Bitmap(BitmapSize, Index);
    if Bitmap <> nil then begin
      BitmapRect := TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height);
      Canvas.DrawBitmap(Bitmap, BitmapRect, R, AOpacity, False);
    end;
  end;
end;

procedure TDrawableIcon.DrawStateTo(Canvas: TCanvas; const R: TRectF; AState: TViewState; const AOpacity: Single);
var
  ImageIndex: Integer;
begin
  inherited DrawStateTo(Canvas, R, AState, AOpacity);
  ImageIndex := GetStateImageIndex(AState);
  if (ImageIndex >= 0) and Assigned(FImageLink.Images) then
    DrawImage(Canvas, ImageIndex, R, AOpacity);
end;

function TDrawableIcon.GetComponent: TComponent;
begin
  Result := FView.GetComponent;
end;

function TDrawableIcon.GetEmpty: Boolean;
begin
  if GetStateImageIndex >= 0 then
    Result := not Assigned(FImageLink.Images)
  else begin
    Result := (FWidth <= 0) or (FHeight <= 0);
    if not Result then
      Result := inherited GetEmpty;
  end;
end;

function TDrawableIcon.GetImageIndex: TImageIndex;
begin
  Result := FImageLink.ImageIndex;
end;

function TDrawableIcon.GetImageList: TBaseImageList;
begin
  Result := GetImages;
end;

function TDrawableIcon.GetImages: TCustomImageList;
begin
  if Assigned(FImageLink.Images) then
    Result := TCustomImageList(FImageLink.Images)
  else
    Result := nil;
end;

function TDrawableIcon.GetStateImageIndex: Integer;
begin
  if Assigned(FView) then
    Result := GetStateImageIndex(FView.GetDrawState)
  else
    Result := -1;
end;

function TDrawableIcon.GetStateImageIndex(State: TViewState): Integer;
var
  V: TBrush;
begin
  Result := -1;
  if Assigned(FView) then begin
    V := GetStateImagesItem(State);
    if Assigned(V) then
      Result := TViewImagesBrush(V).FImageIndex;
  end;
end;

procedure TDrawableIcon.ImagesChanged;
begin
  DoChange(Self);
end;

function TDrawableIcon.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK
  else Result := E_NOINTERFACE
end;

procedure TDrawableIcon.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then begin
    FHeight := Value;
    DoChange(Self);
  end;
end;

procedure TDrawableIcon.SetImageIndex(const Value: TImageIndex);
begin
  FImageLink.ImageIndex := Value;
end;

procedure TDrawableIcon.SetImageList(const Value: TBaseImageList);
begin
  ValidateInheritance(Value, TCustomImageList);
  SetImages(TCustomImageList(Value));
end;

procedure TDrawableIcon.SetImages(const Value: TCustomImageList);
begin
  FImageLink.Images := Value;
end;

procedure TDrawableIcon.SetPadding(const Value: Integer);
begin
  if FPadding <> Value then begin
    FPadding := Value;
    DoChange(Self);
  end;
end;

procedure TDrawableIcon.SetPosition(const Value: TDrawablePosition);
begin
  if FPosition <> Value then begin
    FPosition := Value;
    DoChange(Self);
  end;
end;

procedure TDrawableIcon.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then begin
    FWidth := Value;
    DoChange(Self);
  end;
end;

function TDrawableIcon._AddRef: Integer;
begin
  Result := -1;
end;

function TDrawableIcon._Release: Integer;
begin
  Result := -1;
end;

{ TViewColor }

procedure TViewColor.Assign(Source: TPersistent);
var
  Src: TViewColor;
begin
  if Source = nil then begin
    Self.FPressed := TAlphaColorRec.Null;
    Self.FFocused := TAlphaColorRec.Null;
    Self.FHovered := TAlphaColorRec.Null;
    Self.FSelected := TAlphaColorRec.Null;
    Self.FChecked := TAlphaColorRec.Null;
    Self.FEnabled := TAlphaColorRec.Null;
    Self.FActivated := TAlphaColorRec.Null;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end else if Source is TViewColor then begin
    Src := TViewColor(Source);
    Self.FDefault := Src.FDefault;
    Self.FPressed := Src.FPressed;
    Self.FFocused := Src.FFocused;
    Self.FHovered := Src.FHovered;
    Self.FSelected := Src.FSelected;
    Self.FChecked := Src.FChecked;
    Self.FEnabled := Src.FEnabled;
    Self.FActivated := Src.FActivated;
    Self.FHintText := Src.FHintText;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end else
    inherited;
end;

constructor TViewColor.Create(const ADefaultColor: TAlphaColor);
begin
  FDefault := ADefaultColor;
  FPressed := TAlphaColorRec.Null;
  FFocused := TAlphaColorRec.Null;
  FHovered := TAlphaColorRec.Null;
  FSelected := TAlphaColorRec.Null;
  FChecked := TAlphaColorRec.Null;
  FEnabled := TAlphaColorRec.Null;
  FActivated := TAlphaColorRec.Null;
  FHintText := TAlphaColorRec.Gray;
end;

function TViewColor.ColorActivatedStored: Boolean;
begin
  Result := GetColorStoreState(8);
end;

function TViewColor.ColorCheckedStored: Boolean;
begin
  Result := GetColorStoreState(6);
end;

function TViewColor.ColorDefaultStored: Boolean;
begin
  Result := GetColorStoreState(1);
end;

function TViewColor.ColorEnabledStored: Boolean;
begin
  Result := GetColorStoreState(7);
end;

function TViewColor.ColorFocusedStored: Boolean;
begin
  Result := GetColorStoreState(3);
end;

function TViewColor.ColorHoveredStored: Boolean;
begin
  Result := GetColorStoreState(4);
end;

function TViewColor.ColorPressedStored: Boolean;
begin
  Result := GetColorStoreState(2);
end;

function TViewColor.ColorSelectedStored: Boolean;
begin
  Result := GetColorStoreState(5);
end;

destructor TViewColor.Destroy;
begin
  inherited;
end;

procedure TViewColor.DoChange(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Sender);
end;

function TViewColor.GetColor(State: TViewState): TAlphaColor;
begin
  case State of
    TViewState.None: Result := FDefault;
    TViewState.Pressed: Result := FPressed;
    TViewState.Focused: Result := FFocused;
    TViewState.Hovered: Result := FHovered;
    TViewState.Selected: Result := FSelected;
    TViewState.Checked: Result := FChecked;
    TViewState.Enabled: Result := FEnabled;
    TViewState.Activated: Result := FActivated;
  else
    if Ord(State) = 8 then
      Result := FHintText
    else
      raise EDrawableError.Create(Format(SInvViewValue, [Integer(State)]));
  end;
end;

function TViewColor.GetColorStoreState(const Index: Integer): Boolean;
begin
  Result := (FColorStoreState and Index) <> 0;
end;

function TViewColor.GetStateColor(State: TViewState): TAlphaColor;
begin
  Result := GetColor(State);
  if (Result = TAlphaColorRec.Null) and (State <> TViewState.None) then begin
    if (State = TViewState.Pressed) and (FFocused <> TAlphaColorRec.Null) then
      Result := FFocused
    else
      Result := FDefault
  end;
end;

function TViewColor.GetValue(const Index: Integer): TAlphaColor;
begin
  Result := GetColor(TViewState(Index));
end;

procedure TViewColor.SetActivated(const Value: TAlphaColor);
begin
  if FActivated <> Value then begin
    FActivated := Value;
    ActivatedChange := True;
    DoChange(Self);
  end;
end;

procedure TViewColor.SetChecked(const Value: TAlphaColor);
begin
  if FChecked <> Value then begin
    FChecked := Value;
    CheckedChange := True;
    DoChange(Self);
  end;
end;

procedure TViewColor.SetColor(State: TViewState; const Value: TAlphaColor);
begin
  case State of
    TViewState.None: FDefault := Value;
    TViewState.Pressed: FPressed := Value;
    TViewState.Focused: FFocused := Value;
    TViewState.Hovered: FHovered := Value;
    TViewState.Selected: FSelected := Value;
    TViewState.Checked: FChecked := Value;
    TViewState.Enabled: FEnabled := Value;
    TViewState.Activated: FActivated := Value;
  else
    if Ord(State) = 8 then
      FHintText := Value
    else
      raise EDrawableError.Create(Format(SInvViewValue, [Integer(State)]));
  end;
  DoChange(Self);
end;

procedure TViewColor.SetColorStoreState(const Index: Integer;
  const Value: Boolean);
begin
  if Value then
    FColorStoreState := (FColorStoreState or Cardinal(Index))
  else
    FColorStoreState := (FColorStoreState and (not Index));
end;

procedure TViewColor.SetDefault(const Value: TAlphaColor);
begin
  if Value <> FDefault then begin
    FDefault := Value;
    DefaultChange := True;
    DoChange(Self);
  end;
end;

procedure TViewColor.SetEnabled(const Value: TAlphaColor);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    EnabledChange := True;
    DoChange(Self);
  end;
end;

procedure TViewColor.SetFocused(const Value: TAlphaColor);
begin
  if Focused <> Value then begin
    FFocused := Value;
    FocusedChange := True;
    DoChange(Self);
  end;
end;

procedure TViewColor.SetHovered(const Value: TAlphaColor);
begin
  if FHovered <> Value then begin
    FHovered := Value;
    HoveredChange := True;
    DoChange(Self);
  end;
end;

procedure TViewColor.SetPressed(const Value: TAlphaColor);
begin
  if FPressed <> Value then begin
    FPressed := Value;
    PressedChange := True;
    DoChange(Self);
  end;
end;

procedure TViewColor.SetSelected(const Value: TAlphaColor);
begin
  if FSelected <> Value then begin
    FSelected := Value;
    SelectedChange := True;
    DoChange(Self);
  end;
end;

procedure TViewColor.SetValue(const Index: Integer; const Value: TAlphaColor);
begin
  SetColor(TViewState(Index), Value);
end;

{ TTextColor }

function TTextColor.GetHintText: TAlphaColor;
begin
  Result := FHintText;
end;

procedure TTextColor.SetHintText(const Value: TAlphaColor);
begin
  if FHintText <> Value then begin
    FHintText := Value;
    DoChange(Self);
  end;
end;

{ TViewLayout }

procedure TViewLayout.Assign(Source: TPersistent);
var
  SaveChange: TNotifyEvent;
  Src: TViewLayout;
begin
  if Source is TViewLayout then begin
    SaveChange := FOnChanged;
    FOnChanged := nil;
    Src := TViewLayout(Source);
    FToLeftOf := Src.FToLeftOf;
    FToRightOf := Src.FToRightOf;
    FAbove := Src.FAbove;
    FBelow := Src.FBelow;
    FAlignBaseline := Src.FAlignBaseline;
    FAlignLeft := Src.FAlignLeft;
    FAlignTop := Src.FAlignTop;
    FAlignRight := Src.FAlignRight;
    FAlignBottom := Src.FAlignBottom;

    FAlignParentLeft := Src.FAlignParentLeft;
    FAlignParentTop := Src.FAlignParentTop;
    FAlignParentRight := Src.FAlignParentRight;
    FAlignParentBottom := Src.FAlignParentBottom;
    FCenterInParent := Src.FCenterInParent;
    FCenterHorizontal := Src.FCenterHorizontal;
    FCenterVertical := Src.FCenterVertical;

    FOnChanged := SaveChange;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end else
    inherited;
end;

constructor TViewLayout.Create(View: IView);
begin
  FView := View;
end;

destructor TViewLayout.Destroy;
begin
  inherited;
end;

procedure TViewLayout.DoChange();
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TViewLayout.GetHeight: TViewSize;
begin
  Result := FView.HeightSize;
end;

function TViewLayout.GetWidth: TViewSize;
begin
  Result := FView.WidthSize;
end;

function TViewLayout.IsEmpty: Boolean;
begin
  Result := not (Assigned(FAbove) or Assigned(FAlignBaseline) or
    Assigned(FAlignBottom) or Assigned(FAlignLeft) or Assigned(FAlignRight) or
    Assigned(FAlignTop) or Assigned(FBelow) or Assigned(FToLeftOf) or
    Assigned(FToRightOf));
end;

procedure TViewLayout.SetAbove(const Value: TControl);
begin
  SetValue(FAbove, Value);
end;

procedure TViewLayout.SetAlignBaseline(const Value: TControl);
begin
  SetValue(FAlignBaseline, Value);
end;

procedure TViewLayout.SetAlignBottom(const Value: TControl);
begin
  SetValue(FAlignBottom, Value);
end;

procedure TViewLayout.SetAlignLeft(const Value: TControl);
begin
  SetValue(FAlignLeft, Value);
end;

procedure TViewLayout.SetAlignParentBottom(const Value: Boolean);
begin
  SetValue(FAlignParentBottom, Value);
end;

procedure TViewLayout.SetAlignParentLeft(const Value: Boolean);
begin
  SetValue(FAlignParentLeft, Value);
end;

procedure TViewLayout.SetAlignParentRight(const Value: Boolean);
begin
  SetValue(FAlignParentRight, Value);
end;

procedure TViewLayout.SetAlignParentTop(const Value: Boolean);
begin
  SetValue(FAlignParentTop, Value);
end;

procedure TViewLayout.SetAlignRight(const Value: TControl);
begin
  SetValue(FAlignRight, Value);
end;

procedure TViewLayout.SetAlignTop(const Value: TControl);
begin
  SetValue(FAlignTop, Value);
end;

procedure TViewLayout.SetBelow(const Value: TControl);
begin
  SetValue(FBelow, Value);
end;

procedure TViewLayout.SetCenterHorizontal(const Value: Boolean);
begin
  SetValue(FCenterHorizontal, Value);
end;

procedure TViewLayout.SetCenterInParent(const Value: Boolean);
begin
  SetValue(FCenterInParent, Value);
end;

procedure TViewLayout.SetCenterVertical(const Value: Boolean);
begin
  SetValue(FCenterVertical, Value);
end;

procedure TViewLayout.SetHeight(const Value: TViewSize);
begin
  FView.HeightSize := Value;
end;

procedure TViewLayout.SetToLeftOf(const Value: TControl);
begin
  SetValue(FToLeftOf, Value);
end;

procedure TViewLayout.SetToRightOf(const Value: TControl);
begin
  SetValue(FToRightOf, Value);
end;

procedure TViewLayout.SetValue(var Dest: Boolean; const Value: Boolean);
begin
  if Dest <> Value then begin
    Dest := Value;
    DoChange();
  end;
end;

procedure TViewLayout.SetValue(var Dest: TControl; const Value: TControl);
var
  Tmp: TControl;
begin
  if Dest <> Value then begin
    if Assigned(Value) then begin
      if Value = TObject(FView) then
        raise EViewLayoutError.Create(SNotAllowSelf);
      if Value.Parent <> FView.ParentControl then
        raise EViewLayoutError.Create(SMustSameParent);
      if not (csLoading in FView.GetComponentState) then begin
        Tmp := Dest;
        Dest := Value;
        try
          CheckRecursionState(FView);
        finally
          Dest := Tmp;
        end;
      end;
    end;
    Dest := Value;
    DoChange();
  end;
end;

procedure TViewLayout.SetWidth(const Value: TViewSize);
begin
  FView.WidthSize := Value;
end;

{ TViewBase }

function TViewBase.GetBackground: TDrawable;
begin
  Result := nil;
end;

function TViewBase.GetMaxHeight: Single;
begin
  Result := 0;
end;

function TViewBase.GetMaxWidth: Single;
begin
  Result := 0;
end;

function TViewBase.GetMinHeight: Single;
begin
  Result := 0;
end;

function TViewBase.GetMinWidth: Single;
begin
  Result := 0;
end;

function TViewBase.GetViewBackground: TDrawable;
begin
  Result := nil;
end;

function TViewBase.GetViewStates: TViewStates;
begin
  Result := [];
  if Self.FIsFocused then
    Include(Result, TViewState.Focused);
  if Self.Pressed then
    Include(Result, TViewState.Pressed);
end;

{ TView }

procedure TView.AfterPaint;
begin
  inherited;
  FInvaliding := False;
end;

function TView.AllowUseLayout: Boolean;
begin
  Result := (not (csDesigning in ComponentState)) or
    (Assigned(ParentControl)) and (ParentControl is TRelativeLayout);
end;

procedure TView.ApplyTriggerEffect(const AInstance: TFmxObject;
  const ATrigger: string);
begin
  // inherited; disable all effect
end;

function TView.CanAnimation: Boolean;
begin
  Result := False;
end;

function TView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
begin
  Result := CanRepaint and EmptyBackground(View.Background, State);
end;

function TView.EmptyBackground(const V: TDrawable;
  const State: TViewState): Boolean;
begin
  Result := Assigned(V) and
    (Assigned(V.GetStateBrush(State)) or
    ((V is TDrawableBorder) and Assigned(TDrawableBorder(V)._Border) and
    (TDrawableBorder(V)._Border.Color.GetColor(State) and $FF000000 > 0)));
end;

procedure TView.Click;
begin
  {$IFNDEF MSWINDOWS}
  if Abs(FDownUpOffset) > 10 then // 防止滚动时触发点击事件
    Exit;
  if Assigned(OnClick) then
    PlayClickEffect;
  {$ENDIF}
  DoClickEvent();
  inherited Click;
end;

constructor TView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAcceptsControls(False);
  ClipChildren := True;
  HitTest := False;
  FAdjustViewBounds := True;
  FRecalcInVisible := True;
  FViewState := [];
  FDrawState := TViewState.None;
  if csDesigning in ComponentState then begin
    FBackground := CreateBackground();
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  WidthSize := TViewSize.CustomSize;
  DisableFocusEffect := True;
end;

function TView.CreateBackground: TDrawable;
begin
  Result := TDrawableBorder.Create(Self);
  Result.OnChanged := DoBackgroundChanged;
end;

procedure TView.DecChildState(State: TViewState);
var
  I: Integer;
  View: IView;
begin
  if State = TViewState.None then Exit;
  if (csDestroying in ComponentState) or (csDesigning in ComponentState) then Exit;
  for I := 0 to Controls.Count - 1 do begin
    if Supports(Controls.Items[I], IView, View) then
      View.DecViewState(State);
  end;
end;

procedure TView.DecViewState(const State: TViewState);
begin
  Exclude(FViewState, State);
  FDrawState := GetRealDrawState;
  DecChildState(State);
end;

destructor TView.Destroy;
begin
  FreeScrollbar();
  FreeAndNil(FBackground);
  FreeAndNil(FLayout);
  inherited Destroy;
end;

procedure TView.DoActivate;
begin
  //IncViewState(TViewState.Activated);
  inherited DoActivate;
end;

procedure TView.DoAdjustViewBounds(var ANewWidth, ANewHeight: Single);
var
  AMaxW, AMaxH: Single;
begin
  if FAdjustViewBounds then begin
    AMaxW := FMaxWidth;
    AMaxH := FMaxHeight;

    if Assigned(ParentView) then begin
      if (AMaxW <= 0) and (WidthSize = TViewSize.WrapContent) then
        AMaxW := ParentView.MaxWidth;
      if (AMaxH <= 0) and (HeightSize = TViewSize.WrapContent) then
        AMaxH := ParentView.MaxHeight;
    end;

    if (AMaxW > 0) and (ANewWidth > AMaxW) then
      ANewWidth := AMaxW;
    if (AMaxH > 0) and (ANewHeight > AMaxH) then
      ANewHeight := AMaxH;
    if (FMinWidth > 0) and (ANewWidth < FMinWidth) then
      ANewWidth := FMinWidth;
    if (FMinHeight > 0) and (ANewHeight < FMinHeight) then
      ANewHeight := FMinHeight;
  end;
end;

procedure TView.DoBackgroundChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TView.DoChangeSize(var ANewWidth, ANewHeight: Single);
begin
  DoRecalcSize(ANewWidth, ANewHeight);
  DoAdjustViewBounds(ANewWidth, ANewHeight);
end;

procedure TView.DoCheckedChange;
begin
end;

procedure TView.DoClickEvent;
begin
end;

procedure TView.DoDeactivate;
begin
  DecViewState(TViewState.Activated);
  inherited DoDeactivate;
end;

procedure TView.DoEndUpdate;
begin
  inherited DoEndUpdate;
  TempMaxHeight := 0;
  TempMaxWidth := 0;
end;

function TView.DoGetUpdateRect: TRectF;
var
  LastFocus: Boolean;
begin
  LastFocus := CanFocus;
  CanFocus := False;
  Result := inherited DoGetUpdateRect;
  CanFocus := LastFocus;
end;

procedure TView.DoGravity;
begin
  Repaint;
end;

procedure TView.DoInVisibleChange;
begin
  if FInVisible then begin
    FAbsoluteInVisible := True;
    FRecalcInVisible := False;
  end else
    RecalcInVisible();
end;

procedure TView.DoLayoutChanged(Sender: TObject);
begin
  HandleSizeChanged;
end;

procedure TView.DoLinkClick(const Text, URL: string);
begin
end;

procedure TView.DoMatrixChanged(Sender: TObject);
begin
  inherited DoMatrixChanged(Sender);
  if Assigned(FBadgeView) and FBadgeView.GetEnabled then begin
    FBadgeView.SetVisible(Visible);
    if Visible then
      FBadgeView.Realign;
  end;
end;

procedure TView.DoMaxSizeChange;
begin
  HandleSizeChanged;
end;

procedure TView.DoMinSizeChange;
begin
  HandleSizeChanged;
end;

procedure TView.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if (csDesigning in ComponentState) or FInVisible then Exit;
  if (TMouseButton.mbLeft = Button) and (Clickable or (not (Self is TViewGroup))) then begin
    IncViewState(TViewState.Pressed);
    if CanRePaintBk(Self, TViewState.Pressed) then Repaint;
  end;
  {$IFDEF MSWINDOWS}
  if FCaptureDragForm then
    StartWindowDrag;
  {$ENDIF}
end;

procedure TView.DoMouseEnter;
begin
  inherited DoMouseEnter;
  if (csDesigning in ComponentState) or FInVisible then Exit;
  IncViewState(TViewState.Hovered);
  if CanRePaintBk(Self, TViewState.Hovered) then Repaint;
end;

procedure TView.DoMouseLeave;
begin
  inherited DoMouseLeave;
  DecViewState(TViewState.Hovered);
  if (csDesigning in ComponentState) or FInVisible then Exit;
  if CanRePaintBk(Self, FDrawState) then begin
    FInvaliding := False;
    Repaint;
  end;
end;

procedure TView.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if (TMouseButton.mbLeft = Button) and (Clickable or (not (Self is TViewGroup))) then begin
    DecViewState(TViewState.Pressed);
    if (csDesigning in ComponentState) or FInVisible then
      Exit;
    if CanRePaintBk(Self, TViewState.Pressed) then Repaint;
  end;
end;

function TView.GetCaptureDragForm: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := FCaptureDragForm;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TView.GetClickable: Boolean;
begin
  Result := HitTest;
end;

function TView.GetComponent: TComponent;
begin
  Result := Self;
end;

function TView.GetComponentState: TComponentState;
begin
  Result := ComponentState;
end;

function TView.GetContentBounds: TRectD;
begin
  Result := TRectD.Empty;
end;

function TView.GetDrawState: TViewState;
begin
  Result := FDrawState;
end;

function TView.GetGravity: TLayoutGravity;
begin
  Result := FGravity;
end;

function TView.GetHeightSize: TViewSize;
begin
  Result := FHeightSize;
end;

function TView.GetHScrollBar: TScrollBar;
begin
  Result := nil;
end;

function TView.GetInVisible: Boolean;
begin
  Result := FInVisible;
end;

function TView.GetIsChecked: Boolean;
begin
  Result := TViewState.Checked in FViewState;
end;

function TView.GetLayout: TViewLayout;
begin
  if not AllowUseLayout then
    Result := nil
  else begin
    if not Assigned(FLayout) then begin
      FLayout := TViewLayout.Create(Self);
      FLayout.OnChanged := DoLayoutChanged;
    end;
    Result := FLayout;
  end;
end;

function TView.GetMargin: string;
begin
  Result := GetBoundsFloat(Margins);
end;

function TView.GetMaxHeight: Single;
begin
  Result := FMaxHeight;
end;

function TView.GetMaxWidth: Single;
begin
  Result := FMaxWidth;
end;

function TView.GetMinHeight: Single;
begin
  Result := FMinHeight;
end;

function TView.GetMinWidth: Single;
begin
  Result := FMinWidth;
end;

class function TView.GetNavigationBarHeight: Single;
begin
  Result := NavigationBarHeight;
end;

function TView.GetOpacity: Single;
begin
  Result := AbsoluteOpacity;
end;

function TView.GetOrientation: TOrientation;
begin
  Result := FOrientation;
end;

function TView.GetPaddings: string;
begin
  Result := GetBoundsFloat(Padding);
end;

function TView.GetParentControl: TControl;
begin
  Result := ParentControl;
end;

function TView.GetParentForm: TCustomForm;
var
  P: TFmxObject;
begin
  Result := nil;
  P := Self;
  while P <> nil do begin
    if P is TCustomForm then begin
      Result := P as TCustomForm;
      Break;
    end else
      P := P.Parent;
  end;
end;

function TView.GetParentMaxHeight: Single;
begin
  if FMaxHeight > 0 then
    Result := FMaxHeight
  else begin
    if Assigned(ParentView) then begin
      Result := TView(Parent).GetParentMaxHeight;
      if Result > 0 then
        Result := Result - Margins.Top - Margins.Bottom
    end else begin
      Result := 0;
//      if HeightSize = TViewSize.WrapContent then begin
//        if (not (csDesigning in ComponentState)) and (Parent is TControl) then
//          Result := TControl(Parent).Height
//        else
//          Result := 0
//      end else
//        Result := Height;
    end;
  end;
end;

function TView.GetParentMaxWidth: Single;
begin
  if FMaxWidth > 0 then
    Result := FMaxWidth
  else begin
    if Assigned(ParentView) then begin
      Result := TView(Parent).GetParentMaxWidth;
      if Result > 0 then
        Result := Result - Margins.Left - Margins.Right
    end else
      Result := 0;
  end;
end;

function TView.GetParentView: IViewGroup;
begin
  Supports(Parent, IViewGroup, Result);
end;

function TView.GetPosition: TPosition;
begin
  Result := Position;
end;

function TView.GetSceneScale: Single;
begin
  Result := 0;
  if Scene <> nil then
    Result := Scene.GetSceneScale;
  if Result <= 0 then
    Result := 1;
end;

function TView.GetScrollSmallChangeFraction: Single;
begin
  Result := SmallChangeFraction;
end;

class function TView.GetStatusHeight: Single;
begin
  Result := StatusHeight;
end;

function TView.GetViewBackground: TDrawable;
begin
  if not Assigned(FBackground) then
    FBackground := CreateBackground();
  Result := FBackground;
end;

function TView.GetViewRect: TRectF;
begin
  Result := RectF(Padding.Left, Padding.Top,
    Width - Padding.Right + Padding.Left, Height - Padding.Bottom + Padding.Top);
end;

function TView.GetViewRectD: TRectD;
begin
  Result := RectD(Padding.Left, Padding.Top,
    Width - Padding.Right + Padding.Left, Height - Padding.Bottom + Padding.Top);
end;

function TView.GetViewStates: TViewStates;
begin
  Result := FViewState;
end;

function TView.GetVScrollBar: TScrollBar;
begin
  Result := nil;
end;

function TView.GetWeight: Single;
begin
  Result := FWeight;
end;

function TView.GetWidthSize: TViewSize;
begin
  Result := FWidthSize;
end;

class procedure TView.SetRttiObject(Instance: TObject; const Name: string;
  const Value: TObject);
var
  FType: TRttiType;
  FFiled: TRttiField;
  FContext: TRttiContext;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Instance.ClassType);
    FFiled := FType.GetField(Name);
    if Assigned(FFiled) and (FFiled.FieldType.TypeKind = tkClass) then
      FFiled.SetValue(Instance, Value);
  finally
    FContext.Free;
  end;
end;

class procedure TView.SetRttiValue(Instance: TObject; const Name: string; const Value: TValue);
var
  FType: TRttiType;
  FFiled: TRttiField;
  FContext: TRttiContext;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Instance.ClassType);
    FFiled := FType.GetField(Name);
    if Assigned(FFiled) then
      FFiled.SetValue(Instance, Value);
  finally
    FContext.Free;
  end;
end;

class procedure TView.SetRttiValue<T>(Instance: TObject; const Name: string;
  const Value: T);
var
  FType: TRttiType;
  FFiled: TRttiField;
  FContext: TRttiContext;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Instance.ClassType);
    FFiled := FType.GetField(Name);
    if not Assigned(FFiled) then Exit;
    if FFiled.FieldType.TypeKind <> PTypeInfo(TypeInfo(T)).Kind then
      Exit;
    FFiled.SetValue(Instance, TValue.From(Value));
  finally
    FContext.Free;
  end;
end;

class function TView.GetRttiValue(Instance: TObject; const Name: string): TValue;
var
  FType: TRttiType;
  FFiled: TRttiField;
  FContext: TRttiContext;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Instance.ClassType);
    FFiled := FType.GetField(Name);
    if Assigned(FFiled) then
      Result := FFiled.GetValue(Instance)
    else
      Result := nil;
  finally
    FContext.Free;
  end;
end;

class function TView.GetRttiValue<T>(Instance: TObject; const Name: string): T;
var
  FType: TRttiType;
  FFiled: TRttiField;
  FContext: TRttiContext;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Instance.ClassType);
    FFiled := FType.GetField(Name);
    if not Assigned(FFiled) then
      Result := T(nil)
    else
      Result := FFiled.GetValue(Instance).AsType<T>();
  finally
    FContext.Free;
  end;
end;

function TView.GetRealDrawState: TViewState;
begin
  if FViewState = [] then
    Result := TViewState.None
  else begin
    if TViewState.Enabled in FViewState then
      Result := TViewState.Enabled
    else if TViewState.Pressed in FViewState then
      Result := TViewState.Pressed
    else if TViewState.Focused in FViewState then
      Result := TViewState.Focused
    else if TViewState.Selected in FViewState then
      Result := TViewState.Selected
    else if TViewState.Checked in FViewState then
      Result := TViewState.Checked
    else if TViewState.Activated in FViewState then
      Result := TViewState.Activated
    else if TViewState.Hovered in FViewState then
      Result := TViewState.Hovered
    else
      Result := TViewState.None
  end;
end;

class function TView.GetRttiObject(Instance: TObject; const Name: string): TObject;
var
  V: TValue;
begin
  V := GetRttiValue(Instance, Name);
  if (V.IsEmpty) or (not V.IsObject) then
    Result := nil
  else
    Result := V.AsObject;
end;

procedure TView.HandleSizeChanged;
begin
  inherited HandleSizeChanged;
  if Assigned(ParentView) then begin
    if (csLoading in ComponentState) and (Children <> nil) then
      Exit;
    ParentControl.RecalcSize;
  end;
end;

procedure TView.HitTestChanged;
begin
  inherited HitTestChanged;
  if HitTest and (not AutoCapture) then
    AutoCapture := True;
end;

procedure TView.ImagesChanged;
begin
  Repaint;
end;

procedure TView.IncChildState(State: TViewState);
var
  I: Integer;
  View: IView;
begin
  if State = TViewState.None then Exit;
  if (csDestroying in ComponentState) or (csDesigning in ComponentState) then Exit;
  for I := 0 to Controls.Count - 1 do begin
    if (State = TViewState.Pressed) and Controls.Items[I].HitTest then
      Continue;
    if Supports(Controls.Items[I], IView, View) then begin
      if (State = TViewState.Hovered) and (Assigned(View.Background)) and Assigned(View.Background.FHovered) then begin
        if View.Background.FHovered.Kind <> TBrushKind.None then
          Continue;
      end;
      View.IncViewState(State);
    end;
  end;
end;

procedure TView.IncViewState(const State: TViewState);
begin
  Include(FViewState, State);
  FDrawState := GetRealDrawState;
  IncChildState(State);
end;

procedure TView.InitScrollbar;
begin
end;

procedure TView.InternalAlign;
begin
end;

procedure TView.Invalidate;
begin
  if not FInvaliding then
  begin
    InvalidateRect(LocalRect);
    FInvaliding := True;
  end;
end;

{$IFDEF ANDROID}
class function TView.AppContext: JContext;
begin
  {$IF CompilerVersion > 27}
  Result := TAndroidHelper.Context;
  {$ELSE}
  Result := SharedActivityContext;
  {$ENDIF}
end;
{$ENDIF}

class procedure TView.InvokeMethod(Instance: TObject; const Name: string;
  const Args: array of TValue);
var
  FMethod: TRttiMethod;
  FType: TRttiType;
  FContext: TRttiContext;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Instance.ClassType);
    FMethod := FType.GetMethod(Name);
    if Assigned(FMethod) then
      FMethod.Invoke(Instance, Args);
  finally
    FContext.Free;
  end;
end;

class function TView.InvokeMethod<T>(Instance: TObject; const Name: string;
  const Args: array of TValue): T;
var
  FMethod: TRttiMethod;
  FType: TRttiType;
  FContext: TRttiContext;
  FResult: TValue;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Instance.ClassType);
    FMethod := FType.GetMethod(Name);
    if Assigned(FMethod) then begin
      FResult := FMethod.Invoke(Instance, Args);
      if not FResult.IsEmpty then
        Result := FResult.AsType<T>
      else
        Result := T(nil);
    end else
      Result := T(nil);
  finally
    FContext.Free;
  end;
end;

{$IFDEF ANDROID}
class procedure TView.InitAudioManager();
var
  NativeService: JObject;
begin
  try
    NativeService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
    if not Assigned(NativeService) then
      Exit;
    FAudioManager := TJAudioManager.Wrap((NativeService as ILocalObject).GetObjectID);
  except
  end;
end;

class procedure TView.InitFontGlyphs();
//var
//  FCurrentManager: TFontGlyphManager;
begin
//  try
//    FCurrentManager := TFontGlyphManager.Current;
//    if Assigned(FCurrentManager) then
//      FreeAndNil(FCurrentManager);
//    FCurrentManager := TAndroidFontGlyphManagerFMXUI.Create;
//    SetRttiValue<TFontGlyphManager>('FCurrentManager', FCurrentManager);
//  except
//  end;
end;
{$ENDIF}

function TView.IsActivated: Boolean;
begin
  Result := TViewState.Activated in FViewState;
end;

function TView.IsAdjustLayout: Boolean;
begin
  Result := True;
end;

function TView.IsAutoSize: Boolean;
begin
  Result := False;
end;

function TView.IsDesignerControl(Control: TControl): Boolean;
begin
  Result := (csDesigning in ComponentState) and
    (Supports(Control, IDesignerControl) or
    (Control.ClassNameIs('TDesignRectangle')));
end;

function TView.IsDrawing: Boolean;
begin
  Result := FDrawing;
end;

function TView.IsHovered: Boolean;
begin
  Result := TViewState.Hovered in FViewState;
end;

function TView.IsPressed: Boolean;
begin
  Result := TViewState.Pressed in FViewState;
end;

function TView.GetAbsoluteInVisible: Boolean;
var
  PV: IViewGroup;
begin
  if FRecalcInVisible then begin
    if FInVisible then
      FAbsoluteInVisible := True
    else begin
      PV := ParentView;
      if Assigned(PV) then
        FAbsoluteInVisible := PV.GetAbsoluteInVisible
      else
        FAbsoluteInVisible := FInVisible;
    end;
    FRecalcInVisible := False;
  end;
  Result := FAbsoluteInVisible;
end;

function TView.GetAdjustViewBounds: Boolean;
begin
  Result := FAdjustViewBounds;
end;

function TView.GetAniCalculations: TScrollCalculations;
begin
  Result := nil;
end;

function TView.GetBackground: TDrawable;
begin
  Result := FBackground;
end;

function TView.GetBadgeView: IViewBadge;
begin
  Result := FBadgeView;
end;

procedure TView.Loaded;
begin
  inherited Loaded;
end;

procedure TView.MouseClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  {$IFDEF POSIX}
  FDownUpOffset := Y - FDownUpOffset;
  {$ENDIF}
  inherited;
end;

procedure TView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  {$IFDEF POSIX}
  FDownUpOffset := Y;
  {$ENDIF}
  inherited MouseDown(Button, Shift, X, Y);
  DoMouseDown(Button, Shift, X, Y);
end;

procedure TView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp(Button, Shift, X, Y);
end;

procedure TView.DoOrientation;
begin
end;

procedure TView.DoRecalcSize(var AWidth, AHeight: Single);
begin
end;

procedure TView.DoResize;
begin
  Resize;
end;

procedure TView.DoSetScrollBarValue(Scroll: TScrollBar; const Value, ViewportSize: Double);
begin
  //Scroll.ValueRange.Min := Min(Value, ContentBounds.Top);
  //Scroll.ValueRange.Max := Max(Value + ViewportSize, ContentBounds.Bottom);
  if Scroll.Height > Scroll.Width then begin
    Scroll.ValueRange.Min := Min(Value, ContentBounds.Top);
    Scroll.ValueRange.Max := Max(Value + ViewportSize, ContentBounds.Bottom);
  end else begin
    Scroll.ValueRange.Min := Min(Value, ContentBounds.Left);
    Scroll.ValueRange.Max := Max(Value + ViewportSize, ContentBounds.Right);
  end;
  Scroll.ValueRange.ViewportSize := ViewportSize;
  Scroll.ValueD := Value;
end;

function TView.DoSetSize(const ASize: TControlSize;
  const NewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
  var ALastWidth, ALastHeight: Single): Boolean;
begin
  DoChangeSize(ANewWidth, ANewHeight);
  Result := inherited DoSetSize(ASize, NewPlatformDefault, ANewWidth, ANewHeight,
    ALastWidth, ALastHeight);
end;

procedure TView.DoWeight;
begin
  HandleSizeChanged;
end;

procedure TView.EnabledChanged;
begin
  inherited EnabledChanged;
  if Enabled then begin
    DecViewState(TViewState.Enabled);
  end else begin
    IncViewState(TViewState.Enabled);
  end;
end;

class function TView.ExistRttiValue(Instance: TObject;
  const Name: string): Boolean;
var
  FType: TRttiType;
  FFiled: TRttiField;
  FContext: TRttiContext;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Instance.ClassType);
    FFiled := FType.GetField(Name);
    Result := Assigned(FFiled);
  finally
    FContext.Free;
  end;
end;

function TView.FindAndCloneStyleResource<T>(const AStyleLookup: string;
  var AResource: T): Boolean;
var
  StyleObject: TFmxObject;
begin
  StyleObject := nil;
  if FindStyleResource(AStyleLookup, StyleObject) then
    AResource := T(FindStyleResource(AStyleLookup, True));
  Result := StyleObject <> nil;
end;

function TView.FindStyleResource<T>(const AStyleLookup: string;
  var AResource: T): Boolean;
var
  StyleObject: TFmxObject;
begin
  StyleObject := FindStyleResource(AStyleLookup, False);
  Result := StyleObject is T;
  if Result then
    AResource := T(StyleObject);
end;

procedure TView.FreeScrollbar;
begin
end;

procedure TView.Paint;
begin
  if not FDrawing then begin
    inherited Paint;
    if FIsFocused then begin
      Include(FViewState, TViewState.Focused);
      FDrawState := GetRealDrawState;
    end else begin
      Exclude(FViewState, TViewState.Focused);
      if FDrawState = TViewState.Focused then
        FDrawState := GetRealDrawState;
    end;
    FDrawing := True;
    Canvas.BeginScene();
    try
      PaintBackground();
      if (csDesigning in ComponentState) and not Locked then
        DrawDesignBorder;
    finally
      Canvas.EndScene;
      FDrawing := False;
    end;
  end;
end;

procedure TView.PaintBackground;
begin
  if Assigned(FBackground) and (AbsoluteInVisible = False) then
    FBackground.Draw(Canvas);
end;

procedure TView.PlayClickEffect;
begin
  {$IFDEF ANDROID}
  PlaySoundEffect(0); // SoundEffectConstants.CLICK
  {$ENDIF}
end;

procedure TView.PlaySoundEffect(ASoundConstant: Integer);
{$IFDEF ANDROID}
var
  RingerMode: Integer;
begin
  if not Assigned(FAudioManager) then
    Exit;
  RingerMode := FAudioManager.getRingerMode;
  // 静音或者震动时不发出声音
  if (ringerMode = TJAudioManager.JavaClass.RINGER_MODE_SILENT) or
    (ringerMode = TJAudioManager.JavaClass.RINGER_MODE_VIBRATE) then
    Exit;
  FAudioManager.playSoundEffect(ASoundConstant);
{$ELSE}
begin
{$ENDIF}
end;

function TView.PointInObject(X, Y: Single): Boolean;
begin
  if AbsoluteInVisible then
    Result := False
  else
    Result := inherited PointInObject(X, Y);
end;

procedure TView.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
end;

procedure TView.RecalcInVisible;
var
  I: Integer;
  Item: TControl;
begin
  if FRecalcInVisible then Exit;
  FRecalcInVisible := True;
  for I := 0 to ControlsCount - 1 do begin
    Item := Controls[I];
    if Item is TView then
      (Item as TView).RecalcInVisible;
  end;
end;

procedure TView.SetAdjustViewBounds(const Value: Boolean);
begin
  if FAdjustViewBounds <> Value then begin
    FAdjustViewBounds := Value;
    HandleSizeChanged;
  end;
end;

procedure TView.SetBackground(const Value: TBitmap);
begin
  Background.SetBitmap(TViewState.None, Value);
end;

procedure TView.SetBackground(const Value: TBrushBitmap);
begin
  Background.SetBitmap(TViewState.None, Value);
end;

procedure TView.SetBackgroundBase(const Value: TDrawable);
begin
  SetBackground(Value);
end;

procedure TView.SetBadgeView(const Value: IViewBadge);
begin
  if Assigned(Self) then
    FBadgeView := Value;
end;

procedure TView.SetBackground(const Value: TAlphaColor);
begin
  Background.SetColor(TViewState.None, Value);
end;

procedure TView.SetBackground(const Value: TGradient);
begin
  Background.SetGradient(TViewState.None, Value);
end;

procedure TView.SetBackground(const Value: TDrawable);
begin
  if (not Assigned(FBackground)) and (Assigned(Value)) then
    FBackground := CreateBackground();
  if Assigned(FBackground) then
    FBackground.SetDrawable(Value);
end;

procedure TView.SetCaptureDragForm(const Value: Boolean);
begin
  {$IFDEF MSWINDOWS}
  FCaptureDragForm := Value;
  {$ELSE}
  {$ENDIF}
end;

procedure TView.SetClickable(const Value: Boolean);
begin
  HitTest := Value;
end;

procedure TView.SetDisableMouseWheel(const Value: Boolean);
begin
  if FDisableMouseWheel <> Value then begin
    FDisableMouseWheel := Value;
  end;
end;

procedure TView.SetGravity(const Value: TLayoutGravity);
begin
  if FGravity <> Value then begin
    FGravity := Value;
    DoGravity();
  end;
end;

procedure TView.SetHeightSize(const Value: TViewSize);
begin
  if Value <> FHeightSize then begin
    FHeightSize := Value;
    DoLayoutChanged(Self);
  end;
end;

procedure TView.SetInVisible(const Value: Boolean);
begin
  if FInVisible <> Value then begin
    FInVisible := Value;
    DoInVisibleChange;
    if Visible then
      Repaint;
  end;
end;

procedure TView.SetIsChecked(const Value: Boolean);
begin
  if Value <> GetIsChecked then begin
    if Value then
      IncViewState(TViewState.Checked)
    else
      DecViewState(TViewState.Checked);
    DoCheckedChange();
    Invalidate;
  end;
end;

procedure TView.SetLayout(const Value: TViewLayout);
begin
  if not AllowUseLayout then
    Exit;
  if (not Assigned(FLayout)) and (Assigned(Value)) then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  if Assigned(FLayout) then
    FLayout.Assign(Value);
end;

procedure TView.SetMargin(const Value: string);
var V: Single;
begin
  if Assigned(Margins) and GetFloatValue(Value, V) then
    Margins.Rect := RectF(V, V, V, V);
end;

procedure TView.SetMaxHeight(const Value: Single);
begin
  if FMaxHeight <> Value then begin
    FMaxHeight := Value;
    DoMaxSizeChange();
  end;
end;

procedure TView.SetMaxWidth(const Value: Single);
begin
  if FMaxWidth <> Value then begin
    FMaxWidth := Value;
    DoMaxSizeChange();
  end;
end;

procedure TView.SetMinHeight(const Value: Single);
begin
  if FMinHeight <> Value then begin
    FMinHeight := Value;
    DoMinSizeChange();
  end;
end;

procedure TView.SetMinWidth(const Value: Single);
begin
  if FMinWidth <> Value then begin
    FMinWidth := Value;
    DoMinSizeChange();
  end;
end;

procedure TView.SetOrientation(const Value: TOrientation);
begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    DoOrientation();
  end;
end;

procedure TView.SetPaddings(const Value: string);
var
  V: Single;
begin
  if Assigned(Padding) and GetFloatValue(Value, V) then
    Padding.Rect := RectF(V, V, V, V);
end;

procedure TView.SetScrollbar(const Value: TViewScroll);
begin
  if FScrollbar <> Value then begin
    FScrollbar := Value;
    FreeScrollbar;
    if FScrollbar <> TViewScroll.None then
      InitScrollbar;
    Repaint;
  end;
end;

procedure TView.SetTempMaxHeight(const Value: Single);
begin
  if FMaxHeight <> Value then begin
    if Value > 0 then begin
      FSaveMaxHeight := FMaxHeight;
      FMaxHeight := Value;
      DoMaxSizeChange();
    end else begin
      FMaxHeight := FSaveMaxHeight;
      FSaveMaxHeight := 0;
    end;
  end;
end;

procedure TView.SetTempMaxWidth(const Value: Single);
begin
  if FMaxWidth <> Value then begin
    if Value > 0 then begin
      FSaveMaxWidth := FMaxWidth;
      FMaxWidth := Value;
      DoMaxSizeChange()
    end else begin
      FMaxWidth := FSaveMaxWidth;
      FSaveMaxWidth := 0;
    end;
  end;
end;

procedure TView.SetViewStates(const Value: TViewStates);
begin
  FViewState := Value;
end;

procedure TView.SetWeight(const Value: Single);
begin
  if FWeight <> Value then begin
    FWeight := Value;
    DoWeight;
  end;
end;

procedure TView.SetWidthSize(const Value: TViewSize);
begin
  if Value <> FWidthSize then begin
    FWidthSize := Value;
    DoLayoutChanged(Self);
  end;
end;

procedure TView.StartScrolling;
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(Self, True);
end;

procedure TView.StartTriggerAnimation(const AInstance: TFmxObject;
  const ATrigger: string);
begin
  // inherited; disable all effect
end;

procedure TView.StartTriggerAnimationWait(const AInstance: TFmxObject;
  const ATrigger: string);
begin
  // inherited; disable all effect
end;

procedure TView.StartWindowDrag;
var
  F: TCustomForm;
begin
  F := ParentForm;
  if Assigned(F) then
    F.StartWindowDrag;
end;

procedure TView.StopScrolling;
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(nil, False);
end;

procedure TView.UpdateHScrollBar(const Value, ViewportSize: Double);
var
  AScroll: TScrollBar;
begin
  AScroll := HScrollBar;
  if AScroll <> nil then
  begin
    AScroll.ValueRange.BeginUpdate;
    try
      DoSetScrollBarValue(AScroll, Value, ViewportSize);
    finally
      AScroll.ValueRange.EndUpdate;
    end;
    AScroll.SmallChange := AScroll.ViewportSizeD / GetScrollSmallChangeFraction;
  end;
end;

procedure TView.UpdateVScrollBar(const Value, ViewportSize: Double);
var
  AScroll: TScrollBar;
begin
  AScroll := VScrollBar;
  if AScroll <> nil then
  begin
    AScroll.ValueRange.BeginUpdate;
    try
      DoSetScrollBarValue(AScroll, Value, ViewportSize);
    finally
      AScroll.ValueRange.EndUpdate;
    end;
    AScroll.SmallChange := AScroll.ViewportSizeD / GetScrollSmallChangeFraction;
  end;
end;

procedure TView.VisibleChanged;
begin
  inherited VisibleChanged;
  HandleSizeChanged;
end;

{ TViewGroup }

function TViewGroup.AddView(View: TView): Integer;
begin
  Result := Controls.Add(View);
end;

constructor TViewGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAcceptsControls(True);
end;

destructor TViewGroup.Destroy;
begin
  inherited Destroy;
end;

procedure TViewGroup.DoAddObject(const AObject: TFmxObject);
begin
  inherited DoAddObject(AObject);
  Realign;
end;

procedure TViewGroup.DoGravity;
begin
  //inherited DoGravity;
  Realign;
end;

procedure TViewGroup.DoLayoutChanged(Sender: TObject);
begin
  inherited DoLayoutChanged(Sender);
  Realign;
end;

procedure TViewGroup.DoMaxSizeChange;
begin
  inherited DoMaxSizeChange;
  if not Assigned(ParentView) then
    Realign;
end;

procedure TViewGroup.DoMinSizeChange;
begin
  inherited DoMinSizeChange;
  if not Assigned(ParentView) then
    Realign;
end;

// 此处的自动调整大小，是相对于父级线性布局组件相反方向而言，也就是在线性布局中
// 是否要调整组件的宽度或高度
function TViewGroup.IsAdjustSize(View: IView; Align: TAlignLayout;
  AParentOrientation: TOrientation): Boolean;
begin
  if Assigned(View) then begin
    // 实现了 IView 接口
    if AParentOrientation = TOrientation.Horizontal then
      // 当父级线性布局组件为水平方向时，高度随父组则需要调整高度
      Result := (View.GetHeightSize = TViewSize.FillParent)
    else
      // 当父组线性布局组件为垂直方向时，判断是否需要调整宽度
      Result := (View.GetWidthSize = TViewSize.FillParent);
  end else if (Align = TAlignLayout.None) or (Align = TAlignLayout.Scale) then
    // Align 不会调整大小
    Result := False
  else if AParentOrientation = TOrientation.Horizontal then
    // 这些 Align 值需要调整组件高度
    Result := Align in [TAlignLayout.Left, TAlignLayout.Right,
      TAlignLayout.MostLeft, TAlignLayout.MostRight,
      TAlignLayout.Client, TAlignLayout.Contents,
      TAlignLayout.HorzCenter, TAlignLayout.Vertical, TAlignLayout.Fit,
      TAlignLayout.FitLeft, TAlignLayout.FitRight]
  else
    // 这些 Align 值需要调整组件宽度
    Result := Align in [TAlignLayout.Top, TAlignLayout.Bottom,
      TAlignLayout.MostTop, TAlignLayout.MostBottom,
      TAlignLayout.Client, TAlignLayout.Contents,
      TAlignLayout.VertCenter, TAlignLayout.Horizontal, TAlignLayout.Fit,
      TAlignLayout.FitLeft, TAlignLayout.FitRight];
end;

procedure TViewGroup.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DoRealign;
end;

function TViewGroup.RemoveView(View: TView): Integer;
begin
  Result := Controls.Remove(View);
end;

procedure TViewGroup.Resize;
begin
  if csReading in ComponentState then
    Exit;
  inherited Resize;
  Realign;
end;

{ TLinearLayout }

procedure TLinearLayout.DoOrientation;
begin
  Realign;
end;

procedure TLinearLayout.DoRealign;
var
  CtrlCount: Integer;
  I: Integer;
  WeightSum: Double;
  LIsAdjustSize: Boolean;
  CurPos: TPointD;
  W, H, Fix: Single;
  VL, VT, VW, VH: Double;
  MaxW, MaxH: Double;
  Control: TControl;
  ReSizeView: TView;
  View: IView;
  SaveAdjustViewBounds, LAutoSize: Boolean;
  LAdjustControl: TControl;
  LAdjustSize: Single;
  IsWeight: Boolean;
begin
  if FDisableAlign then
    Exit;
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  //LogD(Self.ClassName + '.DoRealign.');

  FDisableAlign := True;

  // 得到父级组件的最大高宽
  MaxW := GetParentMaxWidth;
  MaxH := GetParentMaxHeight;

  // 得到子组件的开始坐标
  CurPos := TPointD.Create(Padding.Left, Padding.Top);
  W := Width - CurPos.X - Padding.Right;
  H := Height - CurPos.Y - Padding.Bottom;
  CtrlCount := ControlsCount;

  // 如果长宽 > 0 和子控件 > 0 时才处理布局
  if ((W > 0) and (H > 0)) or (CtrlCount > 0) then begin
    // 获取所有子组件的重力大小之和
    WeightSum := GetWeightSum(Fix);
    IsWeight := WeightSum > 0;
    LAdjustControl := nil;
    // 如果 WeightSum 大于0，说明使用了重力, 则不进行组件自动大小处理了
    LIsAdjustSize := (WeightSum <= 0) and AdjustAutoSizeControl(LAdjustControl, LAdjustSize);

    // 如果没有自动调整指定方向上的大小，则根据重力，自动决定组件的开始位置
    if (not LIsAdjustSize) then begin
      if Orientation = TOrientation.Horizontal then begin
        // 水平布局
        if FGravity in [TLayoutGravity.CenterHorizontal, TLayoutGravity.CenterHBottom, TLayoutGravity.Center] then
          // 水平居中
          CurPos.X := (W - Fix) / 2 + Padding.Left
        else if FGravity in [TLayoutGravity.RightTop, TLayoutGravity.RightBottom, TLayoutGravity.CenterVRight] then
          // 右边
          CurPos.X := W - Fix + Padding.Left;
      end else begin
        // 垂直布局
        if FGravity in [TLayoutGravity.CenterVertical, TLayoutGravity.CenterVRight, TLayoutGravity.Center] then
          // 垂直居中
          CurPos.Y := (H - Fix) / 2 + Padding.Top
        else if FGravity in [TLayoutGravity.LeftBottom, TLayoutGravity.RightBottom, TLayoutGravity.CenterHBottom] then
          // 底边
          CurPos.Y := H - Fix + Padding.Top;
      end;
    end;

    for I := 0 to CtrlCount - 1 do begin
      Control := Controls[I];
      {$IFDEF MSWINDOWS}
      // 如果在设计状态，组件是 DesignerControl 时忽略
      if (csDesigning in ComponentState) then begin
        if Supports(Control, IDesignerControl) then
          Continue;
        if IsDesignerControl(Control) then
          Continue;
      end;
      {$ENDIF}
      if not Control.Visible then Continue;

      // 得到组件IView接口，及是否启用最大最小大小限制
      View := nil;
      if (Supports(Control, IView, View)) then begin
        SaveAdjustViewBounds := View.GetAdjustViewBounds;
      end else
        SaveAdjustViewBounds := False;

      // 判断组件在另一个方向是否需要自动大小
      LAutoSize := IsAdjustSize(View, Control.Align, FOrientation);

      // 水平布局
      if FOrientation = TOrientation.Horizontal then begin
        // 计算 Left
        VL := CurPos.X + Control.Margins.Left;

        // 计算宽度
        if Assigned(View) and (WeightSum > 0) and (View.GetWeight > 0) then begin
          // 如果使用重力，则根据重力计算宽度
          VW := (W - Fix) / WeightSum * View.GetWeight - Control.Margins.Left - Control.Margins.Right;
        end else if Control = LAdjustControl then begin
          // 如果是需要自动调整大小的组件
          VW := LAdjustSize - Control.Margins.Right - Control.Margins.Left;
        end else begin
          VW := Control.Width;
        end;

        //LogD(Format('I: %d, WeightSum: %.3f, Fix: %.2f, VW: %.2f', [I, WeightSum, Fix, Control.Width]));

        // 检测宽度大小限制
        if SaveAdjustViewBounds then begin
          if (View.GetMaxWidth > 0) and (VW > View.GetMaxWidth) then
            VW := View.GetMaxWidth;
          if (View.GetMinWidth > 0) and (VW < View.GetMinWidth) then
            VW := View.GetMinWidth;
        end;

        if LAutoSize then begin
          // 自动高度
          VT := CurPos.Y + Control.Margins.Top;
          VH :=  H - VT - Control.Margins.Bottom + Padding.Top;

          // 检测高度大小限制
          if SaveAdjustViewBounds then begin
            if (View.GetMaxHeight > 0) and (VH > View.GetMaxHeight) then
              VH := View.GetMaxHeight;
            if (View.GetMinHeight > 0) and (VH < View.GetMinHeight) then
              VH := View.GetMinHeight;
          end;
        end else begin
          VH := Control.Height;

          // 检测高度大小限制
          if SaveAdjustViewBounds then begin
            if (View.GetMaxHeight > 0) and (VH > View.GetMaxHeight) then
              VH := View.GetMaxHeight;
            if (View.GetMinHeight > 0) and (VH < View.GetMinHeight) then
              VH := View.GetMinHeight;
          end;

          // 非自动高度时，以重力设置来调整位置
          case FGravity of
            TLayoutGravity.LeftTop, TLayoutGravity.RightTop:
              // 顶部
              VT := CurPos.Y + Control.Margins.Top;
            TLayoutGravity.LeftBottom, TLayoutGravity.RightBottom, TLayoutGravity.CenterHBottom:
              // 底部
              VT := H - VH - Control.Margins.Bottom + Padding.Top;
            TLayoutGravity.CenterVertical, TLayoutGravity.Center, TLayoutGravity.CenterVRight:
              // 居中
              VT := (H - (VH + Control.Margins.Top + Control.Margins.Bottom)) / 2 + Padding.Top + Control.Margins.Top;
          else
            begin
              if Align in [TAlignLayout.None, TAlignLayout.Scale] then
                // 自定义位置
                VT := Control.Position.Y
              else
                // 使用 Align 属性，默认左上角
                VT := CurPos.Y + Control.Margins.Top;
            end;
          end;
        end;

        // 根据 Align 来调整单个组件的位置
        if not LAutoSize then begin
          case Control.Align of
            TAlignLayout.Bottom, TAlignLayout.MostBottom:
              VT := H - VH - Control.Margins.Bottom + Padding.Top;
            TAlignLayout.Center, TAlignLayout.VertCenter:
              VT := (H - VH) / 2 + Padding.Top;
          end;
        end;

        // 重置重力设置
        if Assigned(View) and (View.GetWeight > 0) then begin
          Fix := Fix + VW + Control.Margins.Left + Control.Margins.Right;
          WeightSum := WeightSum - View.GetWeight;
        end;

      // 垂直布局
      end else begin

        // 计算 Top
        VT := CurPos.Y + Control.Margins.Top;
        // 计算高度
        if Assigned(View) and (WeightSum > 0) and (View.GetWeight > 0) then begin
          // 如果使用重力，则根据重力计算宽度
          VH := (H - Fix) / WeightSum * View.GetWeight - Control.Margins.Top - Control.Margins.Bottom;
        end else if Control = LAdjustControl then begin
          // 如果是需要自动调整大小的组件
          VH := LAdjustSize - Control.Margins.Bottom - Control.Margins.Top;
        end else
          VH := Control.Height;

        // 检测高度大小限制
        if SaveAdjustViewBounds then begin
          if (View.GetMaxHeight > 0) and (VH > View.GetMaxHeight) then
            VH := View.GetMaxHeight;
          if (View.GetMinHeight > 0) and (VH < View.GetMinHeight) then
            VH := View.GetMinHeight;
        end;

        if LAutoSize then begin
          // 自动宽度
          VL := CurPos.X + Control.Margins.Left;
          VW := W - VL - Control.Margins.Right + Padding.Left;

          // 检测宽度大小限制
          if SaveAdjustViewBounds then begin
            if (View.GetMaxWidth > 0) and (VW > View.GetMaxWidth) then
              VW := View.GetMaxWidth;
            if (View.GetMinWidth > 0) and (VW < View.GetMinWidth) then
              VW := View.GetMinWidth;
          end;
        end else begin
          VW := Control.Width;

          // 检测宽度大小限制
          if SaveAdjustViewBounds then begin
            if (View.GetMaxWidth > 0) and (VW > View.GetMaxWidth) then
              VW := View.GetMaxWidth;
            if (View.GetMinWidth > 0) and (VW < View.GetMinWidth) then
              VW := View.GetMinWidth;
          end;

          // 非自动宽度时，以重力设置来调整位置
          case FGravity of
            TLayoutGravity.LeftTop, TLayoutGravity.LeftBottom:
              // 左边
              VL := CurPos.X + Control.Margins.Left;
            TLayoutGravity.RightTop, TLayoutGravity.RightBottom, TLayoutGravity.CenterVRight:
              // 右边
              VL := W - VW - Control.Margins.Right + Padding.Left;
            TLayoutGravity.CenterHBottom, TLayoutGravity.Center:
              // 中间
              VL := (W - (VW + Control.Margins.Left + Control.Margins.Right)) / 2 + Padding.Left + Control.Margins.Left;
          else
            begin
              if Align in [TAlignLayout.None, TAlignLayout.Scale] then
                // 自定义位置
                VL := Control.Position.X
              else
                // 使用 Align 属性，默认左上角
                VL := CurPos.X + Control.Margins.Left;
            end;
          end;
        end;

        // 根据 Align 来调整单个组件的位置
        if not LAutoSize then begin
          case Control.Align of
            TAlignLayout.Right, TAlignLayout.MostRight:
              VL := W - VW - Control.Margins.Right + Padding.Left;
            TAlignLayout.Center, TAlignLayout.HorzCenter:
              VL := (W - VW) / 2 + Padding.Left;
          end;
        end;

        // 重置重力设置
        if Assigned(View) and (View.GetWeight > 0) then begin
          Fix := Fix + VH + Control.Margins.Top + Control.Margins.Bottom;
          WeightSum := WeightSum - View.GetWeight;
        end;
      end;

//      LogD(Format('I: %d, Name: %s, Width: %.2f, VW: %.2f', [I, Control.Name, Control.Width, VW]));

      // 调整组件大小
      if Assigned(View) then begin
        Control.SetBounds(VL, VT, VW, VH);
        //SetAdjustViewBounds(SaveAdjustViewBounds);
      end else
        Control.SetBounds(VL, VT, VW, VH);

      // 重新计算Fix，以组件调整后的真实大小为准
      if FOrientation = TOrientation.Horizontal then
        Fix := Fix + Control.Width - VW
      else
        Fix := Fix + Control.Height - VH;

      // 更新当前坐标
      if FOrientation = TOrientation.Horizontal then begin
        CurPos.X := VL + Control.Width + Control.Margins.Right;
      end else
        CurPos.Y := VT + Control.Height + Control.Margins.Bottom;
    end;

    // 判断是否组件大小为随内容。如果是，根据内容大小调整大小
    if Orientation = TOrientation.Horizontal then begin
      VW := CurPos.X + Padding.Right;
      VH := Height;

      // 高度超过上限
      if (VW > MaxW) and (MaxW > 0) then begin
        // 如果使用了 Weight
        if IsWeight then begin
          // 获取最后使用 Weight 属性的组件，重新调整大小
          ReSizeView := GetLastWeightView();
          if Assigned(ReSizeView) then begin
            ReSizeView.TempMaxWidth := ReSizeView.Width - (VW - MaxW);
            ReSizeView.Width := ReSizeView.TempMaxWidth;
          end;
        end;
        VW := MaxW;
      end;

      if (WidthSize = TViewSize.WrapContent) and (Width <> VW) then
        SetBounds(Left, Top, VW, VH);

    end else begin
      VW := Width;
      VH := CurPos.Y + Padding.Bottom;

      // 高度超过上限
      if (VH > MaxH) and (MaxH > 0) then begin
        // 如果使用了 Weight
        if IsWeight then begin
          // 获取最后使用 Weight 属性的组件，重新调整大小
          ReSizeView := GetLastWeightView();
          if Assigned(ReSizeView) then begin
            ReSizeView.TempMaxHeight := ReSizeView.Height - (VH - MaxH);
            ReSizeView.Height := ReSizeView.TempMaxHeight;
          end;
        end;
        VH := MaxH;
      end;

      if (HeightSize = TViewSize.WrapContent) and (Height <> VH) then
        SetBounds(Left, Top, VW, VH);

    end;

  end else
    inherited DoRealign;
  FDisableAlign := False;
  //LogD(Self.ClassName + '.DoRealign OK.');
end;

procedure TLinearLayout.DoRecalcSize(var AWidth, AHeight: Single);
var
  I: Integer;
  P, Control: TControl;
  IsAW, IsAH, IsASW, IsASH: Boolean;
  V: Single;
begin
  if IsUpdating or (csDestroying in ComponentState) or (csLoading in ComponentState) then
    Exit;
  if not Assigned(ParentView) then begin
    P := ParentControl;
    IsAW := False;
    IsAH := False;

    // 在水平上是否自动大小
    IsASW := IsAdjustSize(nil, Align, TOrientation.Vertical);
    // 在垂直上是否自动大小
    IsASH := IsAdjustSize(nil, Align, TOrientation.Horizontal);

    // 水平方向
    if (FOrientation = TOrientation.Horizontal) and (not IsASW) then begin
      if WidthSize = TViewSize.WrapContent then begin
        AWidth := Padding.Left + Padding.Right;
        IsAW := True;
      end else if WidthSize = TViewSize.FillParent then begin
        if Assigned(P) then
          AWidth := P.Width - P.Padding.Left - P.Padding.Right - Margins.Left - Margins.Right
        else
          AWidth := Padding.Left + Padding.Right;
      end;
    end else begin
      IsASW := ((WidthSize = TViewSize.WrapContent) and (FOrientation = TOrientation.Vertical));
      if IsASW then
        AWidth := 0;
    end;

    // 垂直方向
    if (FOrientation = TOrientation.Vertical) and (not IsASH) then begin
      if HeightSize = TViewSize.WrapContent then begin
        AHeight := Padding.Top + Padding.Bottom;
        IsAH := True;
      end else if HeightSize = TViewSize.FillParent then begin
        if Assigned(P) then
          AHeight := P.Height - P.Padding.Top - P.Padding.Bottom - Margins.Top - Margins.Bottom
        else
          AHeight := Padding.Top + Padding.Bottom;
      end
    end else begin
      IsASH := ((HeightSize = TViewSize.WrapContent)) and (FOrientation = TOrientation.Horizontal);
      if IsASH then
        AHeight := 0;
    end;

    // 如果有需要自动大小的，则将子控件大小加起来
    if IsAW or IsAH or IsASW or IsASH then begin
      for I := 0 to ControlsCount - 1 do begin
        Control := Controls[I];
        if not Control.Visible then Continue;
        {$IFDEF MSWINDOWS}
        if IsDesignerControl(Control) then Continue;
        {$ENDIF}

        if IsAW then
          AWidth := AWidth + Control.Width + Control.Margins.Left + Control.Margins.Right
        else if IsASW then begin
          V := Control.Position.X + Control.Width + Control.Margins.Right;
          if V > AWidth then
            AWidth := V;
        end;

        if IsAH then
          AHeight := AHeight + Control.Height + Control.Margins.Top + Control.Margins.Bottom
        else if IsASH then begin
          V := Control.Position.Y + Control.Height + Control.Margins.Bottom;
          if V > AHeight then
            AHeight := V;
        end;
      end;

      if IsASW then AWidth := AWidth + Padding.Left + Padding.Right;
      if IsASH then AHeight := AHeight + Padding.Top + Padding.Bottom;

    end;

  end else begin

    if FDisableAlign then
      Exit;

    // 在水平上是否自动大小
    IsASW := IsAdjustSize(Self, Align, TOrientation.Vertical);
    // 在垂直上是否自动大小
    IsASH := IsAdjustSize(Self, Align, TOrientation.Horizontal);

    IsAW := (WidthSize = TViewSize.WrapContent) and (not IsASW);
    IsAH := (HeightSize = TViewSize.WrapContent) and (not IsASH);

    if IsAW then AWidth := 0;
    if IsAH then AHeight := 0;

    // 如果有需要自动大小的，则将子控件大小加起来
    if IsAW or IsAH then begin
      for I := 0 to ControlsCount - 1 do begin
        Control := Controls[I];
        if not Control.Visible then Continue;
        {$IFDEF MSWINDOWS}
        if IsDesignerControl(Control) then Continue;
        {$ENDIF}

        if IsAW then begin
          V := Control.Position.X + Control.Width + Control.Margins.Right;
          if V > AWidth then
            AWidth := V;
        end;
        if IsAH then begin
          V := Control.Position.Y + Control.Height + Control.Margins.Bottom;
          if V > AHeight then
            AHeight := V;
        end;
      end;

      if IsAW then AWidth := AWidth + Padding.Left + Padding.Right;
      if IsAH then AHeight := AHeight + Padding.Top + Padding.Bottom;
    end;
  end;
end;

function TLinearLayout.AdjustAutoSizeControl(out AControl: TControl;
  out AdjustSize: Single): Boolean;
var
  I: Integer;
  Control: TControl;
  View: IView;
  AO, FO: TOrientation;
  NewSize: Single;
begin
  Result := False;
  AControl := nil;  // 注意：东京版 out 参数后，这里即使会默认设为 nil ，但在 Release 模式无效
  AdjustSize := 0;
  NewSize := 0;

  // 得到一个相反的布局方面，用于 IsAutoSize
  FO := FOrientation;
  if FO = TOrientation.Horizontal then
    AO := TOrientation.Vertical
  else
    AO := TOrientation.Horizontal;

  for I := ControlsCount - 1 downto 0 do begin
    Control := Controls[I];
    if not Control.Visible then Continue;
    {$IFDEF MSWINDOWS}
    if IsDesignerControl(Control) then Continue;
    {$ENDIF}

    // 如果还没有找到需要自动大小的组件，则进行检测
    if (AControl = nil) then begin
      View := nil;
      Supports(Control, IView, View);
      if (IsAdjustSize(View, Control.Align, AO)) then begin
        AControl := Control;
        Continue;
      end;
    end;
    //  累加非自动大小控件的大小
    if FO = TOrientation.Horizontal then
      NewSize := NewSize + Control.Margins.Left + Control.Width + Control.Margins.Right
    else
      NewSize := NewSize + Control.Margins.Top + Control.Height + Control.Margins.Bottom;
  end;

  // 如果存在有需要自动大小的组件，则调整其大小
  if AControl <> nil then begin
    Result := True;
    if FO = TOrientation.Horizontal then
      AdjustSize := FSize.Width - Padding.Left - Padding.Right - NewSize
    else
      AdjustSize := FSize.Height - Padding.Top - Padding.Bottom - NewSize
  end;
end;

function TLinearLayout.GetLastWeightView: TView;
var
  I: Integer;
  Control: TControl;
  View: IView;
begin
  Result := nil;
  for I := ControlsCount - 1 downto 0 do begin
    Control := Controls[I];
    if (not Control.Visible) then Continue;
    if (Supports(Control, IView, View)) and (View.GetWeight > 0) then begin
      Result := Control as TView;
      Break;
    end
  end;
end;

function TLinearLayout.GetWeightSum(var FixSize: Single): Single;
var
  I: Integer;
  Control: TControl;
  View: IView;
begin
  Result := 0;
  FixSize := 0;
  for I := 0 to ControlsCount - 1 do begin
    Control := Controls[I];
    if (not Control.Visible) then Continue;
    {$IFDEF MSWINDOWS}
    if IsDesignerControl(Control) then Continue;
    {$ENDIF}
    if (Supports(Control, IView, View)) and (View.GetWeight > 0) then
      Result := Result + View.GetWeight
    else begin
      if FOrientation = TOrientation.Horizontal then
        FixSize := FixSize + Control.Width + Control.Margins.Left + Control.Margins.Right
      else
        FixSize := FixSize + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
    end;
    // LogD(Format('I: %d, W: %.2f, FixSize: %.2f', [I, Control.Width, FixSize]));
  end;
end;

function TLinearLayout.IsUseWeight: Boolean;
var V: Single;
begin
  Result := GetWeightSum(V) > 0;
end;

{ TRelativeLayout }

constructor TRelativeLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViewList := TList<TControl>.Create;
end;

destructor TRelativeLayout.Destroy;
begin
  FreeAndNil(FViewList);
  inherited;
end;

procedure TRelativeLayout.DoAlignControl(const X, Y, W, H: Single);
var
  R: TRectF;
  AlignList: TInterfaceList;
  ALastWidth, ALastHeight: Single;
  List: TList<TControl>;

  function InsertBefore(const C1, C2: IAlignableObject; AAlign: TAlignLayout): Boolean;
  begin
    Result := False;
    case AAlign of
      TAlignLayout.Top, TAlignLayout.MostTop:
        Result := C1.Top < C2.Top;
      TAlignLayout.Bottom, TAlignLayout.MostBottom:
        Result := (C1.Top + C1.Height) >= (C2.Top + C2.Height);
      TAlignLayout.Left, TAlignLayout.MostLeft:
        Result := C1.Left < C2.Left;
      TAlignLayout.Right, TAlignLayout.MostRight:
        Result := (C1.Left + C1.Width) >= (C2.Left + C2.Width);
    end;
  end;

  procedure DoAlign(List: TList<TControl>; AAlign: TAlignLayout);
  var
    I, J: Integer;
    Control: TControl;
    LControl: IAlignableObject;
    ALCount: Integer;
  begin
    AlignList.Clear;
    for I := 0 to List.Count - 1 do begin
      Control := TControl(List.Items[I]);
      if not Supports(Control, IAlignableObject, LControl) then
        Continue;
      if (AAlign = TALignLayout.None) and (csLoading in Control.ComponentState) then
        Continue;
      if (LControl.Align = AAlign) and (LControl.AllowAlign) then
      begin
        J := 0;
        ALCount := AlignList.Count;
        while (J < ALCount) and (AAlign <> TAlignLayout.None) and not InsertBefore(LControl, IAlignableObject(AlignList[J]), AAlign) do
          Inc(J);
        AlignList.Insert(J, LControl);
      end;
    end;
    ALCount := AlignList.Count;
    for I := 0 to ALCount - 1 do begin
      ArrangeControl(IAlignableObject(AlignList[I]), AAlign, W, H, ALastWidth, ALastHeight, R);
    end;
  end;

  procedure DoGetList(const List: TList<TControl>);
  var
    View: IView;
    Control: TControl;
    I: Integer;
  begin
    List.Clear;
    for I := 0 to ControlsCount - 1 do begin
      Control := Controls[I];
      if not Control.Visible then Continue;
      {$IFDEF MSWINDOWS}
      if (csDesigning in ComponentState)
        and Supports(Control, IDesignerControl) then Continue;
      {$ENDIF}

      if (Supports(Control, IView, View)) then begin
        if (Assigned(View.GetLayout)) then begin
          FViewList.Add(Control);
          Continue;
        end;
      end;

      if (Control.Align = TALignLayout.None) or (csLoading in Control.ComponentState) then
        Continue;

      List.Add(Control);
    end;
  end;

begin
  if (csDestroying in ComponentState) or (W < 1) or (H < 1) then
    Exit;
  AlignList := TInterfaceList.Create;
  ALastWidth := W;
  ALastHeight := H;
  R := RectF(0, 0, W, H);
  R := Padding.PaddingRect(R);
  List := TList<TControl>.Create;
  try
    DoGetList(List);
    // Align
    DoAlign(List, TAlignLayout.MostTop);
    DoAlign(List, TAlignLayout.MostBottom);
    DoAlign(List, TAlignLayout.MostLeft);
    DoAlign(List, TAlignLayout.MostRight);
    DoAlign(List, TAlignLayout.Top);
    DoAlign(List, TAlignLayout.Bottom);
    DoAlign(List, TAlignLayout.Left);
    DoAlign(List, TAlignLayout.Right);
    DoAlign(List, TAlignLayout.FitLeft);
    DoAlign(List, TAlignLayout.FitRight);
    DoAlign(List, TAlignLayout.Client);
    DoAlign(List, TAlignLayout.Horizontal);
    DoAlign(List, TAlignLayout.Vertical);
    DoAlign(List, TAlignLayout.Contents);
    DoAlign(List, TAlignLayout.Center);
    DoAlign(List, TAlignLayout.HorzCenter);
    DoAlign(List, TAlignLayout.VertCenter);
    DoAlign(List, TAlignLayout.Scale);
    DoAlign(List, TAlignLayout.Fit);
    // Anchors
    DoAlign(List, TAlignLayout.None);
    FLastWidth := W;
    FLastHeight := H;
  finally
    AlignList.Free;
    List.Free;
  end;
end;

procedure TRelativeLayout.DoRealign;
var
  List: TList<TControl>;
  W, H: Single;
  I: Integer;
  CurPos: TPointF;
  VL, VT, VW, VH: Single;
  View: TControl;
  LView: IView;
  //SaveAdjustViewBounds: Boolean;
begin
  if FDisableAlign or (not Assigned(FViewList)) then
    Exit;
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  FDisableAlign := True;
  CurPos := PointF(Padding.Left, Padding.Top);
  W := Self.Width - CurPos.X - Padding.Right;
  H := Self.Height - CurPos.Y - Padding.Bottom;

  if (W > 0) and (H > 0) then begin
    FViewList.Clear;
    DoAlignControl(CurPos.X, CurPos.Y, FSize.Width, FSize.Height);
    //SaveAdjustViewBounds := False;
    List := TList<TControl>.Create;
    try
      for I := 0 to FViewList.Count - 1 do begin
        View := FViewList[I];
        if not Supports(View, IView, LView) then Continue;

        List.Clear;
        if GetXY(List, View, VL, VT, VW, VH) < 0 then Exit;
        VL := VL + View.Margins.Left;
        VT := VT + View.Margins.Top;
        VW := VW - View.Margins.Left - View.Margins.Right;
        VH := VH - View.Margins.Top - View.Margins.Bottom;
        //LView.SetAdjustViewBounds(False);
        View.SetBounds(VL, VT, VW, VH);
        //LView.SetAdjustViewBounds(SaveAdjustViewBounds);
      end;
    finally
      List.Free;
    end;
  end;
  FDisableAlign := False;
end;

procedure TRelativeLayout.DoRecalcSize(var AWidth, AHeight: Single);
var
  I: Integer;
  Control: TControl;
  IsAW, IsAH: Boolean;
  V: Single;
begin
  if IsUpdating or (csDestroying in ComponentState) then
    Exit;

  IsAW := (WidthSize = TViewSize.WrapContent) and
    (not IsAdjustSize(nil, Align, TOrientation.Vertical));
  IsAH := (HeightSize = TViewSize.WrapContent) and
    (not IsAdjustSize(nil, Align, TOrientation.Horizontal));

  // 如果有需要自动大小的，则将子控件大小加起来
  if IsAW or IsAH then begin

    if IsAW then AWidth := 0;
    if IsAH then AHeight := 0;

    for I := 0 to {$IF CompilerVersion >= 30}ControlsCount{$ELSE}ChildrenCount{$ENDIF} - 1 do begin
      Control := Controls[I];
      if not Control.Visible then Continue;
      {$IFDEF MSWINDOWS}
      if IsDesignerControl(Control) then Continue;
      {$ENDIF}

      if IsAW then begin
        V := Control.Width + Control.Position.X + Control.Margins.Right + Padding.Right;
        if V > AWidth then
          AWidth := V;
      end;
      if IsAH then begin
        V := Control.Height + Control.Position.Y + Control.Margins.Bottom + Padding.Bottom;
        if V > AHeight then
          AHeight := V;
      end;
    end;
  end;
end;

procedure TRelativeLayout.DoRemoveObject(const AObject: TFmxObject);

  procedure RemoveLink(var Data: TControl);
  begin
    if Data = AObject then
      Data := nil;
  end;

var
  I: Integer;
  Item: TControl;
  View: IView;
  Layout: TViewLayout;
begin
  if not (csDestroying in ComponentState) then begin
    // 删除对象时，解除所有引用到它的地方
    for I := 0 to ControlsCount - 1 do begin
      Item := Controls[I];
      if Supports(Item, IView, View) then begin
        Layout := View.Layout;
        if Layout = nil then Continue;
        RemoveLink(Layout.FToLeftOf);
        RemoveLink(Layout.FToRightOf);
        RemoveLink(Layout.FAbove);
        RemoveLink(Layout.FBelow);
        RemoveLink(Layout.FAlignBaseline);
        RemoveLink(Layout.FAlignLeft);
        RemoveLink(Layout.FAlignTop);
        RemoveLink(Layout.FAlignRight);
        RemoveLink(Layout.FAlignBottom);
      end;
    end;
  end;
  inherited DoRemoveObject(AObject);
end;

function TRelativeLayout.GetXY(const StackList: TList<TControl>; const Control: TControl;
  var X, Y, W, H: Single): Integer;
var
  View: IView;
  Layout: TViewLayout;
  PW, PH: Single;
  AX, AY, AW, AH: Single;
  BX, BY, BW, BH: Single;
  I: Integer;
  DecH, DecW, DecHD2: Boolean;
  AutoW, AutoH: Boolean;
  Parent: TControl;
begin
  Result := 1;
  if not Assigned(Control) then Exit;
  if csDestroying in Control.ComponentState then Exit;

  if Control.Visible then begin
    W := Control.Width + Control.Margins.Left + Control.Margins.Right;
    H := Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  end else begin
    W := 0;
    H := 0;
  end;

  if not (Supports(Control, IView, View)) then begin
    X := Control.Position.X;
    Y := Control.Position.Y;
    Exit;
  end else begin
    X := 0;
    Y := 0;
  end;
  if (StackList.Count > 0) then begin
    if StackList.Count > 256 then begin
      Result := -1;
      Exit;
    end;
    I := StackList.IndexOf(Control);
    if (I >= 0) then begin
      Result := -2;
      Exit;
    end;
  end;
  Layout := View.Layout;
  if not Assigned(Layout) then
    Exit;
  Parent := View.ParentControl;
  if Assigned(Parent) then begin
    PW := Parent.Width - Parent.Padding.Left - Parent.Padding.Right;
    PH := Parent.Height - Parent.Padding.Top - Parent.Padding.Bottom;
  end else begin
    PW := 0; PH := 0;
  end;
  if (not Layout.AlignParentLeft) and Assigned(View.Position) then
    X := View.Position.X;
  if (not Layout.AlignParentTop) and Assigned(View.Position) then
    Y := View.Position.Y;
  StackList.Add(Control);
  try

    DecH := False;
    DecW := False;
    DecHD2 := False;

    AutoW := View.WidthSize = TViewSize.FillParent;
    AutoH := View.HeightSize = TViewSize.FillParent;

    if (Layout.FCenterInParent) or (Layout.FCenterVertical and Layout.FCenterHorizontal) then begin
      if AutoW then W := PW;
      if AutoH then H := PH;
      if Assigned(Parent) then begin
        X := Parent.Padding.Left + (PW - W) / 2;
        Y := Parent.Padding.Top + (PH - H) / 2;
      end;
      Exit;
    end;

    if Layout.FCenterVertical then begin
      if AutoH then H := PH;
      if Assigned(Parent) then
        Y := (PH - H) / 2;
    end else if Assigned(Layout.FAlignBaseline) then begin
      if AutoH then begin
        H := PH;
        Y := 0;
      end else begin
        Result := GetXY(StackList, Layout.FAlignBaseline, AX, AY, AW, AH);
        if Result < 0 then Exit;
        Y := AY + AH / 2;
        DecHD2 := True;
      end;
    end else if Assigned(Layout.FAlignTop) then begin
      Result := GetXY(StackList, Layout.FAlignTop, AX, AY, AW, AH);
      if Result < 0 then Exit;
      Y := AY + Layout.FAlignTop.Margins.Top;
      if Assigned(Layout.FAlignBottom) then begin
        Result := GetXY(StackList, Layout.FAlignBottom, BX, BY, BW, BH);
        if Result < 0 then Exit;
        H := (BY + BH + Layout.FAlignBottom.Margins.Bottom) - Y;
      end else if AutoH then
        H := PH - Y;
    end else if Assigned(Layout.FAlignBottom) then begin
      Result := GetXY(StackList, Layout.FAlignBottom, AX, AY, AW, AH);
      if Result < 0 then Exit;
      Y := AY + AH - Layout.FAlignBottom.Margins.Bottom;
      if AutoH then begin
        H := Y;
        Y := 0;
      end else
        DecH := True;
    end else if Assigned(Layout.FAbove) then begin
      Result := GetXY(StackList, Layout.FAbove, AX, AY, AW, AH);
      if Result < 0 then Exit;
      Y := AY + Layout.FAbove.Margins.Top;
      if Assigned(Layout.FBelow) then begin
        Result := GetXY(StackList, Layout.FBelow, BX, BY, BW, BH);
        if Result < 0 then Exit;
        H := Y - (BY + BH + Layout.FBelow.Margins.Bottom);
      end else begin
        if AutoH then begin
          H := Y;
          Y := 0;
        end else
          DecH := True;
      end;
    end else if Assigned(Layout.FBelow) then begin
      Result := GetXY(StackList, Layout.FBelow, BX, BY, BW, BH);
      if Result < 0 then Exit;
      Y := BY + BH + Layout.FBelow.Margins.Bottom;
      if AutoH then
        H := PH - Y;
    end else if Layout.FAlignParentTop then begin
      if Assigned(Parent) then
        Y := Parent.Padding.Top
      else
        Y := 0;
      if AutoH then H := PH;
    end else if Layout.FAlignParentBottom then begin
      if AutoH then begin
        Y := 0;
        H := PH;
      end else if Assigned(Parent) then
        Y := PH - H + Parent.Padding.Top
      else
        Y := PH - H;
    end else begin
      if AutoH then
        H := PH - Y;
    end;

    if Layout.FCenterHorizontal then begin
      if AutoW then W := PW;
      if Assigned(Parent) then
        X := Parent.Padding.Left + (PW - W) / 2;
    end else if Assigned(Layout.FAlignLeft) then begin
      Result := GetXY(StackList, Layout.FAlignLeft, AX, AY, AW, AH);
      if Result < 0 then Exit;
      X := AX - Layout.FAlignLeft.Margins.Left;
      if Assigned(Layout.FAlignRight) then begin
        Result := GetXY(StackList, Layout.FAlignRight, BX, BY, BW, BH);
        if Result < 0 then Exit;
        W := (BX + BW + Layout.FAlignRight.Margins.Right) - X;
      end else if AutoW then
        W := PW - X;
    end else if Assigned(Layout.FAlignRight) then begin
      Result := GetXY(StackList, Layout.FAlignRight, AX, AY, AW, AH);
      if Result < 0 then Exit;
      X := AX + AW + Layout.FAlignRight.Margins.Right;
      if AutoW then begin
        W := X;
        X := 0;
      end else
        DecW := True;
    end else if Assigned(Layout.FToRightOf) then begin
      Result := GetXY(StackList, Layout.FToRightOf, AX, AY, AW, AH);
      if Result < 0 then Exit;
      X := AX + AW + Layout.FToRightOf.Margins.Right;
      if Assigned(Layout.FToLeftOf) then begin
        Result := GetXY(StackList, Layout.FToLeftOf, BX, BY, BW, BH);
        if Result < 0 then Exit;
        W := (BX - Layout.FToLeftOf.Margins.Left) - X;
      end else begin
        if AutoW then
          W := PW - X;
      end;
    end else if Assigned(Layout.FToLeftOf) then begin
      Result := GetXY(StackList, Layout.FToLeftOf, AX, AY, AW, AH);
      if Result < 0 then Exit;
      X := AX; // - Layout.FToLeftOf.Margins.Left;
      if AutoW then begin
        W := X;
        X := 0;
      end else
        DecW := True;
    end else if Layout.FAlignParentLeft then begin
      if Assigned(Parent) then
        X := Parent.Padding.Left
      else
        X := 0;
      if AutoW then W := PW;
    end else if Layout.FAlignParentRight then begin
      if AutoW then begin
        X := 0;
        W := PW;
      end else if Assigned(Parent) then
        X := PW - W + Parent.Padding.Left
      else
        X := PW - W;
    end else begin
      if AutoW then
        W := PW - X;
    end;

    if DecH then
      Y := Y - H
    else if DecHD2 then
      Y := Y - H / 2;
    if DecW then
      X := X - W;

  finally
    if StackList.Count > 0 then
      StackList.Delete(StackList.Count - 1);
  end;
end;

{ TTextSettingsBase }

function RoundToScale(const Value, Scale: Single): Single;
begin
  if Scale > 0 then
    Result := Ceil(Value * Scale) / Scale
  else
    Result := Ceil(Value);
end;

procedure TTextSettingsBase.Assign(Source: TPersistent);
var
  Src: TTextSettingsBase;
  LastOnChange: TNotifyEvent;
begin
  if Source is TTextSettingsBase then begin
    Src := TTextSettingsBase(Source);
    LastOnChange := Self.OnChanged;
    Self.OnChanged := nil;
    Self.Font := Src.Font;
    Self.Gravity := Src.Gravity;
    Self.AutoSize := Src.AutoSize;
    Self.PrefixStyle := Src.PrefixStyle;
    Self.Trimming := Src.Trimming;
    Self.WordWrap := Src.WordWrap;
    Self.FText := Src.FText;
    Self.OnChanged := LastOnChange;
    DoChange;
  end else
    inherited;
end;

function TTextSettingsBase.CalcTextHeight(const AText: string;
  SceneScale: Single): Single;
var
  S: TSizeF;
begin
  CalcTextObjectSize(AText, $FFFFFF, SceneScale, nil, S);
  Result := S.Height;
end;

function TTextSettingsBase.CalcTextObjectSize(const AText: string;
  const MaxWidth, SceneScale: Single; const Margins: TBounds; var Size: TSizeF): Boolean;
const
  FakeText = 'P|y'; // Do not localize
var
  LText: string;
  LMaxWidth: Single;
  Layout: TTextLayout;
begin
  Result := False;
  if (SceneScale >= 0) then
  begin
    if Margins <> nil then
      LMaxWidth := MaxWidth - Margins.Left - Margins.Right
    else
      LMaxWidth := MaxWidth;

    Layout := FLayout;
    if FPrefixStyle = TPrefixStyle.HidePrefix then
      LText := DelAmp(AText)
    else
      LText := AText;

    Layout.BeginUpdate;
    Layout.TopLeft := TPointF.Zero;
    if Layout.WordWrap and (LMaxWidth > 1) then
      Layout.MaxSize := TPointF.Create(LMaxWidth, TTextLayout.MaxLayoutSize.Y)
    else
      Layout.MaxSize := TTextLayout.MaxLayoutSize;
    if LText.IsEmpty then
      Layout.Text := FakeText
    else
      Layout.Text := LText;
    Layout.Trimming := FTrimming;
    Layout.VerticalAlign := TTextAlign.Leading;
    Layout.HorizontalAlign := TTextAlign.Leading;
    Layout.RightToLeft := False;
    Layout.EndUpdate;

    if LText.IsEmpty then begin
      Size.Width := 0;
    end else begin
      Size.Width := RoundToScale(FLayout.Width + FLayout.TextRect.Left * 2 + FLayout.Font.Size * 0.334, SceneScale);
    end;
    {$IFDEF ANDROID}
    //Size.Height := RoundToScale(FLayout.Height + FLayout.Font.Size * 0.334, SceneScale);
    Size.Height := RoundToScale(FLayout.Height, SceneScale);
//    if Size.Height > 50 then
//      Size.Height := Size.Height + FLayout.Font.Size * 0.6;
    {$ELSE}
    {$IFNDEF MSWINDOWS}
    //Size.Height := RoundToScale(FLayout.Height + FLayout.Font.Size * 0.2, SceneScale);
    Size.Height := RoundToScale(FLayout.Height, SceneScale);
    {$ELSE}
    Size.Height := RoundToScale(FLayout.Height, SceneScale);
    {$ENDIF}
    {$ENDIF}
//    {$IFNDEF MSWINDOWS}
// + FLayout.TextRect.Top * 2 + FLayout.Font.Size * 0.334
//    Size.Height := Size.Height + FLayout.Font.Size * 0.6;
//    {$ENDIF}

    if Margins <> nil then begin
      Size.Width := Size.Width + Margins.Left + Margins.Right;
      Size.Height := Size.Height + Margins.Top + Margins.Bottom;
    end;

    Result := True;
  end;
end;

function TTextSettingsBase.CalcTextWidth(const AText: string; SceneScale: Single): Single;
var
  S: TSizeF;
begin
  CalcTextObjectSize(AText, $FFFFFF, SceneScale, nil, S);
  Result := S.Width;
end;

procedure TTextSettingsBase.Change;
begin
  DoChange();
end;

constructor TTextSettingsBase.Create(AOwner: TComponent);
var
  DefaultValueService: IInterface;
  TrimmingDefault: TValue;
begin
  if AOwner is TControl then
    FOwner := TControl(AOwner)
  else FOwner := nil;

  FLayout := TTextLayoutManager.DefaultTextLayout.Create;
  FOnLastFontChanged := FLayout.Font.OnChanged;
  FLayout.Font.OnChanged := DoFontChanged;

  FPrefixStyle := TPrefixStyle.NoPrefix;
  if (csDesigning in AOwner.ComponentState) then begin
    FIsSizeChange := True;
    if TView(AOwner).SupportsPlatformService(IFMXDefaultPropertyValueService, DefaultValueService) then
    begin
      TrimmingDefault := IFMXDefaultPropertyValueService(DefaultValueService).GetDefaultPropertyValue(Self.ClassName, 'trimming');
      if not TrimmingDefault.IsEmpty then
        FTrimming := TrimmingDefault.AsType<TTextTrimming>;
    end;
  end else
    FIsSizeChange := False;
end;

destructor TTextSettingsBase.Destroy;
begin
  FreeAndNil(FLayout);
  inherited;
end;

procedure TTextSettingsBase.DoChange;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
  FIsSizeChange := False;
  FIsColorChange := False;
end;

procedure TTextSettingsBase.DoColorChanged(Sender: TObject);
begin
  FIsColorChange := True;
  DoChange;
end;

procedure TTextSettingsBase.DoFontChanged(Sender: TObject);
begin
  if Assigned(FOnLastFontChanged) then
    FOnLastFontChanged(Sender);
  FIsSizeChange := True;
  DoChange;
end;

procedure TTextSettingsBase.DoTextChanged;
begin
  if FAutoSize then FIsSizeChange := True;
  FIsEffectsChange := True;
  try
    DoChange;
  finally
    FIsEffectsChange := False;
  end;
end;

procedure TTextSettingsBase.Draw(const Canvas: TCanvas; const AText: string;
  const R: TRectF; const Opacity: Single; State: TViewState);
begin
  if AText <> '' then
    Draw(Canvas, AText, R, Opacity, State, FGravity);
end;

procedure TTextSettingsBase.Draw(const Canvas: TCanvas; const AText: string;
  const R: TRectF; const Opacity: Single; State: TViewState; AGravity: TLayoutGravity);
var
  V, H: TTextAlign;
begin
  if AText <> '' then begin
    V := TTextAlign.Leading;
    H := TTextAlign.Leading;
    case AGravity of
      TLayoutGravity.LeftBottom: V := TTextAlign.Trailing;
      TLayoutGravity.RightTop: H := TTextAlign.Trailing;
      TLayoutGravity.RightBottom:
        begin
          V := TTextAlign.Trailing;
          H := TTextAlign.Trailing;
        end;
      TLayoutGravity.CenterVertical: V := TTextAlign.Center;
      TLayoutGravity.CenterHorizontal: H := TTextAlign.Center;
      TLayoutGravity.CenterHBottom:
        begin
          H := TTextAlign.Center;
          V := TTextAlign.Trailing;
        end;
      TLayoutGravity.CenterVRight:
        begin
          H := TTextAlign.Trailing;
          V := TTextAlign.Center;
        end;
      TLayoutGravity.Center:
        begin
          H := TTextAlign.Center;
          V := TTextAlign.Center;
        end;
    end;
    FillText(Canvas, R, AText, Opacity, FillTextFlags, H, V, State);
  end;
end;

procedure TTextSettingsBase.Draw(const Canvas: TCanvas; const R: TRectF;
  const Opacity: Single; State: TViewState);
begin
  if FPrefixStyle = TPrefixStyle.HidePrefix then
    Draw(Canvas, DelAmp(FText), R, Opacity, State, FGravity)
  else
    Draw(Canvas, FText, R, Opacity, State, FGravity);
end;

procedure TTextSettingsBase.FillText(const Canvas: TCanvas; const ARect: TRectF;
  const AText: string; const AOpacity: Single; const Flags: TFillTextFlags;
  const ATextAlign, AVTextAlign: TTextAlign; State: TViewState);
begin
  with FLayout do begin
    BeginUpdate;
    TopLeft := ARect.TopLeft;
    MaxSize := PointF(ARect.Width, ARect.Height);
    Text := AText;
    WordWrap := Self.WordWrap;
    Opacity := AOpacity;
    HorizontalAlign := ATextAlign;
    VerticalAlign := AVTextAlign;
    Color := GetStateColor(State);
    Trimming := FTrimming;
    RightToLeft := TFillTextFlag.RightToLeft in Flags;
    EndUpdate;
    RenderLayout(Canvas);
  end;
end;

procedure TTextSettingsBase.FillText(const Canvas: TCanvas; const ARect: TRectF;
  const AText: string; const AOpacity: Single; const AColor: TAlphaColor;
  const Flags: TFillTextFlags; ASize: PSizeF; const SceneScale: Single;
  const ATextAlign, AVTextAlign: TTextAlign; State: TViewState);
begin
  with FLayout do begin
    BeginUpdate;
    TopLeft := ARect.TopLeft;
    MaxSize := PointF(ARect.Width, ARect.Height);
    Text := AText;
    WordWrap := Self.WordWrap;
    Opacity := AOpacity;
    HorizontalAlign := ATextAlign;
    VerticalAlign := AVTextAlign;
    Color := AColor;
    Trimming := FTrimming;
    RightToLeft := TFillTextFlag.RightToLeft in Flags;
    EndUpdate;
    if Assigned(ASize) then begin
      ASize.Width := RoundToScale(Width, SceneScale);
      ASize.Height := RoundToScale(Height, SceneScale);
    end;
    RenderLayout(Canvas);
  end;
end;

function TTextSettingsBase.GetFillTextFlags: TFillTextFlags;
begin
  if Assigned(FOwner) then
    Result := TView(FOwner).FillTextFlags
  else Result := [];
end;

function TTextSettingsBase.GetFont: TFont;
begin
  Result := FLayout.Font;
end;

function TTextSettingsBase.GetGravity: TLayoutGravity;
begin
  Result := FGravity;
end;

function TTextSettingsBase.GetHorzAlign: TTextAlign;
begin
  case FGravity of
    TLayoutGravity.None,
    TLayoutGravity.LeftTop, TLayoutGravity.LeftBottom, TLayoutGravity.CenterVertical:
      Result := TTextAlign.Leading;
    TLayoutGravity.CenterHorizontal, TLayoutGravity.CenterHBottom, TLayoutGravity.Center:
      Result := TTextAlign.Center;
  else
    Result := TTextAlign.Trailing;
  end;
end;

function TTextSettingsBase.GetTextLength: Integer;
begin
  Result := Length(FText);
end;

function TTextSettingsBase.GetVertAlign: TTextAlign;
begin
  case FGravity of
    TLayoutGravity.None,
    TLayoutGravity.LeftTop, TLayoutGravity.CenterHorizontal, TLayoutGravity.RightTop:
      Result := TTextAlign.Leading;
    TLayoutGravity.CenterVertical, TLayoutGravity.Center, TLayoutGravity.CenterVRight:
      Result := TTextAlign.Center;
  else
    Result := TTextAlign.Trailing;
  end;
end;

function TTextSettingsBase.GetWordWrap: Boolean;
begin
  Result := FLayout.WordWrap;
end;

function TTextSettingsBase.IsStoredGravity: Boolean;
begin
  Result := FGravity <> TLayoutGravity.None;
end;

procedure TTextSettingsBase.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then begin
    FAutoSize := Value;
    if ([csLoading, csDesigning] * FOwner.ComponentState = [csDesigning]) and FAutoSize then
      FLayout.WordWrap := False;
    if FAutoSize then FIsSizeChange := True;
    DoChange;
  end;
end;

procedure TTextSettingsBase.SetFont(const Value: TFont);
begin
  if (FLayout.Font = nil) or (Value = nil) then Exit;
  FLayout.Font := Value;
end;

procedure TTextSettingsBase.SetGravity(const Value: TLayoutGravity);
begin
  if FGravity <> Value then begin
    FGravity := Value;
    DoChange;
  end;
end;

procedure TTextSettingsBase.SetHorzAlign(const Value: TTextAlign);
begin
  SetHorzVertValue(Value, VertAlign);
end;

procedure TTextSettingsBase.SetHorzVertValue(const H, V: TTextAlign);
begin
  case H of
    TTextAlign.Leading:
      begin
        case V of
          TTextAlign.Center: FGravity := TLayoutGravity.CenterHorizontal;
          TTextAlign.Leading: FGravity := TLayoutGravity.LeftTop;
          TTextAlign.Trailing: FGravity := TLayoutGravity.LeftBottom;
        end;
      end;
    TTextAlign.Center:
      begin
        case V of
          TTextAlign.Center: FGravity := TLayoutGravity.Center;
          TTextAlign.Leading: FGravity := TLayoutGravity.CenterVertical;
          TTextAlign.Trailing: FGravity := TLayoutGravity.CenterHBottom;
        end;
      end;
    TTextAlign.Trailing:
      begin
        case V of
          TTextAlign.Center: FGravity := TLayoutGravity.CenterVRight;
          TTextAlign.Leading: FGravity := TLayoutGravity.RightTop;
          TTextAlign.Trailing: FGravity := TLayoutGravity.RightBottom;
        end;
      end;
  end;
end;

procedure TTextSettingsBase.SetPrefixStyle(const Value: TPrefixStyle);
begin
  if FPrefixStyle <> Value then begin
    FPrefixStyle := Value;
    if FAutoSize then FIsSizeChange := True;
    DoChange;
  end;
end;

procedure TTextSettingsBase.SetText(const Value: string);
begin
  if FText <> Value then begin
    FText := Value;
    FIsTextChange := True;
    if FAutoSize then FIsSizeChange := True;
    DoTextChanged;
  end;
end;

procedure TTextSettingsBase.SetTrimming(const Value: TTextTrimming);
begin
  if FTrimming <> Value then begin
    FTrimming := Value;
    if FAutoSize then FIsSizeChange := True;
    DoChange;
  end;
end;

procedure TTextSettingsBase.SetVertAlign(const Value: TTextAlign);
begin
  SetHorzVertValue(HorzAlign, Value);
end;

procedure TTextSettingsBase.SetWordWrap(const Value: Boolean);
begin
  if FLayout.WordWrap <> Value then begin
    FLayout.WordWrap := Value;
    if FAutoSize then FIsSizeChange := True;
    DoChange;
  end;
end;

procedure TTextSettingsBase.TextSize(const AText: string; var ASize: TSizeF;
  const SceneScale: Single; const MaxWidth: Single; AWordWrap: Boolean);
begin
  with FLayout do begin
    BeginUpdate;
    TopLeft := TPointF.Zero;
    if MaxWidth < 0 then
      MaxSize := TTextLayout.MaxLayoutSize
    else
      MaxSize := PointF(MaxWidth, $FFFFFF);
    Text := AText;
    WordWrap := AWordWrap;
    HorizontalAlign := TTextAlign.Leading;
    VerticalAlign := TTextAlign.Leading;
    RightToLeft := False;
    EndUpdate;
    ASize.Width := RoundToScale(Width, SceneScale);
    ASize.Height := RoundToScale(Height, SceneScale);
  end;
end;

{ TTextSettings }

procedure TTextSettings.Assign(Source: TPersistent);
begin
  if Source is TTextSettings then begin
    Self.FColor.Assign(TTextSettings(Source).FColor);
    Self.FOpacity := TTextSettings(Source).FOpacity;
  end;
  inherited Assign(Source);
end;

constructor TTextSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := TTextColor.Create();
  FColor.OnChanged := DoColorChanged;
  FOpacity := 1;
end;

destructor TTextSettings.Destroy;
begin
  FreeAndNil(FColor);
  inherited Destroy;
end;

function TTextSettings.GetStateColor(const State: TViewState): TAlphaColor;
begin
  Result := FColor.GetStateColor(State);
  if FOpacity < 1 then
    TColorRec(Result).A := Round(TColorRec(Result).A * FOpacity);
end;

function TTextSettings.IsStoreOpacity: Boolean;
begin
  Result := FOpacity < 1;
end;

procedure TTextSettings.SetColor(const Value: TViewColor);
begin
  FColor.Assign(Value);
end;

procedure TTextSettings.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then begin
    FOpacity := Value;
    DoColorChanged(Self);
  end;
end;

{ TViewImageLink }

procedure TViewImageLink.Change;
begin
  if Assigned(OnChange) then
    OnChange(Images);
end;

constructor TViewImageLink.Create(AOwner: TDrawableIcon);
var
  LGlyph: IGlyph;

  procedure DoCreate();
  var
    FContext: TRttiContext;
    FType: TRttiType;
    FFiled: TRttiField;
    V: TValue;
  begin
    // 使用 RTTi 设置其它属性
    FContext := TRttiContext.Create;
    try
      FType := FContext.GetType(TGlyphImageLink);
      FFiled := FType.GetField('FOwner');
      if FFiled <> nil then begin
        V := AOwner.GetComponent;
        FFiled.SetValue(Self, V);
      end;
      FFiled := FType.GetField('FGlyph');
      if FFiled <> nil then begin
        V := V.From(LGlyph);
        FFiled.SetValue(Self, V);
      end;
    finally
      FContext.Free;
    end;
  end;

begin
  if AOwner = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  if not AOwner.GetInterface(IGlyph, LGlyph) then
    raise EArgumentException.CreateFMT(SUnsupportedInterface, [AOwner.ClassName, 'IGlyph']);
  ImageIndex := -1;
  // DoCreate();
end;

{ TDrawableBorder }

procedure TDrawableBorder.Assign(Source: TPersistent);
begin
  if Source is TDrawableBorder then begin
    if TDrawableBorder(Source).FBorder = nil then
      FreeAndNil(FBorder)
    else begin
      if (FBorder = nil) then
        CreateBorder();
      FBorder.Assign(TDrawableBorder(Source).FBorder);
    end;
  end;
  inherited Assign(Source);
end;

constructor TDrawableBorder.Create(View: IView; const ADefaultKind: TViewBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  inherited Create(View, ADefaultKind, ADefaultColor);
end;

procedure TDrawableBorder.CreateBorder;
begin
  if FBorder = nil then begin
    FBorder := TViewBorder.Create;
    FBorder.OnChanged := DoChange;
  end;
end;

destructor TDrawableBorder.Destroy;
begin
  FreeAndNil(FBorder);
  inherited Destroy;
end;

procedure TDrawableBorder.DoDrawed(Canvas: TCanvas; var R: TRectF; AState: TViewState; const AOpacity: Single);
var
  TH: Single;
  LRect: TRectF;
begin
  if Assigned(FBorder) and (FBorder.FStyle <> TViewBorderStyle.None) and (FBorder.Width > 0) then begin
    if FBorder.Kind = TBrushKind.Solid then
      FBorder.Brush.Color :=  FBorder.Color.GetStateColor(AState);
    case FBorder.FStyle of
      TViewBorderStyle.RectBorder:
        begin
          if FBorder.Width > 0.1 then begin
            TH := FBorder.Width / 1.95;
            LRect.Left := R.Left + TH;
            LRect.Top := R.Top + TH;
            LRect.Right := R.Right - TH;
            LRect.Bottom := R.Bottom - TH;
            Canvas.DrawRect(LRect, XRadius, YRadius, FCorners, AOpacity, FBorder.Brush, FCornerType);
          end else
            Canvas.DrawRect(R, XRadius, YRadius, FCorners, AOpacity, FBorder.Brush, FCornerType);
        end;
      TViewBorderStyle.RectBitmap:
        begin
          Canvas.FillRect(R, XRadius, YRadius, FCorners, AOpacity, FBorder.Brush, FCornerType);
        end;
      TViewBorderStyle.CircleBorder:
        begin
          if FBorder.Width > 0.1 then begin
            TH := FBorder.Width / 1.95;
            LRect.Left := R.Left + TH;
            LRect.Top := R.Top + TH;
            LRect.Right := R.Right - TH;
            LRect.Bottom := R.Bottom - TH;
            TH := Min(LRect.Width, LRect.Height) * 0.5;
            Canvas.DrawArc(
              PointF(LRect.Left + LRect.Width * 0.5, LRect.Top + LRect.Height * 0.5),
              PointF(TH, TH), 0, 360, AOpacity, FBorder.Brush);
          end else begin
            TH := Min(R.Width, R.Height) * 0.5;
            Canvas.DrawArc(PointF(R.Left + R.Width * 0.5, R.Top + R.Height * 0.5),
              PointF(TH, TH), 0, 360, AOpacity, FBorder.Brush);
          end;
        end;
      TViewBorderStyle.EllipseBorder:
        begin
          if FBorder.Width > 0.1 then begin
            TH := FBorder.Width / 1.95;
            LRect.Left := R.Left + TH;
            LRect.Top := R.Top + TH;
            LRect.Right := R.Right - TH;
            LRect.Bottom := R.Bottom - TH;
            Canvas.DrawEllipse(LRect, AOpacity, FBorder.Brush);
          end else
            Canvas.DrawEllipse(R, AOpacity, FBorder.Brush);
        end;
      TViewBorderStyle.LineEdit:
        begin
          Canvas.FillRect(RectF(R.Left, R.Bottom - FBorder.Width, R.Right, R.Bottom),
            0, 0, FCorners, AOpacity, FBorder.Brush, FCornerType);
          TH := Min(6, Min(FBorder.Width * 4, R.Height / 4));
          Canvas.DrawLine(PointF(R.Left, R.Bottom - TH), PointF(R.Left, R.Bottom), AOpacity, FBorder.Brush);
          Canvas.DrawLine(PointF(R.Right, R.Bottom - TH), R.BottomRight, AOpacity, FBorder.Brush);
        end;
      TViewBorderStyle.LineTop:
        begin
          Canvas.FillRect(AlignToPixel(Canvas, RectF(R.Left, R.Top, R.Right, R.Top + FBorder.Width)),
            XRadius, YRadius, FCorners, AOpacity, FBorder.Brush, FCornerType);
        end;
      TViewBorderStyle.LineBottom:
        begin
          Canvas.FillRect(RectF(R.Left, R.Bottom - FBorder.Width, R.Right, R.Bottom),
            XRadius, YRadius, FCorners, AOpacity, FBorder.Brush, FCornerType);
        end;
      TViewBorderStyle.LineLeft:
        begin
          Canvas.FillRect(RectF(R.Left, R.Top, R.Left + FBorder.Width, R.Bottom),
            XRadius, YRadius, FCorners, AOpacity, FBorder.Brush, FCornerType);
        end;
      TViewBorderStyle.LineRight:
        begin
          Canvas.FillRect(RectF(R.Right - FBorder.Width, R.Top, R.Right, R.Bottom),
            XRadius, YRadius, FCorners, AOpacity, FBorder.Brush, FCornerType);
        end;
    end;
  end;
end;

procedure TDrawableBorder.DrawBorder(Canvas: TCanvas; var R: TRectF;
  AState: TViewState);
begin
  DoDrawed(Canvas, R, AState, FView.Opacity);
end;

function TDrawableBorder.GetBorder: TViewBorder;
begin
  if FBorder = nil then
    CreateBorder;
  Result := FBorder;
end;

function TDrawableBorder.GetEmpty: Boolean;
begin
  if Assigned(FBorder) and (FBorder.FStyle <> TViewBorderStyle.None) then
    Result := False
  else
    Result := inherited GetEmpty;
end;

procedure TDrawableBorder.SetBorder(const Value: TViewBorder);
begin
  FBorder.Assign(Value);
end;

{ TViewBorder }

procedure TViewBorder.Assign(Source: TPersistent);
var
  SaveChange: TNotifyEvent;
begin
  if Source is TViewBorder then begin
    SaveChange := FOnChanged;
    FOnChanged := nil;
    FColor.OnChanged := nil;
    FColor.Assign(TViewBorder(Source).FColor);
    FStyle := TViewBorder(Source).FStyle;
    FBrush.OnChanged := nil;
    FBrush.Assign(TViewBorder(Source).FBrush);
    FBrush.OnChanged := DoGradientChanged;
    FOnChanged := SaveChange;
    FColor.OnChanged := SaveChange;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end else
    inherited;
end;

constructor TViewBorder.Create(ADefaultStyle: TViewBorderStyle);
begin
  FBrush := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Null);
  FBrush.OnChanged := DoGradientChanged;
  FColor := TViewColor.Create(TAlphaColorRec.Null);
  FStyle := ADefaultStyle;
  FDefaultStyle := ADefaultStyle;
end;

destructor TViewBorder.Destroy;
begin
  FColor.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TViewBorder.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TViewBorder.DoGradientChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TViewBorder.GetBitmap: TBrushBitmap;
begin
  Result := FBrush.Bitmap;
end;

function TViewBorder.GetCap: TStrokeCap;
begin
  Result := FBrush.Cap;
end;

function TViewBorder.GetDash: TStrokeDash;
begin
  Result := FBrush.Dash;
end;

function TViewBorder.GetGradient: TGradient;
begin
  Result := FBrush.Gradient;
end;

function TViewBorder.GetJoin: TStrokeJoin;
begin
  Result := FBrush.Join;
end;

function TViewBorder.GetKind: TBrushKind;
begin
  Result := FBrush.Kind;
end;

function TViewBorder.GetWidth: Single;
begin
  Result := FBrush.Thickness;
end;

function TViewBorder.IsBitmapStored: Boolean;
begin
  Result := (FBrush.Kind = TBrushKind.Bitmap);
end;

function TViewBorder.IsGradientStored: Boolean;
begin
  Result := (FBrush.Kind = TBrushKind.Gradient);
end;

procedure TViewBorder.SetBitmap(const Value: TBrushBitmap);
begin
  if FBrush.Bitmap <> Value then begin
    FBrush.Bitmap := Value;
    DoChanged;
  end;
end;

procedure TViewBorder.SetCap(const Value: TStrokeCap);
begin
  if FBrush.Cap <> Value then begin
    FBrush.Cap := Value;
    DoChanged;
  end;
end;

procedure TViewBorder.SetColor(const Value: TViewColor);
begin
  FColor.Assign(Value);
end;

procedure TViewBorder.SetDash(const Value: TStrokeDash);
begin
  if FBrush.Dash <> Value then begin
    FBrush.Dash := Value;
    DoChanged;
  end;
end;

procedure TViewBorder.SetGradient(const Value: TGradient);
begin
  if FBrush.Gradient <> Value then
    FBrush.Gradient := Value;
end;

procedure TViewBorder.SetJoin(const Value: TStrokeJoin);
begin
  if FBrush.Join <> Value then begin
    FBrush.Join := Value;
    DoChanged;
  end;
end;

procedure TViewBorder.SetKind(const Value: TBrushKind);
begin
  if FBrush.Kind <> Value then begin
    FBrush.Kind := Value;
    DoChanged;
  end;
end;

procedure TViewBorder.SetOnChanged(const Value: TNotifyEvent);
begin
  FOnChanged := Value;
  FColor.OnChanged := FOnChanged;
end;

procedure TViewBorder.SetStyle(const Value: TViewBorderStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    DoChanged;
  end;
end;

procedure TViewBorder.SetWidth(const Value: Single);
begin
  if Value <> FBrush.Thickness then begin
    FBrush.Thickness := Value;
    DoChanged;
  end;
end;

function TViewBorder.StyleStored: Boolean;
begin
  Result := FStyle <> FDefaultStyle;
end;

function TViewBorder.WidthStored: Boolean;
begin
  Result := Width <> 1;
end;

{ TViewBrushBase }

procedure TViewBrushBase.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TViewBrushBase then begin
    if TViewBrushBase(Source).FAccessory = nil then begin
      if FAccessory <> nil then begin
        FreeAndNil(FAccessory);
        DoAccessoryChange(Self);
      end;
    end else
      Accessory.Assign(TViewBrushBase(Source).FAccessory);
  end;
end;

procedure TViewBrushBase.ChangeToSolidColor(const AColor: TAlphaColor;
  IsDefault: Boolean);
begin
  Color := AColor;
  DefaultColor := AColor;
  Kind := TViewBrushKind.Solid;
  DefaultKind := TBrushKind.Solid;
end;

destructor TViewBrushBase.Destroy;
begin
  if FAccessory <> nil then
    FreeAndNil(FAccessory);
  FreeAndNil(FSvgImage);
  inherited;
end;

procedure TViewBrushBase.DoAccessoryChange(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TViewBrushBase.DoSvgImageChange(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

function TViewBrushBase.GetAccessory: TViewAccessory;
begin
  if not Assigned(FAccessory) then begin
    FAccessory := TViewAccessory.Create;
    FAccessory.OnChanged := DoAccessoryChange;
  end;
  Result := FAccessory;
end;

function TViewBrushBase.GetKind: TViewBrushKind;
begin
  Result := TViewBrushKind(inherited Kind);
end;

function TViewBrushBase.GetSvgImage: TSVGImage;
begin
  if FSvgImage = nil then begin
    FSvgImage := TSVGImage.Create;
    FSvgImage.OnChange := DoSvgImageChange;
  end;
  Result := FSvgImage;
end;

function TViewBrushBase.IsKindStored: Boolean;
begin
  Result := inherited Kind <> DefaultKind;
end;

procedure TViewBrushBase.SetAccessory(const Value: TViewAccessory);
begin
  if Value = nil then begin
    FreeAndNil(FAccessory);
    DoAccessoryChange(Self);
  end else
    Accessory.Assign(Value);
end;

procedure TViewBrushBase.SetKind(const Value: TViewBrushKind);
begin
  inherited Kind := TBrushKind(Value);
end;

procedure TViewBrushBase.SetSvgImage(const Value: TSVGImage);
begin
  if Value = nil then
    FreeAndNil(FSvgImage)
  else begin
    if FSvgImage = nil then begin
      FSvgImage := TSVGImage.Create;
      FSvgImage.OnChange := DoSvgImageChange;
    end;
    FSvgImage.Assign(Value);
  end;
end;

{ TViewBrush }

procedure TViewBrush.Assign(Source: TPersistent);
begin
  if Source is TViewBrush then
    Self.Bitmap := TViewBrush(Source).Bitmap;
  inherited;
end;

constructor TViewBrush.Create(const ADefaultKind: TViewBrushKind;
  const ADefaultColor: TAlphaColor);
var
  Bmp: TBrushBitmap;
begin
  inherited Create(TBrushKind(ADefaultKind), ADefaultColor);
  Bmp := inherited Bitmap;
  Bmp.Free;
  inherited Bitmap := nil;
  Bmp := TPatch9Bitmap.Create;
  Bmp.OnChanged := Self.BitmapChanged;
  Bmp.Bitmap.OnChange := Self.BitmapChanged;
  TPatch9Bitmap(Bmp).FBounds.OnChange := Self.BitmapChanged;
  inherited Bitmap := Bmp;
end;

function TViewBrush.GetBitmap: TPatch9Bitmap;
begin
  Result := TPatch9Bitmap(inherited Bitmap);
end;

function TViewBrush.IsPatch9BitmapStored: Boolean;
begin
  Result := Kind in [TViewBrushKind.Bitmap, TViewBrushKind.Patch9Bitmap];
end;

procedure TViewBrush.SetBitmap(const Value: TPatch9Bitmap);
begin
  inherited Bitmap.Assign(Value);
end;

{ TPatch9Bitmap }

procedure TPatch9Bitmap.Assign(Source: TPersistent);
begin
  if Source is TPatch9Bitmap then begin
    FRemoveBlackLine := TPatch9Bitmap(Source).FRemoveBlackLine;
    FBounds.Assign(TPatch9Bitmap(Source).FBounds);
  end;
  inherited;
end;

constructor TPatch9Bitmap.Create;
begin
  inherited Create;
  FBounds := TPatchBounds.Create(RectF(0, 0, 0, 0));
  FBounds.OnChange := Bitmap.OnChange;
end;

destructor TPatch9Bitmap.Destroy;
begin
  FBounds.Free;
  inherited;
end;

procedure TPatch9Bitmap.SetBounds(const Value: TPatchBounds);
begin
  FBounds.Assign(Value);
end;

procedure TPatch9Bitmap.SetRemoveBlackLine(const Value: Boolean);
begin
  if FRemoveBlackLine <> Value then begin
    FRemoveBlackLine := Value;
    if Assigned(OnChanged) then
      OnChanged(Self);
  end;
end;

{ TViewAccessoryImageList }

procedure TViewAccessoryImageList.AddAddAccessory;
begin
  AddPath('M1024 472.436364H549.236364V0h-76.8v472.436364H0v76.8h472.436364V1024h76.8V549.236364H1024z', 1024, 1024);
end;

procedure TViewAccessoryImageList.AddBackAccessory;
begin
  AddPath('M360.44 511.971l442.598-422.3c21.503-20.53 21.503-53.782 0-74.286-21.477-20.505-56.316-20.505-77.794 '+
    '0L282.645 437.71 204.8 511.97l77.82 74.262 442.624 422.3c21.503 20.555 56.317 20.555 77.82 0 21.477-20.48 '+
    '21.477-53.758 0-74.237L360.439 511.971z', 1024, 1024);
end;

{
procedure TViewAccessoryImageList.AddEllipsesAccessory;
var
  AAcc: TBitmap;
  ARect: TRectF;
  ASpacing: single;
  ASize: single;
begin
  AAcc := TBitmap.Create;
  AAcc.SetSize(Round(32 * GetScreenScale), Round(32 * GetScreenScale));
  ASize := 7 * GetScreenScale;
  ASpacing := (AAcc.Width - (3 * ASize)) / 2;

  AAcc.Clear(claNull);
  AAcc.Canvas.BeginScene;
  try
    AAcc.Canvas.Fill.Color := claSilver;
    ARect := RectF(0, 0, ASize, ASize);
    OffsetRect(ARect, 0, (AAcc.Height - ARect.Height) / 2);
    AAcc.Canvas.FillEllipse(ARect, 1);
    OffsetRect(ARect, ASize+ASpacing, 0);
    AAcc.Canvas.FillEllipse(ARect, 1);
    OffsetRect(ARect, ASize+ASpacing, 0);
    AAcc.Canvas.FillEllipse(ARect, 1);
  finally
    AAcc.Canvas.EndScene;
  end;
  Add(AAcc);
end;
}

{
procedure TViewAccessoryImageList.AddFlagAccessory;
var
  AAcc: TBitmap;
  ARect: TRectF;
  s: single;
  r1, r2: TRectF;
begin
  s := GetScreenScale;
  AAcc := TBitmap.Create;
  AAcc.SetSize(Round(32 * s), Round(32 * s));
  AAcc.Clear(claNull);
  ARect := RectF(0, 0, AAcc.Width, AAcc.Height);
  ARect.Inflate(0-(AAcc.Width / 4), 0-(AAcc.Height / 7));


  AAcc.Canvas.BeginScene;
  try
    r1 := ARect;
    r2 := ARect;

    r2.Top := ARect.Top + (ARect.Height / 12);


    r2.Left := r2.Left;
    r2.Height := ARect.Height / 2;
    AAcc.Canvas.stroke.Color := claSilver;
    AAcc.Canvas.Stroke.Thickness := s*2;
    AAcc.Canvas.Fill.Color := claSilver;
    AAcc.Canvas.FillRect(r2, 0, 0, AllCorners, 1);
    AAcc.Canvas.DrawLine(r1.TopLeft, PointF(r1.Left, r1.Bottom), 1);
  finally
    AAcc.Canvas.EndScene;
  end;
  Add(AAcc);
end;
}

procedure TViewAccessoryImageList.AddPath(const PathData: string;const SW, SH: Single);

  procedure ParserPathSize(Path: TPathData; var W, H: Single);
  var
     I: Integer;
  begin
    W := 0;
    H := 0;
    for I := 0 to Path.Count - 1 do begin
      with Path.Points[I] do begin
        if Kind <> TPathPointKind.Close then begin
          W := Max(Point.X, W);
          H := Max(Point.Y, H);
        end;
      end;
    end;
  end;

const
  SWH = 64;
var
  AAcc: TBitmap;
  Path: TPathData;
  W, H, SX: Single;
begin
  AAcc := TBitmap.Create;
  AAcc.SetSize(SWH, SWH);
  Path := TPathData.Create;
  try
    Path.Data := PathData;
    if (SW = 0) and (SH = 0) then
      ParserPathSize(Path, W, H)
    else begin
      W := SW;
      H := SH;
    end;
    if W <= 1 then W := SWH;
    if H <= 1 then H := SWH;
    SX := SWH / W;
    if SWH / H < SX then
      SX := SWH / H;
    if (SX <> 1) then
      Path.Scale(SX, SX);
    AAcc.Canvas.BeginScene;
    try
      AAcc.Clear(claNull);
      AAcc.Canvas.Fill.Color := claBlack;
      AAcc.Canvas.FillPath(Path, 1);
    finally
      AAcc.Canvas.EndScene;
    end;
  finally
    FreeAndNil(Path);
  end;
  Add(AAcc);
end;

procedure TViewAccessoryImageList.AddRefreshAccessory;
begin
  AddPath('M817.093 597.188c-7.486 157.805-137.796 283.457-297.455 283.457-164.47 '+
    '0-297.805-133.336-297.805-297.805 0-159.643 125.608-289.923 283.375-297.452'+
    'v141.624l311.986-170.174L505.208 58.3v141.862c-204.746 7.63-368.38 176.022-368.38 '+
    '382.622 0 211.472 171.421 382.893 382.891 382.893 206.65 0 375.053-163.692 382.627-368.49h-85.253z', 1024, 1024);
end;

procedure TViewAccessoryImageList.CalculateImageScale;
begin
  if FImageScale = 0 then
  begin
    FImageScale := Min(Trunc(GetScreenScale), 3);
    {$IFDEF MSWINDOWS}
    FImageScale := 1;
    {$ENDIF}
  end;
end;

constructor TViewAccessoryImageList.Create;
begin
  inherited Create(True);
  FImageScale := 0;
  FImageMap := TBitmap.Create;
end;

destructor TViewAccessoryImageList.Destroy;
begin
  FreeAndNil(FImageMap);
  if FActiveStyle <> nil then
    FreeAndNil(FActiveStyle);
  inherited;
end;

procedure TViewAccessoryImageList.Draw(ACanvas: TCanvas; const ARect: TRectF;
  AAccessory: TViewAccessoryType; const AOpacity: Single; const AStretch: Boolean);
var
  R: TRectF;
  Bmp: TBitmap;
begin
  Bmp := GetAccessoryImage(AAccessory);
  if not Assigned(Bmp) then
    Exit;
  if AStretch = False then begin
    R := RectF(0, 0, Bmp.Width / GetScreenScale, Bmp.Height / GetScreenScale);
    OffsetRect(R, ARect.Left, ARect.Top);
    OffsetRect(R, (ARect.Width - R.Width) * 0.5, (ARect.Height - R.Height) * 0.5);
    ACanvas.DrawBitmap(Bmp, RectF(0, 0, Bmp.Width, Bmp.Height), R, AOpacity, True);
  end else
    ACanvas.DrawBitmap(Bmp, RectF(0, 0, Bmp.Width, Bmp.Height), ARect, AOpacity, True);
end;

function TViewAccessoryImageList.GetAccessoryFromResource(const AStyleName: string;
  const AState: string): TBitmap;
var
  AStyleObj: TStyleObject;
  AImgRect: TBounds;
  AIds: TStrings;
  ABitmapLink: TBitmapLinks;
  AImageMap: TBitmap;
begin
  CalculateImageScale;

  Result := TBitmap.Create;
  AIds := TStringList.Create;
  try
    AIds.Text := StringReplace(AStyleName, '.', #13, [rfReplaceAll]);
    AStyleObj := TStyleObject(TStyleManager.ActiveStyle(nil));

    while AIds.Count > 0 do begin
      AStyleObj := TStyleObject(AStyleObj.FindStyleResource(AIds[0]));
      AIds.Delete(0);
    end;

    if AStyleObj <> nil then begin
      if FImageMap.IsEmpty then begin
        AImageMap := ((AStyleObj as TStyleObject).Source.MultiResBitmap.Bitmaps[FImageScale]);

        FImageMap.SetSize(Round(AImageMap.Width), Round(AImageMap.Height));
        FImageMap.Clear(claNull);

        FImageMap.Canvas.BeginScene;
        try
          FImageMap.Canvas.DrawBitmap(AImageMap,
              RectF(0, 0, AImageMap.Width, AImageMap.Height),
              RectF(0, 0, FImageMap.Width, FImageMap.Height),
              1,
              True);
        finally
          FImageMap.Canvas.EndScene;
        end;
      end;

      ABitmapLink := nil;
      if AStyleObj = nil then
        Exit;
      if (AStyleObj.ClassType = TCheckStyleObject) then begin
        if AState = 'checked' then
          ABitmapLink := TCheckStyleObject(AStyleObj).ActiveLink
        else
          ABitmapLink := TCheckStyleObject(AStyleObj).SourceLink
      end;

      if ABitmapLink = nil then
        ABitmapLink := AStyleObj.SourceLink;

      {$IFDEF XE8_OR_NEWER}
      AImgRect := ABitmapLink.LinkByScale(FImageScale, True).SourceRect;
      {$ELSE}
      AImgRect := ABitmapLink.LinkByScale(FImageScale).SourceRect;
      {$ENDIF}
      Result.SetSize(Round(AImgRect.Width), Round(AImgRect.Height));
      Result.Clear(claNull);
      Result.Canvas.BeginScene;

      Result.Canvas.DrawBitmap(FImageMap, AImgRect.Rect, RectF(0, 0, Result.Width,
        Result.Height), 1, True);
      Result.Canvas.EndScene;
    end;
  finally
    {$IFDEF NEXTGEN}
    FreeAndNil(AIds);
    {$ELSE}
    AIds.Free;
    {$ENDIF}
  end;
end;

function TViewAccessoryImageList.GetAccessoryImage(
  AAccessory: TViewAccessoryType): TBitmap;
begin
  if Count = 0 then
    Initialize;
  Result := Items[Ord(AAccessory)];
end;

procedure TViewAccessoryImageList.SetAccessoryImage(
  AAccessory: TViewAccessoryType; const Value: TBitmap);
begin
  if Count = 0 then
    Initialize;
  Items[Ord(AAccessory)].Assign(Value);
end;

procedure TViewAccessoryImageList.Initialize;
var
  ICount: TViewAccessoryType;
begin
  for ICount := Low(TViewAccessoryType) to High(TViewAccessoryType) do
  begin
    case ICount of
      TViewAccessoryType.None: Add(GetAccessoryFromResource('none'));
      TViewAccessoryType.More: Add(LoadFromResource('ICON_More'));
      TViewAccessoryType.Checkmark: Add(LoadFromResource('ICON_Checkmark')); //Add(GetAccessoryFromResource('listviewstyle.accessorycheckmark'));
      TViewAccessoryType.Detail: Add(LoadFromResource('ICON_DETAIL')); // Add(GetAccessoryFromResource('listviewstyle.accessorydetail'));
      TViewAccessoryType.Ellipses: Add(LoadFromResource('ICON_Ellipses')); //AddEllipsesAccessory;
      TViewAccessoryType.Flag: Add(LoadFromResource('icon_Flag')); //AddFlagAccessory;
      TViewAccessoryType.Back: AddBackAccessory;// Add(GetAccessoryFromResource('backtoolbutton.icon'));
      TViewAccessoryType.Refresh: Add(LoadFromResource('ICON_REFRESH')); // Add(GetAccessoryFromResource('refreshtoolbutton.icon'));
      TViewAccessoryType.Action: Add(LoadFromResource('ICON_ACTION')); // Add(GetAccessoryFromResource('actiontoolbutton.icon'));

      TViewAccessoryType.Play: Add(LoadFromResource('ICON_PLAY')); // Add(GetAccessoryFromResource('playtoolbutton.icon'));
      TViewAccessoryType.Rewind: Add(LoadFromResource('ICON_REWIND')); //Add(GetAccessoryFromResource('rewindtoolbutton.icon'));
      TViewAccessoryType.Forwards: Add(LoadFromResource('ICON_FORWARDS')); //Add(GetAccessoryFromResource('forwardtoolbutton.icon'));
      TViewAccessoryType.Pause: Add(LoadFromResource('ICON_PAUSE')); //Add(GetAccessoryFromResource('pausetoolbutton.icon'));
      TViewAccessoryType.Stop: Add(LoadFromResource('ICON_STOP')); // Add(GetAccessoryFromResource('stoptoolbutton.icon'));
      TViewAccessoryType.Prior: Add(LoadFromResource('ICON_PRIOR'));// Add(GetAccessoryFromResource('priortoolbutton.icon'));
      TViewAccessoryType.Next: Add(LoadFromResource('ICON_NEXT'));// Add(GetAccessoryFromResource('nexttoolbutton.icon'));
      TViewAccessoryType.BackWard: Add(LoadFromResource('ICON_NEXT'));//
      TViewAccessoryType.ForwardGo: Add(LoadFromResource('ICON_FORWARD'));//

      TViewAccessoryType.Add: Add(LoadFromResource('ICON_ADD')); // AddAddAccessory; // Add(GetAccessoryFromResource('addtoolbutton.icon'));
      TViewAccessoryType.ArrowUp: Add(LoadFromResource('ICON_ARROWUP')); //Add(GetAccessoryFromResource('arrowuptoolbutton.icon'));
      TViewAccessoryType.ArrowDown: Add(LoadFromResource('ICON_ARROWDOWN'));// Add(GetAccessoryFromResource('arrowdowntoolbutton.icon'));
      TViewAccessoryType.ArrowLeft: Add(LoadFromResource('ICON_ARROWLEFT')); // Add(GetAccessoryFromResource('arrowlefttoolbutton.icon'));
      TViewAccessoryType.ArrowRight: Add(LoadFromResource('ICON_ARROWRIGHT')); //Add(GetAccessoryFromResource('arrowrighttoolbutton.icon'));
      TViewAccessoryType.Reply: Add(LoadFromResource('ICON_REPLY')); // Add(GetAccessoryFromResource('replytoolbutton.icon'));
      TViewAccessoryType.Search: Add(LoadFromResource('ICON_SEARCH'));// Add(GetAccessoryFromResource('searchtoolbutton.icon'));
      TViewAccessoryType.Bookmarks: Add(LoadFromResource('ICON_BOOKMARKS')); // Add(GetAccessoryFromResource('bookmarkstoolbutton.icon'));
      TViewAccessoryType.Trash: Add(LoadFromResource('ICON_TRASH'));// Add(GetAccessoryFromResource('trashtoolbutton.icon'));
      TViewAccessoryType.Organize: Add(LoadFromResource('ICON_ORGANIZE'));// Add(GetAccessoryFromResource('organizetoolbutton.icon'));
      TViewAccessoryType.Camera: Add(LoadFromResource('ICON_CAMERA')); // Add(GetAccessoryFromResource('cameratoolbutton.icon'));
      TViewAccessoryType.Compose: Add(LoadFromResource('ICON_COMPOSE')); // Add(GetAccessoryFromResource('composetoolbutton.icon'));
      TViewAccessoryType.Info: Add(LoadFromResource('ICON_INFO')); // Add(GetAccessoryFromResource('infotoolbutton.icon'));
      TViewAccessoryType.Pagecurl: Add(LoadFromResource('ICON_PAGECURL')); //Add(GetAccessoryFromResource('pagecurltoolbutton.icon'));
      TViewAccessoryType.Details: Add(LoadFromResource('ICON_DETAILS')); //Add(GetAccessoryFromResource('detailstoolbutton.icon'));
      TViewAccessoryType.RadioButton: Add(LoadFromResource('ICON_RADIOBUTTON')); // Add(GetAccessoryFromResource('radiobuttonstyle.background'));
      TViewAccessoryType.RadioButtonChecked: Add(LoadFromResource('ICON_RADIOBUTTONCHECKED')); // Add(GetAccessoryFromResource('radiobuttonstyle.background', 'checked'));
      TViewAccessoryType.CheckBox: Add(LoadFromResource('ICON_CheckBox')); // Add(GetAccessoryFromResource('checkboxstyle.background'));
      TViewAccessoryType.CheckBoxChecked: Add(LoadFromResource('ICON_CheckBoxChecked'));// Add(GetAccessoryFromResource('checkboxstyle.background', 'checked'));
      TViewAccessoryType.User: Add(LoadFromResource('ICON_User'));
      TViewAccessoryType.Password: Add(LoadFromResource('ICON_PWD'));
      TViewAccessoryType.Down: Add(LoadFromResource('ICON_Down'));
      TViewAccessoryType.Exit: Add(LoadFromResource('ICON_Exit'));
      TViewAccessoryType.Finish: Add(LoadFromResource('ICON_Finish'));
      TViewAccessoryType.Calendar: Add(LoadFromResource('ICON_Calendar'));
      TViewAccessoryType.Cross: Add(LoadFromResource('ICON_Cross'));
      TViewAccessoryType.Menu: Add(LoadFromResource('ICON_Menu'));
      TViewAccessoryType.About: Add(LoadFromResource('ICON_About'));

      TViewAccessoryType.Share: Add(LoadFromResource('ICON_Share'));
      TViewAccessoryType.UserMsg: Add(LoadFromResource('ICON_MESSAGE'));
      TViewAccessoryType.Cart: Add(LoadFromResource('ICON_Cart'));
      TViewAccessoryType.Setting: Add(LoadFromResource('ICON_Setting'));
      TViewAccessoryType.Edit: Add(LoadFromResource('ICON_Edit'));
      TViewAccessoryType.Home: Add(LoadFromResource('ICON_Home'));
      TViewAccessoryType.Heart: Add(LoadFromResource('ICON_Heart'));

      TViewAccessoryType.Comment: Add(LoadFromResource('ICON_Comment'));
      TViewAccessoryType.Collection: Add(LoadFromResource('ICON_Collection'));
      TViewAccessoryType.Fabulous: Add(LoadFromResource('ICON_Fabulous'));
      TViewAccessoryType.Image: Add(LoadFromResource('ICON_Image'));
      TViewAccessoryType.Help: Add(LoadFromResource('ICON_Help'));
      TViewAccessoryType.VCode: Add(LoadFromResource('ICON_VCode'));
      TViewAccessoryType.Time: Add(LoadFromResource('ICON_Time'));
      TViewAccessoryType.UserReg: Add(LoadFromResource('ICON_REGISTER'));
      TViewAccessoryType.Scan: Add(LoadFromResource('ICON_Scan'));
      TViewAccessoryType.Circle: Add(LoadFromResource('ICON_Circle'));
      TViewAccessoryType.Location: Add(LoadFromResource('ICON_LOCATION'));

      TViewAccessoryType.UserDefined1: Add(GetAccessoryFromResource('userdefined1'));
      TViewAccessoryType.UserDefined2: Add(GetAccessoryFromResource('userdefined2'));
      TViewAccessoryType.UserDefined3: Add(GetAccessoryFromResource('userdefined3'));
    end;
  end;
end;

function TViewAccessoryImageList.LoadFromResource(
  const AStyleName: string): TBitmap;
var
  AImageStream: TResourceStream;
begin
  AImageStream := nil;
  Result := TBitmap.Create;
  try
    try
      AImageStream := TResourceStream.Create(HInstance, AStyleName, RT_RCDATA);
      Result.LoadFromStream(AImageStream);
    finally
      FreeAndNil(AImageStream);
    end;
  except
  end;
end;

{ TViewImagesBrush }

procedure TViewImagesBrush.Assign(Source: TPersistent);
begin
  if Source is TViewImagesBrush then begin
    FImageIndex := TViewImagesBrush(Source).FImageIndex;
    Self.Images := TViewImagesBrush(Source).Images;
  end;
  inherited;
end;

constructor TViewImagesBrush.Create(const ADefaultKind: TBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  inherited Create(ADefaultKind, ADefaultColor);
  FImageIndex := -1;
end;

function TViewImagesBrush.GetComponent: TComponent;
var
  LI: IInterfaceComponentReference;
begin
  if Assigned(FOwner) and (Supports(FOwner, IInterfaceComponentReference, LI)) then
    Result := LI.GetComponent
  else
    Result := nil;
end;

function TViewImagesBrush.GetImageIndex: TImageIndex;
begin
  Result := FImageIndex;
end;

function TViewImagesBrush.GetImageList: TBaseImageList;
var
  LI: IGlyph;
begin
  if Assigned(FOwner) and (Supports(FOwner, IGlyph, LI)) then
    Result := LI.Images
  else
    Result := nil;
end;

procedure TViewImagesBrush.ImagesChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

function TViewImagesBrush.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK
  else Result := E_NOINTERFACE
end;

procedure TViewImagesBrush.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    if Assigned(OnChanged) then
      OnChanged(Self);
  end;
end;

procedure TViewImagesBrush.SetImageList(const Value: TBaseImageList);
var
  LI: IGlyph;
begin
  if Assigned(FOwner) and (Supports(FOwner, IGlyph, LI)) then
    LI.Images := Value;
end;

procedure TViewImagesBrush.SetImages(const Value: TCustomImageList);
var
  LI: IGlyph;
begin
  if Assigned(FOwner) and (Supports(FOwner, IGlyph, LI)) then
    LI.Images := Value;
end;

function TViewImagesBrush._AddRef: Integer;
begin
  Result := -1;
end;

function TViewImagesBrush._Release: Integer;
begin
  Result := -1;
end;

{ TAniCalculationsEx }

function TAniCalculationsEx.GetDownPoint: TPointD;
begin
  Result := TView.GetRttiValue<TPointD>(Self, 'FDownPoint');
end;

{ TScrollCalculations }

constructor TScrollCalculations.Create(AOwner: TPersistent);
begin
  if not (AOwner is TView) then
    raise EArgumentException.Create('Argument Invalid.');
  inherited Create(AOwner);
  FScrollView := TView(AOwner);
end;

procedure TScrollCalculations.DoChanged;
begin
  if (FScrollView <> nil) and not (csDestroying in FScrollView.ComponentState) then
    FScrollView.InternalAlign;
  inherited;
end;

procedure TScrollCalculations.DoStart;
begin
  inherited;
  if (FScrollView <> nil) and not (csDestroying in FScrollView.ComponentState) then
    FScrollView.StartScrolling;
end;

procedure TScrollCalculations.DoStop;
begin
  inherited;
  if (FScrollView <> nil) and not (csDestroying in FScrollView.ComponentState) then
    FScrollView.StopScrolling;
end;

{ TScrollBarHelper }

function TScrollBarHelper.GetMaxD: Double;
begin
  Result := ValueRange.Max;
end;

function TScrollBarHelper.GetMinD: Double;
begin
  Result := ValueRange.Min;
end;

function TScrollBarHelper.GetValueD: Double;
begin
  Result := ValueRange.Value;
end;

function TScrollBarHelper.GetViewportSizeD: Double;
begin
  Result := ValueRange.ViewportSize;
end;

procedure TScrollBarHelper.SetMaxD(const Value: Double);
begin
  ValueRange.Max := Value;
end;

procedure TScrollBarHelper.SetMinD(const Value: Double);
begin
  ValueRange.Min := Value;
end;

procedure TScrollBarHelper.SetValueD(const Value: Double);
begin
  ValueRange.Value := Value;
end;

procedure TScrollBarHelper.SetViewportSizeD(const Value: Double);
begin
  ValueRange.ViewportSize := Value;
end;

{ TCustomTrackHelper }

function TCustomTrackHelper.GetMaxD: Double;
begin
  Result := ValueRange.Max;
end;

function TCustomTrackHelper.GetMinD: Double;
begin
  Result := ValueRange.Min;
end;

function TCustomTrackHelper.GetValueD: Double;
begin
  Result := ValueRange.Value;
end;

function TCustomTrackHelper.GetViewportSizeD: Double;
begin
  Result := ValueRange.ViewportSize;
end;

procedure TCustomTrackHelper.SetMaxD(const Value: Double);
begin
  ValueRange.Max := Value;
end;

procedure TCustomTrackHelper.SetMinD(const Value: Double);
begin
  ValueRange.Min := Value;
end;

procedure TCustomTrackHelper.SetValueD(const Value: Double);
begin
  ValueRange.Value := Value;
end;

procedure TCustomTrackHelper.SetViewportSizeD(const Value: Double);
begin
  ValueRange.ViewportSize := Value;
end;

{ TGridsLayout }

constructor TGridsLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNumColumns := 0;
  FColumnWidth := CDefaultColumnWidth;
  FColumnHeight := CDefaultColumnHeight;
  FStretchMode := TViewStretchMode.None;
  FSpacingBorder := True;
  FVerticalSpacing := 0;
  FHorizontalSpacing := 0;
  FLastRows := 0;
  FLastColumns := 0;
  FDividerBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Null);
  FDividerBrush.Color := CDefaultDividerColor;
  FForceColumnSize := False;
end;

destructor TGridsLayout.Destroy;
begin
  FreeAndNil(FDividerBrush);
  inherited Destroy;
end;

procedure TGridsLayout.DoRealign;
var
  I, CtrlCount: Integer;
  LColumns: Integer;
  LItemWidth, LItemHeight, AW: Single;
  CurPos: TPointD;
  VL, VT, VW, VH, PW: Double;
  Control: TControl;
  View: IView;
  SaveAdjustViewBounds, LAutoSize: Boolean;
  LStretchMode: TViewStretchMode;
begin
  if FDisableAlign then
    Exit;
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  //LogD(Self.ClassName + '.DoRealign.');

  FDisableAlign := True;

  // 得到子组件的开始坐标
  if FSpacingBorder then begin
    CurPos := TPointD.Create(Padding.Left + FHorizontalSpacing, Padding.Top + FVerticalSpacing);
    VW := Width - CurPos.X - Padding.Right - FHorizontalSpacing;
    VH := Height - CurPos.Y - Padding.Bottom - FVerticalSpacing;
  end else begin
    CurPos := TPointD.Create(Padding.Left, Padding.Top);
    VW := Width - CurPos.X - Padding.Right;
    VH := Height - CurPos.Y - Padding.Bottom;
  end;
  CtrlCount := ControlsCount;
  LColumns := AbsoluteColumnsNum;
  if FColumnWidth < 0 then
    LItemWidth := CDefaultColumnWidth
  else
    LItemWidth := FColumnWidth;

  if (FNumColumns > 0) and (FWidthSize = TViewSize.WrapContent) then begin
    VW := LColumns * (LItemWidth + FHorizontalSpacing) - FHorizontalSpacing;
    if FStretchMode = FStretchMode then
      VW := VW + FHorizontalSpacing * 2;
  end;

  PW := 0;
  {$IF CompilerVersion <> 32}
  VL := 0;
  {$ENDIF}

  // 如果长宽 > 0 和子控件 > 0 时才处理布局
  if ((VW > 0) and (VH > 0)) or (CtrlCount > 0) then begin
    AW := VW + CurPos.X;

    // 根据拉伸模式，计算出每列的宽度、空白大小和实际使用的拉伸模式
    LStretchMode := FStretchMode;
    case FStretchMode of
      TViewStretchMode.None:
        begin
          LStretchMode := TViewStretchMode.None;
        end;
      TViewStretchMode.SpacingWidth:
        begin
          if LColumns > 1 then begin
            LStretchMode := TViewStretchMode.SpacingWidth;
            if csDesigning in ComponentState then begin
              I := GetCount;
              if LColumns > I then
                LColumns := I;
            end else begin
              if LColumns > CtrlCount then
                LColumns := CtrlCount;
            end;
            PW := (VW - LItemWidth) / (LColumns - 1) - LItemWidth;
            if PW < 0 then PW := 0;
            if FSpacingBorder then
              AW := AW - FHorizontalSpacing;
          end else begin
            LStretchMode := TViewStretchMode.None;
          end;
        end;
      TViewStretchMode.ColumnWidth:
        begin
          if LColumns > 0 then begin
            if csDesigning in ComponentState then begin
              I := GetCount;
              if LColumns > I then
                LColumns := I;
            end else begin
              if LColumns > CtrlCount then
                LColumns := CtrlCount;
            end;
            LStretchMode := TViewStretchMode.ColumnWidth;
            LItemWidth := (VW + FHorizontalSpacing) / LColumns - FHorizontalSpacing;
            if FSpacingBorder then
              AW := AW - FHorizontalSpacing;
          end else begin
            LStretchMode := TViewStretchMode.None;
          end;
        end;
      TViewStretchMode.SpacingWidthUniform:
        begin
          if LColumns > 0 then begin
            if csDesigning in ComponentState then begin
              I := GetCount;
              if LColumns > I then
                LColumns := I;
            end else begin
              if LColumns > CtrlCount then
                LColumns := CtrlCount;
            end;
            LStretchMode := TViewStretchMode.SpacingWidthUniform;
            if FSpacingBorder then
              PW := (VW - LItemWidth * LColumns + FHorizontalSpacing * 2) / (LColumns + 1)
            else
              PW := (VW - LItemWidth * LColumns) / (LColumns + 1);
            if PW < 0 then PW := 0;
            CurPos.X := Padding.Left;
            AW := AW - PW;
            if not FSpacingBorder then
              AW := AW - FHorizontalSpacing;
          end else begin
            LStretchMode := TViewStretchMode.None;
          end;
        end;
    end;

    if LStretchMode = TViewStretchMode.None then begin
      AW := AW - LItemWidth;
      //if FSpacingBorder then
      //  AW := AW - FHorizontalSpacing;
    end;

    if FColumnHeight < 0 then
      LItemHeight := CDefaultColumnHeight
    else
      LItemHeight := FColumnHeight;

    FLastColumns := LColumns;
    FLastRows := 1;
    FLastRH := LItemHeight;
    FLastCW := LItemWidth;
    FLastPW := PW;
    FLastStretchMode := LStretchMode;
    //VW := 0;

    for I := 0 to CtrlCount - 1 do begin
      Control := Controls[I];
      if not Control.Visible then Continue;
      {$IFDEF MSWINDOWS}
      // 如果在设计状态，组件是 DesignerControl 时忽略
      if IsDesignerControl(Control) then
        Continue;
      {$ENDIF}

      if CurPos.X > AW then begin
        if (LStretchMode = TViewStretchMode.SpacingWidthUniform) or (not FSpacingBorder) then
          CurPos.X := Padding.Left
        else
          CurPos.X := Padding.Left + FHorizontalSpacing;
        CurPos.Y := CurPos.Y + LItemHeight + FVerticalSpacing;
        if FLastRows = 1 then begin
          if I > FLastColumns then
            FLastColumns := I;
        end;
        Inc(FLastRows);
      end;

      // 得到组件IView接口，及是否启用最大最小大小限制
      View := nil;
      if (Supports(Control, IView, View)) then
        SaveAdjustViewBounds := View.GetAdjustViewBounds
      else
        SaveAdjustViewBounds := False;

      case LStretchMode of
        TViewStretchMode.None: // 不自动调整大小
          begin
            if FForceColumnSize then
              LAutoSize := True
            else begin
              if Assigned(View) then
                LAutoSize := View.GetWidthSize = TViewSize.FillParent
              else begin
                LAutoSize := Control.Align in [TAlignLayout.Top, TAlignLayout.Bottom,
                  TAlignLayout.MostTop, TAlignLayout.MostBottom,
                  TAlignLayout.Client, TAlignLayout.Contents,
                  TAlignLayout.VertCenter, TAlignLayout.Horizontal, TAlignLayout.Fit,
                  TAlignLayout.FitLeft, TAlignLayout.FitRight];
              end;
            end;
            if LAutoSize then begin
              // 自动宽度
              VL := CurPos.X + Control.Margins.Left;
              VW := LItemWidth - Control.Margins.Left - Control.Margins.Right;
            end else begin
              VW := Control.Width;
              // 非自动宽度
              case FGravity of
                TLayoutGravity.LeftTop, TLayoutGravity.LeftBottom:
                  VL := CurPos.X + Control.Margins.Left;
                TLayoutGravity.RightTop, TLayoutGravity.RightBottom, TLayoutGravity.CenterVRight:
                  VL := CurPos.X + (LItemWidth - VW - Control.Margins.Right);
                TLayoutGravity.CenterHorizontal, TLayoutGravity.CenterHBottom, TLayoutGravity.Center:
                  VL := CurPos.X + (LItemWidth - (VW + Control.Margins.Left + Control.Margins.Right)) * 0.5 + Control.Margins.Left;
              else
                begin
                  case Control.Align of
                    TAlignLayout.Left, TAlignLayout.MostLeft:
                      VL := CurPos.X + Control.Margins.Left;
                    TAlignLayout.Center, TAlignLayout.HorzCenter:
                      VL := CurPos.X + (LItemWidth - (VW + Control.Margins.Left + Control.Margins.Right)) * 0.5 + Control.Margins.Left;
                    TAlignLayout.Right, TAlignLayout.MostRight:
                      VL := CurPos.X + (LItemWidth - VW - Control.Margins.Right);
                  else
                    VL := CurPos.X;
                  end;
                end;
              end;
            end;
            CurPos.X := CurPos.X + LItemWidth + FHorizontalSpacing;
          end;

        TViewStretchMode.SpacingWidth: // 自动调整间隔距离
          begin
            VL := CurPos.X + Control.Margins.Left;
            VW := LItemWidth - Control.Margins.Left - Control.Margins.Right;
            CurPos.X := CurPos.X + LItemWidth + PW;
          end;

        TViewStretchMode.ColumnWidth:
          begin
            VL := CurPos.X + Control.Margins.Left;
            VW := LItemWidth - Control.Margins.Left - Control.Margins.Right;
            CurPos.X := CurPos.X + LItemWidth + FHorizontalSpacing;
          end;

        TViewStretchMode.SpacingWidthUniform:
          begin
            VL := CurPos.X + Control.Margins.Left + PW;
            VW := LItemWidth - Control.Margins.Left - Control.Margins.Right;
            CurPos.X := CurPos.X + LItemWidth + PW;
          end;
      else
        Continue;
      end;

      // 判断组件在垂直方向是否自动大小
      if FForceColumnSize then
        LAutoSize := True
      else begin
        if Assigned(View) then
          LAutoSize := View.GetHeightSize = TViewSize.FillParent
        else begin
          LAutoSize := Control.Align in [TAlignLayout.Left, TAlignLayout.Right,
            TAlignLayout.MostLeft, TAlignLayout.MostRight,
            TAlignLayout.Client, TAlignLayout.Contents,
            TAlignLayout.HorzCenter, TAlignLayout.Vertical, TAlignLayout.Fit,
            TAlignLayout.FitLeft, TAlignLayout.FitRight]
        end;
      end;

      // 检测宽度大小限制
      if SaveAdjustViewBounds then begin
        if (View.GetMaxWidth > 0) and (VW > View.GetMaxWidth) then
          VW := View.GetMaxWidth;
        if (View.GetMinWidth > 0) and (VW < View.GetMinWidth) then
          VW := View.GetMinWidth;
      end;

      if LAutoSize then begin
        // 自动高度时
        VT := CurPos.Y + Control.Margins.Top;
        VH := LItemHeight - Control.Margins.Bottom - Control.Margins.Top;
        // 检测高度大小限制
        if SaveAdjustViewBounds then begin
          if (View.GetMaxHeight > 0) and (VH > View.GetMaxHeight) then
            VH := View.GetMaxHeight;
          if (View.GetMinHeight > 0) and (VH < View.GetMinHeight) then
            VH := View.GetMinHeight;
        end;
      end else begin
        // 非自动高度时，以重力设置来调整位置
        VH := Control.Height;
        case FGravity of
          TLayoutGravity.LeftTop, TLayoutGravity.RightTop:
            // 顶部
            VT := CurPos.Y + Control.Margins.Top;
          TLayoutGravity.LeftBottom, TLayoutGravity.RightBottom, TLayoutGravity.CenterHBottom:
            // 底部
            VT := CurPos.Y + (LItemHeight - VH - Control.Margins.Bottom);
          TLayoutGravity.CenterVertical, TLayoutGravity.Center, TLayoutGravity.CenterVRight:
            // 居中
            VT := CurPos.Y + (LItemHeight - (VH + Control.Margins.Top + Control.Margins.Bottom)) * 0.5 + Control.Margins.Top;
        else
          begin
            case Control.Align of
              TAlignLayout.Top, TAlignLayout.MostTop:
                VT := CurPos.Y + Control.Margins.Top;
              TAlignLayout.Center, TAlignLayout.VertCenter:
                VT := CurPos.Y + (LItemHeight - (VH + Control.Margins.Top + Control.Margins.Bottom)) * 0.5 + Control.Margins.Top;
              TAlignLayout.Bottom, TAlignLayout.MostBottom:
                VT := CurPos.Y + (LItemHeight - VH - Control.Margins.Bottom);
            else
              VT := CurPos.Y;
            end;
          end;
        end;
      end;

      // 调整组件大小
      if Assigned(View) then begin
        Control.SetBounds(VL, VT, VW, VH);
      end else
        Control.SetBounds(VL, VT, VW, VH);

    end;

    if FLastRows = 1 then
      FLastColumns := CtrlCount;

    // 判断是否组件大小为随内容。如果是，根据内容大小调整大小
    if (WidthSize = TViewSize.WrapContent) then begin
      if LColumns > CtrlCount then
        LColumns := CtrlCount;
      VW := LColumns * (LItemWidth + FHorizontalSpacing) + FHorizontalSpacing + Padding.Left + Padding.Right;
      PW := GetParentMaxWidth;
      if (VW > PW) and (PW > 0) then
        VW := PW;
    end else
      VW := Width;

    if (HeightSize = TViewSize.WrapContent) then begin
      VH := CurPos.Y + LItemHeight + Padding.Bottom;
      if FSpacingBorder then
        VH := VH + FVerticalSpacing;
      PW := GetParentMaxHeight;
      if (VH > PW) and (PW > 0) then
        VH := PW;
    end else
      VH := Height;

    if (WidthSize = TViewSize.WrapContent) or (HeightSize = TViewSize.WrapContent) then begin
      if (Height <> VH) or (Width <> VW) then
        SetBounds(Position.X, Position.Y, VW, VH);
    end;

  end else begin
    FLastColumns := 0;
    FLastRows := 0;
  end;

  FDisableAlign := False;
end;

procedure TGridsLayout.DrawDivider(Canvas: TCanvas);
var
  I, J, S: Integer;
  X, Y, W, H: Single;
begin
  if FSpacingBorder then
    S := 0
  else
    S := 1;
  // 垂直方向
  if FVerticalSpacing > 0 then begin
    J := FLastRows;
    if FSpacingBorder then begin
      Y := Padding.Top;
      X := Padding.Left;
      W := Width - Padding.Right;
    end else begin
      Y := Padding.Top + FLastRH;
      X := Padding.Left;
      W := Width - Padding.Right;
      Dec(J);
    end;
    for I := S to J do begin
      Canvas.FillRect(RectF(X, Y, W, Y + FVerticalSpacing), 0, 0, [], Opacity, FDividerBrush);
      Y := Y + FVerticalSpacing + FLastRH;
    end;
  end;

  // 水平方向
  if FHorizontalSpacing > 0 then begin
    J := FLastColumns;
    if not FSpacingBorder then
      Dec(J);
    X := Padding.Left;
    Y := Padding.Top;
    H := Height - Padding.Bottom;
    for I := 0 to J do begin
      if FSpacingBorder then begin
        Canvas.FillRect(RectF(X, Y, X + FHorizontalSpacing, H), 0, 0, [], Opacity, FDividerBrush);
      end else begin
        if (I > 0) then
          Canvas.FillRect(RectF(X - FHorizontalSpacing, Y, X, H), 0, 0, [], Opacity, FDividerBrush);
      end;
      case FLastStretchMode of
        TViewStretchMode.None,
        TViewStretchMode.ColumnWidth:
          begin
            if (J = 1) and (FSpacingBorder) then
              X := Width - Padding.Right - FHorizontalSpacing
            else
              X := X + FHorizontalSpacing + FLastCW;
          end;
        TViewStretchMode.SpacingWidth:
          begin
            if (I = 0) or (I = (J - 1)) then begin
              if FSpacingBorder then
                X := X + FLastCW + (FLastPW + FHorizontalSpacing) * 0.5
              else begin
                if I = 0 then
                  X := X + FLastCW + (FLastPW + FHorizontalSpacing) * 0.5
                else
                  X := X + FLastCW + (FLastPW);
              end;
            end else
              X := X + FLastCW + (FLastPW);
          end;
        TViewStretchMode.SpacingWidthUniform:
          begin
            if J = S + 1 then begin
              X := Width - Padding.Right - FHorizontalSpacing
            end else begin
              if (I = 0) or (I = (J - 1)) then begin
                if FSpacingBorder then
                  X := X + FLastCW + (FLastPW) * 1.5 - FHorizontalSpacing * 0.5
                else begin
                  if I = 0 then
                    X := X + FLastCW + (FLastPW) * 1.5 + FHorizontalSpacing * 0.5
                  else
                    X := X + FLastCW + (FLastPW)
                end;
              end else
                X := X + FLastCW + (FLastPW);
            end;
          end;
      end;
    end;
  end;
end;

function TGridsLayout.GetAbsoluteColumnsNum: Integer;
begin
  if FNumColumns > 0 then
    Result := FNumColumns
  else begin
    if FSpacingBorder then begin
      if FColumnWidth > 0 then
        Result := Trunc((Width - Padding.Left - Padding.Right - FHorizontalSpacing) / (FColumnWidth + FHorizontalSpacing))
      else
        Result := Trunc((Width - Padding.Left - Padding.Right - FHorizontalSpacing) / (CDefaultColumnWidth + FHorizontalSpacing));
    end else begin
      if FColumnWidth > 0 then
        Result := Trunc((Width - Padding.Left - Padding.Right + FHorizontalSpacing) / (FColumnWidth + FHorizontalSpacing))
      else
        Result := Trunc((Width - Padding.Left - Padding.Right + FHorizontalSpacing) / (CDefaultColumnWidth + FHorizontalSpacing));
    end;
  end;
end;

function TGridsLayout.GetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ControlsCount - 1 do begin
    if not Controls[I].Visible then Continue;
    {$IFDEF MSWINDOWS}
    if IsDesignerControl(Controls[I]) then
      Continue;
    {$ENDIF}
    Inc(Result);
  end;
end;

function TGridsLayout.GetDivider: TAlphaColor;
begin
  Result := FDividerBrush.Color;
end;

function TGridsLayout.IsStoredColumnHeight: Boolean;
begin
  Result := FColumnHeight <> CDefaultColumnHeight;
end;

function TGridsLayout.IsStoredColumnWidth: Boolean;
begin
  Result := FColumnWidth <> CDefaultColumnWidth;
end;

procedure TGridsLayout.PaintBackground;
begin
  inherited PaintBackground;
  if (FLastColumns > 0) and (FLastRows > 0) and (FDividerBrush.Color and $FF000000 <> 0) then
    DrawDivider(Canvas);
end;

procedure TGridsLayout.SetColumnHeight(const Value: Single);
begin
  if FColumnHeight <> Value then begin
    FColumnHeight := Value;
    DoRealign;
  end;
end;

procedure TGridsLayout.SetColumnWidth(const Value: Single);
begin
  if FColumnWidth <> Value then begin
    FColumnWidth := Value;
    DoRealign;
  end;
end;

procedure TGridsLayout.SetDivider(const Value: TAlphaColor);
begin
  if FDividerBrush.Color <> Value then begin
    FDividerBrush.Color := Value;
    Repaint;
  end;
end;

procedure TGridsLayout.SetForceColumnSize(const Value: Boolean);
begin
  if FForceColumnSize <> Value then begin
    FForceColumnSize := Value;
    DoRealign;
  end;
end;

procedure TGridsLayout.SetHorizontalSpacing(const Value: Single);
begin
  if FHorizontalSpacing <> Value then begin
    FHorizontalSpacing := Value;
    DoRealign;
  end;
end;

procedure TGridsLayout.SetNumColumns(const Value: Integer);
begin
  if FNumColumns <> Value then begin
    FNumColumns := Value;
    DoRealign;
    Repaint;
  end;
end;

procedure TGridsLayout.SetSpacingBorder(const Value: Boolean);
begin
  if FSpacingBorder <> Value then begin
    FSpacingBorder := Value;
    DoRealign;
    Repaint;
  end;
end;

procedure TGridsLayout.SetStretchMode(const Value: TViewStretchMode);
begin
  if FStretchMode <> Value then begin
    FStretchMode := Value;
    DoRealign;
    Repaint;
  end;
end;

procedure TGridsLayout.SetVerticalSpacing(const Value: Single);
begin
  if FVerticalSpacing <> Value then begin
    FVerticalSpacing := Value;
    DoRealign;
  end;
end;

{ TDrawableBrush }

constructor TDrawableBrush.Create(AOwner: TComponent);
begin
  FImageLink := TGlyphImageLink.Create(Self);
  FImageLink.OnChange := DoChange;
  inherited Create(AOwner);
  if (csDesigning in ComponentState) then
    CreateBrush(FBrush);
end;

procedure TDrawableBrush.CreateBrush(var Value: TBrush);
begin
  if Assigned(Value) then
    FreeAndNil(Value);
  Value := TViewImagesBrush.Create(TBrushKind.None, TAlphaColorRec.Null);
  TViewImagesBrush(Value).FOwner := Self;
  Value.OnChanged := DoChange;
end;

destructor TDrawableBrush.Destroy;
begin
  FreeAndNil(FBrush);
  FImageLink.DisposeOf;
  inherited Destroy;
end;

procedure TDrawableBrush.DoChange(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Sender);
end;

procedure TDrawableBrush.Draw(Canvas: TCanvas; const R: TRectF;
  const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ACornerType: TCornerType);

  procedure DrawImage(const Index: Integer);
  var
    Images: TCustomImageList;
    Bitmap: TBitmap;
    BitmapSize: TSize;
  begin
    Images := GetImages;
    if Assigned(Images) and (Index >= 0) and (Index < Images.Count) then begin
      BitmapSize := TSize.Create(Round(R.Width) * 2, Round(R.Height) * 2);
      if BitmapSize.IsZero then
        Exit;
      Bitmap := Images.Bitmap(BitmapSize, Index);
      if Bitmap <> nil then
        Canvas.DrawBitmap(Bitmap, TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height), R, AOpacity, False);
    end;
  end;

begin
  if (csDestroying in ComponentState) or IsEmpty then Exit;
  if (Ord(FBrush.Kind) = Ord(TViewBrushKind.Patch9Bitmap)) and (FBrush is TViewBrush) then begin
    TDrawableBase.FillRect9Patch(Canvas, R, XRadius, YRadius, ACorners, AOpacity, TViewBrush(FBrush), ACornerType);
  end else
    Canvas.FillRect(R, XRadius, YRadius, ACorners, AOpacity, FBrush, ACornerType);
  if Assigned(FImageLink.Images) and (ImageIndex >= 0) then
    DrawImage(ImageIndex);
end;

function TDrawableBrush.GetBrush: TBrush;
begin
  if not Assigned(FBrush) then
    CreateBrush(FBrush);
  Result := FBrush;
end;

function TDrawableBrush.GetComponent: TComponent;
begin
  Result := Self;
end;

function TDrawableBrush.GetImageIndex: TImageIndex;
begin
  Result := FImageLink.ImageIndex;
end;

function TDrawableBrush.GetImageIndexEx: TImageIndex;
begin
  Result := TViewImagesBrush(Brush).ImageIndex;
end;

function TDrawableBrush.GetImageList: TBaseImageList;
begin
  Result := GetImages;
end;

function TDrawableBrush.GetImages: TCustomImageList;
begin
  if Assigned(FImageLink.Images) then
    Result := TCustomImageList(FImageLink.Images)
  else
    Result := nil;
end;

function TDrawableBrush.GetIsEmpty: Boolean;
begin
  if ImageIndex >= 0 then
    Result := not Assigned(FImageLink.Images)
  else
    Result := ((FBrush = nil) or (FBrush.Kind = TBrushKind.None));
end;

procedure TDrawableBrush.ImagesChanged;
begin
  DoChange(Self);
end;

procedure TDrawableBrush.SetBrush(const Value: TBrush);
begin
  if (Value = nil) then begin
    FreeAndNil(FBrush);
  end else begin
    if not Assigned(FBrush) then
      CreateBrush(FBrush);
    FBrush.Assign(Value);
  end;
end;

procedure TDrawableBrush.SetImageIndex(const Value: TImageIndex);
begin
  FImageLink.ImageIndex := Value;
end;

procedure TDrawableBrush.SetImageIndexEx(const Value: TImageIndex);
begin
  TViewImagesBrush(Brush).ImageIndex := Value;
end;

procedure TDrawableBrush.SetImageList(const Value: TBaseImageList);
begin
  ValidateInheritance(Value, TCustomImageList);
  SetImages(TCustomImageList(Value));
end;

procedure TDrawableBrush.SetImages(const Value: TCustomImageList);
begin
  FImageLink.Images := Value;
end;

{ TRectFHelper }

procedure TRectFHelper.Clear;
begin
  Left := 0;
  Top := 0;
  Right := 0;
  Bottom := 0;
end;

{ TControlHelper }

procedure TControlHelper.FocusToNext;
var
  I, J, K, M: Integer;
  Item: TControl;
begin
  if not Assigned(Self) then
    Exit;
  K := $FFFFFF;
  J := -1;
  M := TabOrder;
  if Assigned(ParentControl) then begin
    for I := 0 to ParentControl.ControlsCount - 1 do begin
      Item := ParentControl.Controls[I];
      if (not Assigned(Item)) or (not Item.Visible) or (not Item.Enabled) or (not Item.CanFocus) then
        Continue;
      if (Item.TabOrder < K) and (Item.TabOrder > M) then begin
        K := Item.TabOrder;
        J := I;
      end;
    end;
    if J >= 0 then
      ParentControl.Controls[J].SetFocus;
  end;
end;

function TControlHelper.SetFocusObject(V: TControl): Boolean;
var
  Item: TControl;
  I: Integer;
begin
  Result := False;
  for I := 0 to V.ControlsCount - 1 do begin
    Item := V.Controls[I];
    if Item.Visible and (Item.Enabled) and Item.CanFocus then begin
      Item.SetFocus;
      Result := True;
      Break;
    end else if Item.ControlsCount > 0 then begin
      if SetFocusObject(Item) then
        Break;
    end;
  end;
end;

{ TViewHtmlText }

type
  TViewHtmlReadAttr = reference to procedure (var Item: THtmlTextItem; const Key, Value: string);

procedure TViewHtmlText.Assign(Source: TPersistent);
begin
  if Source is TViewHtmlText then begin
    SetHtmlText(TViewHtmlText(Source).HtmlText);
    FDefaultCursor := TViewHtmlText(Source).FDefaultCursor;
  end else
    inherited;
end;

procedure TViewHtmlText.CalcTextSize(Canvas: TCanvas;
  TextSet: UI.Base.TTextSettings; const R: TRectF; var ASize: TSizeF);
begin
  Draw(Canvas, TextSet, R, 1, TViewState.None, @ASize);
end;

constructor TViewHtmlText.Create(const AHtmlText: string);
begin
  FList := THtmlDataList.Create();
  FDefaultCursor := crDefault;
  SetHtmlText(AHtmlText);
end;

destructor TViewHtmlText.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FFont);
  FreeAndNil(FLinkHrefs);
  inherited;
end;

procedure TViewHtmlText.Draw(Canvas: TCanvas; TextSet: UI.Base.TTextSettings;
  const R: TRectF; const Opacity: Single; State: TViewState; ASize: PSizeF);
var
  CharW, CharH: Single;

  procedure UpdateXY(var X, Y: Single; const S: TSizeF);
  begin
    case TextSet.Gravity of
      TLayoutGravity.LeftBottom:
        begin
          X := R.Left;
          Y := R.Bottom - S.Height;
        end;
      TLayoutGravity.RightTop:
        begin
          X := R.Right - S.Width;
          Y := R.Top;
        end;
      TLayoutGravity.RightBottom:
        begin
          X := R.Right - S.Width;
          Y := R.Bottom - S.Height;
        end;
      TLayoutGravity.CenterVertical:
        begin
          X := R.Left;
          Y := R.Top + (R.Bottom - R.Top - S.Height) / 2;
        end;
      TLayoutGravity.CenterHorizontal:
        begin
          X := R.Left + (R.Right - R.Left - S.Width) / 2;
          Y := R.Top;
        end;
      TLayoutGravity.CenterHBottom:
        begin
          X := R.Left + (R.Right - R.Left - S.Width) / 2;
          Y := R.Bottom - S.Height;
        end;
      TLayoutGravity.CenterVRight:
        begin
          X := R.Right - S.Width;
          Y := R.Top + (R.Bottom - R.Top - S.Height) / 2;
        end;
      TLayoutGravity.Center:
        begin
          X := R.Left + (R.Right - R.Left - S.Width) / 2;
          Y := R.Top + (R.Bottom - R.Top - S.Height) / 2;
        end;
    end;
  end;

  function IncludeStyle(const Src: TFontStyles; const V: TFontStyle): TFontStyles;
  begin
    Result := Src;
    Include(Result, V);
  end;

  procedure DrawText(const LText: string; const Item: THtmlTextItem; const LColor: TAlphaColor;
    var X, Y: Single; var S: TSizeF);
  begin
    TextSet.FillText(Canvas, RectF(X, Y, R.Right, R.Bottom), LText, Opacity, LColor,
          TextSet.FillTextFlags, @S, Canvas.Scale, TTextAlign.Leading, TTextAlign.Leading);

    if Item.Link >= 0 then // 记录超链接区域
      FLinkRange[Item.Link] := RectF(X, Y, X + S.Width, Y + S.Height);

     X := X + S.Width;
  end;

  // Flag 非0时用于计算大小
  procedure DrawWordWarpText(const LText: string; const Item: THtmlTextItem; const LColor: TAlphaColor;
    var X, Y: Single; const LX, MW: Single; var S: TSizeF; Flag: Integer = 0);
  var
    J: Integer;
    P, PE, P1: PChar;
    LW: Single;
    LTmp: string;
  begin
    if MW < CharW then
      Exit;

    LW := S.Width;

    if X + CharW >= MW then begin
      Y := Y + S.Height;
      X := LX;
    end;


    if Item.Link >= 0 then begin
      // 超链接不换行
      if X > LX then begin
        TextSet.TextSize(LText, S, Canvas.Scale);
        if X + S.Width > MW then begin
          X := LX;
          Y := Y + S.Height;
        end;
      end;

      if Flag = 0 then begin
        TextSet.WordWrap := True;
        DrawText(LText, Item, LColor, X, Y, S);
        TextSet.WordWrap := False;
      end else begin
        TextSet.TextSize(LText, S, Canvas.Scale, MW - X, True);
        X := X + S.Width;
        S.Width := Max(LW, X);
      end;
      if S.Height > CharH then begin  // 超链接换行后，尾部不再跟其它内容
        Y := Y + S.Height;
        X := LX;
      end;

    end else begin
      // 自动换行
      P := PChar(LText);
      PE := P + Length(LText);
      while P < PE do begin
        J := Trunc((MW - X) / CharW);
        if J < 1 then Break;

        SetString(LTmp, P, Min(PE - P, J));
        if J > 1 then begin
          TextSet.TextSize(LTmp, S, Canvas.Scale);
          if X + S.Width > MW then begin
            P1 := P + J;
            while P1 > P do begin
              SetString(LTmp, P, P1 - P);
              TextSet.TextSize(LTmp, S, Canvas.Scale);
              if X + S.Width < MW then
                Break;
              Dec(P1);
            end;
            J := P1 - P;
            if J < 1 then Break;
          end;
        end;

        if Flag = 0 then
          DrawText(LTmp, Item, LColor, X, Y, S)
        else begin
          X := X + S.Width;
          LW := Max(LW, X);
        end;

        Inc(P, J);
        if PE - P > 0 then begin
          Y := Y + S.Height;
          X := LX;
        end;

      end;

      S.Width := LW;
    end;
  end;

  procedure ClacWordWarpTextSize(var S: TSizeF);
  var
    I: Integer;
    LText: string;
    Item: THtmlTextItem;
    X, Y, MW: Single;
  begin
    X := 0;
    Y := 0;
    S.Width := 0;
    S.Height := 0;
    MW := R.Right - R.Left;

    for I := 0 to FList.Count - 1 do begin
      Item := FList[I];
      if Item.Len = 0 then Continue;

      if (Item.Len = 1) and (Item.P^ = #13) then begin
        Y := Y + S.Height;
        X := 0;
        Continue;
      end;

      SetString(LText, Item.P, Item.Len);

      if FReplace then
        LText := ReplaceValue(LText);

      TextSet.Font.Style := FFont.Style;
      if (Item.Link >= 0) and (Item.Link = FLinkHot) then begin
        TextSet.Font.Style := IncludeStyle(Item.Style, TFontStyle.fsUnderline)
      end else
        TextSet.Font.Style := Item.Style;

      DrawWordWarpText(LText, Item, 0, X, Y, 0, MW, S, 1);
    end;

    S.Height := Y + S.Height;
  end;

var
  I: Integer;
  Item: THtmlTextItem;
  X, Y, LX: Single;
  S: TSizeF;
  LColor: TAlphaColor;
  LWordWarp, LVCenter: Boolean;
  LFontChange: TNotifyEvent;
  LText: string;
begin
  CharW := 0;
  CharH := 0;
  X := R.Left;
  Y := R.Top;
  LWordWarp := TextSet.WordWrap;
  LVCenter := LWordWarp and
    (TextSet.Gravity in [TLayoutGravity.CenterVertical, TLayoutGravity.CenterVRight, TLayoutGravity.Center]);

  LFontChange := TextSet.Font.OnChanged;
  TextSet.Font.OnChanged := nil;
  if not Assigned(FFont) then
    FFont := TFont.Create;
  FFont.Assign(TextSet.Font);

  if LWordWarp then begin
    TextSet.TextSize('yh中', S, Canvas.Scale);
    CharW := S.Width / 4;
    CharH := S.Height;
  end;

  if LVCenter or (ASize <> nil) then begin
    ClacWordWarpTextSize(S);
  end else begin
    TextSet.CalcTextObjectSize(FText, $FFFFFF, Canvas.Scale, nil, S);
  end;

  if ASize <> nil then begin
    // 测算高度
    ASize.Width := S.Width;
    ASize.Height := S.Height;

    TextSet.WordWrap := LWordWarp;
    TextSet.Font.Assign(FFont);
    TextSet.Font.OnChanged := LFontChange;

    Exit;
  end;

  UpdateXY(X, Y, S);
  if LWordWarp then begin
    if X < R.Left then X := R.Left;
    if Y < R.Top then Y := R.Top;
  end;

  try
    TextSet.WordWrap := False;
    LX := X;

    for I := 0 to FList.Count - 1 do begin
      Item := FList[I];
      if Item.Len = 0 then Continue;

      SetString(LText, Item.P, Item.Len);

      if LText = #13 then begin
        Y := Y + S.Height;
        X := LX;
        Continue;
      end;

      LText := StringReplace(LText, #13, '', [rfReplaceAll]);
      LText := StringReplace(LText, #10, '', [rfReplaceAll]);
      if FReplace then
        LText := ReplaceValue(LText);

      TextSet.Font.Assign(FFont);
      if (Item.Link >= 0) and (Item.Link = FLinkHot) then begin
        TextSet.Font.Style := IncludeStyle(Item.Style, TFontStyle.fsUnderline)
      end else
        TextSet.Font.Style := Item.Style;

      if Item.Color = 0 then
        LColor := TextSet.GetStateColor(State)
      else
        LColor := Item.Color;

      if LWordWarp then begin
        DrawWordWarpText(LText, Item, LColor, X, Y, LX, R.Right, S);
      end else begin
        DrawText(LText, Item, LColor, X, Y, S);
      end;
    end;

  finally
    TextSet.WordWrap := LWordWarp;
    TextSet.Font.Assign(FFont);
    TextSet.Font.OnChanged := LFontChange;
  end;
end;

function TViewHtmlText.GetHtmlText: string;
var
  I: Integer;
  LText: string;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    for I := 0 to FList.Count - 1 do begin
      if FList[I].P <> PLineBreak then  begin
        with FList[I] do
          SetString(LText, P, Len);
        LText := StringReplace(LText, #13, '', [rfReplaceAll]);
        LText := StringReplace(LText, #10, '', [rfReplaceAll]);
        if FReplace then
          SB.Append(ReplaceValue(LText))
        else
          SB.Append(LText);
      end else
        SB.Append(PLineBreak);
    end;
  finally
    Result := SB.ToString;
    FreeAndNil(SB);
  end;
end;

procedure TViewHtmlText.MouseDown(Sender: TView; Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
end;

procedure TViewHtmlText.MouseLeave(Sender: TView);
begin
  FLinkHot := -1;
  Sender.Cursor := FDefaultCursor;
end;

procedure TViewHtmlText.MouseMove(Sender: TView; X, Y: Single);
var
  I: Integer;
begin
  I := PointInLink(X, Y);
  if I <> FLinkHot then begin
    FLinkHot := I;
    if I >= 0 then
      Sender.Cursor := crHandPoint
    else
      Sender.Cursor := FDefaultCursor;
    if Assigned(Sender) then
      Sender.Invalidate;
  end;
end;

procedure TViewHtmlText.MouseUp(Sender: TView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  Item: THtmlTextItem;
  I: Integer;
begin
  if (Button = TMouseButton.mbLeft) and (FLinkHot >= 0) and (FLinkHot < FLinkHrefs.Count) then begin
    if Assigned(Sender) then begin
      for I := 0 to FList.Count - 1 do begin
        if FList[I].Link = FLinkHot then begin
          Item := FList[I];
          Sender.DoLinkClick(Item.Text, FLinkHrefs[Item.LinkURL]);
          Break;
        end;
      end;
    end;
  end;
end;

procedure ReadStyleProperty(var Item: THtmlTextItem; const Data: string);
var
  P, PE, P1: PChar;
  Key, Value: string;
begin
  P := PChar(Data);
  PE := P + Length(Data);
  while P < PE do begin
    SkipSpace(P);
    P1 := P;
    while (P < PE) and (P^ <> ':') do
      Inc(P);
    if (P >= PE) then Break;

    SetString(Key, P1, P - P1);
    Trim(Key);

    Inc(P);
    SkipSpace(P);
    P1 := P;
    while (P^ <> #0) and (P^ <> ';') do
      Inc(P);
    SetString(Value, P1, P - P1);
    if (Key <> '') then begin
      if Key = 'color' then
        Item.Color := HtmlColorToColor(Value)

      else if Key = 'text-decoration' then begin
        if Value = 'none' then
          Item.Style := []
        else if Value = 'underline' then
          Item.Style := [TFontStyle.fsUnderline]
        else if Value = 'overline' then
          Item.Style := []
        else if Value = 'line-through' then
          Item.Style := [TFontStyle.fsStrikeOut]
        else if Value = 'blink' then
          Item.Style := [TFontStyle.fsBold]

      end else if Key = 'font-style' then begin
        if Value = 'normal' then
          Item.Style := []
        else if Value = 'italic' then
          Item.Style := [TFontStyle.fsItalic]
        else if Value = 'oblique' then
          Item.Style := [TFontStyle.fsItalic]

      end else if Key = 'font-weight' then begin
        if (Value = 'normal') or (Value = 'lighter') then
          Exclude(Item.Style, TFontStyle.fsBold)
        else if (Value = 'bold') or (Value = 'bolder') then
          Include(Item.Style, TFontStyle.fsBold);
      end;
    end;
    Inc(P);
  end;
end;

procedure TViewHtmlText.ParseHtmlText(const Text: string);

  {$WARNINGS OFF}
  procedure ReadProperty(var Item: THtmlTextItem; var P, PE: PChar; OnReadAttr: TViewHtmlReadAttr);
  var
    P1: PChar;
    Key, Value: string;
  begin
    if not Assigned(OnReadAttr) then
      Exit;
    while P < PE do begin
      SkipSpace(P);
      P1 := P;
      while (P < PE) and (P^ <> '=') do
        Inc(P);
      if (P >= PE) then Break;

      SetString(Key, P1, P - P1);
      Trim(Key);

      Inc(P);
      SkipSpace(P);
      P1 := P;

      if (P^ = '"') or (P^ = '''') then begin
        Inc(P);
        while (P^ <> #0) and (P^ <> P1^) do
          Inc(P);
        Inc(P1);
        SetString(Value, P1, P - P1);
        Inc(P);
      end else begin
        while not CharInSet(P^, [#0, #9, ' ', #13, #10]) do
          Inc(P);
        SetString(Value, P1, P - P1);
      end;

      if Key <> '' then
        OnReadAttr(Item, Key, Value);
    end;
  end;
  {$WARNINGS ON}

  procedure SetItem(var Item: THtmlTextItem; const LText: string);
  var
    P, PE: PChar;
  begin
    if LText = 'b' then
      Include(Item.Style, TFontStyle.fsBold)
    else if LText = 'i' then
      Include(Item.Style, TFontStyle.fsItalic)
    else if LText = 'u' then
      Include(Item.Style, TFontStyle.fsUnderline)
    else if LText = 's' then
      Include(Item.Style, TFontStyle.fsStrikeOut)
    else if (LText = 'em') or (LText = 'strong') then begin
      Include(Item.Style, TFontStyle.fsItalic);
      Include(Item.Style, TFontStyle.fsBold);
    end else begin
      P := PChar(LText);
      PE := P + Length(LText);

      if StrLComp(P, 'font', 4) = 0 then begin  // font
        Inc(P, 4);
        ReadProperty(Item, P, PE,
          procedure (var Item: THtmlTextItem; const Key, Value: string)
          begin
            if Key = 'color' then
              Item.Color := HtmlColorToColor(Value)
            else if (Key = PStyle) and (Value <> '') then
              ReadStyleProperty(Item, Value);
          end
        );
      end else if StrLComp(P, 'a ', 2) = 0 then begin  // a 超链接
        Inc(P, 1);
        ReadProperty(Item, P, PE,
          procedure (var Item: THtmlTextItem; const Key, Value: string)
          begin
            if Key = 'href' then begin
              if not Assigned(FLinkHrefs) then
                FLinkHrefs := TStringList.Create;
              Item.LinkURL := FLinkHrefs.IndexOf(Value);
              if Item.LinkURL < 0 then begin
                Item.LinkURL := FLinkHrefs.Count;
                FLinkHrefs.Add(Value);
              end;
              Item.Link := FLinkRangeCount;
              Inc(FLinkRangeCount);
            end else if Key = 'color' then
              Item.Color := HtmlColorToColor(Value)
            else if (Key = PStyle) and (Value <> '') then
              ReadStyleProperty(Item, Value);
          end
        );
      end else if StrLComp(P, 'span ', 5) = 0 then begin  // span
        Inc(P, 4);
        ReadProperty(Item, P, PE,
          procedure (var Item: THtmlTextItem; const Key, Value: string)
          begin
            if Key = PStyle then
              ReadStyleProperty(Item, Value);
          end
        );
      end else if StrLComp(P, 'div ', 3) = 0 then begin  // div
        Inc(P, 3);
        ReadProperty(Item, P, PE,
          procedure (var Item: THtmlTextItem; const Key, Value: string)
          begin
            if (Key = PStyle) and (Value <> '') then
              ReadStyleProperty(Item, Value);
          end
        );
      end else if StrLComp(P, 'p ', 2) = 0 then begin  // p
        Inc(P, 1);
        ReadProperty(Item, P, PE,
          procedure (var Item: THtmlTextItem; const Key, Value: string)
          begin
            if (Key = PStyle) and (Value <> '') then
              ReadStyleProperty(Item, Value);
          end
        );
      end else if (P^ = 'h') and (PE - P > 1) and (P[1] > '0') and (P[1] < '9') then begin  // h1, h2, h3, ... , h9
        Inc(P, 2);
        Include(Item.Style, TFontStyle.fsBold);
        if PE - P > 0 then begin
          ReadProperty(Item, P, PE,
            procedure (var Item: THtmlTextItem; const Key, Value: string)
            begin
              if (Key = PStyle) and (Value <> '') then
                ReadStyleProperty(Item, Value);
            end
          );
        end;
      end;

    end;
  end;

  procedure AddItem(const P: PChar; const Len: Integer; const LText: string; NoneTag: Boolean = True);
  var
    Item: THtmlTextItem;
  begin
    if Len = 0 then
      Exit;
    Item.P := P;
    Item.Len := Len;
    Item.Color := 0;
    Item.Link := -1;
    Item.LinkURL := -1;
    Item.Style := [];
    Item.NoneTag := NoneTag;
    if LText <> '' then
      SetItem(Item, LText);
    List.Add(Item);
  end;

  procedure UpdateItem(const I: Integer; const LText: string);
  var
    Item: THtmlTextItem;
  begin
    Item := List.Items[I];
    if (Item.P = PLineBreak) or (not Item.NoneTag) then
      Exit;
    SetItem(Item, LText);
    List.Items[I] := Item;
  end;

  procedure ParseText(var P, PE: PChar; const AFlag: string);
  var
    P1, P2: PChar;
    LS, LE: string;
    SIndex, I: Integer;
    NeedBreak: Boolean;
  begin
    if PE - P < 1 then
      Exit;
    LS := AFlag;
    P2 := P;
    NeedBreak := False;

    while P < PE do begin
      P1 := StrScan(P, '<');

      if P1 = nil then begin
        AddItem(P, PE - P, '', False);
        P := PE;
        Break;
      end;

      if (P <> P1) and ((AFlag = '') or (LS = '')) then begin
        AddItem(P, P1 - P, '', False);
        if NeedBreak then begin
          P := P1;
          Inc(P1);
          P2 := P1 + Min(6, PE - P1);
          if (P2 = nil) or (P2 = P1) then
            Break;
          SetString(LS, P1, P2 - P1);
          LS := LowerCase(LS);
          P1 := PChar(LS);
          if (StrLComp(P1, 'div', 3) = 0) or (StrLComp(P1, 'p', 1) = 0) or (StrLComp(P1, 'li', 2) = 0) then begin
            AddItem(PLineBreak, 1, '');
          end else if P1^ = 'h' then begin
            Inc(P1);
            if (P1^ > '0') and (P1^ < '9') then begin
              AddItem(PLineBreak, 1, '');
            end;
          end;
          Break;
        end else
          LS := '';

      end else if NeedBreak then begin
        Break;
      end else if (P = P1) and (AFlag = '') then
        LS := '';

      P := P1 + 1;
      P1 := StrScan(P, '>');
      if (P1 = nil) or (P1 = P) then
        Break;

      if LS = '' then begin
        SetString(LS, P, P1 - P);
        LS := LowerCase(LS);
      end else begin
        SkipSpace(P);
        if P^ = '/' then begin
          Inc(P);
          SetString(LE, P, P1 - P);
          LE := LowerCase(LE);
          if (LE = LS) or ((Length(LS) > Length(LE)) and (Pos(LE + ' ', LS) > 0)) then begin
            AddItem(P2, P - P2 - 2, LS);
            if LE <> '' then begin
              if (LE = 'p') or (LE = 'div') or (LE = 'li') or ((Length(LE) = 2) and (PChar(LE)^ = 'h')) then begin
                AddItem(PLineBreak, 1, LS);
              end;
            end;
          end;
          P := P1 + 1;
          if P1^ <> '>' then
            SkipSpace(P);
          LS := '';
          LE := '';
          NeedBreak := True;
          Continue;
        end else begin
          SetString(LE, P, P1 - P);
          LE := LowerCase(Trim(LE));

          if LE = 'br' then begin // 换行
            AddItem(PLineBreak, 1, LS);
            LS := '';
            P := P1 + 1;
            Continue;
          end;

          P := P1 + 1;
          SIndex := List.Count;
          ParseText(P, PE, LE);

          SkipSpace(P);
          P2 := P;
          for I := SIndex to List.Count - 1 do
            UpdateItem(I, LS);

          Continue;
        end;
      end;

      P := P1 + 1;

      if LS = 'br' then begin // 换行
        AddItem(PLineBreak, 1, LS);
        SkipSpace(P);
        LS := '';
        Continue;
      end else begin
        if LE <> '' then begin
          if (LE = 'p') or (LE = 'div') or (LE = 'li') or ((Length(LE) = 2) and (PChar(LE)^ = 'h')) then begin
            AddItem(PLineBreak, 1, LS);
            NeedBreak := False;
          end;
        end;

        SIndex := List.Count;
        ParseText(P, PE, LS);

        for I := SIndex to List.Count - 2 do
          UpdateItem(I, LS);

        SkipSpace(P);
      end;

    end;
  end;

var
  P, PE: PChar;
begin
  List.Clear;
  if Text = '' then Exit;
  P := PChar(Text);
  PE := P + Length(Text);
  ParseText(P, PE, '');
end;

function TViewHtmlText.PointInLink(const X, Y: Single): Integer;
var
  I: Integer;
  P: TPointF;
begin
  P := PointF(X, Y);
  for I := 0 to High(FLinkRange) do begin
    if IsPointInRect(P, FLinkRange[I]) then begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TViewHtmlText.ReplaceValue(const Value: string): string;
begin
  Result := StringReplace(Value, '&#60;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&#62;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&#61;', '=', [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
end;

procedure TViewHtmlText.SetHtmlText(const Value: string);
begin
  if FHtmlText <> Value then begin
    FHtmlText := Value;
    FLinkRangeCount := 0;
    FLinkHot := -1;
    if Assigned(FLinkHrefs) then
      FLinkHrefs.Clear;
    if Pos('&', Value) > 0 then begin
      FReplace := True;
      FRealHtmlText := Value;
      FRealHtmlText := StringReplace(FRealHtmlText, '&#32;', ' ', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&#33;', '!', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&#34;', '"', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&#35;', '#', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&#36;', '$', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&#37;', '%', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&#38;', '&', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&#39;', '''', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&#64;', '@', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&nbsp;', ' ', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&amp;', '&', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&quot;', '"', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&apos;', '''', [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&cent;', #$FFE0, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&pound;', #$FFE1, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&yen;', #$FFE5, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&euro;', string(#8364), [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&sect;', #$00a7, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&copy;', #$00a9, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&reg;', #$00ae, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&trade;', string(#8482), [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&trade;', string(#8482), [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&times;', #$00d7, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&divide;', #$00f7, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&plusmn;', #$00b1, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&laquo;', #$00ab, [rfReplaceAll]);
      FRealHtmlText := StringReplace(FRealHtmlText, '&raquo;', #$00bb, [rfReplaceAll]);
      ParseHtmlText(FRealHtmlText);
    end else begin
      FReplace := False;
      FRealHtmlText := '';
      ParseHtmlText(FHtmlText);
    end;
    FText := GetHtmlText;
    SetLength(FLinkRange, FLinkRangeCount);
  end;
end;

{ THtmlTextItem }

function THtmlTextItem.Text: string;
begin
  if (P = nil) or (Len < 1) then
    Result := ''
  else
    SetString(Result, P, Len);
end;

{ TViewAccessory }

procedure TViewAccessory.Assign(Source: TPersistent);
begin
  if Source is TViewAccessory then begin
    Self.FAccessoryType := TViewAccessory(Source).FAccessoryType;
    Self.FAccessoryColor := TViewAccessory(Source).FAccessoryColor;
    Self.FStyle := TViewAccessory(Source).FStyle;
    FreeAndNil(Self.FAccessoryBmp);
    if TViewAccessory(Source).FPathData = nil then
      FreeAndNil(Self.FPathData)
    else begin
      if not Assigned(FPathData) then
        FPathData := TPathData.Create;
      FPathData.Assign(TViewAccessory(Source).FPathData);
    end;
    DoChanged;
  end else
    inherited;
end;

constructor TViewAccessory.Create;
begin
  FPathData := nil;
  FAccessoryBmp := nil;
  FAccessoryColor := TAlphaColorRec.White;
  FStyle := TViewAccessoryStyle.Accessory;
end;

destructor TViewAccessory.Destroy;
begin
  FreeAndNil(FAccessoryBmp);
  FreeAndNil(FPathData);
  inherited;
end;

procedure TViewAccessory.DoChanged;
begin
  case FStyle of
    TViewAccessoryStyle.Path:
      DoPathChanged(Self);
    TViewAccessoryStyle.Accessory:
      begin
        if FAccessoryType <> TViewAccessoryType.None then begin
          if not Assigned(FAccessoryBmp) then
            FAccessoryBmp := TBitmap.Create
          else
            FAccessoryBmp.Clear(claNull);
          FAccessoryBmp.Assign(FAccessoryImages.GetAccessoryImage(FAccessoryType));
          if FAccessoryColor <> TAlphaColorRec.Null then
            ReplaceOpaqueColor(FAccessoryBmp, FAccessoryColor);
        end else
          FreeAndNil(FAccessoryBmp);
      end;
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TViewAccessory.DoPathChanged(Sender: TObject);

  procedure UpdateSize(Path: TPathData);
  var
    I: Integer;
    MW, MH: Single;
  begin
    MW := 0;
    MH := 0;
    for I := 0 to Path.Count - 1 do begin
      with Path.Points[I] do begin
        if Kind = TPathPointKind.Close then
          Continue;
        MW := Max(Point.X, MW);
        MH := Max(Point.Y, MH);
      end;
    end;

    FAccessoryBmp.SetSize(Round(MW), Round(MH));
  end;

begin
  if Assigned(FPathData) then begin
    try
      if not Assigned(FAccessoryBmp) then
        FAccessoryBmp := TBitmap.Create
      else
        FAccessoryBmp.Clear(claNull);
      UpdateSize(FPathData);
      FAccessoryBmp.Canvas.BeginScene();
      try
        FAccessoryBmp.Canvas.Fill.Color := Color;
        FAccessoryBmp.Canvas.Fill.Kind := TBrushKind.Solid;
        FAccessoryBmp.Canvas.FillPath(FPathData, 1);
      finally
        FAccessoryBmp.Canvas.EndScene;
      end;
    except
      FreeAndNil(FAccessoryBmp);
    end;
  end;
end;

function TViewAccessory.GetIsEmpty: Boolean;
begin
  Result := (not Assigned(Self)) or (FAccessoryBmp = nil);
end;

function TViewAccessory.GetPathData: string;
begin
  if Assigned(FPathData) then
    Result := FPathData.Data
  else
    Result := '';
end;

procedure TViewAccessory.SetAccessoryColor(const Value: TAlphaColor);
begin
  if FAccessoryColor <> Value then begin
    FAccessoryColor := Value;
    DoChanged;
  end;
end;

procedure TViewAccessory.SetAccessoryType(const Value: TViewAccessoryType);
begin
  if FAccessoryType <> Value then begin
    FAccessoryType := Value;
    DoChanged;
  end;
end;

procedure TViewAccessory.SetPathData(const Value: string);
begin
  if Value = '' then begin
    if Assigned(FPathData) then begin
      FreeAndNil(FPathData);
      DoChanged;
    end;
  end else begin
    if not Assigned(FPathData) then
      FPathData := TPathData.Create;
    FPathData.Data := Value;
    DoChanged;
  end;
end;

procedure TViewAccessory.SetStyle(const Value: TViewAccessoryStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    DoChanged;
  end;
end;

initialization
  FAccessoryImages := TViewAccessoryImageList.Create;
  {$IFDEF ANDROID}
  TView.InitAudioManager();
  TView.InitFontGlyphs();
  DoInitFrameStatusHeight();
  DoInitNavigationBarHeight();
  {$ENDIF}

finalization
  {$IFDEF ANDROID}
  FAudioManager := nil;
  {$ENDIF}
  FreeAndNil(FAccessoryImages);

end.
