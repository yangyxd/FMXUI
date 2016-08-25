unit UI.Base;

interface

{$SCOPEDENUMS ON}

uses
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
  FMX.Utils, FMX.ImgList, FMX.MultiResBitmap, FMX.ActnList, System.Rtti, FMX.Consts,
  FMX.TextLayout, FMX.Objects, System.ImageList, System.RTLConsts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, System.Math,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement;

const
  AllCurrentPlatforms =
    pidWin32 or pidWin64 or pidOSX32 or
    pidiOSSimulator or pidiOSDevice or pidAndroid;

type
  IView = interface;
  IViewGroup = interface;
  TView = class;
  TViewGroup = class;

  TDrawable = class;
  TDrawableIcon = class;
  TViewColor = class;

  EViewError = class(Exception);
  EViewLayoutError = class(Exception);
  EDrawableError = class(Exception);

  /// <summary>
  /// 视图状态
  /// </summary>
  TViewState = (None {正常}, Pressed {按下}, Focused {取得焦点}, Hovered {悬停},
    Selected{选中}, Checked{复选}, Enabled{禁用}, Activated{激活});
  TViewStates = set of TViewState;

  /// <summary>
  /// 视图大小
  /// </summary>
  TViewSize = (WrapContent {随内容}, FillParent {填充父级});

  TViewBrushKind = (None, Solid, Gradient, Bitmap, Resource, Patch9Bitmap);

  /// <summary>
  /// 9 宫格位图
  /// </summary>
  TPatch9Bitmap = class(TBrushBitmap)
  private
    FBounds: TBounds;
    procedure SetBounds(const Value: TBounds);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Bounds: TBounds read FBounds write SetBounds;
  end;

  TViewBrush = class(TBrush)
  private
    function GetKind: TViewBrushKind;
    procedure SetKind(const Value: TViewBrushKind);
    function IsKindStored: Boolean;
    function IsPatch9BitmapStored: Boolean;
    function GetBitmap: TPatch9Bitmap;
    procedure SetBitmap(const Value: TPatch9Bitmap);
  protected
  public
    constructor Create(const ADefaultKind: TViewBrushKind; const ADefaultColor: TAlphaColor);
    destructor Destroy; override;
  published
    property Kind: TViewBrushKind read GetKind write SetKind stored IsKindStored;
    property Bitmap: TPatch9Bitmap read GetBitmap write SetBitmap stored IsPatch9BitmapStored;
  end;

  /// <summary>
  /// 绘制位置
  /// </summary>
  TDrawablePosition = (Left, Right, Top, Bottom);

  /// <summary>
  /// 可绘制对象
  /// </summary>
  TDrawableBase = class(TPersistent)
  private
    [Weak] FView: IView;
    FOnChanged: TNotifyEvent;

    FDefaultColor: TAlphaColor;
    FDefaultKind: TViewBrushKind;


    FXRadius, FYRadius: Single;
    FIsEmpty: Boolean;

    function GetStateBrush(const State: TViewState): TBrush; overload;
    procedure GetStateBrush(const State: TViewState; var V: TBrush); overload;
    procedure SetStateBrush(const State: TViewState; const V: TBrush);
    function GetBrush(const State: TViewState; AutoCreate: Boolean): TBrush;
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
  protected
    FDefault: TBrush;
    FPressed: TBrush;
    FFocused: TBrush;
    FHovered: TBrush;
    FSelected: TBrush;
    FChecked: TBrush;
    FEnabled: TBrush;
    FActivated: TBrush;

    function GetEmpty: Boolean; virtual;
    function GetDrawRect(const ALeft, ATop, ARight, ABottom: Single): TRectF; virtual;
    function GetValue(const Index: Integer): TBrush;
    procedure SetValue(const Index: Integer; const Value: TBrush);

    procedure DoChange(Sender: TObject);

    procedure FillRect9Patch(Canvas: TCanvas; const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TViewBrush; const ACornerType: TCornerType = TCornerType.Round);
    procedure FillRect(Canvas: TCanvas; const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.Round); inline;
    procedure DoDrawed(Canvas: TCanvas; var R: TRectF; AState: TViewState); virtual;
  public
    constructor Create(View: IView; const ADefaultKind: TViewBrushKind = TViewBrushKind.None;
      const ADefaultColor: TAlphaColor = TAlphaColors.Null);
    destructor Destroy; override;

    function GetStateItem(AState: TViewState): TBrush;

    procedure Assign(Source: TPersistent); override;
    procedure Change; virtual;
    procedure CreateBrush(var Value: TBrush); overload; virtual; abstract;
    function CreateBrush(): TBrush; overload;

    procedure Draw(Canvas: TCanvas); virtual;
    procedure DrawTo(Canvas: TCanvas; const R: TRectF); virtual;

    procedure SetRadius(const X, Y: Single);
    procedure SetDrawable(const Value: TDrawableBase); overload;
    procedure SetBrush(State: TViewState; const Value: TBrush); overload;
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
    procedure CreateBrush(var Value: TBrush); override;
  published
    property Padding: TBounds read FPadding write SetPadding;
    property Paddings: string read GetPaddings write SetPaddings;

    property XRadius;
    property YRadius;
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
  TViewBorderStyle = (None, RectBorder, LineBottom, LineSimple);

  TViewBorder = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FBrush: TStrokeBrush;
    FColor: TViewColor;
    FStyle: TViewBorderStyle;
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
  protected
    procedure DoChanged();
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property OnChanged: TNotifyEvent read FOnChanged write SetOnChanged;
    property Brush: TStrokeBrush read FBrush;
  published
    property Color: TViewColor read FColor write SetColor;
    property Width: Single read GetWidth write SetWidth stored WidthStored;
    property Style: TViewBorderStyle read FStyle write SetStyle default TViewBorderStyle.None;
    property Dash: TStrokeDash read GetDash write SetDash default TStrokeDash.Solid;
    property Cap: TStrokeCap read GetCap write SetCap default TStrokeCap.Flat;
    property Join: TStrokeJoin read GetJoin write SetJoin default TStrokeJoin.Miter;
  end;

  TDrawableBorder = class(TDrawable)
  private
    FBorder: TViewBorder;
    procedure SetBorder(const Value: TViewBorder);
    function GetBorder: TViewBorder;
  protected
    function GetEmpty: Boolean; override;
    procedure DoDrawed(Canvas: TCanvas; var R: TRectF; AState: TViewState); override;
  public
    constructor Create(View: IView; const ADefaultKind: TViewBrushKind = TViewBrushKind.None;
      const ADefaultColor: TAlphaColor = TAlphaColors.Null);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Border: TViewBorder read GetBorder write SetBorder;
  end;

  TViewImageLink = class(TGlyphImageLink)
  public
    constructor Create(AOwner: TDrawableIcon); reintroduce;
    procedure Change; override;
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
    function ImageIndexStored: Boolean; virtual;
  public
    constructor Create(View: IView; const ADefaultKind: TViewBrushKind = TViewBrushKind.None;
      const ADefaultColor: TAlphaColor = TAlphaColors.Null);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    /// <summary>
    /// 绘制，并调整原来的区域
    /// </summary>
    procedure AdjustDraw(Canvas: TCanvas; var R: TRectF; ExecDraw: Boolean);

    procedure CreateBrush(var Value: TBrush); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawTo(Canvas: TCanvas; const R: TRectF); override;
    procedure DrawImage(Canvas: TCanvas; Index: Integer; const R: TRectF); virtual;
  published
    property SizeWidth: Integer read FWidth write SetWidth default 16;
    property SizeHeight: Integer read FHeight write SetHeight default 16;
    property Padding: Integer read FPadding write SetPadding default 4;
    property Position: TDrawablePosition read FPosition write SetPosition default TDrawablePosition.Left;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex stored ImageIndexStored default -1;

    property XRadius;
    property YRadius;
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
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Default: TAlphaColor index 0 read GetValue write SetValue;
    property Pressed: TAlphaColor index 1 read GetValue write SetValue;
    property Focused: TAlphaColor index 2 read GetValue write SetValue;
    property Hovered: TAlphaColor index 3 read GetValue write SetValue;
    property Selected: TAlphaColor index 4 read GetValue write SetValue;
    property Checked: TAlphaColor index 5 read GetValue write SetValue;
    property Enabled: TAlphaColor index 6 read GetValue write SetValue;
    property Activated: TAlphaColor index 7 read GetValue write SetValue;
  end;

  TTextColor = class(TViewColor)
  published
    property HintText: TAlphaColor index 8 read GetValue write SetValue;
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

    FWidth: TViewSize;
    FHeight: TViewSize;

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
    property WidthSize: TViewSize read FWidth write SetWidth;
    property HeightSize: TViewSize read FHeight write SetHeight;
    property AlignParentLeft: Boolean read FAlignParentLeft write SetAlignParentLeft;
    property AlignParentTop: Boolean read FAlignParentTop write SetAlignParentTop;
    property AlignParentRight: Boolean read FAlignParentRight write SetAlignParentRight;
    property AlignParentBottom: Boolean read FAlignParentBottom write SetAlignParentBottom;
    property CenterInParent: Boolean read FCenterInParent write SetCenterInParent;
    property CenterHorizontal: Boolean read FCenterHorizontal write SetCenterHorizontal;
    property CenterVertical: Boolean read FCenterVertical write SetCenterVertical;
  end;

  /// <summary>
  /// 内容重力
  /// </summary>
  TLayoutGravity = (None, LeftTop, LeftBottom, RightTop, RightBottom,
    CenterVertical, CenterHorizontal, CenterHBottom, CenterVRight, Center);

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
    function GetViewState: TViewStates;
    function GetOrientation: TOrientation;
    function GetComponent: TComponent;
    function GetComponentState: TComponentState;

    function GetPosition: TPosition;
    function GetWidth: Single;
    function GetHeight: Single;
    function GetOpacity: Single;

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

    property Opacity: Single read GetOpacity;
    property Width: Single read GetWidth;
    property Height: Single read GetHeight;
    property Position: TPosition read GetPosition;
    property ParentControl: TControl read GetParentControl;
    property ParentView: IViewGroup read GetParentView;
  end;

  /// <summary>
  /// 可绘制背景层接口
  /// </summary>
  IViewGroup = interface(IView)
    ['{73A1B9E5-D4AF-4956-A15F-73B0B8EDADF9}']
    function AddView(View: TView): Integer;
    function RemoveView(View: TView): Integer;
  end;

  /// <summary>
  /// 字体设置
  /// </summary>
  TTextSettings = class(TPersistent)
  private
    [Weak] FOwner: TControl;
    FOnChanged: TNotifyEvent;
    FOnTextChanged: TNotifyEvent;
    FColor: TViewColor;
    FFont: TFont;
    FPrefixStyle: TPrefixStyle;
    FGravity: TLayoutGravity;
    FTrimming: TTextTrimming;
    FWordWrap: Boolean;
    FText: string;
    FAutoSize: Boolean;
    FIsSizeChange: Boolean;
    FIsEffectsChange: Boolean;
    FIsColorChange: Boolean;
    function GetGravity: TLayoutGravity;
    function GetWordWrap: Boolean;
    procedure SetColor(const Value: TViewColor);
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
  protected
    procedure DoChange; virtual;
    procedure DoTextChanged;
    procedure DoFontChanged(Sender: TObject);
    procedure DoColorChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Change;

    function CalcTextObjectSize(const MaxWidth, SceneScale: Single;
      const Margins: TBounds; var Size: TSizeF): Boolean;
    procedure FillText(const Canvas: TCanvas; const ARect: TRectF; const AText: string; const WordWrap: Boolean; const AOpacity: Single;
      const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; State: TViewState = TViewState.None);

    procedure Draw(const Canvas: TCanvas; const R: TRectF;
        const Opacity: Single; State: TViewState); overload;
    procedure Draw(const Canvas: TCanvas; const AText: string; const R: TRectF;
        const Opacity: Single; State: TViewState); overload;

    property IsColorChange: Boolean read FIsColorChange;
    property IsSizeChange: Boolean read FIsSizeChange;
    property IsEffectsChange: Boolean read FIsEffectsChange;

    property Text: string read FText write SetText;
    property FillTextFlags: TFillTextFlags read GetFillTextFlags;

    property HorzAlign: TTextAlign read GetHorzAlign write SetHorzAlign;
    property VertAlign: TTextAlign read GetVertAlign write SetVertAlign;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnTextChanged: TNotifyEvent read FOnTextChanged write FOnTextChanged;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Color: TViewColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property PrefixStyle: TPrefixStyle read FPrefixStyle write SetPrefixStyle;
    property Trimming: TTextTrimming read FTrimming write SetTrimming default TTextTrimming.None;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property Gravity: TLayoutGravity read GetGravity write SetGravity;
  end;

  /// <summary>
  /// 基本视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TView = class(TControl, IView)
  private
    function GetParentView: IViewGroup;
    function GetClickable: Boolean;
    procedure SetClickable(const Value: Boolean);
    function GetPaddings: string;
    procedure SetPaddings(const Value: string);
    function GetMargin: string;
    procedure SetMargin(const Value: string);
    procedure SetWeight(const Value: Single);
    procedure SetOrientation(const Value: TOrientation);
    function GetViewStates: TViewStates;
    function GetBackground: TDrawable;
    procedure SetMaxHeight(const Value: Single);
    procedure SetMaxWidth(const Value: Single);
    procedure SetMinHeight(const Value: Single);
    procedure SetMinWidth(const Value: Single);
    procedure SetAdjustViewBounds(const Value: Boolean);
    function GetLayout: TViewLayout;
    procedure SetLayout(const Value: TViewLayout);
    function GetHeightSize: TViewSize;
    function GetWidthSize: TViewSize;
    procedure SetHeightSize(const Value: TViewSize);
    procedure SetWidthSize(const Value: TViewSize);
    function GetAdjustViewBounds: Boolean;
    function GetGravity: TLayoutGravity;
    function GetMaxHeight: Single;
    function GetMaxWidth: Single;
    function GetMinHeight: Single;
    function GetMinWidth: Single;
    function GetWeight: Single;
    function GetOrientation: TOrientation;
    function GetComponent: TComponent;
    function GetComponentState: TComponentState;
    function GetOpacity: Single;
    function GetParentControl: TControl;
    function GetPosition: TPosition;
    function GetDrawState: TViewState;
    function GetViewRect: TRectF;
  protected
    FWeight: Single;
    FGravity: TLayoutGravity;
    FOrientation: TOrientation;
    FBackground: TDrawable;
    FDrawing: Boolean;
    FViewState: TViewStates;
    FMinWidth: Single;
    FMinHeight: Single;
    FMaxWidth: Single;
    FMaxHeight: Single;
    FLayout: TViewLayout;

    function IsDrawing: Boolean;
    function IsDesignerControl(Control: TControl): Boolean;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; virtual;
    function GetViewState: TViewStates;
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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    function DoSetSize(const ASize: TControlSize; const NewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
      var ALastWidth, ALastHeight: Single): Boolean; override;
  protected
    FAdjustViewBounds: Boolean;
    procedure Paint; override;
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    procedure DoAutoSize; virtual;
    procedure DoOrientation; virtual;
    procedure DoGravity; virtual;
    procedure DoWeight; virtual;
    procedure DoMaxSizeChange; virtual;
    procedure DoMinSizeChange; virtual;
    procedure DoAdjustViewBounds; virtual;
    procedure DoBackgroundChanged(Sender: TObject); virtual;
    procedure DoLayoutChanged(Sender: TObject); virtual;
    procedure DoChangeSize(var ANewWidth, ANewHeight: Single); virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure PaintBackground; virtual;
    procedure SetGravity(const Value: TLayoutGravity); virtual;
    function AllowUseLayout(): Boolean; virtual;
    procedure ImagesChanged; virtual;
    function CreateBackground: TDrawable; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
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

    property ParentView: IViewGroup read GetParentView;
    property Orientation: TOrientation read GetOrientation write SetOrientation;
    property ViewState: TViewStates read GetViewStates;
    property DrawState: TViewState read GetDrawState;
    property ViewRect: TRectF read GetViewRect;
  published
    property Align;
    property Action;
    property Anchors;
    property AdjustViewBounds: Boolean read GetAdjustViewBounds write SetAdjustViewBounds default True;
    property Background: TDrawable read GetBackground write SetBackground;
    property Cursor;
    property ClipChildren;
    property ClipParent;
    property Clickable: Boolean read GetClickable write SetClickable default False;
    property Enabled;
    property Locked;
    property Layout: TViewLayout read GetLayout write SetLayout;
    property Opacity;
    property RotationAngle;
    property RotationCenter;
    property Padding;
    property Paddings: string read GetPaddings write SetPaddings;
    property Margins;
    property Margin: string read GetMargin write SetMargin;
    property PopupMenu;
    property Visible;
    property Width;
    property Height;
    property WidthSize: TViewSize read GetWidthSize write SetWidthSize;
    property HeightSize: TViewSize read GetHeightSize write SetHeightSize;
    property MinWidth: Single read GetMinWidth write SetMinWidth;
    property MinHeight: Single read GetMinHeight write SetMinHeight;
    property MaxWidth: Single read GetMaxWidth write SetMaxWidth;
    property MaxHeight: Single read GetMaxHeight write SetMaxHeight;
    property Gravity: TLayoutGravity read GetGravity write SetGravity;
    property Weight: Single read GetWeight write SetWeight;
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

  //TViewEditor = class(TDefaultEditor)


  /// <summary>
  /// 基本视图组
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TViewGroup = class(TView, IViewGroup)
  private
  protected
    function IsAutoSize(View: IView;  Align: TAlignLayout;
      AOrientation: TOrientation): Boolean;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    procedure DoLayoutChanged(Sender: TObject); override;
    procedure DoGravity(); override;
    procedure DoMaxSizeChange; override;
    procedure DoMinSizeChange; override;
    procedure DoAdjustViewBounds; override;
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
  protected
    function GetWeightSum(var FixSize: Single): Single;
    function IsUseWeight(): Boolean;
    procedure DoRealign; override;
    procedure DoOrientation; override;
  published
    property Orientation;
  end;
  
  /// <summary>
  /// 相对布局
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TRelativeLayout = class(TViewGroup)
  private
    FViewList: TList;
    procedure DoAlignControl(const X, Y, W, H: Single);
  protected
    function GetXY(const StackList: TList; const Control: TControl;
      var X, Y, W, H: Single): Integer;
    procedure DoRealign; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

resourcestring
  SInvViewValue = '无效的视图状态值: %d';
  SNotAllowSelf = '不允许设定为自己';
  SMustSameParent = '必须指定一个与当前组件所属视图中的同级兄弟组件';
  SLocateFailed = '存在循环引用';
  SRefOutLimitMax = '组件引用层级超过上限值: 256';

type TPrivateControl = class(TControl);

function GetBoundsFloat(const R: TBounds): string;
begin
  if Assigned(R) and (R.Left = R.Top) and (R.Left = R.Right) and (R.Left = R.Bottom) then
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

procedure TDrawableBase.Change;
begin
  DoChange(Self);
end;

constructor TDrawableBase.Create(View: IView; const ADefaultKind: TViewBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  FView := View;
  FDefaultColor := ADefaultColor;
  FDefaultKind := ADefaultKind;

  if Assigned(FView) and (csDesigning in FView.GetComponentState) then begin
    CreateBrush(FDefault);
    CreateBrush(FPressed);
    CreateBrush(FFocused);
    CreateBrush(FHovered);
    CreateBrush(FSelected);
    CreateBrush(FChecked);
    CreateBrush(FEnabled);
    CreateBrush(FActivated);
    FIsEmpty := GetEmpty;
  end else
    FIsEmpty := True;
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

procedure TDrawableBase.DoDrawed(Canvas: TCanvas; var R: TRectF; AState: TViewState);
begin
end;

function TDrawableBase.GetValue(const Index: Integer): TBrush;
begin
  Result := GetBrush(TViewState(Index), False);
end;

function TDrawableBase.GetBrush(const State: TViewState; AutoCreate: Boolean): TBrush;
begin
  GetStateBrush(State, Result);
  if (not Assigned(Result)) and
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

function TDrawableBase.GetStateItem(AState: TViewState): TBrush;

  function BrushIsEmpty(V: TBrush): Boolean;
  begin
    if (not Assigned(V)) or (V.Kind = TBrushKind.None) or
      ((V.Color and $FF000000 = 0) and (V.Kind = TBrushKind.Solid)) then
      Result := True
    else
      Result := False;
  end;

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
  if not Assigned(FView) or (csDestroying in FView.GetComponentState) then Exit;
  if IsEmpty then Exit;
  AState := FView.GetDrawState;
  R := GetDrawRect(0, 0, FView.GetWidth, FView.GetHeight);
  V := GetStateItem(AState);
  if V <> nil then
    FillRect(Canvas, R, FXRadius, FYRadius, AllCorners, FView.GetOpacity, V);
  DoDrawed(Canvas, R, AState);
end;

procedure TDrawableBase.DrawTo(Canvas: TCanvas; const R: TRectF);
var
  V: TBrush;
  VR: TRectF;
  AState: TViewState;
begin
  if not Assigned(FView) or (csDestroying in FView.GetComponentState) then Exit;
  if IsEmpty then Exit;
  AState := FView.GetDrawState;
  V := GetStateItem(AState);
  VR := GetDrawRect(R.Left, R.Top, R.Right, R.Bottom);
  if V <> nil then
    FillRect(Canvas, VR, FXRadius, FYRadius, AllCorners, FView.GetOpacity, V);
  DoDrawed(Canvas, VR, AState);
end;

procedure TDrawableBase.FillRect(Canvas: TCanvas; const ARect: TRectF;
  const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ABrush: TBrush;
  const ACornerType: TCornerType = TCornerType.Round);
begin
  if (Ord(ABrush.Kind) = Ord(TViewBrushKind.Patch9Bitmap)) and (ABrush is TViewBrush) then begin
    FillRect9Patch(Canvas, ARect, XRadius, YRadius, ACorners, AOpacity, TViewBrush(ABrush), ACornerType);
  end else
    Canvas.FillRect(ARect, XRadius, YRadius, ACorners, AOpacity, ABrush, ACornerType);
end;

procedure TDrawableBase.FillRect9Patch(Canvas: TCanvas; const ARect: TRectF;
  const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ABrush: TViewBrush;
  const ACornerType: TCornerType);
var
  Bmp: TPatch9Bitmap;
begin
  if (ABrush.Bitmap = nil) or (ABrush.Bitmap.Bitmap = nil) or
    ABrush.Bitmap.Bitmap.IsEmpty then
    Exit;
  Bmp := TPatch9Bitmap(ABrush.Bitmap);
  if Bmp.Bounds.Empty then begin
    ABrush.OnChanged := nil;
    ABrush.Kind := TViewBrushKind.Bitmap;
    Canvas.FillRect(ARect, XRadius, YRadius, ACorners, AOpacity, ABrush, ACornerType);
    ABrush.Kind := TViewBrushKind.Patch9Bitmap;
    ABrush.OnChanged := ABrush.BitmapChanged;
    Exit;
  end;
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

procedure TDrawableBase.SetGradient(State: TViewState; const Value: TGradient);
var V: TBrush;
begin
  V := GetBrush(State, True);
  V.Gradient.Assign(Value);
  V.Kind := TBrushKind.Gradient;
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

procedure TDrawableBase.SetBrush(State: TViewState; const Value: TBrush);
begin
  GetBrush(State, True).Assign(Value);
end;

procedure TDrawableBase.SetBitmap(State: TViewState; const Value: TBitmap);
var V: TBrush;
begin
  V := GetBrush(State, True);
  V.Bitmap.Bitmap.Assign(Value);
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

procedure TDrawable.CreateBrush(var Value: TBrush);
begin
  if Assigned(Value) then
    FreeAndNil(Value);
  Value := TViewBrush.Create(FDefaultKind, FDefaultColor);
  Value.OnChanged := DoChange;
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
  Result := inherited GetValue(Index) as TViewBrush;
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

procedure TDrawableIcon.AdjustDraw(Canvas: TCanvas; var R: TRectF; ExecDraw: Boolean);
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
          DrawTo(Canvas, DR);
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
          DrawTo(Canvas, DR);
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
          DrawTo(Canvas, DR);
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
          DrawTo(Canvas, DR);
        end;
        R.Bottom := R.Bottom - FHeight - FPadding;
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

procedure TDrawableIcon.CreateBrush(var Value: TBrush);
begin
  if Assigned(Value) then
    FreeAndNil(Value);
  Value := TBrush.Create(TBrushKind(FDefaultKind), FDefaultColor);
  Value.OnChanged := DoChange;
end;

destructor TDrawableIcon.Destroy;
begin
  FImageLink.DisposeOf;
  inherited;
end;

procedure TDrawableIcon.Draw(Canvas: TCanvas);
begin
  inherited Draw(Canvas);
  if (ImageIndex >= 0) and Assigned(FImageLink.Images) then
    DrawImage(Canvas, ImageIndex, GetDrawRect(0, 0, FView.GetWidth, FView.GetHeight));
end;

procedure TDrawableIcon.DrawImage(Canvas: TCanvas; Index: Integer;
  const R: TRectF);
var
  Images: TCustomImageList;
  Bitmap: TBitmap;
  BitmapSize: TSize;
  BitmapRect: TRectF;
begin
  Images := GetImages;
  if Assigned(Images) and (Index >= 0) and (Index < Images.Count) then begin
    BitmapSize := TSize.Create(FWidth, FHeight);
    if BitmapSize.IsZero then
      Exit;
    Bitmap := Images.Bitmap(BitmapSize, Index);
    if Bitmap <> nil then begin
      BitmapRect := TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height);
      Canvas.DrawBitmap(Bitmap, BitmapRect, R, 1, True);
    end;
  end;
end;

procedure TDrawableIcon.DrawTo(Canvas: TCanvas; const R: TRectF);
begin
  inherited DrawTo(Canvas, R);
  if (ImageIndex >= 0) and Assigned(FImageLink.Images) then
    DrawImage(Canvas, ImageIndex, R);
end;

function TDrawableIcon.GetComponent: TComponent;
begin
  Result := FView.GetComponent;
end;

function TDrawableIcon.GetEmpty: Boolean;
begin
  if ImageIndex >= 0 then
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

function TDrawableIcon.ImageIndexStored: Boolean;
begin
  Result := (ImageIndex <> -1);
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

procedure TViewColor.SetValue(const Index: Integer; const Value: TAlphaColor);
begin
  SetColor(TViewState(Index), Value);
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
    FWidth := Src.FWidth;
    FHeight := Src.FHeight;

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
  if FHeight <> Value then begin
    FHeight := Value;
    DoChange;
  end;
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
  if FWidth <> Value then begin
    FWidth := Value;
    DoChange;
  end;
end;

{ TView }

function TView.AllowUseLayout: Boolean;
begin
  Result := (not (csDesigning in ComponentState)) or
    (Assigned(ParentControl)) and (ParentControl is TRelativeLayout);
end;

function TView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
begin
  Result := Assigned(View.Background) and
    Assigned(View.Background.GetStateBrush(State));
end;

constructor TView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAcceptsControls(False);
  ClipChildren := True;
  HitTest := False;
  FAdjustViewBounds := True;
  FViewState := [];
  if csDesigning in ComponentState then begin
    FBackground := CreateBackground();
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
end;

function TView.CreateBackground: TDrawable;
begin
  Result := TDrawable.Create(Self);
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
  DecChildState(State);
end;

destructor TView.Destroy;
begin
  FreeAndNil(FBackground);
  FreeAndNil(FLayout);
  inherited Destroy;
end;

procedure TView.DoActivate;
begin
  //IncViewState(TViewState.Activated);
  inherited DoActivate;
end;

procedure TView.DoAdjustViewBounds;
begin
  RecalcSize;
  if Assigned(ParentControl) then
    TPrivateControl(ParentControl).Realign;
end;

procedure TView.DoAutoSize;
begin
  RecalcSize;
  if Assigned(ParentControl) then
    TPrivateControl(ParentControl).Realign;
end;

procedure TView.DoBackgroundChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TView.DoChangeSize(var ANewWidth, ANewHeight: Single);
begin
  if FAdjustViewBounds then begin
    if (FMaxWidth > 0) and (ANewWidth > FMaxWidth) then
      ANewWidth := FMaxWidth;
    if (FMaxHeight > 0) and (ANewHeight > FMaxHeight) then
      ANewHeight := FMaxHeight;
    if (FMinWidth > 0) and (ANewWidth < FMinWidth) then
      ANewWidth := FMinWidth;
    if (FMinHeight > 0) and (ANewHeight < FMinHeight) then
      ANewHeight := FMinHeight;
  end;
end;

procedure TView.DoDeactivate;
begin
  DecViewState(TViewState.Activated);
  inherited DoDeactivate;
end;

procedure TView.DoGravity;
begin
  Repaint;
end;

procedure TView.DoLayoutChanged(Sender: TObject);
begin
  if Assigned(ParentControl) then
    TPrivateControl(ParentControl).Realign;
end;

procedure TView.DoMaxSizeChange;
begin
  RecalcSize;
  if Assigned(ParentControl) then
    TPrivateControl(ParentControl).Realign;
end;

procedure TView.DoMinSizeChange;
begin
  RecalcSize;
  if Assigned(ParentControl) then
    TPrivateControl(ParentControl).Realign;
end;

procedure TView.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if (csDesigning in ComponentState) then Exit;
  if TMouseButton.mbLeft = Button then begin
    IncViewState(TViewState.Pressed);
    if CanRePaintBk(Self, TViewState.Pressed) then Repaint;
  end;
end;

procedure TView.DoMouseEnter;
begin
  inherited DoMouseEnter;
  if (csDesigning in ComponentState) then Exit;
  IncViewState(TViewState.Hovered);
  if CanRePaintBk(Self, TViewState.Hovered) then Repaint;
end;

procedure TView.DoMouseLeave;
begin
  inherited DoMouseLeave;
  if (csDesigning in ComponentState) then Exit;
  DecViewState(TViewState.Hovered);
  if CanRePaintBk(Self, TViewState.Hovered) then Repaint;
end;

procedure TView.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if (csDesigning in ComponentState) then Exit;
  if TMouseButton.mbLeft = Button then begin
    DecViewState(TViewState.Pressed);
    if CanRePaintBk(Self, TViewState.Pressed) then Repaint;
  end;
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

function TView.GetDrawState: TViewState;
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
    else if TViewState.Hovered in FViewState then
      Result := TViewState.Hovered
    else if TViewState.Selected in FViewState then
      Result := TViewState.Selected
    else if TViewState.Checked in FViewState then
      Result := TViewState.Checked
    else if TViewState.Activated in FViewState then
      Result := TViewState.Activated
    else
      Result := TViewState.None
  end;
end;

function TView.GetGravity: TLayoutGravity;
begin
  Result := FGravity;
end;

function TView.GetHeightSize: TViewSize;
begin
  if Assigned(FLayout) then
    Result := FLayout.FHeight
  else Result := TViewSize.WrapContent;
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

function TView.GetParentView: IViewGroup;
begin
  Supports(Parent, IViewGroup, Result);
end;

function TView.GetPosition: TPosition;
begin
  Result := Position;
end;

function TView.GetViewRect: TRectF;
begin
  Result := RectF(Padding.Left, Padding.Top,
    Width - Padding.Right, Height - Padding.Bottom);
end;

function TView.GetViewState: TViewStates;
begin
  Result := FViewState;
end;

function TView.GetViewStates: TViewStates;
begin
  Result := FViewState;
end;

function TView.GetWeight: Single;
begin
  Result := FWeight;
end;

function TView.GetWidthSize: TViewSize;
begin
  if Assigned(FLayout) then
    Result := FLayout.FWidth
  else Result := TViewSize.WrapContent;
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
    if Supports(Controls.Items[I], IView, View) then
      View.IncViewState(State);
  end;
end;

procedure TView.IncViewState(const State: TViewState);
begin
  Include(FViewState, State);
  IncChildState(State);
end;

function TView.IsActivated: Boolean;
begin
  Result := TViewState.Activated in FViewState;
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

function TView.GetAdjustViewBounds: Boolean;
begin
  Result := FAdjustViewBounds;
end;

function TView.GetBackground: TDrawable;
begin
  if not Assigned(FBackground) then
    FBackground := CreateBackground();
  Result := FBackground;
end;

procedure TView.Loaded;
begin
  inherited Loaded;
end;

procedure TView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
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
  if Assigned(ParentControl) then
    TPrivateControl(ParentControl).Realign;
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

procedure TView.Paint;
begin
  inherited Paint;
  if not FDrawing then begin
    if FIsFocused then
      Include(FViewState, TViewState.Focused)
    else
      Exclude(FViewState, TViewState.Focused);
    FDrawing := True;
    try
      PaintBackground();
    finally
      FDrawing := False;
    end;
  end;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

procedure TView.PaintBackground;
begin
  if Assigned(FBackground) then
    FBackground.Draw(Canvas);
end;

procedure TView.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
end;

procedure TView.SetAdjustViewBounds(const Value: Boolean);
begin
  if FAdjustViewBounds <> Value then begin
    FAdjustViewBounds := Value;
    if Value then begin
      if (FMaxWidth > 0) or (FMaxHeight > 0) or (FMinWidth > 0) or (FMinHeight > 0) then
        DoAdjustViewBounds();
    end;
  end;
end;

procedure TView.SetBackground(const Value: TBitmap);
begin
  FBackground.SetBitmap(TViewState.None, Value);
end;

procedure TView.SetBackground(const Value: TBrushBitmap);
begin
  FBackground.SetBitmap(TViewState.None, Value);
end;

procedure TView.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited SetBounds(X, Y, AWidth, AHeight);
end;

procedure TView.SetBackground(const Value: TAlphaColor);
begin
  FBackground.SetColor(TViewState.None, Value);
end;

procedure TView.SetBackground(const Value: TGradient);
begin
  FBackground.SetGradient(TViewState.None, Value);
end;

procedure TView.SetBackground(const Value: TDrawable);
begin
  if (not Assigned(FBackground)) and (Assigned(Value)) then
    FBackground := CreateBackground();
  if Assigned(FBackground) then
    FBackground.SetDrawable(Value);
end;

procedure TView.SetClickable(const Value: Boolean);
begin
  HitTest := Value;
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
  if (not Assigned(FLayout)) and (Value <> TViewSize.WrapContent) then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  if Assigned(FLayout) then
    FLayout.HeightSize := Value;
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

procedure TView.SetWeight(const Value: Single);
begin
  if FWeight <> Value then begin
    FWeight := Value;
    DoWeight;
  end;
end;

procedure TView.SetWidthSize(const Value: TViewSize);
begin
  if (not Assigned(FLayout)) and (Value <> TViewSize.WrapContent) then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  if Assigned(FLayout) then
    FLayout.WidthSize := Value;
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
  inherited;
  Realign;
end;

procedure TViewGroup.DoAdjustViewBounds;
begin
  //inherited DoAdjustViewBounds;
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

procedure TViewGroup.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  Realign;
end;

function TViewGroup.IsAutoSize(View: IView; Align: TAlignLayout;
  AOrientation: TOrientation): Boolean;
begin
  if Assigned(View) then begin
    // 实现了 IView 接口时，以 HeightSize 或 WidthSize 为依据
    if AOrientation = TOrientation.Horizontal then
      Result := (View.GetHeightSize <> TViewSize.WrapContent)
    else
      Result := (View.GetWidthSize <> TViewSize.WrapContent)
  end else if (Align = TAlignLayout.None) or (Align = TAlignLayout.Scale) then
    // Align 不会调整大小
    Result := False
  else if AOrientation = TOrientation.Horizontal then
    Result := Align in [TAlignLayout.Left, TAlignLayout.Right,
      TAlignLayout.MostLeft, TAlignLayout.MostRight,
      TAlignLayout.Client, TAlignLayout.Contents,
      TAlignLayout.HorzCenter, TAlignLayout.Vertical, TAlignLayout.Fit,
      TAlignLayout.FitLeft, TAlignLayout.FitRight]
  else
    Result := Align in [TAlignLayout.Top, TAlignLayout.Bottom,
      TAlignLayout.MostTop, TAlignLayout.MostBottom,
      TAlignLayout.Client, TAlignLayout.Contents,
      TAlignLayout.VertCenter, TAlignLayout.Horizontal, TAlignLayout.Fit,
      TAlignLayout.FitLeft, TAlignLayout.FitRight];
end;

function TViewGroup.RemoveView(View: TView): Integer;
begin
  Result := Controls.Remove(View);
end;

{ TLinearLayout }

procedure TLinearLayout.DoOrientation;
begin
  Realign;
end;

procedure TLinearLayout.DoRealign;
var
  CtrlCount, LastAutoSizeIndex: Integer;

  // 检查后面的控件，是否有自动大小的, 有的时候返回 -1
  function CheckAutoPos(const Index: Integer; const WeightSum: Single): Single;
  var
    I: Integer;
    View: IView;
    Control: TControl;
    AO: TOrientation;
    LAutoSize: Boolean;
  begin
    if Index <= LastAutoSizeIndex then begin
      Result := -1;
      Exit;
    end;
    Result := 0;

    if FOrientation = TOrientation.Horizontal then
      AO := TOrientation.Vertical
    else
      AO := TOrientation.Horizontal;

    for I := Index to CtrlCount do begin
      Control := Controls[I];
      {$IFDEF MSWINDOWS}
      if IsDesignerControl(Control) then Continue;
      {$ENDIF}
      if not Control.Visible then Continue;
      View := nil;
      Supports(Control, IView, View);

      // 如果启用了重力，则无视会影响组件大小的 Align 设置
      if WeightSum > 0 then begin
        LAutoSize := Assigned(View) and (IsAutoSize(View, Control.Align, AO));
      end else
        LAutoSize := IsAutoSize(View, Control.Align, AO);

      if LAutoSize then begin
        LastAutoSizeIndex := I;
        Result := -1;
        Break;
      end else begin
        if Orientation = TOrientation.Horizontal then
          Result := Result + Control.Margins.Left + Control.Width + Control.Margins.Right
        else
          Result := Result + Control.Margins.Top + Control.Height + Control.Margins.Bottom;
      end;
    end;
  end;

var
  I: Integer;
  WeightSum: Single;
  CurPos: TPointF;
  W, H, Fix: Single;
  VL, VT, VW, VH, AW, AH, VRB: Single;
  Control: TControl;
  View: IView;
  SaveAdjustViewBounds, LAutoSize, LAutoPos, LAllowAutoPos: Boolean;
begin
  if FDisableAlign then
    Exit;
  if csLoading in ComponentState then
    Exit;
  FDisableAlign := True;
  WeightSum := GetWeightSum(Fix);
  LastAutoSizeIndex := -1;
  CtrlCount := ControlsCount - 1;

  CurPos := PointF(Padding.Left, Padding.Top);
  //LogD(Format('CurPos.X: %.2f, CurPos.Y: %.2f', [CurPos.X, CurPos.Y]));
  W := Self.Width - CurPos.X - Padding.Right;
  H := Self.Height - CurPos.Y - Padding.Bottom;
  if (WeightSum = 0) and (CheckAutoPos(0, WeightSum) >= 0) then begin
    if Orientation = TOrientation.Horizontal then begin
      if FGravity in [TLayoutGravity.CenterHorizontal, TLayoutGravity.CenterHBottom, TLayoutGravity.Center] then
        CurPos.X := (W - Fix) / 2 + Padding.Left
      else if FGravity in [TLayoutGravity.RightTop, TLayoutGravity.RightBottom, TLayoutGravity.CenterVRight] then
        CurPos.X := W - Fix + Padding.Left;
    end else begin
      if FGravity in [TLayoutGravity.CenterVertical, TLayoutGravity.CenterVRight, TLayoutGravity.Center] then
        CurPos.Y := (H - Fix) / 2 + Padding.Top
      else if FGravity in [TLayoutGravity.LeftBottom, TLayoutGravity.RightBottom, TLayoutGravity.CenterHBottom] then
        CurPos.Y := H - Fix + Padding.Top;
    end;
  end;

  if (W > 0) and (H > 0) then begin
    LAllowAutoPos := True;
    VRB := 0;
    SaveAdjustViewBounds := False;
    for I := 0 to CtrlCount do begin
      Control := Controls[I];
      {$IFDEF MSWINDOWS}
      if (csDesigning in ComponentState)
        and Supports(Control, IDesignerControl) then Continue;
      {$ENDIF}
      if not Control.Visible then Continue;

      View := nil;
      if (Supports(Control, IView, View)) then
        SaveAdjustViewBounds := View.GetAdjustViewBounds;

      LAutoSize := IsAutoSize(View, Control.Align, FOrientation);
      if LAutoSize and LAllowAutoPos then begin
        VRB := CheckAutoPos(I + 1, WeightSum);
        LAutoPos := VRB >= 0;
        if LAutoPos then
          LAllowAutoPos := False;
      end else
        LAutoPos := False;

      if (csDesigning in ComponentState) then begin
        {$IFDEF MSWINDOWS}
        if IsDesignerControl(Control) then begin
          Dec(CtrlCount);
          Continue;
        end;
        {$ENDIF}
      end;

      if Orientation = TOrientation.Horizontal then begin  // 横排
        VL := CurPos.X + Control.Margins.Left;
        if Assigned(View) and (WeightSum > 0) and (View.GetWeight > 0) then begin
          VW := (W - Fix) / WeightSum * View.GetWeight - Control.Margins.Left - Control.Margins.Right;
        end else
          VW := Control.Width;

        if LAutoSize then begin
          VT := CurPos.Y + Control.Margins.Top;
          // 处理高度
          if Assigned(View) and (View.GetHeightSize = TViewSize.WrapContent) then begin
            VH := Control.Height;
            // 非自动高度时，以重力设置来调整位置
            case FGravity of
              TLayoutGravity.LeftTop, TLayoutGravity.RightTop:
                VT := CurPos.Y + Control.Margins.Top;
              TLayoutGravity.LeftBottom, TLayoutGravity.RightBottom, TLayoutGravity.CenterHBottom:
                VT := H - VH - Control.Margins.Bottom + Padding.Top;
              TLayoutGravity.CenterVertical, TLayoutGravity.Center, TLayoutGravity.CenterVRight:
                VT := (H - (VH + Control.Margins.Top + Control.Margins.Bottom)) / 2 + Padding.Top + Control.Margins.Top;
            end;
          end else
            VH := H - Control.Margins.Bottom - VT;
        end else begin
          VH := Control.Height;
          case FGravity of
            TLayoutGravity.LeftTop, TLayoutGravity.RightTop:
              VT := CurPos.Y + Control.Margins.Top;
            TLayoutGravity.LeftBottom, TLayoutGravity.RightBottom, TLayoutGravity.CenterHBottom:
              VT := H - VH - Control.Margins.Bottom + Padding.Top;
            TLayoutGravity.CenterVertical, TLayoutGravity.Center, TLayoutGravity.CenterVRight:
              VT := (H - (VH + Control.Margins.Top + Control.Margins.Bottom)) / 2 + Padding.Top + Control.Margins.Top;
          else
            VT := Control.Position.Y;
          end;
        end;

        if Assigned(View) and (SaveAdjustViewBounds) then begin
          if (View.GetHeightSize = TViewSize.WrapContent) and (View.GetMaxHeight > 0) and (VH > View.GetMaxHeight) then
            VH := View.GetMaxHeight;
          if (View.GetMinHeight > 0) and (VH < View.GetMinHeight) then
            VH := View.GetMinHeight;

          if (LAutoPos) and (View.GetWidthSize = TViewSize.FillParent) then
            VW := (W - VRB) - VL - Control.Margins.Right + Padding.Left;
          AW := VW;
          if (View.GetMaxWidth > 0) and (AW > View.GetMaxWidth) then
            AW := View.GetMaxWidth;
          if (View.GetMinWidth > 0) and (AW < View.GetMinWidth) then
            AW := View.GetMinWidth;
          if AW <> VW then
            VW := AW;
        end else if LAutoSize then begin // and (FGravity = TLayoutGravity.None) then begin
          // Align AutoSize
          AH := VH;
          if LAutoPos then
            AW := (W - VRB) - VL - Control.Margins.Right + Padding.Left
          else
            AW := Control.Width;
          VH := Control.Height;  // 高度先默认为原始高度

          case Control.Align of
            TAlignLayout.Top, TAlignLayout.MostTop: 
              begin
                VW := AW;
              end;
            TAlignLayout.Left, TAlignLayout.MostLeft: 
              begin
                VH := AH;
              end;
            TAlignLayout.Right, TAlignLayout.MostRight:
              begin
                VW := AW;
              end;
            TAlignLayout.Bottom, TAlignLayout.MostBottom: 
              begin
                VT := H - VH - Control.Margins.Bottom;
                VW := AW;
              end;
            TAlignLayout.Client, TAlignLayout.Contents:
              begin
                VH := AH;
                VW := AW;
              end;
            TAlignLayout.Center, TAlignLayout.Fit, TAlignLayout.FitLeft, TAlignLayout.FitRight: 
              begin
                VT := (H - VH) / 2;
                if LAutoPos then
                  VL := VL + (AW - VW) / 2;
              end;
            TAlignLayout.VertCenter:
              begin                 
                VT := (H - VH) / 2;
                VW := AW;
              end;
            TAlignLayout.HorzCenter: 
              begin
                if LAutoPos then
                  VL := VL + (AW - VW) / 2;  
              end;
            TAlignLayout.Horizontal:
              VW := AW;
            TAlignLayout.Vertical: 
              VH := AH;
          end;
          
        end;

        if Assigned(View) and (View.GetWeight > 0) then begin
          Fix := Fix + VW + Control.Margins.Left + Control.Margins.Right;
          WeightSum := WeightSum - View.GetWeight;
        end;
        CurPos.X := VL + VW + Control.Margins.Right;

      end else begin // 竖排
        VT := CurPos.Y + Control.Margins.Top;
        if Assigned(View) and (WeightSum > 0) and (View.GetWeight > 0) then
          VH := (H - Fix) / WeightSum * View.GetWeight - Control.Margins.Top - Control.Margins.Bottom
        else
          VH := Control.Height;

        if LAutoSize then begin
          VL := CurPos.X + Control.Margins.Left;
          // 处理宽度
          if Assigned(View) and (View.GetWidthSize = TViewSize.WrapContent) then begin
            VW := Control.Height;
            // 非自动高度时，以重力设置来调整位置
            case FGravity of
              TLayoutGravity.LeftTop, TLayoutGravity.LeftBottom:
                VL := CurPos.X + Control.Margins.Left;
              TLayoutGravity.RightTop, TLayoutGravity.RightBottom, TLayoutGravity.CenterVRight:
                VL := W - VW - Control.Margins.Right + Padding.Left;
              TLayoutGravity.CenterHBottom, TLayoutGravity.Center:
                VL := (W - (VW + Control.Margins.Left + Control.Margins.Right)) / 2 + Padding.Left + Control.Margins.Left;
            end;
          end else
            VW := W - Control.Margins.Right; // - VL;
        end else begin
          VW := Control.Width;
          case FGravity of
            TLayoutGravity.LeftTop, TLayoutGravity.LeftBottom:
              VL := CurPos.X + Control.Margins.Left;
            TLayoutGravity.RightTop, TLayoutGravity.RightBottom, TLayoutGravity.CenterVRight:
              VL := W - VW - Control.Margins.Right + Padding.Left;
            TLayoutGravity.CenterHBottom, TLayoutGravity.Center:
              VL := (W - (VW + Control.Margins.Left + Control.Margins.Right)) / 2 + Padding.Left + Control.Margins.Left;
          else
            VL := Control.Position.X;
          end;
        end;

        if Assigned(View) and (SaveAdjustViewBounds) then begin
          if (View.GetWidthSize = TViewSize.WrapContent) and (View.GetMaxWidth > 0) and (VW > View.GetMaxWidth) then
            VW := View.GetMaxWidth;
          if (View.GetMinWidth > 0) and (VW < View.GetMinWidth) then
            VW := View.GetMinWidth;
          if (LAutoPos) and (View.GetHeightSize = TViewSize.FillParent) then
            VH := (H - VRB) - VT - Control.Margins.Bottom + Padding.Top;
          AH := VH;
          if (View.GetMaxHeight > 0) and (AH > View.GetMaxHeight) then
            AH := View.GetMaxHeight;
          if (View.GetMinHeight > 0) and (AH < View.GetMinHeight) then
            AH := View.GetMinHeight;
          if AH <> VH then
            VH := AH;
        end else if LAutoSize then begin // and (FGravity = TLayoutGravity.None) then begin
          // Align AutoSize
          AW := VW;
          if LAutoPos then
            AH := (H - VRB) - VT - Control.Margins.Bottom + Padding.Top
          else
            AH := Control.Height;
          VW := Control.Width;

          case Control.Align of
            TAlignLayout.Top, TAlignLayout.MostTop:
              begin
                VW := AW;
              end;
            TAlignLayout.Left, TAlignLayout.MostLeft:
              begin
                VH := AH;
              end;
            TAlignLayout.Right, TAlignLayout.MostRight:
              begin
                VH := AH;
                VL := W - VW - Control.Margins.Right;
              end;
            TAlignLayout.Bottom, TAlignLayout.MostBottom:
              begin
                VW := AW;
                if LAutoPos then
                  VT := H - VH - Control.Margins.Bottom;
              end;
            TAlignLayout.Client, TAlignLayout.Contents:
              begin
                VH := AH;
                VW := AW;
              end;
            TAlignLayout.Center, TAlignLayout.Fit, TAlignLayout.FitLeft, TAlignLayout.FitRight:
              begin
                VL := (W - VW) / 2;
                if LAutoPos then
                  VT := VT + (AH - VH) / 2;
              end;
            TAlignLayout.VertCenter:
              begin
                if LAutoPos then
                  VT := VT + (H - VH) / 2;
              end;
            TAlignLayout.HorzCenter:
              begin
                VL := (AW - VW) / 2;
              end;
            TAlignLayout.Horizontal:
              VW := AW;
            TAlignLayout.Vertical:
              VH := AH;
          end;

        end;
        
        if Assigned(View) and (View.GetWeight > 0) then begin
          Fix := Fix + VH + Control.Margins.Top + Control.Margins.Bottom;
          WeightSum := WeightSum - View.GetWeight;
        end;
        CurPos.Y := VT + VH + Control.Margins.Bottom;

      end;

      if Assigned(View) then begin
        View.SetAdjustViewBounds(False);
        Control.SetBounds(VL, VT, VW, VH);
        View.SetAdjustViewBounds(SaveAdjustViewBounds);
      end else
        Control.SetBounds(VL, VT, VW, VH);

    end;
  end;
  FDisableAlign := False;
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
    {$IFDEF MSWINDOWS}
    if IsDesignerControl(Control) then Continue;
    {$ENDIF}
    if (not Control.Visible) then Continue;
    // TDesignRectangle
    if (Supports(Control, IView, View)) and (View.GetWeight > 0) then
      Result := Result + View.GetWeight
    else begin
      if Orientation = TOrientation.Horizontal then       
        FixSize := FixSize + Control.Width + Control.Margins.Left + Control.Margins.Right
      else
        FixSize := FixSize + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
    end;
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
  FViewList := TList.Create;
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
  List: TList;

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

  procedure DoAlign(List: TList; AAlign: TAlignLayout);
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

  procedure DoGetList(const List: TList);
  var
    View: IView;
    Control: TControl;
    I: Integer;
  begin
    List.Clear;
    for I := 0 to ControlsCount - 1 do begin
      Control := Controls[I];
      {$IFDEF MSWINDOWS}
      if (csDesigning in ComponentState)
        and Supports(Control, IDesignerControl) then Continue;
      {$ENDIF}
      if not Control.Visible then Continue;

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
  List := TList.Create;
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
  List: TList;
  W, H: Single;
  I: Integer;
  CurPos: TPointF;
  VL, VT, VW, VH: Single;
  View: TControl;
  LView: IView;
  SaveAdjustViewBounds: Boolean;
begin
  if FDisableAlign or (not Assigned(FViewList)) then
    Exit;
  if csLoading in ComponentState then
    Exit;
  FDisableAlign := True;
  CurPos := PointF(Padding.Left, Padding.Top);
  W := Self.Width - CurPos.X - Padding.Right;
  H := Self.Height - CurPos.Y - Padding.Bottom;

  if (W > 0) and (H > 0) then begin
    FViewList.Clear;
    DoAlignControl(CurPos.X, CurPos.Y, FSize.Width, FSize.Height);
    SaveAdjustViewBounds := False;
    List := TList.Create;
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
        LView.SetAdjustViewBounds(False);
        View.SetBounds(VL, VT, VW, VH);
        LView.SetAdjustViewBounds(SaveAdjustViewBounds);
      end;
    finally
      List.Free;
    end;
  end;
  FDisableAlign := False;
end;

function TRelativeLayout.GetXY(const StackList: TList; const Control: TControl;
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

  W := Control.Width + Control.Margins.Left + Control.Margins.Right;
  H := Control.Height + Control.Margins.Top + Control.Margins.Bottom;

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

    AutoW := View.WidthSize <> TViewSize.WrapContent;
    AutoH := View.HeightSize <> TViewSize.WrapContent;

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

{ TTextSettings }

function TTextSettings.CalcTextObjectSize(const MaxWidth, SceneScale: Single;
  const Margins: TBounds; var Size: TSizeF): Boolean;
const
  FakeText = 'P|y'; // Do not localize

  function RoundToScale(const Value, Scale: Single): Single;
  begin
    if Scale > 0 then
      Result := Ceil(Value * Scale) / Scale
    else
      Result := Ceil(Value);
  end;

var
  Layout: TTextLayout;
  LText: string;
  LMaxWidth: Single;
begin
  Result := False;
  if (SceneScale >= 0) then
  begin
    LMaxWidth := MaxWidth - Margins.Left - Margins.Right;
    Layout := TTextLayoutManager.DefaultTextLayout.Create;
    try
      if FPrefixStyle = TPrefixStyle.HidePrefix then
        LText := DelAmp(FText)
      else
        LText := FText;
      Layout.BeginUpdate;
      if LText.IsEmpty then
        Layout.Text := FakeText
      else
        Layout.Text := LText;

      Layout.Font := FFont;
      if FWordWrap and (LMaxWidth > 1) then
        Layout.MaxSize := TPointF.Create(LMaxWidth, Layout.MaxSize.Y);
      Layout.WordWrap := FWordWrap;
      Layout.Trimming := FTrimming;
      Layout.VerticalAlign := TTextAlign.Leading;
      Layout.HorizontalAlign := TTextAlign.Leading;
      Layout.EndUpdate;

      if LText.IsEmpty then
        Size.Width := 0
      else
        Size.Width := RoundToScale(Layout.Width + Layout.TextRect.Left * 2 + Layout.Font.Size / 3, SceneScale);
      Size.Width := Size.Width + Margins.Left + Margins.Right;
      Size.Height := RoundToScale(Layout.Height, SceneScale) + Margins.Top + Margins.Bottom;
      Result := True;
    finally
      Layout.Free;
    end;
  end;
end;

procedure TTextSettings.Change;
begin
  DoChange();
end;

constructor TTextSettings.Create(AOwner: TComponent);
var
  DefaultValueService: IInterface;
  TrimmingDefault: TValue;
begin
  if AOwner is TControl then
    FOwner := TControl(AOwner)
  else FOwner := nil;
  FColor := TTextColor.Create();
  FColor.OnChanged := DoColorChanged;
  FFont := TFont.Create;
  FFont.OnChanged := DoFontChanged;
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

destructor TTextSettings.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FColor);
  inherited;
end;

procedure TTextSettings.DoChange;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
  FIsSizeChange := False;
  FIsColorChange := False;
end;

procedure TTextSettings.DoColorChanged(Sender: TObject);
begin
  FIsColorChange := True;
  DoChange;
end;

procedure TTextSettings.DoFontChanged(Sender: TObject);
begin
  FIsSizeChange := True;
  DoChange;
end;

procedure TTextSettings.DoTextChanged;
begin
  if FAutoSize then FIsSizeChange := True;
  FIsEffectsChange := True;
  try
    DoChange;
  finally
    FIsEffectsChange := False;
  end;
end;

procedure TTextSettings.Draw(const Canvas: TCanvas; const AText: string;
  const R: TRectF; const Opacity: Single; State: TViewState);
var
  V, H: TTextAlign;
begin
  if AText <> '' then begin
    V := TTextAlign.Leading;
    H := TTextAlign.Leading;
    case FGravity of
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
    FillText(Canvas, R, AText, FWordWrap, Opacity, FillTextFlags, H, V, State);
  end;
end;

procedure TTextSettings.Draw(const Canvas: TCanvas; const R: TRectF;
  const Opacity: Single; State: TViewState);
var
  TextStr: string;
begin
  if FPrefixStyle = TPrefixStyle.HidePrefix then
    TextStr := DelAmp(FText)
  else
    TextStr := FText;
  if TextStr <> '' then
    Draw(Canvas, TextStr, R, Opacity, State);
end;

procedure TTextSettings.FillText(const Canvas: TCanvas; const ARect: TRectF;
  const AText: string; const WordWrap: Boolean; const AOpacity: Single;
  const Flags: TFillTextFlags; const ATextAlign, AVTextAlign: TTextAlign;
  State: TViewState);
var
  Layout: TTextLayout;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create(Canvas);
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.Text := AText;
    Layout.WordWrap := WordWrap;
    Layout.Opacity := AOpacity;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := FFont;
    Layout.Color := FColor.GetStateColor(State);
    Layout.Trimming := FTrimming;
    Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
    Layout.EndUpdate;
    Layout.RenderLayout(Canvas);
  finally
    FreeAndNil(Layout);
  end;
end;

function TTextSettings.GetFillTextFlags: TFillTextFlags;
begin
  if Assigned(FOwner) then
    Result := TView(FOwner).FillTextFlags
  else Result := [];
end;

function TTextSettings.GetGravity: TLayoutGravity;
begin
  Result := FGravity;
end;

function TTextSettings.GetHorzAlign: TTextAlign;
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

function TTextSettings.GetVertAlign: TTextAlign;
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

function TTextSettings.GetWordWrap: Boolean;
begin
  Result := FWordWrap;
end;

procedure TTextSettings.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then begin
    FAutoSize := Value;
    if ([csLoading, csDesigning] * FOwner.ComponentState = [csDesigning]) and FAutoSize then
      FWordWrap := False;
    if FAutoSize then FIsSizeChange := True;
    DoChange;
  end;
end;

procedure TTextSettings.SetColor(const Value: TViewColor);
begin
  FColor.Assign(Value);
end;

procedure TTextSettings.SetFont(const Value: TFont);
begin
  if (FFont = nil) or (Value = nil) then Exit;
  if not FFont.Equals(Value) then
    FFont.Assign(Value);
end;

procedure TTextSettings.SetGravity(const Value: TLayoutGravity);
begin
  if FGravity <> Value then begin
    FGravity := Value;
    DoChange;
  end;
end;

procedure TTextSettings.SetHorzAlign(const Value: TTextAlign);
begin
  SetHorzVertValue(Value, VertAlign);
end;

procedure TTextSettings.SetHorzVertValue(const H, V: TTextAlign);
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

procedure TTextSettings.SetPrefixStyle(const Value: TPrefixStyle);
begin
  if FPrefixStyle <> Value then begin
    FPrefixStyle := Value;
    if FAutoSize then FIsSizeChange := True;
    DoChange;
  end;
end;

procedure TTextSettings.SetText(const Value: string);
begin
  if FText <> Value then begin
    FText := Value;
    if FAutoSize then FIsSizeChange := True;
    DoTextChanged;
  end;
end;

procedure TTextSettings.SetTrimming(const Value: TTextTrimming);
begin
  if FTrimming <> Value then begin
    FTrimming := Value;
    if FAutoSize then FIsSizeChange := True;
    DoChange;
  end;
end;

procedure TTextSettings.SetVertAlign(const Value: TTextAlign);
begin
  SetHorzVertValue(HorzAlign, Value);
end;

procedure TTextSettings.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    if FAutoSize then FIsSizeChange := True;
    DoChange;
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
    FBorder.Assign(TDrawableBorder(Source).FBorder);
  end;
  inherited Assign(Source);
end;

constructor TDrawableBorder.Create(View: IView; const ADefaultKind: TViewBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  inherited Create(View, ADefaultKind, ADefaultColor);
  FBorder := TViewBorder.Create;
  FBorder.OnChanged := DoChange;
end;

destructor TDrawableBorder.Destroy;
begin
  FBorder.Free;
  inherited Destroy;
end;

procedure TDrawableBorder.DoDrawed(Canvas: TCanvas; var R: TRectF; AState: TViewState);
var
  TH: Single;
begin
  if Assigned(FBorder) and (FBorder.FStyle <> TViewBorderStyle.None) and (FBorder.Width > 0) then begin
    FBorder.Brush.Color :=  FBorder.Color.GetStateColor(AState);
    case FBorder.FStyle of
      TViewBorderStyle.RectBorder:
        Canvas.DrawRect(R, XRadius, YRadius, AllCorners, FView.Opacity, FBorder.Brush);
      TViewBorderStyle.LineBottom:
        begin
          Canvas.DrawLine(R.BottomRight, PointF(R.Left, R.Bottom), FView.Opacity, FBorder.Brush);
          TH := Min(6, Min(FBorder.Width * 4, R.Height / 4));
          Canvas.DrawLine(PointF(R.Left, R.Bottom - TH), PointF(R.Left, R.Bottom), FView.Opacity, FBorder.Brush);
          Canvas.DrawLine(PointF(R.Right, R.Bottom - TH), R.BottomRight, FView.Opacity, FBorder.Brush);
        end;
      TViewBorderStyle.LineSimple:
        Canvas.DrawLine(R.BottomRight, PointF(R.Left, R.Bottom), FView.Opacity, FBorder.Brush);
    end;
  end;
end;

function TDrawableBorder.GetBorder: TViewBorder;
begin
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
    FBrush.Assign(TViewBorder(Source).FBrush);
    FOnChanged := SaveChange;
    FColor.OnChanged := SaveChange;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end else
    inherited;
end;

constructor TViewBorder.Create;
begin
  FBrush := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Null);
  FColor := TViewColor.Create();
  FStyle := TViewBorderStyle.None;
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

function TViewBorder.GetCap: TStrokeCap;
begin
  Result := FBrush.Cap;
end;

function TViewBorder.GetDash: TStrokeDash;
begin
  Result := FBrush.Dash;
end;

function TViewBorder.GetJoin: TStrokeJoin;
begin
  Result := FBrush.Join;
end;

function TViewBorder.GetWidth: Single;
begin
  Result := FBrush.Thickness;
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

procedure TViewBorder.SetJoin(const Value: TStrokeJoin);
begin
  if FBrush.Join <> Value then begin
    FBrush.Join := Value;
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

function TViewBorder.WidthStored: Boolean;
begin
  Result := Width <> 1;
end;

{ TViewBrush }

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

destructor TViewBrush.Destroy;
begin
  inherited Destroy;
end;

function TViewBrush.GetBitmap: TPatch9Bitmap;
begin
  if inherited Bitmap <> nil then
    Result := inherited Bitmap as TPatch9Bitmap
  else
    Result := nil;
end;

function TViewBrush.GetKind: TViewBrushKind;
begin
  Result := TViewBrushKind(inherited Kind);
end;

function TViewBrush.IsKindStored: Boolean;
begin
  Result := inherited Kind <> DefaultKind;
end;

function TViewBrush.IsPatch9BitmapStored: Boolean;
begin
  Result := Kind in [TViewBrushKind.Bitmap, TViewBrushKind.Patch9Bitmap];
end;

procedure TViewBrush.SetBitmap(const Value: TPatch9Bitmap);
begin
  inherited Bitmap.Assign(Value);
end;

procedure TViewBrush.SetKind(const Value: TViewBrushKind);
begin
  inherited Kind := TBrushKind(Value);
end;

{ TPatch9Bitmap }

procedure TPatch9Bitmap.Assign(Source: TPersistent);
begin
  if Source is TPatch9Bitmap then begin
    WrapMode := TPatch9Bitmap(Source).WrapMode;
    FBounds.Assign(TPatch9Bitmap(Source).FBounds);
    Bitmap.Assign(TPatch9Bitmap(Source).Bitmap);
    DoChanged;
  end else
    inherited;
end;

constructor TPatch9Bitmap.Create;
begin
  inherited Create;
  FBounds := TBounds.Create(RectF(0, 0, 0, 0));
end;

destructor TPatch9Bitmap.Destroy;
begin
  FBounds.Free;
  inherited;
end;

procedure TPatch9Bitmap.SetBounds(const Value: TBounds);
begin
  FBounds.Assign(Value);
end;

end.
