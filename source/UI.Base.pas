unit UI.Base;

interface

{$SCOPEDENUMS ON}

uses
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
  FMX.TextLayout, FMX.Objects, System.Rtti,
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

  TViewBrushBitmap = class(TBrushBitmap);
  TViewBrush = class(TBrush);

  /// <summary>
  /// 可绘制对象
  /// </summary>
  TDrawable = class(TPersistent)
  private
    [Weak] FView: IView;
    FDefaultKind: TBrushKind;
    FDefaultColor: TAlphaColor;
    FOnChanged: TNotifyEvent;

    FDefault: TViewBrush;
    FPressed: TViewBrush;
    FFocused: TViewBrush;
    FHovered: TViewBrush;
    FSelected: TViewBrush;
    FChecked: TViewBrush;
    FEnabled: TViewBrush;
    FActivated: TViewBrush;

    FPadding: TBounds;
    FXRadius, FYRadius: Single;

    function GetStateBrush(const State: TViewState): TViewBrush; overload;
    procedure GetStateBrush(const State: TViewState; var V: TViewBrush); overload;
    procedure SetStateBrush(const State: TViewState; const V: TViewBrush);
    function GetValue(const Index: Integer): TViewBrush;
    procedure SetValue(const Index: Integer; const Value: TViewBrush);
    function GetBrush(const State: TViewState; AutoCreate: Boolean): TViewBrush;
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    procedure SetPadding(const Value: TBounds);
    function GetPaddings: string;
    procedure SetPaddings(const Value: string);
  protected
    procedure CreateBrush(var Value: TViewBrush);
    procedure DoChange(Sender: TObject);
  public
    constructor Create(View: IView); overload;
    constructor Create(View: IView; const ADefaultKind: TBrushKind;
      const ADefaultColor: TAlphaColor); overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Draw(const Canvas: TCanvas);
    procedure SetRadius(const X, Y: Single);

    procedure SetDrawable(const Value: TDrawable); overload;
    procedure SetBrush(State: TViewState; const Value: TBrush); overload;
    procedure SetColor(State: TViewState; const Value: TAlphaColor); overload;
    procedure SetGradient(State: TViewState; const Value: TGradient); overload;
    procedure SetBitmap(State: TViewState; const Value: TBitmap); overload;
    procedure SetBitmap(State: TViewState; const Value: TBrushBitmap); overload;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    // 边框圆角
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property Padding: TBounds read FPadding write SetPadding;
    property Paddings: string read GetPaddings write SetPaddings;

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
    function GetHeightSize: TViewSize;
    function GetWidthSize: TViewSize;
    function GetViewState: TViewStates;
    function GetOrientation: TOrientation;
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
    FColor: TAlphaColor;
    FFont: TFont;
    FPrefixStyle: TPrefixStyle;
    FGravity: TLayoutGravity;
    FTrimming: TTextTrimming;
    FWordWrap: Boolean;
    FText: string;
    FAutoSize: Boolean;
    FIsSizeChange: Boolean;
    FIsEffectsChange: Boolean;
    function GetGravity: TLayoutGravity;
    function GetWordWrap: Boolean;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetFont(const Value: TFont);
    procedure SetGravity(const Value: TLayoutGravity);
    procedure SetPrefixStyle(const Value: TPrefixStyle);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetAutoSize(const Value: Boolean);
    function GetFillTextFlags: TFillTextFlags;
  protected
    procedure DoChange; virtual;
    procedure DoTextChanged;
    procedure DoFontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function CalcTextObjectSize(const MaxWidth, SceneScale: Single;
      const Margins: TBounds; var Size: TSizeF): Boolean;
    procedure FillText(const Canvas: TCanvas; const ARect: TRectF; const AText: string; const WordWrap: Boolean; const AOpacity: Single;
      const Flags: TFillTextFlags; const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.Center);
    procedure Draw(const Canvas: TCanvas; const R: TRectF; const Opacity: Single);
    property IsSizeChange: Boolean read FIsSizeChange;
    property IsEffectsChange: Boolean read FIsEffectsChange;
    property Text: string read FText write SetText;
    property FillTextFlags: TFillTextFlags read GetFillTextFlags;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnTextChanged: TNotifyEvent read FOnTextChanged write FOnTextChanged;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Color: TAlphaColor read FColor write SetColor default TAlphaColorRec.Black;
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
    function GetComponentState: TComponentState;
    function GetOpacity: Single;
    function GetParentControl: TControl;
    function GetPosition: TPosition;
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
    class function CanRePaintBk(const View: IView; State: TViewState): Boolean; virtual;
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

    property ParentView: IViewGroup read GetParentView;
    property Orientation: TOrientation read GetOrientation write SetOrientation;
    property ViewState: TViewStates read GetViewStates;
  published
    property Align;
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
    function IsAutoSize(View: IView;  Align: TAlignLayout): Boolean;
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

procedure Register;

implementation

resourcestring
  SInvViewValue = '无效的视图状态值: %d';
  SNotAllowSelf = '不允许设定为自己';
  SMustSameParent = '必须指定一个与当前组件所属视图中的同级兄弟组件';
  SLocateFailed = '存在循环引用';
  SRefOutLimitMax = '组件引用层级超过上限值: 256';

procedure Register;
begin
  RegisterComponents('YxdFMX', [TView, TLinearLayout, TRelativeLayout]);
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TLayoutGravity),
    ['None', 'LeftTop', 'LeftBottom', 'RightTop', 'RightBottom',
    'CenterVertical', 'CenterHorizontal', 'CenterHBottom', 'CenterVRight', 'Center']);
  AddEnumElementAliases(TypeInfo(TViewSize),
    ['WrapContent', 'FillParent']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TLayoutGravity));
  RemoveEnumElementAliases(TypeInfo(TViewSize));
end;

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

{ TDrawable }

procedure TDrawable.Assign(Source: TPersistent);

  procedure AssignItem(State: TViewState; const Src: TDrawable);
  var V: TViewBrush;
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
  if Source is TDrawable then begin
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

constructor TDrawable.Create(View: IView);
begin
  Create(View, TBrushKind.None, TAlphaColors.White);
end;

constructor TDrawable.Create(View: IView; const ADefaultKind: TBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  FView := View;
  FPadding := TBounds.Create(TRectF.Empty);
  FPadding.OnChange := DoChange;
  FDefaultKind := ADefaultKind;
  FDefaultColor := ADefaultColor;
  if Assigned(FView) and (csDesigning in FView.GetComponentState) then begin
    CreateBrush(FDefault);
    CreateBrush(FPressed);
    CreateBrush(FFocused);
    CreateBrush(FHovered);
    CreateBrush(FSelected);
    CreateBrush(FChecked);
    CreateBrush(FEnabled);
    CreateBrush(FActivated);
  end;
end;

procedure TDrawable.CreateBrush(var Value: TViewBrush);
begin
  if Assigned(Value) then
    FreeAndNil(Value);
  Value := TViewBrush.Create(FDefaultKind, FDefaultColor);
  Value.OnChanged := DoChange;
end;

destructor TDrawable.Destroy;
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
  FreeAndNil(FPadding);
  inherited;
end;

procedure TDrawable.DoChange(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Sender);
end;

function TDrawable.GetValue(const Index: Integer): TViewBrush;
begin
  Result := GetBrush(TViewState(Index), False);
end;

function TDrawable.GetBrush(const State: TViewState; AutoCreate: Boolean): TViewBrush;
begin
  GetStateBrush(State, Result);
  if (not Assigned(Result)) and
    (AutoCreate or (csLoading in FView.GetComponentState)) then
  begin
    CreateBrush(Result);
    SetStateBrush(State, Result);
  end;
end;

function TDrawable.GetPaddings: string;
begin
  Result := GetBoundsFloat(FPadding);
end;

function TDrawable.GetStateBrush(const State: TViewState): TViewBrush;
begin
  GetStateBrush(State, Result);
end;

procedure TDrawable.GetStateBrush(const State: TViewState; var V: TViewBrush);
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

procedure TDrawable.Draw(const Canvas: TCanvas);
var
  States: TViewStates;
  V: TViewBrush;
begin
  if not Assigned(FView) or (csDestroying in FView.GetComponentState) then Exit;
  States := FView.GetViewState;
  if States = [] then
    V := FDefault
  else begin
    if TViewState.Enabled in States then
      GetStateBrush(TViewState.Enabled, V)
    else if TViewState.Pressed in States then
      GetStateBrush(TViewState.Pressed, V)
    else if TViewState.Hovered in States then
      GetStateBrush(TViewState.Hovered, V)
    else if TViewState.Activated in States then
      GetStateBrush(TViewState.Activated, V)
    else if TViewState.Focused in States then
      GetStateBrush(TViewState.Focused, V)
    else if TViewState.Selected in States then
      GetStateBrush(TViewState.Selected, V)
    else if TViewState.Checked in States then
      GetStateBrush(TViewState.Checked, V)
    else
      V := FDefault;
  end;
  if V <> FDefault then begin
    if (not Assigned(V)) or (V.Kind = TBrushKind.None) or
      ((V.Color and $FF000000 = 0) and (V.Kind = TBrushKind.Solid)) then
      V := FDefault;
  end;
  if (not Assigned(V)) or (V.Kind = TBrushKind.None) or
      ((V.Color and $FF000000 = 0) and (V.Kind = TBrushKind.Solid)) then
    Exit;
  Canvas.FillRect(RectF(FPadding.Left, FPadding.Top,
    FView.GetWidth - FPadding.Right,
    FView.GetHeight - FPadding.Bottom),
    FXRadius, FYRadius,
    AllCorners, FView.GetOpacity, V);
end;

procedure TDrawable.SetDrawable(const Value: TDrawable);
begin
  Assign(Value);
end;

procedure TDrawable.SetColor(State: TViewState; const Value: TAlphaColor);
var V: TBrush;
begin
  V := GetBrush(State, True);
  V.Kind := TBrushKind.Solid;
  V.Color := Value;
end;

procedure TDrawable.SetGradient(State: TViewState; const Value: TGradient);
var V: TBrush;
begin
  V := GetBrush(State, True);
  V.Gradient.Assign(Value);
  V.Kind := TBrushKind.Gradient;
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

procedure TDrawable.SetRadius(const X, Y: Single);
begin
  FYRadius := Y;
  FXRadius := X;
  DoChange(Self);
end;

procedure TDrawable.SetBitmap(State: TViewState; const Value: TBrushBitmap);
var V: TBrush;
begin
  V := GetBrush(State, True);
  V.Bitmap.Assign(Value);
  V.Kind := TBrushKind.Bitmap;
end;

procedure TDrawable.SetBrush(State: TViewState; const Value: TBrush);
begin
  GetBrush(State, True).Assign(Value);
end;

procedure TDrawable.SetBitmap(State: TViewState; const Value: TBitmap);
var V: TBrush;
begin
  V := GetBrush(State, True);
  V.Bitmap.Bitmap.Assign(Value);
  V.Kind := TBrushKind.Bitmap;
end;

procedure TDrawable.SetStateBrush(const State: TViewState; const V: TViewBrush);
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

procedure TDrawable.SetValue(const Index: Integer; const Value: TViewBrush);
begin
  SetBrush(TViewState(Index), Value);
end;
procedure TDrawable.SetXRadius(const Value: Single);
begin
  if FXRadius <> Value then begin
    FXRadius := Value;
    DoChange(Self);
  end;
end;

procedure TDrawable.SetYRadius(const Value: Single);
begin
  if FYRadius <> Value then begin
    FYRadius := Value;
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

class function TView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
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
    FBackground := TDrawable.Create(Self);
    FBackground.OnChanged := DoBackgroundChanged;
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
end;

procedure TView.DecChildState(State: TViewState);
var
  I: Integer;
  View: IView;
begin
  if State = TViewState.None then Exit;
  if csDestroying in ComponentState then Exit;
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
  IncViewState(TViewState.Activated);
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
  if TMouseButton.mbLeft = Button then begin
    IncViewState(TViewState.Pressed);
    if CanRePaintBk(Self, TViewState.Pressed) then Repaint;
  end;
end;

procedure TView.DoMouseEnter;
begin
  inherited DoMouseEnter;
  IncViewState(TViewState.Hovered);
  if CanRePaintBk(Self, TViewState.Hovered) then Repaint;
end;

procedure TView.DoMouseLeave;
begin
  inherited DoMouseLeave;
  DecViewState(TViewState.Hovered);
  if CanRePaintBk(Self, TViewState.Hovered) then Repaint;
end;

procedure TView.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if TMouseButton.mbLeft = Button then begin
    DecViewState(TViewState.Pressed);
    if CanRePaintBk(Self, TViewState.Pressed) then Repaint;
  end;
end;

function TView.GetClickable: Boolean;
begin
  Result := HitTest;
end;

function TView.GetComponentState: TComponentState;
begin
  Result := ComponentState;
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

procedure TView.IncChildState(State: TViewState);
var
  I: Integer;
  View: IView;
begin
  if State = TViewState.None then Exit;
  if csDestroying in ComponentState then Exit;
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
  if not Assigned(FBackground) then begin
    FBackground := TDrawable.Create(Self);
    FBackground.OnChanged := DoBackgroundChanged;
  end;
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
  if (not Assigned(FBackground)) and (Assigned(Value)) then begin
    FBackground := TDrawable.Create(Self);
    FBackground.OnChanged := DoBackgroundChanged;
  end;
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

function TViewGroup.IsAutoSize(View: IView; Align: TAlignLayout): Boolean;
begin
  if FOrientation = TOrientation.Horizontal then
    Result := Assigned(View) and (View.GetHeightSize <> TViewSize.WrapContent)
  else
    Result := Assigned(View) and (View.GetWidthSize <> TViewSize.WrapContent);
  if not Result then begin
    if FOrientation = TOrientation.Horizontal then
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
  I: Integer;
  WeightSum: Single;
  CurPos: TPointF;
  W, H, Fix: Single;
  VL, VT, VW, VH, AW, AH: Single;
  Control: TControl;
  View: IView;
  SaveAdjustViewBounds: Boolean;
begin
  if FDisableAlign then
    Exit;
  if csLoading in ComponentState then
    Exit;
  FDisableAlign := True;
  WeightSum := GetWeightSum(Fix);

  CurPos := PointF(Padding.Left, Padding.Top);
  W := Self.Width - CurPos.X - Padding.Right;
  H := Self.Height - CurPos.Y - Padding.Bottom;
  if WeightSum = 0 then begin
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
    SaveAdjustViewBounds := False;
    for I := 0 to ControlsCount - 1 do begin
      Control := Controls[I];
      {$IFDEF MSWINDOWS}
      if (csDesigning in ComponentState)
        and Supports(Control, IDesignerControl) then Continue;
      {$ENDIF}
      if not Control.Visible then Continue;

      View := nil;
      if (Supports(Control, IView, View)) then
        SaveAdjustViewBounds := View.GetAdjustViewBounds;

      if Orientation = TOrientation.Horizontal then begin  // 横排
        VL := CurPos.X + Control.Margins.Left;
        if Assigned(View) and (WeightSum > 0) and (View.GetWeight > 0) then begin
          VW := (W - Fix) / WeightSum * View.GetWeight - Control.Margins.Left - Control.Margins.Right;
        end else
          VW := Control.Width;

        if IsAutoSize(View, Control.Align) then begin
          VT := CurPos.Y + Control.Margins.Top;
          VH := H - Control.Margins.Bottom;
        end else begin
          VH := Control.Height;
          case FGravity of
            TLayoutGravity.LeftTop, TLayoutGravity.RightTop:
              VT := CurPos.Y + Control.Margins.Top;
            TLayoutGravity.LeftBottom, TLayoutGravity.RightBottom, TLayoutGravity.CenterHBottom:
              VT := H - VH - Control.Margins.Bottom + Padding.Top;
            TLayoutGravity.CenterVertical, TLayoutGravity.Center, TLayoutGravity.CenterVRight:
              VT := (H - (VH + Control.Margins.Top + Control.Margins.Bottom)) / 2 + Padding.Top;
          else
            VT := Control.Position.Y;
          end;
        end;

        if Assigned(View) and (SaveAdjustViewBounds) then begin
          if (View.GetHeightSize = TViewSize.WrapContent) and (View.GetMaxHeight > 0) and (VH > View.GetMaxHeight) then
            VH := View.GetMaxHeight;
          if (View.GetMinHeight > 0) and (VH < View.GetMinHeight) then
            VH := View.GetMinHeight;
          AW := VW;
          if (View.GetMaxWidth > 0) and (AW > View.GetMaxWidth) then
            AW := View.GetMaxWidth;
          if (View.GetMinWidth > 0) and (AW < View.GetMinWidth) then
            AW := View.GetMinWidth;
          if AW <> VW then
            VW := AW;
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

        if IsAutoSize(View, Control.Align) then begin
          VL := CurPos.X + Control.Margins.Left;
          VW := W - Control.Margins.Right;
        end else begin
          VW := Control.Width;
          case FGravity of
            TLayoutGravity.LeftTop, TLayoutGravity.LeftBottom:
              VL := CurPos.X + Control.Margins.Left;
            TLayoutGravity.RightTop, TLayoutGravity.RightBottom, TLayoutGravity.CenterVRight:
              VL := W - VW - Control.Margins.Right + Padding.Left;
            TLayoutGravity.CenterHBottom, TLayoutGravity.Center:
              VL := (W - (VW + Control.Margins.Left + Control.Margins.Right)) / 2 + Padding.Left;
          else
            VL := Control.Position.X;
          end;
        end;

        if Assigned(View) and (SaveAdjustViewBounds) then begin
          if (View.GetWidthSize = TViewSize.WrapContent) and (View.GetMaxWidth > 0) and (VW > View.GetMaxWidth) then
            VW := View.GetMaxWidth;
          if (View.GetMinWidth > 0) and (VW < View.GetMinWidth) then
            VW := View.GetMinWidth;
          AH := VH;
          if (View.GetMaxHeight > 0) and (AH > View.GetMaxHeight) then
            AH := View.GetMaxHeight;
          if (View.GetMinHeight > 0) and (AH < View.GetMinHeight) then
            AH := View.GetMinHeight;
          if AH <> VH then
            VH := AH;
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
    if (csDesigning in ComponentState)
      and (Supports(Control, IDesignerControl) or (Control.ClassNameIs('TDesignRectangle'))) then Continue;
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
        X := (PW - W) / 2;
        Y := (PH - H) / 2;
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
      Y := AY - Layout.FAbove.Margins.Top;
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
      Y := 0;
      if AutoH then H := PH;
    end else if Layout.FAlignParentBottom then begin
      if AutoH then begin
        Y := 0;
        H := PH;
      end else
        Y := PH - H;
    end else begin
      if AutoH then
        H := PH - Y;
    end;

    if Layout.FCenterHorizontal then begin
      if AutoW then W := PW;
      if Assigned(Parent) then
        X := (PW - W) / 2;
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
      X := 0;
      if AutoW then W := PW;
    end else if Layout.FAlignParentRight then begin
      if AutoW then begin
        X := 0;
        W := PW;
      end else
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

constructor TTextSettings.Create(AOwner: TComponent);
var
  DefaultValueService: IInterface;
  TrimmingDefault: TValue;
begin
  if AOwner is TControl then
    FOwner := TControl(AOwner)
  else FOwner := nil;
  FFont := TFont.Create;
  FFont.OnChanged := DoFontChanged;
  FColor := TAlphaColorRec.Black;
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
  inherited;
end;

procedure TTextSettings.DoChange;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
  FIsSizeChange := False;
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

procedure TTextSettings.Draw(const Canvas: TCanvas; const R: TRectF;
  const Opacity: Single);
var
  V, H: TTextAlign;
  TextStr: string;
begin
  if FPrefixStyle = TPrefixStyle.HidePrefix then
    TextStr := DelAmp(FText)
  else
    TextStr := FText;
  if (Length(TextStr) > 0) then begin
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
    FillText(Canvas, R, TextStr, FWordWrap, Opacity, FillTextFlags, H, V);
  end;
end;

procedure TTextSettings.FillText(const Canvas: TCanvas; const ARect: TRectF;
  const AText: string; const WordWrap: Boolean; const AOpacity: Single;
  const Flags: TFillTextFlags; const ATextAlign, AVTextAlign: TTextAlign);
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
    Layout.Color := FColor;
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

procedure TTextSettings.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    DoChange;
  end;
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

procedure TTextSettings.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    if FAutoSize then FIsSizeChange := True;
    DoChange;
  end;
end;

initialization
  RegisterAliases;
  RegisterFmxClasses([TView, TLinearLayout, TRelativeLayout]);

finalization
  UnregisterAliases;

end.
