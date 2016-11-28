{*******************************************************}
{                                                       }
{       FMX UI ListView 组件单元                        }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.ListView;

interface

{$SCOPEDENUMS ON}

uses
  UI.Debug, UI.Base, UI.Standard, UI.Utils.ArrayEx,
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  FMX.Utils, FMX.ImgList, FMX.MultiResBitmap, FMX.ActnList, System.Rtti, FMX.Consts,
  FMX.TextLayout, FMX.Objects, System.ImageList, System.RTLConsts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, System.Math,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement,
  FMX.Styles.Objects, FMX.Forms;

const
  ListViewType_Default = 0;
  ListViewType_Header = -1;
  ListViewType_Footer = -2;

type
  TListViewEx = class;

  TOnItemMeasureHeight = procedure(Sender: TObject; Index: Integer;
    var AHeight: Single) of object;
  TOnItemClick = procedure(Sender: TObject; ItemIndex: Integer; const ItemView: TControl) of object;
  TOnItemClickEx = procedure(Sender: TObject; ItemIndex: Integer; const ItemObject: TControl) of object;

  /// <summary>
  /// 列表适配器接口
  /// </summary>
  IListAdapter = interface
    ['{5CC5F4AB-2D8C-4A84-98A7-51566E38EA47}']
    function GetCount: Integer;
    function GetItemID(const Index: Integer): Int64;
    function GetItem(const Index: Integer): Pointer;
    function IndexOf(const AItem: Pointer): Integer;
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
    function GetItemViewType(const Index: Integer): Integer;
    function IsEmpty: Boolean;
    function IsEnabled(const Index: Integer): Boolean;
    function ItemDefaultHeight: Single;
    procedure Clear;
    procedure Repaint;
    procedure NotifyDataChanged;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: Pointer read GetItem; default;
  end;

  PListItemPoint = ^TListItemPoint;
  TListItemPoint = record
    H: Single;
  end;

  TListDividerView = class(TView);   

  TListTextItem = class(TTextView)
  private const
    C_MinHeight = 48;
    C_FontSize = 15;
  end;

  TListViewItemCheck = class(TLinearLayout)
  private const
    C_MinHeight = 48;
    C_FontSize = 15;
  public
    TextView1: TTextView;
    CheckBox1: TCheckBox;
  end;

  TListViewItemSingle = class(TLinearLayout)
  private const
    C_MinHeight = 48;
    C_FontSize = 15;
  public
    TextView1: TTextView;
    RadioButton: TRadioButton;
  end;

  TListViewList = TList<TViewBase>;

  /// <summary>
  /// 列表视图内容区域
  /// </summary>
  TListViewContent = class(TViewGroup)
  private
    [Weak] ListView: TListViewEx;
    [Weak] FAdapter: IListAdapter;
    FViews: TDictionary<Integer, TViewBase>;  // 当前显示的控件列表
    FCacleViews: TDictionary<Integer, TListViewList>; // 缓存的控件

    FItemViews: TDictionary<Pointer, Integer>; // 当前显示的控件及索引号
    FItemClick: TDictionary<Pointer, TNotifyEvent>; // 当前显示的控件的原始事件字典
    
    FFirstRowIndex: Integer;
    FLastRowIndex: Integer;

    FOffset: Double;
    FLastPosition: Double;
    FMaxParentHeight: Double;  // 父级控件最大高度（当值>0时，根据列表高度自动调整大小)
    FViewBottom: Double;
    FDividerBrush: TBrush;

    function GetVisibleRowCount: Integer;
    function GetControlFormCacle(const ItemType: Integer): TViewBase;
    procedure AddControlToCacle(const ItemType: Integer; const Value: TViewBase);
  protected
    procedure DoRealign; override;
    procedure AfterPaint; override;
    procedure PaintBackground; override;
    procedure DrawDivider(Canvas: TCanvas);   // 画分隔线
    function ObjectAtPoint(AScreenPoint: TPointF): IControl; override;
  protected
    procedure DoItemClick(Sender: TObject);
    procedure DoItemChildClick(Sender: TObject);
    procedure DoPaintFrame(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure DoMouseDownFrame(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ClearViews;
    procedure HideViews;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// 当前显示的首行索引号
    /// </summary>
    property FirstRowIndex: Integer read FFirstRowIndex;
    /// <summary>
    /// 当前显示的最后一行索引号
    /// </summary>
    property LastRowIndex: Integer read FLastRowIndex;
    /// <summary>
    /// 当前显示了几行
    /// </summary>
    property VisibleRowCount: Integer read GetVisibleRowCount;
  end;

  /// <summary>
  /// 列表框视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TListViewEx = class(TScrollView)
  private
    FAdapter: IListAdapter;
    FDivider: TAlphaColor;
    FDividerHeight: Single;
    FItemsPoints: TArray<TListItemPoint>;
    FContentViews: TListViewContent;
    FLocalDividerHeight: Single;
    FAllowItemChildClick: Boolean;
    FLastHeight: Single;
    FResizeing: Boolean;

    FOnDrawViewBackgroud: TOnDrawViewBackgroud;
    FOnItemMeasureHeight: TOnItemMeasureHeight;
    FOnItemClick: TOnItemClick;
    FOnItemClickEx: TOnItemClickEx;
    procedure SetAdapter(const Value: IListAdapter);
    procedure SetDivider(const Value: TAlphaColor);
    procedure SetDividerHeight(const Value: Single);
    function GetItemPosition(Index: Integer): TListItemPoint;
    function GetFirstRowIndex: Integer;
    function GetLastRowIndex: Integer;
    function GetVisibleRowCount: Integer;
  protected
    function CreateScroll: TScrollBar; override;
    function GetDrawState: TViewState; override;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
    function IsStoredDividerHeight: Boolean; virtual;
  protected
    function GetCount: Integer;
    function IsEmpty: Boolean;
    procedure InvalidateContentSize(); override;
    procedure DoRealign; override;
    procedure DoInVisibleChange; override;
    procedure DoScrollVisibleChange; override;
    procedure DoRecalcSize(var AWidth, AHeight: Single); override;
    procedure DoChangeSize(var ANewWidth, ANewHeight: Single); override;
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure PaintBackground; override;
    procedure DoDrawBackground(var R: TRectF); virtual;
    procedure DoPaintBackground(var R: TRectF); virtual;
    procedure CreateCoentsView();
    procedure HScrollChange(Sender: TObject); override;
    procedure VScrollChange(Sender: TObject); override;
    function InnerCalcDividerHeight: Single;
    function GetDividerHeight: Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // 清空数据
    procedure Clear;
     
    // 通知数据发生改变
    procedure NotifyDataChanged; virtual;

    function IsEnabled(const Index: Integer): Boolean;

    property Count: Integer read GetCount;
    property Empty: Boolean read IsEmpty;
    property Adapter: IListAdapter read FAdapter write SetAdapter;
    property ItemPosition[Index: Integer]: TListItemPoint read GetItemPosition;
    /// <summary>
    /// 当前显示的首行索引号
    /// </summary>
    property FirstRowIndex: Integer read GetFirstRowIndex;
    /// <summary>
    /// 当前显示的最后一行索引号
    /// </summary>
    property LastRowIndex: Integer read GetLastRowIndex;
    /// <summary>
    /// 当前显示了几行
    /// </summary>
    property VisibleRowCount: Integer read GetVisibleRowCount;
  published
    /// <summary>
    /// 是否允许触发列表项中的子控件事件
    /// </summary>
    property AllowItemClickEx: Boolean read FAllowItemChildClick write FAllowItemChildClick default True;
    /// <summary>
    /// 分隔线颜色
    /// </summary>
    property Divider: TAlphaColor read FDivider write SetDivider;
    /// <summary>
    /// 分隔线高度
    /// </summary>
    property DividerHeight: Single read FDividerHeight write SetDividerHeight stored IsStoredDividerHeight;

    property ScrollStretchGlowColor;
    property OnScrollChange;

    property OnDrawBackgroud: TOnDrawViewBackgroud read FOnDrawViewBackgroud
      write FOnDrawViewBackgroud;
    /// <summary>
    /// 测量高度事件
    /// </summary>
    property OnItemMeasureHeight: TOnItemMeasureHeight read FOnItemMeasureHeight
      write FOnItemMeasureHeight;
    /// <summary>
    /// 列表项点击事件
    /// </summary>
    property OnItemClick: TOnItemClick read FOnItemClick write FOnItemClick;
    /// <summary>
    /// 列表项内部控件点击事件
    /// </summary>
    property OnItemClickEx: TOnItemClickEx read FOnItemClickEx write FOnItemClickEx;

    property HitTest default True;
    property Clickable default True;
  end;

  /// <summary>
  /// ListView 数据适配器基类
  /// </summary>
  TListAdapterBase = class(TInterfacedObject, IListAdapter)
  private
    [Weak] FListView: TListViewEx;
  protected
    procedure DoInitData; virtual;
    { IListAdapter }
    function GetItemID(const Index: Integer): Int64; virtual;
    function ItemDefaultHeight: Single; virtual;
    function GetItemViewType(const Index: Integer): Integer; virtual;
    function IsEmpty: Boolean;
    function IsEnabled(const Index: Integer): Boolean; virtual;
  protected
    { IListAdapter }
    function GetCount: Integer; virtual; abstract;
    function GetItem(const Index: Integer): Pointer; virtual; abstract;
    function IndexOf(const AItem: Pointer): Integer; virtual; abstract;
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; virtual; abstract;
  public
    constructor Create();

    procedure Clear; virtual;
    procedure Repaint; virtual;
    procedure NotifyDataChanged; virtual;

    property ListView: TListViewEx read FListView write FListView;
    property Count: Integer read GetCount;
    property Empty: Boolean read IsEmpty;
  end;

  /// <summary>
  /// ListView 数据适配器泛型基类
  /// </summary>
  TListAdapter<T> = class(TListAdapterBase)
  private
    FList: TList<T>;
    function GetItems: TList<T>;
    procedure SetItems(const Value: TList<T>);
  protected
    FListNeedFree: Boolean;
    function GetCount: Integer; override;
  public
    constructor Create(const AItems: TList<T>); overload;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(const Value: T);
    procedure Insert(const Index: Integer; const Value: T);
    procedure Delete(const Index: Integer);
    function Remove(const Value: T): Integer;
    property Items: TList<T> read GetItems write SetItems;
  end;

  /// <summary>
  /// 简单字符串数据接口
  /// </summary>
  TStringsListAdapter = class(TListAdapterBase)
  private
    FFlags: Integer;
    FList: TStrings;
    FArray: TArrayEx<string>;
    function GetItemValue(const Index: Integer): string;
    procedure SetItemValue(const Index: Integer; const Value: string);
    procedure SetArray(const Value: TArray<string>);
    procedure SetList(const Value: TStrings);
    function GetList: TStrings;
    function GetArray: TArray<string>;
  protected
    FListNeedFree: Boolean;
    { IListAdapter }
    function GetCount: Integer; override;
    function GetItem(const Index: Integer): Pointer; override;
    function IndexOf(const AItem: Pointer): Integer; override;
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
    function ItemDefaultHeight: Single; override;
  public
    constructor Create(const AItems: TStrings); overload;
    constructor Create(const AItems: TArray<string>); overload;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(const V: string); overload; virtual;
    procedure Add(const V: TArray<string>); overload; virtual;
    procedure Insert(const Index: Integer; const V: string); virtual;
    procedure Delete(const Index: Integer); virtual;
    procedure SetArrayLength(const ACount: Integer);
    property Items[const Index: Integer]: string read GetItemValue write SetItemValue; default;
    property Strings: TStrings read GetList write SetList;
    property StringArray: TArray<string> read GetArray write SetArray;
  end;

  /// <summary>
  /// 多选列表适配器
  /// </summary>
  TStringsListCheckAdapter = class(TStringsListAdapter)
  private
    FChecks: TArrayEx<Boolean>;
    function GetItemCheck(const Index: Integer): Boolean;
    procedure SetItemCheck(const Index: Integer; const Value: Boolean);
    procedure SetChecks(const Value: TArray<Boolean>);
    function GetChecks: TArray<Boolean>;
  protected
    procedure DoCheckChange(Sender: TObject);
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
  public
    procedure Insert(const Index: Integer; const V: string); override;
    procedure Delete(const Index: Integer); override;
    property Checks: TArray<Boolean> read GetChecks write SetChecks;
    property ItemCheck[const Index: Integer]: Boolean read GetItemCheck write SetItemCheck;
  end;

  /// <summary>
  /// 单选列表适配器
  /// </summary>
  TStringsListSingleAdapter = class(TStringsListAdapter)
  private
    FItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
  protected
    procedure DoInitData; override;
    procedure DoItemIndexChange(Sender: TObject);
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
  public
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  end;

implementation

{ TListViewEx }

function TListViewEx.CanRePaintBk(const View: IView;
  State: TViewState): Boolean;
begin
  Result := (State = TViewState.None) and (not AniCalculations.Animation);
end;

procedure TListViewEx.Clear;
begin
  if Assigned(FAdapter) then begin
    FAdapter.Clear;
    NotifyDataChanged;
  end;
end;

constructor TListViewEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateCoentsView();
  FAllowItemChildClick := True;
  FDivider := $afe3e4e5;
  FDividerHeight := -1;
  FLocalDividerHeight := -1;
  SetLength(FItemsPoints, 0);
  ScrollBars := TViewScroll.Vertical;
  DisableFocusEffect := True;
  AutoCapture := True;
  ClipChildren := True;
  with Background.ItemPressed do begin
    Color := $ffd9d9d9;
    Kind := TViewBrushKind.Solid; 
  end;
  HitTest := True;
end;

procedure TListViewEx.CreateCoentsView;
begin
  FContentViews := TListViewContent.Create(Self);
  FContentViews.Visible := True;
  FContentViews.Stored := False;
  FContentViews.Locked := True;
  FContentViews.Parent := Self;
  FContentViews.ListView := Self;
  RealignContent;
end;

function TListViewEx.CreateScroll: TScrollBar;
begin
  {$IFDEF MSWINDOWS}
  Result := TScrollBar.Create(Self);
  {$ELSE}
  Result := TSmallScrollBar.Create(Self);
  {$ENDIF}
end;

destructor TListViewEx.Destroy;
begin
  FAdapter := nil;
  inherited Destroy;
end;

procedure TListViewEx.DoChangeSize(var ANewWidth, ANewHeight: Single);
begin
  inherited DoChangeSize(ANewWidth, ANewHeight);
end;

procedure TListViewEx.DoDrawBackground(var R: TRectF);
begin
  if Assigned(FOnDrawViewBackgroud) then
    FOnDrawViewBackgroud(Self, Canvas, R, DrawState);
end;

procedure TListViewEx.DoInVisibleChange;
begin
  inherited DoInVisibleChange;
  FContentViews.InVisible := InVisible;
end;

procedure TListViewEx.DoPaintBackground(var R: TRectF);
begin
  R := RectF(R.Left + Padding.Left, R.Top + Padding.Top,
    R.Right - Padding.Right, R.Bottom - Padding.Bottom);
end;

procedure TListViewEx.DoRealign;
var
  W: Single;
begin
  inherited;
  if FDisableAlign or IsUpdating then
    Exit;
  if (csDestroying in ComponentState) then
    Exit;
  FDisableAlign := True;
  {$IFDEF MSWINDOWS}
  if Assigned(FScroll) and (FScroll.Visible) then
    W := Width - Padding.Right - Padding.Left{$IFDEF MSWINDOWS} - FScroll.Width{$ENDIF}
  else
    W := Width - Padding.Right - Padding.Left;
  {$ELSE}
  W := Width - Padding.Right - Padding.Left;
  {$ENDIF}
  // 如果列表高度为自动大小时，计算一下父级视图的最大高度，在自动调整大小时会使用
  if HeightSize = TViewSize.WrapContent then
    FContentViews.FMaxParentHeight := GetParentMaxHeight
  else  
    FContentViews.FMaxParentHeight := 0;
  FContentViews.SetBounds(Padding.Left, Padding.Top, W,
    Height - Padding.Bottom - Padding.Top);
  FDisableAlign := False;
end;

procedure TListViewEx.DoRecalcSize(var AWidth, AHeight: Single);
begin
  if HeightSize = TViewSize.WrapContent then
    if FContentViews.FViewBottom < FContentBounds.Height then begin
      AHeight := FContentViews.FViewBottom + Padding.Height
    end else begin
      AHeight := FContentBounds.Height + Padding.Height;
    end;
end;

procedure TListViewEx.DoScrollVisibleChange;
begin
  inherited DoScrollVisibleChange;
end;

function TListViewEx.GetCount: Integer;
begin
  if Assigned(FAdapter) then
    Result := FAdapter.GetCount
  else
    Result := 0;
end;

function TListViewEx.GetDividerHeight: Single;
begin
  if FLocalDividerHeight = -1 then
    FLocalDividerHeight := InnerCalcDividerHeight;
  Result := FLocalDividerHeight;
end;

function TListViewEx.GetDrawState: TViewState;
begin
  Result := TViewState.None;
end;

function TListViewEx.GetFirstRowIndex: Integer;
begin
  Result := FContentViews.FirstRowIndex;
end;

function TListViewEx.GetItemPosition(Index: Integer): TListItemPoint;
begin
  Result := FItemsPoints[Index];
end;

function TListViewEx.GetLastRowIndex: Integer;
begin
  Result := FContentViews.LastRowIndex;
end;

function TListViewEx.GetVisibleRowCount: Integer;
begin
  Result := FContentViews.VisibleRowCount;
end;

procedure TListViewEx.HScrollChange(Sender: TObject);
begin
  inherited HScrollChange(Sender);
  if Assigned(FContentViews) then
    FContentViews.Realign;
end;

function TListViewEx.InnerCalcDividerHeight: Single;
var
  PPI: Single;
begin
  if FDividerHeight = -1 then begin
    PPI := Canvas.Scale;
    if PPI > TEpsilon.Scale then
      Result := 1 / PPI
    else
      Result := 1;

    if PPI >= 2 then
      Result := Result * 2;
  end else
    Result := FDividerHeight;
end;

procedure TListViewEx.InvalidateContentSize;
var
  ItemDefaultHeight: Single;
begin
  SetLength(FItemsPoints, Count);
  FContentBounds := TRectD.Empty;
  if Length(FItemsPoints) = 0 then
    Exit;
  ItemDefaultHeight := FAdapter.ItemDefaultHeight;
  FContentBounds.Right := FContentViews.Width;
  FContentBounds.Bottom := (ItemDefaultHeight + GetDividerHeight) * Length(FItemsPoints);
end;

function TListViewEx.IsEmpty: Boolean;
begin
  Result := GetCount = 0;
end;

function TListViewEx.IsEnabled(const Index: Integer): Boolean;
begin
  if Assigned(FAdapter) then
    Result := FAdapter.IsEnabled(Index)
  else
    Result := False;
end;

function TListViewEx.IsStoredDividerHeight: Boolean;
begin
  Result := FDividerHeight <> -1;
end;

procedure TListViewEx.Loaded;
begin
  inherited Loaded;
end;

procedure TListViewEx.NotifyDataChanged;
var
  Offset: Double;
begin
  if (csLoading in ComponentState) or FContentViews.FDisableAlign then
    Exit;
  FContentViews.FDisableAlign := True;
  try
    FContentViews.HideViews;
    FContentViews.FOffset := -1;
    if Length(FItemsPoints) > 0 then
      FillChar(FItemsPoints[0], SizeOf(TListItemPoint) * Length(FItemsPoints), 0);
    InvalidateContentSize;
    if (FContentViews.FFirstRowIndex > -1) then begin
      if FContentViews.FFirstRowIndex > High(FItemsPoints) then
        FContentViews.FFirstRowIndex := High(FItemsPoints);
      Offset := FScroll.Value - FContentViews.FLastPosition;
      FScroll.Value := (FAdapter.ItemDefaultHeight + GetDividerHeight) * FContentViews.FFirstRowIndex + Offset;
      FContentViews.FLastPosition := FScroll.Value - Offset;
    end;
    DoUpdateScrollingLimits(True);
  finally
    FContentViews.FDisableAlign := False;
    FContentViews.Realign;
  end;

  // 如果超出顶部区域, 则将滚动条置为最底部，重新排列列表项
  Offset := FContentViews.FLastPosition - FScroll.Value;
  if Offset > 0 then begin
    FContentViews.FDisableAlign := True;
    try
      FContentViews.HideViews;
      FContentViews.FOffset := -1;
      FContentViews.FFirstRowIndex := -1;
      FContentViews.FLastPosition := 0;
      ViewportPosition := PointF(0, FContentBounds.Bottom - FScroll.ViewportSize);
    finally
      FContentViews.FDisableAlign := False;
      FContentViews.Realign;
    end;
  end;

  RecalcSize;
end;

procedure TListViewEx.PaintBackground;
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

procedure TListViewEx.Resize;
begin
  if FResizeing or
    (csLoading in ComponentState) or
    (csDestroying in ComponentState) or
    (csDesigning in ComponentState) then
    Exit;
  FResizeing := True;
  inherited Resize;
  if Assigned(FAdapter) then begin
    FContentViews.HideViews;
    FContentViews.FOffset := FContentViews.FOffset + FLastHeight - Height;
    FContentViews.FLastRowIndex := -1;
    UpdateScrollBar;
    FContentViews.Realign;
    RecalcSize;
  end;
  FLastHeight := Height;
  FResizeing := False;
end;

procedure TListViewEx.SetAdapter(const Value: IListAdapter);
begin
  if FAdapter <> Value then begin
    FAdapter := Value;
    FContentViews.FAdapter := Value;
    if FAdapter is TListAdapterBase then
      (FAdapter as TListAdapterBase).FListView := Self;
    NotifyDataChanged;
    HandleSizeChanged;
  end;
end;

procedure TListViewEx.SetDivider(const Value: TAlphaColor);
begin
  if FDivider <> Value then begin
    FDivider := Value;
    Invalidate;
  end;
end;

procedure TListViewEx.SetDividerHeight(const Value: Single);
begin
  if FDividerHeight <> Value then begin
    FDividerHeight := Value;
    if not (csLoading in ComponentState) then begin     
      FLocalDividerHeight := GetDividerHeight;
      RealignContent;
      Invalidate;
    end;
  end;
end;

procedure TListViewEx.VScrollChange(Sender: TObject);
begin
  if FScrolling then Exit;  
  inherited VScrollChange(Sender);
  if Assigned(FContentViews) then
    FContentViews.Realign;
end;

{ TListViewContent }

procedure TListViewContent.AddControlToCacle(const ItemType: Integer;
  const Value: TViewBase);
var
  List: TListViewList;
begin
  if not FCacleViews.ContainsKey(Itemtype) then begin
    List := TListViewList.Create;
    FCacleViews.Add(ItemType, List);
  end else begin
    List := FCacleViews[ItemType];
  end;
  Value.Visible := False;
  List.Add(Value)
end;

procedure TListViewContent.AfterPaint;
begin
  inherited AfterPaint;
  // 画滚动伸展效果
  if Assigned(ListView) and ListView.NeedPaintScrollingStretchGlow then
    ListView.PaintScrollingStretchGlow(Canvas, Width, Height,
      ListView.FScrollStretchStrength, GetAbsoluteOpacity);
end;

procedure TListViewContent.ClearViews;
var
  Item: TPair<Integer, TListViewList>;
  ItemView: TPair<Integer, TViewBase>;
  I: Integer;
begin
  for Item in FCacleViews do begin
    for I := 0 to Item.Value.Count - 1 do
      RemoveObject(Item.Value.Items[I]);
    Item.Value.DisposeOf;
  end;
  FCacleViews.Clear;
  for ItemView in FViews do
    RemoveObject(ItemView.Value);
  FViews.Clear;
end;

constructor TListViewContent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViews := TDictionary<Integer, TViewBase>.Create(256);
  FCacleViews := TDictionary<Integer, TListViewList>.Create(17);
  FItemViews := TDictionary<Pointer, Integer>.Create(256);
  FItemClick := TDictionary<Pointer, TNotifyEvent>.Create(256);
  FFirstRowIndex := -1;
  FLastRowIndex := -1;
  
  FLastPosition := 0;

  FDividerBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Null);
end;

destructor TListViewContent.Destroy;
begin
  ClearViews;
  FreeAndNil(FViews);
  FreeAndNil(FCacleViews);
  FreeAndNil(FDividerBrush);
  FreeAndNil(FItemViews);
  FreeAndNil(FItemClick);
  inherited;
end;

procedure TListViewContent.DoItemChildClick(Sender: TObject);
begin
  if (FItemViews.ContainsKey(Sender)) then begin
    if FItemClick.ContainsKey(Sender) and Assigned(FItemClick[Sender]) then
      FItemClick[Sender](Sender);
    if Assigned(ListView.FOnItemClickEx) then
      ListView.FOnItemClickEx(ListView, FItemViews[Sender], TControl(Sender));
  end;
end;

procedure TListViewContent.DoItemClick(Sender: TObject);
begin
  if (FItemViews.ContainsKey(Sender)) then begin
    if FItemClick.ContainsKey(Sender) and Assigned(FItemClick[Sender]) then
      FItemClick[Sender](Sender);
    if Assigned(ListView.FOnItemClick) then
      ListView.FOnItemClick(ListView, FItemViews[Sender], TView(Sender));
  end;
end;

procedure TListViewContent.DoMouseDownFrame(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
    Repaint;
end;

procedure TListViewContent.DoPaintFrame(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if TControl(Sender).Pressed then begin
    Canvas.FillRect(ARect, 0, 0, [], Opacity, ListView.Background.ItemPressed);
  end;
end;

procedure TListViewContent.DoRealign;

  // 递归设置组件及其子项的点击事件
  procedure SetChildClickEvent(const Parent: TControl; const Index: Integer);
  var
    I: Integer;
    Control: TControl;
  begin
    for I := 0 to Parent.ChildrenCount - 1 do begin
      Control := Parent.Controls[I];
      if not Control.Visible then
        Continue;
      if Control.HitTest then begin
        Control.OnClick := DoItemChildClick;
        FItemViews.Add(Control, Index);
      end;
      if Control.ChildrenCount > 0 then
        SetChildClickEvent(Control, Index);
    end;
  end;

  // 获取 View
  function GetView(const ItemView: TViewBase): TView;
  begin
    if ItemView is TView then begin
      Result := ItemView as TView;
    end else if (TControl(ItemView) is TFrame) and (ItemView.ControlsCount = 1) and (ItemView.Controls[0] is TView) then
      Result := TView(ItemView.Controls[0]) // 如果是 Frame ，且只有一个TView子组件
    else
      Result := nil;
  end;

  // 移除列表项, 归还到缓存中
  procedure RemoveItemView(const I: Integer; const ItemView: TViewBase);
  begin
    FViews.Remove(I);
    AddControlToCacle(FAdapter.GetItemViewType(I), ItemView);
  end;

  function EqulsMethod(const A, B: TNotifyEvent): Boolean;
  begin
    Result := TMethod(A) = TMethod(B);
  end;

var
  First, Last: Double;
  ItemDefaultHeight, Offset, LV, V, AdjustH, H: Double;
  AH, AW, MinH, MaxH: Single;
  I, J, S, K, ItemType: Integer;
  Item: PListItemPoint;
  Control, ItemView: TViewBase;
  View: TView;
  DividerHeight: Double;
  IsMoveDown, LCheckViews: Boolean;
  FOnItemMeasureHeight: TOnItemMeasureHeight;
  FNewOnClick: TNotifyEvent;
begin
  if FDisableAlign or (FAdapter = nil) or
    (csLoading in ComponentState) or
    (csDestroying in ComponentState) then
    Exit;

  FDisableAlign := True;

  // 偏移位置 (滚动条位置)
  Offset := ListView.VScrollBarValue;
  // 根据滚动条偏移值，判断是否有变化
  AdjustH := Offset - FOffset;
  FOffset := Offset;

  // 如果滚动条不变，则不处理
  {$IFDEF MSWINDOWS}
  if (FViews.Count > 0) and (AdjustH = 0) then begin
    FDisableAlign := False;
    Exit;
  end;
  {$ENDIF}

  if (Abs(AdjustH) > Height * 2) then begin
    HideViews;
    LCheckViews := False;
  end else
    LCheckViews := FViews.Count > 0;

  // 默认行高
  ItemDefaultHeight := FAdapter.ItemDefaultHeight;
  // 计算出当前可显示的第一行位置和最后一行位置
  First := Offset;
  // 如果需要自动调整大小，且滚动条偏移为0时，说明正在初始化列表
  if (FMaxParentHeight > 0) and (Offset = 0) then
    Last := Offset + FMaxParentHeight // 使用父级视图的最大高度为列表项的底边
  else
    Last := Offset + Height; // ListView.FContentBounds.Height;     
  // 分隔条高度
  DividerHeight := ListView.GetDividerHeight;

  // 变量初始化
  J := 0;
  K := FFirstRowIndex + FViews.Count;
  AW := Width;
  IsMoveDown := AdjustH >= 0;  // 当前是否向下滚动
  FOnItemMeasureHeight := ListView.FOnItemMeasureHeight;
  AdjustH := 0;

  BeginUpdate;
  try
    // 根据记录的状态，计算出首行显示位置, 避免每次都从头开始算位置
    if IsMoveDown then begin
      // 向下滚动时，直接从记录的开始位置开始算
      if FFirstRowIndex < 0 then begin
        S := 0;
        V := 0;
      end else begin
        S := FFirstRowIndex;
        V := FLastPosition;
      end;
    end else begin
      // 向上滚动时，计算出开始位置
      V := FLastPosition;
      S := FFirstRowIndex;
      while (S > 0) do begin
        if V <= First then
          Break;
        Dec(S);
        H := ListView.FItemsPoints[S].H;
        if H < 0 then
          Continue;
        if H = 0 then
          V := V - ItemDefaultHeight - DividerHeight
        else
          V := V - H - DividerHeight;
      end;
    end;

    FFirstRowIndex := -1;
    FLastRowIndex := -1;

    // 从指定位置开始，生成并调整列表项
    for I := S to High(ListView.FItemsPoints) do begin
      Item := @ListView.FItemsPoints[I];

      // 获取列表项高度
      H := Item.H;
      if H = 0 then
        H := ItemDefaultHeight
      else if H < 0 then
        Continue;

      // 判断列表项可视状态
      if (V + H + DividerHeight <= First) then begin
        // 超出顶部可视区域
        // 如果已经显示，则将它删除
        if LCheckViews and FViews.ContainsKey(I) then
          RemoveItemView(I, FViews[I]);
        // 计算出下一项的位置
        V := V + H + DividerHeight;
        Continue;
      end else if V >= Last then begin
        // 超出尾部可视区域
        S := I;
        while S < K do begin
          if FViews.ContainsKey(S) then
            RemoveItemView(S, FViews[S]);
          Inc(S);
        end;
        Break;
      end;

      // 如果是第一个可视项, 记录状态
      if FFirstRowIndex = -1 then begin
        FFirstRowIndex := I;
        FLastPosition := V;
      end;

      // 可视组件计数器增加
      Inc(J);

      // 记录列表项的开始位置
      LV := V;

      // 如果已经存在，说明之前加载过，并且正在显示，且已经调整好位置
      if FViews.ContainsKey(I) then begin

        AH := H;
        // 获取一个列表项视图
        ItemView := FViews[I];

        // 如果返回nil, 抛出错误
        if not Assigned(ItemView) then
          raise Exception.Create('View is null.');

        // 触发用户修改行高的事件
        if Assigned(FOnItemMeasureHeight) then
          FOnItemMeasureHeight(ListView, I, AH);

        // 如果行高更改了，则后续需要调整滚动区的大小，这里记录一下变化大小
        if Item.H <> AH then begin
          if AH <= 0 then begin
            if Item.H > 0 then
              AdjustH := AdjustH - H
            else
              AdjustH := AdjustH - ItemDefaultHeight;
          end else begin
            if Item.H > 0 then
              AdjustH := AdjustH + (AH - H)
            else
              AdjustH := AdjustH + (AH - ItemDefaultHeight);
          end;
          Item.H := AH;
        end;

        // 更新 V, 代表列表项的底部位置
        if AH > 0 then begin
          V := V + AH + DividerHeight
        end else begin
          // 移除
          RemoveItemView(I, ItemView);
          Continue;
        end;

      end else begin
        // 如果不存在
        // 在缓存视图列表中取一个缓存的视图
        ItemType := FAdapter.GetItemViewType(I);
        Control := GetControlFormCacle(ItemType);

        // 获取一个列表项视图
        ItemView := FAdapter.GetView(I, Control, Self);

        // 如果返回nil, 抛出错误
        if not Assigned(ItemView) then
          raise Exception.Create('View is null.');

        // 记录到可视列表中
        FViews.AddOrSetValue(I, ItemView);

        // 获取 View
        View := GetView(ItemView);

        // 如果当前项与缓存项不同，说明是新生成的, 初始化一些数据
        if Control <> ItemView then begin
          {$IFDEF Debug} {$IFDEF MSWINDOWS}
          OutputDebugString(PChar(Format('增加列表视图 Index: %d (ViewCount: %d)',
            [I, FViews.Count])));
          {$ENDIF}{$ENDIF}

          ItemView.Name := '';
          ItemView.Parent := Self;

          if Assigned(View) then begin
            // 如果是 TView ， 设置按下时的背景颜色
            if ItemView <> View then
              ItemView.HitTest := False;
            View.Background.ItemPressed.Assign(ListView.Background.ItemPressed);
            View.HitTest := True;
            if ListView.FAllowItemChildClick then
              SetChildClickEvent(View, I);
          end else begin
            // 如果是一个 Frame，让它可以点击
            // 设置点击事件，设置鼠村按下和松开事件时重绘
            ItemView.HitTest := True;
            ItemView.OnPainting := DoPaintFrame;
            ItemView.OnMouseDown := DoMouseDownFrame;
            ItemView.OnMouseUp := DoMouseDownFrame;
            if ListView.FAllowItemChildClick then
              SetChildClickEvent(ItemView, I);
          end;
        end;

        // 记录列表项与索引号的对应关系和事件与视图对应关系到字典中
        FNewOnClick := DoItemClick;
        if Assigned(View) then begin
          View.ViewState := [];
          FItemViews.AddOrSetValue(View, I);
          if Assigned(View.OnClick) and (not EqulsMethod(FNewOnClick, View.OnClick)) then
            FItemClick.AddOrSetValue(View, View.OnClick);
          View.OnClick := FNewOnClick;
        end else begin
          FItemViews.AddOrSetValue(ItemView, I);
          if Assigned(ItemView.OnClick) and (not EqulsMethod(FNewOnClick, ItemView.OnClick)) then
            FItemClick.AddOrSetValue(ItemView, ItemView.OnClick);
          ItemView.OnClick := FNewOnClick;
        end;

        // 调整大小和位置
        if Assigned(View) then begin
          AH := View.Height;
          MinH := View.MinHeight;
          MaxH := View.MaxHeight;
          if (MaxH > 0) and (AH > MaxH) then AH := MaxH;
          if (MinH > 0) and (AH < MinH) then AH := MinH;
        end else
          AH := ItemView.Height;

        // 触发用户修改行高的事件
        if Assigned(FOnItemMeasureHeight) then
          FOnItemMeasureHeight(ListView, I, AH);

        // 如果行高更改了，则后续需要调整滚动区的大小，这里记录一下变化大小
        if Item.H <> AH then begin
          if AH <= 0 then begin
            if Item.H > 0 then
              AdjustH := AdjustH - Item.H
            else
              AdjustH := AdjustH - ItemDefaultHeight;
          end else begin
            if Item.H > 0 then
              AdjustH := AdjustH + (AH - Item.H)
            else
              AdjustH := AdjustH + (AH - ItemDefaultHeight);
          end;
          Item.H := AH;
        end;

        // 更新 V, 代表列表项的底部位置
        if AH > 0 then begin
          V := V + AH + DividerHeight
        end else begin
          // 移除
          RemoveItemView(I, ItemView);
          Continue;
        end;

        ItemView.Visible := True;

        if Assigned(View) then begin
          TListViewContent(View).FInVisible := ListView.FInVisible;
          if AH <> ItemView.Height then
            TListViewContent(View).HeightSize := TViewSize.CustomSize;
        end;

      end;

      // 更新大小并显示出来
      ItemView.SetBounds(0, LV - Offset, AW, AH);

      // 更新完大小后，如果高度还是不一致，则使用实际的视图高度
      if ItemView.Height <> AH then begin
        Item.H := ItemView.Height;
        H := Item.H - AH;
        AdjustH := AdjustH + H;
        V := V + H;
      end;

      // 记录底部位置
      FViewBottom := V;
    end;
  finally
    // 显示的最后一个列表项索引号
    FLastRowIndex := FFirstRowIndex + J;
    EndUpdate;
    if (FMaxParentHeight > 0) and (FFirstRowIndex = 0) then begin
      // 当需要自动调整大小，并且显示的首行为第一行时
      // 如果当前列表视图的底部位置小于父级大小，则使用当前视图底部为列表框高度
      if FViewBottom < FMaxParentHeight then      
        SetSize(Width, FViewBottom)
      else // 如果超出时，则使用父级最大高度为列表视图高度
        SetSize(Width, FMaxParentHeight)
    end;
    if AdjustH <> 0 then begin
      // 高度变化了, 更新滚动条状态
      ListView.FContentBounds.Bottom := ListView.FContentBounds.Bottom + AdjustH;
      ListView.DoUpdateScrollingLimits(True);
      Invalidate;
    end;
    FDisableAlign := False;
  end;
end;

procedure TListViewContent.DrawDivider(Canvas: TCanvas);
var
  I: Integer;
  Y, DividerHeight: Double;
begin
  DividerHeight := ListView.GetDividerHeight;
  if (DividerHeight > 0) and (ListView.FDivider and $FF000000 <> 0) then begin
    FDividerBrush.Color := ListView.FDivider;
    Y := FLastPosition - FOffset;
    for I := FirstRowIndex to FLastRowIndex do begin
      Y := Y + ListView.FItemsPoints[I].H;
      Canvas.FillRect(RectF(0, Y, Width, Y + DividerHeight),
        0, 0, [], ListView.Opacity, FDividerBrush);
      Y := Y + DividerHeight;
    end;
  end;
end;

function TListViewContent.GetControlFormCacle(const ItemType: Integer): TViewBase;
var
  List: TListViewList;
begin
  if not FCacleViews.ContainsKey(Itemtype) then begin
    List := TListViewList.Create;
    FCacleViews.Add(ItemType, List);
  end else begin
    List := FCacleViews[ItemType];
  end;
  if List.Count > 0 then begin
    Result := List.Last;
    List.Delete(List.Count - 1);
  end else
    Result := nil;
end;

function TListViewContent.GetVisibleRowCount: Integer;
begin
  Result := FLastRowIndex - FFirstRowIndex;
end;

procedure TListViewContent.HideViews;
var
  ItemView: TPair<Integer, TViewBase>;
begin
  for ItemView in FViews do
    AddControlToCacle(FAdapter.GetItemViewType(ItemView.Key), ItemView.Value);
  FViews.Clear;
end;

function TListViewContent.ObjectAtPoint(AScreenPoint: TPointF): IControl;
begin
  if Assigned(ListView.FAniCalculations) and (ListView.FAniCalculations.Shown) then
    Result := nil   // 手势滚动中，不允许点击子项
  else
    Result := inherited ObjectAtPoint(AScreenPoint);
end;

procedure TListViewContent.PaintBackground;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  if (FInVisible) or (Assigned(ListView) and (ListView.FInVisible)) then
    Exit;
  inherited PaintBackground;
  // 画分隔线
  if (FirstRowIndex < FLastRowIndex) and (FViewBottom > FLastPosition) then
    DrawDivider(Canvas);
end;

{ TListAdapterBase }

procedure TListAdapterBase.Clear;
begin
end;

constructor TListAdapterBase.Create;
begin
  DoInitData;
end;

procedure TListAdapterBase.DoInitData;
begin
end;

function TListAdapterBase.GetItemID(const Index: Integer): Int64;
begin
  Result := Index;
end;

function TListAdapterBase.GetItemViewType(const Index: Integer): Integer;
begin
  Result := ListViewType_Default;
end;

function TListAdapterBase.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TListAdapterBase.IsEnabled(const Index: Integer): Boolean;
begin
  Result := True;
end;

function TListAdapterBase.ItemDefaultHeight: Single;
begin
  Result := 50;
end;

procedure TListAdapterBase.NotifyDataChanged;
begin
  if Assigned(FListView) then
    FListView.NotifyDataChanged;
end;

procedure TListAdapterBase.Repaint;
begin
  if Assigned(FListView) and Assigned(FListView.FContentViews) then
    FListView.FContentViews.Realign;
end;

{ TListAdapter<T> }

procedure TListAdapter<T>.Add(const Value: T);
begin
  Items.Add(Value);
end;

procedure TListAdapter<T>.Clear;
begin
  if Assigned(FList) then
    FList.Clear;
end;

constructor TListAdapter<T>.Create(const AItems: TList<T>);
begin
  SetItems(AItems);
  DoInitData;
end;

procedure TListAdapter<T>.Delete(const Index: Integer);
begin
  Items.Delete(Index);
end;

destructor TListAdapter<T>.Destroy;
begin
  if FListNeedFree then
    FreeAndNil(FList)
  else
    FList := nil;
  inherited;
end;

function TListAdapter<T>.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Count
  else
    Result := 0;
end;

function TListAdapter<T>.GetItems: TList<T>;
begin
  if FList = nil then begin
    FList := TList<T>.Create;
    FListNeedFree := True;
  end;
  Result := FList;
end;

procedure TListAdapter<T>.Insert(const Index: Integer; const Value: T);
begin
  Items.Insert(Index, Value);
end;

function TListAdapter<T>.Remove(const Value: T): Integer;
begin
  Result := Items.Remove(Value);
end;

procedure TListAdapter<T>.SetItems(const Value: TList<T>);
begin
  if Assigned(FList) then begin
    if FListNeedFree then
      FreeAndNil(FList);
    FListNeedFree := False;
  end;
  FList := Value;
end;

{ TStringsListAdapter }

procedure TStringsListAdapter.Add(const V: string);
begin
  if FFlags = 0 then
    GetList.Add(V)
  else
    FArray.Append(V);
end;

procedure TStringsListAdapter.Add(const V: TArray<string>);
var
  I: Integer;
begin
  if FFlags = 0 then begin
    for I := 0 to High(V) do
      GetList.Add(V[I])
  end else
    FArray.Append(V);
end;

procedure TStringsListAdapter.Clear;
begin
  inherited Clear;
  if Assigned(FList) then
    FList.Clear;
  FArray.Clear;
end;

constructor TStringsListAdapter.Create(const AItems: TArray<string>);
begin
  SetArray(AItems);
  DoInitData;
end;

constructor TStringsListAdapter.Create(const AItems: TStrings);
begin
  if AItems <> nil then
    SetList(AItems);
  DoInitData;
end;

procedure TStringsListAdapter.Delete(const Index: Integer);
begin
  if FFlags = 0 then
    GetList.Delete(Index)
  else
    FArray.Delete(Index);
end;

destructor TStringsListAdapter.Destroy;
begin
  if FListNeedFree then
    FreeAndNil(FList)
  else
    FList := nil;
  inherited Destroy;
end;

function TStringsListAdapter.GetArray: TArray<string>;
begin
  Result := FArray;
end;

function TStringsListAdapter.GetCount: Integer;
begin
  if FFlags = 0 then begin
    if Assigned(FList) then
      Result := FList.Count
    else
      Result := 0;
  end else
    Result := FArray.Len;
end;

function TStringsListAdapter.GetItem(const Index: Integer): Pointer;
begin
  if FFlags = 0 then
    Result := PChar(FList[Index])
  else
    Result := PChar(FArray[Index]);
end;

function TStringsListAdapter.GetItemValue(const Index: Integer): string;
begin
  if FFlags = 0 then
    Result := FList[Index]
  else
    Result := FArray[Index];
end;

function TStringsListAdapter.GetList: TStrings;
begin
  if FList = nil then begin
    FListNeedFree := True;
    FList := TStringList.Create;
  end;
  Result := FList;
end;

function TStringsListAdapter.GetView(const Index: Integer;
  ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
var
  ViewItem: TListTextItem;
begin
  if (ConvertView = nil) or (not (ConvertView is TListTextItem)) then begin
    ViewItem := TListTextItem.Create(Parent);
    ViewItem.Parent := Parent;
    ViewItem.Width := Parent.Width;
    ViewItem.MinHeight := TListTextItem.C_MinHeight;
    ViewItem.TextSettings.Font.Size := TListTextItem.C_FontSize;
    ViewItem.TextSettings.WordWrap := True;
    ViewItem.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.Padding.Rect := RectF(8, 8, 8, 8);
  end else
    ViewItem := ConvertView as TListTextItem;
  ViewItem.HeightSize := TViewSize.WrapContent;
  ViewItem.Text := Items[Index];
  Result := ViewItem;
end;

function TStringsListAdapter.IndexOf(const AItem: Pointer): Integer;
begin
  Result := -1;
end;

procedure TStringsListAdapter.Insert(const Index: Integer; const V: string);
begin
  if FFlags = 0 then
    GetList.Insert(Index, V)
  else
    FArray.Insert(Index, V)
end;

function TStringsListAdapter.ItemDefaultHeight: Single;
begin
  Result := TListTextItem.C_MinHeight;
end;

procedure TStringsListAdapter.SetArray(const Value: TArray<string>);
begin
  FArray := Value;
  FFlags := 1;
end;

procedure TStringsListAdapter.SetArrayLength(const ACount: Integer);
begin
  FArray.Len := ACount;
  FFlags := 1;
end;

procedure TStringsListAdapter.SetItemValue(const Index: Integer;
  const Value: string);
begin
  if FFlags = 0 then begin
    FList[Index] := Value
  end else
    FArray[Index] := Value;
end;

procedure TStringsListAdapter.SetList(const Value: TStrings);
begin
  if not Assigned(FList) then begin
    if FListNeedFree then
      FreeAndNil(FList);
    FListNeedFree := False;
  end;
  FList := Value;
  FFlags := 0;
end;

{ TStringsListCheckAdapter }

procedure TStringsListCheckAdapter.Delete(const Index: Integer);
begin
  inherited;
  FChecks.Delete(Index);
end;

procedure TStringsListCheckAdapter.DoCheckChange(Sender: TObject);
var
  V: Boolean;
begin
  V := not ItemCheck[TControl(Sender).Tag];
  ItemCheck[TControl(Sender).Tag] := V;
  if Sender is TListViewItemCheck then
    TListViewItemCheck(Sender).CheckBox1.IsChecked := V;
end;

function TStringsListCheckAdapter.GetChecks: TArray<Boolean>;
begin
  Result := FChecks;
end;

function TStringsListCheckAdapter.GetItemCheck(const Index: Integer): Boolean;
begin
  if Index < FChecks.Len then
    Result := FChecks[Index]
  else
    Result := False;
end;

function TStringsListCheckAdapter.GetView(const Index: Integer;
  ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
var
  ViewItem: TListViewItemCheck;
begin
  if (ConvertView = nil) or (not (TControl(ConvertView) is TListViewItemCheck)) then begin
    ViewItem := TListViewItemCheck.Create(Parent);
    ViewItem.WidthSize := TViewSize.FillParent;
    ViewItem.HeightSize := TViewSize.WrapContent;
    ViewItem.MinHeight := TListTextItem.C_MinHeight;
    ViewItem.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.Parent := Parent;
    ViewItem.Width := Parent.Width;

    ViewItem.TextView1 := TTextView.Create(ViewItem);
    ViewItem.TextView1.WidthSize := TViewSize.FillParent;
    ViewItem.TextView1.Weight := 1;
    ViewItem.TextView1.HeightSize := TViewSize.WrapContent;
    ViewItem.TextView1.TextSettings.Font.Size := TListTextItem.C_FontSize;
    ViewItem.TextView1.TextSettings.WordWrap := True;
    ViewItem.TextView1.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.TextView1.Margins.Rect := RectF(0, 0, 4, 0);
    ViewItem.TextView1.Padding.Rect := RectF(8, 8, 8, 8);
    ViewItem.TextView1.AutoSize := True;
    ViewItem.TextView1.Parent := ViewItem;

    ViewItem.CheckBox1 := TCheckBox.Create(ViewItem);
    ViewItem.CheckBox1.Text := '';
    ViewItem.CheckBox1.Width := 42;
    ViewItem.CheckBox1.Height := ViewItem.MinHeight;
    ViewItem.CheckBox1.Parent := ViewItem;
    ViewItem.CheckBox1.HitTest := False;
    ViewItem.TextView1.Width := ViewItem.Width - ViewItem.CheckBox1.Width - 4;
  end else
    ViewItem := TControl(ConvertView) as TListViewItemCheck;

  ViewItem.Tag := Index;   // 使用 Tag 记录索引号
  ViewItem.OnClick := DoCheckChange;
  ViewItem.HeightSize := TViewSize.WrapContent;
  ViewItem.CheckBox1.IsChecked := ItemCheck[Index];
  ViewItem.TextView1.Text := Items[Index];
  ViewItem.Height := ViewItem.TextView1.Height;
  Result := TViewBase(ViewItem);
end;

procedure TStringsListCheckAdapter.Insert(const Index: Integer;
  const V: string);
begin
  inherited;
  if FChecks.Len >= Index + 1 then
    FChecks.Insert(Index, False);
end;

procedure TStringsListCheckAdapter.SetChecks(const Value: TArray<Boolean>);
begin
  FChecks := Value;
  Repaint;
end;

procedure TStringsListCheckAdapter.SetItemCheck(const Index: Integer;
  const Value: Boolean);
begin
  if FChecks.Len < Count then
    FChecks.Len := Count;
  FChecks[Index] := Value;
end;

{ TStringsListSingleAdapter }

procedure TStringsListSingleAdapter.DoInitData;
begin
  inherited DoInitData;
  FItemIndex := -1;
end;

procedure TStringsListSingleAdapter.DoItemIndexChange(Sender: TObject);
begin
  FItemIndex := TControl(Sender).Tag;
  if Sender is TListViewItemSingle then
    TListViewItemSingle(Sender).RadioButton.IsChecked := True;
end;

function TStringsListSingleAdapter.GetView(const Index: Integer;
  ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
var
  ViewItem: TListViewItemSingle;
begin
  if (ConvertView = nil) or (not (TControl(ConvertView) is TListViewItemSingle)) then begin
    ViewItem := TListViewItemSingle.Create(Parent);
    ViewItem.WidthSize := TViewSize.FillParent;
    ViewItem.HeightSize := TViewSize.WrapContent;
    ViewItem.MinHeight := TListTextItem.C_MinHeight;
    ViewItem.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.Parent := Parent;
    ViewItem.Width := Parent.Width;

    ViewItem.TextView1 := TTextView.Create(ViewItem);
    ViewItem.TextView1.WidthSize := TViewSize.FillParent;
    ViewItem.TextView1.Weight := 1;
    ViewItem.TextView1.HeightSize := TViewSize.WrapContent;
    ViewItem.TextView1.TextSettings.Font.Size := TListTextItem.C_FontSize;
    ViewItem.TextView1.TextSettings.WordWrap := True;
    ViewItem.TextView1.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.TextView1.Margins.Rect := RectF(0, 0, 4, 0);
    ViewItem.TextView1.Padding.Rect := RectF(8, 8, 8, 8);
    ViewItem.TextView1.AutoSize := True;
    ViewItem.TextView1.Parent := ViewItem;

    ViewItem.RadioButton := TRadioButton.Create(ViewItem);
    ViewItem.RadioButton.Width := 42;
    ViewItem.RadioButton.Height := ViewItem.MinHeight;
    ViewItem.RadioButton.Parent := ViewItem;
    ViewItem.RadioButton.Text := '';
    ViewItem.RadioButton.HitTest := False;
    ViewItem.TextView1.Width := ViewItem.Width - ViewItem.RadioButton.Width - 4;
  end else
    ViewItem := TControl(ConvertView) as TListViewItemSingle;

  ViewItem.Tag := Index; // 使用 Tag 记录索引号
  ViewItem.OnClick := DoItemIndexChange;
  ViewItem.HeightSize := TViewSize.WrapContent;
  ViewItem.RadioButton.IsChecked := FItemIndex = Index;
  ViewItem.TextView1.Text := Items[Index];
  ViewItem.Height := ViewItem.TextView1.Height;
  Result := TViewBase(ViewItem);
end;

procedure TStringsListSingleAdapter.SetItemIndex(const Value: Integer);
begin
  FItemIndex := Value;
end;

end.
