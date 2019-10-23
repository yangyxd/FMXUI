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
  UI.Debug, UI.Utils, UI.Base, UI.Standard, UI.Utils.ArrayEx, UI.Ani,
  {$IFDEF MSWINDOWS}
  Windows, FMX.Platform.Win,
  {$ENDIF}
  FMX.Utils, FMX.ImgList, FMX.MultiResBitmap, FMX.ActnList, System.Rtti, FMX.Consts,
  FMX.TextLayout, FMX.Objects, System.ImageList, System.RTLConsts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, System.Math,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement,
  FMX.Styles.Objects, FMX.Forms;

const
  ListViewType_Default = 0;
  ListViewType_Remove = -1;

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
    function DisableCache: Boolean;
    function GetCount: Integer;
    function GetItemID(const Index: Integer): Int64;
    function GetItem(const Index: Integer): Pointer;
    function IndexOf(const AItem: Pointer): Integer;
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
    function GetItemViewType(const Index: Integer): Integer;
    function IsEmpty: Boolean;
    function IsEnabled(const Index: Integer): Boolean;
    function ItemDefaultHeight: Single;
    procedure ItemMeasureHeight(const Index: Integer; var AHeight: Single);
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
  /// 列表项调节时的状态数据
  /// </summary>
  TListViewRealginState = record
  private
    function GetIsUp: Boolean;
  public
    Left: Single;           // 左边开始坐标
    Width: Single;          // 总宽度
    Height: Single;         // 总高度
    MoveSpace: Double;      // 滚动距离
    ScrollValue: Double;    // 滚动条位置
    ColumnW: Single;        // 列宽
    ColumnCount: Integer;   // 当前列数
    CheckViews: Boolean;    // 是否需要检查列表项是否可视。全部重新排列时，不检查
    ItemDefaultH: Double;   // 默认行高
    DividerH: Double;       // 分隔条高度
    DividerW: Double;       // 垂直分隔条宽度
    IsAutoSize: Boolean;    // 是否是自动大小
    Bottom: Double;         // 显示列表项最大底部位置,仅向上滚动时有效

    Adjust: Double;         // 需要调节的大小
    OnItemMeasureHeight: TOnItemMeasureHeight;

    property IsUp: Boolean read GetIsUp;
  end;
  PListViewRealginState = ^TListViewRealginState;

  /// <summary>
  /// 列表视图内容区域
  /// </summary>
  TListViewContent = class(TView)
  private
    [Weak] ListView: TListViewEx;
    [Weak] FAdapter: IListAdapter;
    FIsDesigning: Boolean;                    // 是否为设计模式

    FViews: TDictionary<Integer, TViewBase>;  // 当前显示的控件列表
    FCacleViews: TDictionary<Integer, TListViewList>; // 缓存的控件

    //FItemViews: TDictionary<Pointer, Integer>; // 当前显示的控件及索引号
    FItemViews: TIntHash; // 当前显示的控件及索引号
    FItemClick: TDictionary<Pointer, TNotifyEvent>; // 当前显示的控件的原始事件字典

    FFirstRowIndex: Integer;  // 当前显示的第一行行号
    FLastRowIndex: Integer;   // 当前显示的最后一行行号
    FCount: Integer;          // 列表项总数

    FLastW, FLastH: Single;   // 最后一次排列时，组件的宽度和高度
    FLastScrollValue: Double; // 上次排列时，滚动条位置
    FMaxParentHeight: Double;  // 父级控件最大高度（当值>0时，根据列表高度自动调整大小)

    FViewTop: Double;         // 当前显示列表项的顶部位置
    FViewBottom: Double;      // 当前显示的内容底部位置
    FViewItemBottom: Double;  // 当前显示列表项的底部位置

    FDividerBrush: TBrush;    // 分隔线绘制刷子

    FLastColumnCount: Integer;  // 当前显示列数
    FLastColumnWidth: Single;   // 当前显示的列宽

    function GetVisibleRowCount: Integer;
    function GetControlFormCacle(const ItemType: Integer): TViewBase;
    procedure AddControlToCacle(const ItemType: Integer; const Value: TViewBase);
    function GetAbsoluteColumnCount: Integer;
    function GetAbsoluteColumnWidth: Single;
    procedure SetViewTop(const Value: Double);

  protected
    function ObjectAtPoint(AScreenPoint: TPointF): IControl; override;
  protected
    procedure DoRealign; override;

    procedure DoGravity(); override;
    procedure DrawDivider(Canvas: TCanvas); virtual;   // 画分隔线
    procedure DoChangeSize(var ANewWidth, ANewHeight: Single); override;
    procedure DoPaintFrame(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);

    procedure Resize; override;
    procedure AfterPaint; override;
    procedure PaintBackground; override;               // 画列表背景

  protected
    procedure DoItemClick(Sender: TObject);
    procedure DoItemChildClick(Sender: TObject);
    procedure DoFooterClick(Sender: TObject);
    procedure DoMouseDownFrame(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ClearViews;        // 清除当前显示的列表项，并清空缓存
    procedure HideViews;         // 隐藏当前显示的列表项

  protected
    // 下拉刷新，上拉加载更多
    FState: TListViewState;      // 列表视图状态
    FHeader: IListViewHeader;    // 头部下拉刷新列表视图
    FFooter: IListViewHeader;    // 尾部上拉加载更多视图

    FColumnCount: Integer;       // 用户设置的列数
    FColumnWidth: Single;        // 用户设置的列度
    FColumnDivider: Boolean;     // 是否显示列分隔线

    FHeaderView: TControl;       // 额外的头部视图
    FFooterView: TControl;       // 额外的尾部视图

    procedure InitFooter(); virtual;
    procedure InitHeader(); virtual;
    procedure FreeHeader(); virtual;
    procedure FreeFooter(); virtual;
    procedure DoPullLoadComplete; virtual;
    procedure DoPullRefreshComplete; virtual;

    property ViewTop: Double read FViewTop write SetViewTop;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// 在列表头部添加一个View
    /// </summary>
    procedure AddHeaderView(const View: TControl); overload;
    function AddHeaderView(const View: TViewClass): TControl; overload;
    /// <summary>
    /// 删除添加在列表头部的View
    /// </summary>
    procedure RemoveHeaderView();

    /// <summary>
    /// 在列表底部添加一个View
    /// </summary>
    procedure AddFooterView(const View: TControl); overload;
    function AddFooterView(const View: TViewClass): TControl; overload;
    /// <summary>
    /// 删除添加在列表底部的View
    /// </summary>
    procedure RemoveFooterView();

    procedure PullRefreshStart;

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
    /// <summary>
    /// 当前真实显示的列表
    /// </summary>
    property AbsoluteColumnCount: Integer read GetAbsoluteColumnCount;
    /// <summary>
    /// 当前真实显示的列宽
    /// </summary>
    property AbsoluteColumnWidth: Single read GetAbsoluteColumnWidth;

    property State: TListViewState read FState write FState;
  end;

  /// <summary>
  /// 列表框视图
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TListViewEx = class(TScrollView)
  private const
    CDefaultDividerColor = $afe3e4e5;    // 默认行列分隔线颜色
    CDefaultBKPressedColor = $ffd9d9d9;  // 默认列表项按下时背景颜色
  private
    FAdapter: IListAdapter;
    FDivider: TAlphaColor;
    FDividerHeight: Single;
    FItemsPoints: TArray<TListItemPoint>;
    FContentViews: TListViewContent;
    FLocalDividerHeight: Single;
    FAllowItemChildClick: Boolean;
    FLastHeight, FLastWidth: Single;
    FResizeing: Boolean;

    FEnablePullRefresh: Boolean;
    FEnablePullLoad: Boolean;
    FCount: Integer;

    FMaxListItemBottom: Double;

    {$IFNDEF NEXTGEN}
    FDownPos, FMovePos: TPointF;
    {$ENDIF}

    FOnDrawViewBackgroud: TOnDrawViewBackgroud;
    FOnItemMeasureHeight: TOnItemMeasureHeight;
    FOnItemClick: TOnItemClick;
    FOnItemClickEx: TOnItemClickEx;

    FOnInitFooter: TOnInitHeader;
    FOnInitHeader: TOnInitHeader;
    FOnPullRefresh: TNotifyEvent;
    FOnPullLoad: TNotifyEvent;

    procedure SetAdapter(const Value: IListAdapter);
    procedure SetDivider(const Value: TAlphaColor);
    procedure SetDividerHeight(const Value: Single);
    function GetItemPosition(Index: Integer): TListItemPoint;
    function GetFirstRowIndex: Integer;
    function GetLastRowIndex: Integer;
    function GetVisibleRowCount: Integer;
    function GetItemViews(Index: Integer): TControl;
    procedure SetEnablePullLoad(const Value: Boolean);
    procedure SetEnablePullRefresh(const Value: Boolean);
    function GetFooter: IListViewHeader;
    function GetHeader: IListViewHeader;
    function GetColumnCount: Integer;
    function GetColumnWidth: Single;
    function IsStoredColumnWidth: Boolean;
    procedure SetColumnCount(const Value: Integer);
    procedure SetColumnWidth(const Value: Single);
    function GetAbsoluteColumnCount: Integer;
    function GetAbsoluteColumnWidth: Single;
    function GetRowCount: Integer;
    function GetColumnDivider: Boolean;
    procedure SetColumnDivider(const Value: Boolean);
  protected
    function CreateScroll: TScrollBar; override;
    function GetRealDrawState: TViewState; override;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
    function IsStoredDividerHeight: Boolean; virtual;
  protected
    function GetCount: Integer;
    function IsEmpty: Boolean;
    procedure InvalidateContentSize(); override; // 计算内容区大小
    procedure DoRealign; override;
    procedure DoInVisibleChange; override;
    procedure DoScrollVisibleChange; override;

    procedure DoPullLoad(Sender: TObject);
    procedure DoColumnCountChange(const AColumnCount: Integer);

    //procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    procedure AniMouseUp(const Touch: Boolean; const X, Y: Single); override;
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
  protected
    {$IFNDEF NEXTGEN}
    [Weak] FPointTarget: IControl;
    FMouseEnter, FMouseDown: Boolean;
    {$ENDIF}
    function ObjectAtPoint(AScreenPoint: TPointF): IControl; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure CheckMouseLeftState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // 清空数据
    procedure Clear;

    // 通知数据发生改变
    procedure NotifyDataChanged; virtual;

    // 刷新开始
    procedure PullRefreshStart(); override;
    // 刷新完成
    procedure PullRefreshComplete();
    // 加载更多完成
    procedure PullLoadComplete();

    function IsEnabled(const Index: Integer): Boolean;

    /// <summary>
    /// 在列表头部添加一个View
    /// </summary>
    procedure AddHeaderView(const View: TControl); overload;
    function AddHeaderView(const View: TViewClass): TControl; overload;
    /// <summary>
    /// 删除添加在列表头部的View
    /// </summary>
    procedure RemoveHeaderView();

    /// <summary>
    /// 在列表底部添加一个View
    /// </summary>
    procedure AddFooterView(const View: TControl); overload;
    function AddFooterView(const View: TViewClass): TControl; overload;
    /// <summary>
    /// 删除添加在列表底部的View
    /// </summary>
    procedure RemoveFooterView();

    property Count: Integer read GetCount;
    property Empty: Boolean read IsEmpty;
    property Adapter: IListAdapter read FAdapter write SetAdapter;
    property ItemPosition[Index: Integer]: TListItemPoint read GetItemPosition;
    property ItemViews[Index: Integer]: TControl read GetItemViews;
    property ContentViews: TListViewContent read FContentViews;

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
    /// <summary>
    /// 当前真实显示的列表
    /// </summary>
    property AbsoluteColumnCount: Integer read GetAbsoluteColumnCount;
    /// <summary>
    /// 当前真实显示的列宽
    /// </summary>
    property AbsoluteColumnWidth: Single read GetAbsoluteColumnWidth;
    /// <summary>
    /// 总行数
    /// </summary>
    property RowCount: Integer read GetRowCount;

    property Header: IListViewHeader read GetHeader;
    property Footer: IListViewHeader read GetFooter;
  published
    /// <summary>
    /// 是否允许触发列表项中的子控件事件
    /// </summary>
    property AllowItemClickEx: Boolean read FAllowItemChildClick write FAllowItemChildClick default True;
    /// <summary>
    /// 每行显示的列数，列数必须 >= 1
    /// </summary>
    property ColumnCount: Integer read GetColumnCount write SetColumnCount default 1;
    /// <summary>
    /// 每列的宽度，默认-1，表示根据每行显示的列数自动调整。列宽>0时，ColumnCount 设置无效，将根据列宽自动计算
    /// </summary>
    property ColumnWidth: Single read GetColumnWidth write SetColumnWidth stored IsStoredColumnWidth;
    /// <summary>
    /// 显示列分割线
    /// </summary>
    property ColumnDivider: Boolean read GetColumnDivider write SetColumnDivider default True;
    /// <summary>
    /// 分隔线颜色
    /// </summary>
    property Divider: TAlphaColor read FDivider write SetDivider;
    /// <summary>
    /// 分隔线高度
    /// </summary>
    property DividerHeight: Single read FDividerHeight write SetDividerHeight stored IsStoredDividerHeight;

    property ShowScrollBars;
    property ScrollbarWidth;
    //property ScrollBars default TViewScroll.Vertical;
    property ScrollSmallChangeFraction;
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
    property DragScroll;

    /// <summary>
    /// 是否启用下拉刷新
    /// </summary>
    property EnablePullRefresh: Boolean read FEnablePullRefresh write SetEnablePullRefresh;
    /// <summary>
    /// 是否启用上拉加载更多
    /// </summary>
    property EnablePullLoad: Boolean read FEnablePullLoad write SetEnablePullLoad;

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
    procedure ItemMeasureHeight(const Index: Integer; var AHeight: Single); virtual;
  protected
    { IListAdapter }
    function DisableCache: Boolean; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetItem(const Index: Integer): Pointer; virtual;
    function IndexOf(const AItem: Pointer): Integer; virtual;
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
    function GetItems: TList<T>;
    procedure SetItems(const Value: TList<T>);
  protected
    FList: TList<T>;
    FListNeedFree: Boolean;
    function GetCount: Integer; override;
    function GetItem(const Index: Integer): Pointer; override;
    function IndexOf(const AItem: Pointer): Integer; override;
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
    FDefaultItemHeight: Single;
    function GetItemValue(const Index: Integer): string;
    procedure SetItemValue(const Index: Integer; const Value: string);
    procedure SetArray(const Value: TArray<string>);
    procedure SetList(const Value: TStrings);
    function GetList: TStrings;
    function GetArray: TArray<string>;
  protected
    FListNeedFree: Boolean;
    FWordWrap: Boolean;
    FFontSize: Single;
    FItemIndex: Integer;
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
    procedure SetArrayLength(const ACount: Integer); virtual;
    property Items[const Index: Integer]: string read GetItemValue write SetItemValue; default;
    property Strings: TStrings read GetList write SetList;
    property StringArray: TArray<string> read GetArray write SetArray;
    property DefaultItemHeight: Single read FDefaultItemHeight write FDefaultItemHeight;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property FontSize: Single read FFontSize write FFontSize;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
  end;

  /// <summary>
  /// 带图标的字符串列表适配器
  /// </summary>
  TStringsListIconAdapter = class(TStringsListAdapter)
  private
    FImages: TCustomImageList;
    FIconSize: TSize;
    FPadding: Integer;
    FPosition: TDrawablePosition;
  protected
    function GetItemImageIndex(const Index: Integer): Integer; virtual;
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
    procedure DoInitData; override;
  public
    property Images: TCustomImageList read FImages write FImages;
    property IconSize: TSize read FIconSize write FIconSize;
    property Padding: Integer read FPadding write FPadding;
    property Position: TDrawablePosition read FPosition write FPosition;
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
    function DisableCache: Boolean; override;
    procedure DoCheckChange(Sender: TObject);
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
  public
    procedure SetArrayLength(const ACount: Integer); override;
    procedure Insert(const Index: Integer; const V: string); override;
    procedure Delete(const Index: Integer); override;
    property Checks: TArray<Boolean> read GetChecks write SetChecks;
    property ItemCheck[const Index: Integer]: Boolean read GetItemCheck write SetItemCheck;
  end;

  /// <summary>
  /// 单选列表适配器
  /// </summary>
  TStringsListSingleAdapter = class(TStringsListAdapter)
  protected
    procedure DoInitData; override;
    procedure DoItemIndexChange(Sender: TObject);
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
  end;

type
  /// <summary>
  /// 树形列表节点数据
  /// </summary>
  TTreeListNode<T> = class(TObject)
  private
    function GetCount: Integer;
    function GetNode(const Index: Integer): TTreeListNode<T>;
    procedure SetNode(const Index: Integer; const Value: TTreeListNode<T>);
    procedure SetParent(const Value: TTreeListNode<T>);
    function GetParentIndex: Integer;
  protected
    FParent: TTreeListNode<T>;
    FNodes: TList<TTreeListNode<T>>;
    FExpanded: Boolean;
    FLevel: Integer;
    FData: T;
    procedure DoNodeNotify(Sender: TObject; const Item: TTreeListNode<T>;
      Action: System.Generics.Collections.TCollectionNotification);
    procedure CreateNodes; virtual;
    procedure InnerRemove(const ANode: TTreeListNode<T>);
    procedure UpdateLevel;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteSelf;

    procedure Add(const ANode: TTreeListNode<T>);
    procedure Remove(const ANode: TTreeListNode<T>);
    procedure Insert(const Index: Integer; const ANode: TTreeListNode<T>);

    function AddNode(const AData: T): TTreeListNode<T>;
    function InsertNode(const Index: Integer; const AData: T): TTreeListNode<T>;

    property Data: T read FData write FData;
    property Count: Integer read GetCount;
    property Nodes[const Index: Integer]: TTreeListNode<T> read GetNode write SetNode; default;
    property Expanded: Boolean read FExpanded write FExpanded;
    property Parent: TTreeListNode<T> read FParent write SetParent;
    property Level: Integer read FLevel;
    property Index: Integer read GetParentIndex;
  end;

  /// <summary>
  /// 树形列表数据适配器基类
  /// </summary>
  TCustomTreeListDataAdapter<T> = class(TListAdapterBase)
  private
    FUpdateRef: Integer;
    function GetNodes(const Index: Integer): TTreeListNode<T>;
    function GetNodeCount: Integer;
    procedure AddListItem(const Parent: TTreeListNode<T>);
  protected
    FRoot: TTreeListNode<T>;
    FList: TList<TTreeListNode<T>>;
    function GetCount: Integer; override;
    function GetItem(const Index: Integer): Pointer; override;
    function IndexOf(const AItem: Pointer): Integer; override;
    function GetItemViewType(const Index: Integer): Integer; override;
    procedure ItemMeasureHeight(const Index: Integer; var AHeight: Single); override;

    function GetView(const Index: Integer; ConvertView: TViewBase;
      Parent: TViewGroup): TViewBase; override;

    function GetNodeGroupView(const Index: Integer; const ANode: TTreeListNode<T>;
      ConvertView: TViewBase; Parent: TViewGroup): TViewBase; virtual;
    function GetNodeItemView(const Index: Integer; const ANode: TTreeListNode<T>;
      ConvertView: TViewBase; Parent: TViewGroup): TViewBase; virtual;

    function GetNodeText(const ANode: TTreeListNode<T>): string; virtual;

    procedure InitList; virtual;
    procedure DoNodeExpandChange(Sender: TObject); virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;
    procedure NotifyDataChanged; override;
    procedure Clear; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Root: TTreeListNode<T> read FRoot;
    property Nodes[const Index: Integer]: TTreeListNode<T> read GetNodes;
    property NodeCount: Integer read GetNodeCount;
  end;

implementation

uses
  UI.ListView.Header, UI.ListView.Footer, UI.ListView.TreeGroup;

{ TListViewEx }

procedure TListViewEx.AddHeaderView(const View: TControl);
begin
  FContentViews.AddHeaderView(View);
end;

procedure TListViewEx.AddFooterView(const View: TControl);
begin
  FContentViews.AddFooterView(View);
end;

function TListViewEx.AddFooterView(const View: TViewClass): TControl;
begin
  Result := FContentViews.AddFooterView(View);
end;

function TListViewEx.AddHeaderView(const View: TViewClass): TControl;
begin
  Result := FContentViews.AddHeaderView(View);
end;

procedure TListViewEx.AniMouseUp(const Touch: Boolean; const X, Y: Single);
begin
  inherited AniMouseUp(Touch, X, Y);

  // 下拉刷新处理
  if FEnablePullRefresh then begin
    if Assigned(FContentViews) and (FContentViews.FState = TListViewState.PullDownOK) then begin
      FContentViews.FHeader.DoUpdateState(TListViewState.PullDownFinish, 0);
      FContentViews.FState := TListViewState.PullDownFinish;
      if Assigned(FOnPullRefresh) then
        FOnPullRefresh(Self);
      Exit;
    end;
  end;

  // 上拉加载更多
  if FEnablePullLoad then begin
    if Assigned(FContentViews) and (FContentViews.FState = TListViewState.PullUpOK) then
      DoPullLoad(Self);
  end;
end;

function TListViewEx.CanRePaintBk(const View: IView;
  State: TViewState): Boolean;
begin
  Result := (State = TViewState.None) and (not AniCalculations.Animation);
end;

procedure TListViewEx.CheckMouseLeftState;
begin
  {$IFNDEF NEXTGEN}
  // 检查鼠标左键是否松开
  if DragScroll and (not FMouseEnter) then begin
    {$IFDEF MSWINDOWS}
    if GetAsyncKeyState(VK_LBUTTON) = 0 then
      MouseUp(TMouseButton.mbLeft, [], FMovePos.X, FMovePos.Y)
    else
      TFrameAnimator.DelayExecute(Self,
        procedure(Sender: TObject)
        begin
          CheckMouseLeftState;
        end,
      0.1);
    {$ENDIF}
  end;
  {$ENDIF}
end;

procedure TListViewEx.Clear;
begin
  if Assigned(FAdapter) then begin
    FAdapter.Clear;
    NotifyDataChanged;
    FCount := 0;
  end;
end;

procedure TListViewEx.Click;
begin
  inherited;
  {$IFNDEF NEXTGEN}
  if DragScroll and Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then
    TListViewEx(FPointTarget as TControl).Click;
  {$ENDIF}
end;

//procedure TListViewEx.CMGesture(var EventInfo: TGestureEventInfo);
//begin
//  if Assigned(FContentViews) then begin
//    if FContentViews.FState in [TListViewState.PullDownFinish, TListViewState.PullUpFinish] then
//      Exit;
//  end;
//  inherited CMGesture(EventInfo);
//end;

constructor TListViewEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  New(FContentBounds);
  CreateCoentsView();
  FAllowItemChildClick := True;
  FDivider := CDefaultDividerColor;
  FDividerHeight := -1;
  FLocalDividerHeight := -1;
  SetLength(FItemsPoints, 0);
  ScrollBars := TViewScroll.Vertical;
  DisableFocusEffect := True;
  AutoCapture := True;
  ClipChildren := True;
  with Background.ItemPressed do begin
    Color := CDefaultBKPressedColor;
    Kind := TViewBrushKind.Solid;
    DefaultColor := Color;
    DefaultKind := TBrushKind.Solid;
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
  FContentViews.WidthSize := TViewSize.FillParent;
  FContentViews.HeightSize := TViewSize.FillParent;
  if csDesigning in ComponentState then begin
    FContentViews.Align := TAlignLayout.Client;
  end else
    RealignContent;
end;

function TListViewEx.CreateScroll: TScrollBar;
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

destructor TListViewEx.Destroy;
begin
  FAdapter := nil;
  inherited Destroy;
end;

procedure TListViewEx.DoColumnCountChange(const AColumnCount: Integer);
begin
  FContentViews.FLastColumnCount := AColumnCount;
  FContentViews.FFirstRowIndex := -1;
  FContentViews.FLastRowIndex := -1;
  FContentViews.FViewBottom := 0;
  FContentViews.FViewItemBottom := 0;
  FContentViews.FViewTop := 0;
  NotifyDataChanged;
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

procedure TListViewEx.DoMouseEnter;
begin
  inherited;
  {$IFNDEF NEXTGEN}
  FMouseEnter := True;
  if DragScroll and Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then
    FPointTarget.DoMouseEnter;
  {$ENDIF}
end;

procedure TListViewEx.DoMouseLeave;
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

procedure TListViewEx.DoPaintBackground(var R: TRectF);
begin
  R := RectF(R.Left + Padding.Left, R.Top + Padding.Top,
    R.Right - Padding.Right, R.Bottom - Padding.Bottom);
end;

procedure TListViewEx.DoPullLoad(Sender: TObject);
begin
  FContentViews.FFooter.DoUpdateState(TListViewState.PullUpFinish, 0);
  FContentViews.FState := TListViewState.PullUpFinish;
  if Assigned(FOnPullLoad) then
    FOnPullLoad(Self);
end;

procedure TListViewEx.DoRealign;
var
  LDisablePaint: Boolean;
  W: Single;
  I: Integer;
begin
  if FDisableAlign or IsUpdating then
    Exit;
  if (csDestroying in ComponentState) then
    Exit;
  LDisablePaint := FDisablePaint;
  try
    FDisablePaint := True;

    if csDesigning in ComponentState then begin
      inherited DoRealign;
      Exit;
    end;

    {$IFDEF MSWINDOWS}
    if Assigned(FScrollV) and (FScrollV.Visible) then
      W := Width - Padding.Right - Padding.Left{$IFDEF MSWINDOWS} - FScrollV.Width{$ENDIF}
    else
      W := Width - Padding.Right - Padding.Left;
    {$ELSE}
    W := Width - Padding.Right - Padding.Left;
    {$ENDIF}

    FContentViews.SetBounds(Padding.Left, Padding.Top, W,
      Height - Padding.Bottom - Padding.Top);

    inherited DoRealign;

    // 固定列宽
    if FContentViews.FColumnWidth > 0 then begin
      I := AbsoluteColumnCount;
      if I <> FContentViews.FLastColumnCount then
        DoColumnCountChange(I);
    end;

    if (HeightSize = TViewSize.WrapContent) and (FContentViews.FViewBottom > 0) and (Height <> FContentViews.FViewBottom) then begin
      FDisableAlign := True;
      BeginUpdate;
      SetSize(Width, FContentViews.FViewBottom + Padding.Top + Padding.Bottom);
      EndUpdate;
      FDisableAlign := False;
    end;

  finally
    FDisablePaint := LDisablePaint;
    FContentViews.Invalidate;
  end;
end;

procedure TListViewEx.DoScrollVisibleChange;
begin
  inherited DoScrollVisibleChange;
end;

function TListViewEx.GetAbsoluteColumnCount: Integer;
begin
  Result := FContentViews.GetAbsoluteColumnCount;
end;

function TListViewEx.GetAbsoluteColumnWidth: Single;
begin
  Result := FContentViews.GetAbsoluteColumnWidth;
end;

function TListViewEx.GetColumnCount: Integer;
begin
  Result := FContentViews.FColumnCount;
end;

function TListViewEx.GetColumnDivider: Boolean;
begin
  Result := FContentViews.FColumnDivider;
end;

function TListViewEx.GetColumnWidth: Single;
begin
  Result := FContentViews.FColumnWidth;
end;

function TListViewEx.GetCount: Integer;
begin
  Result := FCount;
end;

function TListViewEx.GetDividerHeight: Single;
begin
  if FLocalDividerHeight = -1 then
    FLocalDividerHeight := InnerCalcDividerHeight;
  Result := FLocalDividerHeight;
end;

function TListViewEx.GetRealDrawState: TViewState;
begin
  Result := TViewState.None;
end;

function TListViewEx.GetRowCount: Integer;
var
  LColumnCount: Integer;
begin
  Result := FCount;
  LColumnCount := FContentViews.AbsoluteColumnCount;
  if LColumnCount > 1 then begin
    if Result mod LColumnCount > 0 then
      Result := Result div LColumnCount + 1
    else
      Result := Result div LColumnCount;
  end;
end;

function TListViewEx.GetFirstRowIndex: Integer;
begin
  Result := FContentViews.FirstRowIndex;
end;

function TListViewEx.GetFooter: IListViewHeader;
begin
  Result := FContentViews.FFooter;
end;

function TListViewEx.GetHeader: IListViewHeader;
begin
  Result := FContentViews.FHeader;
end;

function TListViewEx.GetItemPosition(Index: Integer): TListItemPoint;
begin
  Result := FItemsPoints[Index];
end;

function TListViewEx.GetItemViews(Index: Integer): TControl;
begin
  Result := FContentViews.FViews.Items[Index];
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
  if FScrolling then Exit;
  inherited HScrollChange(Sender);
  if Assigned(FContentViews) then
    FContentViews.Realign;
end;

function TListViewEx.ObjectAtPoint(AScreenPoint: TPointF): IControl;
{$IFNDEF NEXTGEN}var P: TPointF; {$ENDIF}
begin
  Result := inherited;
  {$IFNDEF NEXTGEN}
  if DragScroll then begin // 如果允许拖动
    if FMouseDown then
      Exit;
    P := ScreenToLocal(AScreenPoint);
    if Assigned(Result) and (P.X < Width - 10) then begin
      FPointTarget := Result;
      Result := Self;
    end else
      FPointTarget := nil;
  end;
  {$ENDIF}
//  if Result <> nil then
//    LogD('ObjectAtPoint: ' + (Result as TObject).ClassName)
//  else
//    LogD('ObjectAtPoint: nil');
end;

function TListViewEx.InnerCalcDividerHeight: Single;
var
  PPI: Single;
begin
  if (FDividerHeight = -1) and (Assigned(Canvas)) then begin
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
  ItemDefaultH: Single;
  I: Integer;
  H, MH, DividerH: Double;
  LColCount: Integer;
begin
  // 调整列表项高度数组大小
  I := Count;
  if I > 0 then begin
    if I mod 1024 <> 0 then
      I := I div 1024 * 1024 + 1024;
  end;
  if I <> Length(FItemsPoints) then
    SetLength(FItemsPoints, I);

  FContentBounds^ := TRectD.Empty;
  H := 0;
  DividerH := GetDividerHeight;

  if Length(FItemsPoints) > 0 then begin

    ItemDefaultH := FAdapter.ItemDefaultHeight;

    // 计算出高度
    LColCount := FContentViews.AbsoluteColumnCount;
    if LColCount > 1 then begin
      MH := 0;
      for I := 0 to FCount - 1 do begin
        MH := Max(FItemsPoints[i].H, MH);
        if (I > 0) and ((I + 1) mod LColCount = 0) then begin
          if MH = 0 then
            H := H + DividerH + ItemDefaultH
          else
            H := H + DividerH + MH;
          MH := 0;
        end;
      end;
      if FCount mod LColCount > 0 then begin
        if MH = 0 then
          H := H + DividerH + ItemDefaultH
        else
          H := H + DividerH + MH;
      end;
    end else begin
      for I := 0 to FCount - 1 do begin
        if FItemsPoints[i].H = 0 then
          H := H + DividerH + ItemDefaultH
        else
          H := H + DividerH + FItemsPoints[i].H;
      end;
    end;

  end;

  FContentBounds.Right := FContentViews.Width;
  FContentBounds.Bottom := H;

  // 加上自定义附加头部
  if Assigned(FContentViews.FHeaderView) then
    FContentBounds.Bottom := FContentBounds.Bottom + FContentViews.FHeaderView.Height + DividerH;

  // 加上头部高度
  if (FEnablePullRefresh) and Assigned(FContentViews)
    and (FContentViews.FState = TListViewState.PullDownFinish)
  then
    FContentBounds.Bottom := FContentBounds.Bottom + (FContentViews.FHeader as TControl).Height;

  FMaxListItemBottom := FContentBounds.Bottom;

  // 加上自定义附加底部
  if Assigned(FContentViews.FFooterView) then
    FContentBounds.Bottom := FContentBounds.Bottom + FContentViews.FFooterView.Height + DividerH;

  // 加上底部高度
  if (FEnablePullLoad) and Assigned(FContentViews.FFooter)
    // and (FContentViews.FState <> TListViewState.PullUpComplete)
  then
    FContentBounds.Bottom := FContentBounds.Bottom + (FContentViews.FFooter as TControl).Height;
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

function TListViewEx.IsStoredColumnWidth: Boolean;
begin
  Result := ColumnWidth > 0;
end;

function TListViewEx.IsStoredDividerHeight: Boolean;
begin
  Result := FDividerHeight <> -1;
end;

procedure TListViewEx.Loaded;
begin
  inherited Loaded;
end;

procedure TListViewEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  {$IFDEF NEXTGEN}
  inherited;
  {$ELSE}
  if DragScroll then begin
    FDownPos.X := X;
    FDownPos.Y := Y;
    FMovePos := FDownPos;
    AniMouseDown(True, X, Y);

    if Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then begin
      TFrameAnimator.DelayExecute(Self,
        procedure (Sender: TObject)
        var
          P: TPointF;
        begin
          try
            if FMovePos <> FDownPos then Exit;
            if Assigned(FPointTarget) and (FPointTarget as TObject <> Self) then begin
              P := (FPointTarget as TControl).AbsoluteToLocal(LocalToAbsolute(PointF(X, Y)));
              FMouseDown := True;
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

procedure TListViewEx.MouseMove(Shift: TShiftState; X, Y: Single);
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

procedure TListViewEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
{$IFNDEF NEXTGEN}
var
  P: TPointF;
{$ENDIF}
begin
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

procedure TListViewEx.NotifyDataChanged;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) or FContentViews.FDisableAlign then
    Exit;
  FContentViews.FDisableAlign := True;
  try
    if Assigned(FAdapter) then
      FCount := FAdapter.Count
    else
      FCount := 0;

    FContentViews.HideViews;
    FContentViews.FLastScrollValue := -1;
    FContentViews.FLastH := 0;
    FContentViews.FLastW := 0;
    FContentViews.FCount := FCount;

    FMaxListItemBottom := 0;
    InvalidateContentSize;

    // 恢复位置
    if (FContentViews.Height > FContentBounds.Bottom) then begin
      FContentViews.FFirstRowIndex := -1;
      FContentViews.FViewTop := 0;
      FContentViews.FLastRowIndex := -1;
      FContentViews.FViewItemBottom := 0;
    end;
    if FContentViews.FLastRowIndex >= FCount then begin
      FContentViews.FLastRowIndex := FCount - 1;
      FContentViews.FViewItemBottom := FMaxListItemBottom;
    end;

    DoUpdateScrollingLimits(True);
  finally
    FContentViews.FDisableAlign := False;
    FContentViews.Realign;
  end;

  Resize;
  Invalidate;
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

procedure TListViewEx.PullLoadComplete;
begin
  if Assigned(FContentViews) and (FContentViews.FState = TListViewState.PullUpFinish) then
    FContentViews.DoPullLoadComplete;
end;

procedure TListViewEx.PullRefreshComplete;
begin
  if Assigned(FContentViews) and (FContentViews.FState = TListViewState.PullDownFinish) then
    FContentViews.DoPullRefreshComplete;
end;

procedure TListViewEx.PullRefreshStart;
begin
  if Assigned(FContentViews) then
    FContentViews.PullRefreshStart;
end;

procedure TListViewEx.RemoveFooterView;
begin
  FContentViews.RemoveFooterView;
end;

procedure TListViewEx.RemoveHeaderView;
begin
  FContentViews.RemoveHeaderView;
end;

procedure TListViewEx.Resize;
begin
  if FResizeing or
    (csLoading in ComponentState) or
    (csDestroying in ComponentState) or
    (csDesigning in ComponentState) then
    Exit;
  FResizeing := True;

  // 如果列表高度为自动大小时，计算一下父级视图的最大高度，在自动调整大小时会使用
  if HeightSize = TViewSize.WrapContent then
    FContentViews.FMaxParentHeight := GetParentMaxHeight
  else
    FContentViews.FMaxParentHeight := 0;

  inherited Resize;

  if Assigned(FAdapter) then begin
    UpdateScrollBar(FScrollV, FScrollbar);
    FContentViews.DoRealign;
    FLastHeight := Height;
    FLastWidth := Width;
  end;

  FResizeing := False;
end;

procedure TListViewEx.SetAdapter(const Value: IListAdapter);
begin
  if FAdapter <> Value then begin
    FAdapter := Value;
    FContentViews.FAdapter := Value;

    FContentViews.FFirstRowIndex := -1;
    FContentViews.FLastRowIndex := -1;
    FContentViews.FViewTop := 0;
    FContentViews.FViewBottom := 0;
    FContentViews.FViewItemBottom := 0;
    FContentViews.FLastScrollValue := 0;

    if FAdapter is TListAdapterBase then
      (FAdapter as TListAdapterBase).FListView := Self;

    NotifyDataChanged;
    HandleSizeChanged;
  end;
end;

procedure TListViewEx.SetColumnCount(const Value: Integer);
begin
  if FContentViews.FColumnCount <> Value then begin
    FContentViews.FColumnCount := Value;
    DoColumnCountChange(AbsoluteColumnCount);
    Invalidate;
  end;
end;

procedure TListViewEx.SetColumnDivider(const Value: Boolean);
begin
  if FContentViews.FColumnDivider <> Value then begin
    FContentViews.FColumnDivider := Value;
    Invalidate;
  end;
end;

procedure TListViewEx.SetColumnWidth(const Value: Single);
begin
  if FContentViews.FColumnWidth <> Value then begin
    FContentViews.FColumnWidth := Value;
    DoColumnCountChange(AbsoluteColumnCount);
    Invalidate;
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
    if csDesigning in ComponentState then
      Exit;
    if not (csLoading in ComponentState) then begin
      FLocalDividerHeight := FDividerHeight;
      RealignContent;
      Invalidate;
    end;
  end;
end;

procedure TListViewEx.SetEnablePullLoad(const Value: Boolean);
begin
  if FEnablePullLoad <> Value then begin
    FEnablePullLoad := Value;
    if Assigned(FContentViews) then begin
      if (not Value) then begin
        if csDesigning in ComponentState then
          FContentViews.FreeFooter
        else
          FContentViews.DoPullLoadComplete;
      end else begin
        FContentViews.InitFooter;
        if Assigned(FContentViews.FFooter) then begin
          FContentBounds.Bottom := FContentBounds.Bottom + (FContentViews.FFooter as TControl).Height;
          DoUpdateScrollingLimits(True);
        end;
        //FContentViews.FLastOffset := -1;
        DoRealign;
      end;
    end;
  end;
end;

procedure TListViewEx.SetEnablePullRefresh(const Value: Boolean);
begin
  if FEnablePullRefresh <> Value then begin
    FEnablePullRefresh := Value;
    if Assigned(FContentViews) then begin
      if (not Value) then begin
        if csDesigning in ComponentState then
          FContentViews.FreeHeader
        else
          FContentViews.DoPullRefreshComplete;
      end else
        FContentViews.InitHeader;
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
  if not FCacleViews.TryGetValue(ItemType, List) then begin
    List := TListViewList.Create;
    FCacleViews.Add(ItemType, List);
  end;
  Value.Visible := False;
  Value.OnClick := nil;
  List.Add(Value)
end;

procedure TListViewContent.AddHeaderView(const View: TControl);
begin
  if not Assigned(View) then Exit;
  if Assigned(FHeaderView) then begin
    RemoveObject(FHeaderView);
    FHeaderView := nil;
  end;
  View.Name := '';
  View.Parent := Self;
  View.Stored := False;
  View.Index := 0;
  View.Visible := False;
  FHeaderView := View;
end;

procedure TListViewContent.AddFooterView(const View: TControl);
begin
  if not Assigned(View) then Exit;
  if Assigned(FFooterView) then begin
    RemoveObject(FFooterView);
    FHeaderView := nil;
  end;
  View.Name := '';
  View.Parent := Self;
  View.Stored := False;
  View.Visible := False;
  FFooterView := View;
end;

function TListViewContent.AddFooterView(const View: TViewClass): TControl;
begin
  if not Assigned(View) then
    Result := nil
  else begin
    Result := View.Create(Self);
    AddFooterView(Result);
  end;
end;

function TListViewContent.AddHeaderView(const View: TViewClass): TControl;
begin
  if not Assigned(View) then
    Result := nil
  else begin
    Result := View.Create(Self);
    AddHeaderView(Result);
  end;
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
  if FCacleViews.Count > 0 then
    FCacleViews.Clear;
  if FItemViews.Count > 0 then
    FItemViews.Clear;
  if FViews.Count > 0 then begin
    for ItemView in FViews do
      RemoveObject(ItemView.Value);
    FViews.Clear;
  end;
end;

constructor TListViewContent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsDesigning := csDesigning in ComponentState;

  FViews := TDictionary<Integer, TViewBase>.Create(256);
  FCacleViews := TDictionary<Integer, TListViewList>.Create(17);
  //FItemViews := TDictionary<Pointer, Integer>.Create(256);
  FItemViews := TIntHash.Create(997);
  FItemClick := TDictionary<Pointer, TNotifyEvent>.Create(256);

  FDividerBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Null);

  FFirstRowIndex := -1;
  FLastRowIndex := -1;
  FViewBottom := 0;
  FViewItemBottom := 0;
  FViewTop := 0;

  FCount := 0;
  FMaxParentHeight := 0;

  FColumnCount := 1;
  FColumnWidth := -1;
  FColumnDivider := True;
  FLastColumnCount := AbsoluteColumnCount;
end;

destructor TListViewContent.Destroy;
begin
  ClearViews;
  FreeAndNil(FViews);
  FreeAndNil(FCacleViews);
  FreeAndNil(FDividerBrush);
  FreeAndNil(FItemViews);
  FreeAndNil(FItemClick);
  FreeHeader;
  FreeFooter;
  if Assigned(FHeaderView) then begin
    RemoveObject(FHeaderView);
    FHeaderView := nil;
  end;
  if Assigned(FFooterView) then begin
    RemoveObject(FFooterView);
    FFooterView := nil;
  end;
  inherited;
end;

procedure TListViewContent.DoChangeSize(var ANewWidth, ANewHeight: Single);
begin
  inherited DoChangeSize(ANewWidth, ANewHeight);
end;

procedure TListViewContent.DoFooterClick(Sender: TObject);
begin
  if ListView.FEnablePullLoad then begin
    if FState in [TListViewState.None, TListViewState.PullUpStart, TListViewState.PullUpOK] then
      ListView.DoPullLoad(Self);
  end;
end;

procedure TListViewContent.DoGravity;
begin
  Realign;
end;

procedure TListViewContent.DoItemChildClick(Sender: TObject);
var
  ItemIndex: NativeInt;
begin
  if FItemViews.TryGetValue(THashType(Sender), ItemIndex) then begin
    if FItemClick.ContainsKey(Sender) and Assigned(FItemClick[Sender]) then
      FItemClick[Sender](Sender);
    if Assigned(ListView.FOnItemClickEx) then
      ListView.FOnItemClickEx(ListView, ItemIndex, TControl(Sender));
  end;
end;

procedure TListViewContent.DoItemClick(Sender: TObject);
var
  ItemIndex: NativeInt;
begin
  if FItemViews.TryGetValue(THashType(Sender), ItemIndex) then begin
    if FItemClick.ContainsKey(Sender) and Assigned(FItemClick[Sender]) then
      FItemClick[Sender](Sender);
    if Assigned(ListView.FOnItemClick) then
      ListView.FOnItemClick(ListView, ItemIndex,  TView(FViews[ItemIndex]));
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
  if TControl(Sender).Pressed then
    Canvas.FillRect(ARect, 0, 0, [], Opacity, ListView.Background.ItemPressed);
end;

procedure TListViewContent.DoPullLoadComplete;
begin
  if Assigned(FFooter) then begin
    if FState = TListViewState.PullUpComplete then
      Exit;
    FFooter.DoUpdateState(TListViewState.PullUpComplete, 0);
    if FIsDesigning then begin
      RemoveObject(FFooter as TControl);
      FFooter := nil;
      FState := TListViewState.None;
      Exit;
    end;

    // 加载完成，回弹
    TFrameAnimator.DelayExecute(Self,
      procedure (Sender: TObject)
      var
        H: Single;
      begin
        try
          if (FState = TListViewState.PullUpFinish) then begin
            H := 0;
            if Assigned(FHeader) then begin
              H := (FHeader as TControl).Height;
              FHeader.DoUpdateState(TListViewState.None, 0);
            end;

            FState := TListViewState.None;
            if not ListView.FEnablePullLoad then
              ListView.FContentBounds.Bottom := ListView.FContentBounds.Bottom - H;
            ListView.DoUpdateScrollingLimits(True, 0);
            DoRealign;
          end;
        except
        end;
      end
    , 0.6);
  end;
end;

procedure TListViewContent.DoPullRefreshComplete;
begin
  if Assigned(FHeader) then begin
    if FState = TListViewState.PullDownComplete then
      Exit;
    FHeader.DoUpdateState(TListViewState.PullDownComplete, 0);
    if FIsDesigning then begin
      RemoveObject(FFooter as TControl);
      FHeader := nil;
      FState := TListViewState.None;
      Exit;
    end;
    // 刷新完成，回弹
    TFrameAnimator.DelayExecute(Self,
      procedure (Sender: TObject)
      var
        H: Single;
      begin
        try
          if (FState = TListViewState.PullDownFinish) then begin
            H := 0;
            if Assigned(FHeader) then
              H := (FHeader as TControl).Height;
            if H > 0 then begin
//              FHeader.DoUpdateState(TListViewState.None, 0);
//
//              ListView.FContentBounds.Bottom := ListView.FContentBounds.Bottom - H;
//              ListView.DoUpdateScrollingLimits(True, 0);
//
//              FState := TListViewState.None;
//              FLastScrollValue := FLastScrollValue - H;
//              ListView.VScrollBarValue := ListView.VScrollBarValue - H;

              ListView.FContentBounds.Bottom := ListView.FContentBounds.Bottom - H;
              ListView.DoUpdateScrollingLimits(True, 0);

              FState := TListViewState.PullChangeing;
              FLastScrollValue := FLastScrollValue - H;
              ListView.VScrollBarValue := ListView.VScrollBarValue - H;

              TFrameAnimator.DelayExecute(FHeader as TControl,
                procedure (Sender: TObject)
                begin
                  try
                    if (FState = TListViewState.PullChangeing) then begin
                      FState := TListViewState.None;
                      FHeader.DoUpdateState(TListViewState.None, 0);
                    end;
                  except
                  end;
                end
              , 0.22);
            end;
          end;
        except
        end;
      end
    , 0.3);
  end;
end;

procedure TListViewContent.DoRealign;

  // 递归设置组件及其子项的点击事件
  procedure SetChildClickEvent(const Parent: TControl; const Index: NativeInt);
  var
    I: Integer;
    Control: TControl;
  begin
    for I := 0 to Parent.ControlsCount - 1 do begin
      Control := Parent.Controls[I];
      if not Control.Visible then
        Continue;
      if Control.HitTest then begin
        Control.OnClick := DoItemChildClick;
        FItemViews.AddOrUpdate(THashType(Control), Index);
      end;
      if Control.ControlsCount > 0 then
        SetChildClickEvent(Control, Index);
    end;
  end;

  // 递归设置组件及其子项的点击事件所对应的索引号
  procedure UpdateChildEventIndex(const Parent: TControl; const Index: NativeInt);
  var
    I: Integer;
    Control: TControl;
  begin
    for I := 0 to Parent.ControlsCount - 1 do begin
      Control := Parent.Controls[I];
      if not Control.Visible then
        Continue;
      if Control.HitTest then
        FItemViews.AddOrUpdate(THashType(Control), Index);
      if Control.ControlsCount > 0 then
        SetChildClickEvent(Control, Index);
    end;
  end;

  // 设置当前列表中显示的组件的位置
  procedure UpdateListItemPosition(const LOffset: Double);
  var
    I: Integer;
    Ctrl: TControl;
  begin
    for I := 0 to Self.ControlsCount - 1 do begin
      Ctrl := Controls[I];
      Ctrl.Position.Y := Ctrl.Position.Y + LOffset;
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

  // 更新 Header 位置和状态
  procedure AdjustHeader(const S: Integer; var V: Double; const LS: TListViewRealginState);
  var
    Ctrl: TControl;
    H: Single;
    LV: Double;
  begin
    Ctrl := FHeader as TControl;
    H := Ctrl.Height;
    if FState = TListViewState.PullDownFinish then begin
      LV := - LS.ScrollValue;
      V := V + H;
    end else
      LV := -H - LS.ScrollValue;
    (FHeader as TControl).Visible := True;
    Ctrl.SetBounds(LS.Left, LV, FSize.Width, H);

    if LS.ScrollValue < 0 then begin
      case FState of
        TListViewState.None:
          begin
            FHeader.DoUpdateState(TListViewState.PullDownStart, LS.ScrollValue);
            FState := TListViewState.PullDownStart;
          end;
        TListViewState.PullDownStart:
          begin
            if (LV >= 0) then begin
              FHeader.DoUpdateState(TListViewState.PullDownOK, LS.ScrollValue);
              FState := TListViewState.PullDownOK;
            end;
          end;
        TListViewState.PullDownOK:
          begin
            if (LV < 0) then begin
              FHeader.DoUpdateState(TListViewState.PullDownStart, LS.ScrollValue);
              FState := TListViewState.PullDownStart;
            end;
          end;
      end;
    end else begin
      if FState = TListViewState.PullDownStart then
        FState := TListViewState.None;
    end;
  end;

  // 更新 Footer 位置和状态
  procedure AdjustFooter(const S: Integer; var V: Double; const LS: TListViewRealginState);
  var
    Ctrl: TControl;
    H: Single;
  begin
    Ctrl := FFooter as TControl;

    // 如果显示到了最后一行，说明已经滚动到最底下
    if (S >= FCount - 1) and ((LS.ScrollValue > 0) or (FState = TListViewState.PullUpFinish))
      and ((FHeader = nil) or (FHeader.Visible = False)) then
    begin
      H := Ctrl.Height;

      //LogD(Format('V: %.2f, ScrollV: %.2f, Top: %.2f, ScrollMove: %.2f', [V, LS.ScrollValue, V - LS.ScrollValue, LS.MoveSpace]));

      Ctrl.SetBounds(LS.Left, V - LS.ScrollValue, FSize.Width, H);
      Ctrl.Visible := True;
      Ctrl.HitTest := True;
      Ctrl.OnClick := DoFooterClick;

      case FState of
        TListViewState.None:
          begin
            FFooter.DoUpdateState(TListViewState.PullUpStart, LS.ScrollValue);
            FState := TListViewState.PullUpStart;
          end;
        TListViewState.PullUpStart:
          begin
            if (V - LS.ScrollValue + H + 8 <= LS.Height) then begin
              FFooter.DoUpdateState(TListViewState.PullUpOK, LS.ScrollValue);
              FState := TListViewState.PullUpOK;
            end;
          end;
        TListViewState.PullUpOK:
          begin
            if (V - LS.ScrollValue + H > LS.Height - 6) then begin
              FFooter.DoUpdateState(TListViewState.PullUpStart, LS.ScrollValue);
              FState := TListViewState.PullUpStart;
            end;
          end;
      end;

    end else begin
      Ctrl.Visible := False;
      if FState in [TListViewState.PullUpOK, TListViewState.PullUpStart] then
        FState := TListViewState.None;
    end;
  end;

  // 调整头部附加视图
  procedure AdjustHeaderView(const S: Integer; var V: Double; const LS: TListViewRealginState);
  var
    H: Double;
  begin
    if Assigned(FHeaderView) and (S = 0) then begin
      H := FHeaderView.Height;
      if LS.IsUp then begin
        V := V - H;
        FHeaderView.Visible := V - LS.ScrollValue + H > 0;
        FHeaderView.SetBounds(LS.Left, V - LS.ScrollValue, FSize.Width, H);
      end else begin
        FHeaderView.Visible := V - LS.ScrollValue + H > 0;
        FHeaderView.SetBounds(LS.Left, V - LS.ScrollValue, FSize.Width, H);
        V := V + H + LS.DividerH;
      end;
    end else if Assigned(FHeaderView) then
      FHeaderView.Visible := False;
  end;

  // 获取一个列表项, AH 返回控件的高度
  procedure DoGetView(const I: NativeInt; var V, H, X: Double; var AH: Single; var LS: TListViewRealginState);
  var
    MinH, MaxH: Single;
    Control, ItemView: TViewBase;
    View: TView;
    ItemType: Integer;
    FNewOnClick: TNotifyEvent;
  begin
    // 如果已经存在，说明之前加载过，并且正在显示，且已经调整好位置
    if FViews.ContainsKey(I) then begin
      AH := H;
      // 获取一个列表项视图
      if FAdapter.DisableCache() then
        ItemView := FAdapter.GetView(I, FViews[I], TViewGroup(Self))
      else
        ItemView := FViews[I];

      // 如果返回nil, 抛出错误
      if not Assigned(ItemView) then
        raise Exception.Create('View is null.');

      // 触发用户修改行高的事件
      FAdapter.ItemMeasureHeight(I, AH);
      if Assigned(LS.OnItemMeasureHeight) then
        LS.OnItemMeasureHeight(ListView, I, AH);

      // 更新 V, 代表列表项的底部位置
      if AH <= 0 then begin
        AH := 0;
        RemoveItemView(I, ItemView); // 移除
        Exit;
      end;

    end else begin
      // 如果不存在
      // 在缓存视图列表中取一个缓存的视图
      ItemType := FAdapter.GetItemViewType(I);
      Control := GetControlFormCacle(ItemType);

      // 获取一个列表项视图
      ItemView := FAdapter.GetView(I, Control, TViewGroup(Self));

      // 如果返回nil, 抛出错误
      if not Assigned(ItemView) then
        raise Exception.Create('View is null.');

      // 记录到可视列表中
      FViews.AddOrSetValue(I, ItemView);

      // 获取 View
      View := GetView(ItemView);

      // 如果当前项与缓存项不同，说明是新生成的, 初始化一些数据
      if Control <> ItemView then begin
        {$IFDEF Debug}
        {$IFDEF MSWINDOWS}
        //OutputDebugString(PChar(Format('增加列表视图 Index: %d, %s. (ViewCount: %d)',
        //  [I, ItemView.ClassName, FViews.Count])));
        {$ENDIF}
        {$IFDEF ANDROID}
        //LogD(Format('增加列表视图 Index: %d (ViewCount: %d)', [I, FViews.Count]));
        {$ENDIF}
        {$ENDIF}

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
      end else begin
        if ListView.FAllowItemChildClick then
          UpdateChildEventIndex(ItemView, I);
      end;

      // 记录列表项与索引号的对应关系和事件与视图对应关系到字典中
      FNewOnClick := DoItemClick;
      if Assigned(View) then begin
        if TViewState.Checked in View.ViewState then
          View.ViewState := [TViewState.Checked]
        else
          View.ViewState := [];
        FItemViews.AddOrUpdate(THashType(View), I);
        if Assigned(View.OnClick) and (not EqulsMethod(FNewOnClick, View.OnClick)) then
          FItemClick.AddOrSetValue(View, View.OnClick);
        View.OnClick := FNewOnClick;
      end else begin
        FItemViews.AddOrUpdate(THashType(View), I);
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
      FAdapter.ItemMeasureHeight(I, AH);
      if Assigned(LS.OnItemMeasureHeight) then
        LS.OnItemMeasureHeight(ListView, I, AH);

      if AH <= 0 then begin  // 移除
        AH := 0;
        RemoveItemView(I, ItemView);
        Exit;
      end;

      ItemView.Visible := True;

      if Assigned(View) then begin
        TListViewContent(View).FInVisible := ListView.FInVisible;
        if AH <> ItemView.Height then
          TListViewContent(View).HeightSize := TViewSize.CustomSize;
      end;

    end;

    if LS.IsUp then begin
      // 更新大小并显示出来
      ItemView.SetBounds(X, V - AH - LS.ScrollValue - LS.DividerH, LS.ColumnW, AH);

      // 更新完大小后，如果高度还是不一致，则使用实际的视图高度
      if ItemView.Height <> AH then begin
        AH := ItemView.Height;
        // 重新设置位置
        ItemView.SetBounds(X, V - AH - LS.ScrollValue - LS.DividerH, LS.ColumnW, AH);
      end;

    end else begin
      // 更新大小并显示出来
      ItemView.SetBounds(X, V - LS.ScrollValue, LS.ColumnW, AH);

      // 更新完大小后，如果高度还是不一致，则使用实际的视图高度
      if ItemView.Height <> AH then
        AH := ItemView.Height;
    end;
  end;

  // 向下滚动
  procedure DoRealignDown(var LS: TListViewRealginState);
  var
    First, Last: Double;
    V, H: Double;
    S, I, J, K, N, M: Integer;

    AL, MH, LMH: Double;
    IsMultiCol: Boolean;

    NewH: Single;
    Item: PListItemPoint;
  begin
    // 计算出当前可显示的第一行位置和最后一行位置
    First := FLastScrollValue;
    if LS.IsAutoSize then
      Last := First + FMaxParentHeight   // 使用父级视图的最大高度为列表项的底部位置
    else
      Last := First + LS.Height;         // 使用当前视图的高度作为底部位置

    // 计算出首行显示位置
    S := FFirstRowIndex;
    if S <= 0 then begin
      S := 0;
      V := 0;
    end else begin
      V := FViewTop;
    end;

    // 当首行显示位置大于0且显示的项目不是第一个时，是因为删除了最后一行引起，
    // 要进行调节，否则会导致上部分空白
    if (First > 0) and (V - First > 0) and (S > 0) then begin
      Item := @ListView.FItemsPoints[S];
      if Item.H > 0 then begin
        Dec(S);
        V := V - LS.DividerH - Item.H;
      end;
    end;

    K := S + FViews.Count;
    J := 0;

    //LogD(Format('V: %.2f, FirstIndex: %d. ScrollV: %.2f, ScrollM: %.2f. Down', [V, S, LS.ScrollValue, LS.MoveSpace]));

    FFirstRowIndex := -1;
    FLastRowIndex := -1;

    // 更新 Header 位置和状态
    if ListView.FEnablePullRefresh and Assigned(FHeader) and (S = 0) then
      AdjustHeader(S, V, LS)
    else if Assigned(FHeader) then
      (FHeader as TControl).Visible := False;

    // 自定义附加头部
    AdjustHeaderView(S, V, LS);

    MH := 0;
    LMH := 0;
    AL := 0;
    M := 0;
    IsMultiCol := LS.ColumnCount > 1;

    // 从指定位置开始，生成并调整列表项
    for I := S to FCount - 1 do begin
      if I < 0 then Continue;

      if M > 0 then begin
        // 多列时，第一行不显示时跳过同行的其它列。并删除相应的View
        Dec(M);
        if LS.CheckViews and FViews.ContainsKey(I) then
          RemoveItemView(I, FViews[I]);
        Continue;
      end;

      Item := @ListView.FItemsPoints[I];

      // 列数大于1时，换行时将AL坐标归0
      if IsMultiCol then begin
        if (I mod LS.ColumnCount = 0) then begin
          AL := 0;
          // 计算出下一项的位置
          if I <> S then begin
            V := V + MH + LS.DividerH;
          end;
          // 高度变化时，更新调整大小
          if MH <> LMH then begin
            if MH <= 0 then
              LS.Adjust := LS.Adjust - LMH
            else
              LS.Adjust := LS.Adjust + (MH - LMH)
          end;
          MH := 0;
          LMH := 0;
        end else
          AL := AL + LS.DividerW + LS.ColumnW;
      end;

      // 获取列表项高度
      H := Item.H;
      if H = 0 then
        H := LS.ItemDefaultH
      else if H < 0 then
        Continue;

      // 记录最大高度
      if IsMultiCol then
        LMH := Max(LMH, H)
      else
        LMH := H;

      // 判断列表项可视状态
      if AL = 0 then begin
        if (V + LMH + LS.DividerH <= First) then begin
          // 超出顶部可视区域
          // 如果已经显示，则将它删除
          if LS.CheckViews and FViews.ContainsKey(I) then
            RemoveItemView(I, FViews[I]);
          // 计算出下一项的位置
          if not IsMultiCol then
            V := V + LMH + LS.DividerH
          else begin
            M := LS.ColumnCount - 1;
            MH := LMH;
          end;
          Continue;
        end else if V > Last then begin
          // 超出尾部可视区域
          N := I;
          while N < K do begin
            if FViews.ContainsKey(N) then
              RemoveItemView(N, FViews[N]);
            Inc(N);
          end;
          Break;
        end;

        // 如果是第一个可视项, 记录状态
        if FFirstRowIndex = -1 then begin
          S := I;
          FFirstRowIndex := I;
          FViewTop := V;
          if I = 0 then
            Last := Last + Height - V;
        end;
      end;

      // 可视组件计数器增加
      Inc(J);

      // 获取列表项
      DoGetView(I, V, H, AL, NewH, LS);

      // 更新 V, 代表列表项的底部位置
      if (NewH > 0) and (not IsMultiCol) then
        V := V + NewH + LS.DividerH;

      // 如果行高更改了，则后续需要调整滚动区的大小，这里记录一下变化大小
      if not IsMultiCol then begin
        if Item.H <> NewH then begin
          if NewH <= 0 then begin
            LS.Adjust := LS.Adjust - H
          end else begin
            LS.Adjust := LS.Adjust + (NewH - H)
          end;
          Item.H := NewH;
        end;
      end else
        Item.H := NewH;

      MH := Max(MH, NewH);
    end;

    // 多列时调节剩下的大小变化
    if IsMultiCol then begin
      AL := 0;
      if (MH > 0) then begin
        // 计算出下一项的位置
        if J > 0 then
          V := V + MH + LS.DividerH;
        FViewBottom := V;
        // 高度变化时，更新调整大小
        if (MH <> LMH) then begin
          if MH <= 0 then
            LS.Adjust := LS.Adjust - LMH
          else
            LS.Adjust := LS.Adjust + (MH - LMH)
        end;
      end;
    end;
    FViewItemBottom := V;

    // 自定义附加尾部
    if Assigned(FFooterView) then begin
      if (S + J >= FCount) then begin
        H := FFooterView.Height;
        FFooterView.SetBounds(lS.Left, V - LS.ScrollValue, FSize.Width, H);
        FFooterView.Visible := True;
        V := V + H + LS.DividerH;
      end else
        FFooterView.Visible := False;
    end;

    // 更新 Footer 位置和状态
    if ListView.FEnablePullLoad and Assigned(FFooter) and (FCount > 0) then begin
      AdjustFooter(S + J, V, LS);
    end else if Assigned(FFooter) then
      (FFooter as TControl).Visible := False;

    FViewBottom := V;
    FFirstRowIndex := S;
    FLastRowIndex := S + J - 1;

    //LogD(Format('Bottom: %.2f, FirstIndex: %d, LastIndex: %d. Down', [FViewBottom, FFirstRowIndex, FLastRowIndex]));
  end;

  // 向上滚动
  procedure DoRealignUp(var LS: TListViewRealginState);
  var
    First, Last: Double;
    V, H: Double;
    S, I, J, K, N: Integer;

    AL, MH, LMH, MAL: Double;
    IsMultiCol, IsBreak: Boolean;

    NewH: Single;
    Item: PListItemPoint;
  begin
    // 计算出当前可显示的第一行位置和最后一行位置
    First := FLastScrollValue;
    //if LS.IsAutoSize then
    //  Last := First + FMaxParentHeight   // 使用父级视图的最大高度为列表项的底部位置
    //else
    Last := First + LS.Height;         // 使用当前视图的高度作为底部位置

    // 计算出最后行显示位置
    S := FLastRowIndex;
    if S >= FCount then
      S := FCount - 1;
    V := FViewItemBottom;

    //LogD(Format('V: %.2f, S: %d, ', [V, S]));
    IsMultiCol := LS.ColumnCount > 1;
    LMH := 0;
    MH := 0;

    if IsMultiCol then begin
      MAL := (LS.ColumnW + LS.DividerW) * (LS.ColumnCount - 1);
      if (S + 1) mod LS.ColumnCount = 0 then
        AL := MAL
      else
        AL := ((FCount mod LS.ColumnCount) - 1) * (LS.ColumnW + LS.DividerW);
    end else begin
      AL := 0;
      MAL := 0;
    end;

    // 自定义附加尾部
    if Assigned(FFooterView) then begin
      H := FFooterView.Height;
      if (S >= FCount - 1) then begin
        FFooterView.SetBounds(lS.Left, V - LS.ScrollValue, FSize.Width, H);
        FFooterView.Visible := True;
        V := V + H + LS.DividerH;
      end else begin
        FFooterView.Visible := False;
      end;
    end;

    // 更新 Footer 位置和状态
    if ListView.FEnablePullLoad and Assigned(FFooter) and (FCount > 0) then begin
      AdjustFooter(S, V, LS);
    end else if Assigned(FFooter) then
      (FFooter as TControl).Visible := False;

    V := FViewItemBottom;

    K := S - FViews.Count;
    J := 0;

    FFirstRowIndex := -1;
    FLastRowIndex := -1;

    // 从指定位置开始，生成并调整列表项
    for I := S downto 0 do begin
      if I >= FCount then Continue;
      Item := @ListView.FItemsPoints[I];

      // 获取列表项高度
      H := Item.H;
      if H = 0 then
        H := LS.ItemDefaultH
      else if H < 0 then
        Continue;

      // 列数大于1时，换行时将AL坐标归0
      if IsMultiCol then begin
        // 多列

        // 记录最大高度
        LMH := Max(LMH, H);

        // 是否需要换行？
        IsBreak := I mod LS.ColumnCount = 0;

        // 判断列表项可视状态
        if (V <= First) then begin
          // 超出顶部可视区域
          N := I;
          while N > K do begin
            if FViews.ContainsKey(N) then
              RemoveItemView(N, FViews[N]);
            Dec(N);
          end;
          Break;
        end else if (V - LMH - LS.DividerH) > Last then begin
          // 超出尾部可视区域
          // 如果已经显示，则将它删除
          if LS.CheckViews and FViews.ContainsKey(I) then
            RemoveItemView(I, FViews[I]);
          // 计算出下一项的位置
          if IsBreak then begin
            V := V - LMH - LS.DividerH;
            AL := MAL;
          end;
          Continue;
        end;

        // 如果是第一个可视项, 记录状态
        if FLastRowIndex = -1 then begin
          FLastRowIndex := I + J;
          FViewItemBottom := V;
        end;

        // 可视组件计数器增加
        Inc(J);

        // 获取列表项
        DoGetView(I, V, H, AL, NewH, LS);

        Item.H := NewH;
        MH := Max(MH, NewH);

        if IsBreak then begin
          AL := MAL;
          // 计算出下一项的位置
          if J > 0 then
            V := V - MH - LS.DividerH;
          // 高度变化时，更新调整大小
          if MH <> LMH then begin
            if MH <= 0 then
              LS.Adjust := LS.Adjust - LMH
            else
              LS.Adjust := LS.Adjust + (MH - LMH)
          end;
          MH := 0;
          LMH := 0;
        end else
          AL := AL - LS.DividerW - LS.ColumnW;

      end else begin

        // 单列
        if (V <= First) then begin
          // 超出顶部可视区域
          N := I;
          while N > K do begin
            if FViews.ContainsKey(N) then
              RemoveItemView(N, FViews[N]);
            Dec(N);
          end;
          Break;
        end else if (V - H - LS.DividerH) >= Last then begin
          // 超出尾部可视区域
          // 如果已经显示，则将它删除
          if LS.CheckViews and FViews.ContainsKey(I) then
            RemoveItemView(I, FViews[I]);
          // 计算出下一项的位置
          V := V - H - LS.DividerH;
          Continue;
        end;

        // 如果是第一个可视项, 记录状态
        if FLastRowIndex = -1 then begin
          FLastRowIndex := I + J;
          FViewItemBottom := V;
        end;

        // 可视组件计数器增加
        Inc(J);

        // 获取列表项
        DoGetView(I, V, H, AL, NewH, LS);

        // 更新 V, 代表列表项的底部位置
        if (NewH > 0) then
          V := V - NewH - LS.DividerH;

        // 如果行高更改了，则后续需要调整滚动区的大小，这里记录一下变化大小
        if Item.H <> NewH then begin
          if NewH <= 0 then begin
            LS.Adjust := LS.Adjust - H
          end else begin
            LS.Adjust := LS.Adjust + (NewH - H)
          end;
          Item.H := NewH;
        end;
      end;

    end;

    // 记录底部位置
    FViewTop := V;
    FFirstRowIndex := FLastRowIndex - J + 1;

    // 自定义附加头部
    S := FFirstRowIndex;
    V := FViewTop - LS.DividerH;
    AdjustHeaderView(S, V, LS);

    // 更新 Header 位置和状态
    if ListView.FEnablePullRefresh and Assigned(FHeader) and (S = 0) then
      AdjustHeader(S, V, LS)
    else if Assigned(FHeader) then
      (FHeader as TControl).Visible := False;

    if LS.Adjust <> 0 then begin
      FViewTop := FViewTop + LS.Adjust;
      FViewBottom := FViewBottom + LS.Adjust;
      FViewItemBottom := FViewItemBottom + LS.Adjust;
    end;

    //LogD(Format('Bottom: %.2f, FirstIndex: %d, LastIndex: %d. Up', [FViewBottom, FFirstRowIndex, FLastRowIndex]));
  end;

var
  LDisablePaint: Boolean;
  LS: TListViewRealginState;
begin
  // 设计模式不处理
  if FIsDesigning then begin
    inherited DoRealign;
    Exit;
  end;
  // 正在调整中不处理
  if FDisableAlign or (FAdapter = nil) or (not Assigned(Canvas))or
    (csLoading in ComponentState) or
    (csDestroying in ComponentState) then
    Exit;
  // 列表项为0时
  if (FCount = 0) then begin
    if Assigned(FFooter) then
      (FFooter as TControl).Visible := False;
    if (not Assigned(FHeader)) and (not Assigned(FHeaderView)) then
      Exit;
  end;

  LDisablePaint := FDisablePaint;
  FDisableAlign := True;
  FDisablePaint := True;

  // 偏移位置 (滚动条位置)
  LS.ScrollValue := ListView.VScrollBarValue;
  // 滚动距离
  LS.MoveSpace := LS.ScrollValue - FLastScrollValue;
  // 记录滚动条值
  FLastScrollValue := LS.ScrollValue;
  // 当前的列数
  LS.ColumnCount := FLastColumnCount;
  // 分隔条高度
  LS.DividerH := ListView.GetDividerHeight;
  if FColumnDivider then
    LS.DividerW := LS.DividerH
  else
    LS.DividerW := 0;

  if (FLastColumnCount > 1) or (FColumnWidth > 0) then begin
    // 多列或固定列宽时
    LS.ColumnW := GetAbsoluteColumnWidth;
    // 记录当前绘制时的列宽
    FLastColumnWidth := LS.ColumnW;
  end else
    LS.ColumnW := FSize.Width;
  LS.Height := Height;
  LS.Width := FSize.Width;

  // 宽度、高度、滚动条均无变化，不处理
  if (FLastW = LS.Width) and (FLastH = LS.Height) and (LS.MoveSpace = 0) then begin
    FDisableAlign := False;
    FDisablePaint := LDisablePaint;
    Exit;
  end else begin
    // 记录下本次排列时的列表区域大小
    FLastW := LS.Width;
    FlastH := LS.Height;
  end;

  //LogD(Format('ScrollValue: %.2f', [LS.ScrollValue]));

  {$IFDEF MSWINDOWS}
  {$IFDEF DEBUG}
  //OutputDebugString(PChar('TListViewContent.DoRealign. MoveSpace: ' + FloatToStr(LS.MoveSpace)));
  {$ENDIF}
  {$ENDIF}

  // 如果一次的滚动距离大于可视区域高度的两倍，则直接先隐藏所有列表项重新排列
  if (Abs(LS.MoveSpace) > LS.Height * 2) then begin
    HideViews;
    LS.CheckViews := False;
  end else
    LS.CheckViews := FViews.Count > 0;

  //LogD(Format('Height: %.2f; ViewCount: %d.', [Height, FViews.Count]));

  // 默认行高
  LS.ItemDefaultH := FAdapter.ItemDefaultHeight;

  // 初始化
  LS.Adjust := 0;
  LS.Left := 0;
  LS.OnItemMeasureHeight := ListView.FOnItemMeasureHeight;

  // 修正向上拉出全部区域后，回弹时显示错误的问题
  if LS.ScrollValue = 0 then begin
    FFirstRowIndex := 0;
    LS.MoveSpace := 0;
  end;

  BeginUpdate;
  try
    if (LS.MoveSpace >= 0) then begin
      LS.IsAutoSize := (FMaxParentHeight > 0) and (LS.ScrollValue = 0);  // 如果需要自动调整大小，且滚动条偏移为0时，说明正在初始化列表
      DoRealignDown(LS);   // 向下滚动
    end else begin
      LS.IsAutoSize := False;
      DoRealignUp(LS);    // 向上滚动
    end;

  finally
    FDisablePaint := LDisablePaint;
    EndUpdate;
    if (FMaxParentHeight > 0) and (FFirstRowIndex = 0) then begin
      // 当需要自动调整大小，并且显示的首行为第一行时
      // 如果当前列表视图的底部位置小于父级大小，则使用当前视图底部为列表框高度
      if FViewBottom < FMaxParentHeight then
        SetSize(Width, FViewBottom)
      else // 如果超出时，则使用父级最大高度为列表视图高度
        SetSize(Width, FMaxParentHeight)
    end;
    if LS.Adjust <> 0 then begin
      // 高度变化了, 更新滚动条状态
      ListView.FContentBounds.Bottom := ListView.FContentBounds.Bottom + LS.Adjust;
      if LS.IsUp then
        ListView.DoUpdateScrollingLimits(True, LS.Adjust)
      else
        ListView.DoUpdateScrollingLimits(True);
    end;
    FDisableAlign := False;
  end;
end;

procedure TListViewContent.DrawDivider(Canvas: TCanvas);
var
  I, J: Integer;
  X, Y, DividerHeight, LY: Double;
begin
  DividerHeight := ListView.GetDividerHeight;
  if (DividerHeight > 0) and (ListView.FDivider and $FF000000 <> 0) then begin
    FDividerBrush.Color := ListView.FDivider;
    Y := FViewTop - FLastScrollValue;
    if FLastColumnCount > 1 then begin
      J := 0;
      X := 0;
      Y := Y - DividerHeight;
      LY := Y;
      // 画横线
      for I := FirstRowIndex to FLastRowIndex do begin
        if (I mod FLastColumnCount = 0) or (J = 0) then begin
          Y := Y + X;
          Canvas.FillRect(RectF(0, Y, Width, Y + DividerHeight),
            0, 0, [], ListView.Opacity, FDividerBrush);
          Y := Y + DividerHeight;
          X := Max(ListView.FItemsPoints[I].H, X);
          Inc(J);
        end;
      end;
      if (FLastRowIndex < FCount) or (FLastRowIndex mod FLastColumnCount > 0) then begin
        Y := Y + X;
        Canvas.FillRect(RectF(0, Y, Width, Y + DividerHeight),
          0, 0, [], ListView.Opacity, FDividerBrush);
      end;
      // 画列线
      if FColumnDivider then begin
        if Assigned(FFooterView) and (FFooterView.Visible) then
          Y := FFooterView.Position.Y
        else if Assigned(FFooter) and (FFooter.Visible) then
          Y := TControl(FFooter).Position.Y;
        X := FLastColumnWidth;
        for I := 0 to FLastColumnCount - 1 do begin
          Canvas.FillRect(RectF(X, LY, X + DividerHeight, Y),
            0, 0, [], ListView.Opacity, FDividerBrush);
          X := X + DividerHeight + FLastColumnWidth;
        end;
      end;
    end else begin
      if Assigned(FHeaderView) then begin
        Y := Y - DividerHeight;
        for I := FirstRowIndex to FLastRowIndex + 1 do begin
          Canvas.FillRect(RectF(0, Y, Width, Y + DividerHeight),
            0, 0, [], ListView.Opacity, FDividerBrush);
          Y := Y + ListView.FItemsPoints[I].H + DividerHeight;
        end;
      end else begin
        for I := FirstRowIndex to FLastRowIndex do begin
          Y := Y + ListView.FItemsPoints[I].H;
          Canvas.FillRect(RectF(0, Y, Width, Y + DividerHeight),
            0, 0, [], ListView.Opacity, FDividerBrush);
          Y := Y + DividerHeight;
        end;
      end;
    end;
  end;
end;

procedure TListViewContent.FreeFooter;
begin
  if Assigned(FFooter) then begin
    RemoveObject(FFooter as TControl);
    FFooter := nil;
  end;
end;

procedure TListViewContent.FreeHeader;
begin
  if Assigned(FHeader) then begin
    RemoveObject(FHeader as TControl);
    FHeader := nil;
  end;
end;

function TListViewContent.GetAbsoluteColumnCount: Integer;
begin
  if FColumnWidth > 0 then begin
    Result := Trunc((Width + ListView.FDividerHeight) / (FColumnWidth + ListView.FDividerHeight));
    if Result < 1 then Result := 1;
  end else
    Result := FColumnCount;
end;

function TListViewContent.GetAbsoluteColumnWidth: Single;
begin
  if FColumnWidth > 0 then
    Result := FColumnWidth
  else if FColumnDivider then
    Result := (FSize.Width - (FColumnCount - 1) * ListView.FDividerHeight) / FColumnCount
  else
    Result := FSize.Width / FColumnCount;
end;

function TListViewContent.GetControlFormCacle(const ItemType: Integer): TViewBase;
var
  List: TListViewList;
begin
  if not FCacleViews.TryGetValue(ItemType, List) then begin
    List := TListViewList.Create;
    FCacleViews.Add(ItemType, List);
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
  ItemViewType: Integer;
begin
  if Assigned(FAdapter) then
    FCount := FAdapter.Count
  else
    FCount := 0;
  if FViews.Count > 0 then begin
    for ItemView in FViews do begin
      if ItemView.Key >= FCount then
        RemoveObject(ItemView.Value)
      else begin
        ItemViewType := FAdapter.GetItemViewType(ItemView.Key);
        if ItemViewType = ListViewType_Remove then // 如果返回状态是删除，则清掉
          RemoveObject(ItemView.Value)
        else begin
          AddControlToCacle(ItemViewType, ItemView.Value);
        end;
      end;
    end;
    FViews.Clear;
  end;
  if Assigned(FFooter) then
    (FFooter as TControl).Visible := False;
  if Assigned(FFooterView) then
    FFooterView.Visible := False;
end;

procedure TListViewContent.InitFooter;
begin
  if not Assigned(FFooter) then begin
    if Assigned(ListView.FOnInitFooter) then
      ListView.FOnInitFooter(ListView, FFooter);
    if not Assigned(FFooter) then
      FFooter := TListViewDefaultFooter.Create(Self);
    (FFooter as TControl).Parent := Self;
    (FFooter as TControl).Stored := False;
    if FIsDesigning then begin
      (FFooter as TControl).Align := TAlignLayout.Bottom;
      FFooter.DoUpdateState(TListViewState.None, 0);
    end else
      (FFooter as TControl).Visible := False;
  end;
end;

procedure TListViewContent.InitHeader;
begin
  if not Assigned(FHeader) then begin
    if Assigned(ListView.FOnInitHeader) then
      ListView.FOnInitHeader(ListView, FHeader);
    if not Assigned(FHeader) then
      FHeader := TListViewDefaultHeader.Create(Self);
    (FHeader as TControl).Parent := Self;
    (FHeader as TControl).Stored := False;
    (FHeader as TControl).Index := 0;
    if FIsDesigning then begin
      FHeader.DoUpdateState(TListViewState.None, 0);
      (FHeader as TControl).Align := TAlignLayout.Top;
    end else
      (FHeader as TControl).Visible := False;
  end;
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
  if (FCount > 0) and (FirstRowIndex <= FLastRowIndex) and (FViewBottom >= FViewTop) then
    DrawDivider(Canvas);
end;

procedure TListViewContent.PullRefreshStart;
begin
  if not Assigned(FHeader) then
    Exit;
  ListView.VScrollBarValue := ListView.VScrollBarValue + (FHeader as TControl).Height;
  FHeader.DoUpdateState(TListViewState.PullDownFinish, 0);
  FState := TListViewState.PullDownFinish;
  DoRealign;
end;

procedure TListViewContent.RemoveFooterView;
begin
  if Assigned(FFooterView) then begin
    RemoveObject(FFooterView);
    FFooterView := nil;
  end;
end;

procedure TListViewContent.RemoveHeaderView;
begin
  if Assigned(FHeaderView) then begin
    RemoveObject(FHeaderView);
    FHeaderView := nil;
  end;
end;

procedure TListViewContent.Resize;
begin
  if csReading in ComponentState then
    Exit;
  inherited Resize;
  Realign;
end;

procedure TListViewContent.SetViewTop(const Value: Double);
begin
  if FViewTop <> Value then begin
    FViewTop := Value;
    DoRealign;
    Invalidate;
  end;
end;

{ TListAdapterBase }

procedure TListAdapterBase.Clear;
var
  B: Boolean;
begin
  if Assigned(ListView) and Assigned(ListView.ContentViews) then begin
    B := ListView.FContentViews.FDisableAlign;
    ListView.FContentViews.FDisableAlign := True;
    try
      ListView.ContentViews.HideViews;
    finally
      ListView.FContentViews.FDisableAlign := B;
    end;
  end;
end;

constructor TListAdapterBase.Create;
begin
  DoInitData;
end;

function TListAdapterBase.DisableCache: Boolean;
begin
  Result := False;
end;

procedure TListAdapterBase.DoInitData;
begin
end;

function TListAdapterBase.GetItem(const Index: Integer): Pointer;
begin
  Result := Pointer(Index);
end;

function TListAdapterBase.GetItemID(const Index: Integer): Int64;
begin
  Result := Index;
end;

function TListAdapterBase.GetItemViewType(const Index: Integer): Integer;
begin
  Result := ListViewType_Default;
end;

function TListAdapterBase.IndexOf(const AItem: Pointer): Integer;
begin
  Result := -1;
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

procedure TListAdapterBase.ItemMeasureHeight(const Index: Integer; var AHeight: Single);
begin
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

function TListAdapter<T>.GetItem(const Index: Integer): Pointer;
begin
  Result := nil;
end;

function TListAdapter<T>.GetItems: TList<T>;
begin
  if FList = nil then begin
    FList := TList<T>.Create;
    FListNeedFree := True;
  end;
  Result := FList;
end;

function TListAdapter<T>.IndexOf(const AItem: Pointer): Integer;
begin
  Result := -1;
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
  FWordWrap := True;
  FFontSize := TListTextItem.C_FontSize;
  SetArray(AItems);
  DoInitData;
end;

constructor TStringsListAdapter.Create(const AItems: TStrings);
begin
  FWordWrap := True;
  FFontSize := TListTextItem.C_FontSize;
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
    ViewItem.MinHeight := ItemDefaultHeight;
    ViewItem.TextSettings.Font.Size := FFontSize;
    ViewItem.TextSettings.WordWrap := FWordWrap;
    ViewItem.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.Padding.Rect := RectF(8, 8, 8, 8);
    ViewItem.CanFocus := False;
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
  if FDefaultItemHeight = 0 then
    Result := TListTextItem.C_MinHeight
  else
    Result := FDefaultItemHeight;
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

function TStringsListCheckAdapter.DisableCache: Boolean;
begin
  Result := True;
end;

procedure TStringsListCheckAdapter.DoCheckChange(Sender: TObject);
var
  V: Boolean;
begin
  V := not ItemCheck[TControl(Sender).Tag];
  ItemCheck[TControl(Sender).Tag] := V;
  if Sender is TListViewItemCheck then
    TListViewItemCheck(Sender).CheckBox1.IsChecked := V;
  if Assigned(ListView.FOnItemClick) then
    ListView.FOnItemClick(ListView, TControl(Sender).Tag, TControl(Sender));
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
    ViewItem.Parent := Parent;
    ViewItem.BeginUpdate;
    ViewItem.WidthSize := TViewSize.FillParent;
    ViewItem.MinHeight := ItemDefaultHeight;
    ViewItem.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.Width := Parent.Width;

    ViewItem.TextView1 := TTextView.Create(ViewItem);
    ViewItem.TextView1.WidthSize := TViewSize.FillParent;
    ViewItem.TextView1.HeightSize := TViewSize.WrapContent;
    ViewItem.TextView1.TextSettings.Font.Size := FFontSize;
    ViewItem.TextView1.TextSettings.WordWrap := FWordWrap;
    //ViewItem.TextView1.TextSettings.Trimming := TTextTrimming.Character;
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
    ViewItem.CanFocus := False;
    ViewItem.EndUpdate;
  end else
    ViewItem := TControl(ConvertView) as TListViewItemCheck;


  ViewItem.BeginUpdate;
  ViewItem.Tag := Index;   // 使用 Tag 记录索引号
  ViewItem.OnClick := DoCheckChange;
  ViewItem.CheckBox1.IsChecked := not ItemCheck[Index];
  ViewItem.HeightSize := TViewSize.WrapContent;
  ViewItem.DoRealign;
  ViewItem.TextView1.Text := Items[Index];
  ViewItem.Height := ViewItem.TextView1.Size.Height;
  ViewItem.EndUpdate;
  ViewItem.CheckBox1.IsChecked := ItemCheck[Index];

  Result := TViewBase(ViewItem);
end;

procedure TStringsListCheckAdapter.Insert(const Index: Integer;
  const V: string);
begin
  inherited;
  if FChecks.Len >= Index + 1 then
    FChecks.Insert(Index, False);
end;

procedure TStringsListCheckAdapter.SetArrayLength(const ACount: Integer);
begin
  inherited SetArrayLength(ACount);
  FChecks.Len := ACount;
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
  FFontSize := TListTextItem.C_FontSize;
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
    ViewItem.Parent := Parent;
    ViewItem.BeginUpdate;
    ViewItem.WidthSize := TViewSize.FillParent;
    ViewItem.MinHeight := ItemDefaultHeight;
    ViewItem.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.Width := Parent.Width;

    ViewItem.TextView1 := TTextView.Create(ViewItem);
    ViewItem.TextView1.WidthSize := TViewSize.FillParent;
    ViewItem.TextView1.HeightSize := TViewSize.WrapContent;
    ViewItem.TextView1.TextSettings.Font.Size := FFontSize;
    ViewItem.TextView1.TextSettings.WordWrap := FWordWrap;
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
    ViewItem.CanFocus := False;
    ViewItem.EndUpdate;
  end else
    ViewItem := TControl(ConvertView) as TListViewItemSingle;

  ViewItem.Tag := Index; // 使用 Tag 记录索引号
  ViewItem.OnClick := DoItemIndexChange;
  ViewItem.BeginUpdate;
  ViewItem.RadioButton.IsChecked := FItemIndex = Index;
  ViewItem.HeightSize := TViewSize.WrapContent;
  ViewItem.DoRealign;
  ViewItem.TextView1.Text := Items[Index];
  ViewItem.Height := ViewItem.TextView1.Height;
  ViewItem.EndUpdate;

  Result := TViewBase(ViewItem);
end;

{ TTreeListNode<T> }

procedure TTreeListNode<T>.Add(const ANode: TTreeListNode<T>);
begin
  if not Assigned(ANode) then
    Exit;
  if not Assigned(FNodes) then CreateNodes;
  if FNodes.IndexOf(ANode) < 0 then begin
    ANode.Parent := Self;
    FNodes.Add(ANode);
  end;
end;

function TTreeListNode<T>.AddNode(const AData: T): TTreeListNode<T>;
begin
  if not Assigned(FNodes) then CreateNodes;
  Result := TTreeListNode<T>.Create;
  Result.FData := AData;
  Result.FParent := Self;
  Result.UpdateLevel;
  FNodes.Add(Result);
end;

procedure TTreeListNode<T>.Clear;
begin
  if Assigned(FNodes) then
    FNodes.Clear;
end;

constructor TTreeListNode<T>.Create;
begin
  FNodes := nil;
  FExpanded := False;
end;

procedure TTreeListNode<T>.CreateNodes;
begin
  if not Assigned(FNodes) then begin
    FNodes := TList<TTreeListNode<T>>.Create;
    FNodes.OnNotify := DoNodeNotify;
  end;
end;

procedure TTreeListNode<T>.DeleteSelf;
begin
  if Assigned(FParent) then
    FParent.Remove(Self);
end;

destructor TTreeListNode<T>.Destroy;
begin
  if Assigned(FNodes) then
    Parent := nil;
  FreeAndNil(FNodes);
  inherited;
end;

procedure TTreeListNode<T>.DoNodeNotify(Sender: TObject;
  const Item: TTreeListNode<T>; Action: System.Generics.Collections.TCollectionNotification);
begin
  if Action = System.Generics.Collections.TCollectionNotification.cnRemoved then
    if Assigned(Item) then Item.DisposeOf;
end;

function TTreeListNode<T>.GetCount: Integer;
begin
  if Assigned(FNodes) then
    Result := FNodes.Count
  else
    Result := 0;
end;

function TTreeListNode<T>.GetNode(const Index: Integer): TTreeListNode<T>;
begin
  if Assigned(FNodes) and (Index >= 0) and (Index < FNodes.Count) then
    Result := FNodes.Items[Index]
  else
    Result := nil;
end;

function TTreeListNode<T>.GetParentIndex: Integer;
begin
  if Assigned(FParent) then
    Result := FParent.FNodes.IndexOf(Self)
  else
    Result := -1;
end;

procedure TTreeListNode<T>.Insert(const Index: Integer;
  const ANode: TTreeListNode<T>);
begin
  if not Assigned(ANode) then Exit;
  if not Assigned(FNodes) then CreateNodes;
  ANode.Parent := Self;
  if (Index < 0) or (Index >= FNodes.Count) then
    FNodes.Add(ANode)
  else
    FNodes.Insert(Index, ANode);
end;

function TTreeListNode<T>.InsertNode(const Index: Integer; const AData: T): TTreeListNode<T>;
begin
  if not Assigned(FNodes) then CreateNodes;
  Result := TTreeListNode<T>.Create;
  Result.FData := AData;
  Result.FParent := Self;
  if (Index < 0) or (Index >= FNodes.Count) then
    FNodes.Add(Result)
  else begin
    FNodes.Insert(Index, Result);
  end;
  Result.UpdateLevel;
end;

procedure TTreeListNode<T>.Remove(const ANode: TTreeListNode<T>);
begin
  if not Assigned(FNodes) then Exit;
  FNodes.Remove(ANode);
end;

procedure TTreeListNode<T>.InnerRemove(const ANode: TTreeListNode<T>);
begin
  if not Assigned(FNodes) then Exit;
  FNodes.OnNotify := nil;
  FNodes.Remove(ANode);
  FNodes.OnNotify := DoNodeNotify;
end;

procedure TTreeListNode<T>.SetNode(const Index: Integer;
  const Value: TTreeListNode<T>);
begin
  if not Assigned(FNodes) then Exit;
  if (Index < 0) or (Index >= FNodes.Count) then Exit;
  FNodes.Items[Index] := Value;
end;

procedure TTreeListNode<T>.SetParent(const Value: TTreeListNode<T>);
begin
  if FParent <> Value then begin
    if Assigned(FParent) then
      FParent.InnerRemove(Self);
    FParent := Value;
    if Assigned(FParent) then begin
      FParent.Add(Self);
      UpdateLevel;
    end else
      FLevel := -1;
  end;
end;

procedure TTreeListNode<T>.UpdateLevel;
var
  I: Integer;
  P: TTreeListNode<T>;
begin
  I := -1;
  P := FParent;
  while P <> nil do begin
    Inc(I);
    P := P.FParent;
  end;
  FLevel := I;
end;

{ TCustomTreeListDataAdapter<T> }

procedure TCustomTreeListDataAdapter<T>.AddListItem(
  const Parent: TTreeListNode<T>);
var
  I: Integer;
begin
  for I := 0 to Parent.Count - 1 do begin
    FList.Add(Parent.Nodes[I]);
    if Parent.Nodes[I].Expanded then
      AddListItem(Parent.Nodes[I]);
  end;
end;

procedure TCustomTreeListDataAdapter<T>.BeginUpdate;
begin
  Inc(FUpdateRef);
end;

procedure TCustomTreeListDataAdapter<T>.Clear;
begin
  inherited Clear;
  if Assigned(FRoot) then
    FRoot.Clear;
  if Assigned(FList) then
    FList.Clear;
end;

constructor TCustomTreeListDataAdapter<T>.Create();
begin
  FRoot := TTreeListNode<T>.Create;
  FList := TList<TTreeListNode<T>>.Create;
end;

destructor TCustomTreeListDataAdapter<T>.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FRoot);
  inherited;
end;

procedure TCustomTreeListDataAdapter<T>.DoNodeExpandChange(Sender: TObject);
var
  B: Boolean;
begin
  B := not FList.Items[TControl(Sender).Tag].Expanded;
  FList.Items[TControl(Sender).Tag].Expanded := B;
  ListView.FContentViews.HideViews;
  NotifyDataChanged;
end;

procedure TCustomTreeListDataAdapter<T>.EndUpdate;
begin
  Dec(FUpdateRef);
  if FUpdateRef < 0 then
    FUpdateRef := 0;
  if FUpdateRef = 0 then
    InitList;
end;

function TCustomTreeListDataAdapter<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCustomTreeListDataAdapter<T>.GetItem(const Index: Integer): Pointer;
begin
  Result := FList.Items[Index];
end;

function TCustomTreeListDataAdapter<T>.GetItemViewType(
  const Index: Integer): Integer;
begin
  Result := FList.Items[Index].Level;
end;

function TCustomTreeListDataAdapter<T>.GetNodeCount: Integer;
begin
  Result := FRoot.Count;
end;

function TCustomTreeListDataAdapter<T>.GetNodeGroupView(const Index: Integer;
  const ANode: TTreeListNode<T>; ConvertView: TViewBase;
  Parent: TViewGroup): TViewBase;
var
  ViewItem: TListViewTreeGroup;
begin
  if (ConvertView = nil) or (not (TControl(ConvertView) is TListViewTreeGroup)) then begin
    ViewItem := TListViewTreeGroup.Create(Parent);
    ViewItem.Parent := Parent;
    ViewItem.Width := Parent.Width;
    ViewItem.Height := TListTextItem.C_MinHeight;
    ViewItem.HitTest := False;
    ViewItem.CanFocus := False;
  end else
    ViewItem := TControl(ConvertView) as TListViewTreeGroup;
  ViewItem.BeginUpdate;
  ViewItem.TextView.Tag := Index;  // 使用 Tag 记录索引号
  ViewItem.TextView.OnClick := DoNodeExpandChange;
  ViewItem.TextView.Text := GetNodeText(ANode);
  ViewItem.TextView.Checked := ANode.Expanded;
  ViewItem.EndUpdate;
  Result := TViewBase(ViewItem);
end;

function TCustomTreeListDataAdapter<T>.GetNodeItemView(const Index: Integer;
  const ANode: TTreeListNode<T>; ConvertView: TViewBase;
  Parent: TViewGroup): TViewBase;
var
  ViewItem: TListTextItem;
begin
  if (ConvertView = nil) or (not (ConvertView is TListTextItem)) then begin
    ViewItem := TListTextItem.Create(Parent);
    ViewItem.Parent := Parent;
    ViewItem.Width := Parent.Width;
    ViewItem.MinHeight := ItemDefaultHeight;
    ViewItem.Height := ViewItem.MinHeight;
    ViewItem.HeightSize := TViewSize.CustomSize;
    ViewItem.TextSettings.Font.Size := TListTextItem.C_FontSize;
    ViewItem.TextSettings.WordWrap := True;
    ViewItem.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.Padding.Rect := RectF(8, 8, 8, 8);
    ViewItem.CanFocus := False;
  end else
    ViewItem := ConvertView as TListTextItem;
  ViewItem.Text := GetNodeText(ANode);
  Result := ViewItem;
end;

function TCustomTreeListDataAdapter<T>.GetNodes(
  const Index: Integer): TTreeListNode<T>;
begin
  Result := FList.Items[Index];
end;

function TCustomTreeListDataAdapter<T>.GetNodeText(
  const ANode: TTreeListNode<T>): string;
begin
  Result := ANode.ToString();
end;

function TCustomTreeListDataAdapter<T>.GetView(const Index: Integer;
  ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
var
  ViewItem: TListTextItem;
  Node: TTreeListNode<T>;
begin
  Node := Nodes[Index];
  if (Node.Level = 0) or (Node.Count > 1) then
    Result := GetNodeGroupView(Index, Node, ConvertView, Parent)
  else
    Result := GetNodeItemView(Index, Node, ConvertView, Parent)
end;

function TCustomTreeListDataAdapter<T>.IndexOf(const AItem: Pointer): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

procedure TCustomTreeListDataAdapter<T>.InitList;
begin
  FList.Clear;
  AddListItem(FRoot);
end;

procedure TCustomTreeListDataAdapter<T>.ItemMeasureHeight(const Index: Integer; var AHeight: Single);
begin
  if FList.Items[Index].Level = 0 then
    AHeight := 36
end;

procedure TCustomTreeListDataAdapter<T>.NotifyDataChanged;
begin
  InitList;
  inherited NotifyDataChanged;
  if Assigned(ListView) then
    ListView.Invalidate;
end;

{ TStringsListIconAdapter }

procedure TStringsListIconAdapter.DoInitData;
begin
  inherited;
  FFontSize := TListTextItem.C_FontSize;
  FIconSize.Width := 16;
  FIconSize.Height := 16;
  FPadding := 8;
end;

function TStringsListIconAdapter.GetItemImageIndex(
  const Index: Integer): Integer;
begin
  Result := Index;
end;

function TStringsListIconAdapter.GetView(const Index: Integer;
  ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
var
  ViewItem: TListTextItem;
begin
  if (ConvertView = nil) or (not (ConvertView is TListTextItem)) then begin
    ViewItem := TListTextItem.Create(Parent);
    ViewItem.Parent := Parent;
    ViewItem.Width := Parent.Width;
    ViewItem.MinHeight := ItemDefaultHeight;
    ViewItem.TextSettings.Font.Size := FFontSize;
    ViewItem.TextSettings.WordWrap := FWordWrap;
    ViewItem.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.Padding.Rect := RectF(8, 8, 8, 8);
    if Assigned(FImages) then begin
      ViewItem.Drawable.Images := FImages;
      ViewItem.Drawable.Position := FPosition;
      ViewItem.Drawable.SizeWidth := FIconSize.Width;
      ViewItem.Drawable.SizeHeight := FIconSize.Height;
      ViewItem.Drawable.Padding := FPadding;
    end;
    ViewItem.CanFocus := False;
  end else
    ViewItem := ConvertView as TListTextItem;
  if Assigned(FImages) then
  ViewItem.HeightSize := TViewSize.WrapContent;
  ViewItem.Text := Items[Index];
  if Assigned(FImages) then
    TViewImagesBrush(ViewItem.Drawable.ItemDefault).ImageIndex := GetItemImageIndex(Index);
  Result := ViewItem;
end;

{ TListViewRealginState }

function TListViewRealginState.GetIsUp: Boolean;
begin
  Result := Self.MoveSpace < 0;
end;

end.

