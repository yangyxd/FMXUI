{*******************************************************}
{                                                       }
{       FMXUI Grid ���                                 }
{                                                       }
{       ��Ȩ���� (C) 2017 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Grid;

interface

{.$DEFINE DEBUGMSG}

uses
  UI.Debug, UI.Utils, UI.Base, UI.Standard, UI.Utils.ArrayEx, UI.Ani, UI.Edit,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Data.DB, Data.DBConsts,
  FMX.Utils, FMX.ImgList, FMX.MultiResBitmap, FMX.ActnList, System.Rtti, FMX.Consts,
  FMX.TextLayout, FMX.Objects, System.ImageList, System.RTLConsts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, System.Math,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement,
  FMX.Styles.Objects, FMX.Forms;

type
  TGridBase = class;
  TGridColumns = class;
  TGridColumnItem = class;

  /// <summary>
  /// ������������
  /// </summary>
  TGridDataType = (
    PlanText {��ͨ�ı�},
    CheckBox {��ѡ��},
    RadioButton {��ѡ��ť},
    Image, {ͼ��}
    ProgressBar, {������}
    CustomDraw {�Զ�����}
  );

  /// <summary>
  /// �̶�����Ϣ
  /// </summary>
  TGridFixedColumnItem = record
    Width: Single;              // �п���
    RowsPan: Integer;           // �п��, -1ʱ����ʾ���FixedRows�趨����, ����1ʱ��ʾ���ָ������
    Tag: NativeInt;             // ��������
    Title: string;              // �б���
  end;
  PGridFixedColumnItem = ^TGridFixedColumnItem;

  /// <summary>
  /// ��Ԫ���ʽ
  /// </summary>
  TGridCellSettings = class(TPersistent)
  public
    BgColor: TAlphaColor;          // ������ɫ
    Gravity: TLayoutGravity;       // ���뷽ʽ
    TextColor: TAlphaColor;        // ������ɫ
    TextStyle: TFontStyles;        // ������ʽ

    Locked: Boolean;               // �Ƿ�����, �����������޸����ݣ������ֱ༭��
    ReadOnly: Boolean;             // ֻ����
    Enabled: Boolean;              // �Ƿ�����, ������ʱ���ܵ���������ֱ༭��

    constructor Create();
    procedure Assign(Source: TPersistent); override;
  end;

  /// <summary>
  /// ����ѡ��
  /// </summary>
  TGridOption = (
    gvEditing,              // �ɱ༭
    gvAlwaysShowEditor,     // �Ƿ�ֱ�ӽ���༭״̬������gvEditingʱ��Ч
    gvTabs,                 // ����ʹ��Tab�л���Ԫ��
    gvIndicator,            // ��ʾָʾ����
    gvRowIndex,             // ��ʾ�к�
    gvColLines,             // ��ʾ��������
    gvRowLines,             // ��ʾ�������
    gvRowSelect,            // ��ѡ��
    gvMultiSelect,          // ��ѡ�������� RowSelect ʱ��Ч
    gvRangeSelect,          // �Ƿ���������ѡ��, ������ RowSelect, Editing ʱ��Ч
    gvTwoColor,             // ��������ɫ������ʾ
    gvShowSelection,        // ʼ����ʾѡ����
    gvCancelOnExit,         // ���뿪����ʱȡ������
    gvEscCancelEdit,        // �ڱ༭ʱ����escape������ȡ������
    gvDisplayZero,          // �������Ƿ���ʾ0
    gvColumnBestWidth,      // �Ƿ���������п������� gvColumnResize ʱ��Ч
    gvColumnResize,         // �п����϶�
    gvColumnMove            // �����ƶ���
  );

  TGridOptions = set of TGridOption;

  TGridFixedHeaderState = record
    Row, Col: Integer;
    R: TRectF;
  end;

  TGridCell = record
    Row, Col: Integer;
    procedure Clear;
  end;

  TOnGridGetCellText = procedure (Item: TGridColumnItem; const ARow: Integer; out Text: string) of object;

  /// <summary>
  /// ������ͷ��
  /// </summary>
  TGridColumnItem = class(TPersistent)
  private
    procedure SetWidth(const Value: Single);
    function GetIndex: Integer;
    procedure SetIndex(const Value: Integer);
    function GetRealWidth: Single;
    function GetRight: Double;
  protected
    [Weak] FOwner: TGridColumns;
    FOnGetCellText: TOnGridGetCellText;
    IsLeftTop: Boolean;         // �Ƿ������Ͻ�
    FWidth: Single;             // �п���
    X: Double;                  // λ��
    procedure DoChange;
    property Right: Double read GetRight;
    function GetDispLayText: string; virtual;
  public
    ColIndex: Integer;          // ��ʵ���������ţ���������ʾ�Ĳ�һ�£�
    RowIndex: Integer;          // ��ʵ����������
    Gravity: TLayoutGravity;    // ���뷽ʽ
    DataType: TGridDataType;    // ��������
    Opacity: Single;            // ͸����
    Padding: TRectF;            // �����ܱ�Padding

    Locked: Boolean;            // �Ƿ�����������ʱ����ʾ�༭��
    DataFilter: Boolean;        // �Ƿ�������������
    ReadOnly: Boolean;          // ֻ����
    Visible: Boolean;           // �Ƿ����
    Enabled: Boolean;           // �Ƿ����� (����ʱ�����������)
    WordWrap: Boolean;          // �Ƿ��Զ�����
    IsBLOB: Boolean;            // �Ƿ��Ƕ����ƴ����

    RowsPan: Integer;           // �п��, -1ʱ����ʾ���FixedRows�趨����, ����1ʱ��ʾ���ָ������
    ColsPan: Integer;           // �п��, ����1ʱ����ʾ���������

    Tag: NativeInt;             // ��������
    Title: string;              // �б���

    FilterText: string;         // ��������
  public
    constructor Create(AOwner: TGridColumns);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    // �����Լ���ͷ���б��е����У��ҵ�����True
    function GetRowCol(var ACol, ARow: Integer): Boolean;

    property Width: Single read FWidth write SetWidth;
    property Index: Integer read GetIndex write SetIndex;
    property Owner: TGridColumns read FOwner;

    property RealWidth: Single read GetRealWidth;
    property DisplayText: string read GetDispLayText;

    /// <summary>
    /// ��ȡ��Ԫ�����ݣ����ô����Ժ��ڻ��Ƶ�Ԫ��ʱ����ô��¼�����ȡ����
    /// </summary>
    property OnGetCellText: TOnGridGetCellText read FOnGetCellText write FOnGetCellText;
  end;

  TGridColumnItemClass = type of TGridColumnItem;

  /// <summary>
  /// DBGrid ��ͷ��Ϣ
  /// </summary>
  TGridDBColumnItem = class(TGridColumnItem)
  protected
    function GetDispLayText: string; override;
  public
    [Weak] Field: TField;
    FieldName: string;         // �ֶ�����
    FieldType: TFieldType;     // �ֶ�����
    destructor Destroy; override;
  end;

  /// <summary>
  /// ������ͷ��Ϣ
  /// </summary>
  TGridColumns = class(TPersistent)
  private
    [Weak] FGridView: TGridBase;
    FColumnClass: TGridColumnItemClass;
    FData: TIntHash;
    FMaxRows: Integer;
    FMaxCols: Integer;
    FLastWidth: Double;
    FExistWordWarp: Boolean;
    FOnChange: TNotifyEvent;
    function GetItem(const ACol, ARow: Integer): TGridColumnItem;
    procedure SetItem(const ACol, ARow: Integer; const Value: TGridColumnItem);
    function GetHeight: Double;
    function GetWidth: Double;
    procedure SetMaxCols(const Value: Integer);
    procedure SetMaxRows(const Value: Integer);
    function GetItemCols(const ACol: Integer): TGridColumnItem;
  protected
    FShowColIndex: Boolean;
    function GetItemOfKey(const Key: Int64): TGridColumnItem;
    procedure DoItemChange(Sender: TObject);
    procedure DoItemChangeEx(Sender: TGridColumnItem; const ACol, ARow: Integer);
    procedure DoChange(); virtual;
    procedure DoValueNotify(Item: PIntHashItem);
    function GetExistWordWarp: Boolean;
  public
    constructor Create(AGridView: TGridBase);
    destructor Destroy; override;
    procedure Clear;

    procedure Assign(Source: TPersistent); override;

    /// <summary>
    /// ע����ͷ��Ϣ��, �ڳ�ʼ��֮�����
    /// </summary>
    procedure RegisterColumnClass(const AColumnClass: TGridColumnItemClass);

    // ��ȡָ���е���Ϣ, �����ڶ���ʱ���Զ�ѡ�����ȼ�Ϊ�����һ�п�ʼ��ColsPan ԽС�����ȼ�Խ��
    property ItemCols[const ACol: Integer]: TGridColumnItem read GetItemCols;

    property Items[const ACol, ARow: Integer]: TGridColumnItem read GetItem write SetItem; default;

    property GridView: TGridBase read FGridView;
  published
    property ColsCount: Integer read FMaxCols write SetMaxCols default 1;
    property RowsCount: Integer read FMaxRows write SetMaxRows default 1;
    property Width: Double read GetWidth;
    property Height: Double read GetHeight;
    // �Ƿ������Ҫ�Զ��߶ȵ���
    property ExistWordWarp: Boolean read FExistWordWarp;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  /// ������ͼ����������
  /// </summary>
  IGridAdapter = interface
    ['{93CBD771-C9E8-47D5-B1A8-7726FA45C649}']
    /// <summary>
    /// ��ȡ�ܵ�����
    /// </summary>
    function GetRowCount: Integer;
    /// <summary>
    /// �����ܵ�����
    /// </summary>
    procedure SetRowCount(const Value: Integer);
    /// <summary>
    /// ��ȡ�ܵ�����
    /// </summary>
    function GetColCount: Integer;

    /// <summary>
    /// Ĭ���и�
    /// </summary>
    function ItemDefaultHeight: Single;
    procedure SetItemDefaultHeight(const Value: Single);

    /// <summary>
    /// ��ȡ����п�
    /// </summary>
    function GetBestColumnWidth(const ACol: Integer): Single;

    /// <summary>
    /// ��ȡ�и�
    /// </summary>
    function GetRowHeight: Single;
    /// <summary>
    /// ��ȡ��ID
    /// </summary>
    function GetRowID(const ARow: Integer): Int64;
    /// <summary>
    /// �Ƿ�Ϊ��
    /// </summary>
    function IsEmpty: Boolean;

    /// <summary>
    /// ��ȡѡ�е��к�, �� GridView �� SelectIndex ���ܲ�һ���������Ҫ������ѡ
    /// </summary>
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);

    /// <summary>
    /// ������ͷ��Ϣ
    /// </summary>
    function GetColumns: TGridColumns;
    /// <summary>
    /// ��ȡ�̶�����Ϣ
    /// </summary>
    function GetFixedColData(const ACol: Integer): TGridFixedColumnItem;

    /// <summary>
    /// ��ȡָ�����ӵ�����
    /// </summary>
    function GetCells(const ACol, ARow: Integer): string;
    /// <summary>
    /// ����ָ�����ӵ�����
    /// </summary>
    procedure SetCells(const ACol, ARow: Integer; const Value: string);

    /// <summary>
    /// �����α�λ��
    /// </summary>
    procedure SetCursor(const ARow: Integer);

    procedure BeginDrawCells(const AFirstRow, ALastRow: Integer);
    procedure EndDrawCells();

    /// <summary>
    /// Checkbox ��Ԫ��ѡ״̬
    /// </summary>
    function GetCellChecked(const ACol, ARow: Integer): Boolean;
    procedure SetCellChecked(const ACol, ARow: Integer; const Value: Boolean);

    /// <summary>
    /// ��ȡָ�����ӵĸ߶�
    /// </summary>
    function GetCellHeight(const ACol, ARow: Integer): Single;

    /// <summary>
    /// ��ȡָ�����ӵ�����
    /// </summary>
    function GetCellData(const ACol, ARow: Integer): Pointer;
    /// <summary>
    /// ����ָ�����ӵ�����
    /// </summary>
    procedure SetCellData(const ACol, ARow: Integer; const Value: Pointer);

    /// <summary>
    /// ��ȡ��Ԫ���Զ����ʽ��������� False ˵��û���Զ���
    /// </summary>
    function GetCellSetting(const ACol, ARow: Integer): TGridCellSettings;
    function GetCellSettings(const ACol, ARow: Integer; out ACellSettings: TGridCellSettings): Boolean;
    procedure SetCellSettings(const ACol, ARow: Integer; const ACellSettings: TGridCellSettings);

    procedure Clear;
    procedure Repaint;
    procedure NotifyDataChanged;

    /// <summary>
    /// ����
    /// </summary>
    property RowCount: Integer read GetRowCount write SetRowCount;
    /// <summary>
    /// ����
    /// </summary>
    property ColCount: Integer read GetColCount;
    /// <summary>
    /// ָ�����ӵ���������
    /// </summary>
    property Cells[const ACol, ARow: Integer]: string read GetCells write SetCells; default;
    /// <summary>
    /// ָ�����ӵĸ�ѡ״̬
    /// </summary>
    property CellChecked[const ACol, ARow: Integer]: Boolean read GetCellChecked write SetCellChecked;
    /// <summary>
    /// ָ�����ӵ���������
    /// </summary>
    property CellData[const ACol, ARow: Integer]: Pointer read GetCellData write SetCellData;
    /// <summary>
    /// ��Ԫ���Զ����ʽ
    /// </summary>
    property CellSettings[const ACol, ARow: Integer]: TGridCellSettings read GetCellSetting write SetCellSettings;
    /// <summary>
    /// ��ȡָ�����ӵĸ߶�
    /// </summary>
    property CellHeight[const ACol, ARow: Integer]: Single read GetCellHeight;
    /// <summary>
    /// ��ͷ��Ϣ
    /// </summary>
    property Columns: TGridColumns read GetColumns;
    /// <summary>
    /// ѡ�е��к�
    /// </summary>
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  end;

  /// <summary>
  /// ��������
  /// </summary>
  TGridTextSettings = class(TTextSettingsBase)
  private
    FColor: TAlphaColor;
    FSelect: TAlphaColor;
    FCustomColor: TAlphaColor;
    FEnabled: TAlphaColor;
    FOpacity: Single;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetOpacity(const Value: Single);
    procedure SetEnabledColor(const Value: TAlphaColor);
    procedure SetSelectColor(const Value: TAlphaColor);
  protected
    function IsStoreOpacity: Boolean; virtual;
    function IsStoredGravity: Boolean; override;
    function GetStateColor(const State: TViewState): TAlphaColor; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property CustomColor: TAlphaColor read FCustomColor write FCustomColor;
  published
    property Color: TAlphaColor read FColor write SetColor default TAlphaColorRec.Black;
    property ColorSelect: TAlphaColor read FSelect write SetSelectColor default TAlphaColorRec.White;
    property ColorEnabled: TAlphaColor read FEnabled write SetEnabledColor default TAlphaColorRec.Gray;
    property Font;
    property PrefixStyle;
    property Trimming;
    property WordWrap;
    property Gravity default TLayoutGravity.CenterVertical;
    property Opacity: Single read FOpacity write SetOpacity stored IsStoreOpacity;
  end;

  TGridViewBrush = class(TDrawableBase)
  protected
    function GetValue(const Index: Integer): TViewBrush;
    procedure SetValue(const Index: Integer; const Value: TViewBrush);
  published
    property ItemDefault: TViewBrush index 0 read GetValue write SetValue;
    property ItemPressed: TViewBrush index 1 read GetValue write SetValue;
    property ItemEnabled: TViewBrush index 6 read GetValue write SetValue;
  end;

  TGridViewCellBrush = class(TGridViewBrush)
  published
    property ItemDefault: TViewBrush index 0 read GetValue write SetValue;
    property ItemPressed: TViewBrush index 1 read GetValue write SetValue;
    property ItemSelected: TViewBrush index 4 read GetValue write SetValue;
    property ItemChecked: TViewBrush index 5 read GetValue write SetValue;
    property ItemEnabled: TViewBrush index 6 read GetValue write SetValue;
    property ItemTwoColor: TViewBrush index 7 read GetValue write SetValue; // ˫ɫ�У�ż���е���ɫ��
  end;

  /// <summary>
  /// ����ѡ���¼�
  /// </summary>
  TOnSelectCellEvent = procedure(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean) of object;
  /// <summary>
  /// ���ƶ��¼�
  /// </summary>
  TOnColumnMovedEvent = procedure(Sender: TObject; const FromIndex, ToIndex: Integer) of object;
  /// <summary>
  /// ���ӵ����¼�
  /// </summary>
  TOnFixedCellClickEvent = procedure(Sender: TObject; const ACol, ARow: Integer) of object;
  /// <summary>
  /// ͷ������¼�
  /// </summary>
  TOnTitleClickEvent = procedure(Sender: TObject; Item: TGridColumnItem) of object;

  /// <summary>
  /// ���ӵ����¼�
  /// </summary>
  TOnCellEvent = procedure(Sender: TObject; const ACell: TGridCell) of object;

  /// <summary>
  /// ���ӱ༭����¼�
  /// </summary>
  TOnCellEditDoneEvent = procedure(Sender: TObject; const ACell: TGridCell; const Value: string) of object;

  /// <summary>
  /// ���Ӹ�ѡ�¼�
  /// </summary>
  TOnCellCheckEvent = procedure(Sender: TObject; const ACell: TGridCell; var Checked: Boolean) of object;

  /// <summary>
  /// ���Ʊ�ͷ�̶���Ԫ���ı�
  /// </summary>
  TOnDrawFixedColText = procedure (Sender: TObject; Canvas: TCanvas; Item: TGridColumnItem;
    const R: TRectF) of object;
  /// <summary>
  /// ������߹̶���Ԫ���ı�
  /// </summary>
  TOnDrawFixedCellsText = procedure (Sender: TObject; Canvas: TCanvas; const ACol, ARow: Integer;
    const Item: TGridFixedColumnItem; const R: TRectF) of object;

  /// <summary>
  /// ���Ƶ�Ԫ��
  /// </summary>
  TOnDrawCells = procedure (Sender: TObject; Canvas: TCanvas; const ACol, ARow: Integer;
    const R: TRectF; ADrawState: TViewState; Column: TGridColumnItem; var DefaultDraw: Boolean) of object;


  /// <summary>
  /// ������ͼ��������
  /// </summary>
  TGridViewContent = class(TView)
  private
    [Weak] GridView: TGridBase;
    [Weak] FAdapter: IGridAdapter;
    FIsDesigning: Boolean;                    // �Ƿ�Ϊ���ģʽ

    FDividerBrush: TBrush;    // �ָ��߻���ˢ��
    FScrollRB: TBrush;        // ����ʾ��ֱ�ͺ��������ʱ�����½�С����Ļ���ˢ��

    FCellBrush: TGridViewCellBrush; // ��Ԫ�����ˢ��
    FTempCellBrush: TBrush;    // ��ʱ��Ԫ�����ˢ��

    FLastW, FLastH: Single;   // ���һ������ʱ������Ŀ��Ⱥ͸߶�
    FLastScrollValue: Double; // �ϴ�����ʱ��������λ��
    FLastHScrollValue: Double; // �ϴ�����ʱ��ˮƽ������λ��
    FMaxParentHeight: Double;    // �����ؼ����߶ȣ���ֵ>0ʱ�������б��߶��Զ�������С)

    FViewTop: Double;         // ��ǰ��ʾ�б���Ķ���λ��
    FViewBottom: Double;      // ��ǰ��ʾ�����ݵײ�λ��
    FViewItemBottom: Double;  // ��ǰ��ʾ�б���ĵײ�λ��
    FViewFullBottom: Double;  // ��ǰ��ʾ���������һ�еײ�λ��

    FFirstRowIndex: Integer;  // ��ǰ��ʾ�ĵ�һ���к�
    FLastRowIndex: Integer;   // ��ǰ��ʾ�����һ���к�
    FLastFullRowIndex: Integer;   // ��ǰ��ʾ���������һ���к�
    FCount: Integer;

    FExistWordWrap: Boolean;  // �Ƿ������Ҫ�Զ��߶ȵ���
    FDefaultItemHeight: Double;// Ĭ���и�

    FTwoColor: Boolean;

    FColumnsList: TList<TGridColumnItem>;
    FBaseColumnsList: TList<TGridColumnItem>;

    LOnChange: TNotifyEvent;

    function GetVisibleRowCount: Integer;
    function GetRowHeight(const ARow: Integer): Single;
  protected
    FDownPos: TPointF;
    FRBRect: TRectF;
    
    FSelectCell: TGridCell;   // ��ǰѡ��ĵ�Ԫ��
    FSelectClickRef: Integer; // ��ǰѡ�и��ӵ������

    FEditor: TEditView;
    FEditorShowing: Boolean;
    FMouseDowning: Boolean;
    FKeyDownIng: Boolean;

    FEditText: string;

    procedure DoEditExit(Sender: TObject); virtual;
    procedure DoKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState); virtual;
    
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;   
    
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure Click; override;
    procedure DblClick; override;

    procedure DoEnter; override;
    procedure DoExit; override;
  protected
    function ObjectAtPoint(AScreenPoint: TPointF): IControl; override;
    
    procedure DoRealign; override;
    procedure PaintBackground; override;
    procedure DoDrawHeaderRows(Canvas: TCanvas; var R: TRectF); // ���ָ���
    procedure DoDrawCell(Canvas: TCanvas; const R: TRectF; const ARow, ACol: Integer; const LOpacity: Single); virtual; // ����Ԫ��

    procedure DoCellBrushChange(Sender: TObject);

    procedure InitColumnList();

    procedure DoClickCell(const ACell: TGridCell); virtual;
    procedure DoEnterCell(const ACell: TGridCell); virtual;
    procedure DoLeaveCell(const ACell: TGridCell); virtual;

    procedure DoShowEditor(); virtual;    // ��ʾ�༭��
    procedure DoHideEditor(); virtual;    // ���ر༭��

    procedure DoEditComplete(); virtual;  // �༭���
    procedure DoEditCancel(); virtual;    // ȡ���༭

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // ������ת��Ϊ����
    function PointAsCell(const X, Y: Single): TGridCell;

    // ��ȡָ����Ԫ�����ʾλ��
    function CellRect(const ACell: TGridCell): TRectF;

    // ��ȡ��ǰѡ�и��ӵľ�������
    function SelectCellRect(): TRectF;

    // ������ѡ��ĵ�Ԫ��
    procedure ScrollToSelectedCell;
    // ������ָ���ĵ�Ԫ��
    procedure ScrollToCell(const ACell: TGridCell);
    
    // �и�
    property RowHeight[const ARow: Integer]: Single read GetRowHeight;

    /// <summary>
    /// ��ǰ��ʾ������������
    /// </summary>
    property FirstRowIndex: Integer read FFirstRowIndex;
    /// <summary>
    /// ��ǰ��ʾ�����һ��������
    /// </summary>
    property LastRowIndex: Integer read FLastRowIndex;
    /// <summary>
    /// ��ǰ��ʾ�˼���
    /// </summary>
    property VisibleRowCount: Integer read GetVisibleRowCount;
  end;

  /// <summary>
  /// ������
  /// </summary>
  TGridColumnsSetting = class(TPersistent)
  private
    [Weak] FOwner: TGridBase;
    [Weak] FColumns: TGridColumns;
  public
    constructor Create(AOwner: TGridBase);

    property Columns: TGridColumns read FColumns;
  published
    property Owner: TGridBase read FOwner;
  end;

  /// <summary>
  /// �̶���Ԫ������
  /// </summary>
  TGridFixedSetting = class(TPersistent)
  private const
    CDefaultFixedDividerColor = $ffc0c0c0;// Ĭ�Ϲ̶���Ԫ�����зָ�����ɫ
  private
    [Weak] FOwner: TGridBase;
    function GetFixedBrush: TGridViewBrush;
    function GetFixedCols: Integer;
    function GetFixedDefaultColWidth: Single;
    function GetFixedDivider: TAlphaColor;
    function GetFixedRowHeight: Single;
    function GetFixedRows: Integer;
    function GetFixedText: TGridTextSettings;
    function GetTextRowIndex: string;
    procedure SetFixedBrush(const Value: TGridViewBrush);
    procedure SetFixedCols(const Value: Integer);
    procedure SetFixedDefaultColWidth(const Value: Single);
    procedure SetFixedDivider(const Value: TAlphaColor);
    procedure SetFixedRowHeight(const Value: Single);
    procedure SetFixedRows(const Value: Integer);
    procedure SetFixedText(const Value: TGridTextSettings);
    procedure SetTextRowIndex(const Value: string);
  public
    constructor Create(AOwner: TGridBase);
  published
    /// <summary>
    /// �̶���Ԫ�����ˢ��
    /// </summary>
    property Brush: TGridViewBrush read GetFixedBrush write SetFixedBrush;
    /// <summary>
    /// �̶���Ԫ�������
    /// </summary>
    property ColCount: Integer read GetFixedCols write SetFixedCols default 0;
    /// <summary>
    /// �̶���Ԫ�������
    /// </summary>
    property RowCount: Integer read GetFixedRows write SetFixedRows default 1;
    /// <summary>
    /// �̶���Ԫ���и�
    /// </summary>
    property RowHeight: Single read GetFixedRowHeight write SetFixedRowHeight;
    /// <summary>
    /// �̶���Ԫ��Ĭ���п������������������иı䣩
    /// </summary>
    property ColWidth: Single read GetFixedDefaultColWidth write SetFixedDefaultColWidth;
    /// <summary>
    /// �̶���Ԫ��ָ�����ɫ
    /// </summary>
    property Divider: TAlphaColor read GetFixedDivider write SetFixedDivider default CDefaultFixedDividerColor;
    /// <summary>
    /// �̶���Ԫ����������
    /// </summary>
    property TextSettings: TGridTextSettings read GetFixedText write SetFixedText;
    /// <summary>
    /// �к��б���
    /// </summary>
    property TextRowIndex: string read GetTextRowIndex write SetTextRowIndex;
  end;

  /// <summary>
  /// �����������
  /// </summary>
  TGridBase = class(TScrollView)
  private const
    CDefaultDividerColor = $afe3e4e5;    // Ĭ�����зָ�����ɫ
    CDefaultBKPressedColor = $ffd9d9d9;  // Ĭ���б����ʱ������ɫ
    CDefaultFixedColor = $ffededed;      // Ĭ�Ϲ̶�������ɫ
    CDefaultCellWidth = 100;             // Ĭ�ϵ�Ԫ�����
    CDefaultMinColWidth = 25;            // Ĭ����ѵ�Ԫ���п�
    CDefaultFixedColWidth = 50;          // Ĭ�Ϲ̶���Ԫ���п�
    CDefaultFixedRowHeight = 20;         // Ĭ�Ϲ̶���Ԫ���и�
    CDefaultEmptyRows = 1;               // Ϊ��ʱ��ʾ�������У�
    CDefaultFixedColPadding = 2;         // Ĭ�Ϲ̶���Ԫ��Padding
    CDefaultTextRowIndex = '�к�';       // Ĭ���к��ı�
    CDefaultAnchorWidth = 6;             // Ĭ����ͷͼ�����
    CDefaultAnchorHeight = 12;           // Ĭ����ͷͼ��߶�
    CDefaultFilterIconWH = 12;           // Ĭ�Ϲ���ͼ�����

    CDefaultOptions = [gvEditing, gvRowIndex, gvColLines, gvRowLines, gvTwoColor, gvCancelOnExit, gvEscCancelEdit];
  private
    FAdapter: IGridAdapter;
    FContentViews: TGridViewContent;
    FCount: Integer;

    FColumns: TGridColumns;
    FOptions: TGridOptions;

    FReadOnly: Boolean;
    FResizeing: Boolean;           // ���ڵ��ڴ�С

    FDrawBuffer: TBitmap;

    FDivider: TAlphaColor;
    FDividerHeight: Single;
    FLocalDividerHeight: Single;

    FDefaultRowHeight: Single;

    FFixedSetting: TGridFixedSetting;
    FColumnsSetting: TGridColumnsSetting;

    FFixedText: TGridTextSettings; // �̶���Ԫ������
    FFixedDivider: TAlphaColor; // �̶���Ԫ��ָ�����ɫ
    FFixedBrush: TGridViewBrush;  // �̶���Ԫ��Ļ���ˢ��
    FFixedCols: Integer;  // �̶���Ԫ�������
    FFixedRowHeight: Single; // �̶���Ԫ���и�
    FFixedColsWidth: Single;   // �̶���Ԫ�����
    FFixedDefaultColWidth: Single;  // �̶���Ԫ��Ĭ���п�

    FText: TGridTextSettings; // ��Ԫ������
    FTextRowIndex: string;    // �к��б���

    FFilterList: TStrings;

    FFixedMergeMap: TIntHash;
    FItemsPoints: TArray<Single>;
    FFixedHeaderRange: TList<TGridFixedHeaderState>;

    FLastFixedIndicatorWidth: Single;
    FFixedIndicatorWidthChange: Boolean;

    FMaxListItemBottom: Double;

    FOnTitleClickEvent: TOnTitleClickEvent;
    FOnColumnMovedEvent: TOnColumnMovedEvent;
    FOnFixedCellClickEvent: TOnFixedCellClickEvent;

    FOnRowSelChangeEvent: TNotifyEvent;

    FOnDrawViewBackgroud: TOnDrawViewBackgroud;
    FOnDrawFixedColText: TOnDrawFixedColText;
    FOnDrawFixedCellsText: TOnDrawFixedCellsText;

    FOnDrawCells: TOnDrawCells;

    FOnCellClickEvent: TOnCellEvent;
    FOnCellEnterEvent: TOnCellEvent;
    FOnCellLeaveEvent: TOnCellEvent;
    FOnCellEditDoneEvent: TOnCellEditDoneEvent;

    FOnCellCheckEvent: TOnCellCheckEvent;
    FOnItemIndexChange: TNotifyEvent;

    function IsEmpty: Boolean;
    procedure SetAdapter(const Value: IGridAdapter);
    procedure SetDivider(const Value: TAlphaColor);
    procedure SetDividerHeight(const Value: Single);
    procedure SetFixedBrush(const Value: TGridViewBrush);
    procedure SetFixedCols(const Value: Integer);
    procedure SetFixedRows(const Value: Integer);
    procedure SetFixedRowHeight(const Value: Single);
    procedure SetFixedDivider(const Value: TAlphaColor);
    procedure SetFixedDefaultColWidth(const Value: Single);
    function GetFixedRows: Integer;
    procedure SetFixedText(const Value: TGridTextSettings);
    procedure SetText(const Value: TGridTextSettings);
    function GetFixedWidth: Single;
    function GetRowHeight(const ARow: Integer): Single;
    function GetCellBrush: TGridViewCellBrush;
    procedure SetCellBrush(const Value: TGridViewCellBrush);
    procedure SetOptions(const Value: TGridOptions);
    function GetFixedIndicatorWidth: Single;
    procedure SetTextRowIndex(const Value: string);
    procedure SetSelectionAnchor(const Value: Integer);
    function GetSelectIndex: Integer;
    procedure SetSelectIndex(const Value: Integer);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    function GetRowHeightPro: Single;
    procedure SetRowHeightPro(const Value: Single);
    function GetCellsData(const ACol, ARow: Integer): string;
    procedure SetCellsData(const ACol, ARow: Integer; const Value: string);
    procedure SetFixedSetting(const Value: TGridFixedSetting);
  protected
    function GetColCount: Integer;
    function GetRowCount: Integer;
  protected
    procedure InvalidateContentSize(); override; // ������������С
    procedure DoRealign; override;
    procedure DoEnter; override;
    procedure DoInVisibleChange; override;
    procedure DoScrollVisibleChange; override;
    procedure DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations); override;
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure PaintBackground; override;
    procedure DoDrawHeaderRows(Canvas: TCanvas; var R: TRectF); virtual;  // ���Ʊ�ͷ

    procedure DoDrawBackground(var R: TRectF); virtual;
    procedure DoDrawFixedColText(Canvas: TCanvas; Item: TGridColumnItem;
      const R: TRectF; const AOpacity: Single); virtual;
    procedure DoDrawFixedColBackground(Canvas: TCanvas; const R: TRectF; const AOpacity: Single); virtual;
    procedure DoDrawFixedRowText(Canvas: TCanvas; const Row: Integer;
      const R: TRectF; const AOpacity: Single); virtual;
    procedure DoDrawFixedRowIndexText(Canvas: TCanvas; const R: TRectF; const AOpacity: Single); virtual;
    procedure DoDrawFixedCellsText(Canvas: TCanvas; const ACol, ARow: Integer;
      const Item: TGridFixedColumnItem; const R: TRectF; const AOpacity: Single); virtual;

    procedure CreateCoentsView();
    procedure HScrollChange(Sender: TObject); override;
    procedure VScrollChange(Sender: TObject); override;

    procedure DoFixedBrushChange(Sender: TObject);
    procedure DoColumnsChange(Sender: TObject);
    procedure DoTextChange(Sender: TObject);

    procedure DoItemIndexChange(Sender: TObject); virtual;

    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
    function InnerCalcDividerHeight: Single;
    function GetDividerHeight: Single;
    function IsStoredDividerHeight: Boolean; virtual;
    function IsStoredScrollSmallChangeFraction: Boolean; override;
    function IsStoredRowHeight: Boolean; virtual;
  protected
    function AllowInitScrollbar: Boolean; override;
    function CreateEditor: TEditView; virtual;
    function CreateScroll: TScrollBar; override;
    function GetRealDrawState: TViewState; override;
    function GetDefaultSize: TSizeF; override;
    procedure RealignContent; override;
    procedure SetScrollbar(const Value: TViewScroll); override;
    procedure UpdateScrollBar(AScroll: TScrollBar; AScrollBar: TViewScroll; const ValueOffset: Double = 0); override;
  protected
    procedure DoClickFixedCell(const ACol, ARow: Integer); virtual;
    // �Զ������п����¼�
    procedure DoAutoAdjuestColWidth(Item: TGridColumnItem; ColIndex: Integer); virtual;

    // ��������
    procedure DoFilterData(Item: TGridColumnItem); virtual;
    // �������ݸı�
    procedure DoFilterDataChange(Item: TGridColumnItem); virtual;
    // ���ع������ݵĲ˵��б�
    procedure DoInitFilterDataList(Item: TGridColumnItem; List: TStrings); virtual;
  protected
    { ��ͷ�϶���� }
    FDownPos: TPointF;
    FMovePos: TPointF;
    FHotItemRange: TRectF;
    FHotHeaderIndex: Integer;
    FDragView: TTextView;
    [Weak] FHotItem: TGridColumnItem;
    [Weak] FAdjuestItem: TGridColumnItem;

    FDownFixedRowIndex: Integer;
    FDownFixedColIndex: Integer;

    FSelectionAnchor: Integer;  // ��ǰѡ���� 

    function PointInItem(const P: TPointF; var R: TRectF): Integer;     
    
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure Click; override;
    procedure DblClick; override;

    procedure ShowEditor; virtual;
    procedure HideEditor; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetKey(const ACol, ARow: Integer): Int64; inline;

    // �������
    procedure Clear;

    // ֪ͨ���ݷ����ı�
    procedure NotifyDataChanged; virtual;

    // ������ת��Ϊ����
    function PointAsCell(const X, Y: Single): TGridCell;

    // ������ѡ��ĵ�Ԫ��
    procedure ScrollToSelectedCell;
    // ������ָ���ĵ�Ԫ��
    procedure ScrollToCell(const ACell: TGridCell);

    property Empty: Boolean read IsEmpty;
    property Adapter: IGridAdapter read FAdapter write SetAdapter;
    property ContentViews: TGridViewContent read FContentViews;
    property Columns: TGridColumns read FColumns;

    property Cells[const ACol, ARow: Integer]: string read GetCellsData write SetCellsData;

    // ָʾ����ǰ��
    property SelectionAnchor: Integer read FSelectionAnchor write SetSelectionAnchor;
    // ��ǰѡ����
    property SelectIndex: Integer read GetSelectIndex write SetSelectIndex;

    /// <summary>
    /// �кŹ̶��п���
    /// </summary>
    property FixedIndicatorWidth: Single read GetFixedIndicatorWidth;

    /// <summary>
    /// ����Ĭ���и�
    /// </summary>
    property RowHeight: Single read GetRowHeightPro write SetRowHeightPro stored IsStoredRowHeight;

    /// <summary>
    /// ����
    /// </summary>
    property RowCount: Integer read GetRowCount;
    /// <summary>
    /// ����
    /// </summary>
    property ColCount: Integer read GetColCount;

    /// <summary>
    /// ѡ���������
    /// </summary>
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;

    /// <summary>
    /// �ָ�����ɫ
    /// </summary>
    property Divider: TAlphaColor read FDivider write SetDivider default CDefaultDividerColor;
    /// <summary>
    /// �ָ��߸߶�
    /// </summary>
    property DividerHeight: Single read FDividerHeight write SetDividerHeight stored IsStoredDividerHeight;

    property ScrollSmallChangeFraction;
    property ScrollStretchGlowColor;

    /// <summary>
    /// ����ѡ��
    /// </summary>
    property Options: TGridOptions read FOptions write SetOptions default CDefaultOptions;

    /// <summary>
    /// ��Ԫ�����ˢ������
    /// </summary>
    property DrawableCells: TGridViewCellBrush read GetCellBrush write SetCellBrush;

    /// <summary>
    /// ��Ԫ��������
    /// </summary>
    property ColumnsSettings: TGridColumnsSetting read FColumnsSetting; // write SetColumnsSetting;

    /// <summary>
    /// �̶���Ԫ������
    /// </summary>
    property FixedSettings: TGridFixedSetting read FFixedSetting write SetFixedSetting;

    /// <summary>
    /// ��������
    /// </summary>
    property TextSettings: TGridTextSettings read FText write SetText;

    /// <summary>
    /// �̶���Ԫ�����ˢ��
    /// </summary>
    property FixedBrush: TGridViewBrush read FFixedBrush write SetFixedBrush;
    /// <summary>
    /// �̶���Ԫ�������
    /// </summary>
    property FixedCols: Integer read FFixedCols write SetFixedCols default 0;
    /// <summary>
    /// �̶���Ԫ�������
    /// </summary>
    property FixedRows: Integer read GetFixedRows write SetFixedRows default 1;
    /// <summary>
    /// �̶���Ԫ���и�
    /// </summary>
    property FixedRowHeight: Single read FFixedRowHeight write SetFixedRowHeight;
    /// <summary>
    /// �̶���Ԫ��Ĭ���п������������������иı䣩
    /// </summary>
    property FixedColWidth: Single read FFixedDefaultColWidth write SetFixedDefaultColWidth;
    /// <summary>
    /// �̶���Ԫ�����
    /// </summary>
    property FixedWidth: Single read GetFixedWidth;
    /// <summary>
    /// �̶���Ԫ��ָ�����ɫ
    /// </summary>
    property FixedDivider: TAlphaColor read FFixedDivider write SetFixedDivider default TGridFixedSetting.CDefaultFixedDividerColor;
    /// <summary>
    /// �̶���Ԫ����������
    /// </summary>
    property FixedTextSettings: TGridTextSettings read FFixedText write SetFixedText;
    /// <summary>
    /// �к��б���
    /// </summary>
    property FixedTextRowIndex: string read FTextRowIndex write SetTextRowIndex;

    /// <summary>
    /// �Ƿ�ȫ��ֻ��
    /// </summary>
    property ReadOnly: Boolean read FReadOnly write FReadOnly;

    property HitTest default True;
    property OnScrollChange;

    /// <summary>
    /// ��ͷ����¼�
    /// </summary>
    property OnTitleClick: TOnTitleClickEvent read FOnTitleClickEvent write FOnTitleClickEvent;
    /// <summary>
    /// ��ͷ�ƶ��¼�
    /// </summary>
    property OnColumnMoved: TOnColumnMovedEvent read FOnColumnMovedEvent write FOnColumnMovedEvent;
    /// <summary>
    /// �̶����ӵ����¼�
    /// </summary>
    property OnFixedCellClick: TOnFixedCellClickEvent read FOnFixedCellClickEvent write FOnFixedCellClickEvent;

    /// <summary>
    /// ��Ԫ�����¼�
    /// </summary>
    property OnCellClick: TOnCellEvent read FOnCellClickEvent write FOnCellClickEvent;
    /// <summary>
    /// ��Ԫ������¼�
    /// </summary>
    property OnCellEnter: TOnCellEvent read FOnCellEnterEvent write FOnCellEnterEvent;
    /// <summary>
    /// ��Ԫ���뿪�¼�
    /// </summary>
    property OnCellLeave: TOnCellEvent read FOnCellLeaveEvent write FOnCellLeaveEvent;
    /// <summary>
    /// ��Ԫ��ѡ�¼�
    /// </summary>
    property OnCellCheck: TOnCellCheckEvent read FOnCellCheckEvent write FOnCellCheckEvent;
    /// <summary>
    /// ��Ԫ��༭����¼�
    /// </summary>
    property OnCellEditDone: TOnCellEditDoneEvent read FOnCellEditDoneEvent write FOnCellEditDoneEvent;
    /// <summary>
    /// ItemIndex �ı��¼�
    /// </summary>
    property OnItemIndexChange: TNotifyEvent read FOnItemIndexChange write FOnItemIndexChange;
    /// <summary>
    /// ��ѡ��ı��¼�
    /// </summary>
    property OnRowSelChange: TNotifyEvent read FOnRowSelChangeEvent write FOnRowSelChangeEvent;

    /// <summary>
    /// �Զ��屳������
    /// </summary>
    property OnDrawBackgroud: TOnDrawViewBackgroud read FOnDrawViewBackgroud
      write FOnDrawViewBackgroud;
    /// <summary>
    /// �Զ���̶��е�Ԫ�����
    /// </summary>
    property OnDrawFixedColText: TOnDrawFixedColText read FOnDrawFixedColText write FOnDrawFixedColText;
    /// <summary>
    /// �Զ���̶��е�Ԫ�����
    /// </summary>
    property OnDrawFixedCellsText: TOnDrawFixedCellsText read FOnDrawFixedCellsText write FOnDrawFixedCellsText;
    /// <summary>
    /// ���Ƶ�Ԫ���ı�
    /// </summary>
    property OnDrawCells: TOnDrawCells read FOnDrawCells write FOnDrawCells;
  end;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TGridView = class(TGridBase)
  published
    property Divider;
    property DividerHeight;

    property ShowScrollBars;
    property ScrollBars default TViewScroll.Vertical;
    property ScrollSmallChangeFraction;
    property ScrollStretchGlowColor;
    property ScrollbarWidth;

    property RowHeight;

    property DrawableCells;
    property ReadOnly;

    property Options;

    property ColumnsSettings;
    property FixedSettings;

    property TextSettings;

    property HitTest default True;
    property Clickable default True;
    property DragScroll;
    property OnScrollChange;

    property OnTitleClick;
    property OnColumnMoved;
    property OnFixedCellClick;
    
    property OnCellClick;
    property OnCellEnter;
    property OnCellLeave;
    property OnCellCheck;
    property OnCellEditDone;

    property OnItemIndexChange;
    property OnRowSelChange;

    property OnDrawBackgroud;
    property OnDrawFixedColText;
    property OnDrawFixedCellsText;

    property OnDrawCells;
  end;

type
  /// <summary>
  /// �ַ�������
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TStringGridView = class(TGridView)
  private
    procedure SetRowCount(const Value: Integer);
    procedure SetColCount(const Value: Integer);
    procedure SetShowColIndex(const Value: Boolean);
    function GetShowColIndex: Boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColCount: Integer read GetColCount write SetColCount default 5;
    property RowCount: Integer read GetRowCount write SetRowCount default 5;

    // ��ʾ�к�
    property ShowColIndex: Boolean read GetShowColIndex write SetShowColIndex default False;
  end;

type
  TDBGridView = class;

  TDBGridDataLink = class(TDataLink)
  private
    FGrid: TDBGridView;
    FModified: Boolean;
    FInUpdateData: Boolean;
    function GetFields(I: Integer): TField;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure EditingChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AGrid: TDBGridView);
    destructor Destroy; override;
    procedure Modified;
    procedure Reset;
    property Fields[I: Integer]: TField read GetFields;
    property Grid: TDBGridView read FGrid;
  end;

  /// <summary>
  /// ���ݱ�����
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TDBGridView = class(TGridView)
  private const
    CDefaultDBOptions = [gvEditing, gvIndicator, gvRowIndex, gvColLines, gvRowLines, gvTwoColor, gvCancelOnExit,
     gvEscCancelEdit, gvColumnBestWidth, gvColumnResize, gvColumnMove ];
  private
    FDataLink: TDBGridDataLink;
    FDataRecordCount: Integer;

    FUseCustomColumns: Boolean;
    FUpdateing: Boolean;

    FShowCheck: Boolean;

    FFilterDataList: TDictionary<Integer, string>;

    function GetMinRowCount: Integer;
    procedure SetMinRowCount(const Value: Integer);
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetSelectedField: TField;
    function GetRecordCount: Integer;
    procedure SetShowCheck(const Value: Boolean);
  protected
    procedure InitColumns(const DataSet: TDataSet); virtual;
    procedure UpdateRowCount(const DataSet: TDataSet); virtual;
    function GetFieldDisplayWidth(Field: TField): Single; virtual;

    class function IsBlob(const DataType: TFieldType): Boolean; inline;
  protected
    { DataLink }
    procedure LinkActive(Value: Boolean); virtual;
    procedure DataChanged;
    procedure EditingChanged;
    procedure RecordChanged(Field: TField);
    procedure UpdateData; virtual;

    procedure DoFilterDataChange(Item: TGridColumnItem); override;
    procedure DoInitFilterDataList(Item: TGridColumnItem; List: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure ClearColumns();

    procedure Post; virtual;
    procedure Reset; virtual;

    /// <summary>
    /// �����ֶ�
    /// </summary>
    function AddField(const FieldName: string; const DisplayText: string = ''; const ADisplayColWidth: Single = -1;
      const Visible: Boolean = True; const ReadOnly: Boolean = False): TGridDBColumnItem;

    /// <summary>
    /// �����ֶ�
    /// </summary>
    function AddCheckField(const FieldName: string; const DisplayText: string = ''; const ADisplayColWidth: Single = 24;
      const ReadOnly: Boolean = False): TGridDBColumnItem;

    property SelectedField: TField read GetSelectedField;

    /// <summary>
    /// ���ݼ���¼����
    /// </summary>
    property RecordCount: Integer read GetRecordCount;

  published
    property MinRowCount: Integer read GetMinRowCount write SetMinRowCount default 0;

    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property Options default CDefaultDBOptions;
    /// <summary>
    /// ���Զ�ʹ������Դ�ֶ���Ϊ��ͷʱ���Ƿ��ڵ�һ����ʾ��ѡ��
    /// </summary>
    property ShowCheck: Boolean read FShowCheck write SetShowCheck;
  end;

type
  /// <summary>
  /// GridView ��������������
  /// </summary>
  TGridAdapterBase = class(TInterfacedObject, IGridAdapter)
  private
    [Weak] FGridView: TGridBase;
    function GetCellSetting(const ACol, ARow: Integer): TGridCellSettings;
    procedure DoValueNotify(Sender: TObject; const Item: TGridCellSettings;
      Action: System.Generics.Collections.TCollectionNotification);
  protected
    FItemIndex: Integer;
    FDefaultRowHeight: Single;
    FCellSetttings: TDictionary<Int64, TGridCellSettings>;  // ��Ԫ���ʽ

    function IsEmpty: Boolean;
    function GetColCount: Integer;
    function GetColumns: TGridColumns; virtual;
  protected
    procedure DoInitData; virtual;

    function GetRowID(const ARow: Integer): Int64; virtual;
    function GetRowHeight: Single; virtual;

    function ItemDefaultHeight: Single; virtual;
    procedure SetItemDefaultHeight(const Value: Single); virtual;

    procedure SetCursor(const ARow: Integer); virtual;
    procedure BeginDrawCells(const AFirstRow, ALastRow: Integer); virtual;
    procedure EndDrawCells(); virtual;

    /// <summary>
    /// ��ȡ����п�
    /// </summary>
    function GetBestColumnWidth(const ACol: Integer): Single; virtual;

    function GetRowCount: Integer; virtual; abstract;
    procedure SetRowCount(const Value: Integer); virtual; abstract;

    function GetFixedColData(const ACol: Integer): TGridFixedColumnItem; virtual;

    function GetCells(const ACol, ARow: Integer): string; virtual;
    procedure SetCells(const ACol, ARow: Integer; const Value: string); virtual;

    function GetCellChecked(const ACol, ARow: Integer): Boolean; virtual;
    procedure SetCellChecked(const ACol, ARow: Integer; const Value: Boolean); virtual;

    function GetCellHeight(const ACol, ARow: Integer): Single; virtual;

    function GetItemIndex: Integer; virtual;
    procedure SetItemIndex(const Value: Integer); virtual;

    function GetCellData(const ACol, ARow: Integer): Pointer; virtual; abstract;
    procedure SetCellData(const ACol, ARow: Integer; const Value: Pointer); virtual; abstract;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure Repaint; virtual;
    procedure NotifyDataChanged; virtual;

    function GetCellSettings(const ACol, ARow: Integer; out ACellSettings: TGridCellSettings): Boolean; virtual;
    procedure SetCellSettings(const ACol, ARow: Integer; const ACellSettings: TGridCellSettings); virtual;

    property GridView: TGridBase read FGridView write FGridView;
    property Empty: Boolean read IsEmpty;

    property RowCount: Integer read GetRowCount write SetRowCount;
    property ColCount: Integer read GetColCount;
    property Cells[const ACol, ARow: Integer]: string read GetCells write SetCells; default;
    property CellChecked[const ACol, ARow: Integer]: Boolean read GetCellChecked write SetCellChecked;
    property CellSettings[const ACol, ARow: Integer]: TGridCellSettings read GetCellSetting write SetCellSettings;
    property CellData[const ACol, ARow: Integer]: Pointer read GetCellData write SetCellData;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  end;

type
  /// <summary>
  /// �ַ�����������������
  /// </summary>
  TStringGridAdapter = class(TGridAdapterBase)
  private
    FData: TDictionary<Int64, string>;
    FRowCount: Integer;
  protected
    procedure DoInitData; override;

    function GetRowCount: Integer; override;
    procedure SetRowCount(const Value: Integer); override;

    function GetCellData(const ACol, ARow: Integer): Pointer; override;
    procedure SetCellData(const ACol, ARow: Integer; const Value: Pointer); override;

    function GetCells(const ACol, ARow: Integer): string; override;
    procedure SetCells(const ACol, ARow: Integer; const Value: string); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
  end;

type
  /// <summary>
  /// ���ݱ�������
  /// </summary>
  TDBGridAdapter = class(TGridAdapterBase)
  private
    FFieldMap: TIntHash;
    FData: TDictionary<Int64, string>;
    FEditMap: TList<TGridCell>;
    FRowCount: Integer;
    FIsOutDataRow: Boolean;
    FMinRowCount: Integer;
    FDrawCelling: Boolean;
  protected
    function GetRowCount: Integer; override;
    procedure SetRowCount(const Value: Integer); override;

    procedure SetCursor(const ARow: Integer); override;
    procedure BeginDrawCells(const AFirstRow, ALastRow: Integer); override;
    procedure EndDrawCells(); override;

    procedure PostData(); virtual;

    function GetCellData(const ACol, ARow: Integer): Pointer; override;
    procedure SetCellData(const ACol, ARow: Integer; const Value: Pointer); override;

    function GetCells(const ACol, ARow: Integer): string; override;
    procedure SetCells(const ACol, ARow: Integer; const Value: string); override;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Clear; override;
    // ��С��ʾ������
    property MinRowCount: Integer read FMinRowCount write FMinRowCount;
  end;

implementation

uses
  UI.Grid.Res, UI.Dialog, UI.ListView;

resourcestring
  SInvFixedIndex = '������ָ���Ĺ̶�����';

type
  TGridResDrawable = class(TDrawableIcon);

  // ����״̬
  TGridViewDrawState = record
  private
    function GetIsUp: Boolean;
  public
    RowIndex: Integer;   // ��ǰ��������
    FirstColIndex: Integer; // ��ǰ��ʾ�ĵ�һ���к�
    FirstColOffset: Double; // ��ǰ��ʾ�ĵ�һ��λ��
    FixedCols: Integer;  // �̶�����
    FixedWidth: Single;  // �̶����ܿ���
    ColumnWidth: Single; // �����е��ܿ���
    Top, Bottom: Double; // ��ǰ�е�λ��
    DividerH: Double;    // �ָ��߸߶�
    Opacity: Single;     // ͸����
    ExistAdapter: Boolean;  // �Ƿ����������

    XOffset: Double;     // X ƫѳ
    MaxCols: Integer;    // �������

    AdjustH: Double;            // ���ֱ仯ʱ�߶ȱ仯

    Width: Single;          // �ܿ���
    Height: Single;         // �ܸ߶�
    MoveSpace: Double;      // ��������
    ScrollValue: Double;    // ������λ��
    RowCount: Integer;      // ������

    SelectionAnchor: Integer;

    ShowColLine, ShowRowLine: Boolean;

    property IsUp: Boolean read GetIsUp;
  end;

  TGridFilterDownListAdapter = class(TStringsListAdapter)
  private
    procedure DoItemIndexChange(Sender: TObject);
  protected
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
  end;

var
  FGridRes: TGridRes; // ������Դ

{ TGridBase }

function TGridBase.AllowInitScrollbar: Boolean;
begin
  Result := True;
end;

function TGridBase.CanRePaintBk(const View: IView; State: TViewState): Boolean;
begin
  Result := ((State = TViewState.None) and (not AniCalculations.Animation)) or
    ((State = TViewState.Pressed) and (Assigned(FFixedBrush.FPressed))) or
    ((FDownFixedRowIndex <> -1) or (FDownFixedColIndex <> -1));
end;

procedure TGridBase.Clear;
begin
  if Assigned(FAdapter) then begin
    FAdapter.Clear;
    FCount := 0;
    NotifyDataChanged;
  end;
end;

procedure TGridBase.Click;
begin
  if (not Assigned(FAdjuestItem)) and ((FDragView = nil) or (not FDragView.Visible)) then begin
    if Assigned(FHotItem) and FHotItem.Enabled then begin
      if FHotItem.DataFilter and (FDownPos.X > FHotItem.X + FContentViews.Left + FHotItem.Width - CDefaultFilterIconWH - 4) then
        DoFilterData(FHotItem)
      else if Assigned(FOnTitleClickEvent) then
        FOnTitleClickEvent(Self, FHotItem);
    end else begin
      if (FMovePos.X >= FContentViews.Left) then
        Exit;
      if (FDownFixedRowIndex <> -1) or (FDownFixedColIndex <> -1) then begin
        SelectionAnchor := FDownFixedRowIndex;
        DoClickFixedCell(FDownFixedColIndex, FDownFixedRowIndex);
      end;
    end;
  end;
  inherited Click;
end;

constructor TGridBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  New(FContentBounds);
  FContentBounds.Top := 0;
  FContentBounds.Left := 0;
  FContentBounds.Right := 0;
  FContentBounds.Bottom := 0;

  FDefaultRowHeight := -1;

  FScrollSmallChangeFraction := CDefaultFixedRowHeight;

  FTextRowIndex := CDefaultTextRowIndex;
  FOptions := CDefaultOptions;

  FDrawBuffer := TBitmap.Create;

  FText := TGridTextSettings.Create(Self);
  FText.OnChanged := DoTextChange;

  FFixedDivider := TGridFixedSetting.CDefaultFixedDividerColor;
  FFixedBrush := TGridViewBrush.Create(Self, TViewBrushKind.Solid, CDefaultFixedColor);
  FFixedBrush.ItemPressed.Kind := TViewBrushKind.Solid;
  FFixedBrush.ItemPressed.Color := $1f000000;
  FFixedBrush.ItemPressed.DefaultColor := FFixedBrush.ItemPressed.Color;
  FFixedBrush.ItemPressed.DefaultKind := TBrushKind.Solid;

  FFixedBrush.OnChanged := DoFixedBrushChange;
  FFixedCols := 0;
  FFixedRowHeight := CDefaultFixedRowHeight;
  FFixedDefaultColWidth := CDefaultFixedColWidth;

  FFixedText := TGridTextSettings.Create(Self);
  FFixedText.OnChanged := DoTextChange;

  FFixedMergeMap := TIntHash.Create(97);
  FFixedHeaderRange := TList<TGridFixedHeaderState>.Create;

  FColumns := TGridColumns.Create(Self);
  FColumns.OnChange := DoColumnsChange;

  FColumnsSetting := TGridColumnsSetting.Create(Self);
  FColumnsSetting.FColumns := FColumns;
  FFixedSetting := TGridFixedSetting.Create(Self);

  FDivider := CDefaultDividerColor;
  FDividerHeight := -1;
  FLocalDividerHeight := -1;
  SetLength(FItemsPoints, 0);

  FDownFixedRowIndex := -1;
  FDownFixedColIndex := -1;

  FSelectionAnchor := 0;

  FDisablePaint := True;

  try
    CreateCoentsView();

    with TDrawableBorder(Background) do begin
      Border.Color.Default := TGridFixedSetting.CDefaultFixedDividerColor;
      Border.Color.DefaultChange := False;
      Border.Style := TViewBorderStyle.RectBorder;
      Border.DefaultStyle := Border.Style;
      ItemDefault.Color := TAlphaColorRec.White;
      ItemDefault.Kind := TViewBrushKind.Solid;
      ItemDefault.DefaultColor := TAlphaColorRec.White;
      ItemDefault.DefaultKind := TBrushKind.Solid;
    end;

    ScrollBars := TViewScroll.Both;
    DisableFocusEffect := True;
    AutoCapture := True;
    ClipChildren := True;
    with Background.ItemPressed do begin
      Color := CDefaultBKPressedColor;
      Kind := TViewBrushKind.Solid;
    end;

    HitTest := True;
    Padding.DefaultValue := RectF(1,1,1,1);
    Padding.Rect := Padding.DefaultValue;

    if not (csDesigning in ComponentState) then begin
      FDragView := TTextView.Create(Self);
      FDragView.Parent := Self;
      FDragView.Background.ItemDefault.Kind := TViewBrushKind.Solid;
      FDragView.Background.ItemDefault.Color := $3f101010;
      FDragView.Locked := True;
      FDragView.Stored := False;
      FDragView.Visible := False;
    end;
  finally
    FDisablePaint := False;
  end;
end;

procedure TGridBase.CreateCoentsView;
begin
  FContentViews := TGridViewContent.Create(Self);
  FContentViews.Name := '';
  FContentViews.Visible := True;
  FContentViews.Stored := False;
  FContentViews.Locked := True;
  FContentViews.Parent := Self;
  FContentViews.GridView := Self;
  FContentViews.WidthSize := TViewSize.FillParent;
  FContentViews.HeightSize := TViewSize.FillParent;
  if not (csDesigning in ComponentState) then
    FContentViews.HitTest := True;
  FContentViews.FEditor := CreateEditor;
  FContentViews.Cursor := crArrow;
  RealignContent;
end;

function TGridBase.CreateEditor: TEditView;
begin
  Result := TEditView.Create(FContentViews);
  Result.Visible := False;
  Result.Stored := False;
  Result.Locked := True;
  Result.Name := '';
  Result.Text := '';
  with Result.Background.ItemDefault do begin
    Color := TAlphaColorRec.White;
    Kind := TViewBrushKind.Solid;
  end;
  Result.Padding.Rect := RectF(CDefaultFixedColPadding,CDefaultFixedColPadding,
    CDefaultFixedColPadding,CDefaultFixedColPadding);
  Result.Caret.Color := TAlphaColorRec.Black;
  Result.SelectionFill.Color := $3f0000ff;
  Result.Parent := FContentViews;
  Result.OnKeyDown := FContentViews.DoKeyDown;
  Result.OnExit := FContentViews.DoEditExit;
end;

function TGridBase.CreateScroll: TScrollBar;
begin
  {$IFDEF MSWINDOWS}
  if DragScroll then
    Result := TSmallScrollBar.Create(Self)
  else
    Result := TScrollBar.Create(Self);
  {$ELSE}
  Result := TSmallScrollBar.Create(Self);
  {$ENDIF}
  Result.Cursor := crArrow;
end;

procedure TGridBase.DblClick;
begin
  if Assigned(FAdjuestItem) then
    DoAutoAdjuestColWidth(FAdjuestItem, FAdjuestItem.Index);
  inherited;
end;

destructor TGridBase.Destroy;
begin
  FAdapter := nil;
  if Assigned(FDragView) then begin
    RemoveComponent(FDragView);
    FreeAndNil(FDragView);
  end;
  FreeAndNil(FFixedBrush);
  FreeAndNil(FColumns);
  FreeAndNil(FText);
  FreeAndNil(FFixedText);
  FreeAndNil(FFixedMergeMap);
  FreeAndNil(FFixedHeaderRange);
  FreeAndNil(FDrawBuffer);
  FreeAndNil(FFixedSetting);
  FreeAndNil(FColumnsSetting);
  FreeAndNil(FFilterList);
  inherited Destroy;
end;

procedure TGridBase.DoAutoAdjuestColWidth(Item: TGridColumnItem;
  ColIndex: Integer);
begin
  if Assigned(FAdapter) and (gvColumnBestWidth in FOptions) then begin
    Item.Width := FAdapter.GetBestColumnWidth(ColIndex);
    FContentViews.InitColumnList;
  end;
end;

procedure TGridBase.DoClickFixedCell(const ACol, ARow: Integer);
begin
  if Assigned(FOnFixedCellClickEvent) then
    FOnFixedCellClickEvent(Self, ACol, ARow);
end;

procedure TGridBase.DoColumnsChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then begin
    if Assigned(FContentViews) then begin
      FContentViews.FLastW := 0;
      FContentViews.InitColumnList;
    end;
    HandleSizeChanged;
    RealignContent;
    Invalidate;
  end;
end;

procedure TGridBase.DoDrawBackground(var R: TRectF);
begin
  if Assigned(FOnDrawViewBackgroud) then
    FOnDrawViewBackgroud(Self, Canvas, R, DrawState);
end;

procedure TGridBase.DoDrawFixedCellsText(Canvas: TCanvas; const ACol,
  ARow: Integer; const Item: TGridFixedColumnItem; const R: TRectF;
  const AOpacity: Single);
var
  VR: TRectF;
begin
  if (ACol = 0) and (ARow < FCount) and (gvRowIndex in FOptions) then begin
    VR := RectF(R.Left + CDefaultFixedColPadding, R.Top + CDefaultFixedColPadding,
        R.Right - CDefaultFixedColPadding, R.Bottom - CDefaultFixedColPadding);
    if gvIndicator in FOptions then
      VR.Right := VR.Right - CDefaultAnchorWidth;
    // ���к�
    FFixedText.Draw(Canvas, Item.Title, VR, AOpacity, DrawState);
  end;
end;

procedure TGridBase.DoDrawFixedColBackground(Canvas: TCanvas; const R: TRectF; const AOpacity: Single);
begin
  if TViewState.Enabled in ViewState then begin
    if Assigned(FFixedBrush.FEnabled) then begin
      Canvas.FillRect(R, 0, 0, [], AOpacity, FFixedBrush.FEnabled);
    end;
  end else if TViewState.Pressed in ViewState then begin
    if Assigned(FFixedBrush.FPressed) then
      Canvas.FillRect(R, 0, 0, [], AOpacity, FFixedBrush.FPressed);
  end;
end;

procedure TGridBase.DoDrawFixedColText(Canvas: TCanvas; Item: TGridColumnItem;
  const R: TRectF; const AOpacity: Single);
begin     
  if Assigned(FOnDrawFixedColText) then
    FOnDrawFixedColText(Self, Canvas, Item, R)
  else 
    FFixedText.Draw(Canvas, Item.DispLayText, R, AOpacity, DrawState, FFixedText.Gravity);
end;

procedure TGridBase.DoDrawFixedRowIndexText(Canvas: TCanvas; const R: TRectF;
  const AOpacity: Single);
var
  Item: TGridFixedColumnItem;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.Width := FixedIndicatorWidth;
  Item.Title := FTextRowIndex;
  if Assigned(FOnDrawFixedCellsText) then
    FOnDrawFixedCellsText(Self, Canvas, 0, -1, Item, R)
  else
    DoDrawFixedCellsText(Canvas, 0, -1, Item, R, AOpacity);
end;

procedure TGridBase.DoDrawFixedRowText(Canvas: TCanvas; const Row: Integer;
  const R: TRectF; const AOpacity: Single);
var
  I: Integer;
  Item: TGridFixedColumnItem;
  L, DH: Single;
  VR: TRectF;
  LFixedCols: Integer;
begin
  if not Assigned(FAdapter) then begin
    FillChar(Item, SizeOf(Item), 0);
    Item.Width := FFixedDefaultColWidth;
  end;
  L := R.Left;
  DH := GetDividerHeight;
  
  LFixedCols := FFixedCols;
  if (gvIndicator in FOptions) or (gvRowIndex in FOptions) then
    Inc(LFixedCols);
    
  for I := 0 to LFixedCols - 1 do begin
    if (I = 0) and (LFixedCols > FFixedCols) then begin
      Item.Width := FixedIndicatorWidth;
      if gvDisplayZero in FOptions then        
        Item.Title := IntToStr(Row)
      else
        Item.Title := IntToStr(Row + 1);

      // ����ǰ�е�С����
      if (Row = FSelectionAnchor) and (gvIndicator in FOptions) then begin
        VR.Left := L + Item.Width - CDefaultAnchorWidth - 1;
        VR.Top := R.Top +  (R.Bottom - R.Top - CDefaultAnchorHeight) * 0.5;
        VR.Right := VR.Left + CDefaultAnchorWidth;
        VR.Bottom := VR.Top + CDefaultAnchorHeight;
        FGridRes.Drawable.ImageIndex := 0;
        FGridRes.Drawable.Draw(Canvas, VR, 0, 0, [], AOpacity);
      end;

    end else if Assigned(FAdapter) then
      Item := FAdapter.GetFixedColData(I)
    else begin
      Item.Width := FFixedDefaultColWidth;
      Item.Title := '';
    end;
    VR := RectF(L, R.Top, L + Item.Width, R.Bottom);
    if (I = FDownFixedColIndex) and (Row = FDownFixedRowIndex) then
      DoDrawFixedColBackground(Canvas, VR, AOpacity);
    if Assigned(FOnDrawFixedCellsText) then
      FOnDrawFixedCellsText(Self, Canvas, I, Row, Item, VR)
    else
      DoDrawFixedCellsText(Canvas, I, Row, Item, VR, AOpacity);
    L := L + Item.Width + DH;
  end;  
end;

procedure TGridBase.DoDrawHeaderRows(Canvas: TCanvas; var R: TRectF);
var
  H, W, LH, LW, MH, MW: Double;
  DH, V, LV: Double;
  XOffset: Double;
  I, J, K, M, N, LI, LFixedCols: Integer;
  Item: TGridColumnItem;
  LDrawLine: Boolean;
  LR, VR: TRectF;
  LOpacity: Single;
  LState: TGridFixedHeaderState;
  PH: PSingle;
begin
  DH := GetDividerHeight;
  if (FFixedRowHeight <= 0) and (DH <= 0) then
    Exit;
    
  R.Left := Padding.Left;
  R.Top := Padding.Top;
  R.Right := R.Right - Padding.Right;
  R.Bottom := R.Bottom - Padding.Bottom;
  {$IFDEF MSWINDOWS}
  if Assigned(FScrollH) and (FScrollH.Visible) then
    R.Bottom := R.Bottom - FScrollH.Height;
  if Assigned(FScrollV) and (FScrollV.Visible) then
    R.Right := R.Right - FScrollV.Width;
  {$ENDIF}

  LOpacity := Opacity;
  XOffset := 0 - HScrollBarValue;
  H := (FFixedRowHeight + DH) * FixedRows;
  LW := FixedWidth;
  LV := R.Left + XOffset;

  MH := Height - Padding.Bottom;
  if Assigned(FScrollH) and (FScrollH.Visible) then
    MH := MH - FScrollH.Height;
  MW := Width - Padding.Right;
  if Assigned(FScrollV) and (FScrollV.Visible) then
    MW := MW - FScrollV.Width;

  if Assigned(FColumns) then
    W := FColumns.Width
  else
    W := 0;

  // ������
  LR := RectF(R.Left + LW, R.Top, R.Left + W + LW + XOffset, R.Top + H);
  if Assigned(FFixedBrush.FDefault) then begin
    if LR.Right > MW then LR.Right := MW;
    Canvas.FillRect(LR, 0, 0, [], LOpacity, FFixedBrush.FDefault);
  end;

  // ����ͷ
  LDrawLine := (DH > 0) and (FDivider and $FF000000 <> 0);
  FContentViews.FDividerBrush.Color := FFixedDivider;

  if FFixedMergeMap.Count > 0 then
    FFixedMergeMap.Clear;
  if FFixedHeaderRange.Count > 0 then
    FFixedHeaderRange.Clear;

  if Assigned(FColumns) then begin
    H := LR.Top;
    for J := 0 to FixedRows - 1 do begin
      I := 0;
      V := LV;

      while I < FColumns.ColsCount do begin
        Item := FColumns[I, J];
        if not Item.Visible then begin
          Inc(I);
          Continue;
        end;
        // �����Ԫ�񱻺ϲ�����ֱ�Ӽ��������
        if FFixedMergeMap.ContainsKey(GetKey(I, J)) then begin
          Inc(I);
          V := V + Item.Width + DH;
          Continue;
        end;
        LI := I;

        if V + LW >= MW then
          Break;

        // �������ǰ���Ӵ�С
        W := V + Item.Width + DH;
        if Item.ColsPan > 1 then begin
          // �жϺϲ�������Ƿ񳬳����һ�У�����ʱ�������Ϊ׼
          if I + Item.ColsPan > FColumns.FMaxCols then
            Item.ColsPan := FColumns.FMaxCols - I;

          for K := 1 to Item.ColsPan - 1 do begin
            if FColumns[I + K, J].Visible then
              W := W + DH + FColumns[I + K, J].Width;
          end;
          Inc(I, Item.ColsPan);
        end else
          Inc(I);
        LH := H;

        if Item.RowsPan > 1 then begin
          M := Item.RowsPan;
          if M + J > FColumns.RowsCount then
            M := FColumns.RowsCount - J;
          for K := 1 to M - 1 do
            LH := LH + DH + FFixedRowHeight;
        end else if Item.RowsPan < 0 then begin
          LH := LR.Bottom - DH;
          M := Columns.RowsCount;
        end else
          M := 0;

        // ����ǰ���Ӻϲ��ĸ��Ӽ���Map
        if (Item.ColsPan > 1) or (M > 1) then begin
          for N := 0 to Item.ColsPan - 1 do begin
            for K := 0 to M - 1 do
              FFixedMergeMap.Add(GetKey(LI + N, J + K), 1);
          end;
        end;

        // ��¼����״̬
        LState.Row := J;
        LState.Col := LI;
        LState.R := RectF(V + LW, H, W + LW, LH + FFixedRowHeight);
        FFixedHeaderRange.Add(LState);

        if W + LW > LW then begin
          // ������
          if not Assigned(FAdjuestItem) then begin
            if Assigned(FHotItem) and (FHotItem = Item) and (FHotItem.Enabled) then begin
              VR.Left := V + LW;
              VR.Top := H;
              VR.Right := W + LW;
              VR.Bottom := LH + FFixedRowHeight;
              DoDrawFixedColBackground(Canvas, VR, LOpacity);
            end;
          end;
          // ������ͼ��
          if Item.DataFilter and (Item.Width > CDefaultFilterIconWH + 2) then begin
            VR.Left := W + LW - Padding.Right - CDefaultFilterIconWH - 1;
            VR.Top := LH + FFixedRowHeight - Padding.Bottom - CDefaultFilterIconWH - 1;
            VR.Right := VR.Left + CDefaultFilterIconWH;
            VR.Bottom := VR.Top + CDefaultFilterIconWH;
            FGridRes.Drawable.ImageIndex := 9;
            FGridRes.Drawable.Draw(Canvas, VR, 0, 0, [], LOpacity);
          end;
          // ������
          with Item.Padding do begin
            VR.Left := V + LW + Left;
            VR.Top := H + Top;
            VR.Right := W + LW - Right;
            VR.Bottom := LH + FFixedRowHeight - Bottom;
          end;
          DoDrawFixedColText(Canvas, Item, VR, LOpacity);

          if LDrawLine then begin
            // ������
            VR.Left := W + LW - DH;
            if VR.Left < R.Right then begin
              VR.Top := H;
              VR.Right := W + LW;
              VR.Bottom := LH + FFixedRowHeight;
              Canvas.FillRect(VR, 0, 0, [], LOpacity, FContentViews.FDividerBrush);
            end;
            // ���ױߺ���
            VR.Left := Max(R.Left, V + LW);
            VR.Top := LH + FFixedRowHeight;
            VR.Right := Min(R.Right, W + LW);
            VR.Bottom := Min(R.Bottom, LH + FFixedRowHeight + DH);
            Canvas.FillRect(VR, 0, 0, [], LOpacity, FContentViews.FDividerBrush);
          end;
        end;

        V := W;
      end;

      H := H + FFixedRowHeight + DH;
    end;
  end;

  // ���̶���
  // ������
  LR := RectF(R.Left, H, R.Left + LW, H + FContentViews.FViewBottom);
  if Assigned(FFixedBrush.FDefault) then begin
    if LR.Bottom > MH then LR.Bottom := MH;
    Canvas.FillRect(LR, 0, 0, [], LOpacity, FFixedBrush.FDefault);
  end;

  // ������
  if FContentViews.FirstRowIndex >= 0 then begin
    if FColumns.ExistWordWarp then begin
      PH := @FItemsPoints[FContentViews.FirstRowIndex];
      LR.Bottom := LR.Top + FContentViews.FViewTop;
      for I := FContentViews.FirstRowIndex to FContentViews.FLastRowIndex do begin
        LR.Bottom := LR.Bottom + PH^ + DH;
        LR.Top := LR.Bottom - DH;

        DoDrawFixedRowText(Canvas, I,
          RectF(LR.Left, LR.Bottom - PH^ - DH, LR.Right, LR.Bottom - DH), LOpacity);
        if LR.Top < R.Bottom then
          Canvas.FillRect(LR, 0, 0, [], LOpacity, FContentViews.FDividerBrush);

        if LR.Top > MH then
          Break;

        Inc(PH);
      end;
    end else begin
      LR.Bottom := LR.Top + FContentViews.FViewTop;
      LH := FContentViews.FDefaultItemHeight;
      for I := FContentViews.FirstRowIndex to FContentViews.FLastRowIndex do begin
        LR.Bottom := LR.Bottom + LH + DH;
        LR.Top := LR.Bottom - DH;

        DoDrawFixedRowText(Canvas, I,
          RectF(LR.Left, LR.Bottom - LH - DH, LR.Right, LR.Bottom - DH), LOpacity);
        if LR.Top < R.Bottom then begin
          if LR.Bottom > R.Bottom then
            LR.Bottom := R.Bottom;
          Canvas.FillRect(LR, 0, 0, [], LOpacity, FContentViews.FDividerBrush);
        end;

        if LR.Top > MH then
          Break;
      end;
    end;
  end;

  // �����Ͻ�
  LR := RectF(R.Left, R.Top, R.Left + LW, H);
  if Assigned(FFixedBrush.FDefault) then
    Canvas.FillRect(LR, 0, 0, [], LOpacity, FFixedBrush.FDefault);

  LFixedCols := FFixedCols;
  if (gvIndicator in FOptions) or (gvRowIndex in FOptions) then
    Inc(LFixedCols);
  
  if FDownFixedRowIndex = -1 then begin
    V := LR.Left;
    for I := 0 to LFixedCols - 1 do begin
      if (I = 0) and (LFixedCols > FFixedCols) then
        LV := V + FixedIndicatorWidth + DH
      else begin
        if Assigned(FAdapter) then
          LV := V + FAdapter.GetFixedColData(I).Width + DH
        else
          LV := V + FFixedDefaultColWidth + DH;
      end;
        
      if I = FDownFixedColIndex then begin
        VR := RectF(V, LR.Top, LV - DH, LR.Bottom);
        DoDrawFixedColBackground(Canvas, VR, LOpacity);
        Break;
      end else
        V := LV;
    end;
  end;

  Canvas.FillRect(RectF(LR.Left, LR.Bottom - DH, LR.Right, LR.Bottom), 0, 0, [], LOpacity, FContentViews.FDividerBrush);

  // �� TextRowIndex
  if (gvRowIndex in FOptions) and (FTextRowIndex <> '') then
    DoDrawFixedRowIndexText(Canvas,
      RectF(LR.Left, LR.Top, LR.Left + FixedIndicatorWidth + DH, LR.Bottom - DH), LOpacity);

  // ����ֱ������
  if LDrawLine then begin
    LR := RectF(R.Left, R.Top, R.Left + LW, H + FContentViews.FViewBottom);
    if LR.Bottom > MH then LR.Bottom := MH;

    // ��������
    V := LR.Left;
    for I := 0 to LFixedCols - 1 do begin
      if (I = 0) and (LFixedCols > FFixedCols) then
        V := V + FixedIndicatorWidth + DH
      else begin
        if Assigned(FAdapter) then
          V := V + FAdapter.GetFixedColData(I).Width + DH
        else
          V := V + FFixedDefaultColWidth + DH;
      end;

      VR.Left := V - DH;
      VR.Top := LR.Top;
      VR.Right := V;
      VR.Bottom := Min(R.Bottom, LR.Bottom);
      Canvas.FillRect(VR, 0, 0, [], LOpacity, FContentViews.FDividerBrush);
    end;
  end;
end;

procedure TGridBase.DoEnter;
begin
  inherited;
  FContentViews.DoEnter;
end;

procedure TGridBase.DoFilterData(Item: TGridColumnItem);
begin
  if not Assigned(FFilterList) then
    FFilterList := TStringList.Create
  else
    FFilterList.Clear;

  FFilterList.Add('<���ɸѡ>');
  DoInitFilterDataList(Item, FFilterList);

  if FFilterList.Count > 0 then begin

    TDialogBuilder.Create(Self)
      .SetSingleChoiceItems(FFilterList, FFilterList.IndexOf(Item.FilterText),
        procedure (Dialog: IDialog; Which: Integer)
        begin
          if Which = 0 then
            Item.FilterText := ''
          else
            Item.FilterText := Dialog.Builder.Items[Which];
          DoFilterDataChange(Item);
          Dialog.AsyncDismiss;
        end
      )
      .SetOnInitListAdapterA(
        procedure (Dialog: IDialog; Builder: TDialogBuilder; var Adapter: IListAdapter)
        var
          LAdapter: TGridFilterDownListAdapter;
        begin
          LAdapter := TGridFilterDownListAdapter.Create(Builder.Items);
          LAdapter.DefaultItemHeight := FFixedRowHeight;
          LAdapter.FontSize := FText.Font.Size;
          LAdapter.ItemIndex := Builder.CheckedItem;
          LAdapter.WordWrap := False;
          Adapter := LAdapter;
        end
      )
      .SetWidth(Max(160, Item.Width))
      .SetMaxHeight(FFixedRowHeight * 12)
      .SetDownPopup(Self, FContentViews.Left + Item.X, (Item.RowIndex + 1) * FFixedRowHeight + GetDividerHeight,
        TLayoutGravity.LeftTop)
      .Show;
  end;
end;

procedure TGridBase.DoFilterDataChange(Item: TGridColumnItem);
begin
end;

procedure TGridBase.DoFixedBrushChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TGridBase.DoInitFilterDataList(Item: TGridColumnItem; List: TStrings);
begin
end;

procedure TGridBase.DoInVisibleChange;
begin
  inherited DoInVisibleChange;
  FContentViews.InVisible := InVisible;
end;

procedure TGridBase.DoItemIndexChange(Sender: TObject);
begin
  Invalidate;
  if Assigned(FOnItemIndexChange) then
    FOnItemIndexChange(Self);
end;

procedure TGridBase.DoRealign;
var
  LDisablePaint: Boolean;
  W, H, FW: Single;
begin
  if (csDestroying in ComponentState) then
    Exit;
  if FDisableAlign or IsUpdating then
    Exit;
  LDisablePaint := FDisablePaint;
  try
    FDisablePaint := True;

    FW := FixedWidth;
    W := Width - Padding.Right - Padding.Left;
    H := (FFixedRowHeight + GetDividerHeight) * FixedRows;

    FContentViews.SetBounds(Padding.Left + FW, Padding.Top + H, W - FW,
      Height - H - Padding.Bottom - Padding.Top);

    inherited DoRealign;

    if (HeightSize = TViewSize.WrapContent) and (Height > FContentViews.Height) then begin
      FDisableAlign := True;
      BeginUpdate;
      SetSize(Width, FContentViews.Height + Padding.Top + Padding.Bottom);
      EndUpdate;
      FDisableAlign := False;
    end;

  finally
    FDisablePaint := LDisablePaint;
    FContentViews.DoRealign;
  end;
end;

procedure TGridBase.DoScrollVisibleChange;
begin
  if FCanScrollV and FCanScrollH then begin
    FScrollV.Margins.Bottom := 0;
    FScrollH.Margins.Right := FScrollV.Width;           
  end else if FCanScrollV and Assigned(FScrollV) then 
    FScrollV.Margins.Bottom := 0
  else if FCanScrollH and Assigned(FScrollH) then
    FScrollH.Margins.Right := 0;
  inherited DoScrollVisibleChange;
end;

procedure TGridBase.DoTextChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TGridBase.DoUpdateAniCalculations(
  const AAniCalculations: TScrollCalculations);
begin
  inherited DoUpdateAniCalculations(AAniCalculations);
  AAniCalculations.TouchTracking := [ttVertical, ttHorizontal];
  {$IFDEF MSWINDOWS}
  AAniCalculations.BoundsAnimation := TScrollingBehaviour.BoundsAnimation in GetScrollingBehaviours;
  {$ENDIF}
end;

function TGridBase.GetCellBrush: TGridViewCellBrush;
begin
  Result := FContentViews.FCellBrush;
end;

function TGridBase.GetCellsData(const ACol, ARow: Integer): string;
var
  Item: TGridColumnItem;
begin
  if Assigned(FAdapter) then begin
    Item := FColumns.ItemCols[ACol];
    if Assigned(Item) and Assigned(Item.FOnGetCellText) then
      Item.FOnGetCellText(Item, ARow, Result)
    else
      Result := FAdapter.Cells[Item.ColIndex, ARow];
  end else
    Result := '';
end;

function TGridBase.GetColCount: Integer;
begin
  if Assigned(FAdapter) then
    Result := FAdapter.ColCount
  else
    Result := FColumns.ColsCount;
end;

function TGridBase.GetDefaultSize: TSizeF;
begin
  Result.Width := 200;
  Result.Height := 100;
end;

function TGridBase.GetDividerHeight: Single;
begin
  if FLocalDividerHeight = -1 then
    FLocalDividerHeight := InnerCalcDividerHeight;
  Result := FLocalDividerHeight;
end;

function TGridBase.GetFixedIndicatorWidth: Single;
var
  LCount: Integer;
begin
  if FFixedIndicatorWidthChange and Assigned(Scene) then begin
    FFixedIndicatorWidthChange := False;

    if gvIndicator in FOptions then
      FLastFixedIndicatorWidth := CDefaultAnchorWidth
    else
      FLastFixedIndicatorWidth := 0;

    if gvRowIndex in FOptions then begin
      LCount := RowCount;
      if LCount < 1 then LCount := CDefaultEmptyRows;
      FLastFixedIndicatorWidth := FLastFixedIndicatorWidth +
        FFixedText.CalcTextWidth(IntToStr(LCount), Scene.GetSceneScale) + CDefaultFixedColPadding * 2;

      if FTextRowIndex <> '' then begin
        FLastFixedIndicatorWidth := Max(FLastFixedIndicatorWidth,
          FFixedText.CalcTextWidth(FTextRowIndex, Scene.GetSceneScale) + CDefaultFixedColPadding * 2)
      end;
    end else
      FLastFixedIndicatorWidth := FLastFixedIndicatorWidth + CDefaultFixedColPadding * 2;

    FLastFixedIndicatorWidth := Max(8, FLastFixedIndicatorWidth);
  end;
  Result := FLastFixedIndicatorWidth;
end;

function TGridBase.GetFixedRows: Integer;
begin
  Result := FColumns.RowsCount;
end;

function TGridBase.GetFixedWidth: Single;
var
  I: Integer;
  DividerH: Single;
begin
  DividerH := GetDividerHeight;
  if Assigned(FAdapter) then begin
    Result := 0;
    for I := 0 to FFixedCols - 1 do
      Result := Result + DividerH + FAdapter.GetFixedColData(I).Width;
  end else
    Result := (DividerH + FFixedDefaultColWidth) * FFixedCols;
  if (gvIndicator in FOptions) or (gvRowIndex in FOptions) then
    Result := Result + FixedIndicatorWidth + DividerH;
end;

function TGridBase.GetItemIndex: Integer;
begin
  if Assigned(FAdapter) then
    Result := FAdapter.ItemIndex
  else
    Result := -1;
end;

class function TGridBase.GetKey(const ACol, ARow: Integer): Int64;
begin
  TGridCell(Result).Row := ARow;
  TGridCell(Result).Col := ACol;
end;

function TGridBase.GetRealDrawState: TViewState;
begin
  Result := TViewState.None;
end;

function TGridBase.GetRowCount: Integer;
begin
  Result := FCount;
end;

function TGridBase.GetRowHeight(const ARow: Integer): Single;
begin
  Result := FContentViews.GetRowHeight(ARow);
end;

function TGridBase.GetRowHeightPro: Single;
begin
  if Assigned(FAdapter) then
    Result := FAdapter.ItemDefaultHeight
  else begin
    if FDefaultRowHeight < 0 then
      Result := CDefaultFixedRowHeight
    else
      Result := FDefaultRowHeight;
  end;
end;

function TGridBase.GetSelectIndex: Integer;
begin
  Result := FContentViews.FSelectCell.Row;
end;

procedure TGridBase.HideEditor;
begin
  FContentViews.FEditor.Visible := False;
end;

procedure TGridBase.HScrollChange(Sender: TObject);
begin
  if FScrolling then Exit;
  inherited HScrollChange(Sender);
  if Assigned(FContentViews) then
    FContentViews.Realign;
end;

function TGridBase.InnerCalcDividerHeight: Single;
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

procedure TGridBase.InvalidateContentSize;
var
  ItemDefaultH: Double;
  I, LCount: Integer;
  W, H, DividerH: Double;
begin
  if not Assigned(Scene) then
    Exit;
  // �����б���߶������С
  LCount := RowCount;
  if LCount < 1 then LCount := CDefaultEmptyRows;

  // Ĭ���и�
  ItemDefaultH := RowHeight;

  // �ָ��߸߶�
  DividerH := GetDividerHeight;

  // ��¼�̶��еĿ���
  FFixedColsWidth := FixedWidth;
  // ��ȥ�̶��еĿ���
  W := FColumns.Width;

  // �����Զ��߶���ʱ��Ҫ����ÿ�еĸ߶�
  if FColumns.ExistWordWarp then begin
    I := LCount;
    if I mod 1024 <> 0 then
      I := I div 1024 * 1024 + 1024;
    if I <> Length(FItemsPoints) then
      SetLength(FItemsPoints, I);

    FContentBounds^ := TRectD.Empty;
    if Length(FItemsPoints) = 0 then
      Exit;


    // ������߶�
    H := 0;
    for I := 0 to LCount - 1 do begin
      if FItemsPoints[i] = 0 then
        H := H + DividerH + ItemDefaultH
      else
        H := H + DividerH + FItemsPoints[i];
    end;

  end else begin
    // �������Զ��߶���ʱ��ֱ������ܸ߶�
    H := (ItemDefaultH + DividerH) * LCount;
  end;

  if Assigned(FScrollV) then
    FContentBounds.Right := W + FFixedColsWidth + FScrollV.Width + CDefaultFixedColWidth
  else
    FContentBounds.Right := W + FFixedColsWidth + CDefaultFixedColWidth;

  if Assigned(FScrollH) then
    FContentBounds.Bottom := H + FFixedRowHeight + FScrollH.Height + CDefaultFixedRowHeight * 2
  else
    FContentBounds.Bottom := H + FFixedRowHeight + CDefaultFixedRowHeight * 2;
end;

function TGridBase.IsEmpty: Boolean;
begin
  Result := GetRowCount = 0;
end;

function TGridBase.IsStoredDividerHeight: Boolean;
begin
  Result := FDividerHeight <> -1;
end;

function TGridBase.IsStoredRowHeight: Boolean;
begin
  Result := FDefaultRowHeight > 0;
end;

function TGridBase.IsStoredScrollSmallChangeFraction: Boolean;
begin
  Result := FScrollSmallChangeFraction <> CDefaultFixedRowHeight;
end;

procedure TGridBase.Loaded;
begin
  inherited Loaded;
  NotifyDataChanged;
end;

procedure TGridBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  I, J: Integer;
  H, LH, DH: Double;
  LFixedCols: Integer;
begin
  inherited;
  FDownPos.X := X;
  FDownPos.Y := Y;
  FDownFixedRowIndex := -1;
  FDownFixedColIndex := -1;

  // ����Ƿ���������ߵĹ̶���Ԫ��
  if (FHotItem = nil) and (FAdjuestItem = nil) and (X < FFixedColsWidth) then begin
    LFixedCols := FFixedCols;
    if (gvIndicator in FOptions) or (gvRowIndex in FOptions) then
      Inc(LFixedCols);
      
    DH := GetDividerHeight;
    if (Y > FColumns.Height) then begin

      H := FContentViews.FViewTop;
      Y := Y - FColumns.Height - Padding.Top;


      for I := FContentViews.FFirstRowIndex to FContentViews.FLastRowIndex do begin
        LH := H + GetRowHeight(I);
        if (Y > H) and (Y < LH) then begin
          H := Padding.Left;
          for J := 0 to LFixedCols - 1 do begin
            if (J = 0) and (LFixedCols > FFixedCols) then
              LH := H + FixedIndicatorWidth
            else begin
              if Assigned(FAdapter) then begin
                LH := H + Adapter.GetFixedColData(J).Width;
              end else
                LH := H + FFixedDefaultColWidth;
            end;
            if (X > H) and (X < LH) then begin
              FDownFixedRowIndex := I;
              FDownFixedColIndex := J;
              Break;
            end else
              H := LH + DH;
          end;
          Break;
        end else
          H := LH + DH;
      end;

    end else begin

      H := Padding.Left;
      for J := 0 to LFixedCols - 1 do begin
        if (J = 0) and (LFixedCols > FFixedCols) then
          LH := H + FixedIndicatorWidth
        else begin
          if Assigned(FAdapter) then begin
            LH := H + FAdapter.GetFixedColData(J).Width;
          end else
            LH := H + FFixedDefaultColWidth;
        end;
        if (X > H) and (X < LH) then begin
          FDownFixedRowIndex := -1;
          FDownFixedColIndex := J;
          Break;
        end else
          H := LH + DH;
      end;

    end;

  end;
end;

procedure TGridBase.MouseMove(Shift: TShiftState; X, Y: Single);
var
  I: Integer;
  W: Single;
  R: TRectF;
begin
  FMovePos.X := X;
  FMovePos.Y := Y;
  if not (csDesigning in ComponentState) then begin
    if (ssLeft in Shift) then begin

      if Assigned(FAdjuestItem) then begin
        // �ı��д�С
        W := FAdjuestItem.Width + (X - FDownPos.X);
        if W < 1 then Exit;
        FAdjuestItem.Width := W;
        FDownPos := PointF(X, Y);
        FContentViews.InitColumnList;
        RealignContent;
        Invalidate;
      end else if Assigned(FHotItem) and (FColumns.RowsCount = 1) and (gvColumnMove in FOptions) and Assigned(FDragView) and (not FHotItem.IsLeftTop) then begin
        // �϶���, ֻ������Ϊ1��ʱ���϶�
        if FDragView.Visible or (Abs(X - FDownPos.X) > 3) then begin
          if not FDragView.Visible then begin
            FDragView.SetBounds(FHotItemRange.Left, FHotItemRange.Top, FHotItemRange.Width, FHotItemRange.Height);
            FDragView.TextSettings.Font.Assign(FFixedText.Font);
            FDragView.TextSettings.Color.Default := FFixedText.Color;
            FDragView.TextSettings.Gravity := FFixedText.Gravity;
            FDragView.Padding.Rect := FHotItem.Padding;
            FDragView.Text := FHotItem.Title;
            FDragView.Visible := True;
          end;
          Cursor := crDrag;
          FDragView.Position.X := FDragView.Position.X + (X - FDownPos.X);
          FDownPos := PointF(X, Y);
        end;
      end;
      Exit;

    end else if (Y < FContentViews.Top) and (X > FContentViews.Left) and (Shift = []) then begin
      I := PointInItem(PointF(X - 1, Y), R);
      if I >= 0 then begin
        FHotHeaderIndex := I;
        FHotItem := FColumns[FFixedHeaderRange[I].Col, FFixedHeaderRange[I].Row];
        FHotItemRange := R;
        if (not FHotItem.IsLeftTop) and (gvColumnResize in FOptions) and (X >= R.Right - GetDividerHeight - 3) then begin
          Cursor := crHSplit;
          FAdjuestItem := FHotItem;
        end else begin
          Cursor := crDefault;
          FAdjuestItem := nil;
        end;
        Exit;
      end;
    end;
    Cursor := crDefault;
    FHotItem := nil;
    FAdjuestItem := nil;
  end;
  inherited;
end;

procedure TGridBase.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  I: Integer;
  R: TRectF;
begin
  if Assigned(FDragView) and FDragView.Visible and Assigned(FHotItem) then begin
    FDragView.Visible := False;
    I := PointInItem(PointF(X, Y), R);
    if I >= 0 then begin
      if (FFixedHeaderRange[I].Col < 0) then
        I := 0;
      if (I <> FHotHeaderIndex) then begin
        if Assigned(FOnColumnMovedEvent) then
          FOnColumnMovedEvent(Self, FHotItem.Index, I);
        FContentViews.DoEditCancel;
        FContentViews.FSelectCell.Clear;
        FHotItem.Index := I;
      end;
    end;
  end;

  inherited MouseUp(Button, Shift, X, Y);

  FDownFixedRowIndex := -1;
  FDownFixedColIndex := -1;
  FAdjuestItem := nil;
  FHotItem := nil;
  Cursor := crDefault;
end;

procedure TGridBase.NotifyDataChanged;
begin
  //MessageBox(0, PChar(ComponentStateToString(ComponentState)), 'NotifyDataChanged', 0);
  if (csLoading in ComponentState) or (csDestroying in ComponentState) or (FContentViews = nil) or FContentViews.FDisableAlign then
    Exit;
  FContentViews.FDisableAlign := True;
  try
    if Assigned(FAdapter) then
      FCount := FAdapter.RowCount
    else
      FCount := 0;

    if FSelectionAnchor < 0 then
      FSelectionAnchor := 0;
    if (FSelectionAnchor > FCount) and (FCount > 0) then
      FSelectionAnchor := FCount -1;

    FContentViews.DoEditCancel;

    FContentViews.FLastScrollValue := -1;
    FContentViews.FLastH := 0;
    FContentViews.FLastW := 0;
    FContentViews.FCount := FCount;

    FContentViews.InitColumnList;

    FMaxListItemBottom := 0;
    InvalidateContentSize;

    // �ָ�λ��
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
    FFixedIndicatorWidthChange := True;
  end;

  HandleSizeChanged;
  Invalidate;
end;

procedure TGridBase.PaintBackground;
var
  R: TRectF;
  LW, LH: Single;
begin
  if (csReading in ComponentState) then
    Exit;
  R := RectF(0, 0, Width, Height);
  if Assigned(FOnDrawViewBackgroud) then
    DoDrawBackground(R)
  else
    inherited PaintBackground;
  if (AbsoluteInVisible = False) and Assigned(FColumns) then begin
    LW := FDrawBuffer.Width;
    LH := FDrawBuffer.Height;
    if (LW > 0) and (LH > 0) then begin    
      FDrawBuffer.Canvas.BeginScene();
      try
        FDrawBuffer.Clear(0);
        R.Right := Min(R.Right, R.Left + LW);
        R.Bottom := Min(R.Bottom, R.Top + LH);
        DoDrawHeaderRows(FDrawBuffer.Canvas, R);
      finally
        FDrawBuffer.Canvas.EndScene;
        Canvas.DrawBitmap(FDrawBuffer, R, R, 1);
      end;
    end else begin
      if (Width > 0) and (Height > 0) then        
        HandleSizeChanged;
    end;
  end;
end;

function TGridBase.PointAsCell(const X, Y: Single): TGridCell;
begin
  Result := FContentViews.PointAsCell(X, Y);
end;

function TGridBase.PointInItem(const P: TPointF; var R: TRectF): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FFixedHeaderRange.Count - 1 do begin
    if IsPointInRect(P, FFixedHeaderRange[I].R) then begin
      Result := I;
      R := FFixedHeaderRange[I].R;
      Exit;
    end;
  end;
end;

procedure TGridBase.RealignContent;
begin
  if Assigned(FContentViews) then
    FContentViews.DoHideEditor;
  FFixedIndicatorWidthChange := True;
  if FScrollbar = TViewScroll.None then begin
    FCanScrollV := False;
    FCanScrollH := False;
  end else begin
    {$IFDEF NEXTGEN}
    FCanScrollV := True;
    FCanScrollH := True;
    {$ELSE}
    FCanScrollH := DragScroll or (FContentBounds.Width > ViewRect.Width);
    FCanScrollV := DragScroll or (FContentBounds.Height > ViewRect.Height);
    {$ENDIF}
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

procedure TGridBase.Resize;
begin
  if FResizeing or
    (csLoading in ComponentState) or
    (csDestroying in ComponentState) then
    Exit;
  FResizeing := True;

  // ����б��߶�Ϊ�Զ���Сʱ������һ�¸�����ͼ�����߶ȣ����Զ�������Сʱ��ʹ��
  if HeightSize = TViewSize.WrapContent then
    FContentViews.FMaxParentHeight := GetParentMaxHeight
  else
    FContentViews.FMaxParentHeight := 0;

  inherited Resize;

  if Assigned(FDrawBuffer) and (Assigned(Canvas)) then
    FDrawBuffer.SetSize(Canvas.Width, Canvas.Height);

  UpdateScrollBar(FScrollV, TViewScroll.Vertical);
  UpdateScrollBar(FScrollH, TViewScroll.Horizontal);
  
  FContentViews.DoRealign;
  FContentViews.Invalidate;
  FLastHeight := Height;
  FLastWidth := Width;

  FResizeing := False;
end;

procedure TGridBase.ScrollToCell(const ACell: TGridCell);
begin
  FContentViews.ScrollToCell(ACell);
end;

procedure TGridBase.ScrollToSelectedCell;
begin
  FContentViews.ScrollToSelectedCell;
end;

procedure TGridBase.SetAdapter(const Value: IGridAdapter);
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

    if FAdapter is TGridAdapterBase then begin
      (FAdapter as TGridAdapterBase).FGridView := Self;
      if FDefaultRowHeight > 0 then
        (FAdapter as TGridAdapterBase).FDefaultRowHeight := FDefaultRowHeight;
    end;

    NotifyDataChanged;
    HandleSizeChanged;
  end;
end;

procedure TGridBase.SetCellBrush(const Value: TGridViewCellBrush);
begin
  FContentViews.FCellBrush.Assign(Value);
end;

procedure TGridBase.SetCellsData(const ACol, ARow: Integer;
  const Value: string);
begin
  if Assigned(FAdapter) then
    FAdapter.Cells[ACol, ARow] := Value;
end;

procedure TGridBase.SetDivider(const Value: TAlphaColor);
begin
  if FDivider <> Value then begin
    FDivider := Value;
    Invalidate;
  end;
end;

procedure TGridBase.SetDividerHeight(const Value: Single);
begin
  if FDividerHeight <> Value then begin
    FDividerHeight := Value;
    if not (csLoading in ComponentState) then begin
      FLocalDividerHeight := FDividerHeight;
      HandleSizeChanged;
      RealignContent;
      Invalidate;
    end;
  end;
end;

procedure TGridBase.SetFixedBrush(const Value: TGridViewBrush);
begin
  if Assigned(Value) then
    FFixedBrush.Assign(Value);
end;

procedure TGridBase.SetFixedCols(const Value: Integer);
begin
  if FFixedCols <> Value then begin
    FFixedCols := Value;
    DoColumnsChange(Self);
  end;
end;

procedure TGridBase.SetFixedDefaultColWidth(const Value: Single);
begin
  if FFixedDefaultColWidth <> Value then begin
    FFixedDefaultColWidth := Value;
    if not (csLoading in ComponentState) then begin
      HandleSizeChanged;
      RealignContent;
      Invalidate;
    end;
  end;
end;

procedure TGridBase.SetFixedDivider(const Value: TAlphaColor);
begin
  if FFixedDivider <> Value then begin
    FFixedDivider := Value;
    Invalidate;
  end;
end;

procedure TGridBase.SetFixedRowHeight(const Value: Single);
begin
  if FFixedRowHeight <> Value then begin
    FFixedRowHeight := Value;
    FContentViews.FLastH := -1;
    DoColumnsChange(Self);
  end;
end;

procedure TGridBase.SetFixedRows(const Value: Integer);
begin
  if FColumns.RowsCount <> Value then
    FColumns.RowsCount := Value;
end;

procedure TGridBase.SetFixedSetting(const Value: TGridFixedSetting);
begin
  FFixedSetting.Assign(Value);
end;

procedure TGridBase.SetFixedText(const Value: TGridTextSettings);
begin
  if FFixedText <> Value then
    FFixedText.Assign(Value);
end;

procedure TGridBase.SetItemIndex(const Value: Integer);
begin
  if Assigned(FAdapter) and (FAdapter.ItemIndex <> Value) then begin
    FAdapter.ItemIndex := Value;
    DoItemIndexChange(Self);
  end;
end;

procedure TGridBase.SetOptions(const Value: TGridOptions);
begin
  if FOptions = Value then Exit;
  FOptions := Value;
  FFixedIndicatorWidthChange := True;
  FContentViews.DoHideEditor;
  HandleSizeChanged;
  RealignContent;
  Invalidate;
end;

procedure TGridBase.SetRowHeightPro(const Value: Single);
begin
  if Value <> RowHeight then begin
    if (Value <> CDefaultFixedRowHeight) and (Value > 0) then
      FDefaultRowHeight := Value;
    if Assigned(FAdapter) then
      FAdapter.SetItemDefaultHeight(Value);
    FContentViews.DoHideEditor;
    FContentViews.FLastH := -1;
    HandleSizeChanged;
    RealignContent;
    Invalidate;
  end;
end;

procedure TGridBase.SetScrollbar(const Value: TViewScroll);
begin
  if FScrollbar <> Value then begin  
    if (FScrollbar = TViewScroll.None) or (Value = TViewScroll.None) then
      inherited
    else
      FScrollbar := Value;
  end;
end;

procedure TGridBase.SetSelectIndex(const Value: Integer);
begin
  if Value < FCount then begin
    if FContentViews.FSelectCell.Row <> Value then begin
      FContentViews.FSelectCell.Row := Value;
      FContentViews.Invalidate;
    end;
  end;
end;

procedure TGridBase.SetSelectionAnchor(const Value: Integer);
begin
  if FSelectionAnchor <> Value then begin
    FSelectionAnchor := Value;
    if Assigned(FContentViews) and (FContentViews.FSelectCell.Row <> Value) then begin
      FContentViews.DoEditCancel;
      FContentViews.FSelectCell.Clear;
    end;
    Invalidate;
    if Assigned(FOnRowSelChangeEvent) then
      FOnRowSelChangeEvent(Self);
  end;
end;

procedure TGridBase.SetText(const Value: TGridTextSettings);
begin
  FText.Assign(Value);
end;

procedure TGridBase.SetTextRowIndex(const Value: string);
begin
  if FTextRowIndex <> Value then begin
    FTextRowIndex := Value;
    FFixedIndicatorWidthChange := True;
    Invalidate;
  end;
end;

procedure TGridBase.ShowEditor;
var
  R: TRectF;
begin
  R := FContentViews.SelectCellRect;
  FContentViews.FEditor.SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  FContentViews.FEditor.Text := '';
  FContentViews.FEditor.Visible := True;
  if FContentViews.FEditor.CanFocus then
    FContentViews.FEditor.SetFocus;
end;

procedure TGridBase.UpdateScrollBar(AScroll: TScrollBar; AScrollBar: TViewScroll; const ValueOffset: Double);
var
  LViewportPosition: TPointD;
  V: Double;
  R: TRectF;
begin
  if not Assigned(AScroll) then Exit;
  LViewportPosition := ViewportPosition;
  R := ViewRect;
  R.Top := FContentViews.Top;
  R.Left := FContentViews.Left;
  
  if AScrollBar = TViewScroll.Vertical then begin
    V := R.Height + FContentViews.Top;
    {$IFDEF NEXTGEN}
    FCanScrollV := True; // �ƶ�ƽ̨ʼ���ܹ���
    {$ELSE}
    FCanScrollV := FContentBounds.Height > V;
    {$ENDIF}
    LViewportPosition.Y := LViewportPosition.Y + ValueOffset;
    if (LViewportPosition.Y > FContentBounds.Height - AScroll.ViewportSizeD) and
      (LViewportPosition.Y > FAniCalculations.MaxTarget.Point.Y) then
      LViewportPosition.Y := FAniCalculations.MaxTarget.Point.Y;
    UpdateVScrollBar(LViewportPosition.Y, V);
    if (AScroll.Visible <> FCanScrollV) then begin
      AScroll.Visible := FCanScrollV and FShowScrollBars and (not FInVisible);
      DoScrollVisibleChange;
    end;
  end else if AScrollBar = TViewScroll.Horizontal then begin
    V := R.Width + FContentViews.Left;
    {$IFDEF NEXTGEN}
    FCanScrollH := True; // �ƶ�ƽ̨ʼ���ܹ���
    {$ELSE}
    FCanScrollH := FContentBounds.Width > V;
    {$ENDIF}
    LViewportPosition.X := LViewportPosition.X + ValueOffset;
    if (LViewportPosition.X > FContentBounds.Width - AScroll.ViewportSizeD) and
      (LViewportPosition.X > FAniCalculations.MaxTarget.Point.X) then
      LViewportPosition.X := FAniCalculations.MaxTarget.Point.X;
    UpdateHScrollBar(LViewportPosition.X, V);
    if (AScroll.Visible <> FCanScrollH) then begin
      AScroll.Visible := FCanScrollH and FShowScrollBars and (not FInVisible);
      DoScrollVisibleChange;
    end;
  end else begin
    FCanScrollV := False;
    FCanScrollH := False;
    AScroll.Visible := False;
  end;
end;

procedure TGridBase.VScrollChange(Sender: TObject);
begin
  if FScrolling then Exit;
  inherited VScrollChange(Sender);
  if Assigned(FContentViews) then
    FContentViews.Realign;
end;

{ TGridViewContent }

function TGridViewContent.CellRect(const ACell: TGridCell): TRectF;
var
  I: Integer;
  V, DH: Double;
  Item: TGridColumnItem;
begin
  if (ACell.Row < 0) or (ACell.Col < 0) or (ACell.Row < FFirstRowIndex) or (ACell.Row > FLastRowIndex) then begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
  end else begin
    DH := GridView.GetDividerHeight;

    V := FViewTop;
    for I := FFirstRowIndex to FLastRowIndex do begin
      if I = ACell.Row then begin
        Result.Top := V;
        Result.Bottom := V + RowHeight[I];
        Break;
      end;
      V := V + RowHeight[I] + DH;
    end;

    {
    V := 0- GridView.HScrollBarValue;
    for I := 0 to GridView.ColCount - 1 do begin
      Item := GridView.Columns[I, 0];
      if not Item.Visible then
        Continue;
      if I = ACell.Col then begin
        Result.Left := V;
        Result.Right := V + Item.Width;
        Break;
      end;
      V := V + Item.Width + DH;
    end;
    }
    V := 0- GridView.HScrollBarValue;
    Item := FBaseColumnsList[ACell.Col];
    Result.Left := Item.X + V;
    Result.Right := Result.Left + Item.Width;
  end;
end;

procedure TGridViewContent.Click;
begin
  inherited;
  if (FSelectCell.Row >= 0) and (FSelectCell.Col >= 0) then
    DoClickCell(FSelectCell);
end;

constructor TGridViewContent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsDesigning := csDesigning in ComponentState;

  FDividerBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Null);
  {$IFDEF MSWINDOWS}
  FScrollRB := TBrush.Create(TBrushKind.Solid, TAlphaColor(TAlphaColorRec.Alpha or TAlphaColor(GetSysColor(COLOR_BTNFACE))));
  {$ELSE}
  FScrollRB := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.White);
  {$ENDIF}

  FSelectCell.Row := -1;
  FSelectCell.Col := -1;

  FColumnsList := TList<TGridColumnItem>.Create;
  FBaseColumnsList := TList<TGridColumnItem>.Create;

  FTempCellBrush := TBrush.Create(TBrushKind.Solid, 0);

  FCellBrush := TGridViewCellBrush.Create(Self);
  with FCellBrush.ItemTwoColor do begin
    Kind := TViewBrushKind.Solid;
    Color := $FFFDFEFF;
    DefaultKind := TBrushKind.Solid;
    DefaultColor := Color;
  end;
  with FCellBrush.ItemSelected do begin
    Kind := TViewBrushKind.Solid;
    Color := $FF99ccff;
    DefaultKind := TBrushKind.Solid;
    DefaultColor := Color;
  end;
  with FCellBrush.ItemPressed do begin
    Kind := TViewBrushKind.Solid;
    Color := $FF3399ff;
    DefaultKind := TBrushKind.Solid;
    DefaultColor := Color;
  end;
  FCellBrush.OnChanged := DoCellBrushChange;

  FFirstRowIndex := -1;
  FLastRowIndex := -1;
  FViewBottom := 0;
  FViewItemBottom := 0;
  FViewTop := 0;

  FCount := 0;
  FMaxParentHeight := 0;

  CanFocus := True;
  TabStop := False;
end;

procedure TGridViewContent.DblClick;
begin
  inherited;
  if IsPointInRect(FDownPos, FRBRect) then
    Exit;
  Inc(FSelectClickRef);
  DoShowEditor;
end;

destructor TGridViewContent.Destroy;
begin
  FreeAndNil(FDividerBrush);
  FreeAndNil(FCellBrush);
  FreeAndNil(FScrollRB);
  FreeAndNil(FColumnsList);
  FreeAndNil(FBaseColumnsList);
  FreeAndNil(FTempCellBrush);
  inherited;
end;

procedure TGridViewContent.DoCellBrushChange(Sender: TObject);
begin
  {$IFNDEF MSWINDOWS}
  FScrollRB.Color := TAlphaColorRec.Alpha or FCellBrush.FDefault.Color;
  {$ENDIF}
  Invalidate;
end;

procedure TGridViewContent.DoClickCell(const ACell: TGridCell);
var
  Item: TGridColumnItem;
  CellSetting: TGridCellSettings;
  V, LV: Boolean;
begin
  if (ACell.Col < 0) then
    Exit;
  if IsPointInRect(FDownPos, FRBRect) then
    Exit;

  Item := FColumnsList[ACell.Col];
  if Assigned(FAdapter) then
    FAdapter.GetCellSettings(ACell.Col, ACell.Row, CellSetting)
  else
    CellSetting := nil;

  if Assigned(CellSetting) and (not CellSetting.Enabled) then
    Exit;

  if Assigned(Item) and Assigned(FAdapter) then begin

    if not Item.Enabled then
      Exit;

    case Item.DataType of
      CheckBox:
        begin
          LV := FAdapter.CellChecked[ACell.Col, ACell.Row];
          V := not LV;
          if Assigned(GridView.FOnCellCheckEvent) then
            GridView.FOnCellCheckEvent(GridView, ACell, V);
          if LV <> V then begin
            FAdapter.CellChecked[ACell.Col, ACell.Row] := V;
            Invalidate;
          end;
        end;
      RadioButton:
        begin
          if FAdapter.ItemIndex <> ACell.Row then begin
            FAdapter.ItemIndex := ACell.Row;
            GridView.DoItemIndexChange(Self);
          end;
        end;
      Image: ;
      ProgressBar: ;
      CustomDraw: ;
    end;
  end;

  if Assigned(GridView.FOnCellClickEvent) then
    GridView.FOnCellClickEvent(GridView, ACell);

  DoShowEditor;
  Inc(FSelectClickRef);
end;

procedure TGridViewContent.DoDrawCell(Canvas: TCanvas; const R: TRectF;
  const ARow, ACol: Integer; const LOpacity: Single);
const
  CSelectIconSize = 15;

  function GetIconDrawRect(Item: TGridColumnItem; const R: TRectF; const W, H: Single): TRectF;
  begin
    case Item.Gravity of
      TLayoutGravity.None, TLayoutGravity.LeftTop:
        begin
          Result.Left := R.Left + Item.Padding.Left;
          Result.Top := R.Top + Item.Padding.Top;
        end;
      TLayoutGravity.LeftBottom:
        begin
          Result.Left := R.Left + Item.Padding.Left;
          Result.Top := R.Bottom - Item.Padding.Bottom - H;
        end;
      TLayoutGravity.RightTop:
        begin
          Result.Left := R.Right - Item.Padding.Right - W;
          Result.Top := R.Top + Item.Padding.Top;
        end;
      TLayoutGravity.RightBottom:
        begin
          Result.Left := R.Right - Item.Padding.Right - W;
          Result.Top := R.Bottom - Item.Padding.Bottom - H;
        end;
      TLayoutGravity.CenterVertical:
        begin
          Result.Left := R.Left + Item.Padding.Left;
          Result.Top := R.Top + ((R.Bottom - Item.Padding.Bottom - R.Top + Item.Padding.Top) - H) * 0.5;
        end;
      TLayoutGravity.CenterHorizontal:
        begin
          Result.Left := R.Left + ((R.Right - Item.Padding.Right - R.Left + Item.Padding.Left) - W) * 0.5;
          Result.Top := R.Top + Item.Padding.Top;
        end;
      TLayoutGravity.CenterHBottom:
        begin
          Result.Left := R.Left + ((R.Right - Item.Padding.Right - R.Left + Item.Padding.Left) - W) * 0.5;
          Result.Top := R.Bottom - Item.Padding.Bottom - H;
        end;
      TLayoutGravity.CenterVRight:
        begin
          Result.Left := R.Right - Item.Padding.Right - W;
          Result.Top := R.Top + ((R.Bottom - Item.Padding.Bottom - R.Top + Item.Padding.Top) - H) * 0.5;
        end;
      TLayoutGravity.Center:
        begin
          Result.Left := R.Left + ((R.Right - Item.Padding.Right - R.Left + Item.Padding.Left) - W) * 0.5;
          Result.Top := R.Top + ((R.Bottom - Item.Padding.Bottom - R.Top + Item.Padding.Top) - H) * 0.5;
        end;
    end;
    Result.Right := Result.Left + W;
    Result.Bottom := Result.Top + H;
  end;

var
  DrawState: TViewState;
  B: Boolean;
  Item: TGridColumnItem;
  CellSet: TGridCellSettings;
  LGravity: TLayoutGravity;
  IsCellSet: Boolean;
  LExistAdapter: Boolean;
  LText: string;
begin
  LExistAdapter := Assigned(FAdapter);
  
  if (ARow = FSelectCell.Row) then begin
    if (ACol = FSelectCell.Col) then begin
      if (gvRowSelect in GridView.FOptions) and not (gvEditing in GridView.FOptions) then
        DrawState := TViewState.None  // ��ѡ���Ҳ������༭ʱ����ͻ����ʾ��ǰ��Ԫ��
      else
        DrawState := TViewState.Pressed;
    end else if gvRowSelect in GridView.FOptions then
      DrawState := TViewState.Selected
    else
      DrawState := TViewState.None;
  end else
    DrawState := TViewState.None;

  if ACol < 0 then Exit;  

  B := True;

  Item := FColumnsList[ACol];
  if not Assigned(Item) then
    Exit;

  if not Item.Enabled then
    DrawState := TViewState.Enabled;

  // �õ�Ҫ��ʾ���ı�
  if (Item.DataType = PlanText) and LExistAdapter then begin
    if Assigned(Item.FOnGetCellText) then begin
      LText := '';
      Item.FOnGetCellText(Item, ARow, LText);
    end else
      LText := FAdapter.GetCells(Item.ColIndex, ARow);
  end;

  if Assigned(GridView.FOnDrawCells) then
    GridView.FOnDrawCells(GridView, Canvas, ACol, ARow, R, DrawState, Item, B);

  // Ĭ�ϻ���
  if B then begin

    // ��ȡ��Ԫ���Զ������ѡ��
    if LExistAdapter then begin
      IsCellSet := FAdapter.GetCellSettings(ACol, ARow, CellSet);
      if IsCellSet and (not CellSet.Enabled) then
        DrawState := TViewState.Enabled;
    end else
      IsCellSet := False;

    // ����
    if Assigned(FCellBrush.FPressed) and (DrawState = TViewState.Pressed) and IsFocused then begin
      Canvas.FillRect(R, 0, 0, [], LOpacity, FCellBrush.FPressed);
    end else if IsCellSet and (CellSet.BgColor and $FF000000 <> 0) then begin
      FTempCellBrush.Color := CellSet.BgColor;
      Canvas.FillRect(R, 0, 0, [], LOpacity, FTempCellBrush);
    end;

    case Item.DataType of
      PlanText:  // �����ı�
        begin
          if LExistAdapter then begin
            if LText <> '' then begin
              LText := StringReplace(LText, #13#10, ' ', [rfReplaceAll]);

              if IsCellSet then begin
                if DrawState <> TViewState.Enabled then
                  DrawState := TViewState.Custom;

                GridView.FText.FCustomColor := CellSet.TextColor;

                if CellSet.TextStyle <> [] then begin
                  B := True;
                  LOnChange := GridView.FText.Font.OnChanged;
                  GridView.FText.Font.Style := CellSet.TextStyle;
                end else
                  B := False;

                if CellSet.Gravity = TLayoutGravity.None then
                  LGravity := Item.Gravity
                else
                  LGravity := CellSet.Gravity;

                GridView.FText.Draw(Canvas, LText,
                  RectF(R.Left + Item.Padding.Left, R.Top + Item.Padding.Top, R.Right - Item.Padding.Right,
                    R.Bottom - Item.Padding.Bottom), LOpacity, DrawState, LGravity
                );

                if B then begin
                  GridView.FText.Font.Style := GridView.FText.Font.Style - [TFontStyle.fsBold];
                  GridView.FText.Font.OnChanged := LOnChange;
                end;

              end else if LText <> '' then begin
                GridView.FText.Draw(Canvas, LText,
                  RectF(R.Left + Item.Padding.Left, R.Top + Item.Padding.Top, R.Right - Item.Padding.Right,
                    R.Bottom - Item.Padding.Bottom), LOpacity, DrawState, Item.Gravity
                );
              end;
            end;

          end;
        end;
      CheckBox:  // ����ѡ��
        begin
          if LExistAdapter then
            B := FAdapter.Cells[Item.ColIndex, ARow] = '1'  // ��Ԫ������Ϊ1ʱ����Ϊ��ѡ��״̬
          else
            B := False;

          if B then
            FGridRes.Drawable.ImageIndex := 2
          else
            FGridRes.Drawable.ImageIndex := 1;

          if DrawState = TViewState.Enabled then
            FGridRes.Drawable.ImageIndex := FGridRes.Drawable.ImageIndex + 4;
          
          FGridRes.Drawable.Draw(Canvas, GetIconDrawRect(Item, R, CSelectIconSize, CSelectIconSize), 0, 0, [], LOpacity);
        end;
      RadioButton:
        begin
          if LExistAdapter then
            B := ARow = FAdapter.ItemIndex
          else
            B := False;

          if B then
            FGridRes.Drawable.ImageIndex := 4
          else
            FGridRes.Drawable.ImageIndex := 3;

          if DrawState = TViewState.Enabled then
            FGridRes.Drawable.ImageIndex := FGridRes.Drawable.ImageIndex + 4;

          FGridRes.Drawable.Draw(Canvas, GetIconDrawRect(Item, R, CSelectIconSize, CSelectIconSize), 0, 0, [], LOpacity);
        end;
      Image: ;
      ProgressBar: ;
      CustomDraw: ;
    end;
  end;
end;

procedure TGridViewContent.DoDrawHeaderRows(Canvas: TCanvas; var R: TRectF);

  // ����Ԫ��
  procedure DrawRowCell(var LS: TGridViewDrawState);
  var
    I: Integer;
    X, LW: Double;
  begin
    FDividerBrush.Color := GridView.FDivider;
    X := LS.XOffset;
    LW := 0;

    if LS.ExistAdapter then
      FAdapter.SetCursor(LS.RowIndex);

    // ����Ԫ�񱳾�
    if FTwoColor and (LS.RowIndex mod 2 = 1) then begin
      if Assigned(FCellBrush.FActivated) and (FCellBrush.FActivated.Kind <> TBrushKind.None) then
        Canvas.FillRect(RectF(0, LS.Top, X + LS.ColumnWidth, LS.Bottom), 0, 0, [], LS.Opacity, FCellBrush.FActivated);
    end;

    // ����ǰ��
    if (LS.RowIndex = FSelectCell.Row) and Assigned(FCellBrush.FSelected) then begin
      if (gvRowSelect in GridView.FOptions) then
        Canvas.FillRect(RectF(0, LS.Top, X + LS.ColumnWidth, LS.Bottom), 0, 0, [], LS.Opacity, FCellBrush.FSelected); 
    end;

    X := LS.FirstColOffset;
    for I := LS.FirstColIndex to LS.MaxCols - 1 do begin
      LW := FBaseColumnsList[I].RealWidth;
      if LW <= 0 then
        Continue;
      LW := X + LW + LS.DividerH;
      // ������
      if (LW > 0) and (X < LS.Width) then
        DoDrawCell(Canvas, RectF(X + LS.FixedWidth, LS.Top, LW + LS.FixedWidth, LS.Bottom - LS.DividerH), LS.RowIndex, I, LS.Opacity);
      if LW > LS.Width then
        Break;
      X := LW;
    end;

    // ����
    if LS.ShowRowLine and (LS.DividerH > 0) then
      Canvas.FillRect(RectF(LS.FixedWidth, LS.Bottom - LS.DividerH, LW + LS.FixedWidth, LS.Bottom), 0, 0, [], LS.Opacity, FDividerBrush);
  end;

var
  V, LW: Double;
  I, J, LFB: Integer;
  LS: TGridViewDrawState;
  {$IFDEF DEBUGMSG}
  T: Int64;
  {$ENDIF}
begin
  if FExistWordWrap and (Length(GridView.FItemsPoints) = 0) then
    Exit;
  {$IFDEF DEBUGMSG}
  T := GetTimestamp;
  {$ENDIF}
  LFB := -1;
  FViewFullBottom := -1;

  LS.Height := R.Height;
  LS.Width := R.Width;
  LS.SelectionAnchor := GridView.FSelectionAnchor;
  
  LS.Top := FViewTop;
  LS.XOffset := 0 - GridView.HScrollBarValue;
  LS.ExistAdapter := Assigned(FAdapter);
  LS.Opacity := Opacity;

  LS.ColumnWidth := GridView.FColumns.Width;
  LS.FixedWidth := 0;

  LS.ShowColLine := gvColLines in GridView.FOptions;
  LS.ShowRowLine := gvRowLines in GridView.FOptions;
  FTwoColor := gvTwoColor in GridView.FOptions;

  // �̶�����
  LS.FixedCols := GridView.FFixedCols;
  // �������
  LS.MaxCols := GridView.ColCount;
  // �ָ��߿���
  LS.DividerH := GridView.GetDividerHeight;

  // ��ȡ����������Ϣ
  if FColumnsList.Count <> LS.MaxCols then
    Exit;

  // ���Ҫ��ʾ�ĵ�һ���к�
  LS.FirstColIndex := 0;
  LS.FirstColOffset := 0;
  LW := LS.XOffset;
  for I := 0 to LS.MaxCols - 1 do begin
    V := LW;
    LW := LW + FBaseColumnsList[I].RealWidth + LS.DividerH;
    if LW >= 0 then begin
      LS.FirstColIndex := I;
      LS.FirstColOffset := V;
      Break;
    end;
  end;

  // ������
  if LS.ExistAdapter then
    FAdapter.BeginDrawCells(FFirstRowIndex, FLastRowIndex);
  try
    for J := FFirstRowIndex to FLastRowIndex do begin
      V := LS.Top + RowHeight[J] + LS.DividerH;
      LS.Bottom := V;
      if V <= 0 then begin
        LS.Top := LS.Bottom;
        Continue;
      end;

      LS.RowIndex := J;
      DrawRowCell(LS);

      if LS.Top > LS.Height then
        Break;

      if (LFB = -1) and (LS.Bottom > LS.Height) then begin
        LFB := 1;
        FLastFullRowIndex := J - 1;
        FViewFullBottom := LS.Top - LS.DividerH;
      end;

      LS.Top := LS.Bottom;
    end;
  finally
    if LS.ExistAdapter then
      FAdapter.EndDrawCells;
  end;

  // ������
  if LS.ShowColLine and (LS.DividerH > 0) then begin
    LS.Top := FViewTop;
    LS.Bottom := FViewBottom;

    V := LS.XOffset;
    FDividerBrush.Color := GridView.FDivider;

    for I := 0 to LS.MaxCols - 1 do begin
      LW := FBaseColumnsList[I].RealWidth;
      if LW <= 0 then
        Continue;
      LW := V + LW + LS.DividerH;
      if (LW > 0) and (V <= R.Width) then
        Canvas.FillRect(RectF(LW + LS.FixedWidth - LS.DividerH, LS.Top, LW + LS.FixedWidth, LS.Bottom), 0, 0, [], LS.Opacity, FDividerBrush);
      V := LW;
    end;
  end;

  {$IFDEF DEBUGMSG}
  {$IFDEF MSWINDOWS}
  OutputDebugString(PChar(Format('�����ӣ���ʱ %dms', [GetTimestamp - T])));
  {$ENDIF}
  {$ENDIF}
end;

procedure TGridViewContent.DoEditCancel;
begin
  DoHideEditor;
end;

procedure TGridViewContent.DoEditComplete;
var
  CellSetting: TGridCellSettings;
begin
  if FEditText <> FEditor.Text then begin
    if Assigned(FAdapter) and FEditor.Visible and (FSelectCell.Row >= 0) and (FSelectCell.Col >= 0) then begin
      if not (GridView.FReadOnly or FColumnsList[FSelectCell.Col].ReadOnly) then begin
        FAdapter.GetCellSettings(FSelectCell.Col, FSelectCell.Row, CellSetting);
        if not ((Assigned(CellSetting) and CellSetting.ReadOnly)) then begin
          FAdapter.SetCursor(FSelectCell.Row);
          FAdapter.Cells[FColumnsList[FSelectCell.Col].ColIndex, FSelectCell.Row] := FEditor.Text;

          if Assigned(GridView.FOnCellEditDoneEvent) then
            GridView.FOnCellEditDoneEvent(GridView, FSelectCell, FEditor.Text);
        end;
      end;
    end;
  end;
  DoHideEditor;
end;

procedure TGridViewContent.DoEditExit(Sender: TObject);
begin
  if (gvCancelOnExit in GridView.FOptions) and (FEditor.Visible) and (not FEditorShowing) then begin
    if FMouseDowning then
      DoEditComplete
    else
      DoEditCancel;
  end;
end;

procedure TGridViewContent.DoEnter;
begin
  inherited;
  if (not FKeyDownIng) then
    DoEnterCell(FSelectCell);
end;

procedure TGridViewContent.DoEnterCell(const ACell: TGridCell);
begin
  if FIsDesigning then Exit;  
  FSelectCell := ACell;
  FSelectClickRef := 0;
  if ACell.Row >= 0 then   
    GridView.SelectionAnchor := ACell.Row;   
  if Assigned(GridView.FOnCellEnterEvent) then
    GridView.FOnCellEnterEvent(GridView, ACell);
  DoShowEditor();
end;

procedure TGridViewContent.DoExit;
begin
  inherited DoExit;
  DoEditExit(Self);
end;

procedure TGridViewContent.DoHideEditor;
var
  B: Boolean;
begin
  if not Assigned(FEditor) then
    Exit;
  B := FEditor.IsFocused;
  GridView.HideEditor;
  if B then
    SetFocus;
end;

procedure TGridViewContent.DoKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
var
  LCell: TGridCell;

  procedure GotoCell(const LCell: TGridCell);
  begin
    if (FSelectCell.Row >= 0) then
      DoLeaveCell(FSelectCell);

    if (FSelectCell.Row <> LCell.Row) or (FSelectCell.Col <> LCell.Col) then begin
      DoEnterCell(LCell);
      ScrollToSelectedCell();
      Invalidate;
    end;

    if (Sender = FEditor) and (not FEditor.Visible) then
      Self.SetFocus;
  end;
  
begin
  if FEditorShowing then
    Exit;
  FKeyDownIng := True;

  try
    if ssCtrl in Shift then begin
      case Key of
        vkUp:
          begin
            Key := 0;
            LCell.Row := Max(0, FFirstRowIndex);
            LCell.Col := FSelectCell.Col;
            ScrollToCell(LCell);
            Exit;
          end;
        vkDown:
          begin
            Key := 0;
            LCell.Row := Max(0, FLastFullRowIndex);
            LCell.Col := FSelectCell.Col;
            ScrollToCell(LCell);
            Exit;
          end;
        vkHome:
          begin
            Key := 0;
            LCell.Row := 0;
            LCell.Col := FSelectCell.Col;
            GotoCell(LCell);
            Exit;
          end;
        vkEnd:
          begin
            Key := 0;
            LCell.Row := GridView.RowCount - 1;
            LCell.Col := FSelectCell.Col;
            GotoCell(LCell);
            Exit;
          end;
      end;
    end;

    LCell.Row := GridView.FSelectionAnchor;
    LCell.Col := FSelectCell.Col;

    case Key of
      vkHome:
        begin
          LCell.Col := 0;
        end;
      vkEnd:
        begin
          LCell.Col := GridView.FColumns.ColsCount - 1;
        end;
      vkPrior:
        begin
          LCell.Row := Max(0, LCell.Row - (GridView.ContentViews.FLastFullRowIndex - GridView.ContentViews.FFirstRowIndex));
        end;
      vkNext:
        begin
          LCell.Row := Min(GridView.RowCount - 1, 
            LCell.Row + (GridView.ContentViews.FLastFullRowIndex - GridView.ContentViews.FFirstRowIndex));
        end;
      vkEscape:
        begin
          if (Sender = FEditor) and (gvEscCancelEdit in GridView.FOptions) then begin
            Key := 0;
            DoEditCancel;
            Exit;
          end;
        end;
      vkReturn:
        begin
          if Sender = FEditor then begin
            DoEditComplete;
            Exit;
          end else begin
            Inc(FSelectClickRef);
            DoShowEditor();
            Exit;
          end;
        end;
      vkLeft:
        begin
          LCell.Col := LCell.Col - 1;

          while LCell.Col > 0 do begin
            if FColumnsList[LCell.Col].Visible then
              Break;
            Dec(LCell.Col);
          end;

          if LCell.Col < 0 then begin
            LCell.Row := Max(LCell.Row - 1, 0);
            LCell.Col := GridView.FColumns.ColsCount - 1;
          end;
        end;
      vkRight, vkTab:
        begin
          Key := 0;

          if Key = vkTab then begin
            if not (gvTabs in GridView.FOptions) then begin
              inherited;
              Exit;
            end;
          end;
          LCell.Col := Max(0, LCell.Col + 1);

          while LCell.Col < GridView.FColumns.ColsCount do begin
            if FColumnsList[LCell.Col].Visible then
              Break;
            Inc(LCell.Col);
          end;

          if LCell.Col >= GridView.FColumns.ColsCount then begin
            LCell.Row := Min(LCell.Row + 1, FCount - 1);
            LCell.Col := 0;
          end;
        end;
      vkUp:
        begin
          LCell.Row := Max(0, LCell.Row - 1);
        end;
      vkDown:
        begin
          LCell.Row := Min(FCount - 1, LCell.Row + 1);
        end;
      vkSpace:
        begin
          if Sender = Self then begin
            Inc(FSelectClickRef);
            DoShowEditor;
            Exit;
          end else begin
            inherited;
            Exit;
          end;
        end
    else
      begin
        if (KeyChar = ' ') and (Sender = Self) then begin
          Inc(FSelectClickRef);
          DoShowEditor;
          Exit;
        end;
        inherited;
        Exit;
      end;
    end;

    Key := 0;
    GotoCell(LCell);

  finally
    FKeyDownIng := False;
  end;
end;

procedure TGridViewContent.DoLeaveCell(const ACell: TGridCell);
begin
  if FIsDesigning then Exit;
  if Assigned(GridView.FOnCellLeaveEvent) then
    GridView.FOnCellLeaveEvent(GridView, ACell);
  if FEditor.Visible then
    DoEditComplete;
end;

procedure TGridViewContent.DoRealign;

  // �̶��и�
  procedure FixedRowHeight(var LS: TGridViewDrawState);
  var
    LH: Double;
  begin
    LH := FDefaultItemHeight + LS.DividerH;
    FFirstRowIndex := Trunc(LS.ScrollValue / LH);
    FViewTop := 0 - LS.ScrollValue + LH * FFirstRowIndex;
    FLastRowIndex := FFirstRowIndex + Trunc(LS.Height / LH) + 1;
    if FLastRowIndex >= LS.RowCount then
      FLastRowIndex := Ls.RowCount - 1;
    FViewBottom := FViewTop + (FLastRowIndex - FFirstRowIndex + 1) * LH;
    FViewItemBottom := FViewBottom;
  end;

  // �����и�
  function CalcRowHeight(var LS: TGridViewDrawState): Double;
  var
    H: Double;
    I: Integer;
    LSize: TSizeF;
    Item: TGridColumnItem;
    LText: string;
  begin
    // ��ȡ�и�
    Result := FDefaultItemHeight;

    // �����Զ��и��е��иߣ�ȡ�����ֵ
    for I := 0 to LS.MaxCols - 1 do begin
      Item := FColumnsList[I];
      if Item.WordWrap and (Item.DataType = TGridDataType.PlanText) and LS.ExistAdapter then begin
        LText := FAdapter.Cells[I, LS.RowIndex];
        if (LText <> '') and
          GridView.FText.CalcTextObjectSize(LText,
            Item.Width - Item.Padding.Left - Item.Padding.Right,
            Scene.GetSceneScale, nil, LSize)
        then begin
          H := LSize.Height + Item.Padding.Top + Item.Padding.Bottom;
          if H > Result then
            Result := H;
        end;
      end else if LS.ExistAdapter and (Item.DataType in [TGridDataType.Image, TGridDataType.CustomDraw]) then
        Result := Max(Result, FAdapter.CellHeight[I, LS.RowIndex]);
    end;
  end;

  // ���¹���
  procedure DoRealignDown(var LS: TGridViewDrawState);
  var
    First, Last: Double;
    V, H: Double;
    S, I, J: Integer;

    AL, MH: Double;
    ItemDefaultH: Double;

    NewH: Single;
    Item: PSingle;
  begin
    // �������ǰ����ʾ�ĵ�һ��λ�ú����һ��λ��
    First := FLastScrollValue;
    Last := First + LS.Height;         // ʹ�õ�ǰ��ͼ�ĸ߶���Ϊ�ײ�λ��

    // �����������ʾλ��
    S := FFirstRowIndex;
    if S <= 0 then begin
      S := 0;
      V := 0;
    end else begin
      V := FViewTop;
    end;

    J := 0;

    ItemDefaultH := FDefaultItemHeight;

    //LogD(Format('V: %.2f, FirstIndex: %d. ScrollV: %.2f, ScrollM: %.2f. Down', [V, S, LS.ScrollValue, LS.MoveSpace]));

    FFirstRowIndex := -1;
    FLastRowIndex := -1;

//    // ���� Header λ�ú�״̬
//    if ListView.FEnablePullRefresh and Assigned(FHeader) and (S = 0) then
//      AdjustHeader(S, V, LS)
//    else if Assigned(FHeader) then
//      (FHeader as TControl).Visible := False;
//
//    // �Զ��帽��ͷ��
//    AdjustHeaderView(S, V, LS);

    MH := 0;
    AL := 0;

    // ��ָ��λ�ÿ�ʼ�����ɲ������б���
    for I := S to FCount - 1 do begin
      if I < 0 then Continue;
      Item := @GridView.FItemsPoints[I];

      // ��ȡ�б���߶�
      H := Item^;
      if H = 0 then
        H := ItemDefaultH
      else if H < 0 then
        Continue;

      // �ж��б������״̬
      if AL = 0 then begin
        if (V + H + LS.DividerH <= First) then begin
          // ����������������
          // �������һ���λ��
          V := V + H + LS.DividerH;
          Continue;
        end else if V > Last then begin
          // ����β����������
          Break;
        end;

        // ����ǵ�һ��������, ��¼״̬
        if FFirstRowIndex = -1 then begin
          S := I;
          FFirstRowIndex := I;
          FViewTop := V;
          if I = 0 then
            Last := Last + Height - V;
        end;
      end;

      // �����������������
      Inc(J);

      // ��ȡ�б���߶�
      LS.RowIndex := I;
      NewH := CalcRowHeight(LS);

      // ���� V, �����б���ĵײ�λ��
      if (NewH > 0) then
        V := V + NewH + LS.DividerH;

      // ����и߸����ˣ��������Ҫ�����������Ĵ�С�������¼һ�±仯��С
      if Item^ <> NewH then begin
        if NewH <= 0 then begin
          LS.AdjustH := LS.AdjustH - H
        end else begin
          LS.AdjustH := LS.AdjustH + (NewH - H)
        end;
        Item^ := NewH;
      end;

      MH := Max(MH, NewH);
    end;

    FViewItemBottom := V;

//    // �Զ��帽��β��
//    if Assigned(FFooterView) then begin
//      if (S + J >= FCount) then begin
//        H := FFooterView.Height;
//        FFooterView.SetBounds(lS.Left, V - LS.ScrollValue, FSize.Width, H);
//        FFooterView.Visible := True;
//        V := V + H + LS.DividerH;
//      end else
//        FFooterView.Visible := False;
//    end;
//
//    // ���� Footer λ�ú�״̬
//    if ListView.FEnablePullLoad and Assigned(FFooter) and (FCount > 0) then begin
//      AdjustFooter(S + J, V, LS);
//    end else if Assigned(FFooter) then
//      (FFooter as TControl).Visible := False;

    FViewBottom := V;
    FFirstRowIndex := S;
    FLastRowIndex := S + J - 1;

    //LogD(Format('Bottom: %.2f, FirstIndex: %d, LastIndex: %d. Down', [FViewBottom, FFirstRowIndex, FLastRowIndex]));
  end;

  // ���Ϲ���
  procedure DoRealignUp(var LS: TGridViewDrawState);
  var
    First, Last: Double;
    V, H: Double;
    S, I, J: Integer;

    ItemDefaultH: Double;
    NewH: Single;
    Item: PSingle;
  begin
    // �������ǰ����ʾ�ĵ�һ��λ�ú����һ��λ��
    First := FLastScrollValue;
    Last := First + LS.Height;         // ʹ�õ�ǰ��ͼ�ĸ߶���Ϊ�ײ�λ��

    // ������������ʾλ��
    S := FLastRowIndex;
    if S >= FCount then
      S := FCount - 1;
    V := FViewItemBottom;

    ItemDefaultH := FDefaultItemHeight;

    //LogD(Format('V: %.2f, S: %d, ', [V, S]));

//    // �Զ��帽��β��
//    if Assigned(FFooterView) then begin
//      H := FFooterView.Height;
//      if (S >= FCount - 1) then begin
//        FFooterView.SetBounds(lS.Left, V - LS.ScrollValue, FSize.Width, H);
//        FFooterView.Visible := True;
//        V := V + H + LS.DividerH;
//      end else begin
//        FFooterView.Visible := False;
//      end;
//    end;
//
//    // ���� Footer λ�ú�״̬
//    if ListView.FEnablePullLoad and Assigned(FFooter) and (FCount > 0) then begin
//      AdjustFooter(S, V, LS);
//    end else if Assigned(FFooter) then
//      (FFooter as TControl).Visible := False;

    //V := FViewItemBottom;
    J := 0;

    FFirstRowIndex := -1;
    FLastRowIndex := -1;

    // ��ָ��λ�ÿ�ʼ�����ɲ������б���
    for I := S downto 0 do begin
      if I >= FCount then Continue;
      Item := @GridView.FItemsPoints[I];

      // ��ȡ�б���߶�
      H := Item^;
      if H = 0 then
        H := ItemDefaultH
      else if H < 0 then
        Continue;

      // ����
      if (V <= First) then begin
        // ����������������
        Break;
      end else if (V - H - LS.DividerH) >= Last then begin
        // ����β����������
        // �������һ���λ��
        V := V - H - LS.DividerH;
        Continue;
      end;

      // ����ǵ�һ��������, ��¼״̬
      if FLastRowIndex = -1 then begin
        FLastRowIndex := I + J;
        FViewItemBottom := V;
      end;

      // �����������������
      Inc(J);

      // ��ȡ�б���
      LS.RowIndex := I;
      NewH := CalcRowHeight(LS);

      // ���� V, �����б���ĵײ�λ��
      if (NewH > 0) then
        V := V - NewH - LS.DividerH;

      // ����и߸����ˣ��������Ҫ�����������Ĵ�С�������¼һ�±仯��С
      if Item^ <> NewH then begin
        if NewH <= 0 then begin
          LS.AdjustH := LS.AdjustH - H
        end else begin
          LS.AdjustH := LS.AdjustH + (NewH - H)
        end;
        Item^ := NewH;
      end;

    end;

    // ��¼�ײ�λ��
    FViewTop := V;
    FFirstRowIndex := FLastRowIndex - J + 1;

//    // �Զ��帽��ͷ��
//    S := FFirstRowIndex;
//    V := FViewTop - LS.DividerH;
//    AdjustHeaderView(S, V, LS);
//
//    // ���� Header λ�ú�״̬
//    if ListView.FEnablePullRefresh and Assigned(FHeader) and (S = 0) then
//      AdjustHeader(S, V, LS)
//    else if Assigned(FHeader) then
//      (FHeader as TControl).Visible := False;

    if LS.AdjustH <> 0 then begin
      FViewTop := FViewTop + LS.AdjustH;
      FViewBottom := FViewBottom + LS.AdjustH;
      FViewItemBottom := FViewItemBottom + LS.AdjustH;
    end;

    //LogD(Format('Bottom: %.2f, FirstIndex: %d, LastIndex: %d. Up', [FViewBottom, FFirstRowIndex, FLastRowIndex]));
  end;

  // �Զ��и�
  procedure WordWrapRow(var LS: TGridViewDrawState);
  begin
    if LS.MoveSpace >= 0 then begin
      DoRealignDown(LS);   // ���¹���
    end else begin
      DoRealignUp(LS);    // ���Ϲ���
    end;

    //PH := @GridView.FItemsPoints[0];
  end;

  procedure DoChangeEditor(var LS: TGridViewDrawState);
  var
    LR: TRectF;
    LV: Boolean;
  begin
    if Assigned(FEditor) and (FEditor.Visible) then begin
      LR := SelectCellRect;
      LV := (LR.Bottom > 0) and (LR.Top < LS.Height) and (LR.Right > 0) and (LR.Left < LS.Width);
      if LV then
        FEditor.SetBounds(LR.Left, LR.Top, LR.Width, LR.Height)
      else
        FEditor.SetBounds(-999, -999, 1, 1);
    end;
  end;

var
  {$IFDEF DEBUGMSG}T: Int64; {$ENDIF}
  LDisablePaint: Boolean;
  LS: TGridViewDrawState;
  LHMove: Double;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  // ���ڵ����в�����
  if FDisableAlign or (Scene = nil) or (not Assigned(Canvas)) then
    Exit;
  if not Assigned(GridView) then
    Exit;
  {$IFDEF DEBUGMSG}
  T := GetTimestamp;
  {$ENDIF}

  LDisablePaint := FDisablePaint;
  FDisableAlign := True;
  FDisablePaint := True;

  LS.ExistAdapter := Assigned(FAdapter);

  // �Ƿ�����Զ��߶ȵ��У�
  FExistWordWrap := GridView.Columns.ExistWordWarp and LS.ExistAdapter;

  if (Length(GridView.FItemsPoints) = 0) then begin
    if FExistWordWrap then begin
      FDisableAlign := False;
      FDisablePaint := LDisablePaint;
      Exit;
    end;
  end;

  LS.AdjustH := 0;
  LS.Height := Height;
  LS.Width := Width;

  // ƫ��λ�� (������λ��)
  LS.ScrollValue := GridView.VScrollBarValue;
  // ��������
  LS.MoveSpace := LS.ScrollValue - FLastScrollValue;
  // ��¼������ֵ
  FLastScrollValue := LS.ScrollValue;

  LHMove := GridView.HScrollBarValue - FLastHScrollValue;
  FLastHScrollValue := GridView.HScrollBarValue;

  // ���ȡ��߶ȡ����������ޱ仯��������
  if (FLastW = LS.Width) and (FLastH = LS.Height) and (LS.MoveSpace = 0) then begin
    if LHMove <> 0 then
      DoChangeEditor(LS);
    FDisableAlign := False;
    FDisablePaint := LDisablePaint;
    Exit;
  end else begin
    // ��¼�±�������ʱ���б������С
    FLastW := LS.Width;
    FlastH := LS.Height;
  end;

  // �̶�����
  LS.FixedCols := GridView.FFixedCols;
  // �������
  LS.MaxCols := GridView.ColCount;
  // �ָ��߿���
  LS.DividerH := GridView.GetDividerHeight;

  if FColumnsList.Count <> LS.MaxCols then
    InitColumnList;

  // ����̶��п���
  LS.FixedWidth := GridView.FFixedColsWidth;

  BeginUpdate;
  try
    FExistWordWrap := False;

    LS.RowCount := FCount;
    if LS.RowCount < 1 then
      LS.RowCount := TGridBase.CDefaultEmptyRows;

    // Ĭ���и�
    FDefaultItemHeight := GridView.RowHeight;

    // ֻ�д�����Ҫ�Զ��߶ȵ���ʱ�����������иߣ�����ʹ��Ĭ���и�
    if FExistWordWrap then begin
      WordWrapRow(LS);
    end else
      FixedRowHeight(LS);

    DoChangeEditor(LS);

  finally
    FDisablePaint := LDisablePaint;
    EndUpdate;
    if LS.AdjustH <> 0 then begin
      // �߶ȱ仯��, ���¹�����״̬
      GridView.FContentBounds.Bottom := GridView.FContentBounds.Bottom + LS.AdjustH;
      if LS.IsUp then
        GridView.DoUpdateScrollingLimits(True, LS.AdjustH)
      else
        GridView.DoUpdateScrollingLimits(True);
    end;
    FDisableAlign := False;
  end;

  {$IFDEF MSWINDOWS}
  {$IFDEF DEBUGMSG}
  OutputDebugString(PChar(Format('DoRealign. Rows: %d, time: %dms.', [LS.RowCount, GetTimestamp - T])));
  {$ENDIF}
  {$ENDIF}
end;

procedure TGridViewContent.DoShowEditor;
var
  Item: TGridColumnItem;
  CellSetting: TGridCellSettings;
begin
  FEditorShowing := True;
  try
    if not (gvEditing in GridView.FOptions) then begin
      if FEditor.Visible then
        DoHideEditor;
      Exit;
    end;
    if (gvAlwaysShowEditor in GridView.FOptions) or (FSelectClickRef > 0) then begin

      if (FSelectCell.Col < 0) then
        Item := nil
      else
        Item := FColumnsList[FSelectCell.Col];

      if Assigned(FAdapter) then
        FAdapter.GetCellSettings(FSelectCell.Col, FSelectCell.Row, CellSetting)
      else
        CellSetting := nil;

      if (Item = nil) or (not (Item.DataType in [TGridDataType.PlanText])) or
        (Item.Locked) or
        ((CellSetting <> nil) and (CellSetting.Locked or (not CellSetting.Enabled))) then begin
        if FEditor.Visible then
          DoHideEditor;
        Exit;
      end;

      GridView.ShowEditor;
      if FEditor.Visible then begin
        FEditor.ReadOnly := Item.ReadOnly or (GridView.FReadOnly) or (Assigned(CellSetting) and CellSetting.ReadOnly);
        if Assigned(FAdapter) then begin
          FAdapter.SetCursor(FSelectCell.Row);
          FEditor.Text := FAdapter.Cells[Item.ColIndex, FSelectCell.Row]
        end else
          FEditor.Text := '';
        FEditText := FEditor.Text;
        FEditor.SelectAll;
      end;
    end;
  finally
    FEditorShowing := False;
  end;
end;

function TGridViewContent.GetRowHeight(const ARow: Integer): Single;
begin
  if FExistWordWrap then begin
    Result := GridView.FItemsPoints[ARow];
    if Result = 0 then begin
      if Assigned(FAdapter) then
        Result := FAdapter.ItemDefaultHeight
      else
        Result := GridView.FFixedRowHeight;
    end;
  end else 
    Result := FDefaultItemHeight;
end;

function TGridViewContent.GetVisibleRowCount: Integer;
begin
  Result := FLastRowIndex - FFirstRowIndex;
end;

procedure TGridViewContent.InitColumnList;
var
  I: Integer;
  //T: Cardinal;
  Item: TGridColumnItem;
  X, DH: Double;
  IsExistWordWarp: Boolean;
begin
  //T := TThread.GetTickCount;
  FColumnsList.Clear;
  FBaseColumnsList.Clear;
  IsExistWordWarp := False;

  if GridView.FColumns.ColsCount > 0 then begin
    FColumnsList.Capacity := GridView.FColumns.ColsCount;
    FBaseColumnsList.Capacity := GridView.FColumns.ColsCount;

    DH := GridView.GetDividerHeight;
    X := 0;

    for I := 0 to GridView.FColumns.ColsCount - 1 do begin
      Item := GridView.FColumns.Items[I, 0];
      Item.X := X;
      X := X + Item.RealWidth + DH;
      FBaseColumnsList.Add(Item);
      if not IsExistWordWarp then
        IsExistWordWarp := Item.WordWrap;
    end;

    for I := 0 to GridView.FColumns.ColsCount - 1 do
      FColumnsList.Add(GridView.FColumns.ItemCols[I]);
  end;

  GridView.FColumns.FExistWordWarp := IsExistWordWarp;
  //T := TThread.GetTickCount - T;
  //OutputDebugString(PChar(Format('InitColumnList, %dms.', [T])));
end;

procedure TGridViewContent.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  DoKeyDown(Self, Key, KeyChar, Shift);
end;

procedure TGridViewContent.KeyUp(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  inherited;
end;

procedure TGridViewContent.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  LCel: TGridCell;
begin
  FMouseDowning := True;

  inherited;

  if FIsDesigning then Exit;

  FDownPos.X := X;
  FDownPos.Y := Y;

  FMouseDowning := False;

  if IsPointInRect(FDownPos, FRBRect) then
    Exit;

  LCel := PointAsCell(X, Y);
  if (FSelectCell.Row <> LCel.Row) or (FSelectCell.Col <> LCel.Col) then begin
    if (FSelectCell.Row >= 0) then begin
      DoLeaveCell(FSelectCell);
    end;
    DoEnterCell(LCel);
    Invalidate;
  end;
end;

procedure TGridViewContent.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
end;

function TGridViewContent.ObjectAtPoint(AScreenPoint: TPointF): IControl;
begin
  if Assigned(GridView.FAniCalculations) and (GridView.FAniCalculations.Shown) then
    Result := nil   // ���ƹ����У��������������
  else
    Result := inherited ObjectAtPoint(AScreenPoint);
end;

procedure TGridViewContent.PaintBackground;
var
  R: TRectF;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  if (FInVisible) or (Assigned(GridView) and (GridView.FInVisible)) then
    Exit;
  inherited PaintBackground;

  if Assigned(GridView) and (Assigned(GridView.Columns)) then begin
    R := RectF(0, 0, Width, Height);

    DoDrawHeaderRows(Canvas, R);

    if Assigned(GridView.FScrollV) and (GridView.FScrollV.Visible) and 
      (Assigned(GridView.FScrollH)) and (GridView.FScrollH.Visible) 
    then begin
      R := RectF(R.Right - GridView.FScrollV.Width, R.Top - GridView.FScrollH.Height, R.Right, R.Bottom);
      Canvas.FillRect(R, 0, 0, [], Opacity, FScrollRB);
    end else
      R.Clear;
    FRBRect := R;
  end;
end;

function TGridViewContent.PointAsCell(const X, Y: Single): TGridCell;
var
  V, LV: Double;
  I: Integer;
  LDividerH: Double;
begin
  Result.Row := -1;
  Result.Col := -1;
  
  V := 0 - GridView.HScrollBarValue;

  for I := 0 to GridView.ColCount - 1 do begin
    LV := V + FBaseColumnsList[I].Right;
    if (X < LV) then begin
      Result.Col := I;
      Break;
    end;
  end;

  if Result.Col < 0 then Exit;  
  LDividerH := GridView.GetDividerHeight;

  V := FViewTop;
  for I := FFirstRowIndex to FLastRowIndex do begin
    LV := V + RowHeight[I] + LDividerH;
    if (Y >= V) and (Y < LV) then begin
      Result.Row := I;
      Break;
    end;
    V := LV;
  end;
end;

procedure TGridViewContent.ScrollToCell(const ACell: TGridCell);
var
  X, Y, LX, LY, V, LV, DH, W: Double;
  I: Integer;
begin
  if (ACell.Row < 0) then
    Exit;
    
  DH := GridView.GetDividerHeight;
  X := GridView.HScrollBarValue;
  Y := GridView.VScrollBarValue;
  LX := X;
  LY := Y;

  if ACell.Row <= FFirstRowIndex then begin
    V := FViewTop;
    for I := FFirstRowIndex downto ACell.Row do
      V := V - RowHeight[I] - DH; 
    Y := Y - (FViewTop - V);
  end else if (ACell.Row >= FLastFullRowIndex) then begin
    V := FViewFullBottom;
    for I := FLastFullRowIndex to ACell.Row do 
      V := V + RowHeight[I] + DH;
    Y := Y + (V - FViewFullBottom);
  end;

  if ACell.Col >= 0 then begin  
    W := Width;
    {$IFDEF MSWINDOWS}
    if Assigned(GridView.FScrollV) and (GridView.FScrollV.Visible) then
      W := W - GridView.FScrollV.Width;
    {$ENDIF}   
      
    V := 0 - GridView.HScrollBarValue;
    for I := 0 to GridView.ColCount - 1 do begin
      LV := V + GridView.Columns[I, 0].Width + DH;
      if I = ACell.Col then begin
        if (V < 0) or (LV >= W) then begin
          if V < 0 then
            X := X - Abs(V)
          else
            X := X + (LV - W);
        end;
        Break;    
      end;
      V := LV;
    end;  
  end;

  if (LX <> X) or (LY <> Y) then   
    GridView.ScrollTo(X, Y);
end;

procedure TGridViewContent.ScrollToSelectedCell;
begin
  ScrollToCell(FSelectCell);
end;

function TGridViewContent.SelectCellRect: TRectF;
begin
  Result := CellRect(FSelectCell);
end;

{ TGridAdapterBase }

procedure TGridAdapterBase.BeginDrawCells(const AFirstRow, ALastRow: Integer);
begin
end;

procedure TGridAdapterBase.Clear;
begin
  FCellSetttings.Clear;
end;

constructor TGridAdapterBase.Create;
begin
  FDefaultRowHeight := TGridBase.CDefaultFixedRowHeight;
  FCellSetttings := TDictionary<Int64, TGridCellSettings>.Create;
  FCellSetttings.OnValueNotify := DoValueNotify;
  DoInitData;
end;

destructor TGridAdapterBase.Destroy;
begin
  FreeAndNil(FCellSetttings);
  inherited;
end;

procedure TGridAdapterBase.DoInitData;
begin
  FItemIndex := -1;
end;

procedure TGridAdapterBase.DoValueNotify(Sender: TObject;
  const Item: TGridCellSettings;
  Action: System.Generics.Collections.TCollectionNotification);
begin
  if Action = System.Generics.Collections.TCollectionNotification.cnRemoved then
    if Assigned(Item) then
      Item.DisposeOf;
end;

procedure TGridAdapterBase.EndDrawCells;
begin
end;

function TGridAdapterBase.GetBestColumnWidth(const ACol: Integer): Single;
var
  DataLen, I, J: Integer;
  Item: TGridColumnItem;
  LText: string;
begin
  if Assigned(GridView) and Assigned(GridView.Scene) then begin
    Item := GridView.Columns.ItemCols[ACol];
    case Item.DataType of
      TGridDataType.PlanText:
        begin
          LText := Item.DisplayText;
          DataLen := CharCount(LText);
          for I := 0 to RowCount - 1 do begin
            SetCursor(I);
            J := CharCount(Cells[ACol, I]);
            if J > DataLen then begin
              DataLen := J;
              LText := Cells[ACol, I];              
            end;
          end;
          Result := GridView.FFixedText.CalcTextWidth(LText, GridView.Scene.GetSceneScale) +
            Item.Padding.Left + Item.Padding.Right;
          if Item.DataFilter then
            Result := Result + TGridBase.CDefaultFilterIconWH + 2;
          Result := Max(12, Result);
        end;
      TGridDataType.CheckBox, TGridDataType.RadioButton:
        begin
          LText := Item.DisplayText;
          Result := Item.Padding.Left + Item.Padding.Right +
            Max(20, GridView.FFixedText.CalcTextWidth(LText, GridView.Scene.GetSceneScale));
        end
    else
      Result := TGridBase.CDefaultMinColWidth;
    end;
  end else
    Result := TGridBase.CDefaultMinColWidth;
end;

function TGridAdapterBase.GetCellChecked(const ACol, ARow: Integer): Boolean;
begin
  Result := GetCells(ACol, ARow) = '1';
end;

function TGridAdapterBase.GetCellHeight(const ACol, ARow: Integer): Single;
begin
  Result := ItemDefaultHeight;
end;

function TGridAdapterBase.GetCells(const ACol, ARow: Integer): string;
begin
  Result := '';
end;

function TGridAdapterBase.GetCellSetting(const ACol,
  ARow: Integer): TGridCellSettings;
begin
  if not GetCellSettings(ACol, ARow, Result) then begin
    Result := TGridCellSettings.Create;
    FCellSetttings.Add(TGridBase.GetKey(ACol, ARow), Result);
  end;
end;

function TGridAdapterBase.GetCellSettings(const ACol, ARow: Integer;
  out ACellSettings: TGridCellSettings): Boolean;
var
  Key: Int64;
begin
  Key := TGridBase.GetKey(ACol, ARow);
  if FCellSetttings.ContainsKey(Key) then begin
    ACellSettings := FCellSetttings.Items[Key];
    Result := True;
  end else begin
    ACellSettings := nil;
    Result := False;
  end;
end;

function TGridAdapterBase.GetColCount: Integer;
begin
  if Assigned(GridView) then
    Result := GridView.FColumns.ColsCount
  else
    Result := 0;
end;

function TGridAdapterBase.GetColumns: TGridColumns;
begin
  if Assigned(GridView) then
    Result := GridView.FColumns
  else
    Result := nil;
end;

function TGridAdapterBase.GetFixedColData(
  const ACol: Integer): TGridFixedColumnItem;
begin
  Result.Title := '';
  FillChar(Result, SizeOf(Result), 0);
  Result.Width := TGridBase.CDefaultFixedColWidth;
end;

function TGridAdapterBase.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

function TGridAdapterBase.GetRowHeight: Single;
begin
  Result := TGridBase.CDefaultFixedRowHeight;
end;

function TGridAdapterBase.GetRowID(const ARow: Integer): Int64;
begin
  Result := ARow;
end;

function TGridAdapterBase.IsEmpty: Boolean;
begin
  Result := RowCount = 0;
end;

function TGridAdapterBase.ItemDefaultHeight: Single;
begin
  Result := FDefaultRowHeight;
end;

procedure TGridAdapterBase.NotifyDataChanged;
begin
  if Assigned(FGridView) then
    FGridView.NotifyDataChanged;
end;

procedure TGridAdapterBase.Repaint;
begin
  if Assigned(FGridView) and Assigned(FGridView.FContentViews) then
    FGridView.FContentViews.Realign;
end;

procedure TGridAdapterBase.SetCellChecked(const ACol, ARow: Integer;
  const Value: Boolean);
begin
  if Value then
    SetCells(ACol, ARow, '1')
  else
    SetCells(ACol, ARow, '0');
end;

procedure TGridAdapterBase.SetCells(const ACol, ARow: Integer;
  const Value: string);
begin
end;

procedure TGridAdapterBase.SetCellSettings(const ACol, ARow: Integer;
  const ACellSettings: TGridCellSettings);
begin
  FCellSetttings.AddOrSetValue(TGridBase.GetKey(ACol, ARow), ACellSettings);
end;

procedure TGridAdapterBase.SetCursor(const ARow: Integer);
begin  
end;

procedure TGridAdapterBase.SetItemDefaultHeight(const Value: Single);
begin
  FDefaultRowHeight := Value;
end;

procedure TGridAdapterBase.SetItemIndex(const Value: Integer);
begin
  FItemIndex := Value;
end;

{ TGridColumnItem }

procedure TGridColumnItem.Assign(Source: TPersistent);
var
  Src: TGridColumnItem;
  LTitle: string;
begin
  if Source is TGridColumnItem then begin
    Src := TGridColumnItem(Source);
    IsLeftTop := Src.IsLeftTop;
    FWidth := Src.FWidth;
    ColIndex := Src.ColIndex;
    RowIndex := Src.RowIndex;
    Gravity := Src.Gravity;
    DataType := Src.DataType;
    Opacity := Src.Opacity;
    Padding := Src.Padding;

    Locked := Src.Locked;
    DataFilter := Src.DataFilter;
    ReadOnly := Src.ReadOnly;
    Visible := Src.Visible;
    Enabled := Src.Enabled;
    WordWrap := Src.WordWrap;
    IsBLOB := Src.IsBLOB;

    RowsPan := Src.RowsPan;
    ColsPan := Src.ColsPan;

    Tag := Src.Tag;
    LTitle := Src.DisplayText;
    if DisplayText <> LTitle then
      Title := LTitle;
  end else
    inherited;
end;

constructor TGridColumnItem.Create(AOwner: TGridColumns);
begin
  FOwner := AOwner;
  ColIndex := -1;
  FWidth := TGridBase.CDefaultCellWidth;
  Gravity := TLayoutGravity.CenterVertical;
  Opacity := 1;

  Visible := True;
  Enabled := True;

  Padding.Left := 3;
  Padding.Top := 3;
  Padding.Right := 3;
  Padding.Bottom := 3;
end;

destructor TGridColumnItem.Destroy;
begin
  Title := '';
  inherited;
end;

procedure TGridColumnItem.DoChange;
begin
  if Assigned(FOwner) then
    FOwner.DoItemChange(Self);
end;

function TGridColumnItem.GetDispLayText: string;
begin
  Result := Title;
  if (Result = '') and (RowIndex = 0) and Assigned(FOwner) and (FOwner.FShowColIndex) then
    Result := IntToStr(Self.ColIndex + 1)
end;

function TGridColumnItem.GetIndex: Integer;
var
  ARow: Integer;
begin
  if not GetRowCol(Result, ARow) then
    Result := -1;
end;

function TGridColumnItem.GetRealWidth: Single;
begin
  if Visible then
    Result := Width
  else
    Result := 0;
end;

function TGridColumnItem.GetRight: Double;
begin
  Result := X + RealWidth;
end;

function TGridColumnItem.GetRowCol(var ACol, ARow: Integer): Boolean;
var
  I, J: Integer;
  Item: TObject;
begin
  Result := False;
  if not Assigned(FOwner) then
    Exit;
  for I := 0 to FOwner.FMaxRows - 1 do begin
    for J := 0 to FOwner.FMaxCols - 1 do begin
      if FOwner.FData.TryGetValue(TGridBase.GetKey(J, I), Item) then begin
        if Item = Self then begin
          ACol := J;
          ARow := I;
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TGridColumnItem.SetIndex(const Value: Integer);
var
  ARow, ACol, NewCol: Integer;
  I: Integer;
  Key: Int64;
  Item, LItem: TGridColumnItem;
  //LNotify: TCollectionNotifyEvent<TGridColumnItem>;
  LNotify: TYXDIntHashItemFreeNotify;

  procedure UpdateItem(const I: Integer);
  begin
    Key := TGridBase.GetKey(I, 0);

    if I = NewCol then
      Item := Self
    else
      Item := LItem;

    if not FOwner.FData.TryGetValue(Key, TObject(LItem)) then
      LItem := nil;

    if Assigned(Item) then
      FOwner.FData.AddOrUpdate(Key, Item)
    else
      FOwner.FData.Remove(Key);
  end;

begin
  if not Assigned(FOwner) then
    Exit;
  NewCol := Value;
  if NewCol < 0 then NewCol := 0;
  if NewCol >= FOwner.FMaxCols then
    NewCol := FOwner.FMaxCols - 1;

  if GetRowCol(ACol, ARow) then begin
    if NewCol = ACol then
      Exit;
    LNotify := FOwner.FData.OnFreeItem;
    FOwner.FData.OnFreeItem := nil;

    try
      if NewCol < ACol then begin

        for I := NewCol to ACol do
          UpdateItem(I);

      end else begin

        for I := NewCol downto ACol do
          UpdateItem(I);

      end;
    finally
      FOwner.FData.OnFreeItem := LNotify;
      FOwner.DoChange;
    end;
  end;
end;

procedure TGridColumnItem.SetWidth(const Value: Single);
begin
  if FWidth <> Value then begin
    FWidth := Value;
    DoChange;
  end;
end;

{ TGridColumns }

procedure TGridColumns.Assign(Source: TPersistent);
var
  Src: TGridColumns;
  I, J: Integer;
  Key: Int64;
  Item: TGridColumnItem;
  LOnChange: TNotifyEvent;
begin
  if Source is TGridColumns then begin
    LOnChange := FOnChange;
    try
      Src := TGridColumns(Source);
      Self.FMaxRows := Src.FMaxRows;
      Self.FMaxCols := Src.FMaxCols;
      Self.FData.Clear;
      for I := 0 to FMaxRows - 1 do begin
        for J := 0 to FMaxCols - 1 do begin
          Key := TGridBase.GetKey(J, I);
          if Src.FData.TryGetValue(Key, TObject(Item)) and Assigned(Item) then
            Items[J, I].Assign(Item);
        end;
      end;
    finally
      FOnChange := LOnChange;
      DoChange;
    end;
  end else
    inherited;
end;

procedure TGridColumns.Clear;
begin
  if FData.Count > 0 then
    FData.Clear;
  FMaxRows := 1;
  FMaxCols := 0;
  FLastWidth := -1;
end;

constructor TGridColumns.Create(AGridView: TGridBase);
begin
  FGridView := AGridView;
  FData := TIntHash.Create(99991);
  FData.OnFreeItem := DoValueNotify;
  FMaxRows := 1;
  FMaxCols := 1;
  FLastWidth := -1;
  FColumnClass := TGridColumnItem;
end;

destructor TGridColumns.Destroy;
begin
  if FData.Count > 0 then
    FData.Clear;
  FreeAndNil(FData);
  inherited;
end;

procedure TGridColumns.DoChange;
begin
  FLastWidth := -1;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGridColumns.DoItemChange(Sender: TObject);
var
  I, J: Integer;
begin
  if Assigned(Sender) and (Sender is TGridColumnItem) then begin
    if TGridColumnItem(Sender).GetRowCol(J, I) then
      DoItemChangeEx(TGridColumnItem(Sender), J, I);
  end;
end;

procedure TGridColumns.DoItemChangeEx(Sender: TGridColumnItem; const ACol, ARow: Integer);
var
  I: Integer;
begin
  if (ACol < 0) then Exit;
  if (FMaxRows < 2) then begin
    FLastWidth := -1;
  end else begin
    FLastWidth := -1;
    for I := 0 to FMaxRows - 1 do begin
      if I <> ARow then
        GetItem(ACol, I).Width := Sender.Width;
    end;
  end;
end;

procedure TGridColumns.DoValueNotify(Item: PIntHashItem);
begin
  if (Item <> nil) and Assigned(Item.AsPointer) then
    TObject(Item.AsPointer).DisposeOf;
end;

function TGridColumns.GetExistWordWarp: Boolean;
var
  I: Integer;
  Item: TGridColumnItem;
begin
  Result := False;
  // ��ȡ����������Ϣ
  for I := 0 to FMaxCols - 1 do begin
    if FData.TryGetValue(TGridBase.GetKey(I, 0), TObject(Item)) then begin
      if Assigned(Item) and Item.WordWrap then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TGridColumns.GetHeight: Double;
begin
  if Assigned(FGridView) then
    Result := (FGridView.FixedRowHeight + FGridView.GetDividerHeight) * RowsCount
  else
    Result := (TGridBase.CDefaultFixedRowHeight + 1) * RowsCount;
end;

function TGridColumns.GetItem(const ACol, ARow: Integer): TGridColumnItem;
var
  Key: Int64;
  V, LMaxRows, LMaxCols: Integer;
begin
  Key := TGridBase.GetKey(ACol, ARow);

  if FData.TryGetValue(Key, TObject(Result)) then begin
    if Result = nil then
      FData.Remove(Key)
    else
      Exit;
  end;

  Result := FColumnClass.Create(Self);
  Result.ColIndex := ACol;
  Result.RowIndex := ARow;
  FData.Add(Key, Result);

  if ARow < 0 then begin
    if ACol < 0 then
      Result.IsLeftTop := True;
    Exit;
  end;

  LMaxRows := FMaxRows;
  LMaxCols := FMaxCols;

  // ���¼������������
  V := Max(Result.RowsPan - 1, 0);
  if ARow + V >= FMaxRows then
    FMaxRows := ARow + V + 1;

  V := Max(Result.ColsPan - 1, 0);
  if ACol + V >= FMaxCols then
    FMaxCols := ACol + V + 1;

  if (FMaxRows <> LMaxRows) or (FMaxCols <> LMaxCols) then
    DoChange;
  DoItemChangeEx(Result, ACol, ARow);
end;

function TGridColumns.GetItemCols(const ACol: Integer): TGridColumnItem;
var
  ARow, I, J: Integer;
  Item: TGridColumnItem;
begin
  ARow := RowsCount - 1;
  if ARow < 0 then begin
    Result := Items[ACol, -1];
    Exit;
  end;

  Result := nil;
  I := ACol;

  while (Result = nil) and (I >= 0) do begin
    J := ARow;
    while J >= 0 do begin
      if FData.TryGetValue(TGridBase.GetKey(I, J), TObject(Item)) then begin
        if Result = nil then
          Result := Item
        else begin
          if Item <> nil then begin
            if Item.ColsPan < Result.ColsPan then
              Result := Item;
          end;
        end;
      end;
      Dec(J);
    end;
    if Result = nil then begin
      Result := Items[ACol, 0];
      Exit;
    end;
    Dec(I);
  end;

  if Result = nil then
    Result := Items[ACol, 0];
end;

function TGridColumns.GetItemOfKey(const Key: Int64): TGridColumnItem;
var
  V, LMaxRows, LMaxCols: Integer;
begin
  if FData.ContainsKey(Key) then begin
    Result := TGridColumnItem(FData[Key].AsPointer);
    if Result = nil then
      FData.Remove(Key)
    else
      Exit;
  end;

  Result := TGridColumnItem.Create(Self);
  Result.ColIndex := TGridCell(Key).Col;
  FData.Add(Key, Result);

  if TGridCell(Key).Row < 0 then begin
    if (TGridCell(Key).Col < 0) then
      Result.IsLeftTop := True;
    Exit;
  end;

  LMaxRows := FMaxRows;
  LMaxCols := FMaxCols;

  // ���¼������������
  V := Max(Result.RowsPan - 1, 0);
  if TGridCell(Key).Row + V >= FMaxRows then
    FMaxRows := TGridCell(Key).Row + V + 1;

  V := Max(Result.ColsPan - 1, 0);
  if TGridCell(Key).Col + V >= FMaxCols then
    FMaxCols := TGridCell(Key).Col + V + 1;

  if (FMaxRows <> LMaxRows) or (FMaxCols <> LMaxCols) then
    DoChange;
  DoItemChangeEx(Result, TGridCell(Key).Col, TGridCell(Key).Row);
end;

function TGridColumns.GetWidth: Double;
var
  DH, V: Double;
  I, J: Integer;
  Item: TGridColumnItem;
begin
  if FLastWidth >= 0 then begin
    Result := FLastWidth;
    Exit;
  end;
  if Assigned(FGridView) then
    DH := FGridView.GetDividerHeight
  else
    DH := 1;

  Result := 0;
  for J := 0 to RowsCount - 1 do begin
    I := 0;
    V := 0;
    while I < ColsCount do begin
      Item := Items[I, J];
      if not Item.Visible then begin
        Inc(I);
        Continue;
      end;
      V := V + Item.Width + DH;
      if Item.ColsPan > 1 then
        Inc(I, Item.ColsPan)
      else
        Inc(I);
    end;
    Result := Max(V, Result);
  end;
  FLastWidth := Result;
end;

procedure TGridColumns.RegisterColumnClass(
  const AColumnClass: TGridColumnItemClass);
begin
  FColumnClass := AColumnClass;
end;

procedure TGridColumns.SetItem(const ACol, ARow: Integer;
  const Value: TGridColumnItem);
var
  Key: Int64;
  V: Integer;
begin
  Key := TGridBase.GetKey(ACol, ARow);
  if FData.ContainsKey(Key) then begin
    if Value = nil then begin
      FData.Remove(Key);
      Exit;
    end else if FData[Key].AsPointer <> Value then
      FData.AddOrUpdate(Key, Value);
  end else
    FData.Add(Key, Value);

  // ���¼������������
  V := Max(Value.RowsPan - 1, 0);
  if ARow + V >= FMaxRows then
    RowsCount := ARow + V + 1;

  V := Max(Value.ColsPan - 1, 0);
  if ACol + V >= FMaxCols then
    ColsCount := ACol + V + 1;

  FLastWidth := -1;
  DoItemChangeEx(Value, ACol, ARow);
end;

procedure TGridColumns.SetMaxCols(const Value: Integer);
begin
  if FMaxCols <> Value then begin
    FMaxCols := Value;
    DoChange;
  end;
end;

procedure TGridColumns.SetMaxRows(const Value: Integer);
begin
  if FMaxRows <> Value then begin
    FMaxRows := Value;
    DoChange;
  end;
end;

{ TGridTextSettings }

constructor TGridTextSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Gravity := TLayoutGravity.CenterVertical;
  FColor := TAlphaColorRec.Black;
  FSelect := TAlphaColorRec.White;
  FEnabled := TAlphaColorRec.Gray;
  FOpacity := 1;
end;

destructor TGridTextSettings.Destroy;
begin
  inherited;
end;

function TGridTextSettings.GetStateColor(const State: TViewState): TAlphaColor;
begin
  case State of
    TViewState.Pressed, TViewState.Selected, TViewState.Checked: Result := FSelect;
    TViewState.Enabled: Result := FEnabled;
    TViewState.Custom:
      begin
        if FCustomColor = 0 then
          Result := FColor
        else
          Result := FCustomColor;
      end
  else
    Result := FColor;
  end;
end;

function TGridTextSettings.IsStoredGravity: Boolean;
begin
  Result := Gravity <> TLayoutGravity.CenterVertical;
end;

function TGridTextSettings.IsStoreOpacity: Boolean;
begin
  Result := FOpacity < 1;
end;

procedure TGridTextSettings.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    DoColorChanged(Self);
  end;
end;

procedure TGridTextSettings.SetEnabledColor(const Value: TAlphaColor);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    DoColorChanged(Self);
  end;
end;

procedure TGridTextSettings.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then begin
    FOpacity := Value;
    DoColorChanged(Self);
  end;
end;

procedure TGridTextSettings.SetSelectColor(const Value: TAlphaColor);
begin
  if FSelect <> Value then begin
    FSelect := Value;
    DoColorChanged(Self);
  end;
end;

{ TGridViewDrawState }

function TGridViewDrawState.GetIsUp: Boolean;
begin
  Result := Self.MoveSpace < 0;
end;

{ TGridViewBrush }

function TGridViewBrush.GetValue(const Index: Integer): TViewBrush;
begin
  Result := inherited GetBrush(TViewState(Index),
    not (csLoading in FView.GetComponentState)) as TViewBrush;
end;

procedure TGridViewBrush.SetValue(const Index: Integer;
  const Value: TViewBrush);
begin
  inherited SetValue(Index, Value);
end;

{ TGridCell }

procedure TGridCell.Clear;
begin
  Row := -1;
  Col := -1;
end;

{ TStringGridAdapter }

procedure TStringGridAdapter.Clear;
begin
  inherited;
  if FData.Count > 0 then
    FData.Clear;
end;

destructor TStringGridAdapter.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TStringGridAdapter.DoInitData;
begin
  inherited;
  FData := TDictionary<Int64, string>.Create();
end;

function TStringGridAdapter.GetCellData(const ACol, ARow: Integer): Pointer;
begin
  Result := PChar(Cells[ACol, ARow]);
end;

function TStringGridAdapter.GetCells(const ACol, ARow: Integer): string;
begin
  if not FData.TryGetValue(TGridBase.GetKey(ACol, ARow), Result) then
    Result := '';
end;

function TStringGridAdapter.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

procedure TStringGridAdapter.SetCellData(const ACol, ARow: Integer;
  const Value: Pointer);
begin
  Cells[ACol, ARow] := PChar(Value);
end;

procedure TStringGridAdapter.SetCells(const ACol, ARow: Integer;
  const Value: string);
begin
  FData.AddOrSetValue(TGridBase.GetKey(ACol, ARow), Value);
end;

procedure TStringGridAdapter.SetRowCount(const Value: Integer);
begin
  FRowCount := Value;
end;

{ TStringGridView }

constructor TStringGridView.Create(AOwner: TComponent);
var
  FAdapter: TStringGridAdapter;
begin
  inherited Create(AOwner);

  FColumns.ColsCount := 5;
  FAdapter := TStringGridAdapter.Create;
  FAdapter.RowCount := 5;
  Adapter := FAdapter;
end;

destructor TStringGridView.Destroy;
begin
  inherited;
end;

function TStringGridView.GetShowColIndex: Boolean;
begin
  Result := FColumns.FShowColIndex;
end;

procedure TStringGridView.SetColCount(const Value: Integer);
begin
  if FColumns.ColsCount <> Value then begin
    FColumns.ColsCount := Value;
    if not (csLoading in ComponentState) then
      NotifyDataChanged;
  end;
end;

procedure TStringGridView.SetRowCount(const Value: Integer);
begin
  if FAdapter.RowCount <> Value then begin
    FAdapter.RowCount := Value;
    NotifyDataChanged;
  end;
end;

procedure TStringGridView.SetShowColIndex(const Value: Boolean);
begin
  if FColumns.FShowColIndex <> Value then begin
    FColumns.FShowColIndex := Value;
    Invalidate;
  end;
end;

{ TGridCellSettings }

procedure TGridCellSettings.Assign(Source: TPersistent);
begin
  if Source is TGridCellSettings then begin
    Self.BgColor := TGridCellSettings(Source).BgColor;
    Self.TextColor := TGridCellSettings(Source).TextColor;
    Self.Gravity := TGridCellSettings(Source).Gravity;
    Self.TextStyle := TGridCellSettings(Source).TextStyle;
    Self.Locked := TGridCellSettings(Source).Locked;
    Self.ReadOnly := TGridCellSettings(Source).ReadOnly;
    Self.Enabled := TGridCellSettings(Source).Enabled;
  end else
    inherited;
end;

constructor TGridCellSettings.Create;
begin
  Enabled := True;
  Gravity := TLayoutGravity.None;
end;

{ TGridDBColumnItem }

destructor TGridDBColumnItem.Destroy;
begin
  Field := nil;
  FieldName := '';
  inherited;
end;

function TGridDBColumnItem.GetDispLayText: string;
begin
  if Title <> '' then
    Result := Title
  else if Assigned(Field) and (Field.DisplayLabel <> '') then
    Result := Field.DisplayLabel
  else
    Result := FieldName;
end;

{ TDBGridView }

function TDBGridView.AddCheckField(const FieldName, DisplayText: string;
  const ADisplayColWidth: Single; const ReadOnly: Boolean): TGridDBColumnItem;
begin
  FUseCustomColumns := True;
  Result := TGridDBColumnItem(FColumns[FColumns.ColsCount, 0]);
  Result.FieldName := FieldName;
  Result.Field := nil;
  Result.FWidth := ADisplayColWidth;
  Result.Visible := True;
  Result.ReadOnly := ReadOnly;
  Result.DataType := TGridDataType.CheckBox;
  Result.Title := DisplayText;
  if (FieldName <> '') and FDataLink.Active then
    REsult.Field := FDataLink.DataSet.FindField(FieldName);
end;

function TDBGridView.AddField(const FieldName, DisplayText: string;
  const ADisplayColWidth: Single; const Visible,
  ReadOnly: Boolean): TGridDBColumnItem;
begin
  FUseCustomColumns := True;
  Result := TGridDBColumnItem(FColumns[FColumns.ColsCount, 0]);
  Result.Title := DisplayText;
  Result.FieldName := FieldName;
  Result.Field := nil;
  Result.FWidth := ADisplayColWidth;
  Result.Visible := Visible;
  Result.ReadOnly := ReadOnly;
  if (FieldName <> '') and FDataLink.Active then
    REsult.Field := FDataLink.DataSet.FindField(FieldName);
end;

procedure TDBGridView.BeginUpdate;
begin
  inherited;
  FColumns.OnChange := nil;
  FUpdateing := True;
end;

procedure TDBGridView.ClearColumns;
begin
  FUseCustomColumns := False;
  FColumns.Clear;
end;

constructor TDBGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFilterDataList := TDictionary<Integer, string>.Create;
  FDataLink := TDBGridDataLink.Create(Self);

  FOptions := CDefaultDBOptions;

  FColumns.FColumnClass := TGridDBColumnItem;
  FColumns.ColsCount := 0;

  Adapter := TDBGridAdapter.Create;
end;

procedure TDBGridView.DataChanged;
begin
  if not TDBGridAdapter(FAdapter).FDrawCelling then
    UpdateRowCount(FDataLink.DataSet);
end;

destructor TDBGridView.Destroy;
begin
  FreeAndNil(FDataLink);
  FreeAndNil(FFilterDataList);
  inherited;
end;

procedure TDBGridView.DoFilterDataChange(Item: TGridColumnItem);
var
  I: Integer;
  DataSet: TDataSet;
  FilterValue: TStringBuilder;
begin
  if Assigned(Item) and (Item.FilterText = '') and (FFilterDataList.ContainsKey(Item.ColIndex)) then
    FFilterDataList.Remove(Item.ColIndex);

  DataSet := FDataLink.DataSet;
  try
    if (DataSet = nil) or (not DataSet.Active) then
      Exit;

    FilterValue := TStringBuilder.Create;

    // �Զ�����ͷ
    for I := 0 to FColumns.ColsCount - 1 do begin
      with TGridDBColumnItem(FColumns[I, 0]) do begin

        if (not DataFilter) or (Field = nil) or (FilterText = '') then
          Continue;

        if FilterValue.Length > 0 then
          FilterValue.Append(' and ');
        FilterValue.Append(Field.FieldName).Append('=''').Append(FilterText).Append(''' ');
      end;
    end;

  finally
    if FilterValue.Length > 0 then begin
      Dataset.Filter := FilterValue.ToString;
      DataSet.Filtered := True;
    end else begin
      Dataset.Filtered := False;
      Dataset.Filter := '';
    end;
    FreeAndNil(FilterValue);
  end;
end;

procedure TDBGridView.DoInitFilterDataList(Item: TGridColumnItem;
  List: TStrings);
var
  Map: TDictionary<string, Byte>;
  Field: TField;
  DataSet: TDataSet;
  Data: string;
begin
  if (not Assigned(FDataLink)) or (not FDataLink.Active) or (not Assigned(FDataLink.DataSet)) then
    Exit;
  Field := TGridDBColumnItem(Item).Field;
  if not Assigned(Field) then
    Exit;

  if FFilterDataList.TryGetValue(Item.ColIndex, Data) then begin
    List.Text := Data;
    Exit;
  end;

  Dataset := FDataLink.DataSet;
  Map := TDictionary<string, Byte>.Create();
  Dataset.DisableControls;
  try
    Dataset.First;
    while not DataSet.Eof do begin
      Data := Field.AsString;
      if (Data <> '') and (not Map.ContainsKey(LowerCase(Data))) then begin
        Map.Add(LowerCase(Data), 1);
        List.Add(Data)
      end;
      Dataset.Next;
    end;
  finally
    FreeAndNil(Map);
    FFilterDataList.AddOrSetValue(Item.ColIndex, List.Text);
    Dataset.EnableControls;
  end;
end;

procedure TDBGridView.EditingChanged;
begin
  if gvIndicator in Options then Invalidate;
end;

procedure TDBGridView.EndUpdate;
begin
  FColumns.OnChange := DoColumnsChange;
  DoColumnsChange(FColumns);
  FUpdateing := False;
  inherited;
end;

function TDBGridView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBGridView.GetFieldDisplayWidth(Field: TField): Single;
var
  LTitle: string;
begin
  Result := Field.DisplayWidth;
  if Assigned(Scene) then begin
    LTitle := Field.DisplayLabel;
    if LTitle = '' then
      LTitle := Field.FieldName;
    if LTitle <> '' then
      Result := Max(Result, FFixedText.CalcTextWidth(LTitle, Scene.GetSceneScale));
    if IsBlob(Field.DataType) then
      Result := Max(Result, 45);
  end;
end;

function TDBGridView.GetMinRowCount: Integer;
begin
  Result := TDBGridAdapter(FAdapter).FMinRowCount;
end;

function TDBGridView.GetRecordCount: Integer;
begin
  if FDataLink.Active and Assigned(FDataLink.DataSet) then
    Result := FDataLink.DataSet.RecordCount
  else
    Result := 0;
end;

function TDBGridView.GetSelectedField: TField;
begin
  with FContentViews.FSelectCell do begin
    if (Row >= 0) and (Col >= 0) and (Row < FDataRecordCount) then
      Result := TGridDBColumnItem(FColumns.ItemCols[Col]).Field
    else
      Result := nil;
  end;
end;

procedure TDBGridView.InitColumns(const DataSet: TDataSet);
var
  I, J: Integer;
begin
  if TDBGridAdapter(FAdapter).FFieldMap.Count > 0 then
    TDBGridAdapter(FAdapter).FFieldMap.Clear;
  BeginUpdate;

  if (not FUseCustomColumns) then
    FColumns.Clear;

  try
    if (DataSet = nil) or (not DataSet.Active) then begin
      FContentViews.InitColumnList;
      Exit;
    end;

    FUseCustomColumns := FUseCustomColumns or (FColumns.ColsCount > 0);

    if not FUseCustomColumns then begin
      // ��������Զ�����ͷ�������ȫ�����ݼ����ֶ���Ϣ
      FColumns.Clear;

      if FShowCheck then begin
        with TGridDBColumnItem(FColumns[0, 0]) do begin
          DataType := TGridDataType.CheckBox;
          FWidth := 35;
          FieldName := '';
          Field := nil;
          Title := 'ѡ��';
        end;
        J := 1;
      end else
        J := 0;

      for I := 0 to DataSet.FieldCount - 1 do begin
        with TGridDBColumnItem(FColumns[I + J, 0]) do begin
          Field := DataSet.Fields[I];
          FieldName := Field.FieldName;
          FieldType := Field.DataType;
          IsBLOB := Field.IsBlob;

          ReadOnly := Field.ReadOnly or IsBLOB;
          Visible := Field.Visible;
          FWidth := GetFieldDisplayWidth(Field) + Padding.Left + Padding.Right;
          WordWrap := False;
          Locked := False;

          DataType := TGridDataType.PlanText;
        
          case FieldType of
            ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftLongWord, ftShortint, ftByte, 
            TFieldType.ftExtended, TFieldType.ftSingle, ftAutoInc, ftBCD:
              Gravity := TLayoutGravity.CenterVRight;
          end;

          TDBGridAdapter(FAdapter).FFieldMap.Add(I + J, Field);
        end;
      end; 

    end else begin

      // �Զ�����ͷ
      for I := 0 to FColumns.ColsCount - 1 do begin
        with TGridDBColumnItem(FColumns[I, 0]) do begin

          Field := DataSet.FindField(FieldName);
          FilterText := '';

          if Assigned(Field) then begin
            TDBGridAdapter(FAdapter).FFieldMap.Add(I, Field); 
          
            IsBLOB := Field.IsBlob;
            FieldType := Field.DataType;

            if FWidth = -1 then begin
              FWidth := GetFieldDisplayWidth(Field) + Padding.Left + Padding.Right;
            end;

            if (DataType = TGridDataType.PlanText) and (Gravity = TLayoutGravity.None) then begin
              case FieldType of
                ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftLongWord, ftShortint, ftByte, 
                TFieldType.ftExtended, TFieldType.ftSingle, ftAutoInc, ftBCD:
                  Gravity := TLayoutGravity.CenterVRight;
              end;
            end;
          
          end else begin
            IsBLOB := False;
            ReadOnly := True;
            Locked := True;

            if FWidth = -1 then
              FWidth := 20;
          end;
        end;
      end;

    end;

    Dataset.Filtered := False;
    Dataset.Filter := '';
  finally
    EndUpdate;
  end;   
end;

class function TDBGridView.IsBlob(const DataType: TFieldType): Boolean;
begin
  Result := DataType in [ftBlob, ftBytes, ftVarBytes, ftGraphic];
end;

procedure TDBGridView.LinkActive(Value: Boolean);
begin
  if not Value then FContentViews.DoEditCancel;
  FFilterDataList.Clear;
  InitColumns(FDataLink.DataSet);
  UpdateRowCount(FDataLink.DataSet);
  if Value and (gvAlwaysShowEditor in Options) then FContentViews.DoEnter;
end;

procedure TDBGridView.Post;
begin
  if FDataLink.Active and FDataLink.Editing then begin
    FDataLink.Modified;
    FDataLink.DataSet.CheckBrowseMode;
    FDataLink.DataSet.Refresh;
  end;
end;

procedure TDBGridView.RecordChanged(Field: TField);
begin
  if not Assigned(Scene) then Exit;
  FContentViews.DoEditCancel;
  Invalidate;
end;

procedure TDBGridView.Reset;
begin
  if FDataLink.Active then begin
    FDataLink.Reset;
    FDataLink.DataSet.Refresh;
  end;
end;

procedure TDBGridView.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBGridView.SetMinRowCount(const Value: Integer);
begin
  if TDBGridAdapter(FAdapter).FMinRowCount <> Value then begin
    TDBGridAdapter(FAdapter).FMinRowCount := Value;
    UpdateRowCount(FDataLink.DataSet);
  end;
end;

procedure TDBGridView.SetShowCheck(const Value: Boolean);
begin
  if FShowCheck <> Value then begin
    FShowCheck := Value;
    InitColumns(FDataLink.DataSet);
    UpdateRowCount(FDataLink.DataSet);
  end;
end;

procedure TDBGridView.UpdateData;
var
  Field: TField;
begin
  Field := SelectedField;
  if Assigned(Field) and (Field.Text <> FContentViews.FEditText) then
    Field.Text := FContentViews.FEditText;
end;

procedure TDBGridView.UpdateRowCount(const DataSet: TDataSet);
var
  LRowCount: Integer;
begin
  LRowCount := TDBGridAdapter(FAdapter).RowCount;
  if Assigned(DataSet) and DataSet.Active then
    TDBGridAdapter(FAdapter).RowCount := DataSet.RecordCount
  else
    TDBGridAdapter(FAdapter).RowCount := 0;
  FDataRecordCount := TDBGridAdapter(FAdapter).RowCount;
  if LRowCount <> TDBGridAdapter(FAdapter).RowCount then begin
    FContentViews.FSelectCell.Clear;
    NotifyDataChanged;
  end else
    Invalidate;
end;

{ TDBGridAdapter }

type
  TMDataSet = class(TDataSet);

procedure TDBGridAdapter.BeginDrawCells(const AFirstRow, ALastRow: Integer);
begin
  FDrawCelling := True;
end;

procedure TDBGridAdapter.Clear;
begin
  inherited Clear;
  FRowCount := FMinRowCount;
end;

constructor TDBGridAdapter.Create;
begin
  FEditMap := TList<TGridCell>.Create;
  FFieldMap := TIntHash.Create;
  FData := TDictionary<Int64, string>.Create();
  inherited;
end;

destructor TDBGridAdapter.Destroy;
begin
  FreeAndNil(FFieldMap);
  FreeAndNil(FEditMap);
  FreeAndNil(FData);
  inherited;
end;

procedure TDBGridAdapter.EndDrawCells;
begin
  if TDBGridView(GridView).FDataLink.Active then
    SetCursor(GridView.SelectionAnchor);
  FDrawCelling := False;
end;

function TDBGridAdapter.GetCellData(const ACol, ARow: Integer): Pointer;
begin
  Result := nil;
end;

function TDBGridAdapter.GetCells(const ACol, ARow: Integer): string;
var
  F: TField;
begin
  if (FIsOutDataRow) then begin
    Result := '';
    Exit;
  end;
  if FFieldMap.TryGetValue(ACol, TObject(F)) and Assigned(F) then begin
    if TDBGridView(GridView).FDataLink.Active then begin
      if TDBGridView.IsBlob(F.DataType) then
        Result := '{BLOB}'
      else
        Result := F.AsString; //.DisplayText;
    end else
      Result := '';
  end else
    FData.TryGetValue(TGridBase.GetKey(ACol, ARow), Result);
end;

function TDBGridAdapter.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

procedure TDBGridAdapter.PostData;
begin
end;

procedure TDBGridAdapter.SetCellData(const ACol, ARow: Integer;
  const Value: Pointer);
begin 
end;

procedure TDBGridAdapter.SetCells(const ACol, ARow: Integer;
  const Value: string);
var
  F: TField;
begin
  if FIsOutDataRow then
    Exit;
  if FFieldMap.TryGetValue(ACol, TObject(F)) and Assigned(F) then begin
    if F.AsString <> Value then begin
      if not TDBGridView(GridView).FDataLink.Editing then
        TDBGridView(GridView).FDataLink.Edit;
      F.Text := Value;
    end;
  end else
    FData.AddOrSetValue(TGridBase.GetKey(ACol, ARow), Value);
end;

procedure TDBGridAdapter.SetCursor(const ARow: Integer);
begin
  if (TDBGridView(GridView).FDataLink.Active) then begin
    FIsOutDataRow := ARow >= TDBGridView(GridView).FDataRecordCount;
    if not FIsOutDataRow then begin
      FDrawCelling := True;
      with TDBGridView(GridView).FDataLink do
        DataSet.MoveBy(ARow - DataSet.RecNo + 1);
      FDrawCelling := False;
    end;
  end else
    FIsOutDataRow := True;
end;

procedure TDBGridAdapter.SetRowCount(const Value: Integer);
begin  
  if Value < FMinRowCount then
    FRowCount := FMinRowCount
  else
    FRowCount := Value;
end;

{ TDBGridDataLink }

procedure TDBGridDataLink.ActiveChanged;
begin
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
        DatabaseError(SDataSetUnidirectional);
  FGrid.LinkActive(Active);
  FModified := False;
end;

constructor TDBGridDataLink.Create(AGrid: TDBGridView);
begin
  FGrid := AGrid;
end;

procedure TDBGridDataLink.DataSetChanged;
begin
  FGrid.DataChanged;
  FModified := False;
end;

destructor TDBGridDataLink.Destroy;
begin
  inherited;
end;

procedure TDBGridDataLink.EditingChanged;
begin
  FGrid.EditingChanged;
end;

function TDBGridDataLink.GetFields(I: Integer): TField;
begin
  if (0 <= I) and (I < DataSet.FieldCount) then
    Result := TGridDBColumnItem(FGrid.Columns.ItemCols[I]).Field
  else
    Result := nil;
end;

procedure TDBGridDataLink.Modified;
begin
  FModified := True;
end;

procedure TDBGridDataLink.RecordChanged(Field: TField);
begin
  if FModified and Assigned(Field) and (Field.FieldKind = fkData) and
     (FGrid.SelectedField <> Field) and not FInUpdateData then
    UpdateData;
  FGrid.RecordChanged(Field);
  FModified := False;
end;

procedure TDBGridDataLink.Reset;
begin
  if FModified then RecordChanged(nil) else Dataset.Cancel;
end;

procedure TDBGridDataLink.UpdateData;
begin
  FInUpdateData := True;
  try
    if FModified then FGrid.UpdateData;
    FModified := False;
  finally
    FInUpdateData := False;
  end;
end;

{ TGridFixedSetting }

constructor TGridFixedSetting.Create(AOwner: TGridBase);
begin
  FOwner := AOwner;
end;

function TGridFixedSetting.GetFixedBrush: TGridViewBrush;
begin
  Result := FOwner.FixedBrush;
end;

function TGridFixedSetting.GetFixedCols: Integer;
begin
  Result := FOwner.FixedCols;
end;

function TGridFixedSetting.GetFixedDefaultColWidth: Single;
begin
  Result := FOwner.FixedColWidth;
end;

function TGridFixedSetting.GetFixedDivider: TAlphaColor;
begin
  Result := FOwner.FixedDivider;
end;

function TGridFixedSetting.GetFixedRowHeight: Single;
begin
  Result := FOwner.FixedRowHeight;
end;

function TGridFixedSetting.GetFixedRows: Integer;
begin
  Result := FOwner.FixedRows;
end;

function TGridFixedSetting.GetFixedText: TGridTextSettings;
begin
  Result := FOwner.FixedTextSettings;
end;

function TGridFixedSetting.GetTextRowIndex: string;
begin
  Result := FOwner.FixedTextRowIndex;
end;

procedure TGridFixedSetting.SetFixedBrush(const Value: TGridViewBrush);
begin
  FOwner.FixedBrush := Value;
end;

procedure TGridFixedSetting.SetFixedCols(const Value: Integer);
begin
  FOwner.FixedCols := Value;
end;

procedure TGridFixedSetting.SetFixedDefaultColWidth(const Value: Single);
begin
  FOwner.FixedColWidth := Value;
end;

procedure TGridFixedSetting.SetFixedDivider(const Value: TAlphaColor);
begin
  FOwner.FixedDivider := Value;
end;

procedure TGridFixedSetting.SetFixedRowHeight(const Value: Single);
begin
  FOwner.FixedRowHeight := Value;
end;

procedure TGridFixedSetting.SetFixedRows(const Value: Integer);
begin
  FOwner.FixedRows := Value;
end;

procedure TGridFixedSetting.SetFixedText(const Value: TGridTextSettings);
begin
  FOwner.FixedTextSettings := Value;
end;

procedure TGridFixedSetting.SetTextRowIndex(const Value: string);
begin
  FOwner.FixedTextRowIndex := Value;
end;

{ TGridColumnsSetting }

constructor TGridColumnsSetting.Create(AOwner: TGridBase);
begin
  FOwner := AOwner;
end;

{ TGridFilterDownListAdapter }

procedure TGridFilterDownListAdapter.DoItemIndexChange(Sender: TObject);
begin
  FItemIndex := TControl(Sender).Tag;
end;

function TGridFilterDownListAdapter.GetView(const Index: Integer;
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
    ViewItem.TextSettings.Color.Checked := $ff0000ff;
    ViewItem.TextSettings.WordWrap := FWordWrap;
    ViewItem.Gravity := TLayoutGravity.CenterVertical;
    ViewItem.Background.ItemChecked.Kind := TViewBrushKind.Solid;
    ViewItem.Background.ItemChecked.Color := $3f0099ff;
    ViewItem.Padding.Rect := RectF(8, 2, 8, 2);
    ViewItem.CanFocus := False;
  end else
    ViewItem := ConvertView as TListTextItem;
  ViewItem.HeightSize := TViewSize.WrapContent;
  ViewItem.Text := Items[Index];
  if Index = FItemIndex then
    ViewItem.Checked := True
  else
    ViewItem.Checked := False;
  ViewItem.Tag := Index; // ʹ�� Tag ��¼������
  ViewItem.OnClick := DoItemIndexChange;
  Result := ViewItem;
end;

initialization
  FGridRes := TGridRes.Create(nil);

finalization
  FreeAndNil(FGridRes);

end.