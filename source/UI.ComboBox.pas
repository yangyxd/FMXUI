unit UI.ComboBox;

interface

uses
  UI.Base, UI.Utils, UI.Ani, UI.ListView, UI.Standard, UI.Edit,
  FMX.Effects, FMX.Text,
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
  {$IF CompilerVersion > 30.0}
  FMX.AcceleratorKey,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows, FMX.Platform.Win,
  {$ENDIF}
  FMX.BehaviorManager, FMX.Forms, System.Messaging, FMX.Styles, FMX.Pickers,
  FMX.Media,
  FMX.ActnList, FMX.Objects, System.Math, System.Actions, System.Rtti, FMX.Consts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, FMX.TextLayout,
  System.SyncObjs,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement,
  FMX.Ani, FMX.StdActns;

type
  TOnInitListAdapter = function (Sender: TObject): TStringsListAdapter of object;
  TOnMultipleCheckedChange = procedure (Sender: TObject; ItemIndex: Integer; var Checked: Boolean) of object;
  TOnMultipleCheckedText = function (Sender: TObject): string of object;

type
  TMultipleStringsListAdapter = class(TStringsListCheckAdapter)
  private
    FOwner: TComponent;
  protected
    procedure DoCheckChange(Sender: TObject);
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
  public
    constructor Create(Owner: TComponent; const AItems: TStrings); overload;
    constructor Create(Owner: TComponent; const AItems: TArray<string>); overload;
  end;

type
  TComboBoxDrawableIcon = class(TDrawableIcon)
  published
    property Padding default 2;
    property Position default TDrawablePosition.Right;
  end;

type
  TCustomDownPopup = class
  private
    [Weak] FOwner: TView;
    FItems: TStrings;
    FOldItemIndex: Integer;
    FItemIndex: Integer;
    FDropDownCount: Integer;
    FOnChange: TNotifyEvent;
    FOnPopup: TNotifyEvent;
    FOnClosePopup: TNotifyEvent;
    FOnMultipleCheckedChange: TOnMultipleCheckedChange;
    FItemWidth: Single;
    FItemHeight: Single;
    FMultiple: Boolean;
    FPopup: TPopup;
    FListBox: TListViewEx;
    FDropDownKind: TDropDownKind;
    FDroppedDown: Boolean;
    FListPicker: TCustomListPicker;
    FMultipleTextSeparator: string;
    FDropDownButton: TDrawableIcon;
    FCanUseListPicker: Boolean;
    FListBackground: TViewBrush;
    FListTextColor: TViewColor;
    FListItemCheckedColor: TAlphaColor;
    FListItemHoveredColor: TAlphaColor;
    FOnInitListAdapter: TOnInitListAdapter;
    function GetCount: Integer;
    function GetItems: TStrings;
    function IsItemHeightStored: Boolean;
    function ItemsStored: Boolean;
    procedure SetDropDownCount(const Value: Integer);
    procedure SetItemHeight(const Value: Single);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetItemWidth(const Value: Single);
    procedure SetListBackground(const Value: TViewBrush);
    procedure SetListTextColor(const Value: TViewColor);
    function IsListItemCheckedColorStored: Boolean;
    function IsListItemHoveredColorStored: Boolean;
    procedure SetMultiple(const Value: Boolean);
    function GetListItemCheck(const Index: Integer): Boolean;
    procedure SetListItemCheck(const Index: Integer; const Value: Boolean);
    function GetMultipleText: string;
    procedure SetDropDownButton(const Value: TDrawableIcon);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); virtual;
    function KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState; out OldItemIndex, NewItemIndex: Integer): Boolean; virtual;
    function KeyDownHandle(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState; var OldItemIndex, NewItemIndex: Integer): Boolean; virtual;
    procedure DoListItemClick(Sender: TObject; ItemIndex: Integer; const ItemView: TControl); virtual;
    procedure DoCheckedChange(Sender: TObject; ItemIndex: Integer; const ItemView: TControl); virtual;
    procedure DoItemMeasureHeight(Sender: TObject; Index: Integer; var AHeight: Single); virtual;
    procedure DoChange; dynamic;
    procedure DoItemsChange(Sender: TObject); virtual;
    procedure DoPopup(Sender: TObject);
    procedure DoClosePopup(Sender: TObject);
    procedure DoClosePicker(Sender: TObject);
    procedure DoDropDownButtonChanged(Sender: TObject);
    procedure DoOnValueChangedFromDropDownList(Sender: TObject; const AValueIndex: Integer);
    function UseNativePicker: Boolean;
    procedure InitPicker(AListPicker: TCustomListPicker); virtual;
    procedure RecalculatePopupSize; virtual;
    procedure DoPaintBackground(Canvas: TCanvas; var R: TRectF; DrawState: TViewState); virtual;
    function CreateListBox(): TListViewEx; virtual;
    function GetListAdapter(): TStringsListAdapter;
    function CreateDropDownButton: TDrawableIcon; virtual;
  public
    constructor Create(AOwner: TView); virtual;
    destructor Destroy; override;
    procedure AddItem(const Item: String; AObject: TObject); virtual;
    procedure Clear; virtual;
    procedure ClearSelection; virtual;
    procedure DeleteSelected; virtual;
    procedure DropDown; virtual;
    property ListBox: TListViewEx read FListBox;
    property ListBackground: TViewBrush read FListBackground write SetListBackground;
    property ListTextColor: TViewColor read FListTextColor write SetListTextColor;
    property ListItemCheckedColor: TAlphaColor read FListItemCheckedColor write FListItemCheckedColor stored IsListItemCheckedColorStored;
    property ListItemHoveredColor: TAlphaColor read FListItemHoveredColor write FListItemHoveredColor stored IsListItemHoveredColorStored;
    property Popup: TPopup read FPopup;
    property Items: TStrings read GetItems write SetItems stored ItemsStored;
    property ItemsChecked[const Index: Integer]: Boolean read GetListItemCheck write SetListItemCheck;
    property Count: Integer read GetCount;
    property CanUseListPicker: Boolean read FCanUseListPicker write FCanUseListPicker default False;
    property DropDownButton: TDrawableIcon read FDropDownButton write SetDropDownButton;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemWidth: Single read FItemWidth write SetItemWidth;
    property ItemHeight: Single read FItemHeight write SetItemHeight stored IsItemHeightStored;
    property Multiple: Boolean read FMultiple write SetMultiple default False;
    property MultipleText: string read GetMultipleText;
    property MultipleTextSeparator: string read FMultipleTextSeparator write FMultipleTextSeparator;
    property DropDownKind: TDropDownKind read FDropDownKind write FDropDownKind default TDropDownKind.Native;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount default 8;
    property DroppedDown: Boolean read FDroppedDown;
    property OnItemChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property OnInitListAdapter: TOnInitListAdapter read FOnInitListAdapter write FOnInitListAdapter;
    property OnMultipleCheckedChange: TOnMultipleCheckedChange read FOnMultipleCheckedChange write FOnMultipleCheckedChange;
  end;

type
  TCustomComboBoxView = class(TTextView)
  private
    FDownPopup: TCustomDownPopup;
    FOnItemChange: TNotifyEvent;
    FOnClosePopup: TNotifyEvent;
    FOnGetMultipleText: TOnMultipleCheckedText;
    FOnMultipleCheckedChange: TOnMultipleCheckedChange;
    function GetCanUseListPicker: Boolean;
    function GetCount: Integer;
    function GetDropDownButton: TDrawableIcon;
    function GetDropDownCount: Integer;
    function GetDropDownKind: TDropDownKind;
    function GetDroppedDown: Boolean;
    function GetItemHeight: Single;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetItemWidth: Single;
    function GetListBackground: TViewBrush;
    function GetListBox: TListViewEx;
    function GetListItemCheckedColor: TAlphaColor;
    function GetListItemHoveredColor: TAlphaColor;
    function GetListTextColor: TViewColor;
    function GetOnInitListAdapter: TOnInitListAdapter;
    function GetOnPopup: TNotifyEvent;
    function GetPopup: TPopup;
    function IsItemHeightStored: Boolean;
    function ItemsStored: Boolean;
    procedure SetDropDownCount(const Value: Integer);
    procedure SetItemHeight(const Value: Single);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetItemWidth(const Value: Single);
    procedure SetListBackground(const Value: TViewBrush);
    procedure SetListTextColor(const Value: TViewColor);
    procedure SetOnInitListAdapter(const Value: TOnInitListAdapter);
    procedure SetOnPopup(const Value: TNotifyEvent);
    procedure SetCanUseListPicker(const Value: Boolean);
    procedure SetDropDownKind(const Value: TDropDownKind);
    procedure SetListItemCheckedColor(const Value: TAlphaColor);
    procedure SetListItemHoveredColor(const Value: TAlphaColor);
    function IsListItemCheckedColorStored: Boolean;
    function IsListItemHoveredColorStored: Boolean;
    function GetListMultiple: Boolean;
    procedure SetListMultiple(const Value: Boolean);
    function GetListItemCheck(const Index: Integer): Boolean;
    procedure SetListItemCheck(const Index: Integer; const Value: Boolean);
    procedure SetDropDownButton(const Value: TDrawableIcon);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoPaintBackground(var R: TRectF); override;
    function CreateBackground: TDrawable; override;
    function GetDefaultSize: TSizeF; override;
    procedure SetName(const Value: TComponentName); override;
    procedure DoItemChange(Sender: TObject); virtual;
    procedure DoMultipleCheckedChange(Sender: TObject; ItemIndex: Integer; var Checked: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const Item: String; AObject: TObject); virtual;
    procedure Clear;
    procedure ClearSelection;
    procedure DeleteSelected;
    procedure DropDown;
    property ListBox: TListViewEx read GetListBox;
    property ListBackground: TViewBrush read GetListBackground write SetListBackground;
    property ListTextColor: TViewColor read GetListTextColor write SetListTextColor;
    property ListItemCheckedColor: TAlphaColor read GetListItemCheckedColor write SetListItemCheckedColor stored IsListItemCheckedColorStored;
    property ListItemHoveredColor: TAlphaColor read GetListItemHoveredColor write SetListItemHoveredColor stored IsListItemHoveredColorStored;
    property ListMultiple: Boolean read GetListMultiple write SetListMultiple default False;
    property Popup: TPopup read GetPopup;
    property CanFocus default True;
    property CanParentFocus;
    property Items: TStrings read GetItems write SetItems stored ItemsStored;
    property ItemsChecked[const Index: Integer]: Boolean read GetListItemCheck write SetListItemCheck;
    property Count: Integer read GetCount;
    property CanUseListPicker: Boolean read GetCanUseListPicker write SetCanUseListPicker default False;
    property DropDownButton: TDrawableIcon read GetDropDownButton write SetDropDownButton;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ItemWidth: Single read GetItemWidth write SetItemWidth;
    property ItemHeight: Single read GetItemHeight write SetItemHeight stored IsItemHeightStored;
    property DropDownKind: TDropDownKind read GetDropDownKind write SetDropDownKind default TDropDownKind.Native;
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount default 8;
    property DroppedDown: Boolean read GetDroppedDown;
    property Gravity default TLayoutGravity.CenterVertical;
    property OnChange: TNotifyEvent read FOnItemChange write FOnItemChange;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnPopup: TNotifyEvent read GetOnPopup write SetOnPopup;
    property OnInitListAdapter: TOnInitListAdapter read GetOnInitListAdapter write SetOnInitListAdapter;
    property OnMultipleCheckedChange: TOnMultipleCheckedChange read FOnMultipleCheckedChange write FOnMultipleCheckedChange;
    property OnMultipleCheckedText: TOnMultipleCheckedText read FOnGetMultipleText write FOnGetMultipleText;
  end;

type
  TCustomComboBoxEditView = class(TEditView)
  private
    FInDropDown: Boolean;
    FDownPopup: TCustomDownPopup;
    FOnItemChange: TNotifyEvent;
    FOnClosePopup: TNotifyEvent;
    FOnGetMultipleText: TOnMultipleCheckedText;
    FOnMultipleCheckedChange: TOnMultipleCheckedChange;
    function GetCanUseListPicker: Boolean;
    function GetCount: Integer;
    function GetDropDownButton: TDrawableIcon;
    function GetDropDownCount: Integer;
    function GetDropDownKind: TDropDownKind;
    function GetDroppedDown: Boolean;
    function GetItemHeight: Single;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetItemWidth: Single;
    function GetListBackground: TViewBrush;
    function GetListBox: TListViewEx;
    function GetListItemCheckedColor: TAlphaColor;
    function GetListItemHoveredColor: TAlphaColor;
    function GetListTextColor: TViewColor;
    function GetOnInitListAdapter: TOnInitListAdapter;
    function GetOnPopup: TNotifyEvent;
    function GetPopup: TPopup;
    function IsItemHeightStored: Boolean;
    function ItemsStored: Boolean;
    procedure SetDropDownCount(const Value: Integer);
    procedure SetItemHeight(const Value: Single);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetItemWidth(const Value: Single);
    procedure SetListBackground(const Value: TViewBrush);
    procedure SetListTextColor(const Value: TViewColor);
    procedure SetOnInitListAdapter(const Value: TOnInitListAdapter);
    procedure SetOnPopup(const Value: TNotifyEvent);
    procedure SetCanUseListPicker(const Value: Boolean);
    procedure SetDropDownKind(const Value: TDropDownKind);
    procedure SetListItemCheckedColor(const Value: TAlphaColor);
    procedure SetListItemHoveredColor(const Value: TAlphaColor);
    function IsListItemCheckedColorStored: Boolean;
    function IsListItemHoveredColorStored: Boolean;
    function GetListMultiple: Boolean;
    procedure SetListMultiple(const Value: Boolean);
    function GetListItemCheck(const Index: Integer): Boolean;
    procedure SetListItemCheck(const Index: Integer; const Value: Boolean);
    procedure SetDropDownButton(const Value: TDrawableIcon);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure SetText(const Value: string); override;
  protected
    procedure DoPaintBackground(var R: TRectF); override;
    procedure RealignDrawableContent(var ContentRect: TRectF); override;
    function GetDefaultSize: TSizeF; override;
    procedure DoItemChange(Sender: TObject); virtual;
    procedure DoMultipleCheckedChange(Sender: TObject; ItemIndex: Integer; var Checked: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const Item: String; AObject: TObject); virtual;
    procedure Clear;
    procedure ClearSelection;
    procedure DeleteSelected;
    procedure DropDown;
    property ListBox: TListViewEx read GetListBox;
    property ListBackground: TViewBrush read GetListBackground write SetListBackground;
    property ListTextColor: TViewColor read GetListTextColor write SetListTextColor;
    property ListItemCheckedColor: TAlphaColor read GetListItemCheckedColor write SetListItemCheckedColor stored IsListItemCheckedColorStored;
    property ListItemHoveredColor: TAlphaColor read GetListItemHoveredColor write SetListItemHoveredColor stored IsListItemHoveredColorStored;
    property ListMultiple: Boolean read GetListMultiple write SetListMultiple default False;
    property Popup: TPopup read GetPopup;
    property Items: TStrings read GetItems write SetItems stored ItemsStored;
    property ItemsChecked[const Index: Integer]: Boolean read GetListItemCheck write SetListItemCheck;
    property Count: Integer read GetCount;
    property CanUseListPicker: Boolean read GetCanUseListPicker write SetCanUseListPicker default False;
    property DropDownButton: TDrawableIcon read GetDropDownButton write SetDropDownButton;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ItemWidth: Single read GetItemWidth write SetItemWidth;
    property ItemHeight: Single read GetItemHeight write SetItemHeight stored IsItemHeightStored;
    property DropDownKind: TDropDownKind read GetDropDownKind write SetDropDownKind default TDropDownKind.Native;
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount default 8;
    property DroppedDown: Boolean read GetDroppedDown;
    property OnItemChange: TNotifyEvent read FOnItemChange write FOnItemChange;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnPopup: TNotifyEvent read GetOnPopup write SetOnPopup;
    property OnInitListAdapter: TOnInitListAdapter read GetOnInitListAdapter write SetOnInitListAdapter;
    property OnMultipleCheckedChange: TOnMultipleCheckedChange read FOnMultipleCheckedChange write FOnMultipleCheckedChange;
    property OnMultipleCheckedText: TOnMultipleCheckedText read FOnGetMultipleText write FOnGetMultipleText;
  end;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TComboBoxView = class(TCustomComboBoxView)
  published
    property CanFocus;
    property CanParentFocus;
    property Clickable default True;
    property Items;
    property ItemIndex default -1;
    property ItemHeight;
    property CanUseListPicker default False;
    property DropDownKind;
    property DropDownCount default 8;
    property DropDownButton;
    property ListBackground;
    property ListTextColor;
    property ListItemCheckedColor;
    property ListItemHoveredColor;
    property ListMultiple default False;

    property OnInitListAdapter;

    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;

    property OnChange;
    property OnClosePopup;
    property OnPopup;
    property OnMultipleCheckedChange;
    property OnMultipleCheckedText;

    property OnKeyDown;
    property OnKeyUp;

    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TComboBoxEditView = class(TCustomComboBoxEditView)
  published
    property Items;
    property ItemIndex default -1;
    property ItemHeight;
    property CanUseListPicker default False;
    property DropDownKind;
    property DropDownCount default 8;
    property DropDownButton;
    property ListBackground;
    property ListTextColor;
    property ListItemCheckedColor;
    property ListItemHoveredColor;
    property ListMultiple default False;

    property OnInitListAdapter;
    property OnItemChange;
    property OnClosePopup;
    property OnPopup;
    property OnMultipleCheckedChange;
    property OnMultipleCheckedText;
  end;

implementation

resourcestring
  SDefaultDownBtnSVG = '<svg t="1750404544438" class="icon" viewBox="0 0 1024 1024" version="1.1"'+
    ' xmlns="http://www.w3.org/2000/svg" p-id="8070" width="128" height="128">'+
    '<path d="M512 658.285714c-10.971429 0-21.942857-3.657143-29.257143-14.628571l-219.428571-223'+
    '.085714c-14.628571-14.628571-14.628571-36.571429 0-47.542858 7.314286-3.657143 18.285714-7.3'+
    '14286 25.6-7.314285 14.628571 0 21.942857 3.657143 29.257143 14.628571l193.828571 193.828572'+
    ' 193.828571-193.828572c3.657143-7.314286 14.628571-14.628571 29.257143-14.628571 10.971429 0'+
    ' 18.285714 3.657143 21.942857 7.314285 14.628571 14.628571 14.628571 36.571429 0 47.542858l-'+
    '219.428571 223.085714c-7.314286 10.971429-14.628571 14.628571-25.6 14.628571z" fill="" p-id="8071"/></svg>';

const
  D_ListItemCheckedColor: TAlphaColor = $ff409eff;
  D_ListItemHoveredColor: TAlphaColor = $2f409eff;

type
  TComboBoxHelper = class
  private
    class var FItems: TDictionary<Pointer, Boolean>;
  public
    class procedure Initialize;
    class procedure Uninitialize;
    class procedure Register(const AControl: TCustomDownPopup);
    class procedure Unregister(const AControl: TCustomDownPopup);
    class function AreItemsChanged(const AControl: TCustomDownPopup): Boolean;
    class procedure SetItemsChanged(const AControl: TCustomDownPopup; const AChanged: Boolean);
  end;

{ TComboBoxHelper }

class function TComboBoxHelper.AreItemsChanged(const AControl: TCustomDownPopup): Boolean;
begin
  Result := FItems.TryGetValue(AControl, Result) and Result;
end;

class procedure TComboBoxHelper.Initialize;
begin
  FItems := TDictionary<Pointer, Boolean>.Create;
end;

class procedure TComboBoxHelper.Uninitialize;
begin
  FreeAndNil(FItems);
end;

class procedure TComboBoxHelper.SetItemsChanged(const AControl: TCustomDownPopup; const AChanged: Boolean);
begin
  FItems.AddOrSetValue(AControl, AChanged);
end;

class procedure TComboBoxHelper.Register(const AControl: TCustomDownPopup);
begin
  FItems.Add(AControl, True);
end;

class procedure TComboBoxHelper.Unregister(const AControl: TCustomDownPopup);
begin
  FItems.Remove(AControl);
end;

type
  TControlEx = class(TControl);
  TScrollViewX = class(TScrollView);


{ TMultipleStringsListAdapter }

constructor TMultipleStringsListAdapter.Create(Owner: TComponent;
  const AItems: TArray<string>);
begin
  FOwner := Owner;
  inherited Create(AItems);
end;

constructor TMultipleStringsListAdapter.Create(Owner: TComponent;
  const AItems: TStrings);
begin
  FOwner := Owner;
  inherited Create(AItems);
end;

procedure TMultipleStringsListAdapter.DoCheckChange(Sender: TObject);
begin
  ItemCheck[TControl(Sender).Tag] := TCheckBoxView(Sender).IsChecked;
end;

function TMultipleStringsListAdapter.GetView(const Index: Integer;
  ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
var
  ViewItem: TCheckBoxView;
begin
  if (ConvertView = nil) or (not (ConvertView is TCheckBoxView)) then begin
    ViewItem := TCheckBoxView.Create(Parent);
    if Assigned(FOwner) and (FOwner is TTextStyleView) then
      ViewItem.StyleManager := TTextStyleView(FOwner).StyleManager;
    ViewItem.Parent := Parent;
    ViewItem.CanFocus := False;
  end else begin
    ViewItem := ConvertView as TCheckBoxView;
  end;
  ViewItem.BeginUpdate;
  ViewItem.Tag := Index;
  ViewItem.Width := Parent.Width;
  ViewItem.MinHeight := ItemDefaultHeight;
  ViewItem.TextSettings.Font.Size := FFontSize;
  ViewItem.TextSettings.WordWrap := FWordWrap;
  ViewItem.Padding.Rect := FPadding;
  if Assigned(FFontColor) then
    ViewItem.TextSettings.Color := FFontColor;
  ViewItem.HeightSize := FHeightSize;
  ViewItem.Background.ItemDefault.Color := TAlphaColorRec.Null;
  ViewItem.Background.ItemDefault.Kind := TViewBrushKind.None;
  ViewItem.Background.ItemChecked.Kind := TViewBrushKind.Solid;
  ViewItem.Background.ItemChecked.Color := FListItemCheckedColor;
  ViewItem.Background.ItemHovered.Kind := TViewBrushKind.Solid;
  ViewItem.Background.ItemHovered.Color := FListItemHoveredColor;
  if Assigned(ViewItem.StyleManager) then
    ViewItem.StyleType := TTextStyleView(FOwner).StyleType;
  ViewItem.OnChange := nil;
  ViewItem.IsChecked := ItemCheck[Index];
  ViewItem.OnChange := DoCheckChange;
  ViewItem.Text := Items[Index];
  ViewItem.EndUpdate;
  Result := ViewItem;
end;

{ TCustomDownPopup }

procedure TCustomDownPopup.AddItem(const Item: String; AObject: TObject);
begin
  FItems.AddObject(Item, AObject);
end;

procedure TCustomDownPopup.Clear;
begin
  if FItems.Count > 0 then FItems.Clear;
end;

procedure TCustomDownPopup.ClearSelection;
begin
  ItemIndex := -1;
end;

constructor TCustomDownPopup.Create(AOwner: TView);
var
  PickerService: IFMXPickerService;
begin
  FOwner := AOwner;
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, PickerService) then
  begin
    FListPicker := PickerService.CreateListPicker;
    FListPicker.Parent := AOwner;
    FListPicker.OnValueChanged := DoOnValueChangedFromDropDownList;
    FListPicker.OnHide := DoClosePicker;
    FListPicker.OnShow := DoPopup;
  end;
  FListItemCheckedColor := D_ListItemCheckedColor;
  FListItemHoveredColor := D_ListItemHoveredColor;
  FCanUseListPicker := False;
  FItemHeight := 18;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := DoItemsChange;
  FMultiple := False;
  FItemIndex := -1;
  FOldItemIndex := -1;
  FDropDownKind := TDropDownKind.Custom;
  DropDownCount := 8;
  FDroppedDown := False;
  FPopup := TPopup.Create(AOwner);
  FPopup.StyleLookup := 'combopopupstyle';
  FPopup.PlacementTarget := AOwner;
  FPopup.Stored := False;
  FPopup.Parent := AOwner;
  FPopup.Locked := True;
  FPopup.DragWithParent := True;
  FPopup.OnClosePopup := DoClosePopup;
  FPopup.OnPopup := DoPopup;
  FListTextColor := TViewColor.Create(TAlphaColorRec.Black);
  FListTextColor.Checked := TAlphaColorRec.White;
  FListTextColor.CheckedChange := False;
  FListBackground := TViewBrush.Create(TViewBrushKind.Solid, TAlphaColorRec.Null);
  FListBox := CreateListBox;
  if FListBox = nil then
    raise EArgumentNilException.CreateFmt(SResultCanNotBeNil, ['CreateListBox']);
  FListBox.Visible := False;
  FListBox.Parent := Popup;
  FListBox.Stored := False;
  FListBox.Align := TAlignLayout.Client;
  FItemIndex := -1;
  DropDownKind := TDropDownKind.Native;
  TComboBoxHelper.Register(Self);
  FDropDownButton := CreateDropDownButton;
  FMultipleTextSeparator := ';';
end;

function TCustomDownPopup.CreateDropDownButton: TDrawableIcon;
begin
  Result := TComboBoxDrawableIcon.Create(FOwner);
  Result.SizeWidth := 16;
  Result.SizeHeight := 16;
  Result.Padding := 2;
  Result.Position := TDrawablePosition.Right;
  TViewBrushBase(Result.ItemDefault).SVGImage.Parse(SDefaultDownBtnSVG, True);
  TViewBrushBase(Result.ItemDefault).SVGImage.Color := $ff000000;
  TViewBrushBase(Result.ItemDefault).SVGImage.DefaultColor := $ff000000;
  TViewBrushBase(Result.ItemDefault).Kind := TViewBrushKind.SVGImage;
  TViewBrushBase(Result.ItemDefault).DefaultKind := TBrushKind(Ord(TViewBrushKind.SVGImage));
  Result.OnChanged := DoDropDownButtonChanged;
end;

function TCustomDownPopup.CreateListBox: TListViewEx;
begin
  Result := TListViewEx.Create(FOwner);
  Result.Background.ItemDefault.Color := $ff909090;
  Result.Background.ItemDefault.DefaultColor := $ff909090;
  Result.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  Result.Adapter := GetListAdapter();
  Result.DividerHeight := 0;
  Result.Margin := '1';
  Result.OnItemClick := DoListItemClick;
  Result.OnItemMeasureHeight := DoItemMeasureHeight;
end;

procedure TCustomDownPopup.DeleteSelected;
begin
  if ItemIndex <> -1 then Items.Delete(ItemIndex);
end;

destructor TCustomDownPopup.Destroy;
begin
  TComboBoxHelper.Unregister(Self);
  TStringsListAdapter(FListBox.Adapter).FontColor := nil;
  FreeAndNil(FDropDownButton);
  FreeAndNil(FItems);
  FreeAndNil(FListPicker);
  FreeAndNil(FListBackground);
  FreeAndNil(FListTextColor);
  FOwner := nil;
  inherited;
end;

procedure TCustomDownPopup.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomDownPopup.DoCheckedChange(Sender: TObject; ItemIndex: Integer;
  const ItemView: TControl);
var
  AChecked, bCheck: Boolean;
  AEvent: TNotifyEvent;
begin
  if Assigned(OnMultipleCheckedChange) then begin
    AChecked := TMultipleStringsListAdapter(FListBox.Adapter).ItemCheck[ItemIndex];
    bCheck := AChecked;
    OnMultipleCheckedChange(Sender, ItemIndex, bCheck);
    if AChecked <> bCheck then begin
      AEvent := TCheckBoxView(ItemView).OnChange;
      TCheckBoxView(ItemView).OnChange := nil;
      TCheckBoxView(ItemView).IsChecked := bCheck;
      TCheckBoxView(ItemView).OnChange := AEvent;
    end;
  end;
end;

procedure TCustomDownPopup.DoClosePicker(Sender: TObject);
begin
  if not (csDestroying in FOwner.ComponentState) then begin
    FDroppedDown := False;
    if Assigned(FOnClosePopup) then
      FOnClosePopup(Self);
  end;
end;

procedure TCustomDownPopup.DoClosePopup(Sender: TObject);
begin
  FDroppedDown := False;
  if Assigned(FOnClosePopup) then
    FOnClosePopup(Self);
end;

procedure TCustomDownPopup.DoDropDownButtonChanged(Sender: TObject);
begin
  FOwner.Repaint;
end;

procedure TCustomDownPopup.DoItemMeasureHeight(Sender: TObject; Index: Integer;
  var AHeight: Single);
begin
  AHeight := FItemHeight;
end;

procedure TCustomDownPopup.DoItemsChange(Sender: TObject);
begin
  if FItemIndex >= FItems.Count - 1 then
    ItemIndex := FItems.Count - 1;
  TComboBoxHelper.SetItemsChanged(Self, True);
end;

procedure TCustomDownPopup.DoListItemClick(Sender: TObject; ItemIndex: Integer;
  const ItemView: TControl);
begin
  if not FMultiple then begin
    Self.ItemIndex := ItemIndex;
    FPopup.IsOpen := False;
  end else
    DoCheckedChange(Sender, ItemIndex, ItemView);
end;

procedure TCustomDownPopup.DoOnValueChangedFromDropDownList(Sender: TObject;
  const AValueIndex: Integer);
var
  LChanged: Boolean;
begin
  if FOwner.Observers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkEdit(FOwner.Observers) then
      Exit;
  LChanged := ItemIndex <> AValueIndex;
  if LChanged then
    TLinkObservers.PositionLinkPosChanging(FOwner.Observers);
  ItemIndex := AValueIndex;
  if LChanged then
    TLinkObservers.ListSelectionChanged(FOwner.Observers);
end;

procedure TCustomDownPopup.DoPaintBackground(Canvas: TCanvas; var R: TRectF; DrawState: TViewState);
begin
  if Assigned(FDropDownButton) and (not FDropDownButton.IsEmpty) then
    FDropDownButton.AdjustDraw(Canvas, R, True, DrawState);
end;

procedure TCustomDownPopup.DoPopup(Sender: TObject);
begin
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

procedure TCustomDownPopup.DropDown;
begin
  if UseNativePicker then
  begin
    if FListPicker.IsShown then
    begin
      FDroppedDown := False;
      FListPicker.Hide;
    end
    else
    begin
      FOldItemIndex := ItemIndex;
      if Items.Count > 0 then
      begin
        FDroppedDown := True;
        InitPicker(FListPicker);
        FListPicker.Show;
      end;
    end;
  end
  else
  begin
    if not FPopup.IsOpen then
    begin
      FOldItemIndex := ItemIndex;
      if Items.Count > 0 then
      begin
        RecalculatePopupSize;
        FDroppedDown := True;
        if ItemIndex >= 0 then
          FListBox.ScrollToIndex(ItemIndex)
        else
          FListBox.ViewportPosition := TPointF.Zero;
        FPopup.IsOpen := True;
        if FPopup.IsOpen then
          FListBox.SetFocus;
      end;
    end
    else
      FPopup.IsOpen := False;
  end;
end;

function TCustomDownPopup.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCustomDownPopup.GetItems: TStrings;
begin
  Result := FItems;
end;

function TCustomDownPopup.GetListAdapter: TStringsListAdapter;
begin
  Result := nil;
  if Assigned(FOnInitListAdapter) then
    Result := FOnInitListAdapter(Self);
  if not Assigned(Result) then begin
    if FMultiple then
      Result := TMultipleStringsListAdapter.Create(FOwner, FItems)
    else
      Result := TStringsListAdapter.Create(FItems);
    Result.DefaultItemHeight := FItemHeight;
    if FOwner is TTextView then
      Result.FontSize := TTextView(FOwner).TextSettings.Font.Size
    else if FOwner is TEditViewBase then
      Result.FontSize := TEditViewBase(FOwner).TextSettings.Font.Size;
    Result.WordWrap := False;
    Result.Padding := RectF(4, 0, 4, 0);
    Result.HeightSize := TViewSize.CustomSize;
  end;
end;

function TCustomDownPopup.GetListItemCheck(const Index: Integer): Boolean;
begin
  if FMultiple and Assigned(FListBox) then begin
    Result := TMultipleStringsListAdapter(FListBox.Adapter).ItemCheck[Index]
  end else
    Result := False;
end;

function TCustomDownPopup.GetMultipleText: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do begin
    if ItemsChecked[I] then
      Result := Result + Items[I] + FMultipleTextSeparator;
  end;
end;

procedure TCustomDownPopup.InitPicker(AListPicker: TCustomListPicker);
begin
  if FOwner.Pressed or TControlEx(FOwner).DoubleClick then
    AListPicker.PreferedDisplayIndex := Screen.DisplayFromPoint(Screen.MousePos).Index
  else
    AListPicker.PreferedDisplayIndex := -1;
  if TComboBoxHelper.AreItemsChanged(Self) then
  begin
    AListPicker.Values := Items;
    TComboBoxHelper.SetItemsChanged(Self, False);
  end;
  AListPicker.ItemIndex := ItemIndex;
  AListPicker.ItemWidth := ItemWidth;
  AListPicker.ItemHeight := ItemHeight;
  AListPicker.CountVisibleItems := DropDownCount;
end;

function TCustomDownPopup.IsItemHeightStored: Boolean;
begin
  Result := FItemHeight <> 18;
end;

function TCustomDownPopup.IsListItemCheckedColorStored: Boolean;
begin
  Result := FListItemCheckedColor <> D_ListItemCheckedColor;
end;

function TCustomDownPopup.IsListItemHoveredColorStored: Boolean;
begin
  Result := FListItemHoveredColor <> D_ListItemHoveredColor;
end;

function TCustomDownPopup.ItemsStored: Boolean;
begin
  Result := Count > 0;
end;

function TCustomDownPopup.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState; out OldItemIndex, NewItemIndex: Integer): Boolean;
begin
  Result := False;
  if not FDroppedDown then
    OldItemIndex := ItemIndex
  else if (DropDownKind = TDropDownKind.Native) and (FCanUseListPicker) then
    OldItemIndex := FListPicker.ItemIndex
  else
    OldItemIndex := TStringsListAdapter(FListBox.Adapter).ItemIndex;
  NewItemIndex := OldItemIndex;

  if FCanUseListPicker and FOwner.Observers.IsObserving(TObserverMapping.EditLinkID) then begin
    if (KeyChar > ' ') or
      (Key in [vkHome, vkEnd, vkUp, vkDown, vkRight, vkLeft]) then
      if not TLinkObservers.EditLinkEdit(FOwner.Observers) then begin
        Result := True;
        Exit;
      end;
  end;
end;

function TCustomDownPopup.KeyDownHandle(var Key: Word;
  var KeyChar: System.WideChar; Shift: TShiftState; var OldItemIndex, NewItemIndex: Integer): Boolean;
  function TryFindMatchingItem(var AItemIndex: Integer): Boolean;
  var
    I: Integer;
    Item: string;
  begin
    if KeyChar = #0 then
      Exit(False);

    for I := 0 to Count - 1 do
    begin
      Item := TStringsListAdapter(FListBox.Adapter).Items[I];
      if not Item.IsEmpty and (LowerCase(Item).Chars[0] = LowerCase(KeyChar)) then
      begin
        AItemIndex := I;
        Exit(True);
      end;
    end;
    Result := False;
  end;

  function IsDropDownKey(const AKey: Word; const Shift: TShiftState): Boolean;
  begin
    Result := (Key = vkDown) and ([ssAlt, ssCtrl, ssShift, ssCommand] * Shift = [ssAlt]);
  end;

  function PrevItemIndex(const AItemIndex: Integer): Integer;
  begin
    Result := EnsureRange(AItemIndex - 1, 0, Count - 1)
  end;

  function NextItemIndex(const AItemIndex: Integer): Integer;
  begin
    Result := EnsureRange(AItemIndex + 1, 0, Count - 1);
  end;

var
  NoVisItems, I: Integer;
  AText: string;
begin
  Result := False;
  if Count = 0 then
    Exit;

  if IsDropDownKey(Key, Shift) or (Key in [vkEscape, vkHome, vkEnd]) or ([ssAlt, ssCtrl, ssShift, ssCommand] * Shift = []) then
  begin
    if TryFindMatchingItem(NewItemIndex) then
      KeyChar := #0;

    case Key of
      vkHome:
        NewItemIndex := 0;
      vkEnd:
        NewItemIndex := Count - 1;
      vkUp, vkLeft:
        NewItemIndex := PrevItemIndex(NewItemIndex);
      vkRight:
        NewItemIndex := NextItemIndex(NewItemIndex);
      vkDown:
        if ssAlt in Shift then
          DropDown
        else
          NewItemIndex := NextItemIndex(NewItemIndex);
      vkPrior:
        begin
          if NewItemIndex > 0 then
          begin
            //calculate the number of visible items of the List Box
            NoVisItems:= DropDownCount;
            // updating the index after PageUp key is pressed
            NewItemIndex:= NewItemIndex - NoVisItems;
          end;
          if NewItemIndex < 0 then
            NewItemIndex:= 0;
        end;
      vkNext:
        begin
          if NewItemIndex < Count - 1 then
          begin
            //calculate the number of visible items of the List Box
            NoVisItems:= DropDownCount;
            //updating the index after PageDown key is pressed
            NewItemIndex:= NewItemIndex + NoVisItems;
          end;
          if NewItemIndex > Count -1 then
            NewItemIndex:= Count - 1;
        end;
      vkF4, vkReturn:
      begin
        // Before closing popup, we should update current value of ItemIndex
        if FDroppedDown then
          ItemIndex := NewItemIndex;
        DropDown;
      end;
      vkEscape:
        if (UseNativePicker and FListPicker.IsShown) or ((not UseNativePicker) and FPopup.IsOpen) then
        begin
          DropDown;
          if InRange(FOldItemIndex, 0, Count - 1) then
            NewItemIndex := FOldItemIndex
          else
            NewItemIndex := -1;
        end
        else
          Exit
    else
      begin
        AText := '';
        if (FOwner is TEditViewBase) then
          AText := TEditViewBase(FOwner).Text;
        if (AText <> '') then begin
          for I := 0 to Count - 1 do begin
            if Items[I].StartsWith(AText) then begin
              NewItemIndex := I;
              Result := True;
              Break;
            end;
          end;
        end;
      end;
    end;

    if NewItemIndex <> OldItemIndex then
    begin
      Result := True;
      TLinkObservers.PositionLinkPosChanging(FOwner.Observers);
      try
        if not FDroppedDown then
          ItemIndex := NewItemIndex
        else if (DropDownKind = TDropDownKind.Native) and (FCanUseListPicker) then
          FListPicker.ItemIndex := NewItemIndex
        else begin
          TStringsListAdapter(FListBox.Adapter).ItemIndex := NewItemIndex;
          TStringsListAdapter(FListBox.Adapter).NotifyDataChanged;
          if not FListBox.IsVisibleIndex(NewItemIndex) then
            FListBox.ScrollToIndex(NewItemIndex);
        end;
      finally
        TLinkObservers.ListSelectionChanged(FOwner.Observers);
      end;
    end;
    Key := 0;
  end;
end;

procedure TCustomDownPopup.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
    DropDown;
end;

procedure TCustomDownPopup.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  if WheelDelta < 0 then
  begin
    if ItemIndex < Count - 1 then
      ItemIndex := ItemIndex + 1
  end
  else
    if ItemIndex > 0 then
      ItemIndex := ItemIndex - 1;
  Handled := True;
end;

procedure TCustomDownPopup.RecalculatePopupSize;
var
  PopupContentHeight: Single;
begin
  FListBox.Align := TAlignLayout.Client;
  FListBox.Parent := FPopup;
  FListBox.Visible := True;
  FListBox.Background.ItemDefault := FListBackground;
  TStringsListAdapter(FListBox.Adapter).FontColor := FListTextColor;
  TStringsListAdapter(FListBox.Adapter).DefaultItemHeight := FItemHeight;
  TStringsListAdapter(FListBox.Adapter).ListItemCheckedColor := FListItemCheckedColor;
  TStringsListAdapter(FListBox.Adapter).ListItemHoveredColor := FListItemHoveredColor;
  TStringsListAdapter(FListBox.Adapter).ItemIndex := ItemIndex;
  FListbox.NotifyDataChanged;
  FPopup.ApplyStyleLookup;
  if FOwner.Pressed or TControlEx(FOwner).DoubleClick then
    FPopup.PreferedDisplayIndex := Screen.DisplayFromPoint(Screen.MousePos).Index
  else
    FPopup.PreferedDisplayIndex := -1;
  if SameValue(ItemWidth, 0, TEpsilon.Position) then
    FPopup.Width := FOwner.Width
  else
    FPopup.Width := ItemWidth;

  if ItemHeight > 0 then
    PopupContentHeight := Min(Count, DropDownCount) * ItemHeight
  else
    PopupContentHeight := Min(Count, DropDownCount) * TStringsListAdapter(FListBox.Adapter).DefaultItemHeight;
  FPopup.Height := FPopup.Padding.Top + PopupContentHeight + FListBox.Padding.Top +
    FListBox.Padding.Bottom + FPopup.Padding.Bottom + FListBox.Margins.Top + FListBox.Margins.Bottom;
end;

procedure TCustomDownPopup.SetDropDownButton(const Value: TDrawableIcon);
begin
  if FDropDownButton <> Value then begin
    if not Assigned(Value) then Exit;
    FDropDownButton.Assign(Value);
  end;
end;

procedure TCustomDownPopup.SetDropDownCount(const Value: Integer);
begin
  if FDropDownCount <> Value then
    FDropDownCount := Value;
end;

procedure TCustomDownPopup.SetMultiple(const Value: Boolean);
begin
  if FMultiple <> Value then begin
    FMultiple := Value;
    if Assigned(FListBox) then begin
      if Assigned(FListBox.Adapter) then
        TStringsListAdapter(FListBox.Adapter).FontColor := nil;
      FListBox.Adapter := GetListAdapter();
    end;
    FCanUseListPicker := False;
  end;
end;

procedure TCustomDownPopup.SetItemHeight(const Value: Single);
begin
  if FItemHeight <> Value then begin
    FItemHeight := Value;
    if Assigned(TStringsListAdapter(FListBox.Adapter)) then
      TStringsListAdapter(FListBox.Adapter).DefaultItemHeight := FItemHeight;
    if FOwner is TScrollView then
      TScrollViewX(FOwner).RealignContent;
  end;
end;

procedure TCustomDownPopup.SetItemIndex(const Value: Integer);
begin
  if FItemIndex <> Value then begin
    if FMultiple then Exit;
    FItemIndex := Value;
    if FPopup.IsOpen and (not FCanUseListPicker) then begin
      TStringsListAdapter(FListBox.Adapter).ItemIndex := Value;
      ListBox.Adapter.NotifyDataChanged;
      if not FListBox.IsVisibleIndex(Value) then
        FListBox.ScrollToIndex(Value);
    end;
    FOwner.Repaint;
    DoChange;
  end;
end;

procedure TCustomDownPopup.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
  DoChange;
end;

procedure TCustomDownPopup.SetItemWidth(const Value: Single);
begin
  if FItemWidth <> Value then begin
    FItemWidth := Value;
    if FOwner is TScrollView then
      TScrollViewX(FOwner).RealignContent;
  end;
end;

procedure TCustomDownPopup.SetListBackground(const Value: TViewBrush);
begin
  if FListBackground <> Value then begin
    if Assigned(Value) then
      FListBackground.Assign(Value);
  end;
end;

procedure TCustomDownPopup.SetListItemCheck(const Index: Integer;
  const Value: Boolean);
begin
  if FMultiple and Assigned(FListBox) and Assigned(FListBox.Adapter) then begin
    TMultipleStringsListAdapter(FListBox.Adapter).ItemCheck[Index] := Value;
    if FPopup.IsOpen then
      TMultipleStringsListAdapter(FListBox.Adapter).NotifyDataChanged;
  end;
end;

procedure TCustomDownPopup.SetListTextColor(const Value: TViewColor);
begin
  if FListTextColor <> Value then
    FListTextColor.Assign(Value);
end;

function TCustomDownPopup.UseNativePicker: Boolean;
begin
  Result := FCanUseListPicker and (TDropDownKind.Native = DropDownKind) and (FListPicker <> nil);
end;


{ TCustomComboBoxView }

procedure TCustomComboBoxView.AddItem(const Item: String; AObject: TObject);
begin
  FDownPopup.AddItem(Item, AObject);
end;

procedure TCustomComboBoxView.Clear;
begin
  FDownPopup.Clear;
end;

procedure TCustomComboBoxView.ClearSelection;
begin
  FDownPopup.ClearSelection;
end;

constructor TCustomComboBoxView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDownPopup := TCustomDownPopup.Create(Self);
  FDownPopup.FOnChange := DoItemChange;
  FDownPopup.FOnMultipleCheckedChange := DoMultipleCheckedChange;

  Clickable := True;
  CanFocus := True;
  Padding.DefaultValue := RectF(4, 4, 4, 4);
  Padding.Rect := Padding.DefaultValue;
  Gravity := TLayoutGravity.CenterVertical;

  if not Assigned(FBackground) then
    FBackground := CreateBackground;
end;

function TCustomComboBoxView.CreateBackground: TDrawable;
begin
  Result := TDrawableBorder.Create(Self, TViewBrushKind.Solid, $ffe1e1e1);
  Result.ItemPressed.Color := $ffcce4f7;
  Result.ItemPressed.DefaultColor := Result.ItemPressed.Color;
  Result.ItemPressed.Kind := TViewBrushKind.Solid;
  Result.ItemPressed.DefaultKind := TBrushKind.Solid;
  Result.ItemHovered.Color := $ffe5f1fb;
  Result.ItemHovered.DefaultColor := Result.ItemHovered.Color;
  Result.ItemHovered.Kind := TViewBrushKind.Solid;
  Result.ItemHovered.DefaultKind := TBrushKind.Solid;
  Result.ItemFocused.Color := $ffe5f1fb;
  Result.ItemFocused.DefaultColor := Result.ItemFocused.Color;
  Result.ItemFocused.Kind := TViewBrushKind.Solid;
  Result.ItemFocused.DefaultKind := TBrushKind.Solid;
  with TDrawableBorder(Result).Border do begin
    DefaultStyle := TViewBorderStyle.RectBorder;
    Style := DefaultStyle;
    Color.Default := $ffadadad;
    Color.DefaultChange := False;
    Color.Pressed := $ef1fc9ff;
    Color.PressedChange := False;
    Color.Hovered := $EF33ccff;
    Color.HoveredChange := False;
    Color.Focused := $EF33ccff;
    Color.FocusedChange := False;
  end;
  Result.OnChanged := DoBackgroundChanged;
end;

procedure TCustomComboBoxView.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('UseSmallScrollBars', IgnoreBooleanValue, nil, False);
end;

procedure TCustomComboBoxView.DeleteSelected;
begin
  FDownPopup.DeleteSelected;
end;

destructor TCustomComboBoxView.Destroy;
begin
  if Assigned(FDownPopup) then begin
    FDownPopup.FOnChange := nil;
    FreeAndNil(FDownPopup);
  end;
  inherited Destroy;
end;

procedure TCustomComboBoxView.DoItemChange;
begin
  if FDownPopup.ItemIndex < 0 then
    Text := ''
  else
    Text := FDownPopup.FItems[FDownPopup.ItemIndex];
  if Assigned(FOnItemChange) then
    FOnItemChange(Self);
end;

procedure TCustomComboBoxView.DoMultipleCheckedChange(Sender: TObject;
  ItemIndex: Integer; var Checked: Boolean);
begin
  if Assigned(FOnMultipleCheckedChange) then
    FOnMultipleCheckedChange(Self, ItemIndex, Checked);
  if Assigned(FOnGetMultipleText) then
    Text := FOnGetMultipleText(Self)
  else
    Text := FDownPopup.MultipleText;
end;

procedure TCustomComboBoxView.DoPaintBackground(var R: TRectF);
begin
  R := RectF(R.Left + Padding.Left, R.Top + Padding.Top + 1,
    R.Right - Padding.Right, R.Bottom - Padding.Bottom);
  if Assigned(FDownPopup) then
    FDownPopup.DoPaintBackground(Canvas, R, DrawState);
  if (Assigned(TextSettings)) then
    DoPaintText(R);
end;

procedure TCustomComboBoxView.DropDown;
begin
  if Assigned(FDownPopup) then FDownPopup.DropDown;
end;

function TCustomComboBoxView.GetCanUseListPicker: Boolean;
begin
  Result := FDownPopup.FCanUseListPicker;
end;

function TCustomComboBoxView.GetCount: Integer;
begin
  Result := FDownPopup.Count;
end;

function TCustomComboBoxView.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(100, 22);
end;

function TCustomComboBoxView.GetDropDownButton: TDrawableIcon;
begin
  Result := FDownPopup.DropDownButton;
end;

function TCustomComboBoxView.GetDropDownCount: Integer;
begin
  Result := FDownPopup.DropDownCount;
end;

function TCustomComboBoxView.GetDropDownKind: TDropDownKind;
begin
  Result := FDownPopup.FDropDownKind;
end;

function TCustomComboBoxView.GetDroppedDown: Boolean;
begin
  Result := FDownPopup.DroppedDown;
end;

function TCustomComboBoxView.GetItemHeight: Single;
begin
  Result := FDownPopup.ItemHeight;
end;

function TCustomComboBoxView.GetItemIndex: Integer;
begin
  Result := FDownPopup.ItemIndex;
end;

function TCustomComboBoxView.GetItems: TStrings;
begin
  Result := FDownPopup.FItems;
end;

function TCustomComboBoxView.GetItemWidth: Single;
begin
  Result := FDownPopup.ItemWidth;
end;

function TCustomComboBoxView.GetListBackground: TViewBrush;
begin
  Result := FDownPopup.FListBackground;
end;

function TCustomComboBoxView.GetListBox: TListViewEx;
begin
  Result := FDownPopup.FListBox;
end;

function TCustomComboBoxView.GetListItemCheck(const Index: Integer): Boolean;
begin
  Result := FDownPopup.ItemsChecked[Index];
end;

function TCustomComboBoxView.GetListItemCheckedColor: TAlphaColor;
begin
  Result := FDownPopup.FListItemCheckedColor;
end;

function TCustomComboBoxView.GetListItemHoveredColor: TAlphaColor;
begin
  Result := FDownPopup.FListItemHoveredColor;
end;

function TCustomComboBoxView.GetListMultiple: Boolean;
begin
  Result := FDownPopup.FMultiple;
end;

function TCustomComboBoxView.GetListTextColor: TViewColor;
begin
  Result := FDownPopup.FListTextColor;
end;

function TCustomComboBoxView.GetOnInitListAdapter: TOnInitListAdapter;
begin
  Result := FDownPopup.FOnInitListAdapter;
end;

function TCustomComboBoxView.GetOnPopup: TNotifyEvent;
begin
  Result := FDownPopup.OnPopup;
end;

function TCustomComboBoxView.GetPopup: TPopup;
begin
  Result := FDownPopup.Popup;
end;

function TCustomComboBoxView.IsItemHeightStored: Boolean;
begin
  Result := FDownPopup.IsItemHeightStored;
end;

function TCustomComboBoxView.IsListItemCheckedColorStored: Boolean;
begin
  Result := FDownPopup.IsListItemCheckedColorStored;
end;

function TCustomComboBoxView.IsListItemHoveredColorStored: Boolean;
begin
  Result := FDownPopup.IsListItemHoveredColorStored;
end;

function TCustomComboBoxView.ItemsStored: Boolean;
begin
  Result := FDownPopup.ItemsStored;
end;

procedure TCustomComboBoxView.KeyDown(var Key: Word;
  var KeyChar: System.WideChar; Shift: TShiftState);
var
  OldItemIndex, NewItemIndex: Integer;
begin
  if Assigned(FDownPopup) then begin
    if FDownPopup.KeyDown(Key, KeyChar, Shift, OldItemIndex, NewItemIndex) then Exit;
    inherited;
    FDownPopup.KeyDownHandle(Key, KeyChar, Shift, OldItemIndex, NewItemIndex);
  end else
    inherited;
end;

procedure TCustomComboBoxView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Assigned(FDownPopup) then FDownPopup.MouseDown(Button, Shift, X, Y);
end;

procedure TCustomComboBoxView.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if Assigned(FDownPopup) then FDownPopup.MouseWheel(Shift, WheelDelta, Handled);
end;

procedure TCustomComboBoxView.SetCanUseListPicker(const Value: Boolean);
begin
  FDownPopup.CanUseListPicker := Value;
end;

procedure TCustomComboBoxView.SetDropDownButton(const Value: TDrawableIcon);
begin
  FDownPopup.DropDownButton := Value;
end;

procedure TCustomComboBoxView.SetDropDownCount(const Value: Integer);
begin
  FDownPopup.DropDownCount := Value;
end;

procedure TCustomComboBoxView.SetDropDownKind(const Value: TDropDownKind);
begin
  FDownPopup.DropDownKind := Value;
end;

procedure TCustomComboBoxView.SetItemHeight(const Value: Single);
begin
  FDownPopup.ItemHeight := Value;
end;

procedure TCustomComboBoxView.SetItemIndex(const Value: Integer);
begin
  FDownPopup.ItemIndex := Value;
end;

procedure TCustomComboBoxView.SetItems(const Value: TStrings);
begin
  FDownPopup.Items := Value;
end;

procedure TCustomComboBoxView.SetItemWidth(const Value: Single);
begin
  FDownPopup.ItemWidth := Value;
end;

procedure TCustomComboBoxView.SetListBackground(const Value: TViewBrush);
begin
  FDownPopup.ListBackground := Value;
end;

procedure TCustomComboBoxView.SetListItemCheck(const Index: Integer;
  const Value: Boolean);
begin
  FDownPopup.ItemsChecked[Index] := Value;
end;

procedure TCustomComboBoxView.SetListItemCheckedColor(const Value: TAlphaColor);
begin
  FDownPopup.ListItemCheckedColor := Value;
end;

procedure TCustomComboBoxView.SetListItemHoveredColor(const Value: TAlphaColor);
begin
  FDownPopup.ListItemHoveredColor := Value;
end;

procedure TCustomComboBoxView.SetListMultiple(const Value: Boolean);
begin
  FDownPopup.Multiple := Value;
end;

procedure TCustomComboBoxView.SetListTextColor(const Value: TViewColor);
begin
  FDownPopup.ListTextColor := Value;
end;

procedure TCustomComboBoxView.SetName(const Value: TComponentName);
var
  LastText: string;
begin
  LastText := Text;
  inherited;
  Text := LastText;
end;

procedure TCustomComboBoxView.SetOnInitListAdapter(
  const Value: TOnInitListAdapter);
begin
  FDownPopup.OnInitListAdapter := Value;
end;

procedure TCustomComboBoxView.SetOnPopup(const Value: TNotifyEvent);
begin
  FDownPopup.OnPopup := Value;
end;

{ TCustomComboBoxEditView }

procedure TCustomComboBoxEditView.AddItem(const Item: String; AObject: TObject);
begin
  FDownPopup.AddItem(Item, AObject);
end;

procedure TCustomComboBoxEditView.Clear;
begin
  FDownPopup.Clear;
end;

procedure TCustomComboBoxEditView.ClearSelection;
begin
  FDownPopup.ClearSelection;
end;

constructor TCustomComboBoxEditView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDownPopup := TCustomDownPopup.Create(Self);
  FDownPopup.FOnChange := DoItemChange;
  FDownPopup.FOnMultipleCheckedChange := DoMultipleCheckedChange;
end;

procedure TCustomComboBoxEditView.DeleteSelected;
begin
  FDownPopup.DeleteSelected;
end;

destructor TCustomComboBoxEditView.Destroy;
begin
  FDownPopup.FOnChange := nil;
  FreeAndNil(FDownPopup);
  inherited;
end;

procedure TCustomComboBoxEditView.DoItemChange(Sender: TObject);
begin
  if FDownPopup.ItemIndex < 0 then
    Text := ''
  else
    Text := FDownPopup.FItems[FDownPopup.ItemIndex];
  if Assigned(FOnItemChange) then
    FOnItemChange(Self);
end;

procedure TCustomComboBoxEditView.DoMultipleCheckedChange(Sender: TObject;
  ItemIndex: Integer; var Checked: Boolean);
begin
  if Assigned(FOnMultipleCheckedChange) then
    FOnMultipleCheckedChange(Self, ItemIndex, Checked);
  if Assigned(FOnGetMultipleText) then
    Text := FOnGetMultipleText(Self)
  else
    Text := FDownPopup.MultipleText;
end;

procedure TCustomComboBoxEditView.DoPaintBackground(var R: TRectF);
begin
  R := RectF(R.Left + Padding.Left, R.Top + Padding.Top + 1,
    R.Right - Padding.Right, R.Bottom - Padding.Bottom);
  if Assigned(FDownPopup) then
    FDownPopup.DoPaintBackground(Canvas, R, DrawState);
  if Assigned(Drawable) and (not Drawable.IsEmpty) then
    Drawable.AdjustDraw(Canvas, R, True, DrawState);
  if (Assigned(TextSettings)) then
    DoPaintText(R);
end;

procedure TCustomComboBoxEditView.DropDown;
begin
  if Assigned(FDownPopup) then FDownPopup.DropDown;
end;

function TCustomComboBoxEditView.GetCanUseListPicker: Boolean;
begin
  Result := FDownPopup.FCanUseListPicker;
end;

function TCustomComboBoxEditView.GetCount: Integer;
begin
  Result := FDownPopup.Count;
end;

function TCustomComboBoxEditView.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(100, 22);
end;

function TCustomComboBoxEditView.GetDropDownButton: TDrawableIcon;
begin
  Result := FDownPopup.DropDownButton;
end;

function TCustomComboBoxEditView.GetDropDownCount: Integer;
begin
  Result := FDownPopup.DropDownCount;
end;

function TCustomComboBoxEditView.GetDropDownKind: TDropDownKind;
begin
  Result := FDownPopup.DropDownKind;
end;

function TCustomComboBoxEditView.GetDroppedDown: Boolean;
begin
  Result := FDownPopup.DroppedDown;
end;

function TCustomComboBoxEditView.GetItemHeight: Single;
begin
  Result := FDownPopup.ItemHeight;
end;

function TCustomComboBoxEditView.GetItemIndex: Integer;
begin
  Result := FDownPopup.ItemIndex;
end;

function TCustomComboBoxEditView.GetItems: TStrings;
begin
  Result := FDownPopup.Items;
end;

function TCustomComboBoxEditView.GetItemWidth: Single;
begin
  Result := FDownPopup.ItemWidth;
end;

function TCustomComboBoxEditView.GetListBackground: TViewBrush;
begin
  Result := FDownPopup.ListBackground;
end;

function TCustomComboBoxEditView.GetListBox: TListViewEx;
begin
  Result := FDownPopup.ListBox;
end;

function TCustomComboBoxEditView.GetListItemCheck(
  const Index: Integer): Boolean;
begin
  Result := FDownPopup.ItemsChecked[Index];
end;

function TCustomComboBoxEditView.GetListItemCheckedColor: TAlphaColor;
begin
  Result := FDownPopup.ListItemCheckedColor;
end;

function TCustomComboBoxEditView.GetListItemHoveredColor: TAlphaColor;
begin
  Result := FDownPopup.ListItemHoveredColor;
end;

function TCustomComboBoxEditView.GetListMultiple: Boolean;
begin
  Result := FDownPopup.FMultiple;
end;

function TCustomComboBoxEditView.GetListTextColor: TViewColor;
begin
  Result := FDownPopup.ListTextColor;
end;

function TCustomComboBoxEditView.GetOnInitListAdapter: TOnInitListAdapter;
begin
  Result := FDownPopup.OnInitListAdapter;
end;

function TCustomComboBoxEditView.GetOnPopup: TNotifyEvent;
begin
  Result := FDownPopup.OnPopup;
end;

function TCustomComboBoxEditView.GetPopup: TPopup;
begin
  Result := FDownPopup.Popup;
end;

function TCustomComboBoxEditView.IsItemHeightStored: Boolean;
begin
  Result := FDownPopup.IsItemHeightStored;
end;

function TCustomComboBoxEditView.IsListItemCheckedColorStored: Boolean;
begin
  Result := FDownPopup.IsListItemCheckedColorStored;
end;

function TCustomComboBoxEditView.IsListItemHoveredColorStored: Boolean;
begin
  Result := FDownPopup.IsListItemHoveredColorStored;
end;

function TCustomComboBoxEditView.ItemsStored: Boolean;
begin
  Result := FDownPopup.ItemsStored;
end;

procedure TCustomComboBoxEditView.KeyDown(var Key: Word;
  var KeyChar: System.WideChar; Shift: TShiftState);
var
  IsDelete: Boolean;
  OldItemIndex, NewItemIndex: Integer;
  LastText, NexText: string;
begin
  if Assigned(FDownPopup) then begin
    IsDelete := (Key = 8) or (Key = 46);
    if FDownPopup.KeyDown(Key, KeyChar, Shift, OldItemIndex, NewItemIndex) then Exit;
    inherited;
    LastText := Self.Text;
    if FDownPopup.KeyDownHandle(Key, KeyChar, Shift, OldItemIndex, NewItemIndex) and (not IsDelete) and FDownPopup.DroppedDown
      and (NewItemIndex >= 0) then begin
      NexText := Items[NewItemIndex];
      if (LastText <> NexText) and (LastText.Length < NexText.Length) then begin
        Text := NexText;
        SelStart := LastText.Length;
        SelLength := Text.Length - LastText.Length;
      end;
    end;
  end else
    inherited;
end;

procedure TCustomComboBoxEditView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Assigned(FDownPopup) and FInDropDown then begin
    FDownPopup.MouseDown(Button, Shift, X, Y);
    FInDropDown := False;
  end;
end;

procedure TCustomComboBoxEditView.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Pos: TPointF;
  Size: TSizeF;
  R: TRectF;
begin
  inherited MouseMove(Shift, X, Y);
  FInDropDown := False;
  if csDesigning in ComponentState then Exit;
  if Assigned(FDownPopup) then begin
    if Assigned(FDownPopup.DropDownButton) and (not FDownPopup.DropDownButton.IsEmpty) then begin
      Pos.X := 0;
      Pos.Y := 0;
      Size.cx := Max(Width - Pos.X, 0);
      Size.cy := Max(Height - Pos.Y, 0);
      R := RectF(Pos.X + Padding.Left, Pos.Y + Padding.Top, Size.cx - Padding.Right, Size.cy - Padding.Bottom);
      Pos.X := R.Left;
      Pos.Y := R.Right;
      FDownPopup.DropDownButton.AdjustDraw(Canvas, R, False, DrawState);
      if R.Left <> Pos.X then begin
        FInDropDown := (X >= 0) and (X <= R.Left);
      end else if R.Right <> Pos.Y then begin
        FInDropDown := (X >= R.Right) and (X <= Size.cx);
      end;
    end;
  end;
  if FInDropDown then
    Self.Cursor := crDefault
  else
    Self.Cursor := crIBeam;
end;

procedure TCustomComboBoxEditView.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if Assigned(FDownPopup) then FDownPopup.MouseWheel(Shift, WheelDelta, Handled);
end;

procedure TCustomComboBoxEditView.RealignDrawableContent(
  var ContentRect: TRectF);
begin
  if Assigned(FDownPopup) then begin
    if Assigned(FDownPopup.DropDownButton) and (not FDownPopup.DropDownButton.IsEmpty) then
      FDownPopup.DropDownButton.AdjustDraw(Canvas, ContentRect, False, DrawState);
  end;
  inherited RealignDrawableContent(ContentRect);
end;

procedure TCustomComboBoxEditView.SetCanUseListPicker(const Value: Boolean);
begin
  FDownPopup.CanUseListPicker := Value;
end;

procedure TCustomComboBoxEditView.SetDropDownButton(const Value: TDrawableIcon);
begin
  FDownPopup.DropDownButton := Value;
end;

procedure TCustomComboBoxEditView.SetDropDownCount(const Value: Integer);
begin
  FDownPopup.DropDownCount := Value;
end;

procedure TCustomComboBoxEditView.SetDropDownKind(const Value: TDropDownKind);
begin
  FDownPopup.DropDownKind := Value;
end;

procedure TCustomComboBoxEditView.SetItemHeight(const Value: Single);
begin
  FDownPopup.ItemHeight := Value;
end;

procedure TCustomComboBoxEditView.SetItemIndex(const Value: Integer);
begin
  FDownPopup.ItemIndex := Value;
end;

procedure TCustomComboBoxEditView.SetItems(const Value: TStrings);
begin
  FDownPopup.Items := Value;
end;

procedure TCustomComboBoxEditView.SetItemWidth(const Value: Single);
begin
  FDownPopup.ItemWidth := Value;
end;

procedure TCustomComboBoxEditView.SetListBackground(const Value: TViewBrush);
begin
  FDownPopup.ListBackground := Value;
end;

procedure TCustomComboBoxEditView.SetListItemCheck(const Index: Integer;
  const Value: Boolean);
begin
  FDownPopup.ItemsChecked[Index] := Value;
end;

procedure TCustomComboBoxEditView.SetListItemCheckedColor(
  const Value: TAlphaColor);
begin
  FDownPopup.ListItemCheckedColor := Value;
end;

procedure TCustomComboBoxEditView.SetListItemHoveredColor(
  const Value: TAlphaColor);
begin
  FDownPopup.ListItemHoveredColor := Value;
end;

procedure TCustomComboBoxEditView.SetListMultiple(const Value: Boolean);
begin
  FDownPopup.Multiple := Value;
end;

procedure TCustomComboBoxEditView.SetListTextColor(const Value: TViewColor);
begin
  FDownPopup.ListTextColor := Value;
end;

procedure TCustomComboBoxEditView.SetOnInitListAdapter(
  const Value: TOnInitListAdapter);
begin
  FDownPopup.OnInitListAdapter := Value;
end;

procedure TCustomComboBoxEditView.SetOnPopup(const Value: TNotifyEvent);
begin
  FDownPopup.OnPopup := Value;
end;

procedure TCustomComboBoxEditView.SetText(const Value: string);
begin
  inherited;
  SelectAll();
end;

initialization
  TComboBoxHelper.Initialize;

finalization
  TComboBoxHelper.Uninitialize;

end.
