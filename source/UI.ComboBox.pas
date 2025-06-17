unit UI.ComboBox;

interface

uses
  UI.Base, UI.Utils, UI.Ani, UI.ListView, UI.Standard,
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
  TCustomComboBoxView = class(TTextView)
  private
    FItems: TStrings;
    FOldItemIndex: Integer;
    FItemIndex: Integer;
    FDropDownCount: Integer;
    FOnChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FOnPopup: TNotifyEvent;
    FOnClosePopup: TNotifyEvent;
    FItemWidth: Single;
    FItemHeight: Single;
    FPopup: TPopup;
    FListBox: TListViewEx;
    FDropDownKind: TDropDownKind;
    FDroppedDown: Boolean;
    FListPicker: TCustomListPicker;
    FDropDownButton: TDrawableIcon;
    FCanUseListPicker: Boolean;
    FListBackground: TViewBrush;
    FListTextColor: TViewColor;
    FListCheckedBackgroudColor: TAlphaColor;
    function GetCount: Integer;
    function GetItems: TStrings;
    function IsItemHeightStored: Boolean;
    function ItemsStored: Boolean;
    procedure SetDropDownCount(const Value: Integer);
    procedure SetItemHeight(const Value: Single);
    procedure SetItems(const Value: TStrings);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItemWidth(const Value: Single);
    procedure SetListBackground(const Value: TViewBrush);
    procedure SetListTextColor(const Value: TViewColor);
    procedure SetListCheckedBackgroudColor(const Value: TAlphaColor);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure DoListItemClick(Sender: TObject; ItemIndex: Integer; const ItemView: TControl);
  protected
    procedure Loaded; override;
    procedure DoChange; dynamic;
    procedure DoPopup(Sender: TObject);
    procedure DoClosePopup(Sender: TObject);
    procedure DoClosePicker(Sender: TObject);
    procedure DoDropDownButtonChanged(Sender: TObject);
    procedure DoOnValueChangedFromDropDownList(Sender: TObject; const AValueIndex: Integer);
    function UseNativePicker: Boolean;
    procedure InitPicker(AListPicker: TCustomListPicker); virtual;
    procedure RecalculatePopupSize; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoPaintBackground(var R: TRectF); override;
    function CreateListBox(): TListViewEx; virtual;
    function GetListAdapter(): TStringsListAdapter;
    function CreateBackground: TDrawable; override;
    function CreateDropDownButton: TDrawableIcon; virtual;
    function GetDefaultSize: TSizeF; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const Item: String; AObject: TObject); virtual;
    procedure Clear; virtual;
    procedure ClearSelection; virtual;
    procedure DeleteSelected; virtual;
    procedure DropDown; virtual;
    property ListBox: TListViewEx read FListBox;
    property ListBackground: TViewBrush read FListBackground write SetListBackground;
    property ListTextColor: TViewColor read FListTextColor write SetListTextColor;
    property ListCheckedBackgroudColor: TAlphaColor read FListCheckedBackgroudColor write SetListCheckedBackgroudColor;
    property Popup: TPopup read FPopup;
    property CanFocus default True;
    property CanParentFocus;
    property Items: TStrings read GetItems write SetItems stored ItemsStored;
    property Count: Integer read GetCount;
    property CanUseListPicker: Boolean read FCanUseListPicker write FCanUseListPicker default True;
    property DropDownButton: TDrawableIcon read FDropDownButton;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemWidth: Single read FItemWidth write SetItemWidth;
    property ItemHeight: Single read FItemHeight write SetItemHeight stored IsItemHeightStored;
    property DropDownKind: TDropDownKind read FDropDownKind write FDropDownKind default TDropDownKind.Native;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount default 8;
    property DroppedDown: Boolean read FDroppedDown;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property Gravity default TLayoutGravity.CenterVertical;
  end;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TComboBoxView = class(TCustomComboBoxView)
  published
    property CanFocus;
    property CanParentFocus;
    property Items;
    property ItemIndex default -1;
    property ItemHeight;
    property CanUseListPicker default True;
    property DropDownKind;
    property DropDownCount default 8;
    property DropDownButton;
    property ListBackground;
    property ListTextColor;
    property ListCheckedBackgroudColor;

    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;

    property OnChange;
    property OnClosePopup;
    property OnPopup;

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

implementation

const
  SDefaultDownBtnSVG = '<svg t="1750145518585" class="icon" viewBox="0 0 1024 1024" version="1.1" '+
    'xmlns="http://www.w3.org/2000/svg" p-id="5310" width="128" height="128">'+
    '<path d="M134.656 330.24c-16.896-17.92-16.896-46.592 0-64.512s45.056-17.92 62.464 0l314.368 '+
    '332.8 313.344-332.8c16.896-17.92 45.056-17.92 62.464 0s16.896 46.592 0 64.512l-336.896 '+
    '349.696c-10.752 9.728-24.064 20.992-37.888 18.432-13.824 3.072-28.672-7.168-39.424-18.432L134.656 330.24z" fill="" p-id="5311"/></svg>';

type
  TComboBoxHelper = class
  private
    class var FItems: TDictionary<Pointer, Boolean>;
  public
    class procedure Initialize;
    class procedure Uninitialize;
    class procedure Register(const AControl: TCustomComboBoxView);
    class procedure Unregister(const AControl: TCustomComboBoxView);
    class function AreItemsChanged(const AControl: TCustomComboBoxView): Boolean;
    class procedure SetItemsChanged(const AControl: TCustomComboBoxView; const AChanged: Boolean);
  end;

{ TComboBoxHelper }

class function TComboBoxHelper.AreItemsChanged(const AControl: TCustomComboBoxView): Boolean;
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

class procedure TComboBoxHelper.SetItemsChanged(const AControl: TCustomComboBoxView; const AChanged: Boolean);
begin
  FItems.AddOrSetValue(AControl, AChanged);
end;

class procedure TComboBoxHelper.Register(const AControl: TCustomComboBoxView);
begin
  FItems.Add(AControl, True);
end;

class procedure TComboBoxHelper.Unregister(const AControl: TCustomComboBoxView);
begin
  FItems.Remove(AControl);
end;

{ TCustomComboBoxView }

procedure TCustomComboBoxView.AddItem(const Item: String; AObject: TObject);
begin
  FItems.AddObject(Item, AObject);
end;

procedure TCustomComboBoxView.Clear;
begin
  FItems.Clear;
end;

procedure TCustomComboBoxView.ClearSelection;
begin
  ItemIndex := -1;
end;

constructor TCustomComboBoxView.Create(AOwner: TComponent);
var
  PickerService: IFMXPickerService;
begin
  inherited Create(AOwner);
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, PickerService) then
  begin
    FListPicker := PickerService.CreateListPicker;
    FListPicker.Parent := Self;
    FListPicker.OnValueChanged := DoOnValueChangedFromDropDownList;
    FListPicker.OnHide := DoClosePicker;
    FListPicker.OnShow := DoPopup;
  end;
  FCanUseListPicker := True;
  FItemHeight := 18;
  FItems := TStringList.Create;
  FItemIndex := -1;
  FOldItemIndex := -1;
  FDropDownKind := TDropDownKind.Custom;
  DropDownCount := 8;
  FDroppedDown := False;
  FPopup := TPopup.Create(Self);
  FPopup.StyleLookup := 'combopopupstyle';
  FPopup.PlacementTarget := Self;
  FPopup.Stored := False;
  FPopup.Parent := Self;
  FPopup.Locked := True;
  FPopup.DragWithParent := True;
  FPopup.OnClosePopup := DoClosePopup;
  FPopup.OnPopup := DoPopup;
  FListTextColor := TViewColor.Create(TextSettings.Color.Default);
  FListTextColor.Assign(TextSettings.Color);
  FListBackground := TViewBrush.Create(TViewBrushKind.Solid, TAlphaColorRec.Null);
  FListBox := CreateListBox;
  if FListBox = nil then
    raise EArgumentNilException.CreateFmt(SResultCanNotBeNil, ['CreateListBox']);
  FListBox.Visible := False;
  FListBox.Parent := Popup;
  FListBox.Stored := False;
  FListBox.Align := TAlignLayout.Client;
  FItemIndex := -1;
  SetAcceptsControls(False);
  DropDownKind := TDropDownKind.Native;
  TComboBoxHelper.Register(Self);

  Clickable := True;
  CanFocus := True;
  Padding.DefaultValue := RectF(4, 4, 4, 4);
  Padding.Rect := Padding.DefaultValue;
  Gravity := TLayoutGravity.CenterVertical;
  FDropDownButton := CreateDropDownButton;
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
  Result.ItemHovered.DefaultColor := Result.ItemPressed.Color;
  Result.ItemHovered.Kind := TViewBrushKind.Solid;
  Result.ItemHovered.DefaultKind := TBrushKind.Solid;
  Result.ItemFocused.Color := $ffe5f1fb;
  Result.ItemFocused.DefaultColor := Result.ItemPressed.Color;
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
    Color.PressedChange := False;
    Color.Focused := $EF33ccff;
    Color.PressedChange := False;
  end;
  Result.OnChanged := DoBackgroundChanged;
end;

function TCustomComboBoxView.CreateDropDownButton: TDrawableIcon;
begin
  Result := TDrawableIcon.Create(Self);
  Result.SizeWidth := 10;
  Result.SizeHeight := 10;
  Result.Position := TDrawablePosition.Right;
  TViewBrushBase(Result.ItemDefault).SVGImage.Parse(SDefaultDownBtnSVG);
  TViewBrushBase(Result.ItemDefault).SVGImage.Color := $ff000000;
  TViewBrushBase(Result.ItemDefault).Kind := TViewBrushKind.SVGImage;
  Result.OnChanged := DoDropDownButtonChanged;
end;

procedure TCustomComboBoxView.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('UseSmallScrollBars', IgnoreBooleanValue, nil, False);
end;

procedure TCustomComboBoxView.DeleteSelected;
begin
  if ItemIndex <> -1 then
    Items.Delete(ItemIndex);
end;

destructor TCustomComboBoxView.Destroy;
begin
  TComboBoxHelper.Unregister(Self);
  FreeAndNil(FDropDownButton);
  FreeAndNil(FItems);
  FreeAndNil(FListPicker);
  FreeAndNil(FListBackground);
  inherited;
end;

procedure TCustomComboBoxView.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomComboBoxView.DoClosePicker(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    FDroppedDown := False;
    if Assigned(FOnClosePopup) then
      FOnClosePopup(Self);
  end;
end;

procedure TCustomComboBoxView.DoClosePopup(Sender: TObject);
begin
  FDroppedDown := False;
  if Assigned(FOnClosePopup) then
    FOnClosePopup(Self);
end;

procedure TCustomComboBoxView.DoDropDownButtonChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TCustomComboBoxView.DoListItemClick(Sender: TObject;
  ItemIndex: Integer; const ItemView: TControl);
begin
  Self.ItemIndex := ItemIndex;
  FPopup.IsOpen := False;
end;

procedure TCustomComboBoxView.DoOnValueChangedFromDropDownList(Sender: TObject;
  const AValueIndex: Integer);
var
  LChanged: Boolean;
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkEdit(Observers) then
      Exit;
  LChanged := ItemIndex <> AValueIndex;
  if LChanged then
    TLinkObservers.PositionLinkPosChanging(Observers);
  ItemIndex := AValueIndex;
  if LChanged then
    TLinkObservers.ListSelectionChanged(Observers);
end;

procedure TCustomComboBoxView.DoPaintBackground(var R: TRectF);
begin
  R := RectF(R.Left + Padding.Left, R.Top + Padding.Top + 1,
    R.Right - Padding.Right, R.Bottom - Padding.Bottom);
  if Assigned(FDropDownButton) and (not FDropDownButton.IsEmpty) then
    FDropDownButton.AdjustDraw(Canvas, R, True, DrawState);
  if (Assigned(TextSettings)) then
    DoPaintText(R);
end;

procedure TCustomComboBoxView.DoPopup(Sender: TObject);
begin
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

procedure TCustomComboBoxView.DropDown;
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
        FDroppedDown := True;
        RecalculatePopupSize;
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

function TCustomComboBoxView.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCustomComboBoxView.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(100, 22);
end;

function TCustomComboBoxView.GetItems: TStrings;
begin
  Result := FItems;
end;

function TCustomComboBoxView.GetListAdapter: TStringsListAdapter;
begin
  Result := TStringsListAdapter.Create(FItems);
  Result.DefaultItemHeight := FItemHeight;
  Result.FontSize := TextSettings.Font.Size;
  Result.WordWrap := False;
  Result.Padding := RectF(4, 0, 4, 0);
  Result.HeightSize := TViewSize.CustomSize;
end;

function TCustomComboBoxView.CreateListBox(): TListViewEx;
begin
  Result := TListViewEx.Create(Self);
  Result.Background.ItemDefault.Color := $ff909090;
  Result.Adapter := GetListAdapter();
  Result.DividerHeight := 0;
  Result.Margin := '1';
  Result.OnItemClick := DoListItemClick;
end;

procedure TCustomComboBoxView.InitPicker(AListPicker: TCustomListPicker);
begin
  if Pressed or DoubleClick then
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

function TCustomComboBoxView.IsItemHeightStored: Boolean;
begin
  Result := FItemHeight <> 18;
end;

function TCustomComboBoxView.ItemsStored: Boolean;
begin
  Result := Count > 0;
end;

procedure TCustomComboBoxView.KeyDown(var Key: Word;
  var KeyChar: System.WideChar; Shift: TShiftState);

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
  NoVisItems: Integer;
  OldItemIndex: Integer;
  NewItemIndex: Integer;
begin
   if not FDroppedDown then
    OldItemIndex := ItemIndex
  else if DropDownKind = TDropDownKind.Native then
    OldItemIndex := FListPicker.ItemIndex
  else
    OldItemIndex := TStringsListAdapter(FListBox.Adapter).ItemIndex;
  NewItemIndex := OldItemIndex;

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if (KeyChar > ' ') or
      (Key in [vkHome, vkEnd, vkUp, vkDown, vkRight, vkLeft]) then
      if not TLinkObservers.EditLinkEdit(Observers) then
        Exit;
  inherited;

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
    end;

    if NewItemIndex <> OldItemIndex then
    begin
      TLinkObservers.PositionLinkPosChanging(Observers);
      try
        if not FDroppedDown then
          ItemIndex := NewItemIndex
        else if DropDownKind = TDropDownKind.Native then
          FListPicker.ItemIndex := NewItemIndex
        else
          TStringsListAdapter(FListBox.Adapter).ItemIndex := NewItemIndex;
      finally
        TLinkObservers.ListSelectionChanged(Observers);
      end;
    end;
    Key := 0;
  end;
end;

procedure TCustomComboBoxView.Loaded;
begin
  inherited Loaded;
end;

procedure TCustomComboBoxView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then
    DropDown;
end;

procedure TCustomComboBoxView.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
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

procedure TCustomComboBoxView.RecalculatePopupSize;
var
  PopupContentHeight: Single;
begin
  FListBox.Parent := FPopup;
  FListBox.Visible := True;
  if Assigned(FListBackground) then
    FListBox.Background.ItemDefault.Assign(FListBackground);
  TStringsListAdapter(FListBox.Adapter).FontColor := FListTextColor;
  TStringsListAdapter(FListBox.Adapter).CheckedBackgroudColor := FListCheckedBackgroudColor;
  FListbox.NotifyDataChanged;
  FPopup.ApplyStyleLookup;
  if Pressed or DoubleClick then
    FPopup.PreferedDisplayIndex := Screen.DisplayFromPoint(Screen.MousePos).Index
  else
    FPopup.PreferedDisplayIndex := -1;
  if SameValue(ItemWidth, 0, TEpsilon.Position) then
    FPopup.Width := Width
  else
    FPopup.Width := ItemWidth;

  if ItemHeight > 0 then
    PopupContentHeight := Min(Count, DropDownCount) * ItemHeight
  else
    PopupContentHeight := Min(Count, DropDownCount) * TStringsListAdapter(FListBox.Adapter).DefaultItemHeight;
  FPopup.Height := FPopup.Padding.Top + PopupContentHeight + FListBox.Padding.Top +
    FListBox.Padding.Bottom + FPopup.Padding.Bottom + FListBox.Margins.Top + FListBox.Margins.Bottom;
end;

procedure TCustomComboBoxView.SetDropDownCount(const Value: Integer);
begin
  if FDropDownCount <> Value then
    FDropDownCount := Value;
end;

procedure TCustomComboBoxView.SetItemHeight(const Value: Single);
begin
  if FItemHeight <> Value then begin
    FItemHeight := Value;
    if Assigned(TStringsListAdapter(FListBox.Adapter)) then
      TStringsListAdapter(FListBox.Adapter).DefaultItemHeight := FItemHeight;
    RealignContent;
  end;
end;

procedure TCustomComboBoxView.SetItemIndex(const Value: Integer);
begin
  if FItemIndex <> Value then begin
    FItemIndex := Value;
    Text := Items[Value];
    if FPopup.IsOpen and (not FCanUseListPicker) then begin
      TStringsListAdapter(FListBox.Adapter).ItemIndex := Value;
      ListBox.Adapter.NotifyDataChanged;
    end;
    Repaint;
  end;
end;

procedure TCustomComboBoxView.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TCustomComboBoxView.SetItemWidth(const Value: Single);
begin
  if FItemWidth <> Value then begin
    FItemWidth := Value;
    RealignContent;
  end;
end;

procedure TCustomComboBoxView.SetListBackground(const Value: TViewBrush);
begin
  if FListBackground <> Value then begin
    if Assigned(Value) then
      FListBackground.Assign(Value);
  end;
end;

procedure TCustomComboBoxView.SetListCheckedBackgroudColor(
  const Value: TAlphaColor);
begin
  if FListCheckedBackgroudColor <> Value then
    FListCheckedBackgroudColor := Value;
end;

procedure TCustomComboBoxView.SetListTextColor(const Value: TViewColor);
begin
  if FListTextColor <> Value then
    FListTextColor.Assign(Value);
end;

procedure TCustomComboBoxView.SetName(const Value: TComponentName);
var
  LastText: string;
begin
  LastText := Text;
  inherited;
  Text := LastText;
end;

function TCustomComboBoxView.UseNativePicker: Boolean;
begin
  Result := FCanUseListPicker and (TDropDownKind.Native = DropDownKind) and (FListPicker <> nil);
end;

initialization
  TComboBoxHelper.Initialize;
finalization
  TComboBoxHelper.Uninitialize;

end.
