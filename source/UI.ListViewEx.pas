{*******************************************************}
{                                                       }
{       FMX UI TListView 扩展单元                       }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.ListViewEx;

interface

{$SCOPEDENUMS ON}

uses
  UI.Debug, UI.Base, UI.Standard,
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  FMX.Utils, FMX.ImgList, FMX.MultiResBitmap, FMX.ActnList, System.Rtti, FMX.Consts,
  FMX.TextLayout, FMX.Objects, System.ImageList, System.RTLConsts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, System.Math,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement,
  FMX.ListView, FMX.ListView.Appearances, FMX.ListView.Types, FMX.Styles.Objects;

type
  TListExView = class;

  /// <summary>
  /// 列表框扩展，这个是基础系统自带的 TListView 的
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TListExView = class(TListView, IView)
  private
    FOnFooterClick: TAppearanceListView.TItemEvent;
    procedure SetBadgeView(const Value: TControl);
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
    function GetInVisible: Boolean;
    procedure SetInVisible(const Value: Boolean);
    function GetToastHeight: Single;
    procedure UpdateScrollBarEx(AW, AH: Single);
    {$IF CompilerVersion < 31}
    function GetOpacity: Single;
    {$ENDIF}
  protected
    FAdjustViewBounds: Boolean;
    FLayout: TViewLayout;
    FMinWidth: Single;
    FMinHeight: Single;
    FMaxWidth: Single;
    FMaxHeight: Single;
    FWeight: Single;
    FInVisible: Boolean;

    [Weak] FScrollBar: TScrollBar;
    FMaxKnownHeight: Single;

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
    function GetPosition: TPosition;

    function IsAutoSize: Boolean;

    procedure IncViewState(const State: TViewState);
    procedure DecViewState(const State: TViewState);

    procedure DoLayoutChanged(Sender: TObject); virtual;
    function AllowUseLayout(): Boolean; virtual;

    procedure DoAdjustViewBounds(var ANewWidth, ANewHeight: Single); virtual;
    procedure DoRecalcSize(var AWidth, AHeight: Single); virtual;
    procedure DoChangeSize(var ANewWidth, ANewHeight: Single); virtual;
    function DoSetSize(const ASize: TControlSize; const NewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
      var ALastWidth, ALastHeight: Single): Boolean; override;
    procedure DoChange; override;
    function GetLocalRect: TRectF; override;
  protected
    FButtonClick: Boolean;
    function FindLocalItemObjectAtPosition(const ItemIndex: Integer; const Position: TPointF): TListItemDrawable;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ToastHeight: Single read GetToastHeight;
    property Background: TDrawable read GetBackground write SetBackground;
    property Gravity: TLayoutGravity read GetGravity write SetGravity;
    property Orientation: TOrientation read GetOrientation write SetOrientation;
    property ParentView: IViewGroup read GetParentView;
  published
    property AdjustViewBounds: Boolean read GetAdjustViewBounds write SetAdjustViewBounds;
    property InVisible: Boolean read GetInVisible write SetInVisible default False;
    property HeightSize: TViewSize read GetHeightSize write SetHeightSize;
    property WidthSize: TViewSize read GetWidthSize write SetWidthSize;
    property MaxHeight: Single read GetMaxHeight write SetMaxHeight;
    property MaxWidth: Single read GetMaxWidth write SetMaxWidth;
    property MinHeight: Single read GetMinHeight write SetMinHeight;
    property MinWidth: Single read GetMinWidth write SetMinWidth;
    property Weight: Single read GetWeight write SetWeight;
    property Layout: TViewLayout read GetLayout write SetLayout;
    property Transparent default True;
    property OnFooterClick: TAppearanceListView.TItemEvent read FOnFooterClick write FOnFooterClick;
  end;

  /// <summary>
  /// 自定义 ListViewItem
  /// 底部详细，右上角备注，带图标
  /// </summary>
  TRightBottomItemAppearance = class(TPresetItemObjects)
  public const
    cTextMarginAccessory = 8;
    cDefaultImagePlaceOffsetX = -3;
    cDefaultImageTextPlaceOffsetX = 4;
    RightTextName = 'RightText';
    DetailTextName = 'DetailText';
    NoteTextName = 'NoteText';
  private
    FRightText: TTextObjectAppearance;
    FDetailText: TTextObjectAppearance;
    FNoteText: TTextObjectAppearance;
    procedure SetNoteText(const Value: TTextObjectAppearance);
    procedure SetRightText(const Value: TTextObjectAppearance);
    procedure SetDetailText(const Value: TTextObjectAppearance);
  protected
    function DefaultHeight: Integer; override;
    procedure UpdateSizes(const FinalSize: TSizeF); override;
    function GetGroupClass: TPresetItemObjects.TGroupClass; override;
    procedure SetObjectData(const AListViewItem: TListViewItem; const AIndex: string; const AValue: TValue; var AHandled: Boolean); override;
  public
    constructor Create(const Owner: TControl); override;
    destructor Destroy; override;
  published
    property Image;
    property Text;
    property RightText: TTextObjectAppearance read FRightText write SetRightText;
    property DetailText: TTextObjectAppearance read FDetailText write SetDetailText;
    property NoteText: TTextObjectAppearance read FNoteText write SetNoteText;
    property Accessory;
  end;

  TRightBottomDeleteAppearance = class(TRightBottomItemAppearance)
  private const
    cDefaultGlyph = TGlyphButtonType.Delete;
  public
    constructor Create(const Owner: TControl); override;
  published
    property GlyphButton;
  end;

  TRightBottomShowCheckAppearance = class(TRightBottomItemAppearance)
  private const
    cDefaultGlyph = TGlyphButtonType.Checkbox;
  public
    constructor Create(const Owner: TControl); override;
  published
    property GlyphButton;
  end;

  TListFooterAppearance = class(TPresetItemObjects)
  private const
    cDefaultHeaderHeight = 60;
  private
    FLoading: Boolean;
  protected
    procedure UpdateSizes(const FinalSize: TSizeF); override;
    function DefaultHeight: Integer; override;
    function GetGroupClass: TPresetItemObjects.TGroupClass; override;
  public
    constructor Create(const Owner: TControl); override;
  published
    property Text;
    property Loading: Boolean read FLoading write FLoading;
  end;

type
  TFreeListView = class(TScrollView)
  private
  end;

implementation

resourcestring
  SNotSupports = '不支持的接口';

{ TListExView }

function TListExView.AllowUseLayout: Boolean;
begin
  Result := (not (csDesigning in ComponentState)) or
    (Assigned(ParentControl)) and (ParentControl is TRelativeLayout);
end;

constructor TListExView.Create(AOwner: TComponent);

  function FindScrollbar(): TScrollBar;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to ChildrenCount - 1 do
      if Children[I] is TScrollBar then begin
        Result := Children[I] as TScrollBar;
        Break;
      end;
  end;

begin
  inherited Create(AOwner);
  FAdjustViewBounds := True;
  if csDesigning in ComponentState then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  WidthSize := TViewSize.CustomSize;
  Transparent := True;
  FScrollBar := FindScrollbar;
  if Assigned(FScrollBar) then
    FScrollBar.Visible := False;
end;

procedure TListExView.DecViewState(const State: TViewState);
begin
end;

destructor TListExView.Destroy;
begin
  FreeAndNil(FLayout);
  inherited;
end;

procedure TListExView.DoAdjustViewBounds(var ANewWidth, ANewHeight: Single);
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

procedure TListExView.DoChange;
begin
  if csDestroying in ComponentState then
    Exit;
  inherited DoChange;
end;

procedure TListExView.DoChangeSize(var ANewWidth, ANewHeight: Single);
begin
  DoRecalcSize(ANewWidth, ANewHeight);
  FMaxKnownHeight := ANewHeight;
  DoAdjustViewBounds(ANewWidth, ANewHeight);
  if Assigned(FScrollBar) and (HeightSize = TViewSize.WrapContent) then begin
    if FMaxKnownHeight <> ANewHeight then begin
      FScrollBar.Visible := FMaxKnownHeight > ANewHeight;
    end;
    FScrollBar.SetBounds(ANewWidth - FScrollBar.Width, 0, FScrollBar.Width, ANewHeight);
    UpdateScrollBarEx(ANewWidth, ANewHeight);
  end;
end;

procedure TListExView.DoLayoutChanged(Sender: TObject);
begin
  HandleSizeChanged;
end;

procedure TListExView.DoRecalcSize(var AWidth, AHeight: Single);
begin
  if HeightSize = TViewSize.WrapContent then
    AHeight := GetToastHeight;
end;

function TListExView.DoSetSize(const ASize: TControlSize;
  const NewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
  var ALastWidth, ALastHeight: Single): Boolean;
begin
  DoChangeSize(ANewWidth, ANewHeight);
  Result := inherited DoSetSize(ASize, NewPlatformDefault, ANewWidth, ANewHeight,
    ALastWidth, ALastHeight);
end;

function TListExView.FindLocalItemObjectAtPosition(const ItemIndex: Integer;
  const Position: TPointF): TListItemDrawable;
var
  I: Integer;
  Item: TListItem;
begin
  if (ItemIndex < 0) or (ItemIndex >= Adapter.Count) then
    Exit(nil);

  Item := Adapter[ItemIndex];

  for I := 0 to Item.Count - 1 do
    if Item.View[I].InLocalRect(Position) then
      Exit(Item.View[I]);

  Result := nil;
end;

function TListExView.GetAdjustViewBounds: Boolean;
begin
  Result := FAdjustViewBounds;
end;

function TListExView.GetBackground: TDrawable;
begin
  Result := nil;
end;

function TListExView.GetComponent: TComponent;
begin
  Result := Self;
end;

function TListExView.GetComponentState: TComponentState;
begin
  Result := ComponentState;
end;

function TListExView.GetDrawState: TViewState;
begin
  Result := TViewState.None;
end;

function TListExView.GetGravity: TLayoutGravity;
begin
  Result := TLayoutGravity.None;
end;

function TListExView.GetHeightSize: TViewSize;
begin
  if Assigned(FLayout) then
    Result := FLayout.HeightSize
  else Result := TViewSize.CustomSize;
end;

function TListExView.GetInVisible: Boolean;
begin
  Result := FInVisible;
end;

function TListExView.GetLayout: TViewLayout;
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

function TListExView.GetLocalRect: TRectF;
begin
  if csDestroying in ComponentState then
    Result := TRectF.Empty
  else
    Result := inherited GetLocalRect;
end;

function TListExView.GetMaxHeight: Single;
begin
  Result := FMaxHeight;
end;

function TListExView.GetMaxWidth: Single;
begin
  Result := FMinWidth;
end;

function TListExView.GetMinHeight: Single;
begin
  Result := FMinHeight;
end;

function TListExView.GetMinWidth: Single;
begin
  Result := FMinWidth;
end;

{$IF CompilerVersion < 31}
function TListExView.GetOpacity: Single;
begin
  Result := Opacity;
end;
{$ENDIF}

function TListExView.GetOrientation: TOrientation;
begin
  Result := TOrientation.Vertical;
end;

function TListExView.GetParentControl: TControl;
begin
  Result := ParentControl;
end;

function TListExView.GetParentView: IViewGroup;
begin
  Supports(Parent, IViewGroup, Result);
end;

function TListExView.GetPosition: TPosition;
begin
  Result := Position;
end;

function TListExView.GetToastHeight: Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    Result := Result + GetItemHeight(I);
end;

function TListExView.GetViewStates: TViewStates;
begin
  Result := [];
end;

function TListExView.GetWeight: Single;
begin
  Result := FWeight;
end;

function TListExView.GetWidthSize: TViewSize;
begin
  if Assigned(FLayout) then
    Result := FLayout.WidthSize
  else Result := TViewSize.CustomSize;
end;

procedure TListExView.IncViewState(const State: TViewState);
begin
end;

function TListExView.IsAutoSize: Boolean;
begin
  Result := False;
end;

procedure TListExView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not ShouldHandleEvents then
    Exit;
  if Button = TMouseButton.mbLeft then
    FButtonClick := True
  else
    FButtonClick := False;
end;

procedure TListExView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Item: TListViewItem;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FButtonClick and Assigned(FOnFooterClick) then begin
    FButtonClick := False;
    if Items.Count > 0 then begin
      Item := Items[Items.Count - 1];
      if FindLocalItemObjectAtPosition(Item.Index, PointF(X, Y)) <> nil then
        FOnFooterClick(Self, Item);
    end;
  end;
end;

procedure TListExView.SetAdjustViewBounds(const Value: Boolean);
begin
  if FAdjustViewBounds <> Value then begin
    FAdjustViewBounds := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetBackground(const Value: TDrawable);
begin
  raise EViewError.Create(SNotSupports);
end;

procedure TListExView.SetBadgeView(const Value: TControl);
begin
end;

procedure TListExView.SetGravity(const Value: TLayoutGravity);
begin
  raise EViewError.Create(SNotSupports);
end;

procedure TListExView.SetHeightSize(const Value: TViewSize);
begin
  if (not Assigned(FLayout)) and (Value <> TViewSize.CustomSize) then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  if Assigned(FLayout) then
    FLayout.HeightSize := Value;
end;

procedure TListExView.SetInVisible(const Value: Boolean);
begin
  if FInVisible <> Value then begin
    FInVisible := Value;
    if Visible then
      Repaint;
  end;
end;

procedure TListExView.SetLayout(const Value: TViewLayout);
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

procedure TListExView.SetMaxHeight(const Value: Single);
begin
  if FMaxHeight <> Value then begin
    FMaxHeight := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetMaxWidth(const Value: Single);
begin
  if FMaxWidth <> Value then begin
    FMaxWidth := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetMinHeight(const Value: Single);
begin
  if FMinHeight <> Value then begin
    FMinHeight := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetMinWidth(const Value: Single);
begin
  if FMinWidth <> Value then begin
    FMinWidth := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetOrientation(const Value: TOrientation);
begin
  raise EViewError.Create(SNotSupports);
end;

procedure TListExView.SetWeight(const Value: Single);
begin
  if FWeight <> Value then begin
    FWeight := Value;
    HandleSizeChanged;
  end;
end;

procedure TListExView.SetWidthSize(const Value: TViewSize);
begin
  if (not Assigned(FLayout)) and (Value <> TViewSize.CustomSize) then begin
    FLayout := TViewLayout.Create(Self);
    FLayout.OnChanged := DoLayoutChanged;
  end;
  if Assigned(FLayout) then
    FLayout.WidthSize := Value;
end;

procedure TListExView.UpdateScrollBarEx(AW, AH: Single);
var
  LocalHeight, ViewSize: Single;
begin
  LocalHeight := AH;

  if ScrollViewPos < 0 then
    ViewSize := LocalHeight + ScrollViewPos
  else if ScrollViewPos > FMaxKnownHeight - LocalHeight then
    ViewSize := LocalHeight - (ScrollViewPos - (FMaxKnownHeight - LocalHeight))
  else
    ViewSize := LocalHeight;

  FScrollBar.BeginUpdate;
  try
    FScrollBar.Max := SideSpace * 2 + FMaxKnownHeight;
    FScrollBar.SmallChange := Adapter.GetDefaultViewHeight * 0.5;
    if (csDesigning in ComponentState) then  // Don't show at design time
      FScrollBar.Visible := False;
    FScrollBar.Value := ScrollViewPos;
    FScrollBar.ViewportSize := ViewSize;
  finally
    FScrollBar.EndUpdate;
  end;

end;

{ TRightBottomItemAppearance }

const
  cMultiDetailMember = 'DetailText';
  cMultiNoteMember = 'NoteText';
  cMultiRightMember = 'RightText';

constructor TRightBottomItemAppearance.Create(const Owner: TControl);
var
  LastFontSize: Single;
begin
  inherited;
  Accessory.DefaultValues.AccessoryType := TAccessoryType.More;
  Accessory.DefaultValues.Visible := True;
  Accessory.RestoreDefaults;
  LastFontSize := Text.DefaultValues.Font.Size;
  Text.DefaultValues.Font.Size := LastFontSize + 4;
  Text.DefaultValues.VertAlign := TListItemAlign.Trailing;
  Text.DefaultValues.TextVertAlign := TTextAlign.Leading;
  Text.DefaultValues.Height := 65;  // Item will be bottom aligned, with text top aligned
  Text.DefaultValues.Visible := True;
  Text.RestoreDefaults;

  FDetailText := TTextObjectAppearance.Create;
  FDetailText.Name := DetailTextName;
  FDetailText.DefaultValues.Assign(Text.DefaultValues);
  FDetailText.DefaultValues.Font.Size := LastFontSize;
  FDetailText.DefaultValues.VertAlign := TListItemAlign.Trailing;
  FDetailText.DefaultValues.TextVertAlign := TTextAlign.Leading;
  FDetailText.DefaultValues.Height := 42;  // Move text down
  FDetailText.DefaultValues.IsDetailText := True; // Use detail font
  FDetailText.RestoreDefaults;
  FDetailText.OnChange := Self.ItemPropertyChange;
  FDetailText.Owner := Self;

  FNoteText := TTextObjectAppearance.Create;
  FNoteText.Name := NoteTextName;
  FNoteText.DefaultValues.Assign(FDetailText.DefaultValues);
  FNoteText.DefaultValues.Height := 26;  // Move text down
  FNoteText.DefaultValues.IsDetailText := True; // Use detail font
  FNoteText.RestoreDefaults;
  FNoteText.OnChange := Self.ItemPropertyChange;
  FNoteText.Owner := Self;

  FRightText := TTextObjectAppearance.Create;
  FRightText.Name := RightTextName;
  FRightText.DefaultValues.Assign(FDetailText.DefaultValues);
  FRightText.DefaultValues.Align := TListItemAlign.Trailing;
  FRightText.DefaultValues.TextAlign := TTextAlign.Trailing;
  FRightText.DefaultValues.VertAlign := TListItemAlign.Leading;
  FRightText.DefaultValues.TextVertAlign := TTextAlign.Center;
  FRightText.DefaultValues.Height := 38;  // Move text down
  FRightText.DefaultValues.IsDetailText := True; // Use detail font
  FRightText.DefaultValues.PlaceOffset.X := -32;
  FRightText.RestoreDefaults;
  FRightText.OnChange := Self.ItemPropertyChange;
  FRightText.Owner := Self;

  // Define livebindings members that make up MultiDetail
  FDetailText.DataMembers :=
    TObjectAppearance.TDataMembers.Create(
      TObjectAppearance.TDataMember.Create(
        cMultiDetailMember, // Displayed by LiveBindings
        Format('Data["%s"]', [DetailTextName])));   // Expression to access value from TListViewItem
  FNoteText.DataMembers :=
    TObjectAppearance.TDataMembers.Create(
      TObjectAppearance.TDataMember.Create(
        cMultiNoteMember, // Displayed by LiveBindings
        Format('Data["%s"]', [NoteTextName])));   // Expression to access value from TListViewItem
  FRightText.DataMembers :=
    TObjectAppearance.TDataMembers.Create(
      TObjectAppearance.TDataMember.Create(
        cMultiRightMember, // Displayed by LiveBindings
        Format('Data["%s"]', [RightTextName])));   // Expression to access value from TListViewItem

  Image.DefaultValues.Width := cDefaultImageWidth;
  Image.DefaultValues.Height := cDefaultImageHeight;
  Image.RestoreDefaults;

  GlyphButton.DefaultValues.VertAlign := TListItemAlign.Center;
  GlyphButton.RestoreDefaults;

  // Define the appearance objects
  AddObject(Text, True);
  AddObject(FDetailText, True);
  AddObject(FNoteText, True);
  AddObject(FRightText, True);
  AddObject(Image, True);
  AddObject(Accessory, True);
  AddObject(GlyphButton, IsItemEdit);  // GlyphButton is only visible when in edit mode
end;

function TRightBottomItemAppearance.DefaultHeight: Integer;
begin
  Result := 70;
end;

destructor TRightBottomItemAppearance.Destroy;
begin
  FDetailText.Free;
  FNoteText.Free;
  FRightText.Free;
  inherited;
end;

function TRightBottomItemAppearance.GetGroupClass: TPresetItemObjects.TGroupClass;
begin
  Result := TRightBottomItemAppearance;
end;

procedure TRightBottomItemAppearance.SetDetailText(
  const Value: TTextObjectAppearance);
begin
  FDetailText.Assign(Value);
end;

procedure TRightBottomItemAppearance.SetNoteText(
  const Value: TTextObjectAppearance);
begin
  FNoteText.Assign(Value);
end;

procedure TRightBottomItemAppearance.SetObjectData(
  const AListViewItem: TListViewItem; const AIndex: string;
  const AValue: TValue; var AHandled: Boolean);
begin
  inherited;
end;

procedure TRightBottomItemAppearance.SetRightText(
  const Value: TTextObjectAppearance);
begin
  FRightText.Assign(Value);
end;

procedure TRightBottomItemAppearance.UpdateSizes(const FinalSize: TSizeF);
var
  LInternalWidth: Single;
  LImagePlaceOffset: Single;
  LImageTextPlaceOffset: Single;
begin
  BeginUpdate;
  try
    inherited;

    // Update the widths and positions of renderening objects within a TListViewItem
    if Image.ActualWidth = 0 then
    begin
      LImagePlaceOffset := 0;
      LImageTextPlaceOffset := 0;
    end else begin
      LImagePlaceOffset := cDefaultImagePlaceOffsetX;
      LImageTextPlaceOffset := cDefaultImageTextPlaceOffsetX;
    end;
    Image.InternalPlaceOffset.X := GlyphButton.ActualWidth + LImagePlaceOffset;
    if Image.ActualWidth > 0 then
      Text.InternalPlaceOffset.X :=
        Image.ActualPlaceOffset.X +  Image.ActualWidth + LImageTextPlaceOffset
    else
      Text.InternalPlaceOffset.X :=
        0 + GlyphButton.ActualWidth;
    FDetailText.InternalPlaceOffset.X := Text.InternalPlaceOffset.X;
    FNoteText.InternalPlaceOffset.X := Text.InternalPlaceOffset.X;
    FRightText.InternalPlaceOffset.X := Text.InternalPlaceOffset.X;
    LInternalWidth := FinalSize.Width - Text.ActualPlaceOffset.X - Accessory.ActualWidth;
    if Accessory.ActualWidth > 0 then
      LInternalWidth := LInternalWidth - cTextMarginAccessory;
    Text.InternalWidth := Max(1, LInternalWidth);
    FDetailText.InternalWidth := Text.InternalWidth;
    FNoteText.InternalWidth := Text.InternalWidth;
    FRightText.InternalWidth := Text.InternalWidth;
  finally
    EndUpdate;
  end;
end;

{ TRightBottomDeleteAppearance }

constructor TRightBottomDeleteAppearance.Create(const Owner: TControl);
begin
  inherited;
  GlyphButton.DefaultValues.ButtonType := cDefaultGlyph;
  GlyphButton.DefaultValues.Visible := True;
  GlyphButton.RestoreDefaults;
end;

{ TRightBottomShowCheckAppearance }

constructor TRightBottomShowCheckAppearance.Create(const Owner: TControl);
begin
  inherited;
  GlyphButton.DefaultValues.ButtonType := cDefaultGlyph;
  GlyphButton.DefaultValues.Visible := True;
  GlyphButton.RestoreDefaults;
end;

{ TListFooterAppearance }

constructor TListFooterAppearance.Create(const Owner: TControl);
begin
  inherited;
  Text.DefaultValues.Visible := True;
  Text.DefaultValues.Align := TListItemAlign.Center;
  Text.DefaultValues.TextAlign := TTextAlign.Center;
  Text.DefaultValues.TextVertAlign := TTextAlign.Center;
  Text.DefaultValues.VertAlign := TListItemAlign.Center;
  Text.RestoreDefaults;
  AddObject(Text, True);
end;

function TListFooterAppearance.DefaultHeight: Integer;
begin
  Result := cDefaultHeaderHeight;
end;

function TListFooterAppearance.GetGroupClass: TPresetItemObjects.TGroupClass;
begin
  Result := TListFooterAppearance;
end;

procedure TListFooterAppearance.UpdateSizes(const FinalSize: TSizeF);
begin
  BeginUpdate;
  try
    inherited;
    Text.InternalWidth := FinalSize.Width;
  finally
    EndUpdate;
  end;
end;

const
  // Will be added to the uses list when appearance is used
  sThisUnit = 'UI.ListViewEx';

initialization
  // MultiDetailItem group
  TAppearancesRegistry.RegisterAppearance(
    TRightBottomItemAppearance, 'CustomRightBottomItem',
    [TRegisterAppearanceOption.Item], sThisUnit);
  TAppearancesRegistry.RegisterAppearance(
    TRightBottomDeleteAppearance, 'CustomRightBottomItemDelete',
    [TRegisterAppearanceOption.ItemEdit], sThisUnit);
  TAppearancesRegistry.RegisterAppearance(
    TRightBottomShowCheckAppearance, 'CustomRightBottomItemShowCheck',
    [TRegisterAppearanceOption.ItemEdit], sThisUnit);
  TAppearancesRegistry.RegisterAppearance(TListFooterAppearance, 'CustomFooterAppearance',
    [TRegisterAppearanceOption.Header, TRegisterAppearanceOption.Footer], sThisUnit);

finalization
  TAppearancesRegistry.UnregisterAppearances(
    TArray<TItemAppearanceObjectsClass>.Create(
      TRightBottomItemAppearance, TRightBottomDeleteAppearance,
      TRightBottomShowCheckAppearance, TListFooterAppearance));

end.

