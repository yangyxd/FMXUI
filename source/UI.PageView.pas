unit UI.PageView;

interface

uses
  Classes, Math, Types, UITypes, Math.Vectors,
  UI.Utils, UI.Base, UI.Standard,
  FMX.Types, FMX.Controls, FMX.InertialMovement, FMX.Platform, FMX.Utils,
  FMX.Effects, FMX.Ani;

type
  TPageTabPosition = (Top, Bottom, None, Dots);

type
  TPageContent = class(TViewGroup)
  private
  protected
  public
  end;

type
  TPageViewBase = class;

  TPageItem = class(TTextView)
  public const
    DotSize = 8;
  private
    FOwner: TPageViewBase;
    FContent: TControl;
    FLeftOffset: Single;
    FRightOffset: Single;
    FShowAsDot: Boolean;
    function IsStoredLeftOffset: Boolean;
    function IsStoredRightOffset: Boolean;
    procedure SetLeftOffset(const Value: Single);
    procedure SetRightOffset(const Value: Single);
    procedure SetShowAsDot(const Value: Boolean);
  protected
    property Content: TControl read FContent;
  public
    procedure UpdateLayoutControl;
    function TouchEnabled: Boolean;

    property PageView: TPageViewBase read FOwner;
    property ShowAsDot: Boolean read FShowAsDot write SetShowAsDot;
  published
    property LeftOffset: Single read FLeftOffset write SetLeftOffset stored IsStoredLeftOffset;
    property RightOffset: Single read FRightOffset write SetRightOffset stored IsStoredRightOffset;
  end;

  TPageViewBase = class(TView, IItemsContainer)
  public type
    TFindKind = (Next, Back, First, Last, Current);
  private
    FContent: TPageContent;
    FNoItemsContent: TControl;
    FTabPosition: TPageTabPosition;
    FTabHeight: Single;
    FTabIndex: Integer;
    FTabBarRect: TRectF;
    FClientRect: TRectF;
    FRealigningTabs: Boolean;
    FFullSize: Boolean;
    FHasTouchScreen: Boolean;
    FAniCalculations: TAniCalculations;
    FIndexOfTargetTab: Integer;
    FInternalContentPosition: Single;
    FTabContentSize: TSizeF;

    FOnChange: TNotifyEvent;
    function GetTabCount: Integer;
    function GetTabItem(AIndex: Integer): TPageItem;
    procedure SetTabPosition(const Value: TPageTabPosition);
    function GetActiveTab: TPageItem;
    procedure SetActiveTab(const Value: TPageItem);
    function IsStoredTabHeight: Boolean;
    procedure SetTabHeight(const Value: Single);
    procedure SetTabIndex(const Value: Integer);
    procedure SetFullSize(const Value: Boolean);
    function GetTabContentPosition: Single;
    procedure SetTabContentPosition(const Value: Single);
  protected
    procedure Loaded; override;
    procedure RealignTabs; virtual;
    procedure DoRealign; override;
    procedure DoChange; virtual;

    procedure UpdateAnimation(const DotItems: Boolean; const ActiveTabLeft, ActiveTabRight: Single);
    procedure SetInternalContentPosition(const Value: Double);
    function RoundByScale(const Value: Double): Single;
    procedure AddOrInsertObject(const AObject: TFmxObject; const Index: Integer = MaxInt);
    function IsSpecialObject(const AObject: TFmxObject): Boolean;
    { IItemContainer }
    function GetItemsCount: Integer;
    function GetItem(const AIndex: Integer): TFmxObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateTabBarButtons;
    function HasActiveTab: Boolean;
    procedure GoToActiveTab;
    function FindVisibleTab(var Index: Integer; const FindKind: TFindKind): Boolean; overload;
    function FindVisibleTab(const FindKind: TFindKind): Integer; overload;

    property AniCalculations: TAniCalculations read FAniCalculations;
    property TabCount: Integer read GetTabCount;
    property Tabs[AIndex: Integer]: TPageItem read GetTabItem;
    property TabPosition: TPageTabPosition read FTabPosition write SetTabPosition default TPageTabPosition.Top;
    property TabBarRect: TRectF read FTabBarRect;
    property TabHeight: Single read FTabHeight write SetTabHeight stored IsStoredTabHeight;
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    property TabContentPosition: Single read GetTabContentPosition write SetTabContentPosition;
    property TabContentSize: TSizeF read FTabContentSize;
    property FullSize: Boolean read FFullSize write SetFullSize default False;
    property HasTouchScreen: Boolean read FHasTouchScreen;
  published
    property ActiveTab: TPageItem read GetActiveTab write SetActiveTab stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

type
  TPageView = class(TPageViewBase)
  published
  end;

implementation

type
  TTabControlAnimation = class(TAniCalculations)
  private
    [Weak] FTabControl: TPageViewBase;
  protected
    procedure DoStart; override;
    procedure DoChanged; override;
    procedure DoStop; override;
  public
    constructor Create(AOwner: TPersistent); override;
    property TabControl: TPageViewBase read FTabControl;
  end;

{ TTabControlAnimation }

constructor TTabControlAnimation.Create(AOwner: TPersistent);
begin
  ValidateInheritance(AOwner, TPageViewBase, False);
  inherited;
  FTabControl := TPageViewBase(AOwner);
end;

procedure TTabControlAnimation.DoChanged;
begin
  if ([csLoading, csUpdating] * TabControl.ComponentState) = [] then
    TabControl.SetInternalContentPosition(ViewportPosition.X);
end;

procedure TTabControlAnimation.DoStart;
begin
  inherited;
end;

procedure TTabControlAnimation.DoStop;
begin
  inherited;
  if TabControl.FIndexOfTargetTab <> -1 then begin
    TabControl.FIndexOfTargetTab := -1;
    TabControl.UpdateAnimation(TabControl.TabPosition in [TPageTabPosition.None, TPageTabPosition.Dots], 0, 0);
  end;
end;

{ TPageViewBase }

procedure TPageViewBase.AddOrInsertObject(const AObject: TFmxObject;
  const Index: Integer);
  function InternalInsert(const Control: TControl): Integer;
  var
    LCount: Integer;
  begin
    LCount := Control.ChildrenCount;
    Result := Min(Max(Index, 0), LCount);
    if Control = Self then
      if Result = LCount then
        inherited DoAddObject(AObject)
      else
        inherited DoInsertObject(Result, AObject)
    else
      if Result = LCount then
        Control.AddObject(AObject)
      else
        Control.InsertObject(Result, AObject);
  end;
var
  I, IndexOfLastTab, LIndex, NewTabIndex: Integer;
  OldActiveTab: TPageItem;
begin
  if AObject is TPageItem then begin
    if HasActiveTab then
      OldActiveTab := ActiveTab
    else
      OldActiveTab := nil;
    NewTabIndex := TabIndex;
    LIndex := InternalInsert(FContent);
    if ([csLoading, csDesigning] * ComponentState = []) then
    begin
      TPageItem(AObject).ShowAsDot := TabPosition in [TPageTabPosition.Dots, TPageTabPosition.None];
      RealignTabs;
      if not HasActiveTab then
        ActiveTab := TPageItem(AObject);
    end;
    if (LIndex = FContent.ControlsCount - 1) and (csLoading in ComponentState) then
    begin
      IndexOfLastTab := TabCount - 1;
    end;
    if [csLoading, csDestroying] * ComponentState = [] then
      if csDesigning in ComponentState then
      begin
        ActiveTab := TPageItem(AObject);
        GoToActiveTab;
      end
      else
      begin
        for I := 0 to TabCount - 1 do
          if OldActiveTab = Tabs[I] then
          begin
            NewTabIndex := I;
            Break;
          end;
        if FindVisibleTab(NewTabIndex, TFindKind.Current) then
          TabIndex := NewTabIndex
        else
          TabIndex := -1;
      end;
  end
  else if IsSpecialObject(AObject) then
    InternalInsert(Self)
  else if HasActiveTab then
    InternalInsert(ActiveTab)
  else
    InternalInsert(FNoItemsContent);
end;

constructor TPageViewBase.Create(AOwner: TComponent);
var
  DeviceService: IFMXDeviceService;
begin
  inherited;
  FTabPosition := TPageTabPosition.Top;
  FTabHeight := 26;
  FTabIndex := -1;

  FIndexOfTargetTab := -1;
  FAniCalculations := TTabControlAnimation.Create(Self);
  FAniCalculations.Animation := not (csDesigning in ComponentState);
  FAniCalculations.BoundsAnimation := False;
  FAniCalculations.TouchTracking := [ttHorizontal];
  FAniCalculations.AutoShowing := False;

  FContent := TPageContent.Create(Self);
  FContent.Name := 'TabControlContent_';
  FContent.Parent := Self;
  FContent.Stored := False;
  FContent.Locked := True;
  FContent.HitTest := False;
  FNoItemsContent := TControl.Create(nil);
  FNoItemsContent.Name := 'NoItemsContent_';
  FNoItemsContent.Parent := Self;
  FNoItemsContent.Stored := False;
  FNoItemsContent.Locked := True;
  FNoItemsContent.HitTest := False;

  if SupportsPlatformService(IFMXDeviceService, DeviceService) then
    FHasTouchScreen := TDeviceFeature.HasTouchScreen in DeviceService.GetFeatures;

  AutoCapture := True;
  SetBounds(0, 0, 200, 200);
  SetAcceptsControls(True);
end;

destructor TPageViewBase.Destroy;
begin
  {$IFDEF AUTOREFCOUNT}
  FAniCalculations.DisposeOf;
  {$ELSE}
  FAniCalculations.Free;
  {$ENDIF}
  inherited;
end;

procedure TPageViewBase.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TPageViewBase.DoRealign;
var
  I: Integer;
  LItem: TPageItem;
  LClientRect, ItemContentRect: TRectF;
  VisibleItemFound: Boolean;
  B: TControl;
begin
  if FDisableAlign then
    Exit;
  FDisableAlign := True;
  try
    RealignTabs;
    VisibleItemFound := False;
    LClientRect := FClientRect;
    case FTabPosition of
      TPageTabPosition.Top:
        LClientRect.Top := TabBarRect.Bottom;
      TPageTabPosition.Bottom:
        LClientRect.Bottom := TabBarRect.Top;
    end;
    if FContent.ControlsCount > 0 then
      for I := 0 to TabCount - 1 do
      begin
        LItem := Tabs[I];
        if not LItem.Visible then
          Continue;
        VisibleItemFound := True;
        LItem.Content.Visible := LItem.Index = TabIndex;
        ItemContentRect := Padding.PaddingRect(LClientRect);
        ItemContentRect := LItem.Content.Margins.PaddingRect(ItemContentRect);
        ItemContentRect.TopLeft := LItem.AbsoluteToLocal(LocalToAbsolute(ItemContentRect.TopLeft));
        ItemContentRect.BottomRight := LItem.AbsoluteToLocal(LocalToAbsolute(ItemContentRect.BottomRight));
        LItem.Content.BoundsRect := ItemContentRect;
        if LItem.Content.Visible then
          LItem.Content.BringToFront;
      end;
    if not VisibleItemFound then
      LClientRect := FClientRect;
    if (FTabPosition in [TPageTabPosition.Dots, TPageTabPosition.None]) and (FContent <> nil) then
      FContent.BringToFront;
  finally
    FDisableAlign := False;
  end;
end;

function TPageViewBase.FindVisibleTab(var Index: Integer;
  const FindKind: TFindKind): Boolean;

  function FindNextVisibleTab(const AFromIndex: Integer): Integer;
  var
    I: Integer;
  begin
    I := AFromIndex;
    repeat
      Inc(I);
    until (I >= TabCount) or Tabs[I].Visible;
    Result := I;
  end;

  function FindPrevVisibleTab(const AFromIndex: Integer): Integer;
  var
    I: Integer;
  begin
    I := AFromIndex;
    repeat
      Dec(I);
    until (I < 0) or Tabs[I].Visible;
    Result := I;
  end;

  function FindCurrentVisibleTab(const AFromIndex: Integer): Integer;
  begin
    if InRange(AFromIndex, 0, TabCount - 1) and Tabs[AFromIndex].Visible then
      Result := AFromIndex
    else
    begin
      Result := FindNextVisibleTab(AFromIndex);
      if Result >= TabCount then
        Result := FindPrevVisibleTab(AFromIndex);
    end;
  end;

var
  NormalizedTabIndex: Integer;
  NewIndex: Integer;
begin
  NormalizedTabIndex := EnsureRange(Index, -1, TabCount);
  case FindKind of
    TPageViewBase.TFindKind.Next:
      NewIndex := FindNextVisibleTab(NormalizedTabIndex);

    TPageViewBase.TFindKind.Back:
      NewIndex := FindPrevVisibleTab(NormalizedTabIndex);

    TPageViewBase.TFindKind.First:
      NewIndex := FindNextVisibleTab(-1);

    TPageViewBase.TFindKind.Last:
      NewIndex := FindPrevVisibleTab(TabCount);

    TPageViewBase.TFindKind.Current:
      NewIndex := FindCurrentVisibleTab(NormalizedTabIndex);
  else
    NewIndex := FindCurrentVisibleTab(NormalizedTabIndex);
  end;

  Result := InRange(NewIndex, 0, TabCount - 1);
  if Result then
    Index := NewIndex;
end;

function TPageViewBase.FindVisibleTab(const FindKind: TFindKind): Integer;
var
  I: Integer;
begin
  I := TabIndex;
  if FindVisibleTab(I, FindKind) then
    Result := I
  else
    Result := -1;
end;

function TPageViewBase.GetActiveTab: TPageItem;
begin
  if InRange(TabIndex, 0, TabCount - 1) then
    Result := Tabs[TabIndex]
  else
    Result := nil;
end;

function TPageViewBase.GetItem(const AIndex: Integer): TFmxObject;
begin
  Result := Tabs[AIndex];
end;

function TPageViewBase.GetItemsCount: Integer;
begin
  Result := TabCount;
end;

function TPageViewBase.GetTabContentPosition: Single;
begin
  Result := RoundByScale(FAniCalculations.ViewportPosition.X);
end;

function TPageViewBase.GetTabCount: Integer;
begin
  Result := FContent.ControlsCount;
end;

function TPageViewBase.GetTabItem(AIndex: Integer): TPageItem;
begin
  if InRange(AIndex, 0, FContent.ControlsCount - 1) then
    Result := FContent.Controls[AIndex] as TPageItem
  else
    Result := nil;
end;

procedure TPageViewBase.GoToActiveTab;
begin
  if (ActiveTab <> nil) and ActiveTab.Visible and (FIndexOfTargetTab <> ActiveTab.Index) then begin
    FIndexOfTargetTab := ActiveTab.Index;
    Realign;
  end;
end;

function TPageViewBase.HasActiveTab: Boolean;
begin
  Result := ActiveTab <> nil;
end;

function TPageViewBase.IsSpecialObject(const AObject: TFmxObject): Boolean;
begin
  Result := (AObject is TEffect) or (AObject is TAnimation) or AObject.Equals(FContent) or
    AObject.Equals(FNoItemsContent) or (AObject.Parent = Self);
end;

function TPageViewBase.IsStoredTabHeight: Boolean;
begin
  Result := FTabHeight <> 26.0;
end;

procedure TPageViewBase.Loaded;
var
  I: Integer;
  LShowAsDot: Boolean;
begin
  FRealigningTabs := True;
  try
    inherited;
    LShowAsDot := TabPosition in [TPageTabPosition.None, TPageTabPosition.Dots];
    for I := 0 to TabCount - 1 do  begin
      Tabs[I].ShowAsDot := LShowAsDot;
      Tabs[I].Resize;
    end;
  finally
    FRealigningTabs := False;
  end;
  RealignTabs;
end;

procedure TPageViewBase.RealignTabs;
const
  MinHeight = 5;
  InvisibleItemPos = $FFFF;
var
  I, VisibleIndex, DesignTabWidth, DesignTabHeight, BorderDotItem, CountVisibleTab: Integer;
  ColCount, RowCount, Surplus: Integer;
  TabPos: TPageTabPosition;
  DotItems: Boolean;
  CurX, CurY, TotalWidth, ActiveTabLeft, ActiveTabRight, LLeftOffset, LRightOffset, AutoWidth: Single;
  LItem: TPageItem;
  ItemRect: TRectF;
begin
  if FRealigningTabs then
    Exit;
  if ([csLoading, csDestroying] * ComponentState = []) then begin
    FRealigningTabs := True;
    try
      { initialization }
      DesignTabWidth := TPageItem.DotSize;
      DesignTabHeight := TPageItem.DotSize;
      TabPos := FTabPosition;
      DotItems := TabPos in [TPageTabPosition.None, TPageTabPosition.Dots];
      FClientRect := LocalRect;
      if FBackground <> nil then
        FClientRect := FBackground.Padding.PaddingRect(FClientRect);
      if DotItems then
        MaxHeight := DesignTabHeight
      else
        MaxHeight := MinHeight;
      TotalWidth := 0;
      CountVisibleTab := 0;
      ActiveTabRight := 0;
      ActiveTabLeft := 0;
      { Calculate count of visible tabs and sizes }
      for I := 0 to TabCount - 1 do begin
        LItem := Tabs[I];
        LItem.FDesignSelectionMarks := not DotItems;
        if LItem.Visible then
        begin
          if not DotItems then begin
            if LItem = ActiveTab then
              ActiveTabLeft := TotalWidth + LItem.FLeftOffset;
            MaxHeight := Max(MaxHeight, Trunc(LItem.Height + LItem.Margins.Top + LItem.Margins.Bottom));
            TotalWidth := TotalWidth + LItem.Width + LItem.Margins.Left + LItem.Margins.Right;
            if LItem = ActiveTab then
              ActiveTabRight := TotalWidth - LItem.FRightOffset;
          end;
          Inc(CountVisibleTab);
        end;
      end;
      BorderDotItem := Max(1, Max(DesignTabHeight, DesignTabWidth) div 10);
      Inc(DesignTabHeight, 2 * BorderDotItem);
      Inc(DesignTabWidth, 2 * BorderDotItem);
      { Initialization of left and right offsets }
      LLeftOffset := 0;
      LRightOffset := 0;
      VisibleIndex := 0;
      if not DotItems then
        for I := 0 to TabCount - 1 do
        begin
          LItem := Tabs[I];
          if LItem.Visible then
          begin
            if VisibleIndex = 0 then
              LLeftOffset := LItem.FLeftOffset;
            if VisibleIndex = CountVisibleTab - 1 then
              LRightOffset := LItem.FRightOffset;
            Inc(VisibleIndex);
          end;
        end;
      { Calculate rows and columns }
      ColCount := CountVisibleTab;
      RowCount := 1;
      if DotItems and (CountVisibleTab > 0) then begin
        if CountVisibleTab * DesignTabWidth > FClientRect.Width then
        begin
          ColCount := Max(1, Trunc(FClientRect.Width / DesignTabWidth));
          RowCount := (CountVisibleTab + ColCount - 1) div ColCount;
        end;
        TotalWidth := ColCount * DesignTabWidth - 2 * BorderDotItem;
        MaxHeight := Max(RowCount * DesignTabHeight, FTabHeight);
        AutoWidth := DesignTabWidth;
      end else begin
        AutoWidth := FClientRect.Width + LLeftOffset + LRightOffset;
        if CountVisibleTab = 0 then
          MaxHeight := 0
        else begin
          if FTabHeight > 0 then
            MaxHeight := FTabHeight;
          if FFullSize then begin
            AutoWidth := Trunc(Max(MinHeight, AutoWidth / CountVisibleTab));
            TotalWidth := AutoWidth * CountVisibleTab;
          end;
          TotalWidth := TotalWidth - LLeftOffset - LRightOffset;
        end;
      end;
      { Initialization of bounds }
      FTabContentSize := TSizeF.Create(TotalWidth, MaxHeight);
      FTabBarRect := TRectF.Create(TPointF.Zero, FClientRect.Width, MaxHeight);
      case TabPos of
        TPageTabPosition.Top:
          FTabBarRect.Offset(FClientRect.TopLeft);
        TPageTabPosition.Bottom:
          FTabBarRect.Offset(FClientRect.Left, FClientRect.Bottom - FTabBarRect.Height);
        TPageTabPosition.None, TPageTabPosition.Dots:
          FTabBarRect.Offset(FClientRect.Left, FClientRect.Bottom - FTabBarRect.Height - DesignTabHeight);
      end;
      FContent.BoundsRect := TabBarRect;
      FNoItemsContent.BoundsRect := TabBarRect;
      FNoItemsContent.Visible := not (FTabPosition in [TPageTabPosition.None, TPageTabPosition.Dots]);
      UpdateTabBarButtons;
      UpdateAnimation(DotItems, ActiveTabLeft, ActiveTabRight);
      CurX := TabContentPosition - LLeftOffset;
      if FFullSize then
        Surplus := Trunc(Max(FClientRect.Width - TotalWidth, 0))
      else
        Surplus := 0;
      if DotItems then
        if (not (csDesigning in ComponentState)) and (TabPos = TPageTabPosition.None) then
          CurY := InvisibleItemPos
        else
          CurY := Round(Max(0, MaxHeight - (DesignTabHeight * RowCount - 2 * BorderDotItem)) / 2)
      else
        CurY := 0;
      VisibleIndex := 0;
      { Update tab positions }
      for I := 0 to TabCount - 1 do
      begin
        LItem := Tabs[I];
        if not LItem.Visible then
          Continue;
        if DotItems then
        begin
          ItemRect := TRectF.Create(CurX, CurY, CurX + DesignTabWidth - 2 * BorderDotItem,
            CurY + DesignTabHeight - 2 * BorderDotItem);
          CurX := CurX + DesignTabWidth;
          if CurX >= TabContentPosition + TabContentSize.Width then
          begin
            CurX := TabContentPosition;
            CurY := CurY + DesignTabHeight;
          end;
        end
        else
        begin
          ItemRect := TRectF.Create(TPointF.Create(CurX, CurY), LItem.Width, MaxHeight);
          ItemRect := LItem.Margins.PaddingRect(ItemRect);
          if FFullSize then
            ItemRect.Width := AutoWidth - LItem.Margins.Left - LItem.Margins.Right;
          if VisibleIndex = CountVisibleTab - 1 then
            ItemRect.Width := ItemRect.Width + Surplus;
          CurX := CurX + ItemRect.Width + LItem.Margins.Left + LItem.Margins.Right;
        end;
        LItem.BoundsRect := ItemRect;
        Inc(VisibleIndex);
      end;
      { aligning }
      for I := 0 to TabCount - 1 do
        if Tabs[I].Visible then
          Tabs[I].UpdateLayoutControl;
      if not FDisableAlign then
        Realign;
    finally
      FRealigningTabs := False;
    end;
  end
  else
    FContent.Height := 0;
end;

function TPageViewBase.RoundByScale(const Value: Double): Single;
var
  Scale: Double;
begin
  Scale := 1;
  if (FContent <> nil) then begin
    if FContent.Scene <> nil then
      Scale := FContent.Scene.GetSceneScale;
    Scale := Scale * FContent.AbsoluteScale.X;
  end;
  Result := RoundTo(RoundTo(Value * Scale, 0) / Scale, -3);
end;

procedure TPageViewBase.SetActiveTab(const Value: TPageItem);
begin

end;

procedure TPageViewBase.SetFullSize(const Value: Boolean);
begin
  if FFullSize <> Value then begin
    FFullSize := Value;
    Realign;
  end;
end;

procedure TPageViewBase.SetInternalContentPosition(const Value: Double);
var
  NewValue: Single;
begin
  if not FRealigningTabs then
  begin
    NewValue := RoundByScale(Value);
    if not SameValue(NewValue, FInternalContentPosition, TEpsilon.Position) then
    begin
      FInternalContentPosition := NewValue;
      Realign;
    end;
  end;
end;

procedure TPageViewBase.SetTabContentPosition(const Value: Single);
begin

end;

procedure TPageViewBase.SetTabHeight(const Value: Single);
begin
  if FTabHeight <> Value then begin
    FTabHeight := Value;
    Realign;
  end;
end;

procedure TPageViewBase.SetTabIndex(const Value: Integer);
begin
  if FTabIndex <> Value then begin
    FTabIndex := Value;
  end;
end;

procedure TPageViewBase.SetTabPosition(const Value: TPageTabPosition);
begin
  if FTabPosition <> Value then begin
    FTabPosition := Value;
    Realign;
  end;
end;

procedure TPageViewBase.UpdateAnimation(const DotItems: Boolean;
  const ActiveTabLeft, ActiveTabRight: Single);
begin
end;

procedure TPageViewBase.UpdateTabBarButtons;
begin
end;

{ TPageItem }

function TPageItem.IsStoredLeftOffset: Boolean;
begin
  Result := FLeftOffset <> 0;
end;

function TPageItem.IsStoredRightOffset: Boolean;
begin
  Result := FRightOffset <> 0;
end;

procedure TPageItem.SetLeftOffset(const Value: Single);
begin
  if FLeftOffset <> Value then begin
    FLeftOffset := Value;
    FOwner.Realign;
  end;
end;

procedure TPageItem.SetRightOffset(const Value: Single);
begin
  if FRightOffset <> Value then begin
    FRightOffset := Value;
    FOwner.Realign;
  end;
end;

procedure TPageItem.SetShowAsDot(const Value: Boolean);
begin
  if FShowAsDot <> Value then
    FShowAsDot := Value;
end;

function TPageItem.TouchEnabled: Boolean;
begin
  Result := (FOwner <> nil) and FOwner.HasTouchScreen and not (csDesigning in ComponentState);
end;

procedure TPageItem.UpdateLayoutControl;
begin
end;

end.
