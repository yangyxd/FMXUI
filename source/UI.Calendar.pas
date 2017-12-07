{*******************************************************}
{                                                       }
{       FMX UI 日历组件单元                             }
{                                                       }
{       版权所有 (C) 2017 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Calendar;

interface

uses
  UI.Base, UI.Utils, UI.Ani, UI.Standard, UI.Calendar.Data,
  FMX.Effects, FMX.Text,
  {$IFDEF MSWINDOWS}Windows, UI.Debug, {$ENDIF}
  FMX.Objects, System.Math, System.Actions, System.DateUtils, FMX.Consts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, FMX.TextLayout,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement,
  FMX.Ani, FMX.StdActns;

type
  /// <summary>
  /// 日历选项
  /// </summary>
  TCalendarOption = (
     coShowNavigation, {显示导航}
     coShowWeek, {显示星期行}
     coShowBeforeAfter, {显示整行的日期，如果指定日期不在本月则显示为灰色}
     coCalendarWeeks, {显示周数}
     coTodayHighlight, {高亮今天}
     coShowTodayButton, {显示今天按钮}
     coShowClearButton, {显示清除按钮}
     coShowLunar, {显示农历}
     coShowTerm, {显示节气，需要开启 coShowLunar}
     coShowRowLines, {显示行线}
     coShowCosLines, {显示列线}
     coCosLinesOut, {当启用coShowCosLines时，启用本项时，列线延伸至星期行}
     coShowWeekLine, {在星期行与日期行之间显示分隔线}
     coEllipseSelect {当Drawable的IsCircle为True时，选中和今天的背景画成椭圆}
  );
  TCalendarOptions = set of TCalendarOption;

  TCalendarWeekItem = (
    Week0, Week1, Week2, Week3, Week4, Week5, Week6
  );
  TCalendarWeeks = set of TCalendarWeekItem;

type
  /// <summary>
  /// 日历视图类型
  /// </summary>
  TCalendarViewType = (Days {日}, Months {月}, Years {年}, Decades {10年});
  /// <summary>
  /// 星期开始值
  /// </summary>
  TWeekStart = Integer;

const
  CDefaultCalendarOptions = [coShowWeek, coShowNavigation, coTodayHighlight];

type
  /// <summary>
  /// 日历语言接口
  /// </summary>
  ICalendarLanguage = interface
    ['{16B861E6-87E7-4C50-9808-33D1C0CF249B}']
    function WeekStrList: TArray<string>;
    function MonthsStrList: TArray<string>;
    function DateToStr(const Value: TDate): string;
    function TodayStr: string;
    function ClearStr: string;
  end;

type
  /// <summary>
  /// 日历语言 - 中文
  /// </summary>
  TCalendarLanguage_CN = class(TComponent, ICalendarLanguage)
  public
    function WeekStrList: TArray<string>;
    function MonthsStrList: TArray<string>;
    function DateToStr(const Value: TDate): string;
    function TodayStr: string;
    function ClearStr: string;
  end;

  /// <summary>
  /// 日历语言 - 英文
  /// </summary>
  TCalendarLanguage_EN = class(TComponent, ICalendarLanguage)
  public
    function WeekStrList: TArray<string>;
    function MonthsStrList: TArray<string>;
    function DateToStr(const Value: TDate): string;
    function TodayStr: string;
    function ClearStr: string;
  end;

type
  /// <summary>
  /// 颜色属性
  /// </summary>
  TCalendarColor = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;

    FDefault: TAlphaColor;      // 默认
    FHovered: TAlphaColor;      // 默认悬停
    FPressed: TAlphaColor;      // 按下
    FToday: TAlphaColor;        // 今天
    FTodayHot: TAlphaColor;     // 今天悬停
    FSelected: TAlphaColor;     // 选中
    FSelectedHot: TAlphaColor;  // 选中悬停
    FEnabled: TAlphaColor;      // 无效
    FWeekend: TAlphaColor;      // 周末
    FWeekendHot: TAlphaColor;   // 周末悬停
    FOutMonth: TAlphaColor;     // 非本月
    FOutMonthHot: TAlphaColor;  // 非本月悬停
    FHighlight: TAlphaColor;    // 高亮

    FColorStoreState: Cardinal;
    function GetColorStoreState(const Index: Integer): Boolean;
    procedure SetColorStoreState(const Index: Integer; const Value: Boolean);
  private
    function ColorDefaultStored: Boolean;
    function ColorEnabledStored: Boolean;
    function ColorHoveredStored: Boolean;
    function ColorSelectedHotStored: Boolean;
    function ColorSelectedStored: Boolean;
    function ColorTodayHotStored: Boolean;
    function ColorTodayStored: Boolean;
    function ColorWeekendStored: Boolean;
    procedure SetDefault(const Value: TAlphaColor);
    procedure SetEnabled(const Value: TAlphaColor);
    procedure SetHovered(const Value: TAlphaColor);
    procedure SetSelected(const Value: TAlphaColor);
    procedure SetSelectedHot(const Value: TAlphaColor);
    procedure SetToday(const Value: TAlphaColor);
    procedure SetTodayHot(const Value: TAlphaColor);
    procedure SetWeekend(const Value: TAlphaColor);
    function ColorOutMonthHotStored: Boolean;
    function ColorOutMonthStored: Boolean;
    function ColorWeekendHotStored: Boolean;
    procedure SetOutMonth(const Value: TAlphaColor);
    procedure SetOutMonthHot(const Value: TAlphaColor);
    procedure SetWeekendHot(const Value: TAlphaColor);
    function ColorHighlightStored: Boolean;
    procedure SetHighlight(const Value: TAlphaColor);
    function ColorPressedStored: Boolean;
    procedure SetPressed(const Value: TAlphaColor);
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(const ADefaultColor: TAlphaColor = TAlphaColorRec.Black);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property DefaultChange: Boolean index 1 read GetColorStoreState write SetColorStoreState;
    property HoveredChange: Boolean index 2 read GetColorStoreState write SetColorStoreState;
    property TodayChange: Boolean index 3 read GetColorStoreState write SetColorStoreState;
    property TodayHotChange: Boolean index 4 read GetColorStoreState write SetColorStoreState;
    property SelectedChange: Boolean index 5 read GetColorStoreState write SetColorStoreState;
    property SelectedHotChange: Boolean index 6 read GetColorStoreState write SetColorStoreState;
    property EnabledChange: Boolean index 7 read GetColorStoreState write SetColorStoreState;
    property WeekendChange: Boolean index 8 read GetColorStoreState write SetColorStoreState;
    property WeekendHotChange: Boolean index 9 read GetColorStoreState write SetColorStoreState;
    property OutMonthChange: Boolean index 10 read GetColorStoreState write SetColorStoreState;
    property OutMonthHotChange: Boolean index 11 read GetColorStoreState write SetColorStoreState;
    property HighlightChange: Boolean index 12 read GetColorStoreState write SetColorStoreState;
    property PressedChange: Boolean index 13 read GetColorStoreState write SetColorStoreState;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Default: TAlphaColor read FDefault write SetDefault stored ColorDefaultStored;
    property Pressed: TAlphaColor read FPressed write SetPressed stored ColorPressedStored;
    property Hovered: TAlphaColor read FHovered write SetHovered stored ColorHoveredStored;
    property Today: TAlphaColor read FToday write SetToday stored ColorTodayStored;
    property TodayHot: TAlphaColor read FTodayHot write SetTodayHot stored ColorTodayHotStored;
    property Selected: TAlphaColor read FSelected write SetSelected stored ColorSelectedStored;
    property SelectedHot: TAlphaColor read FSelectedHot write SetSelectedHot stored ColorSelectedHotStored;
    property Enabled: TAlphaColor read FEnabled write SetEnabled stored ColorEnabledStored;
    property Weekend: TAlphaColor read FWeekend write SetWeekend stored ColorWeekendStored;
    property WeekendHot: TAlphaColor read FWeekendHot write SetWeekendHot stored ColorWeekendHotStored;
    property OutMonth: TAlphaColor read FOutMonth write SetOutMonth stored ColorOutMonthStored;
    property OutMonthHot: TAlphaColor read FOutMonthHot write SetOutMonthHot stored ColorOutMonthHotStored;
    property Highlight: TAlphaColor read FHighlight write SetHighlight stored ColorHighlightStored;
  end;

  TCalendarTextSettings = class(TTextSettingsBase)
  private
    FColor: TCalendarColor;
    procedure SetColor(const Value: TCalendarColor);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function GetStateColor(const State: TViewState): TAlphaColor; override;
  published
    property Color: TCalendarColor read FColor write SetColor;
    property Font;
    property PrefixStyle;
    property Trimming;
    property Gravity default TLayoutGravity.Center;
  end;

  /// <summary>
  /// 日历可绘制对象
  /// </summary>
  TCalendarDrawable = class(TDrawableBase)
  private
    FIsCircle: Boolean;
    function GetValue(const Index: Integer): TViewBrush;
    procedure SetValue(const Index: Integer; const Value: TViewBrush);
    procedure SetIsCircle(const Value: Boolean);
  published
    property XRadius;
    property YRadius;
    property Corners;
    property CornerType;
    property IsCircle: Boolean read FIsCircle write SetIsCircle default False;
    property ItemHovered: TViewBrush index 0 read GetValue write SetValue;
    property ItemToday: TViewBrush index 1 read GetValue write SetValue;
    property ItemTodayHot: TViewBrush index 2 read GetValue write SetValue;
    property ItemSelected: TViewBrush index 3 read GetValue write SetValue;
    property ItemSelectedHot: TViewBrush index 4 read GetValue write SetValue;
    property ItemHighlight: TViewBrush index 5 read GetValue write SetValue;
    property ItemWeekNav: TViewBrush index 6 read GetValue write SetValue;
  end;

type
  TCalendarViewBase = class(TView)
  private const
    CDefaultRowHeihgt = 45;
    CDefaultRowLunarHeight = 20;
    CDefaultWeeksWidth = 40;  // 周数列宽度
    CDefaultDividerColor = $ffc0c0c0;
    CDefaultNextUpW = 30;
  private
    [Weak] FLanguage: ICalendarLanguage;
    [Weak] FInnerLanguage: ICalendarLanguage;
    FOptions: TCalendarOptions;
    FStartView: TCalendarViewType;
    FStartDate: TDate;
    FEndDate: TDate;
    FWeekStart: TWeekStart;
    FDaysOfWeekDisabled: TCalendarWeeks;
    FDaysOfWeekHighlighted: TCalendarWeeks;

    FTextSettings: TCalendarTextSettings;
    FTextSettingsOfLunar: TCalendarTextSettings;
    FTextSettingsOfTitle: TSimpleTextSettings;
    FTextSettingsOfWeeks: TSimpleTextSettings;

    FDrawable: TCalendarDrawable;
    FDividerBrush: TBrush;

    FRowPadding: Single;
    FRowHeihgt: Single;
    FRowLunarHeight: Single;
    FRowLunarPadding: Single;

    FDivider: TAlphaColor; // 分隔线颜色
    FInFitSize: Boolean;

    FOnValueChange: TNotifyEvent;

    FRangeOfNavigation: TRectF;
    FRangeOfDays: TRectF;

    procedure SetOptions(const Value: TCalendarOptions);
    procedure SetEndDate(const Value: TDate);
    procedure SetLanguage(const Value: ICalendarLanguage);
    procedure SetStartDate(const Value: TDate);
    procedure SetStartView(const Value: TCalendarViewType);
    procedure SetWeekStart(const Value: TWeekStart);
    function IsEndDateStored: Boolean;
    function IsStartDateStored: Boolean;
    function GetLanguage: ICalendarLanguage;
    procedure SetDaysOfWeekDisabled(const Value: TCalendarWeeks);
    procedure SetDaysOfWeekHighlighted(const Value: TCalendarWeeks);
    procedure SetTextSettings(const Value: TCalendarTextSettings);
    procedure SetTextSettingsOfLunar(const Value: TCalendarTextSettings);
    procedure SetTextSettingsOfTitle(const Value: TSimpleTextSettings);
    procedure SetTextSettingsOfWeeks(const Value: TSimpleTextSettings);
    procedure SetDrawable(const Value: TCalendarDrawable);
    function GetAutoSize: Boolean;
    function IsStoredRowHeihgt: Boolean;
    function IsStoredRowLunarHeight: Boolean;
    function IsStoredRowLunarPadding: Boolean;
    function IsStoredRowPadding: Boolean;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetRowHeihgt(const Value: Single);
    procedure SetRowLunarHeight(const Value: Single);
    procedure SetRowLunarPadding(const Value: Single);
    procedure SetRowPadding(const Value: Single);
    procedure SetValue(const Value: TDate);
    procedure SetDivider(const Value: TAlphaColor);
  protected
    FValue: TDate;

    FCurDayOfWeek: Integer; // 本月第一天的星期数
    FCurFirst: Integer;  // 当前显示的第一天
    FCurLast: Integer;   // 当前显示的最后一天
    FCurRows: Integer; // 需要显示的行数
    FCurDrawS: Integer; // 当前绘制的第一天

    FCurHotDate: Integer; // 当前鼠标指向的日期

    function IsAutoSize: Boolean; override;
    procedure DoOptionsChange; virtual;
    procedure DoChange; virtual;
    procedure DoTextSettingsChange(Sender: TObject);
    procedure DoDrawableChange(Sender: TObject);
    procedure DoDateChange(); virtual;

    procedure DoAutoSize;
    procedure InitDividerBrush;

    procedure ParseValue(const Value: TDate); virtual;
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure DoRecalcSize(var AWidth, AHeight: Single); override;

    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    
    procedure PaintBackground; override;
    procedure PaintToCanvas(Canvas: TCanvas);

    procedure DoDrawNavigation(Canvas: TCanvas; const R: TRectF);
    procedure DoDrawWeekRow(Canvas: TCanvas; const R: TRectF);
    procedure DoDrawDatesRow(Canvas: TCanvas; const R: TRectF; WeekRowTop: Single);
    procedure DoDrawItemBackground(Canvas: TCanvas; ABrush: TBrush; const R: TRectF; IsCircle: Boolean);
    procedure DoDrawButton(Canvas: TCanvas; const R: TRectF; const Text: string; const ID: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// 自动大小
    /// </summary>
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;

    /// <summary>
    /// 分隔线颜色
    /// </summary>
    property Divider: TAlphaColor read FDivider write SetDivider default CDefaultDividerColor;

    /// <summary>
    /// 字体设置
    /// </summary>
    property TextSettings: TCalendarTextSettings read FTextSettings write SetTextSettings;
    /// <summary>
    /// 字体设置 - 农历和节日
    /// </summary>
    property TextSettingsOfLunar: TCalendarTextSettings read FTextSettingsOfLunar write SetTextSettingsOfLunar;
    /// <summary>
    /// 字体设置 - 顶部星期和导航按钮
    /// </summary>
    property TextSettingsOfTitle: TSimpleTextSettings read FTextSettingsOfTitle write SetTextSettingsOfTitle;
    /// <summary>
    /// 字体设置 - 周数
    /// </summary>
    property TextSettingsOfWeeks: TSimpleTextSettings read FTextSettingsOfWeeks write SetTextSettingsOfWeeks;

    /// <summary>
    /// 绘制背景色
    /// </summary>
    property Drawable: TCalendarDrawable read FDrawable write SetDrawable;

    /// <summary>
    /// 当前选择时间
    /// </summary>
    property DateTime: TDate read FValue write SetValue;

    /// <summary>
    /// 选项
    /// </summary>
    property Options: TCalendarOptions read FOptions write SetOptions;
    /// <summary>
    /// 开始时显示的视图类型
    /// </summary>
    property StartView: TCalendarViewType read FStartView write SetStartView;
    /// <summary>
    /// 限定的开始日期
    /// </summary>
    property StartDate: TDate read FStartDate write SetStartDate stored IsStartDateStored;
    /// <summary>
    /// 限定的结束日期
    /// </summary>
    property EndDate: TDate read FEndDate write SetEndDate stored IsEndDateStored;
    /// <summary>
    /// 星期显示从星期几开始，默认为0，从星期天开始
    /// </summary>
    property WeekStart: TWeekStart read FWeekStart write SetWeekStart;
    /// <summary>
    /// 语言接口
    /// </summary>
    property Language: ICalendarLanguage read GetLanguage write SetLanguage;


    /// <summary>
    /// 禁止选择的日期于星期几
    /// </summary>
    property DaysOfWeekDisabled: TCalendarWeeks read FDaysOfWeekDisabled write SetDaysOfWeekDisabled default [];
    /// <summary>
    /// 高亮显示的日期于星期几
    /// </summary>
    property DaysOfWeekHighlighted: TCalendarWeeks read FDaysOfWeekHighlighted write SetDaysOfWeekHighlighted default [];


    /// <summary>
    /// 行间距
    /// </summary>
    property RowPadding: Single read FRowPadding write SetRowPadding stored IsStoredRowPadding;
    /// <summary>
    /// 行高
    /// </summary>
    property RowHeihgt: Single read FRowHeihgt write SetRowHeihgt stored IsStoredRowHeihgt;
    /// <summary>
    /// 农历和节日行高
    /// </summary>
    property RowLunarHeight: Single read FRowLunarHeight write SetRowLunarHeight stored IsStoredRowLunarHeight;
    /// <summary>
    /// 农历和节日与日期之间的间距
    /// </summary>
    property RowLunarPadding: Single read FRowLunarPadding write SetRowLunarPadding stored IsStoredRowLunarPadding;

    /// <summary>
    /// 选择的日期改变
    /// </summary>
    property OnChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  end;

type
  /// <summary>
  /// 日历视图组件
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TCalendarView = class(TCalendarViewBase)
  published
    property CanFocus default True;
    property HitTest default True;
    property Clickable default True;

    property AutoSize;
    property Options default CDefaultCalendarOptions;
    property StartView default TCalendarViewType.Days;
    property StartDate;
    property EndDate;
    property WeekStart default 0;
    property Language;
    property DaysOfWeekDisabled;
    property DaysOfWeekHighlighted;

    property Divider;
    property Drawable;
    property DateTime;

    property RowPadding;
    property RowHeihgt;
    property RowLunarHeight;
    property RowLunarPadding;

    property TextSettings;
    property TextSettingsOfLunar;
    property TextSettingsOfTitle;
    property TextSettingsOfWeeks;

    property OnChange;
  end;

implementation

const
  BID_Today = -5;
  BID_Next = -1;
  BID_Up = -2;
  BID_Navigation = -3;
  BID_Clear = -4;

type
  TTmpSimpleTextSettings = class(TSimpleTextSettings);

var
  DefaultLanguage: TCalendarLanguage_EN;

{ TCalendarLanguage_CN }

function TCalendarLanguage_CN.ClearStr: string;
begin
  Result := '清除';
end;

function TCalendarLanguage_CN.DateToStr(const Value: TDate): string;
begin
  Result := FormatDateTime('yyyy年mm月', Value);
end;

function TCalendarLanguage_CN.MonthsStrList: TArray<string>;
begin
  Result := ['1月', '2月', '3月', '4月', '5月', '6月',
    '7月', '8月', '9月', '10月', '11月', '12月'];
end;

function TCalendarLanguage_CN.TodayStr: string;
begin
  Result := '今天';
end;

function TCalendarLanguage_CN.WeekStrList: TArray<string>;
begin
  Result := ['日', '一', '二', '三', '四', '五', '六'];
end;

{ TCalendarLanguage_EN }

function TCalendarLanguage_EN.ClearStr: string;
begin
  Result := 'Clear';
end;

function TCalendarLanguage_EN.DateToStr(const Value: TDate): string;
const
  LMonths: array [0..11] of string = (
    'January', 'February', 'March', 'April',
    'May', 'June', 'July', 'August',
    'September', 'October', 'November', 'December');
var
  Y, M, D: Word;
begin
  DecodeDate(Value, Y, M, D);
  Result := Format('%s %d', [LMonths[M - 1], Y]);
end;

function TCalendarLanguage_EN.MonthsStrList: TArray<string>;
begin
  Result := ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
end;

function TCalendarLanguage_EN.TodayStr: string;
begin
  Result := 'Today';
end;

function TCalendarLanguage_EN.WeekStrList: TArray<string>;
begin
  Result := ['Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa'];
end;

{ TCalendarViewBase }

function TCalendarViewBase.CanRePaintBk(const View: IView;
  State: TViewState): Boolean;
begin
  Result := (FTextSettings.FColor.FPressed <> 0) or inherited;
end;

constructor TCalendarViewBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLanguage := nil;
  FOptions := CDefaultCalendarOptions;

  FTextSettings := TCalendarTextSettings.Create(Self);
  FTextSettingsOfLunar := TCalendarTextSettings.Create(Self);
  FTextSettingsOfTitle := TSimpleTextSettings.Create(Self);
  FTextSettingsOfWeeks := TSimpleTextSettings.Create(Self);

  FTextSettingsOfLunar.FColor.FDefault := $ff606060;
  FTextSettingsOfLunar.FColor.FColorStoreState := 0;

  FDrawable := TCalendarDrawable.Create(Self);
  FDrawable.ItemToday.ChangeToSolidColor($ffffdb99);
  FDrawable.ItemSelected.ChangeToSolidColor($ff286090);
  FDrawable.ItemHovered.ChangeToSolidColor($fff5f5f5);
  FDrawable.ItemTodayHot.ChangeToSolidColor($ffffc966);
  FDrawable.ItemSelectedHot.ChangeToSolidColor($ff204d74);
  FDrawable.ItemHighlight.ChangeToSolidColor($bfd9edf7);

  FRowPadding := 0;
  FRowHeihgt := CDefaultRowHeihgt;
  FRowLunarHeight := CDefaultRowLunarHeight;
  FRowLunarPadding := 0;

  FDivider := CDefaultDividerColor;

  SetAcceptsControls(False);
  Clickable := True;
  CanFocus := True;
end;

destructor TCalendarViewBase.Destroy;
begin
  FreeAndNil(FTextSettings);
  FreeAndNil(FTextSettingsOfLunar);
  FreeAndNil(FTextSettingsOfTitle);
  FreeAndNil(FTextSettingsOfWeeks);
  FreeAndNil(FDrawable);
  FreeAndNil(FDividerBrush);
  inherited Destroy;
end;

procedure TCalendarViewBase.DoAutoSize;
var
  W, H: Single;
begin
  if FInFitSize or (not FAdjustViewBounds) or (csLoading in ComponentState) then
    Exit;
  if TextSettings.WordWrap then begin // 只有需要自动换行时，才需要判断父级组件的宽度
    W := GetParentMaxWidth;
    H := GetParentMaxHeight;
  end else begin
    W := 0;
    H := 0;
  end;
  if (MaxHeight > 0) and (W > MaxWidth) then
    W := MaxWidth;
  if (MaxHeight > 0) and (H > MaxHeight) then
    H := MaxHeight;
  if W <= 0 then
    W := FSize.Width;
  if H <= 0 then
    H := FSize.Height;
  DoChangeSize(W, H);
  if (W <> FSize.Width) or (H <> FSize.Height) then begin
    FInFitSize := True;
    SetSize(W, H, False);
    FInFitSize := False;
  end;
end;

procedure TCalendarViewBase.DoChange;
begin
  Invalidate;
end;

procedure TCalendarViewBase.DoDateChange;
begin
  DoChange;
  if Assigned(FOnValueChange) then
    FOnValueChange(Self);
end;

procedure TCalendarViewBase.DoDrawableChange(Sender: TObject);
begin
  if IsAutoSize and (csDesigning in ComponentState) then begin
    DoAutoSize;
  end else
    DoChange;
end;

procedure TCalendarViewBase.DoDrawButton(Canvas: TCanvas; const R: TRectF;
  const Text: string; const ID: Integer);
var
  LColor: TAlphaColor;
begin
  LColor := FTextSettingsOfTitle.Color;
  if FCurHotDate = ID then begin
    DoDrawItemBackground(Canvas, FDrawable.FDefault, R, False);
    if (DrawState = TViewState.Pressed) and (FTextSettings.FColor.FPressed <> 0) then
      TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := FTextSettings.FColor.FPressed
    else if FTextSettings.FColor.FHovered <> 0 then
      TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := FTextSettings.FColor.FHovered;
  end;
  FTextSettingsOfTitle.Draw(Canvas, Text, R, Opacity, TViewState.None, TLayoutGravity.Center);
  TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := LColor;
end;

procedure TCalendarViewBase.DoDrawDatesRow(Canvas: TCanvas; const R: TRectF; WeekRowTop: Single);
var
  X, Y, W, LX, LunarHeight: Single;
  S, LS, LE, LSelect, LToday, Offset, Week: Integer;
  I, J, D, L: Integer;
  Lunar, BeforeAfter, IsEnd: Boolean;
  LColor: TAlphaColor;
  LR: TRectF;
begin
  W := R.Width;
  LX := R.Left;
  Y := R.Top;
  if coCalendarWeeks in FOptions then begin
    W := W - CDefaultWeeksWidth;
    LX := LX + CDefaultWeeksWidth;
  end;
  W := W / 7;

  if coShowLunar in FOptions then
    LunarHeight := FRowLunarHeight + FRowLunarPadding
  else
    LunarHeight := 0;

  LS := FCurFirst;
  LE := FCurLast;
  Offset := (FCurDayOfWeek - FWeekStart) mod 7;
  if Offset < 0 then
    Inc(Offset, 7);
  S := LS - Offset;
  D := DayOf(TDateTime(S));

  FCurDrawS := S;  // 记录当前显示的开始日期

  LSelect := Trunc(FValue);
  LToday := Trunc(Now);

  BeforeAfter := coShowBeforeAfter in FOptions;
  Lunar := coShowLunar in FOptions;

  if (coShowRowLines in FOptions) then begin
    L := 1;
  end else
    L := 0;

  if Assigned(FDividerBrush) then
    FDividerBrush.Color := FDivider
  else
    L := 0;

  IsEnd := False;

  for J := 1 to FCurRows do begin
    X := LX;
    for I := 0 to 6 do begin
      if BeforeAfter or ((S >= LS) and (S <= LE)) then begin

        Week := (I + FWeekStart) mod 7;
        LColor := 0;
        LR := RectF(X, Y, X + W - 1, Y + FRowHeihgt + LunarHeight);

        // 高亮显示背景
        if (TCalendarWeekItem(Week) in DaysOfWeekHighlighted) and Assigned(FDrawable.FChecked) then
          FDrawable.FillRect(Canvas, LR, 0, 0, FDrawable.Corners, GetAbsoluteOpacity, FDrawable.FChecked);

        // 画日期
        if TCalendarWeekItem(Week) in DaysOfWeekDisabled then begin  // 禁止选择
          LColor := FTextSettings.FColor.FEnabled
        end else if S = LSelect then begin  // 选中
          if (FCurHotDate = S) then begin
            DoDrawItemBackground(Canvas, FDrawable.FHovered, LR, FDrawable.IsCircle);
            LColor := FTextSettings.FColor.FSelectedHot;
          end else begin
            DoDrawItemBackground(Canvas, FDrawable.FSelected,  LR, FDrawable.IsCircle);
            LColor := FTextSettings.FColor.FSelected;
          end;
        end else if (S = LToday) and (coTodayHighlight in FOptions) then begin // 今天
          if (FCurHotDate = S) then begin
            DoDrawItemBackground(Canvas, FDrawable.FFocused, LR, FDrawable.IsCircle);
            LColor := FTextSettings.FColor.FTodayHot;
          end else begin
            DoDrawItemBackground(Canvas, FDrawable.FPressed, LR, FDrawable.IsCircle);
            LColor := FTextSettings.FColor.FToday;
          end;
        end else if (S < LS) or (S > LE) then begin  // 不是本月
          if (FCurHotDate = S) then begin
            DoDrawItemBackground(Canvas, FDrawable.FDefault, LR, FDrawable.IsCircle);
            LColor := FTextSettings.FColor.FOutMonthHot;
          end else
            LColor := FTextSettings.FColor.FOutMonth;
        end else if (Week = 0) or (Week = 6) then begin // 周末
          if (FCurHotDate = S) then begin
            DoDrawItemBackground(Canvas, FDrawable.FDefault, LR, FDrawable.IsCircle);
            if IsPressed then
              LColor := FTextSettings.FColor.FPressed
            else
              LColor := FTextSettings.FColor.FWeekendHot;
          end else
            LColor := FTextSettings.FColor.FWeekend;
        end else if (FCurHotDate = S) then begin   // 悬停
          DoDrawItemBackground(Canvas, FDrawable.FDefault, LR, FDrawable.IsCircle);
          if IsPressed then
            LColor := FTextSettings.FColor.FPressed
          else
            LColor := FTextSettings.FColor.FHovered;
        end else if TCalendarWeekItem(Week) in DaysOfWeekHighlighted then begin  // 高亮显示
          LColor := FTextSettings.FColor.FHighlight;
        end;

        if LColor = 0 then
          LColor := FTextSettings.FColor.FDefault;

        FTextSettings.FillText(Canvas, RectF(X, Y, X + W, Y + FRowHeihgt - L),
          IntToStr(D), Opacity, LColor,
          FTextSettings.FillTextFlags, nil, 0, TTextAlign.Center);

      end;
      Inc(S);
      X := X + W;

      if (not IsEnd) and ((S = LS) or (S > LE)) then begin
        D := 1;
        if S > LE then
          IsEnd := True;
      end else
        Inc(D);
    end;


    Y := Y + FRowHeihgt + FRowPadding;
    if Lunar then
      Y := Y + FRowLunarHeight + FRowLunarPadding;

    if L > 0 then // 画行线
      Canvas.FillRect(RectF(R.Left, Y - L, R.Right, Y), 0, 0, [], Opacity, FDividerBrush);
  end;

  // 画列线
  if Assigned(FDividerBrush) and (coShowCosLines in FOptions) then begin
    X := LX;
    Y := R.Top;
    if (coCosLinesOut in FOptions) and (WeekRowTop <> -$FFFF) then
      Y := WeekRowTop;
    for I := 1 to 6 do begin
      X := X + W;
      Canvas.FillRect(RectF(X - 1, Y, X, R.Bottom), 0, 0, [], Opacity, FDividerBrush);
    end;
  end;
end;

procedure TCalendarViewBase.DoDrawItemBackground(Canvas: TCanvas;
  ABrush: TBrush; const R: TRectF; IsCircle: Boolean);
var
  LW, LH, LR: Single;
begin
  if ABrush = nil then
    Exit;
  if IsCircle then begin
    LW := R.Width;
    LH := R.Height;
    if coEllipseSelect in FOptions then begin
      FDrawable.FillArc(Canvas, PointF(R.Left + LW * 0.5, R.Top + LH * 0.5), PointF(LW * 0.5, LH * 0.5), 0, 360, Opacity, ABrush);
    end else  begin
      LR := Min(LW, LH) * 0.5;
      FDrawable.FillArc(Canvas, PointF(R.Left + LW * 0.5, R.Top + LH * 0.5), PointF(LR, LR), 0, 360, Opacity, ABrush);
    end;
  end else
    FDrawable.DrawBrushTo(Canvas, ABrush, R);
end;

procedure TCalendarViewBase.DoDrawNavigation(Canvas: TCanvas; const R: TRectF);
var
  LR: TRectF;
  LColor, SColor: TAlphaColor;
  LOpacity: Single;
begin
  LOpacity := Opacity;
  LColor := FTextSettingsOfTitle.Color;
  LR := RectF(R.Left, R.Top, R.Left + CDefaultNextUpW, R.Bottom - 1);

  if (DrawState = TViewState.Pressed) and (FTextSettings.FColor.FPressed <> 0) then
    SColor := FTextSettings.FColor.FPressed
  else if FTextSettings.FColor.FHovered <> 0 then
    SColor := FTextSettings.FColor.FHovered
  else
    SColor := LColor;

  if FCurHotDate = BID_Up then begin
    DoDrawItemBackground(Canvas, FDrawable.FDefault, LR, FDrawable.IsCircle);
    TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := SColor;
  end;
  FTextSettingsOfTitle.Draw(Canvas, #$00ab, LR, LOpacity, TViewState.None, TLayoutGravity.Center);

  LR := RectF(LR.Right, R.Top, R.Right - CDefaultNextUpW, R.Bottom - 1);
  if FCurHotDate = BID_Navigation then begin
    DoDrawItemBackground(Canvas, FDrawable.FDefault, LR, False);
    TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := SColor;
  end else
    TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := LColor;
  FTextSettingsOfTitle.Draw(Canvas, FInnerLanguage.DateToStr(FValue), LR, LOpacity, TViewState.None, TLayoutGravity.Center);

  LR := RectF(LR.Right, R.Top, R.Right, R.Bottom - 1);
  if FCurHotDate = BID_Next then begin
    DoDrawItemBackground(Canvas, FDrawable.FDefault, LR, FDrawable.IsCircle);
    TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := SColor;
  end else
    TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := LColor;
  FTextSettingsOfTitle.Draw(Canvas, #$00bb, LR, LOpacity, TViewState.None, TLayoutGravity.Center);

  TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := LColor;
end;

procedure TCalendarViewBase.DoDrawWeekRow(Canvas: TCanvas; const R: TRectF);
var
  X, W, L: Single;
  I, J: Integer;
  Items: TArray<string>;
  LColor: TAlphaColor;
begin
  W := R.Width;
  X := R.Left;
  if coCalendarWeeks in FOptions then begin
    W := W - CDefaultWeeksWidth;
    X := X + CDefaultWeeksWidth;
  end;
  W := W / 7;
  Items := FInnerLanguage.WeekStrList;
  if Assigned(FDividerBrush) and ((coShowWeekLine in FOptions) or (coShowRowLines in FOptions)) then
    L := 1
  else
    L := 0;

  if Assigned(FDrawable.FEnabled) then
    FDrawable.DrawBrushTo(Canvas, FDrawable.FEnabled, R);

  LColor := FTextSettingsOfTitle.Color;
  for I := 0 to 6 do begin
    J := (I + FWeekStart) mod 7;
    if ((J = 0) or (J = 6)) and (FTextSettings.FColor.FWeekend <> 0) then
      TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := FTextSettings.FColor.FWeekend
    else
      TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := LColor;
    FTextSettingsOfTitle.Draw(Canvas, Items[J],
      RectF(X, R.Top, X + W, R.Bottom - L), Opacity, TViewState.None, TLayoutGravity.Center);
    X := X + W;
  end;
  TTmpSimpleTextSettings(FTextSettingsOfTitle).FColor := LColor;

  if L > 0 then begin
    FDividerBrush.Color := FDivider;
    Canvas.FillRect(RectF(R.Left, R.Bottom - L, R.Right, R.Bottom), 0, 0, [], Opacity, FDividerBrush);
  end;
end;

procedure TCalendarViewBase.DoMouseLeave;
begin
  FCurHotDate := 0;
  inherited DoMouseLeave;
end;

procedure TCalendarViewBase.DoOptionsChange;
begin
  InitDividerBrush();
  if IsAutoSize then
    DoAutoSize
  else
    Invalidate;
end;

procedure TCalendarViewBase.DoRecalcSize(var AWidth, AHeight: Single);
var
  W, H, V: Single;
begin
  if FInFitSize or (Scene = nil) or (not Assigned(FTextSettings)) or (not AutoSize) then
    Exit;
  FInFitSize := True;
  H := Padding.Top + Padding.Bottom;
  W := 300 + Padding.Left + Padding.Right;
  if coCalendarWeeks in FOptions then
    W := W + CDefaultWeeksWidth;

  if Assigned(FBackground) and Assigned(TDrawableBorder(FBackground)._Border) and
    (TDrawableBorder(FBackground)._Border.Style <> TViewBorderStyle.None) then
  begin
    H := H + TDrawableBorder(FBackground)._Border.Width * 2;
    W := W + TDrawableBorder(FBackground)._Border.Width * 2;
  end;

  if AWidth > W then
    W := AWidth;

  if coShowNavigation in FOptions then
    H := H + FRowHeihgt + FRowPadding;
  if coShowWeek in FOptions then
    H := H + FRowHeihgt + FRowPadding;

  V := FRowHeihgt + FRowPadding;
  if coShowLunar in FOptions then
    V := V + FRowLunarHeight + FRowLunarPadding;
    
  H := H + V * FCurRows;

  if (coShowTodayButton in FOptions) or (coShowClearButton in FOptions) then
    H := H + FRowHeihgt + FRowPadding;

  AWidth := W;
  AHeight := H;
  FInFitSize := False;
end;

procedure TCalendarViewBase.DoTextSettingsChange(Sender: TObject);
begin
  if TTextSettingsBase(Sender).IsSizeChange then begin
    if IsAutoSize then 
      DoAutoSize;
  end;
  Repaint;
  if TTextSettingsBase(Sender).IsEffectsChange then
    UpdateEffects;
end;

function TCalendarViewBase.GetAutoSize: Boolean;
begin
  Result := FTextSettings.AutoSize;
end;

function TCalendarViewBase.GetLanguage: ICalendarLanguage;
begin
  Result := FLanguage;
end;

procedure TCalendarViewBase.InitDividerBrush;
begin
  if ((coShowWeekLine in FOptions) or (coShowRowLines in FOptions) or (coShowCosLines in FOptions)) and
    (FDivider and $FF000000 <> 0)
  then begin
    if not Assigned(FDividerBrush) then
      FDividerBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Null);
  end else
    FreeAndNil(FDividerBrush);
end;

function TCalendarViewBase.IsAutoSize: Boolean;
begin
  Result := AutoSize and (HeightSize <> TViewSize.FillParent);
end;

function TCalendarViewBase.IsEndDateStored: Boolean;
begin
  Result := FEndDate <> 0;
end;

function TCalendarViewBase.IsStartDateStored: Boolean;
begin
  Result := FStartDate <> 0;
end;

function TCalendarViewBase.IsStoredRowHeihgt: Boolean;
begin
  Result := FRowHeihgt <> CDefaultRowHeihgt;
end;

function TCalendarViewBase.IsStoredRowLunarHeight: Boolean;
begin
  Result := FRowLunarHeight <> CDefaultRowLunarHeight;
end;

function TCalendarViewBase.IsStoredRowLunarPadding: Boolean;
begin
  Result := FRowLunarPadding <> 0;
end;

function TCalendarViewBase.IsStoredRowPadding: Boolean;
begin
  Result := FRowPadding <> 0;
end;

procedure TCalendarViewBase.Loaded;
begin
  inherited Loaded;
  FTextSettings.OnChanged := DoTextSettingsChange;
  FTextSettingsOfLunar.OnChanged := DoTextSettingsChange;
  FTextSettingsOfTitle.OnChanged := DoTextSettingsChange;
  FTextSettingsOfWeeks.OnChanged := DoTextSettingsChange;
  FDrawable.OnChanged := DoDrawableChange;
  if IsAutoSize then
    DoAutoSize;
end;

procedure TCalendarViewBase.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  ID, LX, LY: Integer;
begin
  if (csDesigning in ComponentState) then begin
    inherited;
    Exit;
  end;

  P.X := X;
  P.Y := Y;
  ID := 0;

  if IsPointInRect(P, FRangeOfDays) then begin
    // 在日期区域内
    X := FRangeOfDays.Width / 7;
    Y := FRangeOfDays.Height / FCurRows;
    LX := Trunc((P.X - FRangeOfDays.Left) / X);
    LY := Trunc((P.Y - FRangeOfDays.Top) / Y);
    ID := FCurDrawS + LY * 7 + LX;
  end else if IsPointInRect(P, FRangeOfNavigation) then begin
    // 在导航区域内
    if P.X < FRangeOfNavigation.Left + CDefaultNextUpW then
      ID := BID_Up
    else if P.X > FRangeOfNavigation.Right - CDefaultNextUpW then
      ID := BID_Next
    else
      ID := BID_Navigation;
  end else begin
    LY := 0;
    if (coShowTodayButton in FOptions) then
      Inc(LY);
    if (coShowClearButton in FOptions) then
      Inc(LY, 2);
    if (LY > 0) and (P.Y > FRangeOfDays.Bottom) and (P.Y < FRangeOfDays.Bottom + FRowHeihgt) and
      (P.X > FRangeOfDays.Left) and (P.X < FRangeOfDays.Right) then
    begin
      // 在按钮区域内
      if LY = 3 then begin
        if P.X > FRangeOfDays.Left + FRangeOfDays.Width * 0.5 then
          ID := BID_Clear
        else
          ID := BID_Today;
      end else if LY = 1 then
        ID := BID_Today
      else
        ID := BID_Clear;
    end;
  end;

  if FCurHotDate <> ID then begin
    FCurHotDate := ID;
    Invalidate;
  end;

  inherited;
end;

procedure TCalendarViewBase.PaintBackground;
begin
  if AbsoluteInVisible or (csLoading in ComponentState) then
    Exit;
  if (FLanguage <> nil) then
    FInnerLanguage := FLanguage
  else
    FInnerLanguage := DefaultLanguage;
  PaintToCanvas(Canvas);
end;

procedure TCalendarViewBase.PaintToCanvas(Canvas: TCanvas);
var
  R, LR: TRectF;
  LH, LT: Single;
begin
  LH := 0;
  if Assigned(FBackground) then begin
    FBackground.Draw(Canvas);
    if Assigned(TDrawableBorder(FBackground)._Border) and (TDrawableBorder(FBackground)._Border.Style <> TViewBorderStyle.None) then
      LH := TDrawableBorder(FBackground)._Border.Width;
  end;

  R := RectF(Padding.Left + LH, Padding.Top + LH, Width - Padding.Right - LH, Height - Padding.Bottom - LH);

  // 导航栏
  if coShowNavigation in FOptions then begin  
    LR := RectF(R.Left, R.Top, R.Right, R.Top + FRowHeihgt); 
    R.Top := LR.Bottom;
    DoDrawNavigation(Canvas, LR);
    FRangeOfNavigation := LR;
  end else
    FRangeOfNavigation.Clear;

  // 星期
  LT := -$FFFF;
  if coShowWeek in FOptions then begin
    if coCosLinesOut in FOptions then
      LT := R.Top;
    LR := RectF(R.Left, R.Top, R.Right, R.Top + FRowHeihgt); 
    R.Top := LR.Bottom;
    DoDrawWeekRow(Canvas, LR);
  end;

  // 日期
  LH := FRowHeihgt + FRowPadding;
  if coShowLunar in FOptions then
    LH := LH + FRowLunarHeight + FRowLunarPadding;
  LR := RectF(R.Left, R.Top, R.Right, R.Top + FCurRows * LH);
  R.Top := LR.Bottom;
  DoDrawDatesRow(Canvas, LR, LT);
  FRangeOfDays := LR;

  // 今天按钮
  if coShowTodayButton in FOptions then begin
    if not (coShowClearButton in FOptions) then begin
      LR := RectF(R.Left, R.Top + 1, R.Right, R.Top + FRowHeihgt - 1);
      R.Top := LR.Bottom + 1;
    end else
      LR := RectF(R.Left, R.Top + 1, R.Left + (R.Right - R.Left) * 0.5, R.Top + FRowHeihgt - 1);
    DoDrawButton(Canvas, LR, FInnerLanguage.TodayStr, BID_Today);
  end;

  // 清除按钮
  if coShowClearButton in FOptions then begin
    if not (coShowTodayButton in FOptions) then begin
      LR := RectF(R.Left, R.Top + 1, R.Right, R.Top + FRowHeihgt - 1);
    end else begin
      LR.Left := LR.Right;
      LR.Right := R.Right;
    end;
    R.Top := LR.Bottom + 1;
    DoDrawButton(Canvas, LR, FInnerLanguage.ClearStr, BID_Clear);
  end;
end;

procedure TCalendarViewBase.ParseValue(const Value: TDate);
var
  S, E, Offset: Integer;
  Y, M, D: Word;
begin
  DecodeDate(Value, Y, M, D);
  FCurFirst := Trunc(EncodeDateTime(Y, M, 1, 0, 0, 0, 0));
  if M < 12 then
    Inc(M)
  else begin
    M := 1;
    Inc(Y);
  end;
  FCurLast := Trunc(EncodeDateTime(Y, M, 1, 0, 0, 0, 0)) - 1;

  FCurDayOfWeek := DayOfWeek(FCurFirst) - 1;
  Offset := (FCurDayOfWeek - FWeekStart) mod 7;
  if Offset < 0 then
    Inc(Offset, 7);

  S := FCurFirst - Offset;
  E := FCurLast;
  FCurRows := (E - S + 1) div 7;
  if (E - S + 1) mod 7 > 0 then
    Inc(FCurRows);   
end;

procedure TCalendarViewBase.Resize;
begin
  inherited;
end;

procedure TCalendarViewBase.SetAutoSize(const Value: Boolean);
begin
  FTextSettings.AutoSize := Value;
end;

procedure TCalendarViewBase.SetDaysOfWeekDisabled(const Value: TCalendarWeeks);
begin
  if FDaysOfWeekDisabled <> Value then begin
    FDaysOfWeekDisabled := Value;
    DoChange;
  end;
end;

procedure TCalendarViewBase.SetDaysOfWeekHighlighted(
  const Value: TCalendarWeeks);
begin
  if FDaysOfWeekHighlighted <> Value then begin
    FDaysOfWeekHighlighted := Value;
    DoChange;
  end;
end;

procedure TCalendarViewBase.SetDivider(const Value: TAlphaColor);
begin
  if FDivider <> Value then begin
    FDivider := Value;
    InitDividerBrush();
    Invalidate;  
  end;
end;

procedure TCalendarViewBase.SetDrawable(const Value: TCalendarDrawable);
begin
  FDrawable.Assign(Value);
end;

procedure TCalendarViewBase.SetEndDate(const Value: TDate);
begin
  if FEndDate <> Value then begin
    FEndDate := Value;
    DoChange();
  end;
end;

procedure TCalendarViewBase.SetLanguage(const Value: ICalendarLanguage);
begin
  if FLanguage <> Value then begin
    FLanguage := Value;
    if (not Assigned(FLanguage)) and (not (csDesigning in ComponentState)) then
      FLanguage := DefaultLanguage;
    DoChange;
  end;
end;

procedure TCalendarViewBase.SetOptions(const Value: TCalendarOptions);
begin
  if FOptions <> Value then begin
    FOptions := Value;
    DoOptionsChange();
  end;
end;

procedure TCalendarViewBase.SetRowHeihgt(const Value: Single);
begin
  if FRowHeihgt <> Value then begin
    FRowHeihgt := Value;
    if IsAutoSize then
      DoAutoSize
    else
      DoChange;
  end;
end;

procedure TCalendarViewBase.SetRowLunarHeight(const Value: Single);
begin
  if FRowLunarHeight <> Value then begin
    FRowLunarHeight := Value;
    if IsAutoSize and (coShowLunar in FOptions) then
      DoAutoSize
    else
      DoChange;
  end;
end;

procedure TCalendarViewBase.SetRowLunarPadding(const Value: Single);
begin
  if FRowLunarPadding <> Value then begin
    FRowLunarPadding := Value;
    if AutoSize and (coShowLunar in FOptions) then
      DoAutoSize
    else
      DoChange;
  end;
end;

procedure TCalendarViewBase.SetRowPadding(const Value: Single);
begin
  if FRowPadding <> Value then begin
    FRowPadding := Value;
    if IsAutoSize then
      DoAutoSize
    else
      DoChange;
  end;
end;

procedure TCalendarViewBase.SetStartDate(const Value: TDate);
begin
  if FStartDate <> Value then begin
    FStartDate := Value;
    DoChange;
  end;
end;

procedure TCalendarViewBase.SetStartView(const Value: TCalendarViewType);
begin
  if FStartView <> Value then begin
    FStartView := Value;
    DoChange;
  end;
end;

procedure TCalendarViewBase.SetTextSettings(const Value: TCalendarTextSettings);
begin
  FTextSettings.Assign(Value);
end;

procedure TCalendarViewBase.SetTextSettingsOfLunar(
  const Value: TCalendarTextSettings);
begin
  FTextSettingsOfLunar.Assign(Value);
end;

procedure TCalendarViewBase.SetTextSettingsOfTitle(
  const Value: TSimpleTextSettings);
begin
  FTextSettingsOfTitle.Assign(Value);
end;

procedure TCalendarViewBase.SetTextSettingsOfWeeks(
  const Value: TSimpleTextSettings);
begin
  FTextSettingsOfWeeks.Assign(Value);
end;

procedure TCalendarViewBase.SetValue(const Value: TDate);
var
  Y, M, D: Word;
  Y2, M2: Word;
begin
  if FValue <> Value then begin
    if IsAutoSize then begin
      DecodeDate(FValue, Y, M, D);
      DecodeDate(Value, Y2, M2, D);
      if (Y <> Y2) or (M <> M2) then
        D := 1
      else
        D := 0;
    end else
      D := 0;
    FValue := Value;
    ParseValue(Value);  
    if D <> 0 then
      DoAutoSize;
    DoDateChange;
  end;
end;

procedure TCalendarViewBase.SetWeekStart(const Value: TWeekStart);
var
  LRows: Integer;
begin
  if FWeekStart <> Value then begin
    FWeekStart := Value;
    LRows := FCurRows;
    ParseValue(FValue);
    if (FCurRows <> LRows) and IsAutoSize then
      DoAutoSize;
    DoChange;
  end;
end;

{ TCalendarColor }

procedure TCalendarColor.Assign(Source: TPersistent);
var
  Src: TCalendarColor;
begin
  if Source = nil then begin
    Self.FDefault := TAlphaColorRec.Null;
    Self.FHovered := TAlphaColorRec.Null;
    Self.FToday := TAlphaColorRec.Null;
    Self.FTodayHot := TAlphaColorRec.Null;
    Self.FSelected := TAlphaColorRec.Null;
    Self.FSelectedHot := TAlphaColorRec.Null;
    Self.FEnabled := TAlphaColorRec.Null;
    Self.FWeekend := TAlphaColorRec.Null;
    Self.FWeekendHot := TAlphaColorRec.Null;
    Self.FOutMonth := TAlphaColorRec.Null;
    Self.FOutMonthHot := TAlphaColorRec.Null;
    Self.FPressed := TAlphaColorRec.Null;
    Self.FHighlight := TAlphaColorRec.Null;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end else if Source is TViewColor then begin
    Src := TCalendarColor(Source);
    Self.FDefault := Src.FDefault;
    Self.FHovered := Src.FHovered;
    Self.FToday := Src.FToday;
    Self.FTodayHot := Src.FTodayHot;
    Self.FSelected := Src.FSelected;
    Self.FSelectedHot := Src.FSelectedHot;
    Self.FEnabled := Src.FEnabled;
    Self.FWeekend := Src.FWeekend;
    Self.FWeekendHot := Src.FWeekendHot;
    Self.FOutMonth := Src.FOutMonth;
    Self.FOutMonthHot := Src.FOutMonthHot;
    Self.FPressed := Src.FPressed;
    Self.FHighlight := Src.FHighlight;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end else
    inherited;
end;

function TCalendarColor.ColorDefaultStored: Boolean;
begin
  Result := GetColorStoreState(1);
end;

function TCalendarColor.ColorEnabledStored: Boolean;
begin
  Result := GetColorStoreState(7);
end;

function TCalendarColor.ColorHighlightStored: Boolean;
begin
  Result := GetColorStoreState(12);
end;

function TCalendarColor.ColorHoveredStored: Boolean;
begin
  Result := GetColorStoreState(2);
end;

function TCalendarColor.ColorOutMonthHotStored: Boolean;
begin
  Result := GetColorStoreState(11);
end;

function TCalendarColor.ColorOutMonthStored: Boolean;
begin
  Result := GetColorStoreState(10);
end;

function TCalendarColor.ColorPressedStored: Boolean;
begin
  Result := GetColorStoreState(13);
end;

function TCalendarColor.ColorSelectedHotStored: Boolean;
begin
  Result := GetColorStoreState(6);
end;

function TCalendarColor.ColorSelectedStored: Boolean;
begin
  Result := GetColorStoreState(5);
end;

function TCalendarColor.ColorTodayHotStored: Boolean;
begin
  Result := GetColorStoreState(4);
end;

function TCalendarColor.ColorTodayStored: Boolean;
begin
  Result := GetColorStoreState(3);
end;

function TCalendarColor.ColorWeekendHotStored: Boolean;
begin
  Result := GetColorStoreState(9);
end;

function TCalendarColor.ColorWeekendStored: Boolean;
begin
  Result := GetColorStoreState(8);
end;

constructor TCalendarColor.Create(const ADefaultColor: TAlphaColor);
begin
  FDefault := ADefaultColor;
  FHovered := TAlphaColorRec.Null;
  FToday := TAlphaColorRec.Red;
  FTodayHot := TAlphaColorRec.Red;
  FSelected := TAlphaColorRec.White;
  FSelectedHot := TAlphaColorRec.White;
  FEnabled := $ff999999;
  FWeekend := $ff777777;
  FWeekendHot := $ff777777;
  FOutMonth := $ffc0c1c2;
  FOutMonthHot := FOutMonth;
  FPressed := $ff000000;
  FHighlight := 0;
end;

destructor TCalendarColor.Destroy;
begin
  inherited;
end;

procedure TCalendarColor.DoChange(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Sender);
end;

function TCalendarColor.GetColorStoreState(const Index: Integer): Boolean;
begin
  Result := (FColorStoreState and Index) <> 0;
end;

procedure TCalendarColor.SetColorStoreState(const Index: Integer;
  const Value: Boolean);
begin
  if Value then
    FColorStoreState := (FColorStoreState or Cardinal(Index))
  else
    FColorStoreState := (FColorStoreState and (not Index));
end;

procedure TCalendarColor.SetDefault(const Value: TAlphaColor);
begin
  if Value <> FDefault then begin
    FDefault := Value;
    DefaultChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetEnabled(const Value: TAlphaColor);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    EnabledChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetHighlight(const Value: TAlphaColor);
begin
  if Value <> FHighlight then begin
    FHighlight := Value;
    HighlightChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetHovered(const Value: TAlphaColor);
begin
  if Value <> FHovered then begin
    FHovered := Value;
    HoveredChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetOutMonth(const Value: TAlphaColor);
begin
  FOutMonth := Value;
end;

procedure TCalendarColor.SetOutMonthHot(const Value: TAlphaColor);
begin
  FOutMonthHot := Value;
end;

procedure TCalendarColor.SetPressed(const Value: TAlphaColor);
begin
  if Value <> FPressed then begin
    FPressed := Value;
    PressedChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetSelected(const Value: TAlphaColor);
begin
  if Value <> FSelected then begin
    FSelected := Value;
    SelectedChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetSelectedHot(const Value: TAlphaColor);
begin
  if Value <> FSelectedHot then begin
    FSelectedHot := Value;
    SelectedHotChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetToday(const Value: TAlphaColor);
begin
  if Value <> FToday then begin
    FToday := Value;
    TodayChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetTodayHot(const Value: TAlphaColor);
begin
  if Value <> FTodayHot then begin
    FTodayHot := Value;
    TodayHotChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetWeekend(const Value: TAlphaColor);
begin
  if Value <> FWeekend then begin
    FWeekend := Value;
    WeekendChange := True;
    DoChange(Self);
  end;
end;

procedure TCalendarColor.SetWeekendHot(const Value: TAlphaColor);
begin
  FWeekendHot := Value;
end;

{ TCalendarTextSettings }

constructor TCalendarTextSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := TCalendarColor.Create();
  FColor.OnChanged := DoColorChanged;
end;

destructor TCalendarTextSettings.Destroy;
begin
  FreeAndNil(FColor);
  inherited Destroy;
end;

function TCalendarTextSettings.GetStateColor(
  const State: TViewState): TAlphaColor;
begin
  if State = TViewState.Hovered then
    Result := FColor.FHovered
  else
    Result := FColor.FDefault;
end;

procedure TCalendarTextSettings.SetColor(const Value: TCalendarColor);
begin
  FColor.Assign(Value);
end;

{ TCalendarDrawable }

function TCalendarDrawable.GetValue(const Index: Integer): TViewBrush;
begin
  Result := inherited GetBrush(TViewState(Index),
    not (csLoading in FView.GetComponentState)) as TViewBrush;
end;

procedure TCalendarDrawable.SetIsCircle(const Value: Boolean);
begin
  if FIsCircle <> Value then begin
    FIsCircle := Value;
    DoChange(Self);
  end;
end;

procedure TCalendarDrawable.SetValue(const Index: Integer;
  const Value: TViewBrush);
begin
  inherited SetValue(Index, Value);
end;

initialization
  DefaultLanguage := TCalendarLanguage_EN.Create(nil);

finalization
  FreeAndNil(DefaultLanguage);

end.
