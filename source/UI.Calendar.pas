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
  UI.Base, UI.Utils, UI.Ani, UI.Calendar.Data,
  FMX.Effects, FMX.Text,
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
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
     coShowWeek, {显示星期}
     coCalendarWeeks, {显示周数}
     coTodayHighlight, {高亮今天}
     coShowTodayButton, {显示今天按钮}
     coShowLunar, {显示农历}
     coShowTerm {显示节气，需要开启 coShowLunar}
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
  end;

  /// <summary>
  /// 日历语言 - 英文
  /// </summary>
  TCalendarLanguage_EN = class(TComponent, ICalendarLanguage)
  public
    function WeekStrList: TArray<string>;
    function MonthsStrList: TArray<string>;
    function DateToStr(const Value: TDate): string;
  end;

type
  TCalendarViewBase = class(TView)
  private
    [Weak] FLanguage: ICalendarLanguage;
    FOptions: TCalendarOptions;
    FStartView: TCalendarViewType;
    FStartDate: TDate;
    FEndDate: TDate;
    FWeekStart: TWeekStart;
    FDaysOfWeekDisabled: TCalendarWeeks;
    FDaysOfWeekHighlighted: TCalendarWeeks;

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
  protected
    procedure DoOptionsChange; virtual;
    procedure DoChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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
    property DaysOfWeekDisabled: TCalendarWeeks read FDaysOfWeekDisabled write SetDaysOfWeekDisabled;
    /// <summary>
    /// 高亮显示的日期于星期几
    /// </summary>
    property DaysOfWeekHighlighted: TCalendarWeeks read FDaysOfWeekHighlighted write SetDaysOfWeekHighlighted;
  end;

type
  /// <summary>
  /// 日历视图组件
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TCalendarView = class(TCalendarViewBase)
  published
    property Options default CDefaultCalendarOptions;
    property StartView default TCalendarViewType.Days;
    property StartDate;
    property EndDate;
    property WeekStart default 0;
    property Language;
    property DaysOfWeekDisabled;
    property DaysOfWeekHighlighted;
  end;

implementation

var
  DefaultLanguage: TCalendarLanguage_EN;

{ TCalendarLanguage_CN }

function TCalendarLanguage_CN.DateToStr(const Value: TDate): string;
begin
  Result := FormatDateTime('yyyy年mm月', Value);
end;

function TCalendarLanguage_CN.MonthsStrList: TArray<string>;
begin
  Result := ['1月', '2月', '3月', '4月', '5月', '6月',
    '7月', '8月', '9月', '10月', '11月', '12月'];
end;

function TCalendarLanguage_CN.WeekStrList: TArray<string>;
begin
  Result := ['日', '一', '二', '三', '四', '五', '六'];
end;

{ TCalendarLanguage_EN }

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
  Result := Format('%s %d', [LMonths[M], Y]);
end;

function TCalendarLanguage_EN.MonthsStrList: TArray<string>;
begin
  Result := ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
end;

function TCalendarLanguage_EN.WeekStrList: TArray<string>;
begin
  Result := ['Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa'];
end;

{ TCalendarViewBase }

constructor TCalendarViewBase.Create(AOwner: TComponent);
begin
  inherited;
  if not (csDesigning in ComponentState) then  
    FLanguage := DefaultLanguage;
  FOptions := CDefaultCalendarOptions;
end;

destructor TCalendarViewBase.Destroy;
begin
  inherited;
end;

procedure TCalendarViewBase.DoChange;
begin
  Invalidate;
end;

procedure TCalendarViewBase.DoOptionsChange;
begin
  Invalidate;
end;

function TCalendarViewBase.GetLanguage: ICalendarLanguage;
begin
  if (not Assigned(FLanguage)) and (not (csDesigning in ComponentState)) then
    FLanguage := DefaultLanguage;
  Result := FLanguage;
end;

function TCalendarViewBase.IsEndDateStored: Boolean;
begin
  Result := FEndDate <> 0;
end;

function TCalendarViewBase.IsStartDateStored: Boolean;
begin
  Result := FStartDate <> 0;
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

procedure TCalendarViewBase.SetWeekStart(const Value: TWeekStart);
begin
  if FWeekStart <> Value then begin
    FWeekStart := Value;
    DoChange;
  end;
end;

initialization
  DefaultLanguage := TCalendarLanguage_EN.Create(nil);

finalization
  FreeAndNil(DefaultLanguage);

end.
