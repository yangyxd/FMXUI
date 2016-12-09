{*******************************************************}
{                                                       }
{       FMX UI Dialog 通用对话框                        }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

{
  示例：
  1. 基本对话框
    TDialogBuilder.Create(Self)
      .SetTitle('标题')
      .SetMessage('消息内容')
      .SetNegativeButton('取消')
      .Show();
  2. 列表框
    TDialogBuilder.Create(Self)
      .SetItems(['Item1', 'Item2', 'Item3'],
        procedure (Dialog: IDialog; Which: Integer) begin
          Hint(Dialog.Builder.ItemArray[Which]);
        end
      )
      .Show();
  3. 多选框
    TDialogBuilder.Create(Self)
      .SetMultiChoiceItems(
        ['Item1', 'Item2', 'Item3'],
        [False, True, False],
        procedure (Dialog: IDialog; Which: Integer; IsChecked: Boolean) begin
          // Hint(Dialog.Builder.ItemArray[Which]);
        end
      )
      .SetNeutralButton('确定',
        procedure (Dialog: IDialog; Which: Integer) begin
          Hint(Format('您选择了%d项', [Dialog.Builder.CheckedCount]));
        end
      )
      .Show();
}

unit UI.Dialog;

interface

uses
  UI.Base, UI.Standard, UI.ListView, {$IFDEF WINDOWS}UI.Debug, {$ENDIF}
  System.TypInfo, System.SysUtils, System.Character, System.RTLConsts,
  FMX.Graphics, System.Generics.Collections, FMX.TextLayout, FMX.Ani,
  System.Classes, System.Types, System.UITypes, System.Math.Vectors, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Utils,
  FMX.ListView, FMX.ListView.Appearances, FMX.ListView.Types;

const       
  // 没有点击按钮
  BUTTON_NONE = 0;
  // The identifier for the positive button.
  BUTTON_POSITIVE = -1;
  // The identifier for the negative button.
  BUTTON_NEGATIVE = -2;
  // The identifier for the neutral button.
  BUTTON_NEUTRAL = -3;

const
  // 颜色、字体等默认设置项
  {$IFDEF IOS}
  COLOR_BackgroundColor = $fff7f7f7;
  COLOR_TitleTextColor = $ff077dfe;
  COLOR_MessageTextColor = $ff000000;
  COLOR_TitleBackGroundColor = $00000000;
  COLOR_DialogMaskColor = $9f000000;
  COLOR_BodyBackgroundColor = $00ffffff;
  COLOR_MessageTextBackground = $00f00000;
  {$ELSE}
  {$IFDEF MSWINDOWS}
    COLOR_BackgroundColor = $fff0f0f0;
    COLOR_TitleTextColor = $ff000000;
    COLOR_MessageTextColor = $ff101010;
    COLOR_TitleBackGroundColor = $ffffffff;
    COLOR_DialogMaskColor = $9f000000;
    COLOR_BodyBackgroundColor = $ffffffff;
    COLOR_MessageTextBackground = $00f00000;
  {$ELSE}
    COLOR_BackgroundColor = $fff0f0f0;
    COLOR_TitleTextColor = $ff000000;
    COLOR_MessageTextColor = $ff101010;
    COLOR_TitleBackGroundColor = $ffffffff;
    COLOR_DialogMaskColor = $9f000000;
    COLOR_BodyBackgroundColor = $ffffffff;
    COLOR_MessageTextBackground = $00f00000;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  COLOR_ProcessBackgroundColor = $7fbababa;
  {$ENDIF}
  {$IFDEF IOS}
  COLOR_ProcessBackgroundColor = $00000000;
  {$ENDIF}
  {$IFDEF ANDROID}
  COLOR_ProcessBackgroundColor = $7f000000;
  {$ENDIF}
  COLOR_ProcessTextColor = $fff7f7f7;

  {$IFDEF IOS}
  COLOR_ButtonColor = $fff7f7f7;
  COLOR_ButtonPressColor = $ffd8d8d8;
  COLOR_ButtonTextColor = $FF077dfe;
  COLOR_ButtonTextPressColor = $FF0049f5;
  {$ELSE}
  COLOR_ButtonColor = $fff8f8f8;
  COLOR_ButtonPressColor = $ffd9d9d9;
  COLOR_ButtonTextColor = $FF101010;
  COLOR_ButtonTextPressColor = $FF000000;
  {$ENDIF}


  FONT_TitleTextSize = 18;
  FONT_MessageTextSize = 15;
  FONT_ButtonTextSize = 16;

  Title_Gravity = TLayoutGravity.CenterVertical;

  {$IFDEF IOS}
  SIZE_BackgroundRadius = 15;
  SIZE_TitleHeight = 38;
  {$ELSE}
    {$IFDEF MSWINDOWS}
    SIZE_BackgroundRadius = 2;
    SIZE_TitleHeight = 48;
    {$ELSE}
    SIZE_BackgroundRadius = 2;
    SIZE_TitleHeight = 48;
    {$ENDIF}
  {$ENDIF}
  SIZE_ICON = 32;
  SIZE_ButtonBorder = 1;

type
  /// <summary>
  /// 对话框样式管理器
  /// </summary>
  TDialogStyleManager = class(TComponent)
  private
    FDialogMaskColor: TAlphaColor;
    FTitleBackGroundColor: TAlphaColor;
    FTitleTextColor: TAlphaColor;
    FBackgroundColor: TAlphaColor;
    FBodyBackgroundColor: TAlphaColor;
    FProcessBackgroundColor: TAlphaColor;
    FProcessTextColor: TAlphaColor;
    FMessageTextColor: TAlphaColor;
    FMessageTextBackground: TAlphaColor;
    FButtonColor: TViewColor;
    FButtonTextColor: TTextColor;
    FButtonBorder: TViewBorder;
    FMessageTextSize: Integer;
    FTitleHeight: Integer;
    FTitleTextSize: Integer;
    FButtonTextSize: Integer;
    FIconSize: Integer;
    FBackgroundRadius: Single;
    FTitleGravity: TLayoutGravity;
    procedure SetButtonColor(const Value: TViewColor);
    procedure SetButtonBorder(const Value: TViewBorder);
    procedure SetButtonTextColor(const Value: TTextColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // 遮罩层颜色
    property DialogMaskColor: TAlphaColor read FDialogMaskColor write FDialogMaskColor;
    // 消息框背景颜色
    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor;
    // 标题栏背景色
    property TitleBackGroundColor: TAlphaColor read FTitleBackGroundColor write FTitleBackGroundColor;
    // 标题栏文本颜色
    property TitleTextColor: TAlphaColor read FTitleTextColor write FTitleTextColor;
    // 主体区背景颜色
    property BodyBackgroundColor: TAlphaColor read FBodyBackgroundColor write FBodyBackgroundColor;
    // 消息文本颜色
    property MessageTextColor: TAlphaColor read FMessageTextColor write FMessageTextColor;
    // 消息文本背景颜色
    property MessageTextBackground: TAlphaColor read FMessageTextBackground write FMessageTextBackground;

    // 等待消息框背景颜色
    property ProcessBackgroundColor: TAlphaColor read FProcessBackgroundColor write FProcessBackgroundColor;
    // 等待消息框消息文字颜色
    property ProcessTextColor: TAlphaColor read FProcessTextColor write FProcessTextColor;

    // 标题栏文本重力
    property TitleGravity: TLayoutGravity read FTitleGravity write FTitleGravity default Title_Gravity;
    // 标题栏高度
    property TitleHeight: Integer read FTitleHeight write FTitleHeight default SIZE_TitleHeight;
    // 标题文本大小
    property TitleTextSize: Integer read FTitleTextSize write FTitleTextSize default FONT_TitleTextSize;
    // 消息文本大小
    property MessageTextSize: Integer read FMessageTextSize write FMessageTextSize default FONT_MessageTextSize;
    // 消息文本大小
    property ButtonTextSize: Integer read FButtonTextSize write FButtonTextSize default FONT_ButtonTextSize;
    // 图标大小
    property IconSize: Integer read FIconSize write FIconSize default SIZE_ICON;

    property BackgroundRadius: Single read FBackgroundRadius write FBackgroundRadius;
    property ButtonColor: TViewColor read FButtonColor write SetButtonColor;
    property ButtonTextColor: TTextColor read FButtonTextColor write SetButtonTextColor;
    property ButtonBorder: TViewBorder read FButtonBorder write SetButtonBorder;
  end;

type
  TDialogBuilder = class;
  TCustomAlertDialog = class;

  /// <summary>
  /// 对话框接口
  /// </summary>
  IDialog = interface(IInterface)
    ['{53E2915A-B90C-4C9B-85D8-F4E3B9892D9A}']
    function GetBuilder: TDialogBuilder;
    function GetView: TControl;
    function GetCancelable: Boolean;

    /// <summary>
    /// 显示对话框
    /// </summary>
    procedure Show();
    /// <summary>
    /// 关闭对话框
    /// </summary>
    procedure Dismiss();
    /// <summary>
    /// 异步关闭对话框
    /// </summary>
    procedure AsyncDismiss();
    /// <summary>
    /// 关闭对话框
    /// </summary>
    procedure Close();
    /// <summary>
    /// 取消对话框
    /// </summary>
    procedure Cancel();
    /// <summary>
    /// 隐藏对话框
    /// </summary>
    procedure Hide();

    /// <summary>
    /// 构造器
    /// </summary>
    property Builder: TDialogBuilder read GetBuilder;
    /// <summary>
    /// 视图组件
    /// </summary>
    property View: TControl read GetView;
    /// <summary>
    /// 是否能取消对话框
    /// </summary>
    property Cancelable: Boolean read GetCancelable;
  end;

  TOnDialogKeyListener = procedure (Dialog: IDialog; keyCode: Integer) of object;
  TOnDialogKeyListenerA = reference to procedure (Dialog: IDialog; keyCode: Integer);
  TOnDialogMultiChoiceClickListener = procedure (Dialog: IDialog; Which: Integer; IsChecked: Boolean) of object;
  TOnDialogMultiChoiceClickListenerA = reference to procedure (Dialog: IDialog; Which: Integer; IsChecked: Boolean);
  TOnDialogItemSelectedListener = procedure (Dialog: IDialog; Position: Integer; ID: Int64) of object;
  TOnDialogItemSelectedListenerA = reference to procedure (Dialog: IDialog; Position: Integer; ID: Int64);
  TOnDialogClickListener = procedure (Dialog: IDialog; Which: Integer) of object;
  TOnDialogClickListenerA = reference to procedure (Dialog: IDialog; Which: Integer);
  TOnDialogListener = procedure (Dialog: IDialog) of object;
  TOnDialogListenerA = reference to procedure (Dialog: IDialog);

  /// <summary>
  /// 对话框视图 (不要直接使用它)
  /// </summary>
  TDialogView = class(TRelativeLayout)
  private
    [Weak] FDialog: IDialog;
  protected
    FLayBubble: TLinearLayout;
    FTitleView: TTextView;
    FMsgBody: TLinearLayout;
    FMsgMessage: TTextView;
    FButtonLayout: TLinearLayout;
    FButtonPositive: TButtonView;
    FButtonNegative: TButtonView;
    FButtonNeutral: TButtonView;
    FListView: TListViewEx;
    FAnilndictor: TAniIndicator;
  protected
    procedure AfterDialogKey(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitView(StyleMgr: TDialogStyleManager);
    procedure InitProcessView(StyleMgr: TDialogStyleManager);
    procedure InitMessage(StyleMgr: TDialogStyleManager);
    procedure InitList(StyleMgr: TDialogStyleManager);
    procedure InitButton(StyleMgr: TDialogStyleManager);
    procedure InitOK();
    procedure Show; override;
    procedure Hide; override;
    procedure SetTitle(const AText: string);

    property ListView: TListViewEx read FListView;
    property TitleView: TTextView read FTitleView;
    property MessageView: TTextView read FMsgMessage;
    property ButtonLayout: TLinearLayout read FButtonLayout;
    property ButtonPositive: TButtonView read FButtonPositive;
    property ButtonNegative: TButtonView read FButtonNegative;
    property ButtonNeutral: TButtonView read FButtonNeutral;

    property Dialog: IDialog read FDialog write FDialog;
  end;

  TControlClass = type of TControl;
  TDialogViewPosition = (Top, Bottom, Left, Right, Center);

  TDialog = class(TComponent, IDialog)
  private
    FOnCancelListener: TOnDialogListener;
    FOnCancelListenerA: TOnDialogListenerA;
    FOnShowListener: TOnDialogListener;
    FOnShowListenerA: TOnDialogListenerA;
    FOnDismissListener: TOnDialogListener;
    FOnDismissListenerA: TOnDialogListenerA;
    procedure SetOnCancelListener(const Value: TOnDialogListener);
    procedure SetOnCancelListenerA(const Value: TOnDialogListenerA);
    function GetView: TControl;
    function GetRootView: TDialogView;
    function GetIsDismiss: Boolean;
  protected
    FViewRoot: TDialogView;
    FTimer: TTimer;
    FTimerStart: Int64;
    FDestBgColor: TAlphaColor;

    FCancelable: Boolean;
    FCanceled: Boolean;
    FIsDismiss: Boolean;

    FEventing: Boolean;      // 事件处理中
    FAllowDismiss: Boolean;  // 需要释放

    procedure SetCancelable(const Value: Boolean);
    function GetCancelable: Boolean;

    procedure InitOK(); virtual;
    procedure DoFreeBuilder(); virtual;
    procedure DoApplyTitle(); virtual;

    function GetBuilder: TDialogBuilder; virtual;
    function GetMessage: string; virtual;
    procedure SetMessage(const Value: string); virtual;

    procedure SetBackColor(const Value: TAlphaColor);

    function GetFirstParent(): TFmxObject;
  protected
    procedure DoRootClick(Sender: TObject); virtual;
    procedure DoUpdateBackTimer(Sender: TObject);
    procedure DoAsyncDismissTimer(Sender: TObject);
    procedure DoAsyncDismiss();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// 显示对话框
    /// </summary>
    procedure Show();

    /// <summary>
    /// 显示对话框
    /// <param name="Target">定位控件</param>
    /// <param name="ViewClass">要自动创建的视图类</param>
    /// <param name="Position">视图位置（默认位于目标下方）</param>
    /// <param name="XOffset">视图偏移横向位置</param>
    /// <param name="YOffset">视图偏移垂直位置</param>
    /// </summary>
    class function ShowView(const AOwner: TComponent; const Target: TControl;
      const ViewClass: TControlClass;
      XOffset: Single = 0; YOffset: Single = 0;
      Position: TDialogViewPosition = TDialogViewPosition.Bottom;
      Cancelable: Boolean = True): TDialog; overload;
    /// <summary>
    /// 显示对话框
    /// <param name="Target">定位控件</param>
    /// <param name="View">要显示的视图对象</param>
    /// <param name="AViewAutoFree">是否自动释放View对象</param>
    /// <param name="Position">视图位置（默认位于目标下方）</param>
    /// <param name="XOffset">视图偏移横向位置</param>
    /// <param name="YOffset">视图偏移垂直位置</param>
    /// </summary>
    class function ShowView(const AOwner: TComponent; const Target: TControl;
      const View: TControl; AViewAutoFree: Boolean = True;
      XOffset: Single = 0; YOffset: Single = 0;
      Position: TDialogViewPosition = TDialogViewPosition.Bottom;
      Cancelable: Boolean = True): TDialog; overload;

    /// <summary>
    /// 在一个目标控件身上查找与其绑定在一起的对象框
    /// </summary>
    class function GetDialog(const Target: TControl): IDialog;

    /// <summary>
    /// 在一个目标控件身上查找与其绑定在一起的对话框，如果找到，关闭它
    /// </summary>
    class procedure CloseDialog(const Target: TControl);

    /// <summary>
    /// 关闭对话框
    /// </summary>
    procedure Dismiss();
    /// <summary>
    /// 关闭对话框
    /// </summary>
    procedure Close();
    /// <summary>
    /// 取消对话框
    /// </summary>
    procedure Cancel();
    /// <summary>
    /// 隐藏
    /// </summary>
    procedure Hide();
    /// <summary>
    /// 异步释放
    /// </summary>
    procedure AsyncDismiss();

    /// <summary>
    /// 通知数据已经改变，刷新列表
    /// </summary>
    procedure NotifyDataSetChanged();

    /// <summary>
    /// 对话框View
    /// </summary>
    property View: TControl read GetView;

    property RootView: TDialogView read GetRootView;

    /// <summary>
    /// 是否能取消对话框
    /// </summary>
    property Cancelable: Boolean read FCancelable write SetCancelable;

    property Message: string read GetMessage write SetMessage;
    property Canceled: Boolean read FCanceled;
    property IsDismiss: Boolean read GetIsDismiss;

    property OnCancelListener: TOnDialogListener read FOnCancelListener write SetOnCancelListener;
    property OnCancelListenerA: TOnDialogListenerA read FOnCancelListenerA write SetOnCancelListenerA;
    property OnShowListener: TOnDialogListener read FOnShowListener write FOnShowListener;
    property OnShowListenerA: TOnDialogListenerA read FOnShowListenerA write FOnShowListenerA;
    property OnDismissListener: TOnDialogListener read FOnDismissListener write FOnDismissListener;
    property OnDismissListenerA: TOnDialogListenerA read FOnDismissListenerA write FOnDismissListenerA;
  end;

  /// <summary>
  /// 弹出式对话框基类
  /// </summary>
  TCustomAlertDialog = class(TDialog)
  private
    FBuilder: TDialogBuilder;

    FOnKeyListener: TOnDialogKeyListener;
    FOnKeyListenerA: TOnDialogKeyListenerA;

    procedure SetOnKeyListener(const Value: TOnDialogKeyListener);
    function GetItems: TStrings;
  protected
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetMessage: string; override;
    procedure SetMessage(const Value: string); override;
    function GetBuilder: TDialogBuilder; override;

    procedure InitList(const ListView: TListViewEx; IsMulti: Boolean = False);
    procedure InitExtPopView();
    procedure InitSinglePopView();
    procedure InitMultiPopView();
    procedure InitListPopView();
    procedure InitDefaultPopView();

  protected
    procedure DoButtonClick(Sender: TObject);
    procedure DoListItemClick(Sender: TObject; ItemIndex: Integer; const ItemView: TControl);
    procedure DoApplyTitle(); override;
    procedure DoFreeBuilder(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// 以 Builder 的设置来初始化对话框
    /// </summary>
    procedure Apply(const ABuilder: TDialogBuilder); virtual;

    /// <summary>
    /// 对话框构造器
    /// </summary>
    property Builder: TDialogBuilder read FBuilder;

    property Title: string read GetTitle write SetTitle;
    property Items: TStrings read GetItems;
    property OnKeyListener: TOnDialogKeyListener read FOnKeyListener write SetOnKeyListener;
    property OnKeyListenerA: TOnDialogKeyListenerA read FOnKeyListenerA write FOnKeyListenerA;
  end;

  /// <summary>
  /// 对话框构造器
  /// </summary>
  TDialogBuilder = class(TObject)
  private
    [Weak] FOwner: TComponent;
    [Weak] FIcon: TObject;
    [Weak] FItems: TStrings;
    [Weak] FStyleManager: TDialogStyleManager;
    [Weak] FDataObject: TObject;

    FItemArray: TArray<string>;
    FData: TValue;
    FView: TControl;
    FViewAutoFree: Boolean;

    FUseRootBackColor: Boolean;
    FRootBackColor: TAlphaColor;

    FTitle: string;
    FMessage: string;
    FCancelable: Boolean;
    FIsMaxWidth: Boolean;
    FIsSingleChoice: Boolean;
    FIsMultiChoice: Boolean;
    FItemSingleLine: Boolean;
    FClickButtonDismiss: Boolean;
    FMaskVisible: Boolean;
    FCheckedItem: Integer;
    FTag: Integer;

    FCheckedItems: TArray<Boolean>;

    FPositiveButtonText: string;
    FPositiveButtonListener: TOnDialogClickListener;
    FPositiveButtonListenerA: TOnDialogClickListenerA;

    FNegativeButtonText: string;
    FNegativeButtonListener: TOnDialogClickListener;
    FNegativeButtonListenerA: TOnDialogClickListenerA;

    FNeutralButtonText: string;
    FNeutralButtonListener: TOnDialogClickListener;
    FNeutralButtonListenerA: TOnDialogClickListenerA;

    FOnCancelListener: TOnDialogListener;
    FOnCancelListenerA: TOnDialogListenerA;
    FOnKeyListener: TOnDialogKeyListener;
    FOnKeyListenerA: TOnDialogKeyListenerA;

    FOnCheckboxClickListener: TOnDialogMultiChoiceClickListener;
    FOnCheckboxClickListenerA: TOnDialogMultiChoiceClickListenerA;
    FOnItemSelectedListener: TOnDialogItemSelectedListener;
    FOnItemSelectedListenerA: TOnDialogItemSelectedListenerA;
    FOnClickListener: TOnDialogClickListener;
    FOnClickListenerA: TOnDialogClickListenerA;
    function GetCheckedCount: Integer;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;

    function CreateDialog(): IDialog;
    function Show(): IDialog; overload;
    function Show(OnDismissListener: TOnDialogListener): IDialog; overload;
    function Show(OnDismissListener: TOnDialogListenerA): IDialog; overload;

    /// <summary>
    /// 设置一个对话框样式管理器，不设置则会自动查找，找不到则使用默认样式
    /// </summary>
    function SetStyleManager(AValue: TDialogStyleManager): TDialogBuilder;

    /// <summary>
    /// 设置是否最大化宽度
    /// </summary>
    function SetIsMaxWidth(AIsMaxWidth: Boolean): TDialogBuilder;
    /// <summary>
    /// 设置标题
    /// </summary>
    function SetTitle(const ATitle: string): TDialogBuilder;
    /// <summary>
    /// 设置消息
    /// </summary>
    function SetMessage(const AMessage: string): TDialogBuilder;
    /// <summary>
    /// 设置图标
    /// </summary>
    function SetIcon(AIcon: TBrush): TDialogBuilder; overload;
    /// <summary>
    /// 设置图标
    /// </summary>
    function SetIcon(AIcon: TBrushBitmap): TDialogBuilder; overload;
    /// <summary>
    /// 设置图标
    /// </summary>
    function SetIcon(AIcon: TDrawableBase): TDialogBuilder; overload;

    /// <summary>
    /// 设置确认按钮
    /// </summary>
    function SetPositiveButton(const AText: string; AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetPositiveButton(const AText: string; AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    /// <summary>
    /// 设置否定按钮
    /// </summary>
    function SetNegativeButton(const AText: string; AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetNegativeButton(const AText: string; AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    /// <summary>
    /// 设置中间按钮
    /// </summary>
    function SetNeutralButton(const AText: string; AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetNeutralButton(const AText: string; AListener: TOnDialogClickListenerA): TDialogBuilder; overload;

    /// <summary>
    /// 设置是否可以取消
    /// </summary>
    function SetCancelable(ACancelable: Boolean): TDialogBuilder;
    /// <summary>
    /// 设置取消事件
    /// </summary>
    function SetOnCancelListener(AListener: TOnDialogListener): TDialogBuilder; overload;
    function SetOnCancelListener(AListener: TOnDialogListenerA): TDialogBuilder; overload;
    /// <summary>
    /// 设置按键监听事件
    /// </summary>
    function SetOnKeyListener(AListener: TOnDialogKeyListener): TDialogBuilder; overload;
    function SetOnKeyListener(AListener: TOnDialogKeyListenerA): TDialogBuilder; overload;
    /// <summary>
    /// 设置列表项
    /// </summary>
    function SetItems(AItems: TStrings; AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetItems(AItems: TStrings; AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    function SetItems(const AItems: TArray<string>; AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetItems(const AItems: TArray<string>; AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    /// <summary>
    /// 设置一个子视图
    /// </summary>
    function SetView(AView: TControl; AViewAutoFree: Boolean = True): TDialogBuilder;
    /// <summary>
    /// 设置多重选项列表项
    /// </summary>
    function SetMultiChoiceItems(AItems: TStrings; ACheckedItems: TArray<Boolean>;
      AListener: TOnDialogMultiChoiceClickListener = nil): TDialogBuilder; overload;
    function SetMultiChoiceItems(AItems: TStrings; ACheckedItems: TArray<Boolean>;
      AListener: TOnDialogMultiChoiceClickListenerA): TDialogBuilder; overload;
    function SetMultiChoiceItems(const AItems: TArray<string>; ACheckedItems: TArray<Boolean>;
      AListener: TOnDialogMultiChoiceClickListener = nil): TDialogBuilder; overload;
    function SetMultiChoiceItems(const AItems: TArray<string>; ACheckedItems: TArray<Boolean>;
      AListener: TOnDialogMultiChoiceClickListenerA): TDialogBuilder; overload;
    /// <summary>
    /// 设置单选列表项
    /// </summary>
    function SetSingleChoiceItems(AItems: TStrings; ACheckedItem: Integer;
      AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetSingleChoiceItems(AItems: TStrings; ACheckedItem: Integer;
      AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    function SetSingleChoiceItems(const AItems: TArray<string>; ACheckedItem: Integer;
      AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetSingleChoiceItems(const AItems: TArray<string>; ACheckedItem: Integer;
      AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    /// <summary>
    /// 设置列表项选择事件
    /// </summary>
    function SetOnItemSelectedListener(AListener: TOnDialogItemSelectedListener): TDialogBuilder; overload;
    function SetOnItemSelectedListener(AListener: TOnDialogItemSelectedListenerA): TDialogBuilder; overload;
    /// <summary>
    /// 设置列表项是否为单行文本，默认为 True
    /// </summary>
    function SetItemSingleLine(AItemSingleLine: Boolean): TDialogBuilder;
    /// <summary>
    /// 设置是否在点击了按钮后释放对话框
    /// </summary>
    function SetClickButtonDismiss(V: Boolean): TDialogBuilder;

    /// <summary>
    /// 设置 Mask 层是否可视
    /// </summary>
    function SetMaskVisible(V: Boolean): TDialogBuilder;

    /// <summary>
    /// 设置 对话框 Root 层背景颜色
    /// </summary>
    function SetRootBackColor(const V: TAlphaColor): TDialogBuilder;

    /// <summary>
    /// 设置附加的数据
    /// </summary>
    function SetData(const V: TObject): TDialogBuilder; overload;
    function SetData(const V: TValue): TDialogBuilder; overload;
    function SetTag(const V: Integer): TDialogBuilder;
  public
    property Owner: TComponent read FOwner;
    property View: TControl read FView;
    property Icon: TObject read FIcon;
    property Items: TStrings read FItems;
    property ItemArray: TArray<string> read FItemArray;

    property StyleManager: TDialogStyleManager read FStyleManager;

    property Title: string read FTitle;
    property Message: string read FMessage;
    property Cancelable: Boolean read FCancelable;
    property IsMaxWidth: Boolean read FIsMaxWidth;
    property IsSingleChoice: Boolean read FIsSingleChoice;
    property IsMultiChoice: Boolean read FIsMultiChoice;
    property ItemSingleLine: Boolean read FItemSingleLine;
    property MaskVisible: Boolean read FMaskVisible write FMaskVisible;
    property RootBackColor: TAlphaColor read FRootBackColor write FRootBackColor;
    property ClickButtonDismiss: Boolean read FClickButtonDismiss;
    property CheckedItem: Integer read FCheckedItem; 
    property CheckedItems: TArray<Boolean> read FCheckedItems;
    property CheckedCount: Integer read GetCheckedCount;

    property DataObject: TObject read FDataObject write FDataObject;
    property Data: TValue read FData write FData;
    property Tag: Integer read FTag write FTag;

    property PositiveButtonText: string read FPositiveButtonText;
    property PositiveButtonListener: TOnDialogClickListener read FPositiveButtonListener;
    property PositiveButtonListenerA: TOnDialogClickListenerA read FPositiveButtonListenerA;

    property NegativeButtonText: string read FNegativeButtonText;
    property NegativeButtonListener: TOnDialogClickListener read FNegativeButtonListener;
    property NegativeButtonListenerA: TOnDialogClickListenerA read FNegativeButtonListenerA;

    property NeutralButtonText: string read FNeutralButtonText;
    property NeutralButtonListener: TOnDialogClickListener read FNeutralButtonListener;
    property NeutralButtonListenerA: TOnDialogClickListenerA read FNeutralButtonListenerA;

    property OnCancelListener: TOnDialogListener read FOnCancelListener;
    property OnCancelListenerA: TOnDialogListenerA read FOnCancelListenerA;
    property OnKeyListener: TOnDialogKeyListener read FOnKeyListener;
    property OnKeyListenerA: TOnDialogKeyListenerA read FOnKeyListenerA;

    property OnCheckboxClickListener: TOnDialogMultiChoiceClickListener read FOnCheckboxClickListener;
    property OnItemSelectedListener: TOnDialogItemSelectedListener read FOnItemSelectedListener;
    property OnClickListener: TOnDialogClickListener read FOnClickListener;
    property OnClickListenerA: TOnDialogClickListenerA read FOnClickListenerA;
  end;

type
  /// <summary>
  /// 对话框组件
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TAlertDialog = class(TCustomAlertDialog)
  published
    property Cancelable default True;
    property Title;
    property Message;
    property OnCancelListener;
    property OnShowListener;
    property OnKeyListener;
    property OnDismissListener;
  end;

type
  /// <summary>
  /// 等待对话框
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TProgressDialog = class(TDialog)
  private
    [Weak] FStyleManager: TDialogStyleManager;
  protected
    function GetMessage: string; override;
    procedure SetMessage(const Value: string); override;
    procedure DoRootClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitView(const AMsg: string);
    /// <summary>
    /// 显示一个等待对话框
    /// </summary>
    class function Show(AOwner: TComponent; const AMsg: string; ACancelable: Boolean = True): TProgressDialog;
  published
    property StyleManager: TDialogStyleManager read FStyleManager write FStyleManager;
  end;

implementation

var
  DefaultStyleManager: TDialogStyleManager = nil;
  DialogRef: Integer = 0;

function GetDefaultStyleMgr: TDialogStyleManager;
begin
  if DefaultStyleManager = nil then
    DefaultStyleManager := TDialogStyleManager.Create(nil);
  Result := DefaultStyleManager;
end;

{ TDialogBuilder }

constructor TDialogBuilder.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FView := nil;
  FCancelable := True;
  FItemSingleLine := True;
  FClickButtonDismiss := True;
  FMaskVisible := True;
  FUseRootBackColor := False;
  FRootBackColor := TAlphaColorRec.Null;
  FIcon := nil;
end;

function TDialogBuilder.CreateDialog: IDialog;
var
  Dlg: TAlertDialog;
begin
  Dlg := TAlertDialog.Create(FOwner);
  try
    Dlg.Apply(Self);
    Dlg.SetCancelable(FCancelable);
    Dlg.SetOnCancelListener(FOnCancelListener);
    Dlg.SetOnCancelListenerA(FOnCancelListenerA);
    if Assigned(FOnKeyListener) then
      Dlg.SetOnKeyListener(FOnKeyListener);
    Result := Dlg;
  except
    FreeAndNil(Dlg);
    Result := nil;
  end;
end;

destructor TDialogBuilder.Destroy;
begin
  FIcon := nil;
  FIcon := nil;
  FItems := nil;
  if Assigned(FView) then begin
    FView.Parent := nil;
    if FViewAutoFree then begin
      if not (csDestroying in FView.ComponentState) then
        FreeAndNil(FView);
    end;
  end;
  inherited;
end;

function TDialogBuilder.GetCheckedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(FCheckedItems) - 1 do
    if FCheckedItems[I] then
      Inc(Result);
end;

function TDialogBuilder.SetIcon(AIcon: TBrush): TDialogBuilder;
begin
  Result := Self;
  FIcon := AIcon;
end;

function TDialogBuilder.SetCancelable(ACancelable: Boolean): TDialogBuilder;
begin
  Result := Self;
  FCancelable := ACancelable;
end;

function TDialogBuilder.SetClickButtonDismiss(V: Boolean): TDialogBuilder;
begin
  Result := Self;
  FClickButtonDismiss := V;
end;

function TDialogBuilder.SetData(const V: TObject): TDialogBuilder;
begin
  Result := Self;
  FDataObject := V;
end;

function TDialogBuilder.SetData(const V: TValue): TDialogBuilder;
begin
  Result := Self;
  FData := V; 
end;

function TDialogBuilder.SetSingleChoiceItems(AItems: TStrings;
  ACheckedItem: Integer; AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FItems := AItems;
  FOnClickListenerA := AListener;
  FCheckedItem := ACheckedItem;
  FIsSingleChoice := True;
end;

function TDialogBuilder.SetSingleChoiceItems(const AItems: TArray<string>;
  ACheckedItem: Integer; AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FItemArray := AItems;
  FOnClickListenerA := AListener;
  FCheckedItem := ACheckedItem;
  FIsSingleChoice := True;
end;

function TDialogBuilder.SetSingleChoiceItems(const AItems: TArray<string>;
  ACheckedItem: Integer; AListener: TOnDialogClickListener): TDialogBuilder;
begin
  Result := Self;
  FItemArray := AItems;
  FOnClickListener := AListener;
  FCheckedItem := ACheckedItem;
  FIsSingleChoice := True;
end;

function TDialogBuilder.SetStyleManager(
  AValue: TDialogStyleManager): TDialogBuilder;
begin
  Result := Self;
  FStyleManager := AValue;
end;

function TDialogBuilder.SetIcon(AIcon: TDrawableBase): TDialogBuilder;
begin
  Result := Self;
  FIcon := AIcon;
end;

function TDialogBuilder.SetIcon(AIcon: TBrushBitmap): TDialogBuilder;
begin
  Result := Self;
  FIcon := AIcon;
end;

function TDialogBuilder.SetIsMaxWidth(AIsMaxWidth: Boolean): TDialogBuilder;
begin
  Result := Self;
  FIsMaxWidth := AIsMaxWidth;
end;

function TDialogBuilder.SetItems(AItems: TStrings;
  AListener: TOnDialogClickListener): TDialogBuilder;
begin
  Result := Self;
  FItems := AItems;
  FIsMultiChoice := False;
  FOnClickListener := AListener;
end;

function TDialogBuilder.SetItems(AItems: TStrings;
  AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FItems := AItems;
  FIsMultiChoice := False;
  FOnClickListenerA := AListener;
end;


function TDialogBuilder.SetItems(const AItems: TArray<string>;
  AListener: TOnDialogClickListener): TDialogBuilder;
begin
  Result := Self;
  FItemArray := AItems;
  FIsMultiChoice := False;
  FOnClickListener := AListener;
end;

function TDialogBuilder.SetItems(const AItems: TArray<string>;
  AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FItemArray := AItems;
  FIsMultiChoice := False;
  FOnClickListenerA := AListener;
end;

function TDialogBuilder.SetItemSingleLine(
  AItemSingleLine: Boolean): TDialogBuilder;
begin
  FItemSingleLine := AItemSingleLine;
  Result := Self;
end;

function TDialogBuilder.SetMaskVisible(V: Boolean): TDialogBuilder;
begin
  Result := Self;
  FMaskVisible := V;
end;

function TDialogBuilder.SetMessage(const AMessage: string): TDialogBuilder;
begin
  Result := Self;
  FMessage := AMessage;
end;

function TDialogBuilder.SetMultiChoiceItems(const AItems: TArray<string>;
  ACheckedItems: TArray<Boolean>;
  AListener: TOnDialogMultiChoiceClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FItemArray := AItems;
  FOnCheckboxClickListenerA := AListener;
  FCheckedItems := ACheckedItems;
  FIsMultiChoice := True;
end;

function TDialogBuilder.SetMultiChoiceItems(const AItems: TArray<string>;
  ACheckedItems: TArray<Boolean>;
  AListener: TOnDialogMultiChoiceClickListener): TDialogBuilder;
begin
  Result := Self;
  FItemArray := AItems;
  FOnCheckboxClickListener := AListener;
  FCheckedItems := ACheckedItems;
  FIsMultiChoice := True;
end;

function TDialogBuilder.SetMultiChoiceItems(AItems: TStrings;
  ACheckedItems: TArray<Boolean>;
  AListener: TOnDialogMultiChoiceClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FItems := AItems;
  FOnCheckboxClickListenerA := AListener;
  FCheckedItems := ACheckedItems;
  FIsMultiChoice := True;
end;

function TDialogBuilder.SetMultiChoiceItems(AItems: TStrings;
  ACheckedItems: TArray<Boolean>;
  AListener: TOnDialogMultiChoiceClickListener): TDialogBuilder;
begin
  result := Self;
  FItems := AItems;
  FOnCheckboxClickListener := AListener;
  FCheckedItems := ACheckedItems;
  FIsMultiChoice := True;
end;

function TDialogBuilder.SetNegativeButton(const AText: string;
  AListener: TOnDialogClickListener): TDialogBuilder;
begin
  Result := Self;
  FNegativeButtonText := AText;
  FNegativeButtonListener := AListener;
end;

function TDialogBuilder.SetNeutralButton(const AText: string;
  AListener: TOnDialogClickListener): TDialogBuilder;
begin
  Result := Self;
  FNeutralButtonText := AText;
  FNeutralButtonListener := AListener;
end;

function TDialogBuilder.SetOnCancelListener(
  AListener: TOnDialogListener): TDialogBuilder;
begin
  Result := Self;
  FOnCancelListener := AListener;
end;

function TDialogBuilder.SetOnCancelListener(
  AListener: TOnDialogListenerA): TDialogBuilder;
begin
  Result := Self;
  FOnCancelListenerA := AListener;
end;

function TDialogBuilder.SetOnItemSelectedListener(
  AListener: TOnDialogItemSelectedListenerA): TDialogBuilder;
begin
  Result := Self;
  FOnItemSelectedListenerA := AListener;
end;

function TDialogBuilder.SetOnItemSelectedListener(
  AListener: TOnDialogItemSelectedListener): TDialogBuilder;
begin
  Result := Self;
  FOnItemSelectedListener := AListener;
end;

function TDialogBuilder.SetOnKeyListener(
  AListener: TOnDialogKeyListenerA): TDialogBuilder;
begin
  Result := Self;
  FOnKeyListenerA := AListener;
end;

function TDialogBuilder.SetOnKeyListener(
  AListener: TOnDialogKeyListener): TDialogBuilder;
begin
  Result := Self;
  FOnKeyListener := AListener;
end;

function TDialogBuilder.SetPositiveButton(const AText: string;
  AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FPositiveButtonText := AText;
  FPositiveButtonListenerA := AListener;    
end;

function TDialogBuilder.SetRootBackColor(const V: TAlphaColor): TDialogBuilder;
begin
  Result := Self;
  FRootBackColor := V;
  FUseRootBackColor := True;
end;

function TDialogBuilder.SetPositiveButton(const AText: string;
  AListener: TOnDialogClickListener): TDialogBuilder;
begin
  Result := Self;
  FPositiveButtonText := AText;
  FPositiveButtonListener := AListener;
end;

function TDialogBuilder.SetSingleChoiceItems(AItems: TStrings;
  ACheckedItem: Integer; AListener: TOnDialogClickListener): TDialogBuilder;
begin
  Result := Self;
  FItems := AItems;
  FOnClickListener := AListener;
  FCheckedItem := ACheckedItem;
  FIsSingleChoice := True;
end;

function TDialogBuilder.SetTag(const V: Integer): TDialogBuilder;
begin
  Result := Self;
  FTag := V;
end;

function TDialogBuilder.SetTitle(const ATitle: string): TDialogBuilder;
begin
  Result := Self;
  FTitle := ATitle;
end;

function TDialogBuilder.SetView(AView: TControl; AViewAutoFree: Boolean): TDialogBuilder;
begin
  Result := Self;
  FView := AView;
  FViewAutoFree := AViewAutoFree;
end;

function TDialogBuilder.Show(OnDismissListener: TOnDialogListener): IDialog;
begin
  Result := CreateDialog();
  if Assigned(Result) then begin
    if Assigned(OnDismissListener) then
      (Result as TAlertDialog).FOnDismissListener := OnDismissListener;
    Result.Show();
  end;
end;

function TDialogBuilder.Show(OnDismissListener: TOnDialogListenerA): IDialog;
begin
  Result := CreateDialog();
  if Assigned(Result) then begin
    if Assigned(OnDismissListener) then
      (Result as TAlertDialog).FOnDismissListenerA := OnDismissListener;
    Result.Show();
  end;
end;

function TDialogBuilder.Show: IDialog;
begin
  Result := CreateDialog();
  if Assigned(Result) then
    Result.Show();
end;

function TDialogBuilder.SetNegativeButton(const AText: string;
  AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FNegativeButtonText := AText;
  FNegativeButtonListenerA := AListener;
end;

function TDialogBuilder.SetNeutralButton(const AText: string;
  AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FNeutralButtonText := AText;
  FNeutralButtonListenerA := AListener;
end;

{ TDialog }

procedure TDialog.AsyncDismiss;
begin
  DoAsyncDismiss();
end;

procedure TDialog.Cancel;
begin
  if (not FCanceled) then begin
    FCanceled := True;
    if Assigned(FOnCancelListenerA) then
      FOnCancelListenerA(Self)
    else if Assigned(FOnCancelListener) then
      FOnCancelListener(Self);
  end;
  DoAsyncDismiss;
end;

procedure TDialog.Close;
begin
  Dismiss;
end;

class procedure TDialog.CloseDialog(const Target: TControl);
var
  Dialog: IDialog;
begin
  Dialog := GetDialog(Target);
  if Assigned(Dialog) then
    Dialog.Dismiss;
end;

constructor TDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCancelable := True;
  FCanceled := False;
end;

destructor TDialog.Destroy;
begin
  AtomicDecrement(DialogRef);
  if Assigned(Self) then begin
    FIsDismiss := True;
    if (FViewRoot <> nil) then begin
      if Assigned(FTimer) then
        FTimer.OnTimer := nil;
      FEventing := False;
      Dismiss;
      FViewRoot := nil;
    end;
    if Assigned(FTimer) then begin
      FTimer.Enabled := False;
      FTimer := nil;
    end;
  end;
  FViewRoot := nil;
  inherited Destroy;
end;

procedure TDialog.Dismiss;
var
  LParent: TFmxObject;
begin
  if not Assigned(Self) then
    Exit;
  if FEventing then begin
    FAllowDismiss := True;
    Exit;
  end;
  if FIsDismiss then
    Exit;
  FIsDismiss := True;
  if Assigned(FOnDismissListenerA) then begin
    FOnDismissListenerA(Self);
    FOnDismissListenerA := nil;
    FOnDismissListener := nil;
  end else if Assigned(FOnDismissListener) then begin
    FOnDismissListener(Self);
    FOnDismissListener := nil;
  end;
  DoFreeBuilder();
  if (FViewRoot <> nil) and Assigned(FViewRoot.Parent) then begin
    LParent := FViewRoot.Parent;
    FViewRoot.Parent.RemoveObject(FViewRoot);
    FreeAndNil(FViewRoot);
    if LParent <> nil then begin
      if LParent is TControl then
        TControl(LParent).SetFocus
      else if LParent is TCustomForm then
        // 暂不处理
    end;
  end;
  if not (csDestroying in ComponentState) then
    DisposeOf;
end;

procedure TDialog.DoApplyTitle;
begin
end;

procedure TDialog.DoAsyncDismiss;
begin
  if Assigned(Self) and Assigned(Owner) then begin
    if FEventing then begin
      FAllowDismiss := True;
      Exit;
    end;
    FreeAndNil(FTimer);
    if (FViewRoot <> nil) then begin
      if (FViewRoot.FLayBubble <> nil) then
        FViewRoot.FLayBubble.Visible := False
      else if FViewRoot.ControlsCount = 1 then // ShowView 时会是这种情况
        FViewRoot.Controls[0].Visible := False;
    end;
    FDestBgColor := $00000000;
    FTimer := TTimer.Create(Owner);
    FTimer.OnTimer := DoAsyncDismissTimer;
    FTimer.Interval := 15;
    FTimer.Enabled := True;
    FTimerStart := GetTimestamp;
  end;
end;

procedure TDialog.DoAsyncDismissTimer(Sender: TObject);
var
  T: Single;
begin
  if Assigned(Self) then begin
    T := (GetTimestamp - FTimerStart) / 200;
    if FViewRoot <> nil then
      FViewRoot.Background.ItemDefault.Color := LerpColor(FViewRoot.Background.ItemDefault.Color, FDestBgColor, T);
    if T > 1 then begin
      if Assigned(FTimer) then
        FTimer.Enabled := False;
      Dismiss;
    end;
  end;
end;

procedure TDialog.DoFreeBuilder;
begin
end;

procedure TDialog.DoRootClick(Sender: TObject);
begin
  if FCancelable then
    Cancel;
end;

procedure TDialog.DoUpdateBackTimer(Sender: TObject);
var
  T: Single;
begin
  if Assigned(Self) and (Assigned(FTimer)) then begin
    if csDestroying in ComponentState then
      Exit;
    T := (GetTimestamp - FTimerStart) / 1000;
    if FViewRoot <> nil then begin
      FViewRoot.Background.ItemDefault.Color := LerpColor(FViewRoot.Background.ItemDefault.Color, FDestBgColor, T);
      if (T > 0.5) and (FViewRoot.FLayBubble <> nil) then
        FViewRoot.FLayBubble.Visible := True;
    end;
    if T > 1 then
      FreeAndNil(FTimer);
  end;
end;

function TDialog.GetBuilder: TDialogBuilder;
begin
  Result := nil;
end;

function TDialog.GetCancelable: Boolean;
begin
  Result := FCancelable;
end;

class function TDialog.GetDialog(const Target: TControl): IDialog;
var
  V: TControl;
begin
  Result := nil;
  if (Target = nil) or (Target.Parent = nil) then Exit;
  V := Target.ParentControl;
  while V <> nil do begin
    if V is TDialogView then begin
      Result := (V as TDialogView).FDialog;
      Break;
    end;
    V := V.ParentControl;
  end;
end;

function TDialog.GetFirstParent: TFmxObject;
var
  P: TFmxObject;
begin
  if Owner is TFmxObject then begin
    P := TFmxObject(Owner);
    while P.Parent <> nil do
      P := P.Parent;
    Result := P;
  end else
    Result := nil;
end;

function TDialog.GetIsDismiss: Boolean;
begin
  Result := (not Assigned(Self)) or FIsDismiss;
end;

function TDialog.GetMessage: string;
begin
  Result := '';
end;

function TDialog.GetRootView: TDialogView;
begin
  Result := FViewRoot;
end;

function TDialog.GetView: TControl;
begin
  if FViewRoot <> nil then
    Result := FViewRoot.FLayBubble
  else
    Result := nil;
end;

procedure TDialog.Hide;
begin
  if FViewRoot <> nil then
    FViewRoot.Hide;
end;

procedure TDialog.InitOK;
begin
  if FViewRoot <> nil then
    FViewRoot.InitOK;
end;

procedure TDialog.NotifyDataSetChanged;
begin
  if Assigned(FViewRoot) then
    FViewRoot.Repaint;
end;

procedure TDialog.SetBackColor(const Value: TAlphaColor);
begin
  FreeAndNil(FTimer);
  if FDestBgColor <> Value then begin
    FViewRoot.Background.ItemDefault.Color := FDestBgColor;
    FDestBgColor := Value;
    FTimer := TTimer.Create(Owner);
    FTimer.OnTimer := DoUpdateBackTimer;
    FTimer.Interval := 20;
    FTimer.Enabled := True;
    FTimerStart := GetTimestamp;
  end;
end;

procedure TDialog.SetCancelable(const Value: Boolean);
begin
  FCancelable := Value;
end;

procedure TDialog.SetMessage(const Value: string);
begin
end;

procedure TDialog.SetOnCancelListener(const Value: TOnDialogListener);
begin
  FOnCancelListener := Value;
end;

procedure TDialog.SetOnCancelListenerA(const Value: TOnDialogListenerA);
begin
  FOnCancelListenerA := Value;
end;

procedure TDialog.Show;
begin
  try
    if Assigned(FViewRoot) then begin
      DoApplyTitle();

      if Assigned(FOnShowListenerA) then
        FOnShowListenerA(Self)
      else if Assigned(FOnCancelListener) then
        FOnCancelListener(Self);

      FViewRoot.Show;

      if Assigned(FViewRoot.FLayBubble) then begin
        // 如果图层完全不可见，设置动画时会出错
        if (FViewRoot.FLayBubble.Background.ItemDefault.Color and $FF000000 <> 0) then begin
          FViewRoot.FLayBubble.Opacity := 0.3;
          TAnimator.AnimateFloat(FViewRoot.FLayBubble, 'Opacity', 1, 0.3);
        end;
      end;
    end;
  except
    {$IFDEF WINDOWS}LogE(Self, 'Show', Exception(ExceptObject)); {$ENDIF}
    Dismiss;
  end;
end;

class function TDialog.ShowView(const AOwner: TComponent; const Target: TControl;
  const ViewClass: TControlClass; XOffset: Single; YOffset: Single;
  Position: TDialogViewPosition; Cancelable: Boolean): TDialog;
var
  AView: TControl;
begin
  AView := ViewClass.Create(AOwner);
  Result := ShowView(AOwner, Target, AView, True, XOffset, YOffset, Position, Cancelable);
end;

class function TDialog.ShowView(const AOwner: TComponent; const Target, View: TControl;
  AViewAutoFree: Boolean; XOffset: Single; YOffset: Single;
  Position: TDialogViewPosition; Cancelable: Boolean): TDialog;
var
  Dialog: TDialog;
  X, Y, PW, PH: Single;
  P: TPointF;
begin
  Result := nil;
  if View = nil then Exit;
  AtomicIncrement(DialogRef);

  Dialog := TDialog.Create(AOwner);
  Dialog.FViewRoot := TDialogView.Create(AOwner);
  Dialog.FViewRoot.Dialog := Dialog;
  Dialog.FViewRoot.BeginUpdate;
  Dialog.FViewRoot.OnClick := Dialog.DoRootClick;
  Dialog.FViewRoot.Parent := Dialog.GetFirstParent;
  if Dialog.FViewRoot.Parent = nil then begin
    Dialog.Dismiss;
    Exit;
  end;

  Dialog.FViewRoot.Clickable := True;
  Dialog.FViewRoot.Align := TAlignLayout.Client;
  Dialog.FViewRoot.Index := Dialog.FViewRoot.Parent.ChildrenCount - 1;
  Dialog.FViewRoot.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  Dialog.FViewRoot.CanFocus := False;

  View.Name := '';
  View.Parent := Dialog.FViewRoot;
  X := 0;
  Y := 0;
  if Assigned(Target) then begin
    P := TPointF.Zero;
    P := Target.LocalToAbsolute(P);
    PW := Target.Width;
    PH := Target.Height;
    case Position of
      Top:
        begin
          X := (PW - View.Width) / 2 + P.X + XOffset;
          Y := P.Y - View.Height - YOffset;
        end;
      Bottom:
        begin
          X := (PW - View.Width) / 2 + P.X + XOffset;
          Y := P.Y + PH + YOffset;
        end;
      Left:
        begin
          X := P.X - View.Width - XOffset;
          Y := (PH - View.Height) / 2 + P.Y + YOffset;
        end;
      Right:
        begin
          X := P.X + PW + XOffset;
          Y := (PH - View.Height) / 2 + P.Y + YOffset;
        end;
      Center:
        begin
          X := (PW - View.Width) / 2 + P.X + XOffset;
          Y := (PH - View.Height) / 2 + P.Y + YOffset;
        end;
    end;
  end else begin
    PW := Dialog.FViewRoot.Width;
    PH := Dialog.FViewRoot.Height;
    case Position of
      Top:
        begin
          X := (PW - View.Width) / 2 + XOffset;
          Y := 0 + YOffset;
        end;
      Bottom:
        begin
          X := (PW - View.Width) / 2 + XOffset;
          Y := PH - View.Height - YOffset;
        end;
      Left:
        begin
          X := 0 + XOffset;
          Y := (PH - View.Height) / 2 + YOffset;
        end;
      Right:
        begin
          X := PW - View.Width - XOffset;
          Y := (PH - View.Height) / 2 + YOffset;
        end;
      Center:
        begin
          X := (PW - View.Width) / 2 + XOffset;
          Y := (PH - View.Height) / 2 + YOffset;
        end;
    end;
  end;
  View.Position.Point := TPointF.Create(X, Y);

  Dialog.Cancelable := Cancelable;
  Dialog.SetBackColor(GetDefaultStyleMgr.FDialogMaskColor);
  Dialog.InitOK;
  Result := Dialog;
end;

{ TCustomAlertDialog }

procedure TCustomAlertDialog.Apply(const ABuilder: TDialogBuilder);
begin
  AtomicIncrement(DialogRef);
  FBuilder := ABuilder;
  if ABuilder = nil then Exit;
  if ABuilder.View <> nil then
    // 附加 View 的对话框
    InitExtPopView()
  else if ABuilder.FIsSingleChoice then
    // 单选对话框
    InitSinglePopView()
  else if ABuilder.FIsMultiChoice then
    // 多选对话框
    InitMultiPopView()
  else if (Length(ABuilder.FItemArray) > 0) or
    (Assigned(ABuilder.Items) and (ABuilder.Items.Count > 0)) then
    // 列表框
    InitListPopView()
  else
    // 基本对话框
    InitDefaultPopView();
  InitOK();
end;

procedure TCustomAlertDialog.DoApplyTitle;
begin
  SetTitle(FBuilder.FTitle);
end;

constructor TCustomAlertDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCustomAlertDialog.Destroy;
begin
  FreeAndNil(FBuilder);
  inherited Destroy;
end;

procedure TCustomAlertDialog.DoButtonClick(Sender: TObject);
begin
  if FViewRoot <> nil then begin
    FEventing := True;
    FAllowDismiss := False;
    try
      if Sender = FViewRoot.FButtonPositive then begin
        if Assigned(Builder.FPositiveButtonListenerA) then
          Builder.FPositiveButtonListenerA(Self, BUTTON_POSITIVE)
        else if Assigned(Builder.PositiveButtonListener) then
          Builder.PositiveButtonListener(Self, BUTTON_POSITIVE)
        else  // 没有事件的按钮点击后关闭对话框
          FAllowDismiss := True;
      end else if Sender = FViewRoot.FButtonNegative then begin
        if Assigned(Builder.FNegativeButtonListenerA) then
          Builder.FNegativeButtonListenerA(Self, BUTTON_NEGATIVE)
        else if Assigned(Builder.NegativeButtonListener) then
          Builder.NegativeButtonListener(Self, BUTTON_NEGATIVE)
        else
          FAllowDismiss := True;
      end else if Sender = FViewRoot.FButtonNeutral then begin
        if Assigned(Builder.FNeutralButtonListenerA) then
          Builder.FNeutralButtonListenerA(Self, BUTTON_NEUTRAL)
        else if Assigned(Builder.NeutralButtonListener) then
          Builder.NeutralButtonListener(Self, BUTTON_NEUTRAL)
        else
          FAllowDismiss := True;
      end;
    except
    end;
    FEventing := False;
    if FAllowDismiss or (Assigned(Builder) and Builder.FClickButtonDismiss) then begin
      FAllowDismiss := False;
      AsyncDismiss;
    end;
  end;
end;

procedure TCustomAlertDialog.DoFreeBuilder;
begin
  if Assigned(FBuilder) then
    FreeAndNil(FBuilder);
end;

procedure TCustomAlertDialog.DoListItemClick(Sender: TObject; ItemIndex: Integer;
  const ItemView: TControl);
var
  B: Boolean;
begin
  if (FViewRoot = nil) or (FViewRoot.FListView = nil) then Exit;
  FEventing := True;
  FAllowDismiss := False;
  try
    if FBuilder.FIsMultiChoice then begin
      B := TStringsListCheckAdapter(TListViewEx(Sender).Adapter).ItemCheck[ItemIndex];
      if Length(FBuilder.FCheckedItems) > ItemIndex then
        FBuilder.FCheckedItems[ItemIndex] := B;

      if Assigned(FBuilder.FOnCheckboxClickListenerA) then
        FBuilder.FOnCheckboxClickListenerA(Self, ItemIndex, B)
      else if Assigned(FBuilder.FOnCheckboxClickListener) then
        FBuilder.FOnCheckboxClickListener(Self, ItemIndex, B);

    end else begin
      if FBuilder.FIsSingleChoice then
        FBuilder.FCheckedItem := ItemIndex;

      if Assigned(FBuilder.OnClickListenerA) then
        FBuilder.OnClickListenerA(Self, ItemIndex)
      else if Assigned(FBuilder.OnClickListener) then
        FBuilder.OnClickListener(Self, ItemIndex);

      if (not (FBuilder.FIsMultiChoice or FBuilder.FIsSingleChoice)) and (not FAllowDismiss) then
        DoAsyncDismiss;
    end;
  except
  end;
  FEventing := False;
  if FAllowDismiss then begin
    FAllowDismiss := False;
    DoAsyncDismiss;
  end;
end;

function TCustomAlertDialog.GetBuilder: TDialogBuilder;
begin
  Result := FBuilder;
end;

function TCustomAlertDialog.GetItems: TStrings;
begin
  if Assigned(FBuilder) then
    Result := FBuilder.FItems
  else
    Result := nil;
end;

function TCustomAlertDialog.GetMessage: string;
begin
  if Assigned(FBuilder) then
    Result := FBuilder.FMessage
  else
    Result := '';
end;

function TCustomAlertDialog.GetTitle: string;
begin
  if Assigned(FBuilder) then
    Result := FBuilder.FTitle
  else
    Result := '';
end;

procedure TCustomAlertDialog.InitDefaultPopView;
var
  StyleManager: TDialogStyleManager;
  BtnCount: Integer;
  FButtomRadius: TView;
  BodyMH: Single;
begin
  StyleManager := FBuilder.FStyleManager;
  if StyleManager = nil then
    StyleManager := GetDefaultStyleMgr;
  // 初始化基础
  FButtomRadius := nil;
  FViewRoot := TDialogView.Create(Owner);
  FViewRoot.Dialog := Self;
  FViewRoot.BeginUpdate;
  FViewRoot.OnClick := DoRootClick;
  FViewRoot.Parent := GetFirstParent;
  if FViewRoot.Parent = nil then begin
    Dismiss;
    Exit;
  end;
  FViewRoot.Clickable := True;
  FViewRoot.Align := TAlignLayout.Client;
  FViewRoot.Index := FViewRoot.Parent.ChildrenCount - 1;
  FViewRoot.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FViewRoot.InitView(StyleManager);

  // 初始化消息区
  if (Builder.FIcon <> nil) or (Builder.FMessage <> '') then begin
    FViewRoot.InitMessage(StyleManager);
    FViewRoot.FMsgMessage.Text := Builder.FMessage;
    if Assigned(Builder.FIcon) then begin
      if Builder.FIcon is TDrawableBase then
        FViewRoot.FMsgMessage.Drawable.Assign(TDrawableBase(Builder.FIcon))
      else if Builder.FIcon is TBrush then
        FViewRoot.FMsgMessage.Drawable.ItemDefault.Assign(TBrush(Builder.FIcon))
      else if Builder.FIcon is TBrushBitmap then begin
        FViewRoot.FMsgMessage.Drawable.ItemDefault.Bitmap.Assign(TBrushBitmap(Builder.FIcon));
        FViewRoot.FMsgMessage.Drawable.ItemDefault.Kind := TBrushKind.Bitmap;
      end;
    end;
    FButtomRadius := FViewRoot.FMsgMessage;
  end else
    FViewRoot.FMsgBody.Visible := False;

  // 初始化列表
  if (Length(Builder.FItemArray) > 0) or
    ((Assigned(Builder.FItems)) and (Builder.FItems.Count > 0)) then begin
    FViewRoot.InitList(StyleManager);
  end;

  // 初始化按钮
  BtnCount := 0;
  FViewRoot.InitButton(StyleManager);
  if Builder.PositiveButtonText = '' then
    FViewRoot.FButtonPositive.Visible := False
  else begin
    FViewRoot.FButtonPositive.Text := Builder.PositiveButtonText;
    FViewRoot.FButtonPositive.OnClick := DoButtonClick;
    Inc(BtnCount);
    FViewRoot.FButtonPositive.Background.Corners := [TCorner.BottomLeft];
    FButtomRadius := FViewRoot.FButtonPositive;
  end;
  if Builder.NegativeButtonText = '' then
    FViewRoot.FButtonNegative.Visible := False
  else begin
    FViewRoot.FButtonNegative.Text := Builder.NegativeButtonText;
    FViewRoot.FButtonNegative.OnClick := DoButtonClick;
    Inc(BtnCount);
    if BtnCount = 1 then
      FViewRoot.FButtonNegative.Background.Corners := [TCorner.BottomLeft];
    FButtomRadius := FViewRoot.FButtonNegative;
  end;
  if Builder.NeutralButtonText = '' then begin
    FViewRoot.FButtonNeutral.Visible := False;
    if Assigned(FButtomRadius) then begin
      if BtnCount = 1 then
        FButtomRadius.Background.Corners := [TCorner.BottomLeft, TCorner.BottomRight]
      else
        FButtomRadius.Background.Corners := [TCorner.BottomRight];
    end;
  end else begin
    FViewRoot.FButtonNeutral.Text := Builder.NeutralButtonText;
    FViewRoot.FButtonNeutral.OnClick := DoButtonClick;
    Inc(BtnCount);
    if BtnCount = 1 then
      FViewRoot.FButtonNeutral.Background.Corners := [TCorner.BottomLeft, TCorner.BottomRight]
    else
      FViewRoot.FButtonNeutral.Background.Corners := [TCorner.BottomRight];
  end;
  if (BtnCount = 0) and (FViewRoot.FButtonLayout <> nil) then begin
    FViewRoot.FButtonLayout.Visible := False;
  end;

  if (Builder.Title = '') or (BtnCount = 0) then begin
    FViewRoot.FMsgBody.Background.XRadius := StyleManager.FBackgroundRadius;
    FViewRoot.FMsgBody.Background.YRadius := StyleManager.FBackgroundRadius;
    if BtnCount = 0 then begin
      if Builder.Title = '' then
        FViewRoot.FMsgBody.Background.Corners := [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]
      else
        FViewRoot.FMsgBody.Background.Corners := [TCorner.BottomLeft, TCorner.BottomRight]
    end else
      FViewRoot.FMsgBody.Background.Corners := [TCorner.TopLeft, TCorner.TopRight];
  end;

  // 设置 Body 最大高度
  if Assigned(FViewRoot.FMsgBody) then begin
    BodyMH := FViewRoot.FLayBubble.MaxHeight;
    if BtnCount > 0 then
      BodyMH := BodyMH - FViewRoot.FButtonLayout.Height;
    if Assigned(FViewRoot.FTitleView) and (FViewRoot.FTitleView.Visible) then
      BodyMH := BodyMH - FViewRoot.FTitleView.Height;
    FViewRoot.FMsgBody.MaxHeight := BodyMH;

    if Assigned(FViewRoot.FListView) then begin
      if Assigned(FViewRoot.FMsgMessage) and (FViewRoot.FMsgMessage.Visible) then
        FViewRoot.FListView.MaxHeight := BodyMH - FViewRoot.FMsgMessage.Height
      else
        FViewRoot.FListView.MaxHeight := BodyMH;

      if BtnCount = 0 then
        FViewRoot.FListView.Margins.Bottom := StyleManager.FBackgroundRadius;
    end;
  end;

  if Builder.FMaskVisible then
    SetBackColor(StyleManager.FDialogMaskColor);
end;

procedure TCustomAlertDialog.InitExtPopView;
begin
  InitDefaultPopView;
  FViewRoot.FMsgBody.Visible := True;
  if Assigned(FViewRoot.FMsgMessage) then
    FViewRoot.FMsgMessage.Visible := False;
  with Builder.View do begin
    Name := '';
    Parent := FViewRoot.FMsgBody;
    Index := FViewRoot.FButtonLayout.Index - 1;
    Align := TAlignLayout.Client;
  end;
  FViewRoot.FMsgBody.Height := Builder.View.Height;
end;

procedure TCustomAlertDialog.InitList(const ListView: TListViewEx; IsMulti: Boolean);
var
  Adapter: IListAdapter;
begin
  if Length(FBuilder.FItemArray) > 0 then begin
    if IsMulti then begin
      Adapter := TStringsListCheckAdapter.Create(Builder.FItemArray);
      TStringsListCheckAdapter(Adapter).Checks := FBuilder.FCheckedItems;
    end else if FBuilder.IsSingleChoice then begin
      Adapter := TStringsListSingleAdapter.Create(Builder.FItemArray);
      if (Builder.FCheckedItem >= 0) and (Builder.FCheckedItem < Adapter.Count) then
        TStringsListSingleAdapter(Adapter).ItemIndex := Builder.FCheckedItem;
    end else begin
      Adapter := TStringsListAdapter.Create(Builder.FItemArray);
    end;
  end else if Assigned(FBuilder.FItems) and (FBuilder.FItems.Count > 0) then begin
    if IsMulti then begin
      Adapter := TStringsListCheckAdapter.Create(FBuilder.FItems);
      TStringsListCheckAdapter(Adapter).Checks := FBuilder.FCheckedItems;
    end else if FBuilder.IsSingleChoice then begin
      Adapter := TStringsListSingleAdapter.Create(FBuilder.FItems);
      if (Builder.FCheckedItem >= 0) and (Builder.FCheckedItem < Adapter.Count) then
        TStringsListSingleAdapter(Adapter).ItemIndex := Builder.FCheckedItem;
    end else begin
      Adapter := TStringsListAdapter.Create(FBuilder.FItems);
    end;
  end;
  ListView.Adapter := Adapter;
  ListView.Height := ListView.ContentBounds.Height;
end;

procedure TCustomAlertDialog.InitListPopView;
var
  ListView: TListViewEx;
begin
  InitDefaultPopView;
  FViewRoot.FMsgBody.Visible := True;
  if Assigned(FViewRoot.FMsgMessage) then begin
    if FBuilder.Message = '' then
      FViewRoot.FMsgMessage.Visible := False;
  end;

  // 初始化列表
  ListView := FViewRoot.FListView;
  InitList(ListView);
  ListView.OnItemClick := DoListItemClick;
end;

procedure TCustomAlertDialog.InitMultiPopView;
var
  ListView: TListViewEx;
begin
  InitDefaultPopView;
  FViewRoot.FMsgBody.Visible := True;
  if Assigned(FViewRoot.FMsgMessage) then begin
    if FBuilder.Message = '' then
      FViewRoot.FMsgMessage.Visible := False;
  end;

  // 初始化列表
  ListView := FViewRoot.FListView;
  InitList(ListView, True);
  if Length(Builder.FCheckedItems) < ListView.Count then
    SetLength(Builder.FCheckedItems, ListView.Count);
  ListView.EndUpdate;
  ListView.OnItemClick := DoListItemClick;
end;

procedure TCustomAlertDialog.InitSinglePopView;
var
  ListView: TListViewEx;
begin
  InitDefaultPopView;
  FViewRoot.FMsgBody.Visible := True;
  if Assigned(FViewRoot.FMsgMessage) then begin
    if FBuilder.Message = '' then
      FViewRoot.FMsgMessage.Visible := False;
  end;

  // 初始化列表
  ListView := FViewRoot.FListView;
  InitList(ListView);
  ListView.OnItemClick := DoListItemClick;
end;

procedure TCustomAlertDialog.SetMessage(const Value: string);
begin
  if Assigned(FBuilder) then
    FBuilder.FMessage := Value;
end;

procedure TCustomAlertDialog.SetOnKeyListener(const Value: TOnDialogKeyListener);
begin
  FOnKeyListener := Value;
end;

procedure TCustomAlertDialog.SetTitle(const Value: string);
begin
  if Assigned(FBuilder) then
    FBuilder.FTitle := Value;
  if Assigned(FViewRoot) then
    FViewRoot.SetTitle(Value);
end;


{ TDialogView }

procedure TDialogView.AfterDialogKey(var Key: Word; Shift: TShiftState);
begin
  // 如果按下了返回键，且允许取消对话框，则关闭对话框
  if Assigned(Dialog) and (Dialog.Cancelable) and (Key in [vkEscape, vkHardwareBack]) then
    Dialog.Cancel;
  Key := 0;
end;

constructor TDialogView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDialogView.Destroy;
begin
  FLayBubble := nil;
  FTitleView := nil;
  FMsgBody := nil;
  FMsgMessage := nil;
  FButtonLayout := nil;
  FButtonPositive := nil;
  FButtonNegative := nil;
  FButtonNeutral := nil;
  FListView := nil;
  FAnilndictor := nil;
  inherited Destroy;
end;

procedure TDialogView.Hide;
begin
  Visible := False;
end;

procedure TDialogView.InitButton(StyleMgr: TDialogStyleManager);

  procedure SetButtonColor(Button: TButtonView; State: TViewState);
  var
    AColor: TAlphaColor;
    ABrush: TBrush;
  begin
    AColor := StyleMgr.FButtonColor.GetColor(State);
    ABrush := Button.Background.GetBrush(State, False);
    if (AColor = TAlphaColorRec.Null) and (ABrush = nil) then
      Exit;
    if ABrush = nil then
      ABrush := Button.Background.GetBrush(State, True);
    ABrush.Color := AColor;
    ABrush.Kind := TBrushKind.Solid;
  end;

  function CreateButton: TButtonView;
  begin
    Result := TButtonView.Create(Owner);
    Result.Parent := FButtonLayout;
    Result.Weight := 1;
    Result.MinHeight := 42;
    Result.Gravity := TLayoutGravity.Center;
    Result.Paddings := '4';
    Result.CanFocus := True;
    Result.Clickable := True;
    Result.TextSettings.Font.Size := StyleMgr.ButtonTextSize;
    Result.TextSettings.Color.Assign(StyleMgr.ButtonTextColor);
    Result.Background.Corners := [];
    Result.Background.XRadius := StyleMgr.FBackgroundRadius;
    Result.Background.YRadius := StyleMgr.FBackgroundRadius;

    SetButtonColor(Result, TViewState.None);
    SetButtonColor(Result, TViewState.Pressed);
    SetButtonColor(Result, TViewState.Focused);
    SetButtonColor(Result, TViewState.Hovered);
    SetButtonColor(Result, TViewState.Selected);
    SetButtonColor(Result, TViewState.Checked);
    SetButtonColor(Result, TViewState.Enabled);

    TDrawableBorder(Result.Background).Border.Assign(StyleMgr.FButtonBorder);
  end;

begin
  // 按钮布局层
  FButtonLayout := TLinearLayout.Create(Owner);
  {$IFDEF MSWINDOWS}
  FButtonLayout.Name := 'ButtonLayout' + IntToStr(DialogRef);
  {$ENDIF}
  FButtonLayout.Parent := FLayBubble;
  FButtonLayout.WidthSize := TViewSize.FillParent;
  FButtonLayout.Orientation := TOrientation.Horizontal;
  FButtonLayout.Margins.Top := 1;
  FButtonLayout.HeightSize := TViewSize.WrapContent;
  // 按钮
  FButtonPositive := CreateButton();
  FButtonPositive.Default := True;
  FButtonNegative := CreateButton();
  FButtonNeutral := CreateButton();
end;

procedure TDialogView.InitList(StyleMgr: TDialogStyleManager);
begin
  // 列表
  FListView := TListViewEx.Create(Owner);
  {$IFDEF MSWINDOWS}
  FListView.Name := 'FListView' + IntToStr(DialogRef);
  {$ENDIF}
  FListView.Parent := FMsgBody;
  FListView.HitTest := True;
  FListView.CanFocus := True;
  //FListView.ControlType := TControlType.Platform;
  FListView.WidthSize := TViewSize.FillParent;
  FListView.HeightSize := TViewSize.WrapContent;
end;

procedure TDialogView.InitMessage(StyleMgr: TDialogStyleManager);
begin
  if FMsgMessage <> nil then Exit;  
  // 内容区
  FMsgMessage := TTextView.Create(Owner);
  {$IFDEF MSWINDOWS}
  FMsgMessage.Name := 'FMsgMessage' + IntToStr(DialogRef);
  {$ENDIF}
  FMsgMessage.Parent := FMsgBody;
  FMsgMessage.Clickable := False;
  FMsgMessage.WidthSize := TViewSize.FillParent;
  FMsgMessage.HeightSize := TViewSize.WrapContent;
  FMsgMessage.Padding.Rect := RectF(8, 8, 8, 12);
  FMsgMessage.Gravity := TLayoutGravity.CenterVertical;
  FMsgMessage.TextSettings.WordWrap := True;
  FMsgMessage.TextSettings.Color.Default := StyleMgr.MessageTextColor;
  FMsgMessage.TextSettings.Font.Size := StyleMgr.MessageTextSize;
  FMsgMessage.AutoSize := True;
  FMsgMessage.ScrollBars := TViewScroll.Vertical;
  FMsgMessage.Drawable.SizeWidth := StyleMgr.IconSize;
  FMsgMessage.Drawable.SizeHeight := StyleMgr.IconSize;
  FMsgMessage.Drawable.Padding := 8;
  FMsgMessage.Background.ItemDefault.Color := StyleMgr.MessageTextBackground;
  FMsgMessage.Background.ItemDefault.Kind := TViewBrushKind.Solid;
end;

procedure TDialogView.InitOK;
begin
  EndUpdate;
  if Assigned(FAnilndictor) then
    FAnilndictor.Enabled := True;
  if Assigned(FLayBubble) then
    FLayBubble.RecalcSize;
  HandleSizeChanged;
end;

procedure TDialogView.InitProcessView(StyleMgr: TDialogStyleManager);
begin
  CanFocus := False;
  FLayBubble := TLinearLayout.Create(Owner);
  {$IFDEF MSWINDOWS}
  FLayBubble.Name := 'LayBubble' + IntToStr(DialogRef);
  {$ENDIF}
  // 消息框主体
  FLayBubble.Parent := Self;
  FLayBubble.Margin := '16';
  FLayBubble.Paddings := '16';
  FLayBubble.ClipChildren := True;
  FLayBubble.Background.ItemDefault.Color := StyleMgr.ProcessBackgroundColor;
  FLayBubble.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FLayBubble.Background.XRadius := StyleMgr.FBackgroundRadius;
  FLayBubble.Background.YRadius := StyleMgr.FBackgroundRadius;
  FLayBubble.Gravity := TLayoutGravity.Center;
  FLayBubble.Layout.CenterInParent := True;
  FLayBubble.Clickable := True;
  FLayBubble.WidthSize := TViewSize.WrapContent;
  FLayBubble.HeightSize := TViewSize.WrapContent;
  FLayBubble.Orientation := TOrientation.Vertical;
  FLayBubble.CanFocus := False;
  FLayBubble.AdjustViewBounds := True;
  FLayBubble.MaxWidth := Width - FLayBubble.Margins.Left - FLayBubble.Margins.Right;
  FLayBubble.MaxHeight := Height - FLayBubble.Margins.Top - FLayBubble.Margins.Bottom;

  // 等待动画
  FAnilndictor := TAniIndicator.Create(Owner);
  {$IFDEF MSWINDOWS}
  FAnilndictor.Name := 'Anilndictor' + IntToStr(DialogRef);
  {$ENDIF}
  FAnilndictor.Parent := FLayBubble;
  FAnilndictor.Align := TAlignLayout.Center;
  // 消息内容
  FMsgMessage := TTextView.Create(Owner);
  {$IFDEF MSWINDOWS}
  FMsgMessage.Name := 'FMsgMessage' + IntToStr(DialogRef);
  {$ENDIF}
  FMsgMessage.Parent := FLayBubble;
  FMsgMessage.Clickable := False;
  FMsgMessage.Margins.Top := 24;
  FMsgMessage.Padding.Left := 24;
  FMsgMessage.Padding.Right := 24;
  FMsgMessage.WidthSize := TViewSize.WrapContent;
  FMsgMessage.HeightSize := TViewSize.WrapContent;
  FMsgMessage.Gravity := TLayoutGravity.Center;
  FMsgMessage.TextSettings.WordWrap := True;
  FMsgMessage.TextSettings.Color.Default := StyleMgr.ProcessTextColor;
  FMsgMessage.TextSettings.Font.Size := StyleMgr.MessageTextSize;
  FMsgMessage.AutoSize := True;
end;

procedure TDialogView.InitView(StyleMgr: TDialogStyleManager);
begin
  CanFocus := False;
  FLayBubble := TLinearLayout.Create(Owner);
  {$IFDEF MSWINDOWS}
  FLayBubble.Name := 'LayBubble' + IntToStr(DialogRef);
  {$ENDIF}
  // 消息框主体
  FLayBubble.Parent := Self;
  FLayBubble.Margin := '16';
  FLayBubble.ClipChildren := True;
  if FDialog.Builder.FUseRootBackColor then
    FLayBubble.Background.ItemDefault.Color := FDialog.Builder.FRootBackColor
  else begin
    FLayBubble.Background.ItemDefault.Color := StyleMgr.BackgroundColor;
    FLayBubble.Background.XRadius := StyleMgr.FBackgroundRadius;
    FLayBubble.Background.YRadius := StyleMgr.FBackgroundRadius;
  end;
  FLayBubble.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FLayBubble.Layout.CenterInParent := True;
  FLayBubble.Clickable := True;
  FLayBubble.WidthSize := TViewSize.FillParent;
  FLayBubble.HeightSize := TViewSize.WrapContent;
  FLayBubble.Orientation := TOrientation.Vertical;
  FLayBubble.CanFocus := False;
  FLayBubble.AdjustViewBounds := True;
  FLayBubble.MaxHeight := Height - FLayBubble.Margins.Top - FLayBubble.Margins.Bottom;
  // 标题栏
  FTitleView := TTextView.Create(Owner);
  {$IFDEF MSWINDOWS}
  FTitleView.Name := 'TitleView' + IntToStr(DialogRef);
  {$ENDIF}
  FTitleView.Parent := FLayBubble;
  FTitleView.ClipChildren := True;
  FTitleView.TextSettings.Font.Size := StyleMgr.TitleTextSize;
  FTitleView.TextSettings.Color.Default := StyleMgr.TitleTextColor;
  FTitleView.Gravity := StyleMgr.TitleGravity;
  FTitleView.Padding.Rect := RectF(8, 4, 8, 4);
  FTitleView.MinHeight := StyleMgr.TitleHeight;
  FTitleView.WidthSize := TViewSize.FillParent;
  FTitleView.Background.ItemDefault.Color := StyleMgr.TitleBackGroundColor;
  FTitleView.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FTitleView.Background.XRadius := StyleMgr.FBackgroundRadius;
  FTitleView.Background.YRadius := StyleMgr.FBackgroundRadius;
  FTitleView.Background.Corners := [TCorner.TopLeft, TCorner.TopRight];
  FTitleView.Background.Padding.Rect := RectF(1, 1, 1, 0);
  FTitleView.HeightSize := TViewSize.WrapContent;
  // 内容区
  FMsgBody := TLinearLayout.Create(Owner);
  {$IFDEF MSWINDOWS}
  FMsgBody.Name := 'MsgBody' + IntToStr(DialogRef);
  {$ENDIF}
  FMsgBody.Parent := FLayBubble;
  FMsgBody.ClipChildren := True;
  FMsgBody.Weight := 1;
  FMsgBody.MinHeight := 52;
  FMsgBody.WidthSize := TViewSize.FillParent;
  FMsgBody.HeightSize := TViewSize.WrapContent;
  FMsgBody.Orientation := TOrientation.Vertical;
  if FDialog.Builder.FUseRootBackColor then
    FMsgBody.Background.ItemDefault.Color := FDialog.Builder.FRootBackColor
  else
    FMsgBody.Background.ItemDefault.Color := StyleMgr.BodyBackGroundColor;
  FMsgBody.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FMsgBody.Margins.Rect := RectF(0, 1, 0, 0);
end;

procedure TDialogView.SetTitle(const AText: string);
begin
  if FTitleView <> nil then begin
    FTitleView.Text := AText;
    if AText = '' then
      FTitleView.Visible := False;
  end;
end;

procedure TDialogView.Show;
begin
  Visible := True;
  BringToFront;
  Resize;
  if Assigned(FListView) then
    Width := Width + 0.01;
end;

{ TDialogStyleManager }

constructor TDialogStyleManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDialogMaskColor := COLOR_DialogMaskColor;
  FTitleBackGroundColor := COLOR_TitleBackGroundColor;
  FTitleTextColor := COLOR_TitleTextColor;
  FProcessBackgroundColor := COLOR_ProcessBackgroundColor;
  FProcessTextColor := COLOR_ProcessTextColor;
  FBackgroundColor := COLOR_BackgroundColor;
  FBodyBackgroundColor := COLOR_BodyBackgroundColor;
  FMessageTextBackground := COLOR_MessageTextBackground;
  FMessageTextColor := COLOR_MessageTextColor;
  FMessageTextSize := FONT_MessageTextSize;
  FTitleTextSize := FONT_TitleTextSize;
  FButtonTextSize := FONT_ButtonTextSize;
  FIconSize := SIZE_ICON;
  FTitleHeight := SIZE_TitleHeight;
  FTitleGravity := Title_Gravity;
  FBackgroundRadius := SIZE_BackgroundRadius;

  FButtonColor := TViewColor.Create();
  {$IFDEF IOS}
  FButtonColor.Default := COLOR_ButtonColor;
  FButtonColor.Pressed := COLOR_ButtonPressColor;
  {$ELSE}
  FButtonColor.Default := COLOR_ButtonColor;
  FButtonColor.Pressed := COLOR_ButtonPressColor;
  {$ENDIF}

  FButtonBorder := TViewBorder.Create;
  FButtonBorder.Width := SIZE_ButtonBorder;
  FButtonBorder.Style := TViewBorderStyle.RectBorder;
  with FButtonBorder do begin
    Color.Default := $AFCCCCCC;
    Color.Pressed := $FFb0b0b0;
    Color.Hovered := $EFc0c0c0;
  end;

  FButtonTextColor := TTextColor.Create(COLOR_ButtonTextColor);
  FButtonTextColor.Pressed := COLOR_ButtonTextPressColor;

  if Assigned(Owner) and (not (csDesigning in ComponentState)) then begin
    if DefaultStyleManager <> nil then begin
      DefaultStyleManager.DisposeOf;
      DefaultStyleManager := nil;
    end;
    DefaultStyleManager := Self;
  end;
end;

destructor TDialogStyleManager.Destroy;
begin
  if (DefaultStyleManager = Self) and (not (csDesigning in ComponentState)) then
    DefaultStyleManager := nil;
  FreeAndNil(FButtonColor);
  FreeAndNil(FButtonBorder);
  FreeAndNil(FButtonTextColor);
  inherited;
end;

procedure TDialogStyleManager.SetButtonBorder(const Value: TViewBorder);
begin
  FButtonBorder.Assign(Value);
end;

procedure TDialogStyleManager.SetButtonColor(const Value: TViewColor);
begin
  FButtonColor.Assign(Value);
end;

procedure TDialogStyleManager.SetButtonTextColor(const Value: TTextColor);
begin
  FButtonTextColor.Assign(Value);
end;


{ TProgressDialog }

constructor TProgressDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TProgressDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TProgressDialog.DoRootClick(Sender: TObject);
begin
end;

function TProgressDialog.GetMessage: string;
begin
  if Assigned(FViewRoot) and (Assigned(FViewRoot.FMsgMessage)) then
    Result := FViewRoot.FMsgMessage.Text
  else
    Result := '';
end;

procedure TProgressDialog.InitView(const AMsg: string);
var
  Style: TDialogStyleManager;
begin
  Inc(DialogRef);
  Style := FStyleManager;
  if Style = nil then
    Style := GetDefaultStyleMgr;

  // 初始化基础     
  FViewRoot := TDialogView.Create(Owner);
  FViewRoot.Dialog := Self;
  FViewRoot.BeginUpdate;
  FViewRoot.OnClick := DoRootClick;
  FViewRoot.Parent := GetFirstParent;
  if FViewRoot.Parent = nil then begin
    Dismiss;
    Exit;
  end;
  FViewRoot.Clickable := True;
  FViewRoot.Align := TAlignLayout.Client;
  FViewRoot.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FViewRoot.InitProcessView(Style);
  if AMsg = '' then
    FViewRoot.FMsgMessage.Visible := False
  else begin
    FViewRoot.FMsgMessage.Text := AMsg;
    FViewRoot.FMsgMessage.Visible := True;
  end;

  SetBackColor(Style.FDialogMaskColor);
  InitOK();
end;

procedure TProgressDialog.SetMessage(const Value: string);
begin
  if Assigned(FViewRoot) and (Assigned(FViewRoot.FMsgMessage)) then begin
    FViewRoot.FMsgMessage.Text := Value;
    FViewRoot.FMsgMessage.Visible := Value <> '';
    FViewRoot.Realign;
  end;
end;

class function TProgressDialog.Show(AOwner: TComponent; const AMsg: string;
  ACancelable: Boolean): TProgressDialog;
begin
  Result := TProgressDialog.Create(AOwner);
  Result.Cancelable := ACancelable;
  Result.InitView(AMsg);
  TDialog(Result).Show();
end;

initialization

finalization
  FreeAndNil(DefaultStyleManager);

end.
