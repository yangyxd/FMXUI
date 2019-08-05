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
  UI.Base, UI.Ani, UI.Utils, UI.Standard, UI.ListView,
  {$IFDEF WINDOWS}UI.Debug, {$ENDIF}
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
  // The identifier for the cancel button.
  BUTTON_CANCEL = -4;

const
  // 颜色、字体等默认设置项
  COLOR_BodyBackgroundColor = $ffffffff;
  COLOR_BackgroundColor = $ffffffff;
  COLOR_DialogMaskColor = $9f000000;
  COLOR_MessageTextBackground = $00f00000;
  COLOR_MessageTextColor = $ff101010;
  COLOR_TitleBackGroundColor = $00000000;
  {$IFDEF IOS}
  //COLOR_TitleTextColor = $ff077dfe;
  COLOR_TitleTextColor = $ff7D7e7f;
  {$ELSE}
  COLOR_TitleTextColor = $ff7D7D7D;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  COLOR_ProcessBackgroundColor = $7fbababa;
  {$ENDIF}
  {$IFDEF IOS}
  COLOR_ProcessBackgroundColor = $80000000;
  {$ELSE}
  {$IFDEF MACOS}
  COLOR_ProcessBackgroundColor = $80000000;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  COLOR_ProcessBackgroundColor = $7f000000;
  {$ENDIF}
  COLOR_ProcessTextColor = $fff7f7f7;

  COLOR_ButtonColor = $ffffffff;
  COLOR_ButtonPressColor = $ffd9d9d9;
  COLOR_ButtonBorderColor = $ffe0e0e0;
  {$IFDEF IOS}
//  COLOR_ButtonTextColor = $FF077dfe;
//  COLOR_ButtonTextPressColor = $FF0049f5;
  COLOR_ButtonTextColor = $FF404040;
  COLOR_ButtonTextPressColor = $FF101010;
  {$ELSE}
  COLOR_ButtonTextColor = $FF404040;
  COLOR_ButtonTextPressColor = $FF101010;
  {$ENDIF}

  COLOR_ListItemPressedColor = $ffd9d9d9;
  COLOR_LIstItemDividerColor = $afe3e4e5;

  COLOR_TitleSpaceColor = $ffe7e7e7;
  SIZE_TitleSpaceHeight = 1;

  FONT_TitleTextSize = 18;
  FONT_MessageTextSize = 15;
  FONT_ButtonTextSize = 15;

  Title_Gravity = TLayoutGravity.Center;

  {$IFDEF IOS}
  SIZE_BackgroundRadius = 15;
  SIZE_TitleHeight = 42;
  {$ELSE}
  SIZE_BackgroundRadius = 13;
  SIZE_TitleHeight = 50;
  {$ENDIF}
  SIZE_ButtonHeight = 42;
  SIZE_ICON = 32;
  SIZE_ButtonBorder = 0.6;

  SIZE_MENU_WIDTH = 0.8;

type
  TButtonViewColor = class(TViewColor)
  public
    constructor Create(const ADefaultColor: TAlphaColor = COLOR_ButtonColor);
  published
    property Default default COLOR_ButtonColor;
    property Pressed default COLOR_ButtonPressColor;
  end;

type
  /// <summary>
  /// 对话框样式管理器
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
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
    FButtonColor: TButtonViewColor;
    FButtonTextColor: TTextColor;
    FButtonBorder: TViewBorder;
    FMessageTextSize: Integer;
    FTitleHeight: Integer;
    FTitleTextSize: Integer;
    FButtonTextSize: Integer;
    FIconSize: Integer;
    FBackgroundRadius: Single;
    FTitleGravity: TLayoutGravity;
    FTitleSpaceHeight: Single;
    FTitleSpaceColor: TAlphaColor;
    FMaxWidth: Integer;
    FMessageTextMargins: TBounds;
    FMessageTextGravity: TLayoutGravity;
    FTitleTextBold: Boolean;

    FListItemPressedColor: TAlphaColor;
    FListItemDividerColor: TAlphaColor;
    FButtonHeight: Integer;

    procedure SetButtonColor(const Value: TButtonViewColor);
    procedure SetButtonBorder(const Value: TViewBorder);
    procedure SetButtonTextColor(const Value: TTextColor);
    function IsStoredBackgroundRadius: Boolean;
    function IsStoredTitleSpaceHeight: Boolean;
    function GetMessageTextMargins: TBounds;
    procedure SetMessageTextMargins(const Value: TBounds);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // 遮罩层颜色
    property DialogMaskColor: TAlphaColor read FDialogMaskColor write FDialogMaskColor default COLOR_DialogMaskColor;
    // 消息框背景颜色
    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor default COLOR_BackgroundColor;
    // 标题栏背景色
    property TitleBackGroundColor: TAlphaColor read FTitleBackGroundColor write FTitleBackGroundColor default COLOR_TitleBackGroundColor;
    // 标题栏文本颜色
    property TitleTextColor: TAlphaColor read FTitleTextColor write FTitleTextColor default COLOR_TitleTextColor;
    // 主体区背景颜色
    property BodyBackgroundColor: TAlphaColor read FBodyBackgroundColor write FBodyBackgroundColor default COLOR_BodyBackgroundColor;

    // 消息文本颜色
    property MessageTextColor: TAlphaColor read FMessageTextColor write FMessageTextColor default COLOR_MessageTextColor;
    // 消息文本背景颜色
    property MessageTextBackground: TAlphaColor read FMessageTextBackground write FMessageTextBackground default COLOR_MessageTextBackground;
    // 消息文本外边距
    property MessageTextMargins: TBounds  read GetMessageTextMargins write SetMessageTextMargins;
    // 消息文本重力
    property MessageTextGravity: TLayoutGravity read FMessageTextGravity write FMessageTextGravity default TLayoutGravity.CenterVertical;

    // 等待消息框背景颜色
    property ProcessBackgroundColor: TAlphaColor read FProcessBackgroundColor write FProcessBackgroundColor default COLOR_ProcessBackgroundColor;
    // 等待消息框消息文字颜色
    property ProcessTextColor: TAlphaColor read FProcessTextColor write FProcessTextColor default COLOR_ProcessTextColor;

    // 列表框默认列表项按下时背景颜色
    property ListItemPressedColor: TAlphaColor read FListItemPressedColor write FListItemPressedColor default COLOR_ListItemPressedColor;
    // 列表框默认行列分隔线颜色
    property ListItemDividerColor: TAlphaColor read FListItemDividerColor write FListItemDividerColor default COLOR_ListItemDividerColor;

    // 标题栏文本重力
    property TitleGravity: TLayoutGravity read FTitleGravity write FTitleGravity default Title_Gravity;
    // 标题栏高度
    property TitleHeight: Integer read FTitleHeight write FTitleHeight default SIZE_TitleHeight;
    // 标题栏粗体
    property TitleTextBold: Boolean read FTitleTextBold write FTitleTextBold default False;
    // 标题文本大小
    property TitleTextSize: Integer read FTitleTextSize write FTitleTextSize default FONT_TitleTextSize;
    // 消息文本大小
    property MessageTextSize: Integer read FMessageTextSize write FMessageTextSize default FONT_MessageTextSize;
    // 按钮文本大小
    property ButtonTextSize: Integer read FButtonTextSize write FButtonTextSize default FONT_ButtonTextSize;
    // 按钮高度
    property ButtonHeight: Integer read FButtonHeight write FButtonHeight default SIZE_ButtonHeight;
    // 图标大小
    property IconSize: Integer read FIconSize write FIconSize default SIZE_ICON;
    // 最大宽度
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 0;

    property BackgroundRadius: Single read FBackgroundRadius write FBackgroundRadius stored IsStoredBackgroundRadius;
    property ButtonColor: TButtonViewColor read FButtonColor write SetButtonColor;
    property ButtonTextColor: TTextColor read FButtonTextColor write SetButtonTextColor;
    property ButtonBorder: TViewBorder read FButtonBorder write SetButtonBorder;

    // 标题与内容区分隔线颜色
    property TitleSpaceColor: TAlphaColor read FTitleSpaceColor write FTitleSpaceColor default COLOR_TitleSpaceColor;
    // 标题与内容区分隔线高度
    property TitleSpaceHeight: Single read FTitleSpaceHeight write FTitleSpaceHeight stored IsStoredTitleSpaceHeight;
  end;

type
  TDialogBuilder = class;
  TCustomAlertDialog = class;
  TDialogView = class;

  /// <summary>
  /// 对话框接口
  /// </summary>
  IDialog = interface(IInterface)
    ['{53E2915A-B90C-4C9B-85D8-F4E3B9892D9A}']
    function GetBuilder: TDialogBuilder;
    function GetView: TControl;
    function GetViewRoot: TDialogView;
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
    /// 根视图组件
    /// </summary>
    property ViewRoot: TDialogView read GetViewRoot;
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
  TOnDialogInitListAdapterA = reference to procedure (Dialog: IDialog; Builder: TDialogBuilder; var Adapter: IListAdapter);
  TOnDialogInitA = reference to procedure (Dialog: IDialog; Builder: TDialogBuilder);

  /// <summary>
  /// 对话框视图 (不要直接使用它)
  /// </summary>
  TDialogView = class(TRelativeLayout)
  private
    [Weak] FDialog: IDialog;
  protected
    FLayBubble: TLinearLayout;
    FLayBubbleBottom: TLinearLayout;
    FTitleView: TTextView;
    FTitleSpace: TView;
    FMsgBody: TLinearLayout;
    FMsgMessage: TTextView;
    FButtonLayout: TLinearLayout;
    FButtonPositive: TButtonView;
    FButtonNegative: TButtonView;
    FButtonNeutral: TButtonView;
    FCancelButtonLayout: TLinearLayout;
    FButtonCancel: TButtonView;
    FListView: TListViewEx;
    FAnilndictor: TAniIndicator;
    FIsDownPopup: Boolean;
  protected
    procedure AfterDialogKey(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    procedure DoRealign; override;
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
    property CancelButtonLayout: TLinearLayout read FCancelButtonLayout;
    property ButtonCancel: TButtonView read FButtonCancel;

    property Dialog: IDialog read FDialog write FDialog;
  end;

  TControlClass = type of TControl;
  TDialogViewPosition = (Top, Bottom, LeftBottom, RightBottom, Left, Right, Center, LeftFill, RightFill);

  TDialog = class(TComponent, IDialog)
  private
    FAnimate: TFrameAniType;
    FMask: Boolean;
    FOnCancelListener: TOnDialogListener;
    FOnCancelListenerA: TOnDialogListenerA;
    FOnShowListener: TOnDialogListener;
    FOnShowListenerA: TOnDialogListenerA;
    FOnDismissListener: TOnDialogListener;
    FOnDismissListenerA: TOnDialogListenerA;
    procedure SetOnCancelListener(const Value: TOnDialogListener);
    procedure SetOnCancelListenerA(const Value: TOnDialogListenerA);
    function GetView: TControl;
    function GetViewRoot: TDialogView;
    function GetRootView: TDialogView;
    function GetIsDismiss: Boolean;
    function GetAniView: TControl;
  protected
    FViewRoot: TDialogView;

    FCancelable: Boolean;
    FCanceled: Boolean;
    FIsDismiss: Boolean;

    FEventing: Boolean;      // 事件处理中
    FAllowDismiss: Boolean;  // 需要释放

    FTempValue: Single;      // 临时变量

    FIsDowPopup: Boolean;    // 是否是下拉弹出方式

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
    procedure DoAsyncDismiss();

    /// <summary>
    /// 播放动画
    /// <param name="Ani">动画类型</param>
    /// <param name="IsIn">是否是正要显示</param>
    /// <param name="AEvent">动画播放完成事件</param>
    /// </summary>
    procedure AnimatePlay(Ani: TFrameAniType; IsIn: Boolean; AEvent: TNotifyEventA);

    property AniView: TControl read GetAniView;
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
      Cancelable: Boolean = True; Ani: TFrameAniType = TFrameAniType.None; Mask: Boolean = True): TDialog; overload;
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
      Cancelable: Boolean = True; Ani: TFrameAniType = TFrameAniType.None; Mask: Boolean = True): TDialog; overload;

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
    property Animate: TFrameAniType read FAnimate write FAnimate default TFrameAniType.FadeInOut;

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

    procedure InitDownPopupView();
    procedure AdjustDownPopupPosition();
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
    FMessageIsHtml: Boolean;
    FCancelable: Boolean;
    FIsMaxWidth: Boolean;
    FIsSingleChoice: Boolean;
    FIsMultiChoice: Boolean;
    FItemSingleLine: Boolean;
    FClickButtonDismiss: Boolean;
    FMaskVisible: Boolean;
    FCheckedItem: Integer;
    FTag: Integer;

    FWidth: Single;
    FMaxHeight: Single;
    FListItemDefaultHeight: Single;
    FPosition: TDialogViewPosition;

    [Weak] FTarget: TControl;
    FTargetOffsetX, FTargetOffsetY: Single;
    FTargetGravity: TLayoutGravity;
    FWordWrap: Boolean;

    FCheckedItems: TArray<Boolean>;

    FPositiveButtonText: string;
    FPositiveButtonListener: TOnDialogClickListener;
    FPositiveButtonListenerA: TOnDialogClickListenerA;
    FPositiveButtonSize: Single;
    FPositiveButtonColor: Int64;
    FPositiveButtonStyle: TFontStyles;

    FNegativeButtonText: string;
    FNegativeButtonListener: TOnDialogClickListener;
    FNegativeButtonListenerA: TOnDialogClickListenerA;
    FNegativeButtonSize: Single;
    FNegativeButtonColor: Int64;
    FNegativeButtonStyle: TFontStyles;

    FNeutralButtonText: string;
    FNeutralButtonListener: TOnDialogClickListener;
    FNeutralButtonListenerA: TOnDialogClickListenerA;
    FNeutralButtonSize: Single;
    FNeutralButtonColor: Int64;
    FNeutralButtonStyle: TFontStyles;

    FCancelButtonText: string;
    FCancelButtonListener: TOnDialogClickListener;
    FCancelButtonListenerA: TOnDialogClickListenerA;
    FCancelButtonSize: Single;
    FCancelButtonColor: Int64;
    FCancelButtonStyle: TFontStyles;

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

    FOnInitListAdapterA: TOnDialogInitListAdapterA;
    FOnInitA: TOnDialogInitA;

    function GetCheckedCount: Integer;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;

    function CreateDialog(): IDialog;
    function Show(): IDialog; overload;
    function Show(OnDismissListener: TOnDialogListener): IDialog; overload;
    function Show(OnDismissListener: TOnDialogListenerA): IDialog; overload;

    /// <summary>
    /// 设置Dialog初始化事件
    /// </summary>
    function SetOnInitA(AListener: TOnDialogInitA): TDialogBuilder;

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
    function SetMessage(const AMessage: string; IsHtmlText: Boolean = False): TDialogBuilder;
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
    /// 设置将对话框作为下拉弹出菜单时的标靶和偏移 （Target非空时，对话框以下拉弹出菜单样式显示）
    /// </summary>
    function SetDownPopup(ATarget: TControl; const XOffset, YOffset: Single;
      Gravity: TLayoutGravity = TLayoutGravity.LeftBottom;
      MaskVisible: Boolean = False): TDialogBuilder;

    /// <summary>
    /// 设置位置
    /// </summary>
    function SetPosition(APosition: TDialogViewPosition): TDialogBuilder;

    /// <summary>
    /// 设置是否自动换行（列表项）
    /// </summary>
    function SetWordWrap(V: Boolean): TDialogBuilder;

    /// <summary>
    /// 设置确认按钮
    /// </summary>
    function SetPositiveButton(const AText: string; AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetPositiveButton(const AText: string; AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    function SetPositiveButtonStyle(const AColor: Int64; const AStyle: TFontStyles = []; const ASize: Single = -1): TDialogBuilder; overload;
    /// <summary>
    /// 设置否定按钮
    /// </summary>
    function SetNegativeButton(const AText: string; AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetNegativeButton(const AText: string; AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    function SetNegativeButtonStyle(const AColor: Int64; const AStyle: TFontStyles = []; const ASize: Single = -1): TDialogBuilder; overload;
    /// <summary>
    /// 设置中间按钮
    /// </summary>
    function SetNeutralButton(const AText: string; AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetNeutralButton(const AText: string; AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    function SetNeutralButtonStyle(const AColor: Int64; const AStyle: TFontStyles = []; const ASize: Single = -1): TDialogBuilder; overload;
    /// <summary>
    /// 设置底部取消按钮
    /// </summary>
    function SetCancelButton(const AText: string; AListener: TOnDialogClickListener = nil): TDialogBuilder; overload;
    function SetCancelButton(const AText: string; AListener: TOnDialogClickListenerA): TDialogBuilder; overload;
    function SetCancelButtonStyle(const AColor: Int64; const AStyle: TFontStyles = []; const ASize: Single = -1): TDialogBuilder; overload;

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
    /// 设置自定义列表数据适配器
    /// </summary>
    function SetOnInitListAdapterA(AListener: TOnDialogInitListAdapterA): TDialogBuilder;

    /// <summary>
    /// 设置 Mask 层是否可视
    /// </summary>
    function SetMaskVisible(V: Boolean): TDialogBuilder;

    /// <summary>
    /// 设置 对话框 Root 层背景颜色
    /// </summary>
    function SetRootBackColor(const V: TAlphaColor): TDialogBuilder;

    /// <summary>
    /// 设置宽度
    /// </summary>
    function SetWidth(const V: Single): TDialogBuilder;

    /// <summary>
    /// 设置最大高度
    /// </summary>
    function SetMaxHeight(const V: Single): TDialogBuilder;

    /// <summary>
    /// 设置列表项对话框默认行高
    /// </summary>
    function SetListItemDefaultHeight(const V: Single): TDialogBuilder;

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
    property MessageIsHtml: Boolean read FMessageIsHtml;
    property Cancelable: Boolean read FCancelable;
    property IsMaxWidth: Boolean read FIsMaxWidth;
    property IsSingleChoice: Boolean read FIsSingleChoice;
    property IsMultiChoice: Boolean read FIsMultiChoice;
    property ItemSingleLine: Boolean read FItemSingleLine;
    property MaskVisible: Boolean read FMaskVisible write FMaskVisible;
    property RootBackColor: TAlphaColor read FRootBackColor write FRootBackColor;
    property ClickButtonDismiss: Boolean read FClickButtonDismiss write FClickButtonDismiss;
    property CheckedItem: Integer read FCheckedItem;
    property CheckedItems: TArray<Boolean> read FCheckedItems;
    property CheckedCount: Integer read GetCheckedCount;

    property Target: TControl read FTarget;

    property DataObject: TObject read FDataObject write FDataObject;
    property Data: TValue read FData write FData;
    property Tag: Integer read FTag write FTag;

    property PositiveButtonText: string read FPositiveButtonText;
    property PositiveButtonListener: TOnDialogClickListener read FPositiveButtonListener;
    property PositiveButtonListenerA: TOnDialogClickListenerA read FPositiveButtonListenerA;
    property PositiveButtonSize: Single read FPositiveButtonSize;
    property PositiveButtonColor: Int64 read FPositiveButtonColor;
    property PositiveButtonStyle: TFontStyles read FPositiveButtonStyle;

    property NegativeButtonText: string read FNegativeButtonText;
    property NegativeButtonListener: TOnDialogClickListener read FNegativeButtonListener;
    property NegativeButtonListenerA: TOnDialogClickListenerA read FNegativeButtonListenerA;
    property NegativeButtonSize: Single read FNegativeButtonSize;
    property NegativeButtonColor: Int64 read FNegativeButtonColor;
    property NegativeButtonStyle: TFontStyles read FNegativeButtonStyle;

    property NeutralButtonText: string read FNeutralButtonText;
    property NeutralButtonListener: TOnDialogClickListener read FNeutralButtonListener;
    property NeutralButtonListenerA: TOnDialogClickListenerA read FNeutralButtonListenerA;
    property NeutralButtonSize: Single read FNeutralButtonSize;
    property NeutralButtonColor: Int64 read FNeutralButtonColor;
    property NeutralButtonStyle: TFontStyles read FNeutralButtonStyle;

    property CancelButtonText: string read FCancelButtonText;
    property CancelButtonListener: TOnDialogClickListener read FCancelButtonListener;
    property CancelButtonListenerA: TOnDialogClickListenerA read FCancelButtonListenerA;
    property CancelButtonSize: Single read FCancelButtonSize;
    property CancelButtonColor: Int64 read FCancelButtonColor;
    property CancelButtonStyle: TFontStyles read FCancelButtonStyle;

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
    procedure InitView(const AMsg: string; IsHtmlText: Boolean = False);
    /// <summary>
    /// 显示一个等待对话框
    /// </summary>
    class function Show(AOwner: TComponent; const AMsg: string; ACancelable: Boolean = True): TProgressDialog;
  published
    property StyleManager: TDialogStyleManager read FStyleManager write FStyleManager;
  end;

// 默认对话框样式
function GetDefaultStyleMgr: TDialogStyleManager;

implementation

uses
  UI.Frame;

var
  DefaultStyleManager: TDialogStyleManager = nil;
  DialogRef: Integer = 0;

type
  TFrameViewTmp = class(TFrameView);

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
  FWordWrap := True;
  FPosition := TDialogViewPosition.Center;

  FPositiveButtonColor := -1;
  FNegativeButtonColor := -1;
  FNeutralButtonColor := -1;
  FCancelButtonColor := -1;
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
    if Assigned(FOnInitA) then
      FOnInitA(Dlg, Self);
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

function TDialogBuilder.SetCancelButton(const AText: string;
  AListener: TOnDialogClickListener): TDialogBuilder;
begin
  Result := Self;
  FCancelButtonText := AText;
  FCancelButtonListener := AListener;
end;

function TDialogBuilder.SetCancelButton(const AText: string;
  AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FCancelButtonText := AText;
  FCancelButtonListenerA := AListener;
end;

function TDialogBuilder.SetCancelButtonStyle(const AColor: Int64;
  const AStyle: TFontStyles; const ASize: Single): TDialogBuilder;
begin
  Result := Self;
  FCancelButtonSize := ASize;
  FCancelButtonColor := AColor;
  FCancelButtonStyle := AStyle;
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

function TDialogBuilder.SetDownPopup(ATarget: TControl; const XOffset,
  YOffset: Single; Gravity: TLayoutGravity; MaskVisible: Boolean): TDialogBuilder;
begin
  Result := Self;
  FTarget := ATarget;
  FTargetOffsetX := XOffset;
  FTargetOffsetY := YOffset;
  FTargetGravity := Gravity;
  FMaskVisible := MaskVisible;
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

function TDialogBuilder.SetListItemDefaultHeight(
  const V: Single): TDialogBuilder;
begin
  Result := Self;
  FListItemDefaultHeight := V;
end;

function TDialogBuilder.SetMaskVisible(V: Boolean): TDialogBuilder;
begin
  Result := Self;
  FMaskVisible := V;
end;

function TDialogBuilder.SetMaxHeight(const V: Single): TDialogBuilder;
begin
  Result := Self;
  FMaxHeight := V;
end;

function TDialogBuilder.SetWidth(const V: Single): TDialogBuilder;
begin
  Result := Self;
  FWidth := V;
end;

function TDialogBuilder.SetWordWrap(V: Boolean): TDialogBuilder;
begin
  FWordWrap := V;
  Result := Self;
end;

function TDialogBuilder.SetMessage(const AMessage: string; IsHtmlText: Boolean): TDialogBuilder;
begin
  Result := Self;
  FMessage := AMessage;
  FMessageIsHtml := IsHtmlText;
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

function TDialogBuilder.SetOnInitA(AListener: TOnDialogInitA): TDialogBuilder;
begin
  Result := Self;
  FOnInitA := AListener;
end;

function TDialogBuilder.SetOnInitListAdapterA(
  AListener: TOnDialogInitListAdapterA): TDialogBuilder;
begin
  Result := Self;
  FOnInitListAdapterA := AListener;
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

function TDialogBuilder.SetPosition(
  APosition: TDialogViewPosition): TDialogBuilder;
begin
  Result := Self;
  FPosition := APosition;
end;

function TDialogBuilder.SetPositiveButton(const AText: string;
  AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FPositiveButtonText := AText;
  FPositiveButtonListenerA := AListener;
end;

function TDialogBuilder.SetPositiveButtonStyle(const AColor: Int64;
  const AStyle: TFontStyles; const ASize: Single): TDialogBuilder;
begin
  Result := Self;
  FPositiveButtonSize := ASize;
  FPositiveButtonColor := AColor;
  FPositiveButtonStyle := AStyle;
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

function TDialogBuilder.SetNegativeButtonStyle(const AColor: Int64;
  const AStyle: TFontStyles; const ASize: Single): TDialogBuilder;
begin
  Result := Self;
  FNegativeButtonSize := ASize;
  FNegativeButtonColor := AColor;
  FNegativeButtonStyle := AStyle;
end;

function TDialogBuilder.SetNeutralButton(const AText: string;
  AListener: TOnDialogClickListenerA): TDialogBuilder;
begin
  Result := Self;
  FNeutralButtonText := AText;
  FNeutralButtonListenerA := AListener;
end;

function TDialogBuilder.SetNeutralButtonStyle(const AColor: Int64;
  const AStyle: TFontStyles; const ASize: Single): TDialogBuilder;
begin
  Result := Self;
  FNeutralButtonSize := ASize;
  FNeutralButtonColor := AColor;
  FNeutralButtonStyle := AStyle;
end;

{ TDialog }

procedure TDialog.AnimatePlay(Ani: TFrameAniType; IsIn: Boolean;
  AEvent: TNotifyEventA);
var
  AniView: TControl;

  // 背景淡入淡出
  procedure DoFadeInOutBackgroyund();
  var
    NewValue: TAlphaColor;
  begin
    if not FMask then Exit;
    if not Assigned(FViewRoot) then Exit;
    if IsIn then begin
      if (FViewRoot.Background.ItemDefault.Color and $FF000000 = 0) then
        Exit;
      NewValue := FViewRoot.Background.ItemDefault.Color;
      FViewRoot.Background.ItemDefault.Color := 0;
    end else
      NewValue := 0;
    TFrameAnimator.AnimateColor(FViewRoot, 'Background.ItemDefault.Color', NewValue, nil, 0.3);
  end;

  // 淡入淡出
  procedure DoFadeInOut();
  var
    NewValue: Single;
  begin
    // 背景处理
    if Assigned(AniView) then begin
      if IsIn then begin
        AniView.Opacity := 0;
        NewValue := 1;
      end else begin
        NewValue := 0;
      end;
      TFrameAnimator.AnimateFloat(AniView, 'Opacity', NewValue, AEvent, 0.15);
    end;
  end;

  // 从顶部弹出
  procedure DoTopMoveInOut();
  var
    NewValue: Single;
  begin
    if Assigned(AniView) and Assigned(FViewRoot) then begin
      if IsIn then begin
        AniView.Position.Y := - AniView.Height;
        NewValue := 0;
        TFrameAnimator.AnimateFloat(AniView, 'Position.Y', NewValue, AEvent);
      end else begin
        NewValue := - FViewRoot.Height - AniView.Height;
        TFrameAnimator.AnimateFloat(AniView, 'Position.Y', NewValue, AEvent, 0.05);
      end;
    end;
  end;

  // 从底部弹出
  procedure DoBottomMoveInOut();
  var
    NewValue: Single;
  begin
    if Assigned(AniView) and Assigned(FViewRoot) then begin
      if IsIn then begin
        AniView.Position.Y := FViewRoot.Height;
        NewValue := FViewRoot.Height - AniView.Height;
        TFrameAnimator.AnimateFloat(AniView, 'Position.Y', NewValue, AEvent);
      end else begin
        NewValue := FViewRoot.Height;
        TFrameAnimator.AnimateFloat(AniView, 'Position.Y', NewValue, AEvent, 0.05);
      end;
    end;
  end;

  // 从左边弹出 弹入菜单
  procedure DoLeftSlideMenu();
  var
    NewValue: Single;
    LFrame: TFrame;
  begin
    if (Owner is TFrame) then LFrame := TFrame(Owner) else LFrame := nil;
    if Assigned(AniView) and Assigned(FViewRoot) then begin
      if IsIn then begin
        NewValue := AniView.Position.X;
        AniView.Position.X := -FViewRoot.Width + 1;
        TFrameAnimator.AnimateFloat(AniView, 'Position.X', NewValue, AEvent);

        if Assigned(LFrame) then begin
          FTempValue := LFrame.Position.X;
          TFrameAnimator.AnimateFloat(LFrame, 'Position.X', FTempValue + AniView.Width, AEvent);
        end;
      end else begin
        NewValue := -FViewRoot.Width + 1;
        TFrameAnimator.AnimateFloat(AniView, 'Position.X', NewValue, AEvent, 0.15);

        if Assigned(LFrame) then
          TFrameAnimator.AnimateFloat(LFrame, 'Position.X', FTempValue, nil, 0.15);
      end;
    end;
  end;

  // 从右边弹出 弹入菜单
  procedure DoRightSlideMenu();
  var
    NewValue: Single;
    LFrame: TFrame;
  begin
    if (Owner is TFrame) then LFrame := TFrame(Owner) else LFrame := nil;
    if Assigned(AniView) and Assigned(FViewRoot) then begin
      if IsIn then begin
        NewValue := FViewRoot.Width - AniView.Width;
        AniView.Position.X := FViewRoot.Width + 1;
        TFrameAnimator.AnimateFloat(AniView, 'Position.X', NewValue, AEvent);

        if Assigned(LFrame) then begin
          FTempValue := LFrame.Position.X;
          TFrameAnimator.AnimateFloat(LFrame, 'Position.X', FTempValue - AniView.Width, AEvent);
        end
      end else begin
        NewValue := FViewRoot.Width + 1;
        TFrameAnimator.AnimateFloat(AniView, 'Position.X', NewValue, AEvent, 0.15);

        if Assigned(LFrame) then
          TFrameAnimator.AnimateFloat(LFrame, 'Position.X', FTempValue, nil, 0.15);
      end;
    end;
  end;

begin
  if not Assigned(FViewRoot) then Exit;
  AniView := GetAniView;

  // 淡入淡出背景
  DoFadeInOutBackgroyund();
  // 如果图层完全不可见，设置动画时会出错
  if (not Assigned(AniView)) or (not FMask) or
    ((AniView is TView) and (TView(AniView).Background.ItemDefault.Color and $FF000000 = 0)) then begin
    if Assigned(AEvent) then
      AEvent(Self);
    Exit;
  end;

  // 处理动画
  case Ani of
    TFrameAniType.FadeInOut:
      DoFadeInOut;
    TFrameAniType.TopMoveInOut:
      DoTopMoveInOut;
    TFrameAniType.BottomMoveInOut:
      DoBottomMoveInOut;
    TFrameAniType.LeftSlideMenu:
      DoLeftSlideMenu;
    TFrameAniType.RightSlideMenu:
      DoRightSlideMenu;
  else
    begin
      // 无动画效果
      if Assigned(AEvent) then
        AEvent(Self);
      if IsIn then
        AniView.Opacity := 1
      else
        AniView.Opacity := 0;
    end;
  end;
end;

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
    Dialog.AsyncDismiss;
end;

constructor TDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCancelable := True;
  FCanceled := False;
  FAnimate := TFrameAniType.FadeInOut;
  FMask := True;
end;

destructor TDialog.Destroy;
begin
  {$IFNDEF MSWINDOWS}
  AtomicDecrement(DialogRef);// may cause duplicate name
  {$ENDIF}
  if Assigned(Self) then begin
    FIsDismiss := True;
    if (FViewRoot <> nil) then begin
      FEventing := False;
      Dismiss;
      FViewRoot := nil;
    end;
  end;
  FViewRoot := nil;
  inherited Destroy;
end;

procedure TDialog.Dismiss;
var
  LTarget: TControl;
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

  if Assigned(GetBuilder()) then
    LTarget := GetBuilder.FTarget
  else
    LTarget := nil;
  DoFreeBuilder();

  if (FViewRoot <> nil) and Assigned(FViewRoot.Parent) then begin
    {$IFDEF MSWINDOWS}
    FViewRoot.ParentForm.ReleaseCapture;
    {$ENDIF}
    LParent := FViewRoot.Parent;
    FViewRoot.Parent.RemoveObject(FViewRoot);
    FreeAndNil(FViewRoot);
    if LTarget <> nil then
      LTarget.SetFocusObject(LTarget)
    else if LParent <> nil then begin
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
var
  AniView: TControl;
begin
  if Assigned(Self) and Assigned(Owner) then begin
    if FEventing then begin
      FAllowDismiss := True;
      Exit;
    end;
    if (FViewRoot <> nil) then begin
      if (FViewRoot.FLayBubble <> nil) then
        FViewRoot.FLayBubble.Visible := False
//      else if FViewRoot.ControlsCount = 1 then // ShowView 时会是这种情况
//        FViewRoot.Controls[0].Visible := False;
    end;
    if FAnimate = TFrameAniType.None then begin
      AniView := GetAniView;
      if Assigned(AniView) then begin
        TFrameAnimator.DelayExecute(AniView,
          procedure (Sender: TObject)
          begin
            Dismiss;
          end,
        0.05);
      end else
        Dismiss;
    end else
      AnimatePlay(FAnimate, False,
        procedure (Sendet: TObject)
        begin
          Dismiss;
        end
      );
  end;
end;

procedure TDialog.DoFreeBuilder;
begin
  if (not (csDestroying in ComponentState)) and Assigned(FViewRoot) and
   (FViewRoot.ChildrenCount = 1) and (FViewRoot.FLayBubble = nil)
  then
    {$IF CompilerVersion >= 30}
    FViewRoot.Children[0].Parent := nil;
    {$ELSE}
    FViewRoot.Controls[0].Parent := nil;
    {$ENDIF}
end;

procedure TDialog.DoRootClick(Sender: TObject);
begin
  if FCancelable then
    Cancel;
end;

function TDialog.GetAniView: TControl;
begin
  if Assigned(FViewRoot.FLayBubble) then
    Result := FViewRoot.FLayBubble
  else begin
    if FViewRoot.{$IF CompilerVersion >= 30}ControlsCount{$ELSE}ChildrenCount{$ENDIF} = 1 then
      Result := FViewRoot.Controls[0]
    else
      Result := nil;
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
  if FViewRoot <> nil then begin
    Result := FViewRoot.FLayBubble;
    if Result = nil then
      Result := FViewRoot;
  end else
    Result := nil;
end;

function TDialog.GetViewRoot: TDialogView;
begin
  Result := FViewRoot;
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
  if FViewRoot.Background.ItemDefault.Color <> Value then
    FViewRoot.Background.ItemDefault.Color := Value;
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
      AnimatePlay(FAnimate, True, nil);
    end;
  except
    {$IFDEF WINDOWS}LogE(Self, 'Show', Exception(ExceptObject)); {$ENDIF}
    Dismiss;
  end;
end;

class function TDialog.ShowView(const AOwner: TComponent; const Target: TControl;
  const ViewClass: TControlClass; XOffset: Single; YOffset: Single;
  Position: TDialogViewPosition; Cancelable: Boolean; Ani: TFrameAniType; Mask: Boolean): TDialog;
var
  AView: TControl;
begin
  AView := ViewClass.Create(AOwner);
  Result := ShowView(AOwner, Target, AView, True, XOffset, YOffset, Position, Cancelable, Ani, Mask);
end;

class function TDialog.ShowView(const AOwner: TComponent; const Target, View: TControl;
  AViewAutoFree: Boolean; XOffset: Single; YOffset: Single;
  Position: TDialogViewPosition; Cancelable: Boolean; Ani: TFrameAniType; Mask: Boolean): TDialog;
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
  //Dialog.FViewRoot.FDisableAlign := True;

  Dialog.FViewRoot.OnClick := Dialog.DoRootClick;
  Dialog.FViewRoot.Parent := Dialog.GetFirstParent;
  if Dialog.FViewRoot.Parent = nil then begin
    Dialog.FViewRoot.EndUpdate;
    Dialog.Dismiss;
    Exit;
  end;

  Dialog.FViewRoot.Clickable := True;
  Dialog.FViewRoot.Align := TAlignLayout.Contents; //TAlignLayout.Client;
  Dialog.FViewRoot.Index := Dialog.FViewRoot.Parent.ChildrenCount - 1;
  Dialog.FViewRoot.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  Dialog.FViewRoot.CanFocus := False;
  {$IFDEF ANDROID}
  Dialog.FViewRoot.Padding.Top := TView.GetStatusHeight;
  {$ENDIF}

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
      LeftBottom:
        begin
          X := P.X + XOffset;
          Y := P.Y + PH + YOffset;
        end;
      RightBottom:
        begin
          X := PW - P.X + XOffset;
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

  case Position of
    LeftFill:
      begin
        X := 0 + XOffset;
        Y := 0 + YOffset;
        View.Height := Dialog.FViewRoot.Height - YOffset * 2;
        View.Width := (Dialog.FViewRoot.Width - XOffset) * SIZE_MENU_WIDTH;
        View.Align := TAlignLayout.Left;
      end;
    RightFill:
      begin
        PW := Dialog.FViewRoot.Width;
        PH := Dialog.FViewRoot.Height;
        View.Width := (PW - XOffset) * SIZE_MENU_WIDTH;
        X := PW - XOffset - View.Width;
        View.Height := PH - YOffset * 2;
        Y := 0 + YOffset;
        View.Align := TAlignLayout.Right;
      end;
  end;

  View.Position.Point := TPointF.Create(X, Y);
  if View is TFrameView then
    TFrameViewTmp(View).DoShow();

  Dialog.FAnimate := Ani;
  Dialog.FMask := Mask;
  Dialog.Cancelable := Cancelable;
  if Mask then
    Dialog.SetBackColor(GetDefaultStyleMgr.FDialogMaskColor);
  //Dialog.FViewRoot.FDisableAlign := True;
  Dialog.InitOK;
  Dialog.AnimatePlay(Dialog.FAnimate, True, nil);
  Result := Dialog;
end;

{ TCustomAlertDialog }

procedure TCustomAlertDialog.AdjustDownPopupPosition;
var
  P: TPointF;
  PW, PH, W, H, X, Y, OX, OY: Single;
begin
  P := TPointF.Zero;
  P := FBuilder.FTarget.LocalToAbsolute(P);

  W := FBuilder.FTarget.Width;
  H := FBuilder.FTarget.Height;

  PW := FViewRoot.Width;
  PH := FViewRoot.Height;

  X := P.X;
  Y := P.Y;

  OX := FBuilder.FTargetOffsetX;
  OY := FBuilder.FTargetOffsetY;

  case FBuilder.FTargetGravity of
    TLayoutGravity.LeftTop:
      begin
        X := X + OX;
        Y := Y + OY;
      end;
    TLayoutGravity.LeftBottom:
      begin
        X := X + OX;
        Y := Y + H + OY;
      end;
    TLayoutGravity.RightTop:
      begin
        X := X + (W - PW) + OX;
        Y := Y + OY;
      end;
    TLayoutGravity.RightBottom:
      begin
        X := X + (W - PW) + OX;
        Y := Y + H + OY;
      end;
  end;

  FViewRoot.FLayBubble.Position.Point := PointF(X, Y);

  H := PH - Y - FViewRoot.FLayBubble.Margins.Bottom;
  if (H < FViewRoot.FLayBubble.MaxHeight) or (FViewRoot.FLayBubble.MaxHeight = 0) then
    FViewRoot.FLayBubble.MaxHeight := H;
end;

procedure TCustomAlertDialog.Apply(const ABuilder: TDialogBuilder);
begin
  AtomicIncrement(DialogRef);
  FBuilder := ABuilder;
  if ABuilder = nil then Exit;

  FIsDowPopup := False;

  if Assigned(FBuilder.FTarget) then begin
    FIsDowPopup := True;
    InitDownPopupView();
  end else begin
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
  end;

  InitOK();

  FViewRoot.FIsDownPopup := FIsDowPopup;
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
      end else if Sender = FViewRoot.FButtonCancel then begin
        if Assigned(Builder.FCancelButtonListenerA) then
          Builder.FCancelButtonListenerA(Self, BUTTON_CANCEL)
        else if Assigned(Builder.CancelButtonListener) then
          Builder.CancelButtonListener(Self, BUTTON_CANCEL)
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

  procedure SetButton(var B: TButtonView; FText: string;
    FSize: Single; FColor: Int64; FStyle: TFontStyles);
  begin
    B.Text := FText;
    B.OnClick := DoButtonClick;
    if FSize > 0 then
      B.TextSettings.Font.Size := FSize;
    if FColor > -1 then
      B.TextSettings.Color.Default := FColor;
    if FStyle <> [] then
      B.TextSettings.Font.Style := FStyle;
  end;

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
  FViewRoot.Align := TAlignLayout.Contents; // TAlignLayout.Client;
  FViewRoot.Index := FViewRoot.Parent.ChildrenCount - 1;
  FViewRoot.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FViewRoot.InitView(StyleManager);

  if Builder.FWidth > 0 then begin
    FViewRoot.FLayBubble.WidthSize := TViewSize.CustomSize;
    FViewRoot.FLayBubble.Size.Width := Builder.FWidth;
    FViewRoot.FLayBubble.MaxWidth := Builder.FWidth;
    FViewRoot.FLayBubble.AdjustViewBounds := True;
  end else
    FViewRoot.FLayBubble.WidthSize := TViewSize.FillParent;

  if FBuilder.FMaxHeight > 0 then
    FViewRoot.FLayBubble.MaxHeight := FBuilder.FMaxHeight;

  // 初始化消息区
  if (Builder.FIcon <> nil) or (Builder.FMessage <> '') then begin
    FViewRoot.InitMessage(StyleManager);
    if Builder.MessageIsHtml then
      FViewRoot.FMsgMessage.HtmlText := Builder.FMessage
    else
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
    SetButton(FViewRoot.FButtonPositive, Builder.PositiveButtonText,
      Builder.PositiveButtonSize, Builder.PositiveButtonColor, Builder.PositiveButtonStyle);
    Inc(BtnCount);
    FViewRoot.FButtonPositive.Background.Corners := [TCorner.BottomLeft];
    FButtomRadius := FViewRoot.FButtonPositive;
  end;
  if Builder.NegativeButtonText = '' then
    FViewRoot.FButtonNegative.Visible := False
  else begin
    SetButton(FViewRoot.FButtonNegative, Builder.NegativeButtonText,
      Builder.NegativeButtonSize, Builder.NegativeButtonColor, Builder.NegativeButtonStyle);
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
    SetButton(FViewRoot.FButtonNeutral, Builder.NeutralButtonText,
      Builder.NeutralButtonSize, Builder.NeutralButtonColor, Builder.NeutralButtonStyle);
    Inc(BtnCount);
    if BtnCount = 1 then
      FViewRoot.FButtonNeutral.Background.Corners := [TCorner.BottomLeft, TCorner.BottomRight]
    else
      FViewRoot.FButtonNeutral.Background.Corners := [TCorner.BottomRight];
  end;
  if (BtnCount = 0) and (FViewRoot.FButtonLayout <> nil) then begin
    FViewRoot.FButtonLayout.Visible := False;
  end;
  if FViewRoot.FButtonCancel <> nil then begin
    if Builder.CancelButtonText = '' then
      FViewRoot.FButtonCancel.Visible := False
    else begin
      FViewRoot.FButtonCancel.Background.Corners := [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight];
      SetButton(FViewRoot.FButtonCancel, Builder.CancelButtonText,
        Builder.CancelButtonSize, Builder.CancelButtonColor, Builder.CancelButtonStyle);
      FViewRoot.FLayBubble.Margins.Bottom := FViewRoot.FButtonCancel.MinHeight + 30;
    end;
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

  if Assigned(FViewRoot.FTitleSpace) then begin
    if FViewRoot.FTitleView.Visible = False then
      FViewRoot.FTitleSpace.Visible := False
    else if (BtnCount = 0) and
      (FViewRoot.FMsgBody.Visible = False) and
      ((not Assigned(FViewRoot.FListView)) or (FViewRoot.FListView.Visible = False)) then
      FViewRoot.FTitleSpace.Visible := False;
  end;

  if (Builder.Title = '') then begin
    if FBuilder.Message = '' then begin
      if Assigned(FViewRoot.FListView) then
        FViewRoot.FListView.Margins.Top := StyleManager.FBackgroundRadius;
    end;
  end;

  if Builder.FMaskVisible then
    SetBackColor(StyleManager.FDialogMaskColor);
end;

procedure TCustomAlertDialog.InitDownPopupView;
var
  Sytle: TDialogStyleManager;
  BtnCount: Integer;
  BodyMH: Single;
begin
  Inc(DialogRef);
  Sytle := FBuilder.FStyleManager;
  if Sytle = nil then
    Sytle := GetDefaultStyleMgr;

  // 初始化基础
  FViewRoot := TDialogView.Create(Owner);
  FViewRoot.Name := '';
  FViewRoot.Dialog := Self;
  FViewRoot.BeginUpdate;
  FViewRoot.OnClick := DoRootClick;
  FViewRoot.Parent := GetFirstParent;
  if FViewRoot.Parent = nil then begin
    Dismiss;
    Exit;
  end;
  FViewRoot.Clickable := True;
  FViewRoot.Align := TAlignLayout.Contents; //TAlignLayout.Client;
  FViewRoot.Index := FViewRoot.Parent.ChildrenCount - 1;
  FViewRoot.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FViewRoot.InitView(Sytle);

//  FViewRoot.Background.ItemDefault.Kind := TViewBrushKind.Solid;
//  FViewRoot.Background.ItemDefault.Color := $7f33cc33;

//  FViewRoot.FLayBubble.Background.ItemDefault.Kind := TViewBrushKind.Solid;
//  FViewRoot.FLayBubble.Background.ItemDefault.Color := $7f33ccff;

  FViewRoot.FLayBubble.Layout.CenterInParent := False;

  if Builder.FWidth > 0 then begin
    FViewRoot.FLayBubble.WidthSize := TViewSize.CustomSize;
    FViewRoot.FLayBubble.Size.Width := Builder.FWidth;
    FViewRoot.FLayBubble.MaxWidth := Builder.FWidth;
    FViewRoot.FLayBubble.AdjustViewBounds := True;
  end else if Assigned(FBuilder.FTarget) then begin
    FViewRoot.FLayBubble.WidthSize := TViewSize.CustomSize;
    FViewRoot.FLayBubble.Size.Width := FBuilder.FTarget.Width;
    FViewRoot.FLayBubble.MaxWidth := FBuilder.FTarget.Width;
    FViewRoot.FLayBubble.AdjustViewBounds := True;
  end else
    FViewRoot.FLayBubble.WidthSize := TViewSize.FillParent;

  FViewRoot.FLayBubble.Paddings := '1';
  with TDrawableBorder(FViewRoot.FLayBubble.Background).Border do begin
    Style := TViewBorderStyle.RectBorder;
    Color.Default := Sytle.ListItemPressedColor;
  end;

  if FBuilder.FMaxHeight > 0 then
    FViewRoot.FLayBubble.MaxHeight := FBuilder.FMaxHeight;

  AdjustDownPopupPosition();

  // 初始化消息区
  if (Builder.FIcon <> nil) or (Builder.FMessage <> '') then begin
    FViewRoot.InitMessage(Sytle);
    if Builder.FMessageIsHtml then
      FViewRoot.FMsgMessage.HtmlText := Builder.FMessage
    else
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
  end else
    FViewRoot.FMsgBody.Visible := False;

  // 初始化列表
  if (Length(Builder.FItemArray) > 0) or
    ((Assigned(Builder.FItems)) and (Builder.FItems.Count > 0)) then begin
    FViewRoot.InitList(Sytle);
  end;

  // 初始化按钮
  BtnCount := 0;
  FViewRoot.FLayBubble.Background.Corners := [];
  FViewRoot.InitButton(Sytle);
  if Builder.PositiveButtonText = '' then
    FViewRoot.FButtonPositive.Visible := False
  else begin
    FViewRoot.FButtonPositive.Text := Builder.PositiveButtonText;
    FViewRoot.FButtonPositive.OnClick := DoButtonClick;
    Inc(BtnCount);
  end;
  if Builder.NegativeButtonText = '' then
    FViewRoot.FButtonNegative.Visible := False
  else begin
    FViewRoot.FButtonNegative.Text := Builder.NegativeButtonText;
    FViewRoot.FButtonNegative.OnClick := DoButtonClick;
    Inc(BtnCount);
  end;
  if Builder.NeutralButtonText = '' then begin
    FViewRoot.FButtonNeutral.Visible := False;
  end else begin
    FViewRoot.FButtonNeutral.Text := Builder.NeutralButtonText;
    FViewRoot.FButtonNeutral.OnClick := DoButtonClick;
    Inc(BtnCount);
  end;
  if (BtnCount = 0) and (FViewRoot.FButtonLayout <> nil) then begin
    FViewRoot.FButtonLayout.Visible := False;
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
    end;
  end;

  if Assigned(FViewRoot.FTitleSpace) then begin
    if FViewRoot.FTitleView.Visible = False then
      FViewRoot.FTitleSpace.Visible := False
    else if (BtnCount = 0) and
      (FViewRoot.FMsgBody.Visible = False) and
      ((not Assigned(FViewRoot.FListView)) or (FViewRoot.FListView.Visible = False)) then
      FViewRoot.FTitleSpace.Visible := False;
  end;

  if Builder.FMaskVisible then
    SetBackColor(Sytle.FDialogMaskColor);

  if FBuilder.View <> nil then
    // 附加 View 的对话框
    InitExtPopView()
  else if FBuilder.FIsSingleChoice then
    // 单选对话框
    InitSinglePopView()
  else if FBuilder.FIsMultiChoice then
    // 多选对话框
    InitMultiPopView()
  else if (Length(FBuilder.FItemArray) > 0) or
    (Assigned(FBuilder.Items) and (FBuilder.Items.Count > 0)) then
    // 列表框
    InitListPopView();
end;

procedure TCustomAlertDialog.InitExtPopView;
begin
  if not FIsDowPopup then
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
  if Builder.View is TFrameView then
    TFrameViewTmp(Builder.View).DoShow();
end;

procedure TCustomAlertDialog.InitList(const ListView: TListViewEx; IsMulti: Boolean);
var
  Adapter: IListAdapter;
begin
  Adapter := nil;
  if Assigned(FBuilder.FOnInitListAdapterA) then begin
    FBuilder.FOnInitListAdapterA(Self, FBuilder, Adapter);
  end;

  if not Assigned(Adapter) then begin
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

    if FBuilder.FListItemDefaultHeight > 0 then
      TStringsListAdapter(Adapter).DefaultItemHeight := FBuilder.FListItemDefaultHeight;
    TStringsListAdapter(Adapter).WordWrap := FBuilder.FWordWrap;
  end;

  ListView.Adapter := Adapter;
  ListView.Height := ListView.ContentBounds.Height;
end;

procedure TCustomAlertDialog.InitListPopView;
var
  ListView: TListViewEx;
begin
  if not FIsDowPopup then
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
  if not FIsDowPopup then
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
  if not FIsDowPopup then
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

type
  TMyControl = class(TControl);

{ TDialogView }

procedure TDialogView.AfterDialogKey(var Key: Word; Shift: TShiftState);
begin
  // 如果按下了返回键，且允许取消对话框，则关闭对话框
  if Assigned(Dialog) and (Dialog.Cancelable) and (Key in [vkEscape, vkHardwareBack]) then begin
    Dialog.Cancel;
    Key := 0;
  end else if Assigned(FDialog) then begin
    // 入云龙反馈在一些情况下会有问题，
    // 所以判断Key < 80时才传递事件
    if Assigned(FDialog.Builder) and Assigned(FDialog.Builder.View) and (Key < $80) then
      TMyControl(FDialog.Builder.View).KeyDown(Key, Char(Key), Shift)
    else if (ControlsCount = 1) and (not Assigned(FAnilndictor)) and (Key < $80) then
      TMyControl(Controls[0]).KeyDown(Key, Char(Key), Shift)
    else
      Key := 0;
  end;
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
  FTitleSpace := nil;
  inherited Destroy;
end;

procedure TDialogView.DoRealign;
begin
  inherited DoRealign;
  if (not FDisableAlign) and FIsDownPopup then
    TAlertDialog(FDialog).AdjustDownPopupPosition;
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

  function CreateButton(Parent: TFmxObject): TButtonView;
  begin
    Result := TButtonView.Create(Owner);
    Result.Parent := Parent;
    Result.Weight := 1;
    Result.MinHeight := StyleMgr.ButtonHeight;
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
  FButtonLayout.HeightSize := TViewSize.WrapContent;
  // 按钮
  FButtonPositive := CreateButton(FButtonLayout);
  FButtonPositive.Default := True;
  FButtonNegative := CreateButton(FButtonLayout);
  FButtonNeutral := CreateButton(FButtonLayout);

  if Assigned(FLayBubbleBottom) then begin
    // 按钮布局层
    FCancelButtonLayout := TLinearLayout.Create(Owner);
    {$IFDEF MSWINDOWS}
    FCancelButtonLayout.Name := 'CancelButtonLayout' + IntToStr(DialogRef);
    {$ENDIF}
    FCancelButtonLayout.Parent := FLayBubbleBottom;
    FCancelButtonLayout.WidthSize := TViewSize.FillParent;
    FCancelButtonLayout.Orientation := TOrientation.Horizontal;
    FCancelButtonLayout.HeightSize := TViewSize.WrapContent;
    // 按钮
    FButtonCancel := CreateButton(FCancelButtonLayout);
  end;
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
  FListView.Background.ItemPressed.Color := StyleMgr.ListItemPressedColor;
  FListView.Divider := StyleMgr.ListItemDividerColor;
  FListView.DragScroll := True;
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
  FMsgMessage.Gravity := StyleMgr.MessageTextGravity;
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
  FMsgMessage.Margins.Assign(StyleMgr.MessageTextMargins);
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
  FMsgMessage.Padding.Left := 16;
  FMsgMessage.Padding.Right := 16;
  FMsgMessage.WidthSize := TViewSize.WrapContent;
  FMsgMessage.HeightSize := TViewSize.WrapContent;
  FMsgMessage.Gravity := TLayoutGravity.Center;
  FMsgMessage.TextSettings.WordWrap := True;
  FMsgMessage.TextSettings.Color.Default := StyleMgr.ProcessTextColor;
  FMsgMessage.TextSettings.Font.Size := StyleMgr.MessageTextSize;
  FMsgMessage.AutoSize := True;
end;

procedure TDialogView.InitView(StyleMgr: TDialogStyleManager);

  function InitLayBubble(FName: string; FPosition: TDialogViewPosition): TLinearLayout;
  begin
    Result := TLinearLayout.Create(Owner);
    {$IFDEF MSWINDOWS}
    Result.Name := FName + IntToStr(DialogRef);
    {$ENDIF}
    // 消息框主体
    Result.Parent := Self;
    Result.Margin := '16';
    Result.ClipChildren := True;
    if FDialog.Builder.FUseRootBackColor then
      Result.Background.ItemDefault.Color := FDialog.Builder.FRootBackColor
    else begin
      Result.Background.ItemDefault.Color := StyleMgr.BackgroundColor;
      Result.Background.XRadius := StyleMgr.FBackgroundRadius;
      Result.Background.YRadius := StyleMgr.FBackgroundRadius;
    end;
    Result.Background.ItemDefault.Kind := TViewBrushKind.Solid;
    Result.Clickable := True;
    Result.WidthSize := TViewSize.FillParent;

    case FPosition of
      TDialogViewPosition.Top: begin
        Result.Layout.CenterHorizontal := True;
        Result.Layout.AlignParentTop := True;
      end;
      TDialogViewPosition.Bottom: begin
        Result.Layout.CenterHorizontal := True;
        Result.Layout.AlignParentBottom := True;
      end;
      TDialogViewPosition.LeftBottom: begin
        Result.Layout.AlignParentLeft := True;
        Result.Layout.AlignParentBottom := True;
      end;
      TDialogViewPosition.RightBottom: begin
        Result.Layout.AlignParentRight := True;
        Result.Layout.AlignParentBottom := True;
      end;
      TDialogViewPosition.Left: begin
        Result.Layout.CenterVertical := True;
        Result.Layout.AlignParentLeft := True;
      end;
      TDialogViewPosition.Right: begin
        Result.Layout.CenterVertical := True;
        Result.Layout.AlignParentRight := True;
      end;
      TDialogViewPosition.Center: begin
        Result.Layout.CenterInParent := True;
      end;
      TDialogViewPosition.LeftFill: ;
      TDialogViewPosition.RightFill: ;
    end;

    Result.HeightSize := TViewSize.WrapContent;
    Result.Orientation := TOrientation.Vertical;
    Result.CanFocus := False;
    Result.AdjustViewBounds := True;
    if StyleMgr.MaxWidth > 0 then
      Result.MaxWidth := StyleMgr.MaxWidth;
    Result.MaxHeight := Height - Result.Margins.Top - Result.Margins.Bottom;
  end;

begin
  CanFocus := False;
  FLayBubble := InitLayBubble('LayBubble', FDialog.Builder.FPosition);
  if (FDialog.Builder.FPosition = TDialogViewPosition.Bottom) and (FDialog.Builder.CancelButtonText <> '') then
    FLayBubbleBottom := InitLayBubble('LayBubbleBottom', TDialogViewPosition.Bottom);
  // 标题栏
  FTitleView := TTextView.Create(Owner);
  {$IFDEF MSWINDOWS}
  FTitleView.Name := 'TitleView' + IntToStr(DialogRef);
  {$ENDIF}
  FTitleView.Parent := FLayBubble;
  FTitleView.ClipChildren := True;
  FTitleView.TextSettings.Font.Size := StyleMgr.TitleTextSize;
  if StyleMgr.TitleTextBold then
    FTitleView.TextSettings.Font.Style := [TFontStyle.fsBold];
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
  // 标题与内容区的分隔线
  if StyleMgr.FTitleSpaceHeight > 0 then begin
    FTitleSpace := TView.Create(Owner);
    {$IFDEF MSWINDOWS}
    FTitleSpace.Name := 'TitleSpace' + IntToStr(DialogRef);
    {$ENDIF}
    FTitleSpace.Parent := FLayBubble;
    FTitleSpace.ClipChildren := True;
    FTitleSpace.Height := StyleMgr.FTitleSpaceHeight;
    FTitleSpace.Background.ItemDefault.Color := StyleMgr.FTitleSpaceColor;
    FTitleSpace.Background.ItemDefault.Kind := TViewBrushKind.Solid;
    FTitleSpace.WidthSize := TViewSize.FillParent;
  end;
  // 内容区
  FMsgBody := TLinearLayout.Create(Owner);
  {$IFDEF MSWINDOWS}
  FMsgBody.Name := 'MsgBody' + IntToStr(DialogRef);
  {$ENDIF}
  FMsgBody.Parent := FLayBubble;
  FMsgBody.ClipChildren := True;
  FMsgBody.Weight := 1;
  FMsgBody.MinHeight := 24;
  FMsgBody.WidthSize := TViewSize.FillParent;
  FMsgBody.HeightSize := TViewSize.WrapContent;
  FMsgBody.Orientation := TOrientation.Vertical;
  if FDialog.Builder.FUseRootBackColor then
    FMsgBody.Background.ItemDefault.Color := FDialog.Builder.FRootBackColor
  else
    FMsgBody.Background.ItemDefault.Color := StyleMgr.BodyBackGroundColor;
  FMsgBody.Background.ItemDefault.Kind := TViewBrushKind.Solid;
end;

procedure TDialogView.Resize;
begin
  inherited Resize;
  if Assigned(Dialog) and (ControlsCount = 1) then begin
    // 左右边栏菜单调整大小
    if (TDialog(Dialog).FAnimate in [TFrameAniType.LeftSlideMenu, TFrameAniType.RightSlideMenu]) and
      (TDialog(Dialog).Owner is TFrame) then
      Controls[0].Width := Width * SIZE_MENU_WIDTH;
  end;
end;

procedure TDialogView.SetTitle(const AText: string);
begin
  if FTitleView <> nil then begin
    FTitleView.Text := AText;
    if AText = '' then begin
      FTitleView.Visible := False;
      if Assigned(FTitleSpace) then
        FTitleSpace.Visible := False;
    end;
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
  FMessageTextMargins := TBounds.Create(TRectF.Empty);
  FMessageTextGravity := TLayoutGravity.CenterVertical;

  FTitleTextSize := FONT_TitleTextSize;
  FButtonTextSize := FONT_ButtonTextSize;
  FButtonHeight := SIZE_ButtonHeight;
  FIconSize := SIZE_ICON;
  FTitleHeight := SIZE_TitleHeight;
  FTitleGravity := Title_Gravity;
  FBackgroundRadius := SIZE_BackgroundRadius;
  FTitleTextBold := False;

  FButtonColor := TButtonViewColor.Create();
  FButtonColor.Default := COLOR_ButtonColor;
  FButtonColor.Pressed := COLOR_ButtonPressColor;

  FButtonBorder := TViewBorder.Create;
  FButtonBorder.Width := SIZE_ButtonBorder;
  FButtonBorder.Style := TViewBorderStyle.RectBorder;
  FButtonBorder.Color.Default := COLOR_ButtonBorderColor;
  FButtonBorder.Color.DefaultChange := False;

  FButtonTextColor := TTextColor.Create(COLOR_ButtonTextColor);
  FButtonTextColor.Pressed := COLOR_ButtonTextPressColor;

  FTitleSpaceHeight := SIZE_TitleSpaceHeight;
  FTitleSpaceColor := COLOR_TitleSpaceColor;

  FListItemPressedColor := COLOR_ListItemPressedColor;
  FListItemDividerColor := COLOR_LIstItemDividerColor;

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
  FreeAndNil(FMessageTextMargins);
  FreeAndNil(FButtonColor);
  FreeAndNil(FButtonBorder);
  FreeAndNil(FButtonTextColor);
  inherited;
end;

function TDialogStyleManager.GetMessageTextMargins: TBounds;
begin
  Result := FMessageTextMargins;
end;

function TDialogStyleManager.IsStoredBackgroundRadius: Boolean;
begin
  Result := FBackgroundRadius <> SIZE_BackgroundRadius;
end;

function TDialogStyleManager.IsStoredTitleSpaceHeight: Boolean;
begin
  Result := FTitleSpaceHeight <> SIZE_TitleSpaceHeight;
end;

procedure TDialogStyleManager.SetButtonBorder(const Value: TViewBorder);
begin
  FButtonBorder.Assign(Value);
end;

procedure TDialogStyleManager.SetButtonColor(const Value: TButtonViewColor);
begin
  FButtonColor.Assign(Value);
end;

procedure TDialogStyleManager.SetButtonTextColor(const Value: TTextColor);
begin
  FButtonTextColor.Assign(Value);
end;


procedure TDialogStyleManager.SetMessageTextMargins(const Value: TBounds);
begin
  FMessageTextMargins.Assign(Value);
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

procedure TProgressDialog.InitView(const AMsg: string; IsHtmlText: Boolean);
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
  FViewRoot.Align := TAlignLayout.Contents; //TAlignLayout.Client;
  FViewRoot.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  FViewRoot.InitProcessView(Style);
  if AMsg = '' then
    FViewRoot.FMsgMessage.Visible := False
  else begin
    if IsHtmlText then
      FViewRoot.FMsgMessage.HtmlText := AMsg
    else
      FViewRoot.FMsgMessage.Text := AMsg;
    FViewRoot.FMsgMessage.Visible := True;
    FViewRoot.FLayBubble.WidthSize := TViewSize.CustomSize;
    FViewRoot.FLayBubble.Width := FViewRoot.FMsgMessage.Width + 32;
    FViewRoot.FMsgMessage.WidthSize := TViewSize.FillParent;
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

{ TButtonViewColor }

constructor TButtonViewColor.Create(const ADefaultColor: TAlphaColor);
begin
  inherited Create(ADefaultColor);

end;

initialization

finalization
  FreeAndNil(DefaultStyleManager);

end.

