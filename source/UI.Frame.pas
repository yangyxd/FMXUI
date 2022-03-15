﻿{*******************************************************}
{                                                       }
{       FMX UI Frame 管理单元                           }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Frame;

interface

uses
  UI.Base, UI.Toast, UI.Dialog, UI.Ani,
  System.NetEncoding,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.Rtti, System.SyncObjs, System.Messaging,
  {$IFDEF ANDROID}
  FMX.Platform.Android,
  FMX.VirtualKeyboard.Android,
  {$ENDIF}
  {$IFDEF POSIX}Posix.Signal, {$ENDIF}
  FMX.Ani, FMX.VirtualKeyboard,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Platform, IOUtils;

type
  TFrameView = class;
  TFrameViewClass = class of TFrameView;
  TNotifyEventA = UI.Ani.TNotifyEventA;
  TFrameAniType = UI.Ani.TFrameAniType;
  TFrameAnimator = UI.Ani.TFrameAnimator;

  /// <summary>
  /// Frame 参数
  /// </summary>
  TFrameParams = class(TDictionary<string, TValue>);

  TFrameDataType = (fdt_Integer, fdt_Long, fdt_Int64, fdt_Float, fdt_String,
    fdt_DateTime, fdt_Number, fdt_Boolean);

  TFrameDataValue = record
    DataType: TFrameDataType;
    Value: TValue;
  end;

  /// <summary>
  /// Frame 状态数据
  /// </summary>
  TFrameStateData = class(TDictionary<string, TFrameDataValue>);

  TFrameStateDataHelper = class helper for TFrameStateData
    function GetDataValue(DataType: TFrameDataType; const Value: TValue): TFrameDataValue;
    function GetString(const Key: string): string;
    function GetInt(const Key: string; const DefaultValue: Integer = 0): Integer;
    function GetLong(const Key: string; const DefaultValue: Cardinal = 0): Cardinal;
    function GetInt64(const Key: string; const DefaultValue: Int64 = 0): Int64;
    function GetFloat(const Key: string; const DefaultValue: Double = 0): Double;
    function GetDateTime(const Key: string; const DefaultValue: TDateTime = 0): TDateTime;
    function GetNumber(const Key: string; const DefaultValue: NativeUInt = 0): NativeUInt;
    function GetPointer(const Key: string): Pointer;
    function GetBoolean(const Key: string; const DefaultValue: Boolean = False): Boolean;

    function Put(const Key: string; const Value: string): TFrameStateData; overload; inline;
    function Put(const Key: string; const Value: Integer): TFrameStateData; overload; inline;
    function Put(const Key: string; const Value: Cardinal): TFrameStateData; overload; inline;
    function Put(const Key: string; const Value: Int64): TFrameStateData; overload; inline;
    function Put(const Key: string; const Value: Double): TFrameStateData; overload; inline;
    function Put(const Key: string; const Value: NativeUInt): TFrameStateData; overload; inline;
    function Put(const Key: string; const Value: Boolean): TFrameStateData; overload; inline;
    function PutDateTime(const Key: string; const Value: TDateTime): TFrameStateData; inline;
  end;

  TFrameParamsHelper = class helper for TFrameParams
    function GetString(const Key: string): string;
    function GetInt(const Key: string; const DefaultValue: Integer = 0): Integer;
    function GetLong(const Key: string; const DefaultValue: Cardinal = 0): Cardinal;
    function GetInt64(const Key: string; const DefaultValue: Int64 = 0): Int64;
    function GetFloat(const Key: string; const DefaultValue: Double = 0): Double;
    function GetDateTime(const Key: string; const DefaultValue: TDateTime = 0): TDateTime;
    function GetNumber(const Key: string; const DefaultValue: NativeUInt = 0): NativeUInt;
    function GetPointer(const Key: string): Pointer;
    function GetBoolean(const Key: string; const DefaultValue: Boolean = False): Boolean;

    function Put(const Key: string; const Value: string): TFrameParams; overload;
    function Put(const Key: string; const Value: Integer): TFrameParams; overload;
    function Put(const Key: string; const Value: Cardinal): TFrameParams; overload;
    function Put(const Key: string; const Value: Int64): TFrameParams; overload;
    function Put(const Key: string; const Value: Double): TFrameParams; overload;
    function Put(const Key: string; const Value: NativeUInt): TFrameParams; overload;
    function Put(const Key: string; const Value: Boolean): TFrameParams; overload;
    function PutDateTime(const Key: string; const Value: TDateTime): TFrameParams;
  end;

  /// <summary>
  /// Frame 状态
  /// </summary>
  TFrameState = class(TObject)
  private
    [Weak] FOwner: TComponent;
    FData: TFrameStateData;
    FIsChange: Boolean;
    FIsPublic: Boolean;
    FIsLoad: Boolean;
    FLocker: TCriticalSection;
    function GetCount: Integer;
    function GetStoragePath: string;
    procedure SetStoragePath(const Value: string);
  protected
    procedure InitData;
    procedure DoValueNotify(Sender: TObject; const Item: TFrameDataValue;
      Action: TCollectionNotification);
    function GetUniqueName: string;
    function GetFileName(const FileName: string): string;
    procedure Load();
  public
    constructor Create(AOwner: TComponent; IsPublic: Boolean);
    destructor Destroy; override;

    procedure Clear();
    procedure Save();

    function Exist(const Key: string): Boolean;
    function ContainsKey(const Key: string): Boolean;

    function GetString(const Key: string): string;
    function GetInt(const Key: string; const DefaultValue: Integer = 0): Integer;
    function GetLong(const Key: string; const DefaultValue: Cardinal = 0): Cardinal;
    function GetInt64(const Key: string; const DefaultValue: Int64 = 0): Int64;
    function GetFloat(const Key: string; const DefaultValue: Double = 0): Double;
    function GetDateTime(const Key: string; const DefaultValue: TDateTime = 0): TDateTime;
    function GetNumber(const Key: string; const DefaultValue: NativeUInt = 0): NativeUInt;
    function GetBoolean(const Key: string; const DefaultValue: Boolean = False): Boolean;

    procedure Put(const Key: string; const Value: string); overload;
    procedure Put(const Key: string; const Value: Integer); overload;
    procedure Put(const Key: string; const Value: Cardinal); overload;
    procedure Put(const Key: string; const Value: Int64); overload;
    procedure Put(const Key: string; const Value: Double); overload;
    procedure Put(const Key: string; const Value: NativeUInt); overload;
    procedure Put(const Key: string; const Value: Boolean); overload;
    procedure PutDateTime(const Key: string; const Value: TDateTime);

    /// <summary>
    /// 保存文件流
    /// </summary>
    function SaveFile(const FileName: string; const Data: TStream): Boolean;
    /// <summary>
    /// 读取指定的文件流
    /// </summary>
    function ReadFile(const FileName: string; var OutData: TStream): Boolean;

    property Data: TFrameStateData read FData;
    property Count: Integer read GetCount;
    property StoragePath: string read GetStoragePath write SetStoragePath;
  end;

  TCustomFormHelper = class Helper for TCustomForm
  public
    procedure SetFocus();
  end;

  /// <summary>
  /// Frame 视图, Frame 切换处理
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TFrameView = class(FMX.Forms.TFrame)
  private
    FDefaultAni: TFrameAniType;
    FParams: TFrameParams;
    FPrivateState: TFrameState;
    FBackColor: TAlphaColor;
    FStatusColor: TAlphaColor;
    FStatusLight: Boolean;
    FStatusTransparent: Boolean;
    FUseDefaultBackColor: Boolean;
    FUseDefaultStatusColor: Boolean;
    FUseDefaultStatusLight: Boolean;
    FUseDefaultStatusTransparent: Boolean;
    FOnShow: TNotifyEvent;
    FOnShown: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnReStart: TNotifyEvent;
    FOnFree: TNotifyEvent;
    FWaitDialog: TProgressDialog;
    [weak] FToastManager: TToastManager;
    FShowing: Boolean;    // 正在显示中
    FHideing: Boolean;    // 正在隐藏中
    FAnimateing: Boolean; // 动画执行中
    FNeedFree: Boolean;   // 需要释放
    FNeedHide: Boolean;   // 需要隐藏
    FNeedFinish: Boolean; // 需要关闭
    FNeedDoCreate: Boolean; // 需要执行DoCreate;
    FResumed: Boolean;
    procedure SetParams(const Value: TFrameParams);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetDataString: string;
    procedure SetDataString(const Value: string);
    function GetPreferences: TFrameState;
    function GetSharedPreferences: TFrameState;
    function GetParams: TFrameParams;
    function GetDataAsPointer: Pointer;
    function GetIsWaitDismiss: Boolean;
    function GetStatusColor: TAlphaColor;
    procedure SetStatusColor(const Value: TAlphaColor);
    function GetStatusLight: Boolean;
    procedure SetStatusLight(const Value: Boolean);
    function GetStatusTransparent: Boolean;
    procedure SetStatusTransparent(const Value: Boolean);
    function GetParentForm: TCustomForm;
    function GetBackColor: TAlphaColor;
    procedure SetBackColor(const Value: TAlphaColor);
    function GetIsDestroy: Boolean;

    function FinishIsFreeApp: Boolean;
    function GetActiveFrame: TFrameView;
  protected
    class var FActiveFrames: TDictionary<TCommonCustomForm, TFrameView>;
    class procedure DoActivateMessage(const Sender: TObject; const M: TMessage);
  protected
    [Weak] FLastView: TFrameView;
    [Weak] FNextView: TFrameView;
    function MakeFrame(FrameClass: TFrameViewClass): TFrameView; overload;

    /// <summary>
    /// Frame 初始化时触发
    /// </summary>
    procedure DoCreate(); virtual;
    /// <summary>
    /// Frame 正在显示之前触发
    /// </summary>
    procedure DoShow(); virtual;
    /// <summary>
    /// Frame 完全显示之后触发
    /// </summary>
    procedure DoShown(); virtual;
    /// <summary>
    /// Frame 隐藏显示时触发 (尽量使用 DoFinish )
    /// </summary>
    procedure DoHide(); virtual;
    /// <summary>
    /// 检测当前Frame是否允许关闭
    /// </summary>
    function DoCanFinish(): Boolean; virtual;
    /// <summary>
    /// 检测当前Frame是否允许被键盘关闭
    /// </summary>
    function DoCanKeyFinish(): Boolean; virtual;
    /// <summary>
    /// Frame 需要关闭时，在关闭之前触发
    /// </summary>
    procedure DoFinish(); virtual;
    /// <summary>
    /// Frame 在重新显示之前触发
    /// </summary>
    procedure DoReStart(); virtual;
    /// <summary>
    /// Frame 在释放时触发
    /// </summary>
    procedure DoFree(); virtual;

    /// <summary>
    /// 检测是否允许暂停
    /// </summary>
    function DoCanPause: Boolean; virtual;
    /// <summary>
    /// Frame 在前端时触发
    /// </summary>
    procedure DoResume; virtual;
    /// <summary>
    /// Frame 离开前端时触发
    /// </summary>
    procedure DoPause; virtual;

    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;

    function GetToastManager: TToastManager; virtual;

    /// <summary>
    /// 检测是否允许释放
    /// </summary>
    function DoCanFree(): Boolean; virtual;
    // 检查是否需要释放，如果需要，就释放掉
    function CheckFree(): Boolean; virtual;
    // 检查所属窗体是否还存在 Frame
    function CheckChildern(): Boolean;
    // 内部 Show 实现
    procedure InternalShow(TriggerOnShow: Boolean; AOnFinish: TNotifyEventA = nil;
      Ani: TFrameAniType = TFrameAniType.DefaultAni; SwitchFlag: Boolean = False);
    procedure InternalHide();
  protected
    procedure Paint; override;
    procedure SetParent(const Value: TFmxObject); override;
    procedure AfterDialogKey(var Key: Word; Shift: TShiftState); override;
  protected
    /// <summary>
    /// 播放动画
    /// <param name="Ani">动画类型</param>
    /// <param name="IsIn">是否是正要显示</param>
    /// <param name="SwitchFlag">动画切换标志</param>
    /// <param name="AEvent">动画播放完成事件</param>
    /// </summary>
    procedure AnimatePlay(Ani: TFrameAniType; IsIn, SwitchFlag: Boolean; AEvent: TNotifyEventA);

    procedure OnFinishOrClose(Sender: TObject);
  public
    class constructor Create;
    class destructor Destroy;
    class function GetFormActiveFrame(AForm: TCommonCustomForm; var AFrame: TFrameView): Boolean;
    class procedure SetFormActiveFrame(AForm: TCommonCustomForm; AFrame: TFrameView);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// 终止APP
    /// </summary>
    class procedure AppTerminate();

    /// <summary>
    /// 设置 Frame 默认背景颜色
    /// </summary>
    class procedure SetDefaultBackColor(const Value: TAlphaColor);

    /// <summary>
    /// 设置 Frame 默认状态条颜色
    /// </summary>
    class procedure SetDefaultStatusColor(const Value: TAlphaColor);
    /// <summary>
    /// 设置 Frame 默认状态条透明
    /// </summary>
    class procedure SetDefaultStatusTransparent(const Value: Boolean);
    /// <summary>
    /// 设置 Frame 默认状态条黑色图标
    /// </summary>
    class procedure SetDefaultStatusLight(const Value: Boolean);

    /// <summary>
    /// 设置 Frame 状态栏透明新方法，浅色状态栏和全透明必须开启这个
    /// </summary>
    class procedure SetStatusTransparentNewMethod(const Value: Boolean);

    /// <summary>
    /// 更新状态栏
    /// </summary>
    class procedure UpdateStatusBar(const AForm: TCommonCustomForm; const AColor: TAlphaColor; const ATransparent: Boolean; const ALight: Boolean); overload;
    class procedure UpdateStatusBar; overload;

    /// <summary>
    /// 刷新当前Frame状态栏
    /// </summary>
    procedure RefreshStatusBar;

    /// <summary>
    /// 流转化为 string
    /// </summary>
    function StreamToString(SrcStream: TStream; const CharSet: string = ''): string;

    /// <summary>
    /// 显示等待对话框
    /// </summary>
    procedure ShowWaitDialog(const AMsg: string; ACancelable: Boolean = True); overload;
    procedure ShowWaitDialog(const AMsg: string; OnDismissListener: TOnDialogListener; ACancelable: Boolean = True); overload;
    procedure ShowWaitDialog(const AMsg: string; OnDismissListener: TOnDialogListenerA; ACancelable: Boolean = True); overload;

    /// <summary>
    /// 更新等待对话框消息内容
    /// </summary>
    procedure UpdateWaitDialog(const AMsg: string);

    /// <summary>
    /// 隐藏等待对话框
    /// </summary>
    procedure HideWaitDialog();

    /// <summary>
    /// 显示 Frame
    /// </summary>
    class function ShowFrame(Parent: TFmxObject; Params: TFrameParams;
      Ani: TFrameAniType = TFrameAniType.None; SwitchFlag: Boolean = False): TFrameView; overload;
    /// <summary>
    /// 显示 Frame
    /// </summary>
    class function ShowFrame(Parent: TFmxObject; const Title: string = ''; Ani: TFrameAniType = TFrameAniType.None;
      SwitchFlag: Boolean = False): TFrameView; overload;
    /// <summary>
    /// 显示 Frame
    /// </summary>
    class function CreateFrame(Parent: TFmxObject; Params: TFrameParams): TFrameView; overload;
    /// <summary>
    /// 显示 Frame
    /// </summary>
    class function CreateFrame(Parent: TFmxObject; const Title: string = ''): TFrameView; overload;

    /// <summary>
    /// 开始一个视图，并隐藏当前视图
    /// </summary>
    function StartFrame(FrameClass: TFrameViewClass; Ani: TFrameAniType = TFrameAniType.DefaultAni): TFrameView; overload;
    /// <summary>
    /// 开始一个视图，并隐藏当前视图
    /// </summary>
    function StartFrame(FrameClass: TFrameViewClass; Params: TFrameParams; Ani: TFrameAniType = TFrameAniType.DefaultAni): TFrameView; overload;
    /// <summary>
    /// 开始一个视图，并隐藏当前视图
    /// </summary>
    function StartFrame(FrameClass: TFrameViewClass; const Title: string; Ani: TFrameAniType = TFrameAniType.DefaultAni): TFrameView; overload;
    /// <summary>
    /// 开始一个视图，并隐藏当前视图
    /// </summary>
    function StartFrame(FrameClass: TFrameViewClass; const Title: string; const Data: TValue; Ani: TFrameAniType = TFrameAniType.DefaultAni): TFrameView; overload;
    /// <summary>
    /// 开始一个视图，并隐藏当前视图
    /// </summary>
    function StartFrame(FrameClass: TFrameViewClass; const Title: string; const DataString: string; Ani: TFrameAniType = TFrameAniType.DefaultAni): TFrameView; overload;

    /// <summary>
    /// 显示一个提示消息
    /// </summary>
    procedure Hint(const Msg: string); overload;
    procedure Hint(const Msg: Double); overload;
    procedure Hint(const Msg: Int64); overload;
    procedure Hint(const AFormat: string; const Args: array of const); overload;

    /// <summary>
    /// 延时执行任务
    /// </summary>
    procedure DelayExecute(ADelay: Single; AExecute: TNotifyEventA);

    /// <summary>
    /// 显示 Frame
    /// </summary>
    procedure Show(); overload; override;
    procedure Show(Ani: TFrameAniType; AOnFinish: TNotifyEventA;
      SwitchFlag: Boolean = False;
      TriggerOnShow: Boolean = True); reintroduce; overload;
    /// <summary>
    /// 关闭 Frame
    /// </summary>
    procedure Close(); overload;
    procedure Close(Ani: TFrameAniType); overload; virtual;
    /// <summary>
    /// 隐藏 Frame
    /// </summary>
    procedure Hide(); overload; override;
    procedure Hide(Ani: TFrameAniType; SwitchFlag: Boolean = False); reintroduce; overload;
    /// <summary>
    /// 完成当前 Frame (返回上一个 Frame 或 关闭)
    /// </summary>
    procedure Finish(); overload; virtual;
    procedure Finish(Ani: TFrameAniType); overload; virtual;

    /// <summary>
    /// 开始/重新开始当前Frame
    /// </summary>
    procedure Resume; virtual;
    /// <summary>
    /// 暂停当前Frame
    /// </summary>
    procedure Pause(AResume: Boolean = False); virtual;

    /// <summary>
    /// 启动时的参数
    /// </summary>
    property Params: TFrameParams read GetParams write SetParams;
    /// <summary>
    /// 启动此Frame的Frame
    /// </summary>
    property Last: TFrameView read FLastView;

    /// <summary>
    /// 私有预设参数 (私有，非线程安全)
    /// </summary>
    property Preferences: TFrameState read GetPreferences;
    /// <summary>
    /// 共有预设参数 (全局，非线程安全)
    /// </summary>
    property SharedPreferences: TFrameState read GetSharedPreferences;
    /// <summary>
    /// 是否正在Show
    /// </summary>
    property Showing: Boolean read FShowing;

    property DataAsPointer: Pointer read GetDataAsPointer;

    /// <summary>
    /// 当前 Frame 所绑定的 Form 对象
    /// </summary>
    property ParentForm: TCustomForm read GetParentForm;

    /// <summary>
    /// 当前 Frame 所绑定的 Form 对象对应的活动Frame
    /// </summary>
    property ActiveFrame: TFrameView read GetActiveFrame;

    /// <summary>
    /// 等待对话框是否被取消了
    /// </summary>
    property IsWaitDismiss: Boolean read GetIsWaitDismiss;
    /// <summary>
    /// 是否已经释放
    /// </summary>
    property IsDestroy: Boolean read GetIsDestroy;
    /// <summary>
    /// 是否使用了默认背景色
    /// </summary>
    property IsUseDefaultBackColor: Boolean read FUseDefaultBackColor;
    /// <summary>
    /// 是否在前台
    /// </summary>
    property IsResumed: Boolean read FResumed;
  published
    property Title: string read GetTitle write SetTitle;
    property DataString: string read GetDataString write SetDataString;
    /// <summary>
    /// 背景颜色
    /// </summary>
    property BackColor: TAlphaColor read GetBackColor write SetBackColor;
    /// <summary>
    /// APP 顶部状态条颜色
    /// </summary>
    property StatusColor: TAlphaColor read GetStatusColor write SetStatusColor;
    /// <summary>
    /// APP 顶部状态条透明
    /// </summary>
    property StatusTransparent: Boolean read GetStatusTransparent write SetStatusTransparent;
    /// <summary>
    /// APP 顶部状态条黑色图标
    /// </summary>
    property StatusLight: Boolean read GetStatusLight write SetStatusLight;

    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnShown: TNotifyEvent read FOnShown write FOnShown;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnReStart: TNotifyEvent read FOnReStart write FOnReStart;
    property OnFree: TNotifyEvent read FOnFree write FOnFree;
  end;

type
  TFrame = class(TFrameView);
  TFrameClass = type of TFrame;

const
  CS_Title = 'cs_p_title';
  CS_Data = 'cs_p_data';
  CS_DataStr = 'cs_p_data_str';

var
  /// <summary>
  /// 默认过场动画
  /// </summary>
  DefaultAnimate: TFrameAniType = TFrameAniType.MoveInOut;

implementation

{$IFDEF ANDROID}
uses
  {$IF CompilerVersion >= 34}
  Androidapi.AppGlue,
  Androidapi.NativeActivity,
  {$ENDIF}
  Androidapi.Helpers,
  Androidapi.Jni,
  //Androidapi.JNI.Media,
  Androidapi.JNIBridge,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  FMX.Helpers.Android;
{$ENDIF}

var
  /// <summary>
  /// 公共状态数据
  /// </summary>
  FPublicState: TFrameState = nil;

  FDefaultBackColor: TAlphaColor = 0;
  FDefaultStatusColor: TAlphaColor = 0;
  FDefaultStatusTransparent: Boolean = False;
  FDefaultStatusLight: Boolean = True;
  FStatusTransparentNewMethod: Boolean = False;

{$IFDEF ANDROID}
{$IF CompilerVersion >= 33}
type
  // 感谢 谭钦
  // 监听创建事件
  TKSCListener = class(TJavaLocal, JOnKeyboardStateChangedListener)
  public
    { JOnKeyboardStateChangedListener }
    procedure onVirtualKeyboardWillShown; cdecl;
    procedure onVirtualKeyboardFrameChanged(newFrame: JRect); cdecl;
    procedure onVirtualKeyboardWillHidden; cdecl;
  end;

var
  FKSListener: TKSCListener = nil;
  FSystemUiVisibility: Integer = -1;
{$ENDIF}
{$IF CompilerVersion >= 34}
type
  TMyAndroidApplicationGlue = class(TAndroidApplicationGlue)
  private
    class var FOldonWindowFocusChanged: TOnWindowFocusChangedCallback;

    /// <summary>Called when window changes focus</summary>
    class procedure onWindowFocusChanged(activity: PANativeActivity; focused: Integer); cdecl; static;

    class procedure BindCallbacks;
    class function GetActivityCallbacks: PANativeActivityCallbacks;
  end;
{$ENDIF}

// 解决有时返回键失效问题
var
  FVKState: PByte = nil;
  FFirstCreateFrame: Boolean = True;

procedure UpdateAndroidKeyboardServiceState;
var
  ASvc: IFMXVirtualKeyboardService;
  AContext: TRttiContext;
  AType: TRttiType;
  AField: TRttiField;
  AInst: TVirtualKeyboardAndroid;
begin
  Exit;
  if not Assigned(FVKState) then begin
    if (not Assigned(Screen.FocusControl)) and
      TPlatformServices.Current.SupportsPlatformService
      (IFMXVirtualKeyboardService, ASvc) then
    begin
      AInst := ASvc as TVirtualKeyboardAndroid;
      AContext := TRttiContext.Create;
      AType := AContext.GetType(TVirtualKeyboardAndroid);
      AField := AType.GetField('FState');
      if AField.GetValue(AInst).AsOrdinal <> 0 then
      begin
        FVKState := PByte(AInst);
        Inc(FVKState, AField.Offset);
      end;
    end;
  end;
  if Assigned(FVKState) and (FVKState^ <> 0) then
    FVKState^ := 0;
end;

function AlphaColorToJColor(const AColor: TAlphaColor): Integer;
begin
  Result := TJColor.JavaClass.argb(TAlphaColorRec(AColor).A, TAlphaColorRec(AColor).R,
    TAlphaColorRec(AColor).G, TAlphaColorRec(AColor).B);
end;

{$IF CompilerVersion >= 33}
{ TKSCListener }

procedure TKSCListener.onVirtualKeyboardFrameChanged(newFrame: JRect);
begin
  // 底部导航栏隐藏显示，也会触发这里，说白了，这里是布局改变都会触发
  TFrameView.UpdateStatusBar;
  TThread.CreateAnonymousThread(procedure begin
    TFrameView.UpdateStatusBar;
  end).Start;
end;

procedure TKSCListener.onVirtualKeyboardWillHidden;
begin
  TFrameView.UpdateStatusBar;
  TThread.CreateAnonymousThread(procedure begin
    TFrameView.UpdateStatusBar;
  end).Start;
end;

procedure TKSCListener.onVirtualKeyboardWillShown;
begin
  TFrameView.UpdateStatusBar;
end;
{$ENDIF}

{$IF CompilerVersion >= 34}
class procedure TMyAndroidApplicationGlue.onWindowFocusChanged(activity: PANativeActivity; focused: Integer); cdecl;
begin
  if Assigned(FOldonWindowFocusChanged) then
    FOldonWindowFocusChanged(activity, focused);

  if focused <> 0 then
    TFrameView.DoActivateMessage(nil, TApplicationEventMessage.Create(TApplicationEventData.Create(TApplicationEvent.BecameActive, nil)))
  else
    TFrameView.DoActivateMessage(nil, TApplicationEventMessage.Create(TApplicationEventData.Create(TApplicationEvent.WillBecomeInactive, nil)))
end;

class procedure TMyAndroidApplicationGlue.BindCallbacks;
begin
  with GetActivityCallbacks^ do begin
    FOldonWindowFocusChanged := onWindowFocusChanged;
    onWindowFocusChanged := @TMyAndroidApplicationGlue.onWindowFocusChanged;
  end;
end;

class function TMyAndroidApplicationGlue.GetActivityCallbacks: PANativeActivityCallbacks;
begin
  Result := Current.NativeActivity.callbacks;
end;
{$ENDIF}
{$ENDIF}

{ TFrameView }

procedure TFrameView.AfterDialogKey(var Key: Word; Shift: TShiftState);
begin
  // 如果按下了返回键，且允许取消对话框，则关闭对话框
  if DoCanKeyFinish and (Key in [vkEscape, vkHardwareBack]) then begin
    Key := 0;
    Finish;
  end else
    inherited AfterDialogKey(Key, Shift);
end;

procedure TFrameView.AnimatePlay(Ani: TFrameAniType; IsIn, SwitchFlag: Boolean;
  AEvent: TNotifyEventA);

  // 淡入淡出
  procedure DoFadeInOut();
  var
    NewValue: Single;
  begin
    if IsIn then begin
      if SwitchFlag then begin
        Self.Opacity := 1;
        TFrameAnimator.DelayExecute(Self, AEvent, 0.2);
      end else begin
        NewValue := 1;
        TFrameAnimator.AnimateFloat(Self, 'Opacity', NewValue, AEvent, 0.2);
      end;
    end else begin
      if FinishIsFreeApp then begin
        if Assigned(AEvent) then
          AEvent(Self);
        Exit;
      end;
      if SwitchFlag then begin
        NewValue := 0;
        TFrameAnimator.AnimateFloat(Self, 'Opacity', NewValue, AEvent, 0.2);
      end else begin
        TFrameAnimator.DelayExecute(Self, AEvent, 0.2);
      end;
    end;
  end;

  // 移入移出, 右边进入
  procedure DoMoveInOut();
  var
    NewValue: Single;
  begin
    if IsIn then begin
      Self.Opacity := 1;
      if not SwitchFlag then begin
        Self.Position.X := Self.Width - 1;  //目标frame新显示
        NewValue := 0;
      end else begin
        Self.Position.X := -Self.Width + 1;  //目标frame返回显示
        NewValue := 0;
      end;
    end else begin
      if not SwitchFlag then
        NewValue := -Self.Width + 1 //旧的frame向右返回
      else
        NewValue := Self.Width - 1; //旧的frame向左隐藏
      if FinishIsFreeApp then begin
        if Assigned(AEvent) then
          AEvent(Self);
        Exit;
      end;
    end;
    TFrameAnimator.AnimateFloat(Self, 'Position.X', NewValue, AEvent);
  end;

  // 顶部移入移出, 右边进入
  procedure DoTopMoveInOut();
  var
    LForm: TCustomForm;
    Y: Single;
  begin
    if IsIn then begin
      Self.Opacity := 1;
      if not SwitchFlag then begin
        Self.Position.Y := - Self.Height;
        Y := 0;
        LForm := Self.ParentForm;
        if Assigned(LForm) then
          Y := LForm.Padding.Top;
        TFrameAnimator.AnimateFloat(Self, 'Position.Y', Y, AEvent);
      end else if Assigned(AEvent) then
        TFrameAnimator.DelayExecute(Self, AEvent, 0.2);
    end else begin
      if FinishIsFreeApp then begin
        if Assigned(AEvent) then
          AEvent(Self);
        Exit;
      end;
      if SwitchFlag then
        TFrameAnimator.AnimateFloat(Self, 'Position.Y', - Self.Height, AEvent, 0.1)
      else if Assigned(AEvent) then
        TFrameAnimator.DelayExecute(Self, AEvent, 0.65);
    end;
  end;

  // 底部移入移出, 右边进入
  procedure DoBottomMoveInOut();
  var
    LForm: TCustomForm;
    Y: Single;
  begin
    if IsIn then begin
      Self.Opacity := 1;
      if not SwitchFlag then begin
        Self.Position.Y := Self.Height;
        Y := 0;
        LForm := Self.ParentForm;
        if Assigned(LForm) then
          Y := LForm.Padding.Top;
        TFrameAnimator.AnimateFloat(Self, 'Position.Y', Y, AEvent);
      end else if Assigned(AEvent) then
        TFrameAnimator.DelayExecute(Self, AEvent, 0.2);
    end else begin
      if FinishIsFreeApp then begin
        if Assigned(AEvent) then
          AEvent(Self);
        Exit;
      end;
      if SwitchFlag then
        TFrameAnimator.AnimateFloat(Self, 'Position.Y', Self.Height, AEvent, 0.1)
      else if Assigned(AEvent) then
        TFrameAnimator.DelayExecute(Self, AEvent, 0.65);
    end;
  end;

begin
  if not Assigned(Self) or (csDestroying in ComponentState) then
    Exit;
  try
    case Ani of
      TFrameAniType.DefaultAni:
        if not (DefaultAnimate in [TFrameAniType.None, TFrameAniType.DefaultAni]) then
          AnimatePlay(DefaultAnimate, IsIn, SwitchFlag, AEvent)
        else if Assigned(AEvent) then begin
          if IsIn then
            Self.Opacity := 1
          else
            Self.Opacity := 0;
          AEvent(Self);
        end;
      TFrameAniType.FadeInOut:
        DoFadeInOut;
      TFrameAniType.MoveInOut:
        DoMoveInOut;
      TFrameAniType.TopMoveInOut:
        DoTopMoveInOut;
      TFrameAniType.BottomMoveInOut:
        DoBottomMoveInOut;
    else
      begin
        // 无动画效果
        if Assigned(AEvent) then
          AEvent(Self);
        if IsIn then
          Opacity := 1
        else
          Opacity := 0;
      end;
    end;
  except
  end;
end;

class procedure TFrameView.AppTerminate;
begin
  try
    {$IFDEF POSIX}
      {$IFDEF DEBUG}
      Application.Terminate;
      {$ELSE}
      Kill(0, SIGKILL);
      {$ENDIF}
    {$ELSE}
    Application.Terminate;
    {$ENDIF}
  except
  end;
end;

function TFrameView.CheckChildern: Boolean;
var
  I: Integer;
begin
  if (Parent is TForm) then begin
    Result := True;
    if Parent.ChildrenCount >= 2 then begin
      for I := 0 to Parent.ChildrenCount - 1 do begin
        if (Parent.Children[I] <> Self) and (Parent.Children[I] is FMX.Forms.TFrame) then begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end else
    Result := False;
end;

function TFrameView.CheckFree: Boolean;
var
  LForm: TCustomForm;
  LFrame: TFrameView;
begin
  Result := False;
  if Assigned(Parent) then begin
    if (Parent is TForm) and DoCanFree then begin
      {$IFDEF POSIX}
        {$IFDEF DEBUG}
        (Parent as TForm).Close;
        {$ELSE}
        Kill(0, SIGKILL);
        {$ENDIF}
      {$ELSE}
      (Parent as TForm).Close;
      {$ENDIF}
      Result := True;
      Exit;
    end;
    LForm := ParentForm;
    {$IFDEF MSWINDOWS}
    if Assigned(LForm) then
      LForm.ReleaseCapture;
    {$ENDIF}
    if TFrameView.GetFormActiveFrame(LForm, LFrame) and (LFrame = Self) then
      TFrameView.SetFormActiveFrame(LForm, nil);
    {$IFDEF AUTOREFCOUNT}
    Parent := nil;
    if Assigned(Owner) then
      Owner.RemoveComponent(Self);
    Self.DisposeOf;
    {$ELSE}
    Self.Free;
    {$ENDIF}
    {$IFDEF ANDROID}
    if (not Assigned(Screen.FocusControl)) and (Assigned(LForm)) then
      LForm.SetFocus;
    {$ENDIF}
  end;
end;

procedure TFrameView.Close;
begin
  Close(TFrameAniType.DefaultAni);
end;

procedure TFrameView.Close(Ani: TFrameAniType);
begin
  // 如果正在显示中，设置需要Finish标识
  if FShowing then begin
    FNeedFinish := True;
    Exit;
  end;
  // 动画执行中， 设置需要关闭的标识
  FWaitDialog := nil;
  if FAnimateing then
    FNeedFree := True
  else begin
    FAnimateing := True;
    AnimatePlay(Ani, False, True, OnFinishOrClose);
    FAnimateing := False;
  end;
end;

class function TFrameView.CreateFrame(Parent: TFmxObject;
  Params: TFrameParams): TFrameView;

  {$IFDEF ANDROID}
  procedure DoUpdateParentFormState(Parent: TFmxObject);
  begin
    // 设置了状态条颜色，并且状态条高度大于0时，将父级Form的Padding.Top设为状态条高度
    if (FDefaultStatusColor <> 0) and (TView.GetStatusHeight > 0) then begin
      while Parent <> nil do begin
        if (Parent is TCommonCustomForm) then begin
          TCommonCustomForm(Parent).Padding.Top :=
            TCommonCustomForm(Parent).Padding.Top + TView.GetStatusHeight;
          //TCommonCustomForm(Parent).Padding.Bottom :=
          //  TCommonCustomForm(Parent).Padding.Bottom + TView.GetNavigationBarHeight;
          Break;
        end;
        Parent := Parent.Parent;
      end;
    end;
  end;
  {$ENDIF}

var
  Dlg: IDialog;
begin
  Result := nil;
  if (Assigned(Parent)) then begin
    try
      {$IFDEF ANDROID}
      // 检测是否是第一次创建 Frame
      if FFirstCreateFrame then begin
        DoUpdateParentFormState(Parent);
        FFirstCreateFrame := False;
      end;
      {$ENDIF}

      // 检测是否是存在Dialog
      if Parent is TControl then begin
        Dlg := TDialog.GetDialog(Parent as TControl);
        if Assigned(Dlg) then begin
          Parent := Dlg.View;
          while Parent <> nil do begin
            if (Parent is TFrameView) or (Parent is TCustomForm) then begin
              ShowFrame(Parent, Params);
              Break;
            end;
            Parent := Parent.Parent;
          end;
          Dlg.Dismiss;
          Exit;
        end;
      end;

      Result := Create(Parent);
      Result.Name := '';
      Result.TagObject := Params;
      Result.Parent := Parent;
      Result.Align := TAlignLayout.Client;
      Result.FLastView := nil;
    except
      if Assigned(Params) then
        Params.Free;
      raise;
    end;
  end else if Assigned(Params) then
    Params.Free;
end;

constructor TFrameView.Create(AOwner: TComponent);
begin
  try
    inherited Create(AOwner);
  except
    Width := 200;
    Height := 400;
  end;
  FDefaultAni := TFrameAniType(-1);

  FBackColor := FDefaultBackColor;
  FStatusColor := FDefaultStatusColor;
  FStatusTransparent := FDefaultStatusTransparent;
  FStatusLight := FDefaultStatusLight;

  FUseDefaultBackColor := True;
  FUseDefaultStatusColor := True;
  FUseDefaultStatusLight := True;
  FUseDefaultStatusTransparent := True;

  FNeedDoCreate := True;
end;

class constructor TFrameView.Create;
begin
  FActiveFrames := TDictionary<TCommonCustomForm, TFrameView>.Create;

  {$IF CompilerVersion >= 31}
  TMessageManager.DefaultManager.SubscribeToMessage(TFormActivateMessage, TFrameView.DoActivateMessage);
  TMessageManager.DefaultManager.SubscribeToMessage(TFormDeactivateMessage, TFrameView.DoActivateMessage);
  {$ENDIF}

  {$IF Defined(ANDROID) or Defined(IOS)}
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, TFrameView.DoActivateMessage);
  {$ENDIF}

  {$IFDEF ANDROID}
  {$IF CompilerVersion >= 33}
  TMessageManager.DefaultManager.SubscribeToMessage(TPermissionsRequestResultMessage, TFrameView.DoActivateMessage);

  // 感谢 谭钦
  // 增加一个键盘事件的监听 这是因为fmx.jar中的BUG引发的，原本应该是不需要这个监听的
  FKSListener := TKSCListener.Create;
  MainActivity.getVirtualKeyboard.addOnKeyboardStateChangedListener(FKSListener);
  {$ENDIF}

  {$IF CompilerVersion >= 34}
  TMyAndroidApplicationGlue.BindCallbacks;
  {$ENDIF}
  {$ENDIF}
end;

class function TFrameView.CreateFrame(Parent: TFmxObject;
  const Title: string): TFrameView;
begin
  Result := CreateFrame(Parent, nil);
  if Result <> nil then
    Result.Title := Title;
end;

function TFrameView.MakeFrame(FrameClass: TFrameViewClass): TFrameView;
begin
  if FAnimateing then
    Result := nil
  else begin
    Result := FrameClass.Create(Parent);
    Result.Name := '';
    Result.Parent := Parent;
    Result.Align := TAlignLayout.Client;
    Result.FLastView := Self;
    FNextView := Result;
  end;
end;

procedure TFrameView.OnFinishOrClose(Sender: TObject);
begin
  if not Assigned(Self) then
    Exit;
  FNeedFinish := False;
  if FNeedHide then
    InternalHide;
  if CheckFree then Exit;
end;

procedure TFrameView.Paint;
var
  R: TRectF;
begin
  inherited Paint;
  if (BackColor and $FF000000 > 0) then begin
    R := LocalRect;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := BackColor;
    Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  end;
end;

procedure TFrameView.Pause(AResume: Boolean);
var
  [Weak] LFrame: TFrameView;
begin
  if not DoCanPause then
    Exit;
  if not FResumed then
    Exit;
  FResumed := False;

  if AResume then begin
    LFrame := ActiveFrame;
    // Self not Active
    if Assigned(LFrame) and (LFrame <> Self) and (LFrame.FLastView = Self) then
      LFrame.FLastView := FLastView;
  end
  else
    LFrame := nil;

  DoPause;

  if AResume then begin
    if LFrame = Self then
      if Assigned(FLastView) and not FLastView.IsDestroy then
        FLastView.Show(TFrameAniType.None, nil);
  end;
end;

procedure TFrameView.RefreshStatusBar;
begin
  TFrameView.UpdateStatusBar(ParentForm, StatusColor, StatusTransparent, StatusLight);
end;

class procedure TFrameView.UpdateStatusBar(const AForm: TCommonCustomForm; const AColor: TAlphaColor;
  const ATransparent: Boolean; const ALight: Boolean);

  {$IFDEF IOS}
  procedure ExecuteIOS;
  begin
    if not Assigned(AForm) then
      Exit;
    if AForm is TCustomForm then
      TCustomForm(AForm).Fill.Color := AColor;
  end;
  {$ENDIF}

  {$IFDEF ANDROID}
  procedure ExecuteAndroid;
  {$IF CompilerVersion >= 33}
  var
    wnd: JWindow;
  {$ENDIF}
  begin
    if TView.GetStatusHeight > 0 then begin
      if not Assigned(AForm) then
        Exit;
      if AForm is TCustomForm then
        TCustomForm(AForm).Fill.Color := AColor;

      {$IF CompilerVersion >= 33} // Delphi 10.3 及之后的版本
      if not FStatusTransparentNewMethod then
        Exit;

      wnd := TAndroidHelper.Activity.getWindow;
      if (not Assigned(wnd)) then Exit;
      CallInUiThread(
        procedure
        begin
          if TJBuild_VERSION.JavaClass.SDK_INT >= 21 then begin
            FSystemUiVisibility := wnd.getDecorView.getSystemUiVisibility;

            // 是否为系统 view 预留空间
            //wnd.getDecorView().setFitsSystemWindows(True);
            // 需要设置这个 flag 才能调用 setStatusBarColor 来设置状态栏颜色
            wnd.addFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
            if ATransparent then begin // 透明
              // 设置颜色
              wnd.setStatusBarColor(TJcolor.JavaClass.TRANSPARENT);
              // 取消设置透明状态栏,使 ContentView 内容不再覆盖状态栏
              wnd.clearFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS);
              with TJView.JavaClass do
                FSystemUiVisibility := FSystemUiVisibility or SYSTEM_UI_FLAG_LAYOUT_STABLE or SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN;
              // 亮色状态栏
              if (TJBuild_VERSION.JavaClass.SDK_INT >= 23) then
                if ALight then
                  FSystemUiVisibility := FSystemUiVisibility or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR
                else
                  FSystemUiVisibility := FSystemUiVisibility and not TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR;
              wnd.getDecorView().setSystemUiVisibility(FSystemUiVisibility);
            end
            else begin // 半透明
              // 设置颜色
              wnd.setStatusBarColor(AlphaColorToJColor(AColor));
              wnd.addFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS);
              with TJView.JavaClass do
                FSystemUiVisibility := FSystemUiVisibility or SYSTEM_UI_FLAG_LAYOUT_STABLE or SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN;
              wnd.getDecorView().setSystemUiVisibility(FSystemUiVisibility);
            end;
          end
          else begin
            // 是否为系统 view 预留空间
            wnd.getDecorView().setFitsSystemWindows(True);
            wnd.addFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS);
          end;
        end
      );
      {$ENDIF}
    end;
  end;
  {$ENDIF}

begin
  {$IFDEF IOS}
  ExecuteIOS;
  {$ENDIF}
  {$IFDEF ANDROID}
  ExecuteAndroid;
  {$ENDIF}
end;

class procedure TFrameView.UpdateStatusBar;
begin
{$IFDEF ANDROID}
  {$IF CompilerVersion >= 33} // Delphi 10.3 及之后的版本
  if (not FStatusTransparentNewMethod) or (FSystemUiVisibility = -1) then
    Exit;
  CallInUiThread(procedure
  var
    wnd: JWindow;
  begin
    wnd := TAndroidHelper.Activity.getWindow;
    if (not Assigned(wnd)) then Exit;
    wnd.getDecorView().requestApplyInsets();
    wnd.getDecorView().setSystemUiVisibility(FSystemUiVisibility);
    wnd.getDecorView().requestApplyInsets();
  end);
  {$ENDIF}
{$ENDIF}
end;

procedure TFrameView.Resume;
var
  [Weak] LFrame: TFrameView;
begin
  if not DoCanPause then
    Exit;
  if FResumed then
    Exit;
  FResumed := True;

  LFrame := ActiveFrame;
  // Back from nextview
  if (not Assigned(LFrame)) and Assigned(FLastView) then
    FLastView := nil
  else if (FLastView <> LFrame) and Assigned(LFrame) and (LFrame <> Self) and (LFrame.FLastView <> Self) then
    FLastView := LFrame;

  if LFrame <> Self then begin
    if Assigned(LFrame) and (not LFrame.IsDestroy) then
      LFrame.Pause;
    TFrameView.SetFormActiveFrame(ParentForm, Self);
  end;
  DoResume;
end;

procedure TFrameView.DelayExecute(ADelay: Single; AExecute: TNotifyEventA);
begin
  if not Assigned(AExecute) then
    Exit;
  TFrameAnimator.DelayExecute(Self, AExecute, ADelay);
end;

class destructor TFrameView.Destroy;
begin
  {$IFDEF ANDROID}
  {$IF CompilerVersion >= 33}
  TMessageManager.DefaultManager.Unsubscribe(TPermissionsRequestResultMessage, TFrameView.DoActivateMessage);

  MainActivity.getVirtualKeyboard.removeOnKeyboardStateChangedListener(FKSListener);
  FreeAndNil(FKSListener);
  {$ENDIF}
  {$ENDIF}

  {$IF Defined(ANDROID) or Defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, TFrameView.DoActivateMessage);
  {$ENDIF}

  {$IF CompilerVersion >= 31}
  TMessageManager.DefaultManager.Unsubscribe(TFormActivateMessage, TFrameView.DoActivateMessage);
  TMessageManager.DefaultManager.Unsubscribe(TFormDeactivateMessage, TFrameView.DoActivateMessage);
  {$ENDIF}

  FreeAndNil(FActiveFrames);
end;

destructor TFrameView.Destroy;
var
  Obj: TObject;
  LFrame: TFrameView;
begin
  if Assigned(ParentForm) and TFrameView.GetFormActiveFrame(ParentForm, LFrame) and (LFrame = Self) then
    TFrameView.SetFormActiveFrame(ParentForm, nil);
  if Assigned(Screen.ActiveForm) and TFrameView.GetFormActiveFrame(Screen.ActiveForm, LFrame) and (LFrame = Self) then
    TFrameView.SetFormActiveFrame(Screen.ActiveForm, nil);

  DoFree();
  FWaitDialog := nil;
  Obj := TagObject;
  if Assigned(Obj) then
    FreeAndNil(Obj);
  if Assigned(FNextView) then
    FNextView.FLastView := nil;
  FLastView := nil;
  FNextView := nil;
  FreeAndNil(FParams);
  FreeAndNil(FPrivateState);
  inherited;
end;

class procedure TFrameView.DoActivateMessage(const Sender: TObject;
  const M: TMessage);

  procedure DealForm(AForm: TCommonCustomForm; AActivate: Boolean);
  var
    LFrame: TFrameView;
  begin
    if not Assigned(AForm) then
      Exit;
    if not TFrameView.GetFormActiveFrame(AForm, LFrame) then
      Exit;
    if (not Assigned(LFrame)) or LFrame.IsDestroy then
      Exit;

    if AActivate then
      LFrame.Resume
    else
      LFrame.Pause;
  end;

  procedure DealFrame(AForm: TCommonCustomForm);
  var
    LFrame: TFrameView;
  begin
    if not Assigned(AForm) then
      Exit;
    if not TFrameView.GetFormActiveFrame(AForm, LFrame) then
      Exit;

    LFrame.RefreshStatusBar;
  end;

begin
{$IF CompilerVersion >= 31}
  if M is TFormActivateMessage then
    DealForm(TFormActivateMessage(M).Value, True)
  else if M is TFormDeactivateMessage then
    DealForm(TFormActivateMessage(M).Value, False)
  else
{$ENDIF}
{$IF Defined(ANDROID) and (CompilerVersion >= 33)}
  if M is TPermissionsRequestResultMessage then begin
    DealFrame(Screen.ActiveForm);
    TThread.CreateAnonymousThread(procedure begin
      TFrameView.UpdateStatusBar;
    end).Start;
  end
  else
{$ENDIF}
  if M is TApplicationEventMessage then
    case TApplicationEventMessage(M).Value.Event of
      TApplicationEvent.BecameActive: begin
        {$IFDEF ANDROID}
        // 感谢谭钦的u_Immerse.pas
        TFrameView.UpdateStatusBar;
        {$ENDIF}
        DealForm(Screen.ActiveForm, True);
        {$IFDEF ANDROID}
        // 临时的方案，暂未深究原因
        TThread.CreateAnonymousThread(procedure begin
          Sleep(100);
          TFrameView.UpdateStatusBar;
          Sleep(100);
          TFrameView.UpdateStatusBar;
          Sleep(100);
          TFrameView.UpdateStatusBar;
        end).Start;
        {$ENDIF}
      end;
      TApplicationEvent.WillBecomeInactive:
        DealForm(Screen.ActiveForm, False);
    end;
end;

function TFrameView.DoCanFinish: Boolean;
begin
  Result := True;
end;

function TFrameView.DoCanKeyFinish: Boolean;
begin
  Result := Assigned(Self) and Visible and Enabled;
end;

function TFrameView.DoCanFree: Boolean;
begin
  Result := not Assigned(Parent.Parent) and CheckChildern();
end;

function TFrameView.DoCanPause: Boolean;
var
  [Weak] V: TFmxObject;
begin
  Result := False;
  if not Assigned(Self) then
    Exit;
  if Owner is TFrameView then
    Exit;
  V := Parent;
  while Assigned(V) do begin
    if V is TFrameView then
      Exit;
    if V is TDialogView then
      Exit;
    if V.ClassName = 'TListViewEx' then
      Exit;
    V := V.Parent;
  end;
  Result := True;
end;

procedure TFrameView.DoCreate;
begin
end;

procedure TFrameView.DoFinish;
begin
  Pause(True);
  if Assigned(FOnFinish) then begin
    FOnFinish(Self);
    FOnFinish := nil;
  end;
end;

procedure TFrameView.DoFree;
begin
  if Assigned(FOnFree) then
    FOnFree(Self);
end;

procedure TFrameView.DoHide;
begin
  Pause(True);
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TFrameView.DoPause;
begin

end;

procedure TFrameView.DoReStart;
begin
  if Assigned(FOnReStart) then
    FOnReStart(Self);
  Resume;
end;

procedure TFrameView.DoResume;
begin
  RefreshStatusBar;
  TThread.CreateAnonymousThread(procedure begin
    TFrameView.UpdateStatusBar;
  end).Start;
end;

procedure TFrameView.DoShow;
begin
  FToastManager := GetToastManager;
  if Assigned(FOnShow) then
    FOnShow(Self);
  Resume;
end;

procedure TFrameView.DoShown;
begin
  if Assigned(FOnShown) then
    FOnShown(Self);
end;

procedure TFrameView.Finish(Ani: TFrameAniType);
begin
  if FShowing then begin
    FNeedFinish := True;
    Exit;
  end;
  DoFinish();
  if Assigned(FNextView) then begin
    FNextView.FLastView := FLastView;
    FLastView := nil;
    FNextView := nil;
  end else if Assigned(FLastView) then begin
    FLastView.InternalShow(False, nil, Ani, True);
    FLastView.FNextView := nil;
    FLastView := nil;
  end;
  Close(Ani);
end;

procedure TFrameView.Finish;
begin
  if not DoCanFinish then
    Exit;
  if Ord(FDefaultAni) <> -1 then
    Finish(FDefaultAni)
  else
    Finish(TFrameAniType.DefaultAni);
end;

class function TFrameView.GetFormActiveFrame(AForm: TCommonCustomForm; var AFrame: TFrameView): Boolean;
begin
  Result := Assigned(AForm) and FActiveFrames.TryGetValue(AForm, AFrame) and Assigned(AFrame);
end;

function TFrameView.GetActiveFrame: TFrameView;
begin
  TFrameView.GetFormActiveFrame(ParentForm, Result);
end;

function TFrameView.GetData: TValue;
begin
  if (FParams = nil) or (not FParams.ContainsKey(CS_Data)) then
    Result := nil
  else
    Result := FParams.Items[CS_Data];
end;

function TFrameView.GetDataAsPointer: Pointer;
var
  V: TValue;
begin
  V := Data;
  if V.IsEmpty then
    Result := nil
  else
    Result := V.AsVarRec.VPointer;
end;

function TFrameView.GetDataString: string;
begin
  if (FParams = nil) or (not FParams.ContainsKey(CS_DataStr)) then
    Result := ''
  else
    Result := FParams.Items[CS_DataStr].ToString;
end;

function TFrameView.GetIsDestroy: Boolean;
begin
  Result := (not Assigned(Self)) or (csDestroying in ComponentState);
end;

function TFrameView.GetIsWaitDismiss: Boolean;
begin
  Result := IsDestroy or (not Assigned(FWaitDialog)) or FWaitDialog.IsDismiss;
end;

function TFrameView.GetParams: TFrameParams;
begin
  if FParams = nil then begin
    if (TagObject <> nil) and (TagObject is TFrameParams) then begin
      FParams := TagObject as TFrameParams;
      TagObject := nil;
    end else
      FParams := TFrameParams.Create(9);
  end;
  Result := FParams;
end;

function TFrameView.GetParentForm: TCustomForm;
begin
  Result := UI.Base.GetParentForm(Self);
end;

function TFrameView.GetPreferences: TFrameState;
begin
  if not Assigned(FPrivateState) then begin
    FPrivateState := TFrameState.Create(Self, False);
    FPrivateState.Load;
  end;
  Result := FPrivateState;
end;

function TFrameView.GetSharedPreferences: TFrameState;
begin
  Result := FPublicState;
end;

function TFrameView.GetBackColor: TAlphaColor;
begin
  if FUseDefaultBackColor then
    Result := FDefaultBackColor
  else
    Result := FBackColor;
end;

function TFrameView.GetStatusColor: TAlphaColor;
begin
  if FUseDefaultStatusColor then
    Result := FDefaultStatusColor
  else
    Result := FStatusColor;
end;

function TFrameView.GetStatusLight: Boolean;
begin
  if FUseDefaultStatusLight then
    Result := FDefaultStatusLight
  else
    Result := FStatusLight;
end;

function TFrameView.GetStatusTransparent: Boolean;
begin
  if FUseDefaultStatusTransparent then
    Result := FDefaultStatusTransparent
  else
    Result := FStatusTransparent;
end;

function TFrameView.GetTitle: string;
begin
  if (FParams = nil) or (not FParams.ContainsKey(CS_Title)) then
    Result := ''
  else
    Result := FParams.Items[CS_Title].ToString;
end;

function TFrameView.GetToastManager: TToastManager;
var
  LForm: TCustomForm;
  I: Integer;
begin
  Result := nil;
  LForm := GetParentForm;
  if not Assigned(LForm) then
    Exit;
  for I := 0 to LForm.ComponentCount - 1 do
    if LForm.Components[I] is TToastManager then begin
      Result := TToastManager(LForm.Components[I]);
      Exit;
    end;
end;

procedure TFrameView.Hide;
begin
  if FHideing then
    Exit;
  Hide(TFrameAniType.DefaultAni);
end;

procedure TFrameView.Hide(Ani: TFrameAniType; SwitchFlag: Boolean);
begin
  if FAnimateing then
    FNeedHide := True
  else begin
    FAnimateing := True;
    AnimatePlay(Ani, False, SwitchFlag,
      procedure (Sender: TObject) begin
        if not FShowing then begin
          InternalHide;
          if FNeedFree then
            OnFinishOrClose(Sender);
        end;
        FAnimateing := False;
      end
    );
  end;
end;

procedure TFrameView.HideWaitDialog;
begin
  TThread.Synchronize(TThread.CurrentThread, procedure begin
    if not IsWaitDismiss then begin
      FWaitDialog.Dismiss;
      FWaitDialog := nil;
    end;
  end);
end;

procedure TFrameView.Hint(const Msg: string);
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then begin
    if Assigned(FToastManager) then
      FToastManager.Toast(Msg)
    else
      Toast(Msg);
  end
  else
    TThread.Synchronize(TThread.CurrentThread, procedure begin
      if Assigned(FToastManager) then
        FToastManager.Toast(Msg)
      else
        Toast(Msg);
    end);
end;

procedure TFrameView.Hint(const AFormat: string; const Args: array of const);
begin
  Hint(Format(AFormat, Args));
end;

procedure TFrameView.Hint(const Msg: Double);
begin
  Hint(FloatToStr(Msg));
end;

procedure TFrameView.Hint(const Msg: Int64);
begin
  Hint(IntToStr(Msg));
end;

procedure TFrameView.InternalHide;
begin
  FHideing := True;
  DoHide;
  Visible := False;
  FHideing := False;
  FNeedHide := False;
end;

procedure TFrameView.InternalShow(TriggerOnShow: Boolean;
  AOnFinish: TNotifyEventA; Ani: TFrameAniType; SwitchFlag: Boolean);
begin
  if FShowing then Exit;

  FShowing := True;
  if TriggerOnShow and (not SwitchFlag) then
    FDefaultAni := Ani;
  if Title <> '' then begin
    Application.Title := Title;
    if Assigned(Parent) and (Parent is TCustomForm) then
      TCustomForm(Parent).Caption := Title;
  end;
  if TriggerOnShow then
    DoShow()
  else
    DoReStart();
  {$IFDEF ANDROID}
  {$IF CompilerVersion < 32}
  if (not Assigned(Screen.FocusControl)) and (Assigned(ParentForm)) then
    ParentForm.SetFocus;
  {$IFEND}
  {$ENDIF}
  Opacity := 0;
  FHideing := True;
  Visible := True;
  // make sure this is front
  BringToFront;
  AnimatePlay(Ani, True, SwitchFlag,
    procedure (Sender: TObject) begin
      FShowing := False;
      if Assigned(AOnFinish) then
        AOnFinish(Sender);
      DoShown;
    end
  );
  FHideing := False;
  FNeedFree := False;
  FNeedHide := False;
  if FNeedFinish then begin
    FShowing := False;
    FNeedFinish := False;
    Finish;
  end;
end;

function TFrameView.FinishIsFreeApp: Boolean;
begin
  Result := Assigned(Parent) and (not Assigned(Parent.Parent)) and
    (Parent is TForm) and CheckChildern();
end;

procedure TFrameView.SetBackColor(const Value: TAlphaColor);
begin
  if FBackColor <> Value then begin
    FBackColor := Value;
    FUseDefaultBackColor := False;
    Repaint;
  end;
end;

procedure TFrameView.SetData(const Value: TValue);
begin
  if Params.ContainsKey(CS_Data) then
    Params.Items[CS_Data] := Value
  else
    Params.Add(CS_Data, Value);
end;

procedure TFrameView.SetDataString(const Value: string);
begin
  if Params.ContainsKey(CS_DataStr) then
    Params.Items[CS_DataStr] := Value
  else if Value <> '' then
    Params.Add(CS_DataStr, Value);
end;

class procedure TFrameView.SetDefaultBackColor(const Value: TAlphaColor);
begin
  FDefaultBackColor := Value;
end;

class procedure TFrameView.SetDefaultStatusColor(const Value: TAlphaColor);
begin
  if FDefaultStatusColor <> Value then begin
    FDefaultStatusColor := Value;
    {$IF Defined(ANDROID) or Defined(IOS)}
    // 在移动平台时，设置状态条颜色时，如果背景颜色为透明，且状态条高度>0时，
    // 将背景颜色设为白色
    if (Value and $FF000000 > 0) and (FDefaultBackColor = 0){$IFDEF ANDROID} and (TView.GetStatusHeight > 0){$ENDIF} then
      FDefaultBackColor := $fff1f2f3;
    {$ENDIF}
  end;
end;

class procedure TFrameView.SetDefaultStatusLight(const Value: Boolean);
begin
  if FDefaultStatusLight <> Value then
    FDefaultStatusLight := Value;
end;

class procedure TFrameView.SetDefaultStatusTransparent(const Value: Boolean);
begin
  if FDefaultStatusTransparent <> Value then
    FDefaultStatusTransparent := Value;
end;

class procedure TFrameView.SetStatusTransparentNewMethod(const Value: Boolean);
begin
  if FStatusTransparentNewMethod <> Value then
    FStatusTransparentNewMethod := Value;
end;

class procedure TFrameView.SetFormActiveFrame(AForm: TCommonCustomForm; AFrame: TFrameView);
begin
  if not Assigned(AForm) then
    Exit;
  FActiveFrames.AddOrSetValue(AForm, AFrame);
end;

procedure TFrameView.SetParams(const Value: TFrameParams);
begin
  if Assigned(FParams) then
    FParams.Free;
  FParams := Value;
end;

procedure TFrameView.SetParent(const Value: TFmxObject);
begin
  {$IF CompilerVersion >= 34}
  if Value = nil then
    Self.SetRoot(nil); // fix release error in 10.4
  {$ENDIF}
  inherited;
  if FNeedDoCreate and Assigned(Parent) then begin
    FNeedDoCreate := False;
    if FUseDefaultBackColor and Assigned(TDialog.GetDialog(Self)) then
      BackColor := 0;  // 作为Dialog的子View时，不使用默认背景色
    DoCreate();
  end;
end;

procedure TFrameView.SetStatusColor(const Value: TAlphaColor);
begin
  if FStatusColor <> Value then begin
    FUseDefaultStatusColor := False;
    FStatusColor := Value;
    RefreshStatusBar;
  end;
end;

procedure TFrameView.SetStatusLight(const Value: Boolean);
begin
  if FStatusLight <> Value then begin
    FUseDefaultStatusLight := False;
    FStatusLight := Value;
    RefreshStatusBar;
  end;
end;

procedure TFrameView.SetStatusTransparent(const Value: Boolean);
begin
  if FStatusTransparent <> Value then begin
    FUseDefaultStatusTransparent := False;
    FStatusTransparent := Value;
    RefreshStatusBar;
  end;
end;

procedure TFrameView.SetTitle(const Value: string);
begin
  if Params.ContainsKey(CS_Title) then
    Params.Items[CS_Title] := Value
  else if Value <> '' then
    Params.Add(CS_Title, Value);
end;

procedure TFrameView.Show(Ani: TFrameAniType; AOnFinish: TNotifyEventA;
  SwitchFlag, TriggerOnShow: Boolean);
begin
  InternalShow(TriggerOnShow, AOnFinish, Ani, SwitchFlag);
end;

procedure TFrameView.Show;
begin
  if FHideing then
    Exit;
  Show(TFrameAniType.DefaultAni, nil);
end;

class function TFrameView.ShowFrame(Parent: TFmxObject;
  const Title: string; Ani: TFrameAniType; SwitchFlag: Boolean): TFrameView;
begin
  Result := CreateFrame(Parent, Title);
  if Result <> nil then
    Result.Show(Ani, nil, SwitchFlag);
end;

procedure TFrameView.ShowWaitDialog(const AMsg: string;
  OnDismissListener: TOnDialogListener; ACancelable: Boolean);
begin
  ShowWaitDialog(AMsg, ACancelable);
  if Assigned(FWaitDialog) then
    FWaitDialog.OnDismissListener := OnDismissListener;
end;

procedure TFrameView.ShowWaitDialog(const AMsg: string;
  OnDismissListener: TOnDialogListenerA; ACancelable: Boolean);
begin
  ShowWaitDialog(AMsg, ACancelable);
  if Assigned(FWaitDialog) then
    FWaitDialog.OnDismissListenerA := OnDismissListener;
end;

procedure TFrameView.ShowWaitDialog(const AMsg: string; ACancelable: Boolean);
begin
  TThread.Synchronize(TThread.CurrentThread,
  procedure
  begin
    if IsWaitDismiss then begin
      FWaitDialog := nil;
      FWaitDialog := TProgressDialog.Create(Self);
    end;
    FWaitDialog.Cancelable := ACancelable;
    if not Assigned(FWaitDialog.RootView) then
      FWaitDialog.InitView(AMsg)
    else
      FWaitDialog.Message := AMsg;
    TDialog(FWaitDialog).Show();
  end);
end;

function TFrameView.StartFrame(FrameClass: TFrameViewClass;
  const Title: string; Ani: TFrameAniType): TFrameView;
begin
  Result := MakeFrame(FrameClass);
  if Assigned(Result) then begin
    Result.Title := Title;
    Hide(Ani);
    Result.Show(Ani, nil);
  end;
end;

function TFrameView.StartFrame(FrameClass: TFrameViewClass; const Title: string;
  const Data: TValue; Ani: TFrameAniType): TFrameView;
begin
  Result := MakeFrame(FrameClass);
  if Assigned(Result) then begin
    Result.Title := Title;
    Result.Data := Data;
    Hide(Ani);
    Result.Show(Ani, nil);
  end;
end;

function TFrameView.StartFrame(FrameClass: TFrameViewClass; const Title,
  DataString: string; Ani: TFrameAniType): TFrameView;
begin
  Result := MakeFrame(FrameClass);
  if Assigned(Result) then begin
    Result.Title := Title;
    Result.DataString := DataString;
    Hide(Ani);
    Result.Show(Ani, nil);
  end;
end;

function TFrameView.StreamToString(SrcStream: TStream; const CharSet: string): string;
var
  LReader: TStringStream;
begin
  if (CharSet <> '') and (string.CompareText(CharSet, 'utf-8') <> 0) then  // do not translate
    LReader := TStringStream.Create('', System.SysUtils.TEncoding.GetEncoding(CharSet), True)
  else
    LReader := TStringStream.Create('', System.SysUtils.TEncoding.UTF8, False);
  try
    LReader.CopyFrom(SrcStream, 0);
    Result := LReader.DataString;
  finally
    LReader.Free;
  end;
end;

procedure TFrameView.UpdateWaitDialog(const AMsg: string);
begin
  TThread.Synchronize(TThread.CurrentThread,
  procedure
  begin
    if IsWaitDismiss then
      Exit;
    if Assigned(FWaitDialog.RootView) then begin
      FWaitDialog.Message := AMsg;
      FWaitDialog.RootView.MessageView.Text := AMsg;
    end;
  end);
end;

function TFrameView.StartFrame(FrameClass: TFrameViewClass;
  Params: TFrameParams; Ani: TFrameAniType): TFrameView;
begin
  Result := MakeFrame(FrameClass);
  if Assigned(Result) then begin
    Result.Params := Params;
    Hide(Ani);
    Result.Show(Ani, nil);
  end;
end;

function TFrameView.StartFrame(FrameClass: TFrameViewClass; Ani: TFrameAniType): TFrameView;
begin
  Result := MakeFrame(FrameClass);
  if Assigned(Result) then begin
    Hide(Ani);
    Result.Show(Ani, nil);
  end;
end;

class function TFrameView.ShowFrame(Parent: TFmxObject;
  Params: TFrameParams; Ani: TFrameAniType; SwitchFlag: Boolean): TFrameView;
begin
  Result := CreateFrame(Parent, Params);
  if Result <> nil then
    Result.Show(Ani, nil, SwitchFlag);
end;

{ TFrameState }

procedure TFrameState.Clear;
begin
  FLocker.Enter;
  if FData <> nil then
    FData.Clear;
  FLocker.Leave;
end;

function TFrameState.ContainsKey(const Key: string): Boolean;
begin
  FLocker.Enter;
  Result := FData.ContainsKey(Key);
  FLocker.Leave;
end;

constructor TFrameState.Create(AOwner: TComponent; IsPublic: Boolean);
begin
  FOwner := AOwner;
  FData := nil;
  FIsChange := False;
  FIsPublic := IsPublic;
  FLocker := TCriticalSection.Create;
  InitData;
  {$IFNDEF MSWINDOWS}
  StoragePath := TPath.GetDocumentsPath;
  {$ENDIF}
end;

destructor TFrameState.Destroy;
begin
  Save();
  FreeAndNil(FData);
  FreeAndNil(FLocker);
  inherited;
end;

procedure TFrameState.DoValueNotify(Sender: TObject; const Item: TFrameDataValue;
  Action: TCollectionNotification);
begin
  if Action <> TCollectionNotification.cnExtracted then
    FIsChange := True;
end;

function TFrameState.Exist(const Key: string): Boolean;
begin
  FLocker.Enter;
  Result := FData.ContainsKey(Key);
  FLocker.Leave;
end;

function TFrameState.GetBoolean(const Key: string;
  const DefaultValue: Boolean): Boolean;
begin
  FLocker.Enter;
  Result := FData.GetBoolean(Key, DefaultValue);
  FLocker.Leave;
end;

function TFrameState.GetCount: Integer;
begin
  if Assigned(FData) then
    Result := FData.Count
  else
    Result := 0;
end;

function TFrameState.GetDateTime(const Key: string;
  const DefaultValue: TDateTime): TDateTime;
begin
  FLocker.Enter;
  Result := FData.GetDateTime(Key, DefaultValue);
  FLocker.Leave;
end;

function TFrameState.GetFileName(const FileName: string): string;
begin
  Result := 'AF_' + FileName;
end;

function TFrameState.GetFloat(const Key: string;
  const DefaultValue: Double): Double;
begin
  FLocker.Enter;
  Result := FData.GetFloat(Key, DefaultValue);
  FLocker.Leave;
end;

function TFrameState.GetInt(const Key: string;
  const DefaultValue: Integer): Integer;
begin
  FLocker.Enter;
  Result := FData.GetInt(Key, DefaultValue);
  FLocker.Leave;
end;

function TFrameState.GetInt64(const Key: string;
  const DefaultValue: Int64): Int64;
begin
  FLocker.Enter;
  Result := FData.GetInt64(Key, DefaultValue);
  FLocker.Leave;
end;

function TFrameState.GetLong(const Key: string;
  const DefaultValue: Cardinal): Cardinal;
begin
  FLocker.Enter;
  Result := FData.GetLong(Key, DefaultValue);
  FLocker.Leave;
end;

function TFrameState.GetNumber(const Key: string;
  const DefaultValue: NativeUInt): NativeUInt;
begin
  FLocker.Enter;
  Result := FData.GetNumber(Key, DefaultValue);
  FLocker.Leave;
end;

function TFrameState.GetStoragePath: string;
var
  SaveStateService: IFMXSaveStateService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXSaveStateService, SaveStateService) then
    Result := SaveStateService.GetStoragePath
  else
    Result := '';
end;

function TFrameState.GetString(const Key: string): string;
begin
  FLocker.Enter;
  Result := FData.GetString(Key);
  FLocker.Leave;
end;

function TFrameState.GetUniqueName: string;
const
  UniqueNameSeparator = '_';
  UniqueNamePrefix = 'FM';
  UniqueNameExtension = '.Data';
var
  B: TStringBuilder;
begin
  if FIsPublic then
    Result := 'AppPublicState.Data'
  else begin
    B := TStringBuilder.Create(Length(UniqueNamePrefix) + FOwner.ClassName.Length +
      Length(UniqueNameSeparator) + Length(UniqueNameExtension));
    try
      B.Append(UniqueNamePrefix);
      B.Append(UniqueNameSeparator);
      B.Append(FOwner.ClassName);
      B.Append(UniqueNameExtension);
      Result := B.ToString;
    finally
      B.Free;
    end;
  end;
end;

procedure TFrameState.InitData;
begin
  if FData <> nil then
    FData.Clear
  else begin
    if FIsPublic then
      FData := TFrameStateData.Create(97)
    else
      FData := TFrameStateData.Create(29);
    FData.OnValueNotify := DoValueNotify;
  end;
end;

procedure TFrameState.Load;
var
  AStream: TMemoryStream;
  SaveStateService: IFMXSaveStateService;
  Reader: TBinaryReader;
  ACount, I: Integer;
  ASize: Int64;
  AKey: string;
  AType: TFrameDataType;
begin
  FLocker.Enter;
  if FIsLoad then begin
    FLocker.Leave;
    Exit;
  end;
  try
    FData.Clear;
    AStream := TMemoryStream.Create;
    if TPlatformServices.Current.SupportsPlatformService(IFMXSaveStateService, SaveStateService) then
      SaveStateService.GetBlock(GetUniqueName, AStream);
    ASize := AStream.Size;
    Reader := nil;
    if AStream.Size > 0 then begin
      AStream.Position := 0;
      Reader := TBinaryReader.Create(AStream);
      ACount := Reader.ReadInteger;
      for I := 0 to ACount - 1 do begin
        if AStream.Position >= ASize then
          Break;
        AType := TFrameDataType(Reader.ReadShortInt);
        AKey := Reader.ReadString;
        case AType of
          fdt_Integer: FData.Put(AKey, Reader.ReadInt32);
          fdt_Long: FData.Put(AKey, Reader.ReadCardinal);
          fdt_Int64: FData.Put(AKey, Reader.ReadInt64);
          fdt_Float: FData.Put(AKey, Reader.ReadDouble);
          fdt_String: FData.Put(AKey, Reader.ReadString);
          fdt_DateTime: FData.PutDateTime(AKey, Reader.ReadDouble);
          fdt_Number: FData.Put(AKey, NativeUInt(Reader.ReadUInt64));
          fdt_Boolean: FData.Put(AKey, Reader.ReadBoolean);
        else
          Break;
        end;
      end;
    end;
  finally
    FreeAndNil(AStream);
    FreeAndNil(Reader);
    FIsChange := False;
    FIsLoad := True;
    FLocker.Leave;
  end;
end;

procedure TFrameState.Put(const Key: string; const Value: Cardinal);
begin
  FLocker.Enter;
  FData.Put(Key, Value);
  FLocker.Leave;
end;

procedure TFrameState.Put(const Key: string; const Value: Integer);
begin
  FLocker.Enter;
  FData.Put(Key, Value);
  FLocker.Leave;
end;

procedure TFrameState.Put(const Key, Value: string);
begin
  FLocker.Enter;
  FData.Put(Key, Value);
  FLocker.Leave;
end;

procedure TFrameState.Put(const Key: string; const Value: NativeUInt);
begin
  FLocker.Enter;
  FData.Put(Key, Value);
  FLocker.Leave;
end;

procedure TFrameState.Put(const Key: string; const Value: Boolean);
begin
  FLocker.Enter;
  FData.Put(Key, Value);
  FLocker.Leave;
end;

procedure TFrameState.Put(const Key: string; const Value: Int64);
begin
  FLocker.Enter;
  FData.Put(Key, Value);
  FLocker.Leave;
end;

procedure TFrameState.Put(const Key: string; const Value: Double);
begin
  FLocker.Enter;
  FData.Put(Key, Value);
  FLocker.Leave;
end;

procedure TFrameState.PutDateTime(const Key: string; const Value: TDateTime);
begin
  FLocker.Enter;
  FData.PutDateTime(Key, Value);
  FLocker.Leave;
end;

function TFrameState.ReadFile(const FileName: string;
  var OutData: TStream): Boolean;
var
  SaveStateService: IFMXSaveStateService;
  LastPosition: Int64;
begin
  Result := False;
  if not Assigned(OutData) then Exit;
  if TPlatformServices.Current.SupportsPlatformService(IFMXSaveStateService, SaveStateService) then begin
    LastPosition := OutData.Position;
    Result := SaveStateService.GetBlock(GetFileName(ExtractFileName(FileName)), OutData);
    OutData.Position := LastPosition;
  end;
end;

procedure TFrameState.Save;
var
  SaveStateService: IFMXSaveStateService;
  AStream: TMemoryStream;
  Writer: TBinaryWriter;
  ACount: Integer;
  Item: TPair<string, TFrameDataValue>;
  ADoubleValue: Double;
begin
  FLocker.Enter;
  if not FIsChange then begin
    FLocker.Leave;
    Exit;
  end;
  try
    AStream := TMemoryStream.Create;
    Writer := TBinaryWriter.Create(AStream);
    ACount := Count;
    Writer.Write(ACount);
    for Item in FData do begin
      Writer.Write(ShortInt(Ord(Item.Value.DataType)));
      Writer.Write(Item.Key);
      case Item.Value.DataType of
        fdt_Integer: Writer.Write(Item.Value.Value.AsInteger);
        fdt_Long: Writer.Write(Cardinal(Item.Value.Value.AsInteger));
        fdt_Int64: Writer.Write(Item.Value.Value.AsInt64);
        fdt_Float, fdt_DateTime:
          begin
            ADoubleValue := Item.Value.Value.AsExtended;
            Writer.Write(ADoubleValue);
          end;
        fdt_String: Writer.Write(Item.Value.Value.AsString);
        fdt_Number: Writer.Write(Item.Value.Value.AsUInt64);
        fdt_Boolean: Writer.Write(Item.Value.Value.AsBoolean);
      end;
    end;
    if TPlatformServices.Current.SupportsPlatformService(IFMXSaveStateService, SaveStateService) then
      SaveStateService.SetBlock(GetUniqueName, AStream);
  finally
    FreeAndNil(AStream);
    FreeAndNil(Writer);
    FIsChange := False;
    FLocker.Leave;
  end;
end;

function TFrameState.SaveFile(const FileName: string;
  const Data: TStream): Boolean;
var
  SaveStateService: IFMXSaveStateService;
  LastPosition: Int64;
begin
  Result := False;
  if not Assigned(Data) then Exit;
  if TPlatformServices.Current.SupportsPlatformService(IFMXSaveStateService, SaveStateService) then begin
    LastPosition := Data.Position;
    Data.Position := 0;
    SaveStateService.SetBlock(GetFileName(ExtractFileName(FileName)), Data);
    Data.Position := LastPosition;
    Result := True;
  end;
end;

procedure TFrameState.SetStoragePath(const Value: string);
var
  SaveStateService: IFMXSaveStateService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXSaveStateService, SaveStateService) then
    SaveStateService.SetStoragePath(Value);
end;

{ TFrameStateDataHelper }

function TFrameStateDataHelper.GetBoolean(const Key: string;
  const DefaultValue: Boolean): Boolean;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.AsBoolean
  else
    Result := DefaultValue;
end;

function TFrameStateDataHelper.GetDataValue(DataType: TFrameDataType;
  const Value: TValue): TFrameDataValue;
begin
  Result.DataType := DataType;
  Result.Value := Value;
end;

function TFrameStateDataHelper.GetDateTime(const Key: string;
  const DefaultValue: TDateTime): TDateTime;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.AsExtended
  else
    Result := DefaultValue;
end;

function TFrameStateDataHelper.GetFloat(const Key: string;
  const DefaultValue: Double): Double;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.AsExtended
  else
    Result := DefaultValue;
end;

function TFrameStateDataHelper.GetInt(const Key: string;
  const DefaultValue: Integer): Integer;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.AsInteger
  else
    Result := DefaultValue;
end;

function TFrameStateDataHelper.GetInt64(const Key: string;
  const DefaultValue: Int64): Int64;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.AsInt64
  else
    Result := DefaultValue;
end;

function TFrameStateDataHelper.GetLong(const Key: string;
  const DefaultValue: Cardinal): Cardinal;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.AsInteger
  else
    Result := DefaultValue;
end;

function TFrameStateDataHelper.GetNumber(const Key: string;
  const DefaultValue: NativeUInt): NativeUInt;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.AsOrdinal
  else
    Result := DefaultValue;
end;

function TFrameStateDataHelper.GetPointer(const Key: string): Pointer;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.AsVarRec.VPointer
  else
    Result := nil;
end;

function TFrameStateDataHelper.GetString(const Key: string): string;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.ToString
  else
    Result := '';
end;

function TFrameStateDataHelper.Put(const Key: string; const Value: Cardinal): TFrameStateData;
begin
  Result := Self;
  AddOrSetValue(Key, GetDataValue(fdt_Long, Value));
end;

function TFrameStateDataHelper.Put(const Key: string; const Value: Integer): TFrameStateData;
begin
  Result := Self;
  AddOrSetValue(Key, GetDataValue(fdt_Integer, Value));
end;

function TFrameStateDataHelper.Put(const Key, Value: string): TFrameStateData;
begin
  Result := Self;
  AddOrSetValue(Key, GetDataValue(fdt_String, Value));
end;

function TFrameStateDataHelper.Put(const Key: string; const Value: NativeUInt): TFrameStateData;
begin
  Result := Self;
  AddOrSetValue(Key, GetDataValue(fdt_Number, Value));
end;

function TFrameStateDataHelper.Put(const Key: string; const Value: Boolean): TFrameStateData;
begin
  Result := Self;
  AddOrSetValue(Key, GetDataValue(fdt_Boolean, Value));
end;

function TFrameStateDataHelper.Put(const Key: string; const Value: Int64): TFrameStateData;
begin
  Result := Self;
  AddOrSetValue(Key, GetDataValue(fdt_Int64, Value));
end;

function TFrameStateDataHelper.Put(const Key: string; const Value: Double): TFrameStateData;
begin
  Result := Self;
  AddOrSetValue(Key, GetDataValue(fdt_Float, Value));
end;

function TFrameStateDataHelper.PutDateTime(const Key: string;
  const Value: TDateTime): TFrameStateData;
begin
  Result := Self;
  AddOrSetValue(Key, GetDataValue(fdt_DateTime, Value));
end;

{ TFrameParamsHelper }

function TFrameParamsHelper.GetBoolean(const Key: string;
  const DefaultValue: Boolean): Boolean;
begin
  if ContainsKey(Key) then begin
    Result := Items[Key].AsBoolean
  end else
    Result := DefaultValue;
end;

function TFrameParamsHelper.GetDateTime(const Key: string;
  const DefaultValue: TDateTime): TDateTime;
begin
  if ContainsKey(Key) then begin
    Result := Items[Key].AsExtended
  end else
    Result := DefaultValue;
end;

function TFrameParamsHelper.GetFloat(const Key: string;
  const DefaultValue: Double): Double;
begin
  if ContainsKey(Key) then begin
    Result := Items[Key].AsExtended
  end else
    Result := DefaultValue;
end;

function TFrameParamsHelper.GetInt(const Key: string;
  const DefaultValue: Integer): Integer;
begin
  if ContainsKey(Key) then begin
    Result := Items[Key].AsInteger
  end else
    Result := DefaultValue;
end;

function TFrameParamsHelper.GetInt64(const Key: string;
  const DefaultValue: Int64): Int64;
begin
  if ContainsKey(Key) then begin
    Result := Items[Key].AsInt64
  end else
    Result := DefaultValue;
end;

function TFrameParamsHelper.GetLong(const Key: string;
  const DefaultValue: Cardinal): Cardinal;
begin
  if ContainsKey(Key) then begin
    Result := Items[Key].AsInteger
  end else
    Result := DefaultValue;
end;

function TFrameParamsHelper.GetNumber(const Key: string;
  const DefaultValue: NativeUInt): NativeUInt;
begin
  if ContainsKey(Key) then begin
    Result := Items[Key].AsOrdinal
  end else
    Result := DefaultValue;
end;

function TFrameParamsHelper.GetPointer(const Key: string): Pointer;
begin
  if ContainsKey(Key) then begin
    Result := Items[Key].AsVarRec.VPointer
  end else
    Result := nil;
end;

function TFrameParamsHelper.GetString(const Key: string): string;
begin
  if ContainsKey(Key) then begin
    Result := Items[Key].ToString
  end else
    Result := '';
end;

function TFrameParamsHelper.Put(const Key: string; const Value: Integer): TFrameParams;
begin
  Result := Self;
  AddOrSetValue(Key, Value);
end;

function TFrameParamsHelper.Put(const Key, Value: string): TFrameParams;
begin
  Result := Self;
  AddOrSetValue(Key, Value);
end;

function TFrameParamsHelper.Put(const Key: string; const Value: Cardinal): TFrameParams;
begin
  Result := Self;
  AddOrSetValue(Key, Value);
end;

function TFrameParamsHelper.Put(const Key: string; const Value: NativeUInt): TFrameParams;
begin
  Result := Self;
  AddOrSetValue(Key, Value);
end;

function TFrameParamsHelper.Put(const Key: string; const Value: Boolean): TFrameParams;
begin
  Result := Self;
  AddOrSetValue(Key, Value);
end;

function TFrameParamsHelper.Put(const Key: string; const Value: Int64): TFrameParams;
begin
  Result := Self;
  AddOrSetValue(Key, Value);
end;

function TFrameParamsHelper.Put(const Key: string; const Value: Double): TFrameParams;
begin
  Result := Self;
  AddOrSetValue(Key, Value);
end;

function TFrameParamsHelper.PutDateTime(const Key: string;
  const Value: TDateTime): TFrameParams;
begin
  Result := Self;
  AddOrSetValue(Key, Value);
end;

{ TCustomFormHelper }

procedure TCustomFormHelper.SetFocus;
var
  LControl: IControl;
  Item: TFmxObject;
  Ctrl: TControl;
  I: Integer;
begin
  try
    for I := 0 to Self.ChildrenCount - 1 do begin
      Item := Children.Items[I];
      if (Item is TControl) then begin
        Ctrl := Item as TControl;
        if (Ctrl.Visible) and (Ctrl.Enabled) and (Ctrl.CanFocus) then begin
          LControl := Root.NewFocusedControl(Ctrl);
          if LControl <> nil then begin
            Root.SetFocused(LControl);
            Break;
          end;
        end else if Ctrl.ControlsCount > 0 then begin
          if Ctrl.SetFocusObject(Ctrl) then
            Break;
        end;
      end;
    end;
  except
  end;
end;

initialization
  FPublicState := TFrameState.Create(nil, True);
  FPublicState.Load;

finalization
  FreeAndNil(FPublicState);

end.

