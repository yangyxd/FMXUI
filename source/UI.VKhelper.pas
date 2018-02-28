{*******************************************************}
{                                                       }
{       FMXUI 虚拟键遮挡问题处理单元                    }
{                                                       }
{       版权所有 (C) 2018 by YangYxd                    }
{                                                       }
{*******************************************************}

{
   本单元基于QDAC中的qdac_fmx_vkhelper.pas修改。
   感谢QDAC作者swish！
   QDAC官方网站： www.qdac.cc
}

unit UI.VKhelper;

interface

{$DEFINE FMXUI}  // 是否支持 FMXUI

uses
  Classes, SysUtils, Math,
  FMX.Controls, FMX.Layouts, System.Types, System.Messaging;

function IsVirtalKeyboardVisible: Boolean;
function GetVirtalKeyboardBounds: TRectF; overload;
function GetVirtalKeyboardBounds(var ARect: TRect): Boolean; overload;

var
  /// <summary>
  /// 是否启用本单元防虚拟键盘遮档功能
  /// </summary>
  EnableVirtalKeyboardHelper: Boolean = True;
  /// <summary>
  /// 是否启用调节父窗口大小（能更好的适用于底部是文本框的布局）
  /// </summary>
  EnableAdjustFormSize: Boolean = False;

  EnableReturnKeyHook: Boolean = False;

implementation

uses
  {$IFDEF FMXUI}
  UI.Base, UI.Standard,
  {$ENDIF}
  FMX.Text, FMX.Scrollbox, FMX.VirtualKeyboard, FMX.Forms,
  FMX.Platform, TypInfo, FMX.InertialMovement,
  {$IFDEF ANDROID}
  Androidapi.JNI.Os, Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  FMX.Platform.Android, FMX.Helpers.Android,
  FMX.VirtualKeyboard.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Embarcadero,
  {$IF RTLVersion>=32}
  Androidapi.NativeActivity, Androidapi.AppGlue,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF IOS}
  Macapi.Helpers, FMX.Platform.iOS, FMX.VirtualKeyboard.iOS, iOSapi.Foundation,
  iOSapi.UIKit,
  {$ENDIF}
  FMX.Types, System.UITypes, System.Rtti;

type
  PAdjustItem = ^TAdjustItem;
  TAdjustItem = record
    Prior: PAdjustItem;
    Control: TControl;
    LastMargin: TPointF;
    LastViewPos: TPointF;
    LastBounds: TRectF;
    LastAlign: TAlignLayout;
  end;

  TFocusChanged = class(TMessage)
  end;

  TQAdjustStack = class
  private
    function GetLastCtrl: TControl;
  protected
    FLast: PAdjustItem;
    function GetAdjusted: Boolean;
    procedure RemoveLast;
  public
    procedure Save(ACtrl: TControl);
    procedure Restore;
    procedure Remove(ACtrl: TComponent);
    property Adjusted: Boolean read GetAdjusted;
    property LastControl: TControl read GetLastCtrl;
  end;

  TVKNextHelper = class(TFmxObject)
  protected
    FOriginEvent: TKeyEvent;
    procedure SetParent(const Value: TFMXObject); override;
    procedure DoFocusNext(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  end;

  TVKStateHandler = class(TComponent)
  protected
    FVKMsgId: Integer; // TVKStateChangeMessage 消息的订阅ID
    FSizeMsgId: Integer; // TSizeChangedMessage 消息的订阅ID
    FIdleMsgId: Integer;
    FLastControl: TControl;
    [Weak] FLastForm: TCustomForm;
    [Weak] FLastFocused: IControl;
    FAdjustStack: TQAdjustStack; // 最后一次调整的ScrollBox
    {$IFDEF ANDROID}
    FVKState: PByte;
  class var
    FContentRect: TRect;
    {$IF RTLVersion>=32}
    FLastContentRectChanged: TOnContentRectChanged;
    procedure DoContentRectChanged(const App: TAndroidApplicationGlue; const ARect: TRect);
    {$ENDIF}
    {$ENDIF}
    procedure DoVKVisibleChanged(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure DoSizeChanged(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure DoAppIdle(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function AdjustByLayout(ARoot: TFMXObject; AVOffset: Single): Single;
    function AdjustByScrollBox(AScrollBox: TCustomScrollBox; AVOffset: Single): Single; overload;
    {$IFDEF FMXUI}
    function AdjustByScrollBox(AScrollBox: TVertScrollView; AVOffset: Single): Single; overload;
    {$ENDIF}
    function AdjustByPresentedScrollBox(AScrollBox: TCustomPresentedScrollBox; AVOffset: Single): Single;
    procedure AdjustCtrl(ACtrl: TControl; AVKBounds, ACtrlBounds: TRectF; AVKVisible: Boolean);
    function NeedAdjust(ACtrl: TControl; var ACaretRect: TRectF): Boolean;
    function IsAnimating(AniCalculations: TAniCalculations): Boolean;
    procedure AdjustIfNeeded;
    procedure RestoreAdjustForm();
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
  end;

  TAndroidContentChangeMessage = TMessage<TRect>;

var
  VKHandler: TVKStateHandler;
  {$IFDEF IOS}
  _IOS_VKBounds: TRectF;
  {$ENDIF}

{$IFDEF ANDROID}
function JRectToRectF(R: JRect): TRectF;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Right;
  Result.Bottom := R.Bottom;
end;

function GetVKPixelBounds: TRect;
var
  TotalRect: JRect;
  Content, Total: TRectF;
  ContentRect: JRect;
begin
  TotalRect := TJRect.Create;
  ContentRect := TJRect.Create;
  MainActivity.getWindow.getDecorView.getWindowVisibleDisplayFrame(ContentRect);
  Content := JRectToRectF(ContentRect);
  TVKStateHandler.FContentRect := Content.Truncate;
  MainActivity.getWindow.getDecorView.getDrawingRect(TotalRect);
  Total := JRectToRectF(TotalRect);
  Result.Left := Trunc(Total.Left);
  Result.Top := Trunc(Total.Top + Content.Height);
  Result.Right := Trunc(Total.Right);
  Result.Bottom := Trunc(Total.Bottom);
end;

function GetVirtalKeyboardBounds: TRectF; overload;
var
  b: TRect;
begin
  b := GetVKPixelBounds;
  Result := TRectF.Create(ConvertPixelToPoint(b.TopLeft),
    ConvertPixelToPoint(b.BottomRight));
end;

function GetVirtalKeyboardBounds(var ARect: TRect): Boolean; overload;
begin
  ARect := GetVKPixelBounds;
  Result := ARect.Bottom <> TVKStateHandler.FContentRect.Bottom;
  ARect := TRectF.Create(ConvertPixelToPoint(ARect.TopLeft),
    ConvertPixelToPoint(ARect.BottomRight)).Truncate;
end;

function GetVirtalKeyboardBounds(var ARect: TRectF): Boolean; overload;
begin
  ARect := TRectF.Create(GetVKPixelBounds);
  Result := ARect.Bottom <> TVKStateHandler.FContentRect.Bottom;
  ARect := TRectF.Create(ConvertPixelToPoint(ARect.TopLeft),
    ConvertPixelToPoint(ARect.BottomRight)).Truncate;
end;
{$ELSE}
{$IFDEF IOS}
function GetVirtalKeyboardBounds: TRectF; overload;
var
  ATop: Integer;
begin
  Result := _IOS_VKBounds;
  ATop := Screen.WorkAreaTop;
  Result.Top := Result.Top - ATop;
  Result.Bottom := Result.Bottom - ATop;
end;

function GetVirtalKeyboardBounds(var ARect: TRect): Boolean; overload;
var
  ATemp: TRectF;
  AService: IFMXScreenService;
  AScale: Single;
begin
  ATemp := GetVKBounds;
  Result := not ATemp.IsEmpty;
  if Result then
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
      AService) then
    begin
      AScale := AService.GetScreenScale;
      ARect.Left := Trunc(ATemp.Left * AScale);
      ARect.Top := Trunc(ATemp.Top * AScale);
      ARect.Right := Trunc(ATemp.Right * AScale);
      ARect.Bottom := Trunc(ATemp.Bottom * AScale);
    end;
  end;
end;
{$ELSE}
function GetVirtalKeyboardBounds: TRectF; overload;
begin
  Result := TRectF.Empty;
end;

function GetVirtalKeyboardBounds(var ARect: TRect): Boolean; overload;
begin
  Result := false;
end;
{$ENDIF}
{$ENDIF}

function IsVirtalKeyboardVisible: Boolean;
{$IFDEF NEXTGEN}var R: TRect; {$ENDIF}
begin
  {$IFDEF NEXTGEN}
  Result := GetVirtalKeyboardBounds(R);
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

procedure BeginUpdate(AObj: TFMXObject);
begin
  if AObj is TControl then
    (AObj as TControl).BeginUpdate
  else if AObj is TCustomForm then
    (AObj as TCustomForm).BeginUpdate;
end;

procedure EndUpdate(AObj: TFMXObject);
begin
  if AObj is TControl then
    (AObj as TControl).EndUpdate
  else if AObj is TCustomForm then
    (AObj as TCustomForm).EndUpdate;
end;

{ TVKStateHandler }

/// Adjust by layout,return the real adjust offset
function TVKStateHandler.AdjustByLayout(ARoot: TFMXObject; AVOffset: Single): Single;
var
  ALayout: TControl;

  // 移动指定父上的子对象到新的父对象
  procedure MoveCtrls(AOldParent, ANewParent: TFMXObject);
  var
    I: Integer;
    AChild: TFMXObject;
  begin
    I := 0;
    BeginUpdate(AOldParent);
    BeginUpdate(ANewParent);
    try
      while I < AOldParent.ChildrenCount do begin
        AChild := AOldParent.Children[I];
        if AChild <> ANewParent then begin
          if AChild.Parent = AOldParent then begin
            AChild.Parent := ANewParent;
            Continue;
          end;
        end;
        Inc(I);
      end;
    finally
      EndUpdate(AOldParent);
      EndUpdate(ANewParent);
    end;
  end;

  function RootLayout(ARoot: TFMXObject): TControl;
  var
    ACtrl: TComponent;
    I: Integer;
    ALastRootStyle: string;
  begin
    Result := nil;

    if (ARoot.ComponentCount > 0) then begin
      for I := 0 to ARoot.ComponentCount - 1 do begin
        ACtrl := ARoot.Components[I];
        if ACtrl is TLayout then begin
          Result := ACtrl as TLayout;
          if Result.TagObject <> Self then
            Result := nil;
        end;
      end;
    end;

    if Result = nil then begin
      if ARoot is TCustomForm then begin
        with ARoot as TCustomForm do begin
          ALastRootStyle := StyleLookup;
          StyleLookup := '';
        end;
      end else
        ALastRootStyle := '';

      Result := TLayout.Create(ARoot);
      Result.Parent := ARoot;
      Result.TagObject := Self;
      with ARoot as IContainerObject do
        Result.SetBounds(0, 0, ContainerWidth, ContainerHeight);
      MoveCtrls(ARoot, Result);

      // 修正窗体样式被错误删除造成的问题
      if Length(ALastRootStyle) > 0 then
        (ARoot as TCustomForm).StyleLookup := ALastRootStyle;
    end;
  end;

begin
  ALayout := RootLayout(ARoot); // 确认存在用于调整的根布局
  FAdjustStack.Save(ALayout);
  ALayout.Position.Y := ALayout.Position.Y + AVOffset;
  Result := AVOffset;
end;

function TVKStateHandler.AdjustByPresentedScrollBox(AScrollBox: TCustomPresentedScrollBox; AVOffset: Single): Single;
var
  ALastY: Single;
begin
  FAdjustStack.Save(AScrollBox);
  ALastY := AScrollBox.ViewportPosition.Y;
  AScrollBox.ViewportPosition.Offset(0, AVOffset + ALastY);
  // AScrollBox.ScrollBy(0, AVOffset);
  Result := ALastY - AScrollBox.ViewportPosition.Y;
end;

{$IFDEF FMXUI}
function TVKStateHandler.AdjustByScrollBox(AScrollBox: TVertScrollView; AVOffset: Single): Single;
var
  ALastY: Single;
begin
  FAdjustStack.Save(AScrollBox);
  ALastY := AScrollBox.ViewportPosition.Y;
  AScrollBox.ScrollBy(0, AVOffset);
  Result := ALastY - AScrollBox.ViewportPosition.Y;
end;
{$ENDIF}

function TVKStateHandler.AdjustByScrollBox(AScrollBox: TCustomScrollBox; AVOffset: Single): Single;
var
  ALastY: Single;
begin
  FAdjustStack.Save(AScrollBox);
  ALastY := AScrollBox.ViewportPosition.Y;
  AScrollBox.ScrollBy(0, AVOffset);
  Result := ALastY - AScrollBox.ViewportPosition.Y;
end;

{$IFNDEF FMXUI}
var
  StatusHeight: Single = 0;

function GetParentForm(AObj: TFmxObject): TCustomForm;
var
  V: TFmxObject;
begin
  Result := nil;
  if not Assigned(AObj) then
    Exit;
  V := AObj.Parent;
  while Assigned(V) do begin
    if V is TCustomForm then begin
      Result := V as TCustomForm;
      Break;
    end;
    V := V.Parent;
  end;
end;
{$ENDIF}

procedure TVKStateHandler.AdjustCtrl(ACtrl: TControl;
  AVKBounds, ACtrlBounds: TRectF; AVKVisible: Boolean);
var
  ACaretRect: TRectF;
  AForm: TCustomForm;

  function TryByScrollBox(AParent: TFMXObject; var AOffset: Single): TFMXObject;
  begin
    Result := AParent.Parent;

    // 父有滚动框，则尝试滚动解决
    while Assigned(AParent) and (AOffset < 0) do begin
      if AParent is TCustomScrollBox then begin
        // 正在滚动时不需要调整
        if IsAnimating((AParent as TCustomScrollBox).AniCalculations) then begin
          AOffset := 0;
          Exit;
        end;
        AOffset := AOffset - AdjustByScrollBox(AParent as TCustomScrollBox, AOffset)
      {$IFDEF FMXUI}
      end else if AParent is TVertScrollView then begin
        AOffset := AOffset - AdjustByScrollBox(AParent as TVertScrollView, AOffset)
      {$ENDIF}
      end else if AParent is TCustomPresentedScrollBox then begin
        AOffset := AOffset - AdjustByPresentedScrollBox(AParent as TCustomPresentedScrollBox, AOffset)
      end;
      Result := AParent;
      AParent := AParent.Parent;
    end;
  end;

  // 将指定的区域移动可视区
  procedure ScrollInToRect;
  var
    AParent: TFMXObject;
    AOffset: Single;
  begin
    AOffset := AVKBounds.Top - ACaretRect.Bottom;
    AParent := TryByScrollBox(ACtrl, AOffset);
    if AOffset < 0 then
      AdjustByLayout(AParent, AOffset);
  end;

  procedure AddNextHelper;
  var
    AHelper: TVKNextHelper;
    I: Integer;
    AVKCtrl: IVirtualKeyboardControl;
  begin
    if Supports(ACtrl, IVirtualKeyboardControl, AVKCtrl) then begin
      for I := 0 to ACtrl.ComponentCount - 1 do begin
        if ACtrl.Components[I] is TVKNextHelper then
          Exit;
      end;
      AHelper := TVKNextHelper.Create(ACtrl);
      AHelper.SetParent(ACtrl);
    end;
  end;

begin
  if AVKVisible and EnableVirtalKeyboardHelper then begin

    if EnableAdjustFormSize then begin
      AForm := GetParentForm(ACtrl);
      if Assigned(AForm) then begin
        if Assigned(FLastForm) and (FLastForm <> AForm) then
          FLastForm.Padding.Bottom := 0;
        FLastForm := AForm;
        FLastForm.Padding.Bottom := AVKBounds.Height - {$IFNDEF FMXUI}StatusHeight{$ELSE}TView.GetStatusHeight{$ENDIF};
      end;
    end;

    if NeedAdjust(ACtrl, ACaretRect) then begin
      if FLastControl <> ACtrl then begin
        if Assigned(FLastControl) then
          FLastControl.RemoveFreeNotification(Self);
        FLastControl := ACtrl;
        FLastControl.FreeNotification(Self);
      end;

      ScrollInToRect;
    end;

    if EnableReturnKeyHook then
      AddNextHelper;

  end else begin

    RestoreAdjustForm();
    FAdjustStack.Restore;
    FLastControl := nil;
    FLastFocused := nil;
  end;
end;

procedure TVKStateHandler.AdjustIfNeeded;

  {$IFDEF ANDROID}
  procedure UpdateAndroidKeyboardServiceState;
  var
    ASvc: IFMXVirtualKeyboardService;
    AContext: TRttiContext;
    AType: TRttiType;
    AField: TRttiField;
    AInst: TVirtualKeyboardAndroid;
  begin
    if not Assigned(FVKState) then begin
      if (not Assigned(Screen.FocusControl)) and
        TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, ASvc) then
      begin
        AInst := ASvc as TVirtualKeyboardAndroid;
        AContext := TRttiContext.Create;
        AType := AContext.GetType(TVirtualKeyboardAndroid);
        AField := AType.GetField('FState');
        if Assigned(AField) and (AField.GetValue(AInst).AsOrdinal <> 0) then begin
          FVKState := PByte(AInst);
          Inc(FVKState, AField.Offset);
        end;
      end;
    end;

    if Assigned(FVKState) and (FVKState^ <> 0) then
      FVKState^ := 0;
  end;
  {$ENDIF}

  {$IFDEF IOS}
  procedure VKHide;
  var
    ASvc: IFMXVirtualKeyboardService;
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, ASvc) then
      ASvc.HideVirtualKeyboard;
  end;
  {$ENDIF}

  procedure RestoreCtrls;
  begin
    {$IFDEF ANDROID}
    // if not Assigned(Screen.FocusControl) then
    // UpdateAndroidKeyboardServiceState;
    {$ENDIF}
    RestoreAdjustForm();
    if Assigned(FLastControl) then
      AdjustCtrl(FLastControl, RectF(0, 0, 0, 0), FLastControl.AbsoluteRect, false);
  end;

  procedure CheckHidden;
  var
    ACaretRect: TRectF;
    ACtrl: TControl;
  begin
    if Assigned(Screen.FocusControl) then begin
      ACtrl := Screen.FocusControl.GetObject as TControl;
      if Assigned(ACtrl) then begin
        if NeedAdjust(ACtrl, ACaretRect) then
          AdjustCtrl(ACtrl, GetVirtalKeyboardBounds, ACtrl.AbsoluteRect, True);
      end;
    end else begin
      {$IFDEF IOS}
      VKHide;
      {$ENDIF}
      RestoreCtrls;
    end;
  end;

begin
  if IsVirtalKeyboardVisible then // 解决掉虚拟键盘隐藏后的问题
    CheckHidden
  else
    RestoreCtrls;
end;

// 构造函数，订阅消息
constructor TVKStateHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdjustStack := TQAdjustStack.Create;
  FVKMsgId := TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, DoVKVisibleChanged);
  FSizeMsgId := TMessageManager.DefaultManager.SubscribeToMessage(TSizeChangedMessage, DoSizeChanged);
  FIdleMsgId := TMessageManager.DefaultManager.SubscribeToMessage(TIdleMessage, DoAppIdle);
  {$IF DEFINED(Android) and (RTLVersion>=32)}
  with TAndroidApplicationGlue(PANativeActivity(System.DelphiActivity).instance) do
  begin
    FLastContentRectChanged := OnContentRectEvent;
    OnContentRectEvent := DoContentRectChanged;
  end;
  {$ENDIF}
end;

// 析构函数，取消消息订阅
destructor TVKStateHandler.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVKMsgId);
  TMessageManager.DefaultManager.Unsubscribe(TSizeChangedMessage, FSizeMsgId);
  TMessageManager.DefaultManager.Unsubscribe(TIdleMessage, FIdleMsgId);
  FAdjustStack.Restore;
  inherited;
end;

// 在应用空闲时，检查虚拟键盘是否隐藏或是否覆盖住了当前获得焦点的控件
procedure TVKStateHandler.DoAppIdle(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  if FLastFocused <> Screen.FocusControl then begin
    {$IFDEF VER320}   // Tokyo Only
    if Assigned(FLastFocused) then
      with (FLastFocused as TControl) do
        InvalidateRect(LocalRect);
    {$ENDIF}
    TMessageManager.DefaultManager.SendMessage(Sender, TFocusChanged.Create);
    FLastFocused := Screen.FocusControl;
  end;
  AdjustIfNeeded;
end;

{$IF DEFINED(ANDROID) and (RTLVersion>=32)}
procedure TVKStateHandler.DoContentRectChanged
  (const App: TAndroidApplicationGlue; const ARect: TRect);
var
  svc: IFMXScreenService;
  //AScale: Single;
  //ACtrl: TControl;
  //AVKRect: TRectF;
begin
  if ARect <> FContentRect then begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, svc) then
    begin
      // AScale := svc.GetScreenScale;
      FContentRect := ARect;
      // TRectF.Create(ARect.Left/AScale,ARect.Top/AScale,ARect.Right/AScale,ARect.Bottom/AScale).Truncate;
    end;
    FLastContentRectChanged(App, ARect);
    AdjustIfNeeded;
  end;
end;
{$ENDIF}

/// 在横竖屏切换时，处理控件位置
procedure TVKStateHandler.DoSizeChanged(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
var
  ASizeMsg: TSizeChangedMessage absolute Msg;
  R: TRect;
begin
  if Sender = Screen.ActiveForm then begin
    RestoreAdjustForm();
    FAdjustStack.Restore;
    if GetVirtalKeyboardBounds(R) then begin
      TMessageManager.DefaultManager.SendMessage(Sender,
        TVKStateChangeMessage.Create(True, R));
    end
  end;
end;

/// 虚拟键盘可见性变更消息，调整或恢复控件位置
procedure TVKStateHandler.DoVKVisibleChanged(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
var
  AVKMsg: TVKStateChangeMessage absolute Msg;
  ACtrl: TControl;
begin
  if AVKMsg.KeyboardVisible then begin // 键盘可见
    {$IFDEF IOS}
    _IOS_VKBounds := TRectF.Create(AVKMsg.KeyboardBounds);
    {$ENDIF}
    if Screen.FocusControl <> nil then begin
      ACtrl := Screen.FocusControl.GetObject as TControl;
      AdjustCtrl(ACtrl, GetVirtalKeyboardBounds, ACtrl.AbsoluteRect, True);
    end;
  end else begin
    RestoreAdjustForm();
    {$IFDEF IOS}
    _IOS_VKBounds := TRectF.Empty;
    {$ENDIF}
    FAdjustStack.Restore;
  end;
end;

function TVKStateHandler.IsAnimating(AniCalculations: TAniCalculations): Boolean;
var
  AContext: TRttiContext;
  AType: TRttiType;
  AField: TRttiField;
begin
  AContext := TRttiContext.Create;
  AType := AContext.GetType(AniCalculations.ClassType);
  if Assigned(AType) then begin
    AField := AType.GetField('FStarted');
    if Assigned(AField) then
      Result := AField.GetValue(AniCalculations).AsBoolean
    else
      Result := false;
  end else
    Result := false;
end;

function TVKStateHandler.NeedAdjust(ACtrl: TControl;
  var ACaretRect: TRectF): Boolean;
var
  ACaret: ICaret;
  ACaretObj: TCustomCaret;
  ACtrlBounds, AVKBounds: TRectF;

  function GetRootTop: Single;
  var
    ALayout: TLayout;
    AObj: TFMXObject;
  begin
    AObj := ACtrl.Root.GetObject;
    Result := 0;
    if AObj is TForm then begin
      if TForm(AObj).ChildrenCount > 0 then begin
        AObj := TForm(AObj).Children[0];
        if AObj is TLayout then begin
          ALayout := AObj as TLayout;
          Result := ALayout.Position.Y;
        end;
      end;
    end;
  end;

begin
  if Supports(ACtrl, ICaret, ACaret) then begin
    AVKBounds := GetVirtalKeyboardBounds;
    ACtrlBounds := ACtrl.AbsoluteRect;

    ACaretObj := ACaret.GetObject;
    ACaretRect.TopLeft := ACtrl.LocalToAbsolute(ACaretObj.Pos);
    ACaretRect.Right := ACaretRect.Left + ACaretObj.Size.cx + 1;
    ACaretRect.Bottom := ACaretRect.Top + ACaretObj.Size.cy + 1; // 下面加点余量

    if ACaretRect.Bottom > ACtrlBounds.Bottom then begin
      if ACtrl is TCustomPresentedScrollBox then begin
        AdjustByPresentedScrollBox(ACtrl as TCustomPresentedScrollBox,
          ACtrlBounds.Bottom - ACaretRect.Bottom);
        Result := false;
        Exit;
      end;
    end;

    Result := ACaretRect.IntersectsWith(AVKBounds) or (ACaretRect.Top < 0) or
      (ACaretRect.Top > AVKBounds.Bottom);
  end else
    Result := false;
end;

// 响应组件释放通知，以避免访问无效地址
procedure TVKStateHandler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FLastControl then begin
      FLastControl.RemoveFreeNotification(Self);
      FLastControl := nil;
      FLastFocused := nil;
    end else
      FAdjustStack.Remove(AComponent);
  end;
  inherited;
end;

procedure TVKStateHandler.RestoreAdjustForm;
begin
  if Assigned(FLastForm) then begin
    FLastForm.Padding.Bottom := 0;
    FLastForm := nil;
  end;
end;

{ TQAdjustStack }

function TQAdjustStack.GetAdjusted: Boolean;
begin
  Result := Assigned(FLast);
end;

function TQAdjustStack.GetLastCtrl: TControl;
begin
  if Assigned(FLast) then
    Result := FLast.Control
  else
    Result := nil;
end;

procedure TQAdjustStack.Remove(ACtrl: TComponent);
var
  ANext, ACurrent: PAdjustItem;
begin
  if Assigned(ACtrl) then begin
    ACurrent := FLast;
    ANext := nil;
    while Assigned(ACurrent) do begin
      if ACurrent.Control = ACtrl then begin
        if Assigned(ANext) then
          ANext.Prior := ACurrent.Prior
        else
          FLast := ACurrent.Prior;
        Dispose(ACurrent);
        break;
      end;
      ANext := ACurrent;
      ACurrent := ANext.Prior;
    end;
    ACtrl.RemoveFreeNotification(VKHandler);
  end;
end;

procedure TQAdjustStack.RemoveLast;
var
  APrior: PAdjustItem;
begin
  if Assigned(FLast) then begin
    APrior := FLast.Prior;
    Dispose(FLast);
    FLast := APrior;
  end;
end;

procedure TQAdjustStack.Restore;
var
  APrior: PAdjustItem;
begin
  while Assigned(FLast) do begin
    APrior := FLast.Prior;
    with FLast^ do begin
      if Control is TCustomScrollBox then begin
        Control.Margins.Bottom := LastMargin.Y;
        Control.Margins.Left := LastMargin.X;
        (Control as TCustomScrollBox).ViewportPosition := LastViewPos;
      end else if Control is TCustomPresentedScrollBox then begin
        Control.Margins.Bottom := LastMargin.Y;
        Control.Margins.Left := LastMargin.X;
        (Control as TCustomPresentedScrollBox).ViewportPosition := LastViewPos;
      end else begin
        if LastAlign = TAlignLayout.None then
          Control.Position.Point := LastMargin
        else begin
          Control.BoundsRect := LastBounds;
          Control.Align := LastAlign;
        end;
      end;
    end;
    Dispose(FLast);
    FLast := APrior;
  end;
end;

procedure TQAdjustStack.Save(ACtrl: TControl);
var
  AItem: PAdjustItem;
begin
  New(AItem);
  AItem.Prior := FLast;
  AItem.Control := ACtrl;
  if ACtrl is TCustomScrollBox then begin
    AItem.LastViewPos := (ACtrl as TCustomScrollBox).ViewportPosition;
    AItem.LastMargin.X := ACtrl.Margins.Left;
    AItem.LastMargin.Y := ACtrl.Margins.Bottom;
  end else if ACtrl is TCustomPresentedScrollBox then begin
    AItem.LastViewPos := (ACtrl as TCustomPresentedScrollBox).ViewportPosition;
    AItem.LastMargin.X := ACtrl.Margins.Left;
    AItem.LastMargin.Y := ACtrl.Margins.Bottom;
  end else begin
    AItem.LastMargin := ACtrl.Position.Point;
    AItem.LastBounds := ACtrl.BoundsRect;
    AItem.LastAlign := ACtrl.Align;
  end;
  FLast := AItem;
  ACtrl.FreeNotification(VKHandler);
end;

{ TVKNextHelper }

procedure TVKNextHelper.DoFocusNext(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  AVKCtrl: IVirtualKeyboardControl;

  procedure FocusNext(ACtrl: TControl);
  var
    AParent: TControl;
    ANext: IControl;
    ATabList: ITabList;
  begin
    if Assigned(ACtrl) and Assigned(ACtrl.ParentControl) then begin
      AParent := ACtrl.ParentControl;
      ATabList := AParent.GetTabList;
      if Assigned(ATabList) then begin
        ANext := ATabList.FindNextTabStop(ACtrl, not(ssShift in Shift), True);
        if Assigned(ANext) then
          ANext.SetFocus
        else
          FocusNext(AParent);
      end;
    end;
  end;

begin
  if Assigned(FOriginEvent) then
    FOriginEvent(Sender, Key, KeyChar, Shift);
  if Supports(Sender, IVirtualKeyboardControl, AVKCtrl) then begin
    if (Key = vkReturn) and (AVKCtrl.ReturnKeyType = TReturnKeyType.Next) then
      FocusNext(Sender as TControl);
  end;
end;

procedure TVKNextHelper.SetParent(const Value: TFMXObject);
begin
  if Value <> Parent then begin
    inherited;
    with Parent as TControl do begin
      FOriginEvent := OnKeyDown;
      OnKeyDown := DoFocusNext;
    end;
  end;
end;

{$IFNDEF FMXUI}
{$IFDEF ANDROID}
procedure DoInitFrameStatusHeight();
var
  resourceId: Integer;
begin
  if TJBuild_VERSION.JavaClass.SDK_INT < 19 then
    Exit;
  try
    resourceId := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}
      .getResources().getIdentifier(
        StringToJString('status_bar_height'),
        StringToJString('dimen'),
        StringToJString('android'));
    if resourceId <> 0 then begin
      StatusHeight := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}
        .getResources().getDimensionPixelSize(resourceId);
      if StatusHeight > 0 then
        StatusHeight := StatusHeight / {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}
          .getResources().getDisplayMetrics().scaledDensity;
    end else
      StatusHeight := 0;
  except
  end;
end;
{$ENDIF}{$ENDIF}

initialization
  // 仅支持Android+IOS
  {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  VKHandler := TVKStateHandler.Create(nil);
  {$ENDIF}
  EnableReturnKeyHook := True;
  {$IFNDEF FMXUI}
  {$IFDEF ANDROID}
  DoInitFrameStatusHeight();
  {$ENDIF}{$ENDIF}

finalization
  {$IF DEFINED(ANDROID)  OR DEFINED(IOS)}
  VKHandler.DisposeOf;
  VKHandler := nil;
  {$ENDIF}

end.
