{*******************************************************}
{                                                       }
{       FMXUI 虚拟键遮挡问题处理单元                    }
{                                                       }
{       版权所有 (C) 2019 by KngStr                     }
{                                                       }
{*******************************************************}

{
   本单元基于QDAC中的qdac_fmx_vkhelper.pas修改。
   感谢QDAC作者swish！
   QDAC官方网站： www.qdac.cc
}

unit UI.VKhelper;

interface

uses
  Classes, SysUtils, Math,
  FMX.Controls, FMX.Layouts, System.Types, System.Messaging;

type
  TControlHelper = class helper for TControl
    function OffsetOf(AParent: TControl): TPointF;
    function LocalToParent(AParent: TControl; APoint: TPointF): TPointF; overload;
    function LocalToParent(AParent: TControl; R: TRectF): TRectF; overload;
  end;

  TScrollBoxHelper = class helper for TCustomScrollBox
  public
    procedure ScrollInView(ACtrl: TControl);
  end;

  TFocusChanged = class(TMessage)

  end;

function IsVirtalKeyboardVisible: Boolean;
function GetVirtalKeyboardBounds: TRectF; overload;
function GetVirtalKeyboardBounds(var ARect: TRect): Boolean; overload;

var
  /// <summary>
  /// 是否启用本单元防虚拟键盘遮档功能
  /// </summary>
  EnableVirtalKeyboardHelper: Boolean = True;

  //EnableReturnKeyHook: Boolean = False;

implementation

uses
  FMX.Text, FMX.Scrollbox, FMX.VirtualKeyboard, FMX.Forms,
  FMX.Platform, TypInfo,
  {$IFDEF ANDROID}
  FMX.Platform.Android, FMX.Helpers.Android, Androidapi.Helpers,
  FMX.VirtualKeyboard.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Embarcadero,
  {$IF RTLVersion>=32}
  Androidapi.NativeActivity, Androidapi.AppGlue,
  {$ENDIF}
  {$IF RTLVersion=33}
  FMX.Platform.UI.Android,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF IOS}
  Macapi.Helpers, FMX.Platform.iOS, FMX.VirtualKeyboard.iOS, iOSapi.Foundation,
  iOSapi.UIKit,
  {$ENDIF}
  FMX.Types, System.UITypes, System.Rtti;

type

  TVKNextHelper = class(TFmxObject)
  protected
    FOriginEvent: TKeyEvent;
    procedure SetParent(const Value: TFMXObject); override;
    procedure DoFocusNext(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  end;

  TVKStateHandler = class(TComponent)
  protected
    class var FContentRect: TRect;
  protected
    FVKMsgId: Integer; // TVKStateChangeMessage 消息的订阅ID
    FSizeMsgId: Integer; // TSizeChangedMessage 消息的订阅ID
    FIdleMsgId: Integer;
    FLastIdleTick: Cardinal;
    FLastControl: TControl;
    FLastControlForm: TCommonCustomForm;
    FLastRect: TRectF;
    [Weak] FLastFocused: IControl;
    FCaretTarget: TPointF;
    FAdjusting: Boolean;
    procedure DoVKVisibleChanged(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
    procedure DoAppIdle(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure AdjustCtrl(ACtrl: TControl; AVKBounds: TRectF;
      AVKVisible: Boolean);
    function NeedAdjust(ACtrl: TControl; var ACaretRect: TRectF): Boolean;
    procedure AdjustIfNeeded;
    procedure Restore;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
  end;

  TAndroidContentChangeMessage = TMessage<TRect>;

var
  VKHandler: TVKStateHandler;
{$IFDEF ANDROID}
  {$IF RTLVersion>=33}// 10.3+
  _AndroidVKBounds: TRectF;
  {$ENDIF}

function JRectToRectF(R: JRect): TRectF;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Right;
  Result.Bottom := R.Bottom;
end;

function GetVKPixelBounds: TRectF;
var
  TotalRect: JRect;
  Content, Total: TRectF;
  ContentRect: JRect;
  AView: JView;
begin
  TotalRect := TJRect.Create;
  ContentRect := TJRect.Create;
  AView := TAndroidHelper.Activity.getWindow.getDecorView;
  AView.getDrawingRect(ContentRect);
  Content := JRectToRectF(ContentRect);
  TVKStateHandler.FContentRect := Content.Truncate;
  AView.getDrawingRect(TotalRect);
  Total := JRectToRectF(TotalRect);
  Result.Left := Total.Left;
  Result.Top := Total.Top + AView.getHeight;
  Result.Right := Total.Right;
  Result.Bottom := Total.Bottom;
end;

function GetVirtalKeyboardBounds(var ARect: TRectF): Boolean; overload;
begin
{$IF RTLVersion>=33}// 10.3+
  if MainActivity.getVirtualKeyboard.isVirtualKeyboardShown then begin
    ARect := _AndroidVKBounds;
    Result := not ARect.IsEmpty;
  end
  else begin
    ARect := TRectF.Empty;
    Result := False;
  end;
{$ELSE}
  ARect := GetVKPixelBounds;
  Result := ARect.Bottom <> TVKStateHandler.FContentRect.Bottom;
  ARect := TRectF.Create(ConvertPixelToPoint(ARect.TopLeft),
    ConvertPixelToPoint(ARect.BottomRight));
{$ENDIF}
end;

function GetVirtalKeyboardBounds: TRectF; overload;
var
  b: TRectF;
begin
  if not GetVirtalKeyboardBounds(Result) then
    Result := TRectF.Empty;
end;

function GetVirtalKeyboardBounds(var ARect: TRect): Boolean; overload;
var
  R: TRectF;
begin
  Result := GetVirtalKeyboardBounds(R);
  ARect := R.Truncate;
end;
{$ELSE}
{$IFDEF IOS}
  _IOS_VKBounds: TRectF;

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
  ATemp := GetVirtalKeyboardBounds;
  Result := not ATemp.IsEmpty;
  if Result then begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
      AService) then begin
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
  Result := False;
end;
{$ENDIF}
{$ENDIF}

// 根据MainActivity的可视区域和绘图区域大小来确定是否显示了虚拟键盘
function IsVirtalKeyboardVisible: Boolean;
{$IFDEF NEXTGEN}var R: TRect; {$ENDIF}
begin
  {$IFDEF NEXTGEN}
  Result := GetVirtalKeyboardBounds(R);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

{ TControlHelper }

function TControlHelper.LocalToParent(AParent: TControl; APoint: TPointF): TPointF;
var
  AOffset: TPointF;
begin
  AOffset := OffsetOf(AParent);
  Result.X := APoint.X + AOffset.X;
  Result.Y := APoint.Y + AOffset.Y;
end;

function TControlHelper.LocalToParent(AParent: TControl; R: TRectF): TRectF;
var
  AOffset: TPointF;
begin
  AOffset := OffsetOf(AParent);
  Result := R;
  Result.Offset(AOffset.X, AOffset.Y);
end;

function TControlHelper.OffsetOf(AParent: TControl): TPointF;
var
  ACtrl: TControl;
begin
  ACtrl := Self;
  Result.X := 0;
  Result.Y := 0;
  while (ACtrl <> nil) and (ACtrl <> AParent) do begin
    Result.X := Result.X + ACtrl.Position.X;
    Result.Y := Result.Y + ACtrl.Position.Y;
    ACtrl := ACtrl.ParentControl;
  end;
  if not Assigned(ACtrl) then
    raise Exception.CreateFmt('指定的控件 %s 不是 %s 的子控件', [Name, AParent.Name]);
end;

{ TScrollBoxHelper }

procedure TScrollBoxHelper.ScrollInView(ACtrl: TControl);
var
  R, LR: TRectF;
  dx, dy: Single;
begin
  R := ACtrl.LocalToParent(Self, ACtrl.LocalRect);
  LR := LocalRect;
  if not LR.Contains(R) then begin
    if R.Left > LR.Right then
      dx := LR.Right - R.Right
    else if R.Right < R.Left then
      dx := R.Left
    else
      dx := 0;
    if R.Top > LR.Bottom then
      dy := LR.Bottom - R.Bottom
    else if R.Bottom < LR.Top then
      dy := R.Top
    else
      dy := 0;
    ScrollBy(dx, dy);
  end;
end;

{ TVKStateHandler }

procedure TVKStateHandler.AdjustCtrl(ACtrl: TControl; AVKBounds: TRectF;
  AVKVisible: Boolean);
var
  ACaretRect: TRectF;
  AForm: TCommonCustomForm;
  I: Integer;
  ADelta: Integer;
begin
  if EnableVirtalKeyboardHelper and AVKVisible and Assigned(ACtrl) then begin
    if FLastControl <> ACtrl then begin
      if Assigned(FLastControl) then
        FLastControl.RemoveFreeNotification(Self);
      FLastControl := ACtrl;
      FLastControl.FreeNotification(Self);
      {$IFDEF DEBUG}
      if Assigned(FLastControl) then
        Log.d(Format('---FLastControl: %s-----', [FLastControl.Name]))
      else
        Log.d(Format('---FLastControl: nil-----', [FLastControl.Name]));
      {$ENDIF}
    end;
    AForm := (ACtrl.Root as TCommonCustomForm);
    if FLastControlForm <> AForm then begin
      if Assigned(FLastControlForm) then
        FLastControlForm.RemoveFreeNotification(Self);
      FLastControlForm := AForm;
      FLastControlForm.FreeNotification(Self);
      FLastRect := AForm.Padding.Rect;
    end;
    if NeedAdjust(ACtrl, ACaretRect) then begin
      if (ACaretRect.Bottom > AVKBounds.Top) or (AForm.Padding.Top < 0) or
        (ACaretRect.Top < 0) then
        ADelta := Trunc(ACaretRect.Bottom - AVKBounds.Top)
      else
        ADelta := 0;
      //移不动？
      if AForm.Padding.Bottom < AVKBounds.Height then
        AForm.Padding.Rect := RectF(AForm.Padding.Left, AForm.Padding.Top - ADelta,
          AForm.Padding.Right, AForm.Padding.Bottom + ADelta);

      {$IFDEF DEBUG}
      if Assigned(AForm) then
        with AForm.Padding.Rect do
          Log.d(Format('---ACaretRect.Bottom:%.2f-AVKBounds.Top:%.2f-Form Padding: %2f %2f %2f %2f-----',
            [ACaretRect.Bottom, AVKBounds.Top, Left, Top, Right, Bottom]));
      {$ENDIF}
    end;
  end
  else if Assigned(FLastControl) then begin
    Restore;
  end;
end;

procedure TVKStateHandler.AdjustIfNeeded;
var
  ACtrl: TControl;
begin
  if IsVirtalKeyboardVisible then begin // 解决掉虚拟键盘隐藏后的问题
    ACtrl := Screen.FocusControl as TControl;
    AdjustCtrl(ACtrl, GetVirtalKeyboardBounds, True);
  end
  else
    AdjustCtrl(nil, RectF(0, 0, 0, 0), False);
end;

// 构造函数，订阅消息
constructor TVKStateHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVKMsgId := TMessageManager.DefaultManager.SubscribeToMessage
    (TVKStateChangeMessage, DoVKVisibleChanged);
  FIdleMsgId := TMessageManager.DefaultManager.SubscribeToMessage(TIdleMessage,
    DoAppIdle);
end;

// 析构函数，取消消息订阅
destructor TVKStateHandler.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVKMsgId);
  TMessageManager.DefaultManager.Unsubscribe(TIdleMessage, FIdleMsgId);
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
  if TThread.GetTickCount - FLastIdleTick > 100 then begin
    FLastIdleTick := TThread.GetTickCount;
    AdjustIfNeeded;
  end;
end;

/// 虚拟键盘可见性变更消息，调整或恢复控件位置
procedure TVKStateHandler.DoVKVisibleChanged(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
var
  AVKMsg: TVKStateChangeMessage absolute Msg;
  ACtrl: TControl;
begin
  {$IFDEF IOS}
  _IOS_VKBounds := TRectF.Create(AVKMsg.KeyboardBounds);
  {$ENDIF}
{$IFDEF ANDROID}
  {$IF RTLVersion>=33}// 10.3+
  _AndroidVKBounds := TRectF.Create(AVKMsg.KeyboardBounds);
  {$ENDIF}
{$ENDIF}
  if AVKMsg.KeyboardVisible then begin // 键盘可见
    if Screen.FocusControl <> nil then begin
      ACtrl := Screen.FocusControl.GetObject as TControl;
      AdjustCtrl(ACtrl, GetVirtalKeyboardBounds, True);
    end;
  end
  else
    AdjustCtrl(nil, RectF(0, 0, 0, 0), False);
end;

// 响应组件释放通知，以避免访问无效地址
function TVKStateHandler.NeedAdjust(ACtrl: TControl;
  var ACaretRect: TRectF): Boolean;
var
  ACaret: ICaret;
  AFlasher: IFlasher;
  ACtrlBounds, AVKBounds: TRectF;

  function ClientToParent(ARoot: TControl): TPointF;
  var
    AParent: TControl;
  begin
    AParent := ACtrl;
    Result := AFlasher.Pos;
    while AParent <> ARoot do begin
      if AParent is TCustomScrollBox then
        Result := Result - TCustomScrollBox(AParent).ViewportPosition
      else if AParent is TCustomPresentedScrollBox then
        Result := Result - TCustomPresentedScrollBox(AParent).ViewportPosition;
      Result := Result + AParent.Position.Point;
      AParent := AParent.ParentControl;
    end;
  end;

  function CaretVisible: Boolean;
  var
    pt: TPointF;
    AParent, AChild: TControl;
  begin
    pt := AFlasher.Pos;
    AChild := ACtrl;
    Result := AFlasher.Visible;
    while Assigned(AChild) and Result do begin
      if AChild is TCustomScrollBox then begin
        pt := pt - TCustomScrollBox(AChild).ViewportPosition;
        if not AChild.LocalRect.Contains(pt) then
          Result := False;
      end
      else if AChild is TCustomPresentedScrollBox then begin
        pt := pt - TCustomPresentedScrollBox(AChild).ViewportPosition;
        if not AChild.LocalRect.Contains(pt) then
          Result := False;
      end
      else if AChild.ClipChildren and not AChild.LocalRect.Contains(pt) then
        Result := False;
      pt := pt + AChild.Position.Point;
      AChild := AChild.ParentControl;
    end;
  end;

begin
  if Supports(ACtrl, ICaret, ACaret) then begin
    AVKBounds := GetVirtalKeyboardBounds;
    ACtrlBounds := ACtrl.AbsoluteRect;
    AFlasher := ACaret.GetObject.Flasher;
    if CaretVisible then begin
      ACaretRect.TopLeft := ClientToParent(nil);
      {$IF RTLVersion < 33}
      // 加上标题栏的高度
      ACaretRect.TopLeft := ACaretRect.TopLeft +
        (ACtrl.Root as TCommonCustomForm).ClientToScreen(PointF(0, 0));
      {$ENDIF}
      if FAdjusting and (not SameValue(ACaretRect.Top, FCaretTarget.Y, 1.0)) then
        Result := False;
      FAdjusting := False;
      ACaretRect.Right := ACaretRect.Left + AFlasher.Size.cx;
      ACaretRect.Bottom := ACaretRect.Top + AFlasher.Size.cy + 20; // 下面加点余量
      Result := ACaretRect.IntersectsWith(AVKBounds) or (ACaretRect.Top < 0) or
        (ACaretRect.Top > AVKBounds.Bottom);
      if Result then begin
        FCaretTarget.Y := ACaretRect.Top + ACaretRect.Bottom - AVKBounds.Top;
        FAdjusting := True;
      end;
      {$IFDEF DEBUG}
      with ACaretRect do
        Log.d(Format('----ACaretRect: %2f %2f %2f %2f-----', [Left, Top, Right, Bottom]));
      {$ENDIF}
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

// 响应组件释放通知，以避免访问无效地址
procedure TVKStateHandler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FLastControl then begin
      FLastControl.RemoveFreeNotification(Self);
      Restore;
    end else if AComponent = FLastControlForm then begin
      FLastControlForm.RemoveFreeNotification(Self);
      Restore;
    end
  end;
  inherited;
end;

procedure TVKStateHandler.Restore;
var
  AForm: TCommonCustomForm;
begin
  if Assigned(FLastControl) then
    AForm := (FLastControl.Root as TCommonCustomForm)
  else
    AForm := nil;
  if (not Assigned(AForm)) and Assigned(FLastControlForm) then
    AForm := FLastControlForm;
  if Assigned(AForm) and (AForm.Padding.Rect <> FLastRect) then
    AForm.Padding.Rect := FLastRect;

  FLastControl := nil;
  FLastControlForm := nil;
  FLastFocused := nil;

  {$IFDEF DEBUG}
  if Assigned(AForm) then
    with AForm.Padding.Rect do
      Log.d(Format('---x-Form Padding: %2f %2f %2f %2f-----', [Left, Top, Right, Bottom]));
  {$ENDIF}
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

initialization
  // 仅支持Android+IOS
  {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  VKHandler := TVKStateHandler.Create(nil);
  {$ENDIF}
  //EnableReturnKeyHook := True;

finalization
  {$IF DEFINED(ANDROID)  OR DEFINED(IOS)}
  VKHandler.DisposeOf;
  VKHandler := nil;
  {$ENDIF}

end.
