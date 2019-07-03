{*******************************************************}
{                                                       }
{       无边框窗体大小控制单元                          }
{                                                       }
{       版权所有 (C) 2017 by YangYxd                    }
{                                                       }
{*******************************************************}

{
  使用方法： 将需要控制大小的无边框窗口的基类设置为 TSizeForm 即可
}

unit UI.SizeForm;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages, FMX.Platform.Win, Winapi.MultiMon,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  FMX.StdCtrls, FMX.Effects;

type
  TResizeMode = (Normal, LTop, RTop, LBottom, RBottom, Top, Bottom, Left, Right);

  TSizeForm = class(TForm)
  private
    { Private declarations }
    FShadowForm: TCustomForm;
    FCaptureDragForm: Boolean;
    FMouseDraging: Boolean;
    FShowShadow: Boolean;
    {$IFDEF MSWINDOWS}
    FHwnd: HWND;
    {$ENDIF}
    procedure SetShowShadow(const Value: Boolean);
    function GetMonitorIndex: Integer;
  protected
    FSizeWH: Single;   // 可调节区域大小
    FMousePos, FDownPos, FResizeSize, FDownSize: TPointF;
    FResizeMode: TResizeMode;
    function PointInDragBorder(const X, Y: Single): Boolean;
    function CalcResizeMode(const X, Y: Single): TResizeMode;
    procedure UpdateCurror(const AResizeMode: TResizeMode);

    procedure DoShow; override;

    function GetShadowColor: TAlphaColor; virtual;
    function GetShadowBackgroundColor: TAlphaColor; virtual;

    procedure InitShadowForm();
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ObjectAtPoint(AScreenPoint: TPointF): IControl; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoClick: Boolean = True); override;

    /// <summary>
    /// 最小化
    /// </summary>
    procedure ShowMin();
    /// <summary>
    /// 最大化
    /// </summary>
    procedure ShowMax();
    /// <summary>
    /// 恢复
    /// </summary>
    procedure ShowReSize();

    /// <summary>
    /// 双击标题栏
    /// </summary>
    procedure DBClickTitle(Sender: TObject);

    property ShadowForm: TCustomForm read FShadowForm;
    property MonitorIndex: Integer read GetMonitorIndex;
  published
    property CaptureDragForm: Boolean read FCaptureDragForm write FCaptureDragForm;
    property SizeWH: Single read FSizeWH write FSizeWH;
    /// <summary>
    /// 是否显示阴影
    /// </summary>
    property ShowShadow: Boolean read FShowShadow write SetShowShadow default False;
  end;


implementation

uses
  System.Generics.Collections;

type
  TColorView = class(TControl)
  protected
    FColor: TAlphaColor;
    procedure Paint; override;
  end;

  TShadowForm = class(TCustomForm)
  private
    [Weak] FOwner: TSizeForm;
    FShadowSize: Integer;
    FView: TColorView;
    FShadow: TShadowEffect;

    FIsShow: Boolean;

    {$IFDEF MSWINDOWS}
    FHwnd, FMHwnd: HWND; // 保存窗口句柄
    FOldWndProc: LONG; // 保存原始的消息处理函数
    {$ENDIF}
  protected
    procedure InitializeNewForm; override;
    procedure InitView(const BgColor, ShadowColor: TAlphaColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$IFDEF MSWINDOWS}
    function ParentWindowProc(Wnd: HWND; Msg: UINT; wParam: wParam; lParam: lParam): LRESULT;
    {$ENDIF}

    procedure DoShow; override;
    procedure UpdateBounds(ALeft, ATop, AWidth, AHeight: Integer; Flags: Integer = 0);

    property Owner: TSizeForm read FOwner;
    property ShadowSize: Integer read FShadowSize write FShadowSize;
    property Shadow: TShadowEffect read FShadow;
  end;

{$IFDEF MSWINDOWS}
const
  WM_SYNC_SIZE = WM_USER + 920;
type

  TMonitor = record
    Handle: HMONITOR;
    MonitorNum: Integer;
  end;

var
  MonitorList: TList<TMonitor>;
  FormMap: TDictionary<HWND, TShadowForm> = nil;

function EnumMonitorsProc(hm: HMONITOR; dc: HDC; r: PRect; Data: Pointer): Boolean; stdcall;
var
  M: TMonitor;
begin
  M.Handle := hm;
  M.MonitorNum := MonitorList.Count;
  MonitorList.Add(M);
  Result := True;
end;

function WindowProc(wnd: HWND; Msg: UINT; wParam: wParam; lParam: lParam): LRESULT; stdcall;
var
  F: TShadowForm;
begin
  if FormMap.TryGetValue(wnd, F) then
    Result := F.ParentWindowProc(wnd, Msg, wParam, lParam)
  else
    Result := 0;
end;

procedure InitMonitorList();
begin
  EnumDisplayMonitors(0, nil, @EnumMonitorsProc, 0);
end;
{$ENDIF}

function TSizeForm.CalcResizeMode(const X, Y: Single): TResizeMode;
begin
  Result := TResizeMode.Normal;
  if (X < 0) and (Y < 0) then
    Exit;
  if WindowState <> TWindowState.wsNormal then
    Exit;
  if (X > FSizeWH) and (X <= Width - FSizeWH) then begin
    if (Y < FSizeWH) then
      Result := TResizeMode.Top
    else if (Y >= Height - FSizeWH) then
      Result := TResizeMode.Bottom
  end else if (Y > FSizeWH) and (Y < Height - FSizeWH) then begin
    if X <= FSizeWH then
      Result := TResizeMode.Left
    else if X >= Width - FSizeWH then
      Result := TResizeMode.Right
  end else if (X <= FSizeWH) and (Y <= FSizeWH) then
    Result := TResizeMode.LTop
  else if (X >= Width - FSizeWH) and (Y <= FSizeWH) then
    Result := TResizeMode.RTop
  else if (X <= FSizeWH) and (Y >= Height - FSizeWH) then
    Result := TResizeMode.LBottom
  else if (X >= Width - FSizeWH) and (Y >= Height - FSizeWH) then
    Result := TResizeMode.RBottom;
end;

constructor TSizeForm.Create(AOwner: TComponent);
begin
  inherited;
  FSizeWH := 10;
  FShadowForm := nil;
end;

procedure TSizeForm.DBClickTitle(Sender: TObject);
begin
  if WindowState = TWindowState.wsNormal then begin
    ShowMax();
  end else begin
    ShowReSize();
  end;
end;

destructor TSizeForm.Destroy;
begin
  FreeAndNil(FShadowForm);
  inherited;
end;

procedure TSizeForm.DoShow;
begin
  inherited DoShow;
  {$IFDEF MSWINDOWS}
  FHwnd := FmxHandleToHWND(Handle);
  {$ENDIF}
  InitShadowForm;
end;

function TSizeForm.GetMonitorIndex: Integer;
{$IFDEF MSWINDOWS}
var
  HM: HMonitor;
  I: Integer;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  HM := MonitorFromWindow(FHwnd, MONITOR_DEFAULTTONEAREST);
  for I := 0 to MonitorList.Count - 1 do
    if MonitorList[I].Handle = HM then begin
      Result := I;
      Exit;
    end;
  {$ENDIF}
end;

procedure TSizeForm.InitShadowForm;
begin
  if Assigned(FShadowForm) or (not FShowShadow) then
    Exit;
  if (csLoading in ComponentState) or (csDesigning in ComponentState) then
    Exit;
  if BorderStyle <> TFmxFormBorderStyle.None then
    Exit;
  FShadowForm := TShadowForm.Create(nil);
  TShadowForm(FShadowForm).FOwner := Self;
  TShadowForm(FShadowForm).InitView(GetShadowBackgroundColor, GetShadowColor);
  FShadowForm.Show;
end;

procedure TSizeForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  if (BorderStyle = TFmxFormBorderStyle.None) and (WindowState = TWindowState.wsNormal) then begin
    if (csDesigning in ComponentState) then Exit;
    if (Button = TMouseButton.mbLeft) and (Shift = [ssLeft]) then begin
      if FullScreen then
        Exit;
      if not PointInDragBorder(X, Y) then begin
        if FCaptureDragForm then begin
          FMouseDraging := True;
          StartWindowDrag;
        end;
        Exit;
      end;
      FResizeMode := CalcResizeMode(X, Y);
      UpdateCurror(FResizeMode);
      if FResizeMode = TResizeMode.Normal then
        Exit;
      FMousePos := PointF(X, Y);
      FDownPos := FMousePos;
      FResizeSize := PointF(Width, Height);
      FDownSize := FResizeSize;
      FWinService.SetCapture(Self);
    end;
  end;
end;

procedure TSizeForm.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
begin
  if (FResizeMode <> TResizeMode.Normal) and (ssLeft in Shift) then begin
    Engage;
    try
      P.X := Left;
      P.Y := Top;
      case FResizeMode of
        TResizeMode.LTop:
          begin
            P.X := P.X + (X - FDownPos.X);
            P.Y := P.Y + (Y - FDownPos.Y);
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X - (X - FDownPos.X)));
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y - (Y - FDownPos.Y)));
          end;
        TResizeMode.RTop:
          begin
            P.Y := P.Y + (Y - FDownPos.Y);
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X));
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y - (Y - FDownPos.Y)));
          end;
        TResizeMode.LBottom:
          begin
            P.X := P.X + (X - FDownPos.X);
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X - (X - FDownPos.X)));
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y));
          end;
        TResizeMode.RBottom:
          begin
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X));
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y));
          end;
        TResizeMode.Top:
          begin
            P.Y := P.Y + (Y - FDownPos.Y);
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y) - (Y - FDownPos.Y));
          end;
        TResizeMode.Bottom:
          begin
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y));
          end;
        TResizeMode.Left:
          begin
            P.X := P.X + (X - FDownPos.X);
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X - (X - FDownPos.X)));
          end;
        TResizeMode.Right:
          begin
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X));
          end;
      end;
      {$IFDEF MSWINDOWS}
      if Assigned(FShadowForm) then begin
        Lockwindowupdate(FHwnd);
        SetWindowPos(FHwnd, HWND_TOP, Round(P.X), Round(P.Y), Round(FResizeSize.X), Round(FResizeSize.Y), SWP_NOREDRAW or SWP_NOACTIVATE or SWP_NOZORDER or SWP_DEFERERASE);
        Lockwindowupdate(0);
        TShadowForm(FShadowForm).UpdateBounds(Round(P.X), Round(P.Y), Round(FResizeSize.X), Round(FResizeSize.Y));
        UpdateWindow(FHwnd);
      end else
        SetBounds(Round(P.X), Round(P.Y), Round(FResizeSize.X), Round(FResizeSize.Y));
      {$ELSE}
      SetBounds(Round(P.X), Round(P.Y), Round(FResizeSize.X), Round(FResizeSize.Y));
      {$ENDIF}
      FMousePos := PointF(X, Y);
    finally
      Disengage;
    end;
  end else begin
    inherited;
    if (BorderStyle = TFmxFormBorderStyle.None) then begin
      if Shift = [] then
        UpdateCurror(CalcResizeMode(X, Y))
      else
        Cursor := crArrow;
    end else if Cursor <> crDefault then
      Cursor := crDefault;
  end;
end;

procedure TSizeForm.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single;
  DoClick: Boolean);
begin
  if FMouseDraging then begin
    FMouseDraging := False;
    ReleaseCapture;
  end;
  if FResizeMode <> TResizeMode.Normal then begin
    FResizeMode := TResizeMode.Normal;
    ReleaseCapture;
  end;
  inherited;
end;

function TSizeForm.ObjectAtPoint(AScreenPoint: TPointF): IControl;

  function Innder(const P: TPointF): IControl;
  begin
    if (P.X < 0) or (P.Y < 0) or PointInDragBorder(P.X, P.Y) then
      Result := nil
    else
      Result := inherited;
  end;

begin
  if (BorderStyle = TFmxFormBorderStyle.None) and (WindowState = TWindowState.wsNormal) and (FSizeWH > 1) then
    Result := Innder(ScreenToClient(AScreenPoint))
  else
    Result := inherited;
end;

function TSizeForm.PointInDragBorder(const X, Y: Single): Boolean;
begin
  Result := (FSizeWH > 1) and ((X < FSizeWH) or (X >= Width - FSizeWH) or (Y < FSizeWH) or (Y >= Height - FSizeWH));
end;

procedure TSizeForm.SetShowShadow(const Value: Boolean);
begin
  if FShowShadow <> Value then begin
    FShowShadow := Value;
    if (csDesigning in ComponentState) then
      Exit;
    if not Value then
      FreeAndNil(FShadowForm);
  end;
end;

procedure TSizeForm.ShowMax;
begin
  Self.WindowState := TWindowState.wsMaximized;
  {$IFDEF MSWINDOWS}
  //PostMessage(FHwnd, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
  {$ELSE}
  {$ENDIF}
end;

procedure TSizeForm.ShowMin;
begin
  {$IFDEF MSWINDOWS}
  PostMessage(FHwnd, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  {$ELSE}
  Self.WindowState := TWindowState.wsMinimized;
  {$ENDIF}
end;

procedure TSizeForm.ShowReSize;
begin
  Self.WindowState := TWindowState.wsNormal;
end;

function TSizeForm.GetShadowBackgroundColor: TAlphaColor;
begin
  Result := $ffffffff;
end;

function TSizeForm.GetShadowColor: TAlphaColor;
begin
  Result := $7f000000;
end;

procedure TSizeForm.UpdateCurror(const AResizeMode: TResizeMode);
const
  CCursor: array [TResizeMode] of Integer = (
    crArrow, crSizeNWSE, crSizeNESW, crSizeNESW, crSizeNWSE, crSizeNS, crSizeNS, crSizeWE, crSizeWE
  );
begin
  Cursor := CCursor[AResizeMode]
end;

{ TShadowForm }

constructor TShadowForm.Create(AOwner: TComponent);
begin
  FShadowSize := 12;
  FHwnd := 0;
  inherited;
  SetDesigning(False, False);
  Self.BorderStyle := TFmxFormBorderStyle.None;
  Self.Visible := False;
  Self.Transparency := True;
  Self.WindowState := TWindowState.wsNormal;
end;

destructor TShadowForm.Destroy;
begin
  inherited;
  {$IFDEF MSWINDOWS}
  if FormMap.ContainsKey(FHwnd) then
    FormMap.Remove(FHwnd);
  {$ENDIF}
end;

procedure TShadowForm.DoShow;
begin
  inherited;

  FIsShow := True;
  {$IFDEF MSWINDOWS}
  FMHwnd := FmxHandleToHWND(Handle);
  SetWindowLong(FMHwnd, GWL_EXSTYLE, GetWindowLong(FMHwnd, GWL_EXSTYLE)
    or WS_EX_LAYERED or WS_EX_TRANSPARENT or WS_EX_TOPMOST or WS_EX_NOACTIVATE or WS_EX_TOOLWINDOW);
  {$ENDIF}

  if Assigned(FOwner) then begin

    UpdateBounds(FOwner.Left, FOwner.Top, FOwner.Width, FOwner.Height);

    {$IFDEF MSWINDOWS}
    FHwnd := FmxHandleToHWND(FOwner.Handle);
    FormMap.AddOrSetValue(FHwnd, Self);

    FOldWndProc := GetWindowLongPtr(FHwnd, GWL_WNDPROC);
    SetWindowLongPtr(FHwnd, GWL_WNDPROC, NativeInt(@WindowProc));
    {$ENDIF}

  end;
end;

procedure TShadowForm.InitializeNewForm;
begin
  inherited;
  SetDesigning(True, False);
end;

procedure TShadowForm.InitView(const BgColor, ShadowColor: TAlphaColor);
begin
  FView := TColorView.Create(Self);
  FView.FColor := BgColor;
  FView.Margins.Rect := RectF(FShadowSize, FShadowSize, FShadowSize, FShadowSize);
  FView.Align := TAlignLayout.Client;
  FView.Parent := Self;

  FShadow := TShadowEffect.Create(Self);
  FShadow.Direction := 90;
  FShadow.Opacity := 1;
  FShadow.Softness := 0.35;
  FShadow.Distance := 0;
  FShadow.ShadowColor := ShadowColor;
  FShadow.Parent := FView;
  FShadow.Enabled := True;
end;

procedure TShadowForm.UpdateBounds(ALeft, ATop, AWidth, AHeight, Flags: Integer);
var
  R: TRect;
begin
  R.Left := ALeft - FShadowSize;
  R.Top := ATop - FShadowSize;
  R.Right := R.Left + FShadowSize * 2 + AWidth;
  R.Bottom := R.Top + FShadowSize * 2 + AHeight;
  {$IFDEF MSWINDOWS}
  Flags := Flags or SWP_NOREDRAW or SWP_NOACTIVATE or SWP_NOZORDER or SWP_DEFERERASE;
  SetWindowPos(FMHwnd, HWND_TOP, R.Left, R.Top, R.Width, R.Height, Flags);
  //UpdateWindow(FMHwnd);
  {$ELSE}
  SetBounds(R);
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function TShadowForm.ParentWindowProc(Wnd: HWND; Msg: UINT; wParam: wParam;
  lParam: lParam): LRESULT;
begin
  case Msg of
    WM_GETMINMAXINFO:
      begin
        if (FOwner.MonitorIndex = 0) then begin
          PMinMaxInfo(lParam).ptMaxSize.X := Screen.WorkAreaWidth;
          PMinMaxInfo(lParam).ptMaxSize.Y := Screen.WorkAreaHeight;
          Result := 0;
          Exit;
        end;
      end;
  end;
  Result := CallWindowProc(Ptr(FOldWndProc), Wnd, Msg, wParam, lParam);
  case Msg of
    WM_MOVE:
      begin
        if (FOwner.WindowState = TWindowState.wsNormal) and (Abs(Self.Width - FOwner.Width) > FShadowSize * 2) then
          UpdateBounds(FOwner.Left, FOwner.Top, FOwner.Width, FOwner.Height)
        else
          SetWindowPos(FMHwnd, HWND_TOP, FOwner.Left - FShadowSize, FOwner.Top - FShadowSize, 0, 0,
            SWP_NOSIZE or SWP_NOREDRAW or SWP_NOACTIVATE or SWP_NOZORDER or SWP_DEFERERASE);
      end;
    WM_ACTIVATE, WM_NCACTIVATE:
      begin
        UpdateBounds(FOwner.Left, FOwner.Top, FOwner.Width, FOwner.Height, 0);
      end;
    WM_SHOWWINDOW:
      begin
        SendMessage(FMHwnd, MSG, wParam, lParam);
      end;
  end;
end;
{$ENDIF}

{ TColorView }

procedure TColorView.Paint;
begin
  //inherited Paint;
  Canvas.Fill.Color := FColor;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.FillRect(ClipRect, 0, 0, [], 1);
end;

initialization
  {$IFDEF MSWINDOWS}
  FormMap := TDictionary<HWND, TShadowForm>.Create();
  MonitorList := TList<TMonitor>.Create;
  InitMonitorList();
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  FreeAndNil(FormMap);
  FreeAndNil(MonitorList);
  {$ENDIF}

end.
