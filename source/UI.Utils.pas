{*******************************************************}
{                                                       }
{       FMXUI 常用函数库                                }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Utils;

interface

uses
  {$IFDEF MSWINDOWS}Windows, ShellAPI, {$ENDIF}
  {$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.Jni,
  Androidapi.JNI.Media,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.JNI.App,
  FMX.Helpers.Android,
  {$ENDIF}
  {$IFDEF IOS}
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  iOSapi.CoreGraphics,
  Macapi.Helpers,
  FMX.Helpers.iOS,
  {$ENDIF}
  FMX.BehaviorManager,
  System.NetEncoding,
  FMX.MediaLibrary,
  System.Character,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement;

// 获取当前时间
function GetTimestamp: Int64;
// 获取当前时间，结果表示当时时间距离1970年1月1日0时0分0秒0毫秒的毫秒数
function CurrentTimeMillis(): Int64;
// Java时间戳转为Delphi时间
function JavaTimeToDateTime(const V: Int64): TDateTime;

// 插值函数 - 颜色
function LerpColor(const A, B: TAlphaColor; T: Single): TAlphaColor;
// 插值函数 - 浮点数
function LerpFolat(const A, B: Double; T: Single): Double;
// 获取当前设备PPI
function GetPPI(Context: TFmxObject): Single;

// 隐藏手机号中间的四位数字
function HideMobilePhone(const Mobile: string ):String;
// 隐藏用户中文姓名中的前部内容，只显示最后一个字
function HideUserName(const Name: string): string;
// 检测是否是有效的手机号码
function IsMobileNumber(const Value: string): boolean;
// 从字符串中提取出第一个有效的手机号码
function TrimMobileNumber(const Value: string): string;
// 格式化手机号码为 130 1000 1000 这种样式
function FormatMobile(const Phone: string): string;
// 将一个代表流量大小的数字转为字符串
function GetFlowStr(const V: Double): string;
// 获取版本信息
function GetVersionName(): string;

// 打开网址
function OpenURL(const URL: string): Boolean;
// 调用移动平台的分享功能
procedure Share(const AControl: TControl; const Title, Msg: string);

function RectD(const Left, Top, Right, Bottom: Double): TRectD; overload;
function RectD(const R: TRectF): TRectD; overload;
function OffsetRectD(var R: TRectD; const DX, DY: Double): Boolean;
function GetRectF(const R: TRectD): TRectF;


implementation

uses
  {$IFNDEF MSWINDOWS}System.Diagnostics, {$ENDIF}SyncObjs;

{$IFDEF MSWINDOWS}
type
  TGetTickCount64 = function: Int64;
  TGetSystemTimes = function(var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;
{$ENDIF}
var
  {$IFDEF MSWINDOWS}
  GetTickCount64: TGetTickCount64;
  //WinGetSystemTimes: TGetSystemTimes;
  _StartCounter: Int64;
  _PerfFreq: Int64;
  {$ELSE}
  _Watch: TStopWatch;
  {$ENDIF}
  CacleVersion: string = '';

function GetTimestamp: Int64;
begin
  {$IFDEF MSWINDOWS}
  if _PerfFreq > 0 then begin
    QueryPerformanceCounter(Result);
    Result := Trunc((Result - _StartCounter) / _PerfFreq * 1000);
  end else if Assigned(GetTickCount64) then
    Result := (GetTickCount64 - _StartCounter)
  else
    Result := (GetTickCount - _StartCounter)
  {$ELSE}
  Result := _Watch.Elapsed.Ticks div 10000;
  {$ENDIF}
end;

function CurrentTimeMillis(): Int64;
const
  UnixDateDelta = 25569;
  SecsPerDay = 86400000;
begin
  Result := Round((Now() - UnixDateDelta) * SecsPerDay) - 28800000;
end;

function JavaTimeToDateTime(const V: Int64): TDateTime;
const
  UnixDateDelta = 25569;
  SecsPerDay = 86400000;
begin
  Result := ((V + 28800000) / SecsPerDay) + UnixDateDelta;
end;

function HideMobilePhone(const Mobile: string ):String;
begin
  Result := Copy(Mobile, 1, 3) + '****' + Copy(Mobile, 8, 4);
end;

type
  TRGBA = record
    R, G, B, A: Byte;
  end;
  PRGBA = ^TRGBA;

function LerpColor(const A, B: TAlphaColor; T: Single): TAlphaColor;
var
  CA, CB, CR: PRGBA;
begin
  if T <= 0 then
    Result := A
  else if T >= 1 then
    Result := B
  else begin
    CA := @A;
    CB := @B;
    CR := @Result;
    CR.A := CA.A + Round((CB.A - CA.A) * T);
    CR.G := CA.G + Round((CB.G - CA.G) * T);
    CR.B := CA.B + Round((CB.B - CA.B) * T);
    CR.R := CA.R + Round((CB.R - CA.R) * T);
  end;
end;

function LerpFolat(const A, B: Double; T: Single): Double;
begin
  if T <= 0 then
    Result := A
  else if T >= 1 then
    Result := B
  else begin
    Result := A + (B - A) * T;
  end;
end;

function RectD(const Left, Top, Right, Bottom: Double): TRectD;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

function RectD(const R: TRectF): TRectD; overload;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Bottom := R.Bottom;
  Result.Right := R.Right;
end;

function OffsetRectD(var R: TRectD; const DX, DY: Double): Boolean;
begin
{$EXCESSPRECISION OFF}
  if @R <> nil then // Test to increase compatiblity with Windows
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
    Result := True;
  end
  else
    Result := False;
{$EXCESSPRECISION ON}
end;

function GetRectF(const R: TRectD): TRectF;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Right;
  Result.Bottom := R.Bottom;
end;

function GetPPI(Context: TFmxObject): Single;
var
  DeviceBehavior: IDeviceBehavior;
begin
  if TBehaviorServices.Current.SupportsBehaviorService(IDeviceBehavior, DeviceBehavior, Context) then
    Result := DeviceBehavior.GetDisplayMetrics(Context).PixelsPerInch
  else
    Result := 160;
end;

function GetFlowStr(const V: Double): string;
begin
  if V >= 1024 then begin
    if (Round(V) mod 1024) = 0 then
      Result := Format('%.0fGB', [V / 1024])
    else
      Result := Format('%.2fGB', [V / 1024])
  end else
    Result := Format('%.0fMB', [V])
end;

function HideUserName(const Name: string): string;
var
  P, PMax: PChar;
begin
  Result := Name;
  P := PChar(Result);
  PMax := P + Length(Result) - 1;
  while P < PMax do begin
    P^ := '*';
    Inc(P);
  end;
end;

function IsMobileNumber(const Value: string): boolean;
var
  P: PChar;
  I: Integer;
begin
  Result := False;
  if Length(Value) <> 11 then Exit;
  P := PChar(Value);
  if (P^ <> '1') then Exit;
  Inc(P);
  if not P^.IsInArray(['3', '4','5', '7', '8']) then Exit;
  //if not System.SysUtils.CharInSet(P^, ['3', '4','5', '7', '8']) then Exit;
  //if not (P^ in ['3', '4','5', '7', '8']) then Exit;
  Inc(P);
  for I := 0 to 8 do begin
    if (P^ < '0') or (P^ > '9') then Exit;
    Inc(P);
  end;
  Result := True;
end;

function TrimMobileNumber(const Value: string): string;
var
  I: Integer;
  P, V, VS: PChar;
begin
  SetLength(Result, Length(Value));
  V := PChar(Result);
  P := PChar(Value);
  VS := V;
  for I := 1 to Length(Value) do begin
    if (V = VS) and (P^ = '1') then begin
      V^ := P^;
      Inc(V);
    end else if (V > VS) and (P^ >= '0') and (P^ <= '9') then begin
      V^ := P^;
      Inc(V);
    end;
    if V - VS = 11 then Break;
    Inc(P);
  end;
  SetLength(Result, V - VS);
end;

function FormatMobile(const Phone: string): string;
begin
  if Length(Phone) = 11 then
    Result := Copy(Phone, 1, 3) + ' ' + Copy(Phone, 4, 4) + ' ' + Copy(Phone, 8, 4)
  else
    Result := Phone;
end;

function GetVersionName(): string;
begin
  Result := CacleVersion;
end;

function InnerGetVersionName(): string;

  {$IFDEF MSWINDOWS}
  function GetVerInfo(sPath, sInfo:string): string;
  var
    BufSize, Len: DWORD;
    Buf: PChar;
    pBuf, pValue: Pointer;
    VIndexStr: string;
  begin
    Result := '';
    BufSize := GetFileVersionInfoSize(PChar(sPath), BufSize);

    if BufSize > 0 then
    begin
      GetMem(Buf, BufSize);
      try
        if GetFileVersionInfo(PChar(sPath), 0, BufSize, Buf) then
        begin
          VerQueryValue(Buf, PChar('\VarFileInfo\Translation'), pBuf, Len);
          VIndexStr := InttoHex(LoWord(Integer(pBuf^)), 4) + InttoHex(HiWord(Integer(pBuf^)), 4);
          if VerQueryValue(Buf, PChar('StringFileInfo\' + VIndexStr + '\' + sInfo), pValue, Len) then
            Result := PChar(pValue);
        end;
      finally
        FreeMem(Buf, BufSize);
      end;
    end;
  end;

  function InnerWin(): string;
  begin
    Result := GetVerInfo(ParamStr(0), 'FileVersion');
  end;
  {$ENDIF}

  {$IFDEF ANDROID}
  function InnerAndroid(): string;
  var
    PackageInfo: JPackageInfo;
    PackageName: JString;
  begin
    PackageName := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}.getPackageName;
    PackageInfo := {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF}.
      getPackageManager.getPackageInfo(PackageName, 0);
    if Assigned(PackageInfo) then
      Result := JStringToString(PackageInfo.versionName)
    else
      Result := '';
  end;
  {$ENDIF}

  {$IFDEF IOS}
  function InnerIOS(): string;
  var
    AppNameKey: Pointer;
    NSAppName: NSString;
    AppBundle: NSBundle;
  begin
    AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
    // AppNameKey := (NSSTR('CFBundleVersion') as ILocalObject).GetObjectID;
    AppNameKey := (StrToNSStr('CFBundleVersion') as ILocalObject).GetObjectID;
    NSAppName := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppNameKey));
    Result := UTF8ToString(NSAppName.UTF8String);
  end;
  {$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  Result := InnerWin();
  {$ENDIF}
  {$IFDEF ANDROID}
  Result := InnerAndroid();
  {$ENDIF}
  {$IFDEF IOS}
  Result := InnerIOS();
  {$ENDIF}
end;

function OpenURL(const URL: string): Boolean;
{$IFDEF ANDROID}
begin
  try
    //Intent.setClassName(
    //  StringToJString('com.android.browser'),
    //  StringToJString('com.android.browser.BrowserActivity'));
    TAndroidHelper.Activity.startActivity(
      TJIntent.Create()
        .setAction(StringToJString('android.intent.action.VIEW'))
        .setData(TJnet_Uri.JavaClass.parse(StringToJString(URL)))
      );
    Result := True;
  except
    Result := False;
  end;
end;
{$ELSE} {$IFDEF IOS}
var NSU: NSUrl;
begin
  NSU := StrToNSUrl(URL);
  if SharedApplication.canOpenURL(NSU) then
    exit(SharedApplication.openUrl(NSU))
  else
    Result := False;
end;
{$ELSE}
begin
  Result := ShellExecute(0, 'OPEN', PChar(URL), nil, nil, SW_SHOWMAXIMIZED) > 32;
end;
{$ENDIF IOS}{$ENDIF ANDROID}

procedure Share(const AControl: TControl; const Title, Msg: string);

  {$IFDEF ANDROID}
  procedure Exec();
  var
    Intent: JIntent;
  begin
    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_SEND);
    Intent.setType(StringToJString('text/plain'));
    Intent.putExtra(TJIntent.JavaClass.EXTRA_SUBJECT, StringToJString(Title));
    Intent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(Msg));
    Intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK);
    TAndroidHelper.Activity.startActivity(Intent);
  end;
  {$ENDIF}

  procedure ExecShare();
  var
    FSharingService: IFMXShareSheetActionsService;
  begin
    TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetActionsService, FSharingService);
    if Assigned(FSharingService) then
      FSharingService.Share(AControl, Msg, nil);
  end;

begin
  {$IFDEF MSWINDOWS}
  //ShowMessage(FMessage);
  {$ELSE}
  ExecShare();
  {$ENDIF}
end;

initialization
  {$IFDEF MSWINDOWS}
  GetTickCount64 := GetProcAddress(GetModuleHandle(kernel32), 'GetTickCount64');
  if not QueryPerformanceFrequency(_PerfFreq) then begin
    _PerfFreq := -1;
    if Assigned(GetTickCount64) then
      _StartCounter := GetTickCount64
    else
      _StartCounter := GetTickCount;
  end else
    QueryPerformanceCounter(_StartCounter);
  {$ELSE}
    _Watch := TStopWatch.Create;
    _Watch.Start;
  {$ENDIF}
  CacleVersion := InnerGetVersionName;
  {$IFDEF ANDROID}
  {$ENDIF}

finalization
  {$IFDEF ANDROID}
  {$ENDIF}

end.
