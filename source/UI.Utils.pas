{*******************************************************}
{                                                       }
{       FMXUI 常用函数库                                }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Utils;

interface

{.$DEFINE HASHClash} // 统计Hash冲突

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
  Androidapi.JNI.Os,
  androidapi.jni.provider,
  FMX.Helpers.Android,
  {$ENDIF}
  {$IFDEF IOS}
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  iOSapi.CoreGraphics,
  Macapi.Helpers,
  FMX.Helpers.iOS,
  {$ELSE}
  {$IFDEF MACOS}
  Macapi.ObjectiveC,
  Macapi.CocoaTypes,
  Macapi.Foundation,
  Macapi.CoreGraphics,
  Macapi.Helpers,
  Macapi.AppKit,
  FMX.Helpers.Mac,
  {$ENDIF}
  {$ENDIF}
  FMX.BehaviorManager,
  System.NetEncoding,
  FMX.MediaLibrary,
  System.Character,
  System.SyncObjs,
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

// 跳过空格
procedure SkipSpace(var P: PChar);

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

// Android: 自动升级 - 获取下载的apk文件保存目录
// 由于 Android 7.0 之后系统变化，需要授权，但授权内部地址不合适
// 而且emb默认的Secure File Sharing，是授权外部存储的，不支持本函数获取的目录
// 推荐不使用本函数，自行决定保存目录，如：TPath.GetTempPath
function GetInstallDir(): string;
// Android: 自动升级 - 安装apk （由于 Android 7.0 之后系统变化，所以需要 authorities）
// 如果点击了不再提示，并拒绝的话，无法弹出，需要调整APP的未知权限处
// fix: android.os.FileUriExposedException: exposed beyond app through Intent.getData()
// Project -> Options -> Application -> Entitlement List -> Secure File Sharing -> Check it
//  - ApkFileName 安装包文件名称
//  - ApkFileDir  安装包目录 (为空则自动使用GetInstallDir路径)
//  - Authorities 在AndroidManifest.xml中application段中自定义provider中android:authorities值（留空则使用“包名+.fileprovider”）
function InstallApk(const ApkFileName: string; const ApkFileDir: string = ''; const Authorities: string = ''): Boolean;

// 调用移动平台的分享功能
procedure Share(const AControl: TControl; const Title, Msg: string);

// 统计字符串字符数
function CharCount(const S: string): Integer;

// 字符串指针转为数字
function PCharToIntDef(const S: pchar; Len: Integer; def: NativeInt = 0): NativeInt;
function PCharToFloatDef(const S: pchar; Len: Integer; def: Double = 0): Double;
function PHexToIntDef(const S: pchar; Len: Integer; def: NativeInt = 0): NativeInt;
function PCharToStr(const S: PChar; Len: Integer): string;

// Html颜色转为Color
function HtmlColorToColor(const V: string; const DefaultValue: TAlphaColor = 0): TAlphaColor;
function Text2Color(const s:string): TAlphaColor;
function Hex2Color(const s: string): TAlphaColor;
function RgbStrToColor(const s: string): TAlphaColor;

function RectD(const Left, Top, Right, Bottom: Double): TRectD; overload;
function RectD(const R: TRectF): TRectD; overload;
function RectSF(const Left, Top, Width, Height: Single): TRectF;
function OffsetRectD(var R: TRectD; const DX, DY: Double): Boolean;
function GetRectF(const R: TRectD): TRectF;

// 计算角度(0~360)
function GetAngle(const CX, CY, X, Y: Single): Single;

// 判断两个方法是否相等
function EqulsMethod(const A, B: TNotifyEvent): Boolean;

type
  {$if CompilerVersion < 23}
  NativeUInt = Cardinal;
  NativeInt = Integer;
  {$ifend}
  Number = NativeInt;
  NumberU = NativeUInt;
  /// 桶内元素的哈希值列表
  THashType = UInt64;

  PPIntHashItem = ^PIntHashItem;
  PIntHashItem = ^TIntHashItem;
  TIntHashItem = record
    Next: PIntHashItem;
    Key: THashType;
    case Value: Int64 of
      0: (AsNumber: Number);
      1: (AsDouble: Double);
      2: (AsInt64: Int64);
      3: (AsPointer: Pointer);
  end;

  /// <summary>删除哈希表一个元素的通知</summary>
  /// <param name="ATable">哈希表对象</param>
  /// <param name="AHash">要删除的对象的哈希值</param>
  /// <param name="AData">要删除的对象数据指针</param>
  TYXDIntHashItemFreeNotify = procedure (Item: PIntHashItem) of object;

  TOnCanDelete = reference to function (const Key: THashType): Boolean;

  TIntHash = class
  private
    FCount: Integer;
    FOnFreeItem: TYXDIntHashItemFreeNotify;
    function GetBucketsCount: Integer;
    function GetValueItem(const Key: THashType): Number;
    procedure SetValueItem(const Key: THashType; const Value: Number);
    function GetItem(const Key: THashType): TIntHashItem;
  protected
    FMaxClash: Integer;
    FLenBuckets: NativeUInt;
  public
    Buckets: array of PIntHashItem;
    constructor Create(Size: Cardinal = 331);
    destructor Destroy; override;

    function Find(const Key: THashType): PPIntHashItem;

    procedure Add(const Key: THashType; const Value: Number); overload;
    procedure Add(const Key: THashType; const Value: Double); overload;
    procedure Add(const Key: THashType; const Value: Int64); overload;
    procedure Add(const Key: THashType; const Value: Pointer); overload;
    procedure Add(const Key: THashType; const Value: TObject); overload;

    procedure AddOrUpdate(const Key: THashType; const Value: Number); overload;
    procedure AddOrUpdate(const Key: THashType; const Value: Double); overload;
    procedure AddOrUpdate(const Key: THashType; const Value: Int64); overload;
    procedure AddOrUpdate(const Key: THashType; const Value: Pointer); overload;
    procedure AddOrUpdate(const Key: THashType; const Value: TObject); overload;

    function Modify(const Key: THashType; const Value: Number): Boolean; overload;
    function Modify(const Key: THashType; const Value: Double): Boolean; overload;
    function Modify(const Key: THashType; const Value: Int64): Boolean; overload;
    function Modify(const Key: THashType; const Value: Pointer): Boolean; overload;
    function Modify(const Key: THashType; const Value: TObject): Boolean; overload;

    function TryGetValue(const Key: THashType; out Data: TObject): Boolean; overload;
    function TryGetValue(const Key: THashType; out Data: Pointer): Boolean; overload;
    function TryGetValue(const Key: THashType; out Data: Int64): Boolean; overload;
    function TryGetValue(const Key: THashType; out Data: Double): Boolean; overload;
    function TryGetValue(const Key: THashType; out Data: Number): Boolean; overload;

    function ValueOf(const Key: THashType; const DefaultValue: Number = -1): Number;

    function GetInt(const Key: THashType; const DefaultValue: Number = -1): Number;
    function GetInt64(const Key: THashType; const DefaultValue: Int64 = -1): Int64;
    function GetFolat(const Key: THashType; const DefaultValue: Double = -1): Double;
    function GetPointer(const Key: THashType): Pointer;
    function GetObject(const Key: THashType): TObject;

    procedure Clear; overload;
    procedure Clear(const CanDelete: TOnCanDelete); overload;

    function Remove(const Key: THashType): Boolean;
    function Exists(const Key: THashType): Boolean;
    function ContainsKey(const Key: THashType): Boolean;

    property Values[const Key: THashType]: Number read GetValueItem write SetValueItem;
    property Items[const Key: THashType]: TIntHashItem read GetItem; default;

    property Count: Integer read FCount;
    property BucketsCount: Integer read GetBucketsCount;
    property OnFreeItem: TYXDIntHashItemFreeNotify read FOnFreeItem write FOnFreeItem;
  end;

implementation

uses
  {$IFNDEF MSWINDOWS}System.Diagnostics, {$ENDIF} Math;

const
  Convert: array[0..255] of Integer =
    (
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
     );

{$IFDEF ANDROID}
type
  JFileProvider = interface;

  JFileProviderClass = interface(JObjectClass)
    ['{FACE4BE8-CC0C-4E4F-B7BE-CD3C13295C5D}']
    function delete(uri : Jnet_Uri; selection : JString; selectionArgs : TJavaArray<JString>) : Integer; cdecl;// (Landroid/net/Uri;Ljava/lang/String;[Ljava/lang/String;)I A: $1
    function getType(uri : Jnet_Uri) : JString; cdecl;                              // (Landroid/net/Uri;)Ljava/lang/String; A: $1
    function getUriForFile(context : JContext; authority : JString; &file : JFile) : Jnet_Uri; cdecl;// (Landroid/content/Context;Ljava/lang/String;Ljava/io/File;)Landroid/net/Uri; A: $9
    function init : JFileProvider; cdecl;                                       // ()V A: $1
    function insert(uri : Jnet_Uri; values : JContentValues) : Jnet_Uri; cdecl;         // (Landroid/net/Uri;Landroid/content/ContentValues;)Landroid/net/Uri; A: $1
    function onCreate : boolean; cdecl;                                         // ()Z A: $1
    function openFile(uri : Jnet_Uri; mode : JString) : JParcelFileDescriptor; cdecl;// (Landroid/net/Uri;Ljava/lang/String;)Landroid/os/ParcelFileDescriptor; A: $1
    function query(uri : Jnet_Uri; projection : TJavaArray<JString>; selection : JString; selectionArgs : TJavaArray<JString>; sortOrder : JString) : JCursor; cdecl;// (Landroid/net/Uri;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor; A: $1
    function update(uri : Jnet_Uri; values : JContentValues; selection : JString; selectionArgs : TJavaArray<JString>) : Integer; cdecl;// (Landroid/net/Uri;Landroid/content/ContentValues;Ljava/lang/String;[Ljava/lang/String;)I A: $1
    procedure attachInfo(context : JContext; info : JProviderInfo) ; cdecl;     // (Landroid/content/Context;Landroid/content/pm/ProviderInfo;)V A: $1
  end;

  //[JavaSignature('android/support/v4/content/FileProvider$SimplePathStrategy')]
  [JavaSignature('android/support/v4/content/FileProvider')]
  JFileProvider = interface(JObject)
    ['{03ED248A-3365-42CC-AD1E-ACA3A69269EF}']
    function delete(uri : Jnet_Uri; selection : JString; selectionArgs : TJavaArray<JString>) : Integer; cdecl;// (Landroid/net/Uri;Ljava/lang/String;[Ljava/lang/String;)I A: $1
    function getType(uri : Jnet_Uri) : JString; cdecl;                              // (Landroid/net/Uri;)Ljava/lang/String; A: $1
    function insert(uri : Jnet_Uri; values : JContentValues) : Jnet_Uri; cdecl;         // (Landroid/net/Uri;Landroid/content/ContentValues;)Landroid/net/Uri; A: $1
    function onCreate : boolean; cdecl;                                         // ()Z A: $1
    function openFile(uri : Jnet_Uri; mode : JString) : JParcelFileDescriptor; cdecl;// (Landroid/net/Uri;Ljava/lang/String;)Landroid/os/ParcelFileDescriptor; A: $1
    function query(uri : Jnet_Uri; projection : TJavaArray<JString>; selection : JString; selectionArgs : TJavaArray<JString>; sortOrder : JString) : JCursor; cdecl;// (Landroid/net/Uri;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor; A: $1
    function update(uri : Jnet_Uri; values : JContentValues; selection : JString; selectionArgs : TJavaArray<JString>) : Integer; cdecl;// (Landroid/net/Uri;Landroid/content/ContentValues;Ljava/lang/String;[Ljava/lang/String;)I A: $1
    procedure attachInfo(context : JContext; info : JProviderInfo) ; cdecl;     // (Landroid/content/Context;Landroid/content/pm/ProviderInfo;)V A: $1
  end;

  TJFileProvider = class(TJavaGenericImport<JFileProviderClass, JFileProvider>)
  end;

const
  TJFileProviderMETA_DATA_FILE_PROVIDER_PATHS = 'android.support.FILE_PROVIDER_PATHS';
  TJFileProviderTAG_ROOT_PATH = 'root-path';
  TJFileProviderTAG_FILES_PATH = 'files-path';
  TJFileProviderTAG_CACHE_PATH = 'cache-path';
  TJFileProviderTAG_EXTERNAL = 'external-path';
  TJFileProviderATTR_NAME = 'name';
  TJFileProviderATTR_PATH = 'path';
{$ENDIF}

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

{$WARNINGS OFF}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$WARNINGS ON}

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

function RectSF(const Left, Top, Width, Height: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
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

function GetAngle(const CX, CY, X, Y: Single): Single;
begin
  Result := Math.ArcTan2(Abs(X - CX), Abs(Y - CY)) * 180 / PI;
  if (X > CX) and (Y > CY) then
    Result := 90 - Result // 右下角
  else if (X < CX) and (Y > CY) then
    Result := 90 + Result // 左下角
  else if (X < CX) and (Y < CY) then
    Result := 180 + (90 - Result) // 左上角
  else
    Result := 270 + Result // 右上角
end;

function EqulsMethod(const A, B: TNotifyEvent): Boolean;
begin
  Result := TMethod(A) = TMethod(B);
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

procedure SkipSpace(var P: PChar);
begin
  while p^ <> #0 do begin
    if (p^ = #9) or (p^ = #10) or (p^ = #13) or (p^ = #32) or (p^ = #$3000) then
      Inc(p)
    else
      Break;
  end;
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
  if not P^.IsInArray(['3', '4','5', '6', '7', '8', '9']) then Exit;
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
{$IF Defined(ANDROID)}
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
{$ELSEIF Defined(IOS)}
var NSU: NSUrl;
begin
  NSU := StrToNSUrl(URL);
  if SharedApplication.canOpenURL(NSU) then
    exit(SharedApplication.openUrl(NSU))
  else
    Result := False;
end;
{$ELSEIF Defined(MACOS)}
var
  NSU: NSUrl;
  Workspace : NSWorkspace;
begin
  NSU := StrToNSUrl(URL);
  Workspace := TNSWorkspace.Create;
  exit(Workspace.openURL(NSU));
end;
{$ELSEIF Defined(LINUX)}
begin
  Result := False;
end;
{$ELSE}
begin
  Result := ShellExecute(0, 'OPEN', PChar(URL), nil, nil, SW_SHOWMAXIMIZED) > 32;
end;
{$ENDIF}

function GetInstallDir(): string;
begin
  {$IFDEF ANDROID}
  // 说明：将 file_paths.xml 添加到 res/xml 中
  // 该目录等价于 TPath.GetDocumentsPath
  Result := JStringToString(TAndroidHelper.Activity.getFilesDir.getPath()) + '/';
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function InstallApk(const ApkFileName, ApkFileDir, Authorities: string): Boolean;

  {$IFDEF ANDROID}
  function Exec(): Boolean;
  var
    Intent: JIntent;
    f: JFile;
    uri: Jnet_Uri;
    LPath: string;
    LAuth: string;
  begin
    if (ApkFileName = '') or (Trim(ApkFileName) = '') then
      Exit(False);
    if ApkFileDir <> '' then
      LPath := IncludeTrailingPathDelimiter(ApkFileDir) + ApkFileName
    else
      LPath := GetInstallDir() + ApkFileName;
    f := TJFile.JavaClass.init(StringToJString(LPath));
    Intent := TJIntent.Create;
    Intent.addFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK);
    if TJBuild_VERSION.JavaClass.SDK_INT >= 26 then
      Intent.setAction(TJIntent.JavaClass.ACTION_INSTALL_PACKAGE)
    else
      Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
    if TJBuild_VERSION.JavaClass.SDK_INT >= 24 then begin
      if Authorities <> '' then
        LAuth := Authorities
      else
        LAuth := JStringToString({$IF CompilerVersion > 27}TAndroidHelper.Context
          {$ELSE}SharedActivityContext{$ENDIF}.getPackageName) + '.fileprovider';
      // provider authorities
      uri := TJFileProvider.JavaClass.getUriForFile(
        {$IF CompilerVersion > 27}TAndroidHelper.Context{$ELSE}SharedActivityContext{$ENDIF},
        StringToJString(LAuth), f);
      Intent.addFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
    end else
      uri := TJnet_Uri.JavaClass.fromFile(f);
    Intent.setDataAndType(uri, StringToJString('application/vnd.android.package-archive'));
    TAndroidHelper.Activity.startActivity(Intent);
    Result := True;
  end;
  {$ENDIF}

begin
  {$IFDEF ANDROID}
  Result := Exec();
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

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

function PCharToStr(const S: PChar; Len: Integer): string;
begin
  SetString(Result, S, Len);
end;

function PCharToIntDef(const S: pchar; Len: Integer; def: NativeInt = 0): NativeInt;
var
  I: Integer;
  v: Integer;
begin
  Result := 0;
  for I := 0 to len-1 do begin
    V := Convert[ord(s[i])];
    if V<0 then begin
      Result := def;
      Exit;
    end;
    result := (result * 10) + V;
  end;
end;

function PCharToFloatDef(const S: pchar; Len: Integer; def: Double = 0): Double;
var
  I, K, V, M: Integer;
begin
  Result := 0;
  K := 0;
  M := 10;
  for i := 0 to len - 1 do begin
    V := Convert[Ord(s[i])];
    if (s[i] = '.') and (k = 0) then Inc(k);
    if (V < 0) then begin
      if (k > 1) then begin
        Result := def;
        Exit;
      end;
    end else begin
      if k = 0 then
        Result := (Result * 10) + V
      else begin
        Result := Result + V / M;
        M := M * 10;
      end;
    end;
  end;
end;

function PHexToIntDef(const S: pchar; Len: Integer; def: NativeInt = 0): NativeInt;
var
  I: Integer;
  v: Integer;
begin
  Result := 0;
  for I := 0 to len-1 do begin
    V := Convert[ord(s[i])];
    if V<0 then begin
      Result := def;
      Exit;
    end;
    result := (result * 16) + V;
  end;
end;

function CharCount(const S: string): Integer;
var
  p, pe: PWord;
  ALen: Integer;

  procedure CountChar;
  begin
    if (p^ > $D800) and (p^ < $DFFF) then begin
      Inc(p);
      if (p^ >= $DC00) and (p^ < $DFFF) then begin
        Inc(p);
        Inc(Result);
      end else
        Result := -1;
    end else begin
      Inc(Result);
      Inc(p);
    end;
  end;

begin
  Result := 0;
  p := PWord(S);
  ALen := Length(S);
  pe := PWord(IntPtr(p) + (ALen shl 1));
  while IntPtr(p) < IntPtr(pe) do
    CountChar;
end;

function Hex2Color(const s: string): TAlphaColor;
begin
  Result := StrToUIntDef('$ff' + Copy(S, 2, Length(S) - 1), 0)
end;

function Text2Color(const s:string): TAlphaColor;
begin
  Result := TAlphaColorRec.Null;

  if (s='red') then result:=TAlphaColorRec.Red else
  if (s='black') then result:=TAlphaColorRec.Black else
  if (s='blue') then result:=TAlphaColorRec.Blue else
  if (s='green') then result:=TAlphaColorRec.Green else
  if (s='aqua') then result:=TAlphaColorRec.Aqua else
  if (s='yellow') then result:=TAlphaColorRec.Yellow else
  if (s='fuchsia') then result:=TAlphaColorRec.Fuchsia else
  if (s='white') then result:=TAlphaColorRec.White else
  if (s='lime') then result:=TAlphaColorRec.Lime else
  if (s='silver') then result:=TAlphaColorRec.Silver else
  if (s='gray') then result:=TAlphaColorRec.Gray else
  if (s='olive') then result:=TAlphaColorRec.Olive else
  if (s='navy') then result:=TAlphaColorRec.Navy else
  if (s='purple') then result:=TAlphaColorRec.Purple else
  if (s='teal') then result:=TAlphaColorRec.Teal else
  if (s='maroon') then result:=TAlphaColorRec.Maroon else
  if (s='pink') then result:=TAlphaColorRec.Pink else
  if (s='orange') then result:=TAlphaColorRec.Orange else
  if (Length(s) = 6) then result := StrToUIntDef('$ff' + s, 0)
end;

function RgbStrToColor(const s: string): TAlphaColor;
var
  P, PE, P1: PChar;
begin
  Result := TAlphaColorRec.Black;
  P := PChar(s);
  Inc(P, 4);
  PE := P + Length(S);
  SkipSpace(P);
  P1 := P;
  while (P < PE) and (not CharInSet(P^, [',',')',' '])) do Inc(P);
  TAlphaColorRec(Result).R := PCharToIntDef(P1, P - P1, 0);
  Inc(P);
  SkipSpace(P);
  if P < PE then begin
    P1 := P;
    while (P < PE) and (not CharInSet(P^, [',',')',' '])) do Inc(P);
    TAlphaColorRec(Result).G := PCharToIntDef(P1, P - P1, 0);
    Inc(P);
    SkipSpace(P);
    if P < PE then begin
      P1 := P;
      while (P < PE) and (not CharInSet(P^, [',',')',' '])) do Inc(P);
      TAlphaColorRec(Result).B := PCharToIntDef(P1, P - P1, 0);
    end;
  end;
end;

function RgbaStrToColor(const s: string): TAlphaColor;
var
  P, PE, P1: PChar;
begin
  Result := TAlphaColorRec.Black;
  P := PChar(s);
  Inc(P, 4);
  PE := P + Length(S);
  SkipSpace(P);
  P1 := P;
  while (P < PE) and (not CharInSet(P^, [',',')',' '])) do Inc(P);
  TAlphaColorRec(Result).R := PCharToIntDef(P1, P - P1, 0);
  Inc(P);
  SkipSpace(P);
  if P < PE then begin
    P1 := P;
    while (P < PE) and (not CharInSet(P^, [',',')',' '])) do Inc(P);
    TAlphaColorRec(Result).G := PCharToIntDef(P1, P - P1, 0);
    Inc(P);
    SkipSpace(P);
    if P < PE then begin
      P1 := P;
      while (P < PE) and (not CharInSet(P^, [',',')',' '])) do Inc(P);
      TAlphaColorRec(Result).B := PCharToIntDef(P1, P - P1, 0);
      Inc(P);
      SkipSpace(P);
      if P < PE then begin
        P1 := P;
        while (P < PE) and (not CharInSet(P^, [',',')',' '])) do Inc(P);
        TAlphaColorRec(Result).A := Round(PCharToFloatDef(P1, P - P1, 0) * 255);
      end;
    end;
  end;
end;

function HtmlColorToColor(const V: string; const DefaultValue: TAlphaColor): TAlphaColor;
begin
  Result := DefaultValue;
  if V = '' then Exit;
  case PChar(V)^ of
    '#': Result := Hex2Color(V);
    '$': Result := StrToUIntDef(V, DefaultValue);
  else
    if Length(V) > 9 then begin
      if PInt64(PChar(V))^ = PInt64(PChar('rgb('))^ then begin
        Result := RgbStrToColor(V)
      end else if (PInt64(PChar(V))^ = PInt64(PChar('rgba'))^) and (V.Chars[4] = '(') then begin
        Result := RgbaStrToColor(V)
      end else
        Result := Text2Color(V)
    end else
      Result := Text2Color(V)
  end;
end;

{ TIntHash }

procedure TIntHash.Add(const Key: THashType; const Value: Number);
var
  Hash: Integer;
  Bucket: PIntHashItem;
begin
  Hash := Key mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.AsNumber := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  Inc(FCount);
end;

procedure TIntHash.Add(const Key: THashType; const Value: Double);
var
  Hash: Integer;
  Bucket: PIntHashItem;
begin
  Hash := Key mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.AsDouble := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  Inc(FCount);
end;

procedure TIntHash.Add(const Key: THashType; const Value: Int64);
var
  Hash: Integer;
  Bucket: PIntHashItem;
begin
  Hash := Key mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.AsInt64 := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  Inc(FCount);
end;

procedure TIntHash.Add(const Key: THashType; const Value: Pointer);
var
  Hash: Integer;
  Bucket: PIntHashItem;
begin
  Hash := Key mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.AsPointer := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  Inc(FCount);
end;

procedure TIntHash.Add(const Key: THashType; const Value: TObject);
var
  Hash: Integer;
  Bucket: PIntHashItem;
begin
  Hash := Key mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.AsPointer := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  Inc(FCount);
end;

procedure TIntHash.AddOrUpdate(const Key: THashType; const Value: Pointer);
begin
  if not Modify(Key, Value) then
    Add(Key, Value);
end;

procedure TIntHash.AddOrUpdate(const Key: THashType; const Value: TObject);
begin
  if not Modify(Key, Value) then
    Add(Key, Value);
end;

procedure TIntHash.AddOrUpdate(const Key: THashType; const  Value: Number);
begin
  if not Modify(Key, Value) then
    Add(Key, Value);
end;

procedure TIntHash.AddOrUpdate(const Key: THashType; const Value: Int64);
begin
  if not Modify(Key, Value) then
    Add(Key, Value);
end;

procedure TIntHash.AddOrUpdate(const Key: THashType; const Value: Double);
begin
  if not Modify(Key, Value) then
    Add(Key, Value);
end;

procedure TIntHash.Clear;
var
  I: Integer;
  P, N: PIntHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do begin
    P := Buckets[I];
    if P <> nil then begin
      while P <> nil do begin
        N := P^.Next;
        if Assigned(FOnFreeItem) then
          FOnFreeItem(P);
        Dispose(P);
        P := N;
      end;
      Buckets[I] := nil;
    end;
  end;
  FCount := 0;
end;

procedure TIntHash.Clear(const CanDelete: TOnCanDelete);
var
  I: Integer;
  P, N: PIntHashItem;
begin
  if not Assigned(CanDelete) then begin
    Clear;
    Exit;
  end;
  FCount := 0;
  for I := 0 to Length(Buckets) - 1 do begin
    P := Buckets[I];
    Buckets[I] := nil;
    if P <> nil then begin
      while P <> nil do begin
        N := P^.Next;
        if CanDelete(P^.Key) then begin
          if Assigned(FOnFreeItem) then
            FOnFreeItem(P);
          Dispose(P);
        end else begin
          P^.Next := Buckets[I];
          Buckets[I] := P;
          Inc(FCount);
        end;
        P := N;
      end;
    end;
  end;
end;

function TIntHash.ContainsKey(const Key: THashType): Boolean;
begin
  Result := Find(Key)^ <> nil;
end;

constructor TIntHash.Create(Size: Cardinal);
begin
  inherited Create;
  SetLength(Buckets, Size);
  FLenBuckets := Size;
  FCount := 0;
end;

destructor TIntHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TIntHash.Exists(const Key: THashType): Boolean;
begin
  Result := Find(Key)^ <> nil;
end;

function TIntHash.Find(const Key: THashType): PPIntHashItem;
{$IFDEF HASHClash}
var
  J: Integer;
{$ENDIF}
begin
  {$IFDEF WIN64}
  {$ELSE}
  {$ENDIF}
  Result := @Buckets[Key mod FLenBuckets];
  //Result := @Buckets[(Cardinal(Key) xor Cardinal(Key shr 32)) mod Cardinal(Length(Buckets))];
  {$IFDEF HASHClash}J := 0; {$ENDIF}
  while (Result^ <> nil) and (Result^.Key <> Key) do begin
    Result := @Result^.Next;
    {$IFDEF HASHClash}
    Inc(J);
    {$ENDIF}
  end;
  {$IFDEF HASHClash}
  if J > FMaxClash then begin
    FMaxClash := J;
    OutputDebugString(PChar(Format('Max Hash Clash: %d, Key: %s', [J, InttoHex(Key, 8)])));
  end;
  {$ENDIF}
end;

function TIntHash.GetBucketsCount: Integer;
begin
  Result := Length(Buckets);
end;

function TIntHash.GetFolat(const Key: THashType;
  const DefaultValue: Double): Double;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.AsDouble
  else
    Result := DefaultValue;
end;

function TIntHash.GetInt(const Key: THashType;
  const DefaultValue: Number): Number;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.AsNumber
  else
    Result := DefaultValue;
end;

function TIntHash.GetInt64(const Key: THashType;
  const DefaultValue: Int64): Int64;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.AsInt64
  else
    Result := DefaultValue;
end;

function TIntHash.GetItem(const Key: THashType): TIntHashItem;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TIntHash.GetObject(const Key: THashType): TObject;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.AsPointer
  else
    Result := nil;
end;

function TIntHash.GetPointer(const Key: THashType): Pointer;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.AsPointer
  else
    Result := nil;
end;

function TIntHash.GetValueItem(const Key: THashType): Number;
begin
  Result := ValueOf(Key);
end;

function TIntHash.Modify(const Key: THashType; const Value: TObject): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    if Assigned(FOnFreeItem) then
      FOnFreeItem(P);
    P^.AsPointer := Value;
  end else
    Result := False;
end;

function TIntHash.Modify(const Key: THashType; const Value: Pointer): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    if Assigned(FOnFreeItem) then
      FOnFreeItem(P);
    P^.AsPointer := Value;
  end else
    Result := False;
end;

function TIntHash.Modify(const Key: THashType; const Value: Int64): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    if Assigned(FOnFreeItem) then
      FOnFreeItem(P);
    P^.AsInt64 := Value;
  end else
    Result := False;
end;

function TIntHash.Modify(const Key: THashType; const Value: Double): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    if Assigned(FOnFreeItem) then
      FOnFreeItem(P);
    P^.AsDouble := Value;
  end else
    Result := False;
end;

function TIntHash.Modify(const Key: THashType; const Value: Number): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    if Assigned(FOnFreeItem) then
      FOnFreeItem(P);
    P^.AsNumber := Value;
  end else
    Result := False;
end;

function TIntHash.Remove(const Key: THashType): Boolean;
var
  P: PIntHashItem;
  Prev: PPIntHashItem;
begin
  Result := False;
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then begin
    Result := True;
    Prev^ := P^.Next;
    if Assigned(FOnFreeItem) then
      FOnFreeItem(P);
    Dispose(P);
  end;
end;

procedure TIntHash.SetValueItem(const Key: THashType; const Value: Number);
begin
  AddOrUpdate(Key, Value);
end;

function TIntHash.TryGetValue(const Key: THashType; out Data: Number): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then begin
    Result := True;
    Data := P^.AsNumber
  end else begin
    Result := False;
    Data := 0;
  end;
end;

function TIntHash.TryGetValue(const Key: THashType; out Data: Pointer): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then begin
    Result := True;
    Data := P^.AsPointer
  end else begin
    Result := False;
    Data := nil;
  end;
end;

function TIntHash.TryGetValue(const Key: THashType; out Data: TObject): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then begin
    Result := True;
    Data := TObject(P^.AsPointer)
  end else begin
    Result := False;
    Data := nil;
  end;
end;

function TIntHash.TryGetValue(const Key: THashType; out Data: Double): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then begin
    Result := True;
    Data := P^.AsDouble
  end else begin
    Result := False;
    Data := 0;
  end;
end;

function TIntHash.TryGetValue(const Key: THashType; out Data: Int64): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then begin
    Result := True;
    Data := P^.AsInt64
  end else begin
    Result := False;
    Data := 0;
  end;
end;

function TIntHash.ValueOf(const Key: THashType; const DefaultValue: Number): Number;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := DefaultValue;
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
