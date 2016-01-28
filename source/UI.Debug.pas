{*******************************************************}
{                                                       }
{       YxdInclude 日志输出模块                         }
{                                                       }
{       版权所有 (C) 2013      YangYxd                  }
{                                                       }
{*******************************************************}

unit UI.Debug;

{$DEFINE UseUDP}  // 是否使用远程输出日志

interface

uses
  {$IFDEF UseUDP}
  IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient,
  {$ENDIF}
  Windows, SysUtils, Classes;

type
  /// <summary>
  /// 输出信息类型
  /// </summary>
  TTraceSeverity = (tsDebug {调试信息}, tsInformation {信息}, tsWarning {警告},
    tsError {错误});
  TTraceSeverities = set of TTraceSeverity;

const
  TRACE_SEVERITIES_ALL = [tsDebug, tsInformation, tsWarning, tsError];

{$IFDEF ShowUI}
type
  TOnWriteLogEvent = procedure (Sender: TObject; AType: TTraceSeverity; const Log: string) of object;
var
  FOnWriteLog: TOnWriteLogEvent = nil;
{$ENDIF}

// 写日志
procedure Log(sev: TTraceSeverity; const text: string);
procedure LogD(const text: string); inline; // tsDeubg
procedure LogW(const text: string); inline; // tsWarning
procedure LogI(const text: string); inline; // tsInfomation
procedure LogE(const text: string); overload; inline; // tsError
procedure LogE(Sender: TObject; E: TObject); overload; // tsError
procedure LogE(Sender: TObject; const Title: string; E: TObject); overload;  // tsError
procedure LogE(Sender: TObject; const Title: string; E: Exception); overload; // tsError

{$IFDEF UseUDP}
/// <summary>
/// 设置远程调试服务器地址
/// </summary>
procedure LogRemoteDebugSvrAddr(const RemoteAddr: string; RemotePort: Word);
{$ENDIF}

var
  SevToStr: array [TTraceSeverity] of string = ('调试', '信息', '警告', '错误');

implementation

const
  sExceptionLogFmt = '[%s.%s] %s';
  sExceptionLogSFmt = '[%s] %s';
  sLineLogFmt = '[%s][%s] %s (%d)'#13;
  sLogFmt = '[%s][%s] %s (%d)';
  sLogTimeFmt = 'hh:mm:ss.zzz';
  sFileNotExist = 'File does not exist.';
  sInvalidFileHandle = 'Invalid File Handle. Can''t open the file.';
  sLogDir = 'Log\';
  sLogFileNameFmt = 'yyyymmdd';
  sLogFileExtName = '.log';
  sDebuging = '<Debuging>';

type
  ITrace = interface(IInterface)
    procedure FillWrite;
    procedure Write(sev: TTraceSeverity; const text: string);
    procedure Writeln(sev: TTraceSeverity; const text: string);
    function ReadAll: string;
    procedure SetBufferSize(Value: Integer);
  end;

  // 输出调试信息
  TDebugTrace = class(TInterfacedObject, ITrace)
  public
    procedure FillWrite;
    procedure Write(sev: TTraceSeverity; const text: string);
    procedure Writeln(sev: TTraceSeverity; const text: string);
    function ReadAll: string;
    procedure SetBufferSize(Value: Integer);
  end;

  {$IFDEF UseUDP}
  // 输出远程调试信息
  TRemoteDebugTrace = class(TInterfacedObject, ITrace)
  private
    udp: TIdUDPClient;
    FAddr: string;
    FPort: Word;
  protected
    procedure prepare; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FillWrite;
    procedure Write(sev: TTraceSeverity; const text: string);
    procedure Writeln(sev: TTraceSeverity; const text: string);
    function ReadAll: string;
    procedure SetBufferSize(Value: Integer);
    property RemoteAddr: string read FAddr write FAddr;
    property RemotePort: Word read FPort write FPort;
  end;
  {$ENDIF}

var
  locker: TRTLCriticalSection;
  FThread: TThread = nil;
  Trace: ITrace = nil;
  UdpTrace: ITrace;

procedure Lock; inline;
begin
  EnterCriticalSection(locker);
end;

procedure UnLock; inline;
begin
  LeaveCriticalSection(locker);
end;

procedure LogInit(New: Boolean);
{$IFDEF OuputFileLog} var fname: string; {$ENDIF}
begin
  if New then begin
    if Trace <> nil then begin
      Trace._Release;
      Trace := nil;
    end;
    {$IFDEF OuputFileLog}
    fname := TFileTrace.GetFileName;
    if FileExists(fname) then DeleteFile(fname);
    {$ENDIF}
  end;
  if Trace <> nil then Exit;  
  {$IFDEF DebugApp}
  Trace := TDebugTrace.Create;
  {$ELSE} {$IFDEF OuputFileLog}
  Trace := TFileTrace.Create;
  FThread := TFileRealWriteThd.Create(False);
  {$ENDIF} {$ENDIF}
end;

procedure Log(sev: TTraceSeverity; const text: string);
begin
  if Trace <> nil then Trace.Write(sev, text);
  {$IFDEF UseUDP}
  if UdpTrace <> nil then UdpTrace.Write(sev, text);
  {$ENDIF}
  {$IFDEF ShowUI}
  if Assigned(FOnWriteLog) then
    FOnWriteLog(TObject(Trace), sev, text);
  {$ENDIF}
end;

procedure LogD(const text: string);
begin
  Log(tsDebug, text);
end;

procedure LogW(const text: string);
begin
  Log(tsWarning, text);
end;

procedure LogI(const text: string);
begin
  Log(tsInformation, text);
end;

procedure LogE(const text: string);
begin
  Log(tsError, text);
end;

procedure LogE(Sender: TObject; E: TObject);
begin
  if (E <> nil) then begin
    if (E is Exception) then
      Log(tsError, Format(sExceptionLogSFmt, [Sender.ClassName, Exception(E).Message]))
    else
      Log(tsError, Format(sExceptionLogSFmt, [Sender.ClassName, E.ClassName]));
  end else
    Log(tsError, Sender.ClassName);
end;

procedure LogE(Sender: TObject; const Title: string; E: TObject);
begin
  if (E <> nil) then begin
    if (E is Exception) then
      Log(tsError, Format(sExceptionLogFmt, [Sender.ClassName, Title, Exception(E).Message]))
    else
      Log(tsError, Format(sExceptionLogFmt, [Sender.ClassName, Title, E.ClassName]));
  end else
    Log(tsError, Format(sExceptionLogFmt, [Sender.ClassName, Title, '']));
end;

procedure LogE(Sender: TObject; const Title: string; E: Exception);
begin
  if E = nil then
    Log(tsError, Format(sExceptionLogFmt, [Sender.ClassName, Title, '']))
  else
    Log(tsError, Format(sExceptionLogFmt, [Sender.ClassName, Title, E.Message]));
end;

procedure LogRemoteDebugSvrAddr(const RemoteAddr: string; RemotePort: Word);
begin
  if UdpTrace <> nil then begin
    {$IFDEF UseUDP}
    TRemoteDebugTrace(UdpTrace).RemoteAddr := RemoteAddr;     
    TRemoteDebugTrace(UdpTrace).RemotePort := RemotePort;
    {$ENDIF}
  end;
end;

function LogContent: string;
begin
  if Assigned(Trace) then
    Result := Trace.ReadAll
  else Result := '';
end;

procedure LogPushFile();
begin
  if Assigned(Trace) then begin
    try
      Trace.FillWrite;
    except end;
  end;
end;

{ TDebugTrace }

procedure TDebugTrace.FillWrite;
begin
end;

function TDebugTrace.ReadAll: string;
begin
  Result := sDebuging;
end;

procedure TDebugTrace.SetBufferSize(Value: Integer);
begin   
end;

procedure TDebugTrace.Write(sev: TTraceSeverity; const text: string);
begin
  Lock;
  Writeln(sev, text);
  UnLock;
end;

procedure TDebugTrace.Writeln(sev: TTraceSeverity; const text: string); 
{$IFDEF DebugApp}var Msg: string;{$ENDIF}
begin
  {$IFDEF WRITEDEBUG}
  if IsConsole then begin
    Msg := Format(sLogFmt, [FormatDateTime(sLogTimeFmt, Now), SevToStr[sev], text, GetCurrentThreadId]);
    Lock;
    System.Writeln(Msg);
    UnLock;
  end;
  {$ENDIF}
  {$IFDEF OutputDebug}
  if Length(Msg) = 0 then
    Msg := Format(sLogFmt, [FormatDateTime(sLogTimeFmt, Now), SevToStr[sev], text, GetCurrentThreadId]);
  Lock;
  OutputDebugString(PChar(Msg));
  UnLock;
  {$ENDIF}
end;

{ TRemoteDebugTrace }
{$IFDEF UseUDP}
constructor TRemoteDebugTrace.Create;
begin
  udp := TIdUDPClient.Create(nil);
  FAddr := '127.0.0.1';
  FPort := 6699;
end;

destructor TRemoteDebugTrace.Destroy;
begin
  if udp.Active then udp.Disconnect;
  FreeAndNil(udp);
  inherited;
end;

procedure TRemoteDebugTrace.prepare;
begin
  if (not udp.Active) then 
    udp.Active := True;
end;

function TRemoteDebugTrace.ReadAll: string;
begin
  Result := '';
end;

procedure TRemoteDebugTrace.FillWrite;
begin
end;

procedure TRemoteDebugTrace.Write(sev: TTraceSeverity; const text: string);
var
  Msg: string;
begin
  Msg := Format(sLineLogFmt, [FormatDateTime(sLogTimeFmt, Now), SevToStr[sev], text, GetCurrentThreadId]);
  Lock;
  prepare;
  udp.Send(Faddr, FPort, Msg);
  UnLock;
end;

procedure TRemoteDebugTrace.SetBufferSize(Value: Integer);
begin
end;

procedure TRemoteDebugTrace.Writeln(sev: TTraceSeverity; const text: string);
var
  Msg: string;
begin
  Msg := Format(sLineLogFmt, [FormatDateTime(sLogTimeFmt, Now), SevToStr[sev], text, GetCurrentThreadId]);
  Lock;
  prepare;
  udp.Send(FAddr, FPort, Msg);
  UnLock;
end;
{$ENDIF}

initialization
  InitializeCriticalSection(locker);
  LogInit(False);
  {$IFDEF UseUDP}
  UdpTrace := TRemoteDebugTrace.Create;
  {$ENDIF}

finalization
  FreeAndNil(FThread);
  DeleteCriticalSection(locker);
  if Trace <> nil then
    Trace._Release;
  if UdpTrace <> nil then UdpTrace._Release;

end.

