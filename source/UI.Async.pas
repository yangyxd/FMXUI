{*******************************************************}
{                                                       }
{       异步任务处理模式                                }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Async;

interface

uses
  {$IFDEF MSWINDOWS} ActiveX, {$ENDIF}
  Classes, SysUtils;

type
  TAsync = class;

  TExecuteEvent = procedure (Async: TAsync) of object;
  TExecuteEventA = reference to procedure (Async: TAsync);

  TAsyncThread = class(TThread)
  protected
    FAsync: TAsync;
    procedure Execute; override;
  end;

  /// <summary>
  /// 异步任务处理
  /// </summary>
  TAsync = class(TObject)
  private
    function GetComInitialized: Boolean;
  protected
    FExec: TExecuteEvent;
    FExecA: TExecuteEventA;
    FComplete: TExecuteEvent;
    FCompleteA: TExecuteEventA;
    FData: Pointer;
    FDataInterface: IInterface;
    FMessage: string;
    FTag: NativeInt;
    {$IFDEF MSWINDOWS}
    FComInitialized: Boolean;
    {$ENDIF}
  protected
    function IsNeedExecute: Boolean;
    function IsNeedExecuteComplete: Boolean;
    procedure DoExecute;
    procedure DoExecuteComplete;
  public
    destructor Destroy; override;

    /// <summary>
    /// 开始执行
    /// </summary>
    procedure Execute(); virtual;

    procedure Start();

    /// <summary>
    /// 异步执行过程 （非线程安全）
    /// </summary>
    function SetExecute(AValue: TExecuteEvent): TAsync; overload;
    function SetExecute(AValue: TExecuteEventA): TAsync; overload;
    /// <summary>
    /// 异步执行完成后需要执行的过程 （线程安全）
    /// </summary>
    function SetExecuteComplete(AValue: TExecuteEvent): TAsync; overload;
    function SetExecuteComplete(AValue: TExecuteEventA): TAsync; overload;

    function SetData(const Data: Pointer): TAsync;
    function SetDataInterface(const Data: IInterface): TAsync;
    function SetMessage(const Data: string): TAsync;
    function SetTag(Value: NativeInt): TAsync;

    /// <summary>
    /// Windows 平台， 需要 Com 支持时调用以实现线程 Com 初始化
    /// </summary>
    procedure ComNeeded(AInitFlags: Cardinal = 0);

    property Data: Pointer read FData write FData;
    property DataInterface: IInterface read FDataInterface write FDataInterface;
    property Message: string read FMessage write FMessage;
    property Tag: NativeInt read FTag write FTag;
    property ComInitialized: Boolean read GetComInitialized;
  end;

implementation

var
  FAsyncRef: Integer = 0; // 异步任务计数器

{ TAsync }

procedure TAsync.ComNeeded(AInitFlags: Cardinal);
begin
  {$IFDEF MSWINDOWS}
  if not ComInitialized then begin
    if AInitFlags = 0 then
      CoInitialize(nil)
    else
      CoInitializeEx(nil, AInitFlags);
    FComInitialized := True;
  end;
  {$ENDIF}
end;

destructor TAsync.Destroy;
begin
  {$IFDEF MSWINDOWS}
  if ComInitialized then
    CoUninitialize;
  {$ENDIF}
  FDataInterface := nil;
  inherited Destroy;
end;

procedure TAsync.DoExecute;
begin
  if Assigned(FExecA) then
    FExecA(Self)
  else if Assigned(FExec) then
    FExec(Self);
end;

procedure TAsync.DoExecuteComplete;
begin
  if Assigned(FCompleteA) then
    FCompleteA(Self)
  else if Assigned(FComplete) then
    FComplete(Self);
end;

procedure TAsync.Execute;
var
  FThread: TAsyncThread;
begin
  FThread := TAsyncThread.Create(True);
  FThread.FreeOnTerminate := True;
  FThread.FAsync := Self;
  FThread.Start;
end;

function TAsync.GetComInitialized: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := FComInitialized;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TAsync.IsNeedExecute: Boolean;
begin
  Result := Assigned(FExec) or Assigned(FExecA);
end;

function TAsync.IsNeedExecuteComplete: Boolean;
begin
  Result := Assigned(FComplete) or Assigned(FCompleteA);
end;

function TAsync.SetData(const Data: Pointer): TAsync;
begin
  Result := Self;
  FData := Data;
end;

function TAsync.SetDataInterface(const Data: IInterface): TAsync;
begin
  Result := Self;
  FDataInterface := Data;
end;

function TAsync.SetExecute(AValue: TExecuteEvent): TAsync;
begin
  Result := Self;
  FExec := AValue;
end;

function TAsync.SetExecute(AValue: TExecuteEventA): TAsync;
begin
  Result := Self;
  FExecA := AValue;
end;

function TAsync.SetExecuteComplete(AValue: TExecuteEvent): TAsync;
begin
  Result := Self;
  FComplete := AValue;
end;

function TAsync.SetExecuteComplete(AValue: TExecuteEventA): TAsync;
begin
  Result := Self;
  FCompleteA := AValue;
end;

function TAsync.SetMessage  (const Data: string): TAsync;
begin
  Result := Self;
  FMessage := Data;
end;

function TAsync.SetTag(Value: NativeInt): TAsync;
begin
  Result := Self;
  FTag := Value;
end;

procedure TAsync.Start;
begin
  AtomicIncrement(FAsyncRef);
  Execute;
end;

{ TAsyncThread }

procedure TAsyncThread.Execute;
begin
  if Assigned(FAsync) then begin
    try
      try
        // 先执行异步任务
        FAsync.DoExecute;
      except
      end;
      try
        // 然后执行同步任务
        if FAsync.IsNeedExecuteComplete then
          Synchronize(Self, FAsync.DoExecuteComplete);
      except
      end;
    finally
      FAsync.DisposeOf;
      FAsync := nil;
      AtomicDecrement(FAsyncRef);
    end;
  end;
end;

// 等待所有线程关闭
procedure WaitAsyncFinish(const ATimeout: Cardinal = 5000);
var
  T: Cardinal;
begin
  T := TThread.GetTickCount;
  while AtomicDecrement(FAsyncRef) >= 0 do begin
    AtomicIncrement(FAsyncRef);
    Sleep(20);
    if TThread.GetTickCount - T > ATimeout then
      Break;
  end;
end;

initialization

finalization
  WaitAsyncFinish();

end.
