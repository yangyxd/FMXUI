{*******************************************************}
{                                                       }
{       FMX UI Frame 管理单元                           }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Frame;

interface

uses
  UI.Base, UI.Toast, UI.Dialog,
  System.NetEncoding,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.Rtti, System.SyncObjs,
  {$IFDEF ANDROID}FMX.Platform.Android, {$ENDIF}
  {$IFDEF POSIX}Posix.Signal, {$ENDIF}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Platform, IOUtils;

type
  TFrameView = class;
  TFrameViewClass = class of TFrameView;

  /// <summary>
  /// Frame 参数
  /// </summary>
  TFrameParams = TDictionary<string, TValue>;

  TFrameDataType = (fdt_Integer, fdt_Long, fdt_Int64, fdt_Float, fdt_String,
    fdt_DateTime, fdt_Number, fdt_Boolean);

  TFrameDataValue = record
    DataType: TFrameDataType;
    Value: TValue;
  end;

  /// <summary>
  /// Frame 状态数据
  /// </summary>
  TFrameStateData = TDictionary<string, TFrameDataValue>;

  TFrameStateDataHelper = class helper for TFrameStateData
    function GetDataValue(DataType: TFrameDataType; const Value: TValue): TFrameDataValue;
    function GetString(const Key: string): string;
    function GetInt(const Key: string; const DefaultValue: Integer = 0): Integer;
    function GetLong(const Key: string; const DefaultValue: Cardinal = 0): Cardinal;
    function GetInt64(const Key: string; const DefaultValue: Int64 = 0): Int64;
    function GetFloat(const Key: string; const DefaultValue: Double = 0): Double;
    function GetDateTime(const Key: string; const DefaultValue: TDateTime = 0): TDateTime;
    function GetNumber(const Key: string; const DefaultValue: NativeUInt = 0): NativeUInt;
    function GetBoolean(const Key: string; const DefaultValue: Boolean = False): Boolean;

    procedure Put(const Key: string; const Value: string); overload; inline;
    procedure Put(const Key: string; const Value: Integer); overload; inline;
    procedure Put(const Key: string; const Value: Cardinal); overload; inline;
    procedure Put(const Key: string; const Value: Int64); overload; inline;
    procedure Put(const Key: string; const Value: Double); overload; inline;
    procedure Put(const Key: string; const Value: NativeUInt); overload; inline;
    procedure Put(const Key: string; const Value: Boolean); overload; inline;
    procedure PutDateTime(const Key: string; const Value: TDateTime); inline;
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

    property Data: TFrameStateData read FData;
    property Count: Integer read GetCount;
    property StoragePath: string read GetStoragePath write SetStoragePath;
  end;

  /// <summary>
  /// Frame 视图, Frame 切换处理
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TFrameView = class(FMX.Forms.TFrame)
  private
    FParams: TFrameParams;
    FPrivateState: TFrameState;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FWaitDialog: TProgressDialog;
    procedure SetParams(const Value: TFrameParams);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetPreferences: TFrameState;
    function GetSharedPreferences: TFrameState;
  protected
    [Weak] FLastView: TFrameView;
    [Weak] FNextView: TFrameView;
    function MakeFrame(FrameClass: TFrameViewClass): TFrameView; overload;

    procedure DoShow(); virtual;
    procedure DoHide(); virtual;

    // 检查是否需要释放，如果需要，就释放掉
    function CheckFree(): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// 流转化为 string
    /// </summary>
    function StreamToString(SrcStream: TStream; const CharSet: string = ''): string;

    /// <summary>
    /// 显示等待对话框
    /// </summary>
    procedure ShowWaitDialog(const AMsg: string; ACancelable: Boolean = True);
    /// <summary>
    /// 隐藏等待对话框
    /// </summary>
    procedure HideWaitDialog();

    /// <summary>
    /// 显示 Frame
    /// </summary>
    class function ShowFrame(Parent: TFmxObject; Params: TFrameParams): TFrameView; overload;
    /// <summary>
    /// 显示 Frame
    /// </summary>
    class function ShowFrame(Parent: TFmxObject; const Title: string = ''): TFrameView; overload;
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
    function StartFrame(FrameClass: TFrameViewClass): TFrameView; overload;
    /// <summary>
    /// 开始一个视图，并隐藏当前视图
    /// </summary>
    function StartFrame(FrameClass: TFrameViewClass; Params: TFrameParams): TFrameView; overload;
    /// <summary>
    /// 开始一个视图，并隐藏当前视图
    /// </summary>
    function StartFrame(FrameClass: TFrameViewClass; const Title: string): TFrameView; overload;

    /// <summary>
    /// 显示一个提示消息
    /// </summary>
    procedure Hint(const Msg: string); overload;
    procedure Hint(const Msg: Double); overload;
    procedure Hint(const Msg: Int64); overload;

    /// <summary>
    /// 显示 Frame
    /// </summary>
    procedure Show(); override;
    /// <summary>
    /// 关闭 Frame
    /// </summary>
    procedure Close(); virtual;
    /// <summary>
    /// 隐藏 Frame
    /// </summary>
    procedure Hide(); override;
    /// <summary>
    /// 完成当前 Frame (返回上一个 Frame 或 关闭)
    /// </summary>
    procedure Finish(); virtual;

    /// <summary>
    /// 启动时的参数
    /// </summary>
    property Params: TFrameParams read FParams write SetParams;
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
  published
    property Title: string read GetTitle write SetTitle;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
  end;

type
  TFrame = class(TFrameView);

var
  MainFormMinChildren: Integer = 1;

implementation

const
  CS_Title = 'title';

var
  /// <summary>
  /// 公共状态数据
  /// </summary>
  FPublicState: TFrameState = nil;

{ TFrameView }

function TFrameView.CheckFree: Boolean;
begin
  Result := False;
  if Assigned(Parent) then begin
    if not Assigned(Parent.Parent) then begin
      if (Parent is TForm) and (Parent.ChildrenCount <= MainFormMinChildren + 1) then begin
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
    end;
    Parent.RemoveObject(Self);
  end;
end;

procedure TFrameView.Close;
begin
  if CheckFree then Exit;
  {$IFNDEF AUTOREFCOUNT}
  Free;
  {$ENDIF}
end;

class function TFrameView.CreateFrame(Parent: TFmxObject;
  Params: TFrameParams): TFrameView;
begin
  Result := nil;
  if (Assigned(Parent)) then begin
    try
      Result := Create(Parent);
      Result.Parent := Parent;
      Result.Align := TAlignLayout.Client;
      Result.FLastView := nil;
      Result.TagObject := Params;
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
  Result := FrameClass.Create(Parent);
  Result.Parent := Parent;
  Result.Align := TAlignLayout.Client;
  Result.FLastView := Self;
  FNextView := Result;
end;

destructor TFrameView.Destroy;
begin
  if Assigned(FNextView) then
    FNextView.FLastView := nil;
  FLastView := nil;
  FNextView := nil;
  FreeAndNil(FParams);
  FreeAndNil(FPrivateState);
  inherited;
end;

procedure TFrameView.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TFrameView.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TFrameView.Finish;
begin
  if not Assigned(FLastView) then
    Close
  else begin
    if CheckFree then Exit;
    FLastView.Show;
    FLastView.FNextView := nil;
    FLastView := nil;
    {$IFNDEF AUTOREFCOUNT}
    Free;
    {$ENDIF}
  end;
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

function TFrameView.GetTitle: string;
begin
  if FParams = nil then
    Result := ''
  else
    Result := FParams.Items[CS_Title].ToString;
end;

procedure TFrameView.Hide;
begin
  Visible := False;
  DoHide;
end;

procedure TFrameView.HideWaitDialog;
begin
  if Assigned(FWaitDialog) then begin
    FWaitDialog.Dismiss;
    FWaitDialog := nil;
  end;
end;

procedure TFrameView.Hint(const Msg: Double);
begin
  Toast(FloatToStr(Msg));
end;

procedure TFrameView.Hint(const Msg: Int64);
begin
  Toast(IntToStr(Msg));
end;

procedure TFrameView.Hint(const Msg: string);
begin
  Toast(Msg);
end;

procedure TFrameView.SetParams(const Value: TFrameParams);
begin
  if Assigned(FParams) then
    FParams.Free;
  FParams := Value;
end;

procedure TFrameView.SetTitle(const Value: string);
begin
  if FParams = nil then begin
    if Value = '' then Exit;
    FParams := TFrameParams.Create(9);
  end;
  if FParams.ContainsKey(CS_Title) then
    FParams.Items[CS_Title] := Value
  else if Value <> '' then         
    FParams.Add(CS_Title, Value);
end;

procedure TFrameView.Show();
begin
  if Title <> '' then
    Application.Title := Title;
  DoShow();
  Visible := True;
end;

class function TFrameView.ShowFrame(Parent: TFmxObject;
  const Title: string): TFrameView;
begin
  Result := CreateFrame(Parent, Title);
  if Result <> nil then
    Result.Show();
end;

procedure TFrameView.ShowWaitDialog(const AMsg: string; ACancelable: Boolean);
begin
  if not Assigned(FWaitDialog) then
    FWaitDialog := TProgressDialog.Create(Self);
  FWaitDialog.Cancelable := ACancelable;
  if not Assigned(FWaitDialog.RootView) then
    FWaitDialog.InitView(AMsg)
  else
    FWaitDialog.Message := AMsg;
  TDialog(FWaitDialog).Show();
end;

function TFrameView.StartFrame(FrameClass: TFrameViewClass;
  const Title: string): TFrameView;
begin
  Result := MakeFrame(FrameClass);
  Result.Title := Title;
  Result.Show();
  Hide;
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

function TFrameView.StartFrame(FrameClass: TFrameViewClass;
  Params: TFrameParams): TFrameView;
begin
  Result := MakeFrame(FrameClass);
  Result.Params := Params;
  Result.Show();
  Hide;
end;

function TFrameView.StartFrame(FrameClass: TFrameViewClass): TFrameView;
begin
  Result := MakeFrame(FrameClass);
  Result.Show();
  Hide;
end;

class function TFrameView.ShowFrame(Parent: TFmxObject;
  Params: TFrameParams): TFrameView;
begin
  Result := CreateFrame(Parent, Params);
  if Result <> nil then
    Result.Show();
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

function TFrameStateDataHelper.GetString(const Key: string): string;
begin
  if ContainsKey(Key) then
    Result := Items[Key].Value.ToString
  else
    Result := '';
end;

procedure TFrameStateDataHelper.Put(const Key: string; const Value: Cardinal);
begin
  AddOrSetValue(Key, GetDataValue(fdt_Long, Value));
end;

procedure TFrameStateDataHelper.Put(const Key: string; const Value: Integer);
begin
  AddOrSetValue(Key, GetDataValue(fdt_Integer, Value));
end;

procedure TFrameStateDataHelper.Put(const Key, Value: string);
begin
  AddOrSetValue(Key, GetDataValue(fdt_String, Value));
end;

procedure TFrameStateDataHelper.Put(const Key: string; const Value: NativeUInt);
begin
  AddOrSetValue(Key, GetDataValue(fdt_Number, Value));
end;

procedure TFrameStateDataHelper.Put(const Key: string; const Value: Boolean);
begin
  AddOrSetValue(Key, GetDataValue(fdt_Boolean, Value));
end;

procedure TFrameStateDataHelper.Put(const Key: string; const Value: Int64);
begin
  AddOrSetValue(Key, GetDataValue(fdt_Int64, Value));
end;

procedure TFrameStateDataHelper.Put(const Key: string; const Value: Double);
begin
  AddOrSetValue(Key, GetDataValue(fdt_Float, Value));
end;

procedure TFrameStateDataHelper.PutDateTime(const Key: string;
  const Value: TDateTime);
begin
  AddOrSetValue(Key, GetDataValue(fdt_DateTime, Value));
end;

initialization
  FPublicState := TFrameState.Create(nil, True);
  FPublicState.Load;

finalization
  FreeAndNil(FPublicState);

end.
