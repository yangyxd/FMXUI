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
  UI.Base, UI.Toast,
  System.Generics.Collections, System.Rtti,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  {$IFDEF ANDROID}FMX.Platform.Android, {$ENDIF}
  {$IFDEF POSIX}Posix.Signal, {$ENDIF}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics;

type
  TFrameView = class;
  TFrameViewClass = class of TFrameView;
  /// <summary>
  /// Frame 参数
  /// </summary>
  TFrameParams = TDictionary<string, TValue>;

  /// <summary>
  /// Frame 视图, Frame 切换处理
  /// </summary>
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TFrameView = class(FMX.Forms.TFrame)
  private
    FParams: TFrameParams;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    procedure SetParams(const Value: TFrameParams);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
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

function TFrameView.StartFrame(FrameClass: TFrameViewClass;
  const Title: string): TFrameView;
begin
  Result := MakeFrame(FrameClass);
  Result.Title := Title;
  Result.Show();
  Hide;
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

end.
