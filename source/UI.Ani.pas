unit UI.Ani;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.Rtti, System.SyncObjs,
  FMX.Ani, FMX.Utils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Platform, IOUtils;

type
  /// <summary>
  /// 动画类型
  /// </summary>
  TFrameAniType = (None, DefaultAni {默认}, FadeInOut {淡入淡出},
    MoveInOut {移进移出}, TopMoveInOut {顶部弹进弹出}, BottomMoveInOut {底部弹进弹出},
    LeftSlideMenu {左边栏菜单}, RightSlideMenu {右边栏菜单}
  );

  TNotifyEventA = reference to procedure (Sender: TObject);

  TDelayExecute = class(TAnimation)
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
  public
    procedure Start; override;
    procedure Stop; override;
  end;

  TFloatExAnimation = class(TFloatAnimation)
  protected
    function FindProperty: Boolean;
  public
    procedure Start; override;
  end;

  TInt64Animation = class(TCustomPropertyAnimation)
  private
    FStartValue: Int64;
    FStopValue: Int64;
    FStartFromCurrent: Boolean;
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property StartValue: Int64 read FStartValue write FStartValue stored True nodefault;
    property StopValue: Int64 read FStopValue write FStopValue stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
  end;

  TFrameAnimator = class
  private type
    TFrameAnimatorEvent = record
      OnFinish: TNotifyEvent;
      OnFinishA: TNotifyEventA;
      OnProcess: TNotifyEventA;
    end;
    TAnimationDestroyer = class
    private
      FOnFinishs: TDictionary<Integer, TFrameAnimatorEvent>;
      procedure DoAniProcess(Sender: TObject);
      procedure DoAniFinished(Sender: TObject);
      procedure DoAniFinishedEx(Sender: TObject; FreeSender: Boolean);
    public
      constructor Create();
      destructor Destroy; override;
      procedure Add(Sender: TObject; AOnFinish: TNotifyEvent); overload;
      procedure Add(Sender: TObject; AOnFinish: TNotifyEventA); overload;
      procedure Add(Sender: TObject; AOnFinish, AOnProcess: TNotifyEventA); overload;
    end;
  private class var
    FDestroyer: TAnimationDestroyer;
  private
    class procedure CreateDestroyer;
    class procedure Uninitialize;
  public
    /// <summary>
    /// 延时执行任务
    /// </summary>
    class procedure DelayExecute(const Owner: TFmxObject; AOnFinish: TNotifyEventA; Delay: Single = 1.0);

    class procedure AnimateFloat(const Target: TFmxObject;
      const APropertyName: string; const NewValue: Single;
      AOnFinish: TNotifyEvent = nil; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateFloat(const Target: TFmxObject;
      const APropertyName: string; const NewValue: Single;
      AOnFinish: TNotifyEventA; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateFloat(const Target: TFmxObject;
      const APropertyName: string; const NewValue: Single;
      AOnFinish: TNotifyEventA; AOnProcess: TNotifyEventA; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;

    class procedure AnimateInt(const Target: TFmxObject;
      const APropertyName: string; const NewValue: Integer;
      AOnFinish: TNotifyEvent = nil; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateInt(const Target: TFmxObject;
      const APropertyName: string; const NewValue: Integer;
      AOnFinish: TNotifyEventA; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;

    class procedure AnimateInt64(const Target: TFmxObject;
      const APropertyName: string; const NewValue: Int64;
      AOnFinish: TNotifyEvent = nil; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;

    class procedure AnimateColor(const Target: TFmxObject;
      const APropertyName: string; NewValue: TAlphaColor;
      AOnFinish: TNotifyEvent = nil; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateColor(const Target: TFmxObject;
      const APropertyName: string; NewValue: TAlphaColor;
      AOnFinish: TNotifyEventA; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
  end;

function InterpolateInt64(const Start, Stop: Int64; const T: Single): Int64;

implementation

function InterpolateInt64(const Start, Stop: Int64; const T: Single): Int64;
begin
  Result := Round(Start + (Stop - Start) * T);
end;

{ TFrameAnimator }

class procedure TFrameAnimator.AnimateColor(const Target: TFmxObject;
  const APropertyName: string; NewValue: TAlphaColor; AOnFinish: TNotifyEventA;
  Duration, Delay: Single; AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TColorAnimation;
begin
  TAnimator.StopPropertyAnimation(Target, APropertyName);

  CreateDestroyer;

  Animation := TColorAnimation.Create(Target);
  FDestroyer.Add(Animation, AOnFinish);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.OnFinish := FDestroyer.DoAniFinished;
  Animation.Duration := Duration;
  Animation.Delay := Delay;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.Start;

  if not Animation.Enabled then
    FDestroyer.DoAniFinishedEx(Animation, False);
end;

class procedure TFrameAnimator.AnimateColor(const Target: TFmxObject;
  const APropertyName: string; NewValue: TAlphaColor; AOnFinish: TNotifyEvent;
  Duration, Delay: Single; AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TColorAnimation;
begin
  TAnimator.StopPropertyAnimation(Target, APropertyName);

  CreateDestroyer;

  Animation := TColorAnimation.Create(Target);
  FDestroyer.Add(Animation, AOnFinish);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.OnFinish := FDestroyer.DoAniFinished;
  Animation.Duration := Duration;
  Animation.Delay := Delay;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.Start;

  if not Animation.Enabled then
    FDestroyer.DoAniFinishedEx(Animation, False);
end;

class procedure TFrameAnimator.AnimateFloat(const Target: TFmxObject;
  const APropertyName: string; const NewValue: Single; AOnFinish: TNotifyEvent;
  Duration, Delay: Single; AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TFloatAnimation;
begin
  TAnimator.StopPropertyAnimation(Target, APropertyName);

  CreateDestroyer;

  Animation := TFloatAnimation.Create(nil);
  FDestroyer.Add(Animation, AOnFinish);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.OnFinish := FDestroyer.DoAniFinished;
  Animation.Duration := Duration;
  Animation.Delay := Delay;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.Start;

  if not Animation.Enabled then
    FDestroyer.DoAniFinishedEx(Animation, False);
end;

class procedure TFrameAnimator.AnimateFloat(const Target: TFmxObject;
  const APropertyName: string; const NewValue: Single; AOnFinish: TNotifyEventA;
  Duration, Delay: Single; AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TFloatAnimation;
begin
  TAnimator.StopPropertyAnimation(Target, APropertyName);

  CreateDestroyer;

  Animation := TFloatAnimation.Create(nil);
  FDestroyer.Add(Animation, AOnFinish);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.OnFinish := FDestroyer.DoAniFinished;
  Animation.Duration := Duration;
  Animation.Delay := Delay;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.Start;

  if not Animation.Enabled then
    FDestroyer.DoAniFinishedEx(Animation, False);
end;

class procedure TFrameAnimator.AnimateFloat(const Target: TFmxObject;
  const APropertyName: string; const NewValue: Single; AOnFinish,
  AOnProcess: TNotifyEventA; Duration, Delay: Single; AType: TAnimationType;
  AInterpolation: TInterpolationType);
var
  Animation: TFloatAnimation;
begin
  TAnimator.StopPropertyAnimation(Target, APropertyName);

  CreateDestroyer;

  Animation := TFloatExAnimation.Create(nil);
  FDestroyer.Add(Animation, AOnFinish, AOnProcess);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.OnFinish := FDestroyer.DoAniFinished;
  Animation.OnProcess := FDestroyer.DoAniProcess;
  Animation.Duration := Duration;
  Animation.Delay := Delay;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.Start;

  if not Animation.Enabled then
    FDestroyer.DoAniFinishedEx(Animation, False);
end;

class procedure TFrameAnimator.AnimateInt(const Target: TFmxObject;
  const APropertyName: string; const NewValue: Integer; AOnFinish: TNotifyEvent;
  Duration, Delay: Single; AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TIntAnimation;
begin
  CreateDestroyer;

  TAnimator.StopPropertyAnimation(Target, APropertyName);

  Animation := TIntAnimation.Create(nil);
  FDestroyer.Add(Animation, AOnFinish);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.OnFinish := FDestroyer.DoAniFinished;
  Animation.Duration := Duration;
  Animation.Delay := Delay;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.Start;

  if not Animation.Enabled then
    FDestroyer.DoAniFinishedEx(Animation, False);
end;

class procedure TFrameAnimator.AnimateInt(const Target: TFmxObject;
  const APropertyName: string; const NewValue: Integer;
  AOnFinish: TNotifyEventA; Duration, Delay: Single; AType: TAnimationType;
  AInterpolation: TInterpolationType);
var
  Animation: TIntAnimation;
begin
  CreateDestroyer;

  TAnimator.StopPropertyAnimation(Target, APropertyName);

  Animation := TIntAnimation.Create(nil);
  FDestroyer.Add(Animation, AOnFinish);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.OnFinish := FDestroyer.DoAniFinished;
  Animation.Duration := Duration;
  Animation.Delay := Delay;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.Start;

  if not Animation.Enabled then
    FDestroyer.DoAniFinishedEx(Animation, False);
end;

class procedure TFrameAnimator.AnimateInt64(const Target: TFmxObject;
  const APropertyName: string; const NewValue: Int64; AOnFinish: TNotifyEvent;
  Duration, Delay: Single; AType: TAnimationType;
  AInterpolation: TInterpolationType);
var
  Animation: TInt64Animation;
begin
  CreateDestroyer;

  TAnimator.StopPropertyAnimation(Target, APropertyName);

  Animation := TInt64Animation.Create(nil);
  FDestroyer.Add(Animation, AOnFinish);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.OnFinish := FDestroyer.DoAniFinished;
  Animation.Duration := Duration;
  Animation.Delay := Delay;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.Start;

  if not Animation.Enabled then
    FDestroyer.DoAniFinishedEx(Animation, False);
end;

class procedure TFrameAnimator.CreateDestroyer;
begin
  if FDestroyer = nil then
    FDestroyer := TAnimationDestroyer.Create;
end;

class procedure TFrameAnimator.DelayExecute(const Owner: TFmxObject; AOnFinish: TNotifyEventA;
  Delay: Single);
var
  Animation: TDelayExecute;
begin
  CreateDestroyer;

  Animation := TDelayExecute.Create(nil);

  FDestroyer.Add(Animation, AOnFinish);
  Animation.Parent := Owner;
  Animation.AnimationType := TAnimationType.In;
  Animation.Interpolation := TInterpolationType.Linear;
  Animation.OnFinish := FDestroyer.DoAniFinished;
  Animation.Duration := Delay;
  Animation.Delay := 0;
  Animation.Start;

  if not Animation.Enabled then
    FDestroyer.DoAniFinishedEx(Animation, False);
end;

class procedure TFrameAnimator.Uninitialize;
begin
  FreeAndNil(FDestroyer);
end;

{ TFrameAnimator.TAnimationDestroyer }

procedure TFrameAnimator.TAnimationDestroyer.Add(Sender: TObject;
  AOnFinish: TNotifyEvent);
var
  Item: TFrameAnimatorEvent;
begin
  if not Assigned(AOnFinish) then
    Exit;
  Item.OnFinish := AOnFinish;
  Item.OnFinishA := nil;
  Item.OnProcess := nil;
  FOnFinishs.Add(Sender.GetHashCode, Item);
end;

procedure TFrameAnimator.TAnimationDestroyer.Add(Sender: TObject;
  AOnFinish: TNotifyEventA);
var
  Item: TFrameAnimatorEvent;
begin
  if not Assigned(AOnFinish) then
    Exit;
  Item.OnFinishA := AOnFinish;
  Item.OnFinish := nil;
  Item.OnProcess := nil;
  FOnFinishs.AddOrSetValue(Sender.GetHashCode, Item);
end;

procedure TFrameAnimator.TAnimationDestroyer.Add(Sender: TObject; AOnFinish,
  AOnProcess: TNotifyEventA);
var
  Item: TFrameAnimatorEvent;
begin
  if not Assigned(AOnFinish) then
    Exit;
  Item.OnFinishA := AOnFinish;
  Item.OnFinish := nil;
  Item.OnProcess := AOnProcess;
  FOnFinishs.AddOrSetValue(Sender.GetHashCode, Item);
end;

constructor TFrameAnimator.TAnimationDestroyer.Create;
begin
  FOnFinishs := TDictionary<Integer, TFrameAnimatorEvent>.Create(13);
end;

destructor TFrameAnimator.TAnimationDestroyer.Destroy;
begin
  FreeAndNil(FOnFinishs);
  inherited;
end;

procedure TFrameAnimator.TAnimationDestroyer.DoAniFinished(Sender: TObject);
begin
  DoAniFinishedEx(Sender, True);
end;

procedure TFrameAnimator.TAnimationDestroyer.DoAniFinishedEx(Sender: TObject;
  FreeSender: Boolean);
var
  Item: TFrameAnimatorEvent;
  Key: Integer;
begin
  Key := Sender.GetHashCode;
  if FOnFinishs.TryGetValue(Key, Item) then begin
    FOnFinishs.Remove(Key);  // UI操作，默认是单线程，不作同步处理
  end else begin
    Item.OnFinish := nil;
    Item.OnFinishA := nil;
    Item.OnProcess := nil;
  end;
  if FreeSender then
    TAnimation(Sender).DisposeOf;
  try
    if Assigned(Item.OnFinish) then
      Item.OnFinish(Sender);
    if Assigned(Item.OnFinishA) then
      Item.OnFinishA(Sender);
  except
  end;
end;

procedure TFrameAnimator.TAnimationDestroyer.DoAniProcess(Sender: TObject);
var
  Item: TFrameAnimatorEvent;
begin
  if FOnFinishs.TryGetValue(Sender.GetHashCode, Item) then begin
    if Assigned(Item.OnProcess) then
      Item.OnProcess(Sender);
  end;
end;

{ TDelayExecute }

procedure TDelayExecute.FirstFrame;
begin
end;

procedure TDelayExecute.ProcessAnimation;
begin
end;

procedure TDelayExecute.Start;
begin
  inherited Start;
end;

procedure TDelayExecute.Stop;
begin
  inherited Stop;
end;

{ TFloatExAnimation }

function TFloatExAnimation.FindProperty: Boolean;
begin
  Result := True;
end;

procedure TFloatExAnimation.Start;
begin
  inherited Start;
end;

{ TInt64Animation }

constructor TInt64Animation.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TInt64Animation.FirstFrame;
begin
  inherited;

end;

procedure TInt64Animation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind in [tkInt64]) then
        P.SetValue(FInstance, InterpolateInt64(FStartValue, FStopValue, NormalizedTime));
    end;
  end;
end;

initialization

finalization
  TFrameAnimator.Uninitialize;

end.
