unit UI.Ani;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.Rtti, System.SyncObjs,
  FMX.Ani,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Platform, IOUtils;

type
  /// <summary>
  /// 动画类型
  /// </summary>
  TFrameAniType = (None, DefaultAni {默认}, FadeInOut {淡入淡出},
    MoveInOut {移进移出}, BottomMoveInOut {底部弹进弹出},
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

  TFrameAnimator = class
  private type
    TFrameAnimatorEvent = record
      OnFinish: TNotifyEvent;
      OnFinishA: TNotifyEventA;
    end;
    TAnimationDestroyer = class
    private
      FOnFinishs: TDictionary<Integer, TFrameAnimatorEvent>;
      procedure DoAniFinished(Sender: TObject);
      procedure DoAniFinishedEx(Sender: TObject; FreeSender: Boolean);
    public
      constructor Create();
      destructor Destroy; override;
      procedure Add(Sender: TObject; AOnFinish: TNotifyEvent); overload;
      procedure Add(Sender: TObject; AOnFinish: TNotifyEventA); overload;
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

implementation

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
  FOnFinishs.Add(Sender.GetHashCode, Item);
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
  if FOnFinishs.ContainsKey(Key) then begin
    Item := FOnFinishs[Key];
    FOnFinishs.Remove(Key);  // UI操作，默认是单线程，不作同步处理
  end else begin
    Item.OnFinish := nil;
    Item.OnFinishA := nil;
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

initialization

finalization
  TFrameAnimator.Uninitialize;

end.
