unit UI.Standard;

interface

uses
  UI.Base,
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
  FMX.BehaviorManager, FMX.Forms, System.Messaging,
  FMX.ActnList, FMX.Objects, System.Math, System.Actions, System.Rtti, FMX.Consts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, FMX.TextLayout,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement;

type
  TOnDrawText = procedure (Sender: TObject; Canvas: TCanvas;
    Text: UI.Base.TTextSettings; R: TRectF) of object;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TTextView = class(TView, ICaption)
  private
    FOnTextChange: TNotifyEvent;
    FText: UI.Base.TTextSettings;
    FDrawable: TDrawableIcon;
    FIsChanging: Boolean;
    FOnDrawText: TOnDrawText;
    function GetAutoSize: Boolean;
    function GetText: string;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetTextSettings(const Value: UI.Base.TTextSettings);
    function GetDrawable: TDrawableIcon;
    procedure SetDrawable(const Value: TDrawableIcon);
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure DblClick; override;
    procedure ImagesChanged; override;
    procedure PaintBackground; override;
    procedure DoPaintBackground(var R: TRectF); virtual;
    procedure DoPaintText(var R: TRectF); virtual;
    procedure SetGravity(const Value: TLayoutGravity); override;
    procedure DoLayoutChanged(Sender: TObject); override;
    procedure ActionChange(Sender: TBasicAction; CheckDefaults: Boolean); override;
    function GetDefaultSize: TSizeF; override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure SetName(const Value: TComponentName); override;
  protected
    function TextStored: Boolean;
    procedure DoChanged(Sender: TObject); virtual;
    procedure DoDrawableChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ToString: string; override;
    procedure AfterConstruction; override;
    procedure Change;
  published
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default True;
    property Text: string read GetText write SetText stored TextStored;
    property TextSettings: UI.Base.TTextSettings read FText write SetTextSettings;
    property Drawable: TDrawableIcon read GetDrawable write SetDrawable;
    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property OnDrawText: TOnDrawText read FOnDrawText write FOnDrawText;
  end;

type
  TOnDrawViewBackgroud = procedure (Sender: TObject; Canvas: TCanvas;
    R: TRectF; State: TViewState; BgBrush: TViewBrush;
    BorderBrush: TStrokeBrush) of object;

type
  TStyleView = class(TTextView)
  private
    FDefaultBackground: TViewBrush;
    FDefaultPenBrush: TStrokeBrush;
    FOnDrawViewBackgroud: TOnDrawViewBackgroud;
  protected
    procedure PaintBackground; override;
    procedure DoPaintBackground(var R: TRectF); override;
    procedure DoDrawStyleControl(var R: TRectF); virtual;
    function CanRePaintBk(const View: IView; State: TViewState): Boolean; override;
    // Ä¬ÈÏ±³¾°Ë¢×Ó
    property DefaultBgBrush: TViewBrush read FDefaultBackground;
    // Ä¬ÈÏ±ß¿òË¢×Ó
    property DefaultBorderBrush: TStrokeBrush read FDefaultPenBrush;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnDrawViewBackgroud: TOnDrawViewBackgroud read FOnDrawViewBackgroud
      write FOnDrawViewBackgroud;
  end;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TButtonView = class(TStyleView)
  private
    FDefault: Boolean;
    FCancel: Boolean;
    FModalResult: TModalResult;
  protected
    function GetDefaultSize: TSizeF; override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure AfterDialogKey(var Key: Word; Shift: TShiftState); override;
  published
    property CanFocus default True;
    property CanParentFocus;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Default: Boolean read FDefault write FDefault default False;
    property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;
    property Clickable default True;
    property Gravity default TLayoutGravity.Center;
    property OnCanFocus;
  end;

implementation

{ TTextView }

procedure TTextView.ActionChange(Sender: TBasicAction; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then begin
    if not CheckDefaults or Text.IsEmpty or (Text = Name) then
     Text := TCustomAction(Sender).Text;
  end;
  inherited ActionChange(Sender, CheckDefaults);
end;

procedure TTextView.AfterConstruction;
begin
  inherited AfterConstruction;
  FText.OnChanged := DoChanged;
  FText.OnTextChanged := FOnTextChange;
  FIsChanging := False;
end;

procedure TTextView.Change;
begin
  if not FIsChanging and ([csLoading, csDestroying] * ComponentState = []) and not Released then
  begin
    FIsChanging := True;
    try
      DoChanged(FText);
    finally
      FIsChanging := False;
    end;
  end;
end;

constructor TTextView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsChanging := True;
  FText := UI.Base.TTextSettings.Create(Self);
  if csDesigning in ComponentState then begin
    FDrawable := TDrawableIcon.Create(Self);
    FDrawable.SizeWidth := 16;
    FDrawable.SizeHeight := 16;
    FDrawable.OnChanged := DoDrawableChanged;
  end;
  SetAcceptsControls(False);
end;

procedure TTextView.DblClick;
begin
  inherited DblClick;
  Click;
end;

destructor TTextView.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FDrawable);
  inherited;
end;

procedure TTextView.DoChanged(Sender: TObject);
begin
  FGravity := FText.Gravity;
  if FText.IsSizeChange then
    RecalcSize
  else
    Repaint;
  if FText.IsEffectsChange then
    UpdateEffects;
end;

procedure TTextView.DoDrawableChanged(Sender: TObject);
begin
  DoChanged(Sender);
end;

procedure TTextView.DoLayoutChanged(Sender: TObject);
begin
  if FText.AutoSize then
    Resize;
  inherited DoLayoutChanged(Sender);
end;

procedure TTextView.DoPaintBackground(var R: TRectF);
begin
  R := RectF(R.Left + Padding.Left, R.Left + Padding.Top,
    R.Right - Padding.Right, R.Bottom - Padding.Bottom);
  if Assigned(FDrawable) and (not FDrawable.IsEmpty) then
    FDrawable.AdjustDraw(Canvas, R, True);
  if (Assigned(FText)) then
    DoPaintText(R);
end;

procedure TTextView.DoPaintText(var R: TRectF);
begin
  if Assigned(FOnDrawText) then
    FOnDrawText(Self, Canvas, FText, R)
  else
    FText.Draw(Canvas, R, GetAbsoluteOpacity, DrawState);
end;

function TTextView.GetAutoSize: Boolean;
begin
  Result := FText.AutoSize;
end;

function TTextView.GetData: TValue;
begin
  Result := Text;
end;

function TTextView.GetDefaultSize: TSizeF;
var
  DeviceInfo: IDeviceBehavior;
begin
  if TBehaviorServices.Current.SupportsBehaviorService(IDeviceBehavior, DeviceInfo, Self) then
    case DeviceInfo.GetOSPlatform(Self) of
      TOSPlatform.Windows:
        Result := TSizeF.Create(120, 17);
      TOSPlatform.OSX:
        Result := TSizeF.Create(120, 17);
      TOSPlatform.iOS:
        Result := TSize.Create(82, 21);
      TOSPlatform.Android:
        Result := TSize.Create(82, 23);
    end
  else
    Result := TSizeF.Create(120, 17);
end;

function TTextView.GetDrawable: TDrawableIcon;
begin
  if not Assigned(FDrawable) then begin
    FDrawable := TDrawableIcon.Create(Self);
    FDrawable.SizeWidth := 16;
    FDrawable.SizeHeight := 16;
    FDrawable.OnChanged := DoDrawableChanged;
  end;
  Result := FDrawable;
end;

function TTextView.GetText: string;
begin
  Result := FText.Text;
end;

procedure TTextView.ImagesChanged;
begin
  if Assigned(FDrawable) and (FDrawable.ImageIndex >= 0) then
    FDrawable.Change;
  inherited ImagesChanged;
end;

procedure TTextView.Loaded;
begin
  inherited;
  FText.OnChanged := DoChanged;
  Change;
end;

procedure TTextView.PaintBackground;
var
  R: TRectF;
begin
  inherited PaintBackground;
  R := RectF(0, 0, Width, Height);
  DoPaintBackground(R);
end;

procedure TTextView.Resize;
var
  ASize: TSizeF;
  R: TRectF;
begin
  inherited Resize;
  if (FDisableAlign) or (not Assigned(FText)) or (not Assigned(Scene)) or
    (not FText.AutoSize) then Exit;
  if not FText.CalcTextObjectSize(FMaxWidth, Scene.GetSceneScale, Margins, ASize) then Exit;
  FDisableAlign := True;
  try
    if (WidthSize = TViewSize.WrapContent) and (HeightSize = TViewSize.WrapContent) then begin
      R := BoundsRect;
      R.Width := ASize.Width + Padding.Left + Padding.Right;
      R.Height := ASize.Height + Padding.Top + Padding.Bottom;
      SetBoundsRect(R);
    end else if WidthSize = TViewSize.WrapContent then
      Width := ASize.Width + Padding.Left + Padding.Right
    else if HeightSize = TViewSize.WrapContent then
      Height := ASize.Height + Padding.Top + Padding.Bottom;
  finally
    FDisableAlign := False;
  end;
end;

procedure TTextView.SetAutoSize(const Value: Boolean);
begin
  FText.AutoSize := Value;
end;

procedure TTextView.SetData(const Value: TValue);
begin
  if Value.IsEmpty then
    Text := ''
  else
    Text := Value.ToString;
end;

procedure TTextView.SetDrawable(const Value: TDrawableIcon);
begin
  Drawable.Assign(Value);
end;

procedure TTextView.SetGravity(const Value: TLayoutGravity);
begin
  FGravity := Value;
  FText.Gravity := Value;
end;

procedure TTextView.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (csLoading in TComponent(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then
    Text := Value;
end;

procedure TTextView.SetText(const Value: string);
begin
  FText.Text := Value;
end;

procedure TTextView.SetTextSettings(const Value: UI.Base.TTextSettings);
begin
  FText.Assign(Value);
end;

function TTextView.TextStored: Boolean;
begin
  Result := (not Text.IsEmpty and not ActionClient) or (not (ActionClient and (ActionLink <> nil) and
    ActionLink.CaptionLinked and (Action is TContainedAction)));
end;

function TTextView.ToString: string;
begin
  Result := Format('%s ''%s''', [inherited ToString, FText]);
end;

{ TStyleView }

function TStyleView.CanRePaintBk(const View: IView; State: TViewState): Boolean;
begin
  if Assigned(FOnDrawViewBackgroud) or (FBackground = nil) or (FBackground.IsEmpty) then
    Result := True
  else
    Result := inherited CanRePaintBk(View, State);
end;

constructor TStyleView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TStyleView.Destroy;
begin
  FreeAndNil(FDefaultBackground);
  FreeAndNil(FDefaultPenBrush);
  inherited Destroy;
end;

procedure TStyleView.DoDrawStyleControl(var R: TRectF);
var
  AState: TViewState;
  ABorderColor, ABgColor: TAlphaColor;
  RX, RY: Single;
begin
  AState := DrawState;
  ABorderColor := $7FCCCCCC;
  ABgColor := $FFF0F1F2;
  case AState of
    TViewState.Pressed:
      begin
        ABorderColor := $FFCCCCCC;
        ABgColor := $FFE0E0E0;
      end;
    TViewState.Focused, TViewState.Activated:
      begin
        ABorderColor := $AFCCCCCC;
      end;
    TViewState.Hovered, TViewState.Selected,
    TViewState.Checked:
      begin
        ABorderColor := $AFCCCCCC;
      end;
    TViewState.Enabled:
      begin
        ABgColor := $FFE0E0E0;
      end;
  end;

  if FBackground <> nil then begin
    RX := FBackground.XRadius;
    RY := FBackground.YRadius;
  end else begin
    RX := 0;
    RY := 0;
  end;

  if FDefaultBackground = nil then
    FDefaultBackground := TViewBrush.Create(TBrushKind.Solid, ABgColor);
  FDefaultBackground.Color := ABgColor;
  if FDefaultPenBrush = nil then
    FDefaultPenBrush := TStrokeBrush.Create(TBrushKind.Solid, ABorderColor);
  FDefaultPenBrush.Color := ABorderColor;

  if Assigned(FOnDrawViewBackgroud) then
    FOnDrawViewBackgroud(Self, Canvas, R, AState, FDefaultBackground, FDefaultPenBrush)
  else begin
    Canvas.FillRect(R, RX, RY, AllCorners, Opacity, FDefaultBackground);
    Canvas.DrawRect(R, RX, RY, AllCorners, Opacity, FDefaultPenBrush);
  end;
end;

procedure TStyleView.DoPaintBackground(var R: TRectF);
begin
  if Assigned(FOnDrawViewBackgroud) or (FBackground = nil) or (FBackground.IsEmpty) then
    DoDrawStyleControl(R);
  inherited DoPaintBackground(R);
end;

procedure TStyleView.PaintBackground;
var
  R: TRectF;
begin
  if not Assigned(FOnDrawViewBackgroud) then begin
    inherited PaintBackground;
  end else begin
    R := RectF(0, 0, Width, Height);
    DoPaintBackground(R);
  end;
end;

{ TButtonView }

procedure TButtonView.AfterDialogKey(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Default and (Key = vkReturn)) or (Cancel and (Key = vkEscape)) then
  begin
    Click;
    Key := 0;
  end;
end;

procedure TButtonView.Click;
var
  O: TComponent;
begin
  inherited Click;
  if (Self <> nil) and (ModalResult <> mrNone) then
  begin
    O := Scene.GetObject;
    while O <> nil do
    begin
      if (O is TCommonCustomForm) then
      begin
        TCommonCustomForm(O).ModalResult := FModalResult;
        Break;
      end;
      O := O.Owner;
    end;
  end;
end;

constructor TButtonView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Clickable := True;
  CanFocus := True;
  Padding.Rect := RectF(4, 4, 4, 4);
  Gravity := TLayoutGravity.Center;
end;

function TButtonView.GetDefaultSize: TSizeF;
var
  DeviceInfo: IDeviceBehavior;
begin
  if TBehaviorServices.Current.SupportsBehaviorService(IDeviceBehavior, DeviceInfo, Self) then
    case DeviceInfo.GetOSPlatform(Self) of
      TOSPlatform.Windows:
        Result := TSizeF.Create(80, 22);
      TOSPlatform.OSX:
        Result := TSizeF.Create(80, 22);
      TOSPlatform.iOS:
        Result := TSizeF.Create(73, 44);
      TOSPlatform.Android:
        Result := TSizeF.Create(73, 44);
    end
  else
    Result := TSizeF.Create(80, 22);
end;

initialization

finalization

end.
