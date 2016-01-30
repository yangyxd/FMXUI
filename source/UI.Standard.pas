unit UI.Standard;

interface

uses
  UI.Base,
  {$IFDEF MSWINDOWS}UI.Debug, {$ENDIF}
  FMX.BehaviorManager,
  FMX.ActnList, FMX.Objects, System.Math, System.Actions, System.Rtti, FMX.Consts,
  System.TypInfo, FMX.Graphics, System.Generics.Collections, FMX.TextLayout,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors,
  FMX.Types, FMX.StdCtrls, FMX.Platform, FMX.Controls, FMX.InertialMovement;

type
  [ComponentPlatformsAttribute(AllCurrentPlatforms)]
  TTextView = class(TView, ICaption)
  private
    FOnTextChange: TNotifyEvent;
    FText: UI.Base.TTextSettings;
    FIsChanging: Boolean;
    function GetAutoSize: Boolean;
    function GetText: string;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetTextSettings(const Value: UI.Base.TTextSettings);
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure DblClick; override;
    procedure PaintBackground; override;
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
    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('YxdFMX', [TTextView]);
end;

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

procedure TTextView.DoLayoutChanged(Sender: TObject);
begin
  if FText.AutoSize then
    Resize;
  inherited DoLayoutChanged(Sender);
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

function TTextView.GetText: string;
begin
  Result := FText.Text;
end;

procedure TTextView.Loaded;
begin
  inherited;
  FText.OnChanged := DoChanged;
  Change;
end;

procedure TTextView.PaintBackground;
begin
  inherited PaintBackground;
  if (Assigned(FText)) then
    FText.Draw(Canvas,
      RectF(Padding.Left, Padding.Top, Width - Padding.Right, Height - Padding.Bottom),
      GetAbsoluteOpacity);
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
  FText := Value;
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

end.
