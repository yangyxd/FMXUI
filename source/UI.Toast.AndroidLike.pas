unit UI.Toast.AndroidLike;

// FMX Cross Plattform Toast Component by Roland Kossow (https://www.cybertribe.de)
// How to use: Configure location via Objectinspector or at runtime and call
// NameOfToastcomponent.Now('Text to Toast');
// 14/4/10: Added a WordWrapProperty

interface

{$IFNDEF ANDROID}
uses
  System.SysUtils,
  System.Classes,
  System.Types,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Objects,
  System.UITypes,
  FMX.Graphics,
  System.Actions,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
  FMX.Styles,
  FMX.TextLayout,
  FMX.Effects,
  FMX.Layouts;
{$ENDIF}

{$IFNDEF ANDROID}
type

  TToast = class(TFMXObject)
  private
    FAppearanceDuration: Integer;
    FDisappearanceDuration: Integer;
    FLayout: TLayout;
    FRectangle: TRectangle;
    FTextfield: TLabel;
    FTimer: TTimer;
    FTextColor: TAlphaColor;
    FToastBoxColor: TAlphaColor;
    FToastBoxStrokeDash: TStrokeDash;
    FDuration: Integer;
    FInfo: String;
    FOpacity: Single;
    FOptimalWidth: Boolean;
    FTextAlign: TTextAlign;
    FTextFont: TFont;
    FToastBoxAlign: TAlignLayout;
    FToastBoxHeight: Single;
    FToastBoxMargin: TBounds;
    FToastBoxStrokeColor: TAlphaColor;
    FToastBoxStrokeThickness: Single;
    FToastboxPosition: TPosition;
    FToastBoxWidth: Single;
    FVersion: String;
    FWordWrap: Boolean;
    procedure SetDuration(const Value: Integer);
    procedure HideToast(Sender: TObject);
    procedure SetOpacity(const Value: Single);
    procedure SetOptimalWidth(const Value: Boolean);
    procedure SetToastBoxColor(const Value: TAlphaColor);
    procedure SetToastBoxStrokeDash(const Value: TStrokeDash);
    procedure SetTextAlign(const Value: TTextAlign);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetTextFont(const Value: TFont);
    procedure SetToastBoxAlign(const Value: TAlignLayout);
    procedure SetToastBoxMargin(const Value: TBounds);
    procedure SetToastBoxStrokeColor(const Value: TAlphaColor);
    procedure SetToastBoxStrokeThickness(const Value: Single);
    procedure SetToastboxPosition(const Value: TPosition);
    procedure SetWordWrap(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Now(aToastString: String);
  published
    property AppearanceDuration: Integer read FAppearanceDuration
      write FAppearanceDuration;
    property DisappearanceDuration: Integer read FDisappearanceDuration
      write FDisappearanceDuration;
    property ToastBoxStrokeColor: TAlphaColor read FToastBoxStrokeColor
      write SetToastBoxStrokeColor;
    property TextColor: TAlphaColor read FTextColor write SetTextColor;
    property ToastBoxColor: TAlphaColor read FToastBoxColor
      write SetToastBoxColor;
    property ToastBoxStrokeDash: TStrokeDash read FToastBoxStrokeDash
      write SetToastBoxStrokeDash;
    property TextAlign: TTextAlign read FTextAlign write SetTextAlign;
    property Duration: Integer read FDuration write SetDuration;
    property Info: String read FInfo;
    property Opacity: Single read FOpacity write SetOpacity;
    property OptimalWidth: Boolean read FOptimalWidth write SetOptimalWidth;
    property TextFont: TFont read FTextFont write SetTextFont;
    property ToastBoxAlign: TAlignLayout read FToastBoxAlign
      write SetToastBoxAlign;
    property ToastBoxHeight: Single read FToastBoxHeight write FToastBoxHeight;
    property ToastBoxMargin: TBounds read FToastBoxMargin
      write SetToastBoxMargin;
    property ToastBoxStrokeThickness: Single read FToastBoxStrokeThickness
      write SetToastBoxStrokeThickness;
    property ToastboxPosition: TPosition read FToastboxPosition
      write SetToastboxPosition;
    property ToastBoxWidth: Single read FToastBoxWidth write FToastBoxWidth;
    property Version: String read FVersion;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
  end;
{$ENDIF}

{$IFNDEF ANDROID}
procedure Register;
{$ENDIF}

implementation

{$IFNDEF ANDROID}
uses System.RTLConsts, FMX.Consts, FMX.Forms, FMX.Ani, System.Math;

Const
  INFO_URL: string = 'https://www.cybertribe.de/info/components/fmx/toast';
  COMPONENT_VERSION: string = '0.4 Alpha';

procedure Register;
begin
  RegisterComponents('AndroidLike', [TToast]);
end;

constructor TToast.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOpacity := 1.0;
  FAppearanceDuration := 300;
  FDisappearanceDuration := 300;
  FDuration := 3000;
  FToastboxPosition := TPosition.Create(TPointF.Create(0, 0));
  FToastBoxMargin := TBounds.Create(TRectF.Create(100, 100, 100, 30));
  FToastBoxWidth := 100;
  FToastBoxStrokeThickness := 1;
  FTextColor := TAlphaColorRec.Black;
  FToastBoxColor := TAlphaColorRec.Gray;
  FToastBoxHeight := 50;
  FToastBoxAlign := TAlignLayout.alCenter;
  FTextFont := TFont.Create;
  FVersion := COMPONENT_VERSION;
  FInfo := INFO_URL;
  FWordWrap := true;
  if not(csDesigning in ComponentState) then
  begin
    self.FLayout := TLayout.Create(AOwner);
    self.FLayout.Parent := TFMXObject(self.Parent);
    self.FLayout.Visible := false;
    self.FRectangle := TRectangle.Create(AOwner);
    self.FRectangle.Parent := self.FLayout;
    self.FRectangle.Visible := false;
    self.FTextfield := TLabel.Create(AOwner);
    self.FTextfield.WordWrap := self.FWordWrap;
    self.FTextfield.Visible := false;
    self.FTextfield.FontColor := TAlphaColorRec.Red;
    self.FTextfield.Stored := false;
    self.FTextfield.Name := self.Name + 'Textfield';
    self.FTextfield.Parent := self.FLayout;
    self.FTextfield.Text := '';
    self.FTextfield.Align := TAlignLayout.alClient;
    self.FTextfield.StyledSettings := [];
    self.FTimer := TTimer.Create(AOwner);
    self.FTimer.Name := self.Name + 'Timer';
    self.FTimer.Enabled := false;
    self.FTimer.Interval := 1000;
    self.FTimer.OnTimer := self.HideToast;
  end;
end;

destructor TToast.Destroy;
begin
  FTextFont.Free;
  FToastboxPosition.Free;
  FToastBoxMargin.Free;
  // FLayout.Free;
  inherited Destroy;
end;

procedure TToast.HideToast(Sender: TObject);
begin
  self.FTimer.Enabled := false;
  self.FRectangle.AnimateFloat('Opacity', 0, FDisappearanceDuration / 1000);
  self.FTextfield.AnimateFloat('Opacity', 0, FDisappearanceDuration / 1000);
end;

procedure TToast.SetDuration(const Value: Integer);
begin
  FDuration := Value;
end;

procedure TToast.Now(aToastString: String);
var
  ATextRect: TRectF;
  sidemargin: Single;
begin
  ATextRect := TRectF.Create(TPointF.Create(0, 0));
  self.FLayout.BeginUpdate;

  self.FLayout.Parent := self.Parent;
  self.FLayout.Align := FToastBoxAlign;
  self.FLayout.Position := FToastboxPosition;
  self.FLayout.Margins := FToastBoxMargin;
  self.FLayout.Opacity := 0;
  self.FLayout.Visible := true;
  self.FLayout.Width := FToastBoxWidth;
  self.FLayout.Height := FToastBoxHeight;

  self.FTextfield.Font := FTextFont;
  self.FTextfield.FontColor := FTextColor;
  self.FTextfield.TextAlign := FTextAlign;
  self.FTextfield.Text := aToastString;
  self.FTextfield.Opacity := 1.0;
  self.FTextfield.Visible := true;
  self.FTextfield.WordWrap := FWordWrap;

  self.FRectangle.Align := TAlignLayout.alClient;
  //self.FRectangle.StrokeThickness := FToastBoxStrokeThickness;
  //self.FRectangle.StrokeDash := FToastBoxStrokeDash;
  //self.FRectangle.Stroke.Color := FToastBoxStrokeColor;
  self.FRectangle.Stroke.Thickness := FToastBoxStrokeThickness;
  self.FRectangle.Stroke.Dash := FToastBoxStrokeDash;
  self.FRectangle.Stroke.Color := FToastBoxStrokeColor;
  self.FRectangle.Fill.Color := FToastBoxColor;
  self.FRectangle.Opacity := 0;
  self.FRectangle.Visible := true;
  self.FRectangle.BringToFront;
  self.FRectangle.Repaint;
  self.FRectangle.Opacity := FOpacity;

  self.FTextfield.BringToFront;
  self.FTextfield.Repaint;

  if (FOptimalWidth) and (FWordWrap) then
  begin
    if FToastBoxAlign = TAlignLayout.alNone then
    begin
      self.FLayout.Width := Application.MainForm.Width - 50;
    end
    else
    begin
      sidemargin := (Application.MainForm.Width -
        self.FTextfield.Canvas.TextWidth(aToastString) + 50) / 2;
      self.FLayout.Margins.Left := sidemargin;
      self.FLayout.Margins.Right := sidemargin;
    end;
  end;

  if (FOptimalWidth) and (not FWordWrap) then
  begin
    if FToastBoxAlign = TAlignLayout.alNone then
    begin
      self.FLayout.Width :=
        (Max(Application.MainForm.Width - 50,
        self.FTextfield.Canvas.TextWidth(aToastString)));
    end
    else
    begin
      self.FLayout.Width := self.FTextfield.Canvas.TextWidth(aToastString) + 20;

      // sidemargin := 25;
      // self.FLayout.Margins.Left := sidemargin;
      // self.FLayout.Margins.Right := sidemargin;
    end;
  end;

  self.FRectangle.BringToFront;
  self.FLayout.BringToFront;
  self.FLayout.Repaint;
  self.FRectangle.Repaint;
  self.FTextfield.BringToFront;
  self.FTextfield.Repaint;
  self.FLayout.EndUpdate;
  self.FLayout.AnimateFloat('Opacity', 1.0, FAppearanceDuration / 1000);
  self.FTimer.Interval := FDuration;
  self.FTimer.Enabled := true;
end;

procedure TToast.SetOpacity(const Value: Single);
begin
  FOpacity := Value;
end;

procedure TToast.SetOptimalWidth(const Value: Boolean);
begin
  FOptimalWidth := Value;
end;

procedure TToast.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
end;

procedure TToast.SetToastBoxColor(const Value: TAlphaColor);
begin
  FToastBoxColor := Value;
end;

procedure TToast.SetToastBoxStrokeDash(const Value: TStrokeDash);
begin
  FToastBoxStrokeDash := Value;
end;

procedure TToast.SetTextAlign(const Value: TTextAlign);
begin
  FTextAlign := Value;
end;

procedure TToast.SetTextFont(const Value: TFont);
begin
  FTextFont.Assign(Value);
end;

procedure TToast.SetToastBoxAlign(const Value: TAlignLayout);
begin
  FToastBoxAlign := Value;
end;

procedure TToast.SetToastBoxMargin(const Value: TBounds);
begin
  FToastBoxMargin.Assign(Value);
end;

procedure TToast.SetToastBoxStrokeColor(const Value: TAlphaColor);
begin
  FToastBoxStrokeColor := Value;
end;

procedure TToast.SetToastBoxStrokeThickness(const Value: Single);
begin
  FToastBoxStrokeThickness := Value;
end;

procedure TToast.SetToastboxPosition(const Value: TPosition);
begin
  FToastboxPosition.Assign(Value);
end;

procedure TToast.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

{$ENDIF}

end.
