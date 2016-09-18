unit UI.Design.Bounds;

interface

uses
  System.Math,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts;

type
  TBoundsDesigner = class(TForm)
    Layout1: TLayout;
    Button2: TButton;
    btnOk: TButton;
    labelScale: TLabel;
    trackScale: TTrackBar;
    edtScale: TEdit;
    Label5: TLabel;
    Button1: TButton;
    Layout3: TLayout;
    Label1: TLabel;
    edtLeft: TEdit;
    Label2: TLabel;
    edtTop: TEdit;
    Label3: TLabel;
    edtRight: TEdit;
    Label4: TLabel;
    edtBottom: TEdit;
    ScrollBox1: TScrollBox;
    Preview: TPaintBox;
    Panel1: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtLeftExit(Sender: TObject);
    procedure PreviewPaint(Sender: TObject; Canvas: TCanvas);
    procedure trackScaleChange(Sender: TObject);
    procedure trackScaleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure edtScaleChange(Sender: TObject);
    procedure edtLeftChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtScaleChangeTracking(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FChanging: Boolean;
    FBitmap: TBitmap;
    FCheckboardBitmap: TBitmap;
    FBounds: TRectF;
    FOldScale: single;
    FCtrlRects: array [0..3] of TRectF;
    FCurCtrl: Integer;
    FX, FY: Single;
    procedure SetBitmap(const Value: TBitmap);
    procedure PrepareCheckboardBitmap;
    procedure UpdateValue;
    procedure InitValue;

    function GetScrollBoxRect: TRectF;
    function PrepareForCrop: TRectF;
  public
    { Public declarations }
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Bounds: TRectF read FBounds write FBounds;
  end;

var
  BoundsDesigner: TBoundsDesigner;

implementation

{$R *.fmx}

procedure TBoundsDesigner.btnOkClick(Sender: TObject);
begin
  UpdateValue();
  ModalResult := mrOk;
end;

procedure TBoundsDesigner.Button1Click(Sender: TObject);
begin
  PrepareForCrop;
  Preview.Width := FBitmap.Width * trackScale.Value;
  Preview.Height := FBitmap.Height * trackScale.Value;
  Preview.Repaint;
end;

procedure TBoundsDesigner.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TBoundsDesigner.edtLeftChange(Sender: TObject);
begin
  if FChanging then Exit;
  UpdateValue;
  Preview.Repaint;
end;

procedure TBoundsDesigner.edtLeftExit(Sender: TObject);
begin
  TEdit(Sender).Text := FloatToStr(StrToFloatDef(TEdit(Sender).Text, 0));
  UpdateValue;
  Preview.Repaint;
end;

procedure TBoundsDesigner.edtScaleChange(Sender: TObject);
begin
  if FChanging then Exit;
  trackScale.Value := StrToIntDef(edtScale.Text, 100) / 100;
end;

procedure TBoundsDesigner.edtScaleChangeTracking(Sender: TObject);
begin
  if FChanging then Exit;
  FChanging := True;
  trackScale.Value := StrToIntDef(edtScale.Text, 100) / 100;
  FChanging := False;
end;

procedure TBoundsDesigner.FormCreate(Sender: TObject);
begin
  FBitmap := TBitmap.Create(0, 0);
  FOldScale := 1;
end;

procedure TBoundsDesigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCheckboardBitmap);
  FreeAndNil(FBitmap);
end;

procedure TBoundsDesigner.FormShow(Sender: TObject);
begin
  InitValue();
end;

function TBoundsDesigner.GetScrollBoxRect: TRectF;
begin
  Result := ScrollBox1.LocalRect;
  Result.Width := Result.Width - 8;
  Result.Height := Result.Height - 8;
  if (Result.Width > ScrollBox1.ClientWidth) then
    Result.Width := ScrollBox1.ClientWidth;
  if (Result.Height > ScrollBox1.ClientHeight) then
    Result.Height := ScrollBox1.ClientHeight;
end;

procedure TBoundsDesigner.InitValue;
begin
  FChanging := True;
  edtLeft.Text := FloatToStr(FBounds.Left);
  edtTop.Text := FloatToStr(FBounds.Top);
  edtRight.Text := FloatToStr(FBounds.Right);
  edtBottom.Text := FloatToStr(FBounds.Bottom);
  FChanging := False;
end;

procedure TBoundsDesigner.PrepareCheckboardBitmap;
var
  i, j: Integer;
  M: TBitmapData;
begin
  if not Assigned(FCheckboardBitmap) then
  begin
    FCheckboardBitmap := TBitmap.Create(32, 32);
    if FCheckboardBitmap.Map(TMapAccess.Write, M) then
    try
      for j := 0 to FCheckboardBitmap.Height - 1 do
      begin
        for i := 0 to FCheckboardBitmap.Width - 1 do
        begin
          if odd(i div 8) and not odd(j div 8) then
            M.SetPixel(i, j, $FFA0A0A0)
          else if not odd(i div 8) and odd(j div 8) then
            M.SetPixel(i, j, $FFA0A0A0)
          else
            M.SetPixel(i, j, $FFFFFFFF)
        end;
      end;
    finally
      FCheckboardBitmap.Unmap(M);
    end;
  end;
end;

function TBoundsDesigner.PrepareForCrop: TRectF;
var
  ViewRect: TRectF;
  NewScale: Single;
  Pos: TPointF;
begin
  ViewRect := GetScrollBoxRect;

  if (FBitmap.Width < ViewRect.Width) and (FBitmap.Height < ViewRect.Height) then
  begin
    NewScale := 1 / RectF(0, 0, FBitmap.Width, FBitmap.Height).Fit(ViewRect);

    if (NewScale > trackScale.Value) then
      trackScale.Value := NewScale;
  end;
  Pos := ScrollBox1.ViewportPosition;
  Result.Left := Trunc(Min(Pos.X + ScrollBox1.ClientWidth * 0.1,
    FBitmap.Width * 0.1 * trackScale.Value));
  Result.Right := Trunc(Min(Pos.X + ScrollBox1.ClientWidth * 0.9,
    FBitmap.Width * 0.9 * trackScale.Value));
  Result.Top := Trunc(Min(Pos.Y + ScrollBox1.ClientHeight * 0.1,
    FBitmap.Height * 0.1 * trackScale.Value));
  Result.Bottom := Trunc(Min(Pos.Y + ScrollBox1.ClientHeight * 0.9,
    FBitmap.Height * 0.9 * trackScale.Value));
end;

procedure TBoundsDesigner.PreviewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);

  function GetMoveInIndex(const X, Y: Single): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to High(FCtrlRects) do begin
      if (((I = 0) or (I = 3)) and (X >= FCtrlRects[I].Left) and (X <= FCtrlRects[I].Right)) or
        (((I = 1) or (I = 2)) and (Y >= FCtrlRects[I].Top) and (Y <= FCtrlRects[I].Bottom)) then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;

  procedure SetCursor(const ACursor: TCursor);
  begin
    Preview.Cursor := ACursor;
  end;

var
  AV: Single;
begin
  if not (ssLeft in Shift) then begin
    FCurCtrl := GetMoveInIndex(X, Y);
    if (FCurCtrl = 0) or (FCurCtrl = 3) then
      SetCursor(crHSplit)
    else if (FCurCtrl = 1) or (FCurCtrl = 2) then
      SetCursor(crVSplit)
    else begin
      SetCursor(crDefault);
      Exit;
    end;
    FX := X;
    FY := Y;
  end else if FCurCtrl <> -1 then begin
    // Ë®Æ½
    if (FCurCtrl = 0) or (FCurCtrl = 3) then begin
      AV := (X - FX) / trackScale.Value;
      if Abs(AV) < 1 then Exit;
      if FCurCtrl = 0 then
        FBounds.Left := Round(FBounds.Left + AV)
      else
        FBounds.Right := Round(FBounds.Right - AV);
      Preview.Repaint;
      InitValue();
    // ´¹Ö±
    end else if (FCurCtrl = 1) or (FCurCtrl = 2) then begin
      AV := (Y - FY) / trackScale.Value;
      if Abs(AV) < 1 then Exit;
      if FCurCtrl = 1 then
        FBounds.Top := Round(FBounds.Top + AV)
      else
        FBounds.Bottom := Round(FBounds.Bottom - AV);
      Preview.Repaint;
      InitValue();
    end;
    FX := X;
    FY := Y;
  end;
end;

procedure TBoundsDesigner.PreviewPaint(Sender: TObject; Canvas: TCanvas);
const
  CS = 4;
var
  FSourceRect: TRectF;
  S: Single;
begin
  S := trackScale.Value;
  // »­±³¾°
  FSourceRect := RectF(0, 0, FBitmap.Width, FBitmap.Height);
  PrepareCheckboardBitmap;
  Canvas.Fill.Kind := TBrushKind.Bitmap;
  Canvas.Fill.Bitmap.Bitmap := FCheckboardBitmap;
  Canvas.FillRect(
    RectF(0, 0, RectWidth(FSourceRect) * S, RectHeight(FSourceRect) * S),
    0, 0, [], 1);
  // »­Í¼Ïñ
  if S > 1 then
    Canvas.DrawBitmap(FBitmap, FSourceRect,
      RectF(0, 0, RectWidth(FSourceRect) * S, RectHeight(FSourceRect) * S),
      Preview.AbsoluteOpacity, True)
  else
    Canvas.DrawBitmap(FBitmap, FSourceRect,
      RectF(0, 0, RectWidth(FSourceRect) * S, RectHeight(FSourceRect) * S),
      Preview.AbsoluteOpacity, False);
  // »­±ß¿ò
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := $6F3366ff;
  FSourceRect := RectF(FBounds.Left * S, FBounds.Top * S,
      (RectWidth(FSourceRect) - FBounds.Right) * S,
      (RectHeight(FSourceRect) - FBounds.Bottom) * S);
  Canvas.FillRect(FSourceRect, 0, 0, [], 1);

  // »­¿ØÖÆµã

  // ×ó
  FCtrlRects[0].Left := FSourceRect.Left - CS;
  FCtrlRects[0].Top := FSourceRect.Top + FSourceRect.Height / 2 - CS;
  FCtrlRects[0].Right := FCtrlRects[0].Left + CS * 2;
  FCtrlRects[0].Bottom := FCtrlRects[0].Top + CS * 2;
  // ÉÏ
  FCtrlRects[1].Left := FSourceRect.Left + FSourceRect.Width / 2 - CS;
  FCtrlRects[1].Top := FSourceRect.Top - CS;
  FCtrlRects[1].Right := FCtrlRects[1].Left + CS * 2;
  FCtrlRects[1].Bottom := FCtrlRects[1].Top + CS * 2;
  // ÏÂ
  FCtrlRects[2].Left := FSourceRect.Left + FSourceRect.Width / 2 - CS;
  FCtrlRects[2].Top := FSourceRect.Bottom - CS;
  FCtrlRects[2].Right := FCtrlRects[2].Left + CS * 2;
  FCtrlRects[2].Bottom := FCtrlRects[2].Top + CS * 2;
  // ÓÒ
  FCtrlRects[3].Left := FSourceRect.Right - CS;
  FCtrlRects[3].Top := FSourceRect.Top + FSourceRect.Height / 2 - CS;
  FCtrlRects[3].Right := FCtrlRects[3].Left + CS * 2;
  FCtrlRects[3].Bottom := FCtrlRects[3].Top + CS * 2;

  FSourceRect := RectF(0, 0, FBitmap.Width * S, FBitmap.Height * S);
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Stroke.Color := $AF3300ff;
  Canvas.Stroke.Thickness := 1;
  Canvas.Stroke.Join := TStrokeJoin.Miter;
  Canvas.Stroke.Dash := TStrokeDash.Solid;
  Canvas.Stroke.Cap := TStrokeCap.Flat;
  Canvas.DrawLine(
    PointF(FCtrlRects[0].Left + CS, FSourceRect.Top),
    PointF(FCtrlRects[0].Left + CS, FSourceRect.Bottom), 1);
  Canvas.DrawLine(
    PointF(FCtrlRects[3].Left + CS, FSourceRect.Top),
    PointF(FCtrlRects[3].Left + CS, FSourceRect.Bottom), 1);
  Canvas.DrawLine(
    PointF(FSourceRect.Left, FCtrlRects[1].Top + CS),
    PointF(FSourceRect.Right, FCtrlRects[1].Top + CS), 1);
  Canvas.DrawLine(
    PointF(FSourceRect.Left, FCtrlRects[2].Top + CS),
    PointF(FSourceRect.Right, FCtrlRects[2].Top + CS), 1);
end;

procedure TBoundsDesigner.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  PrepareForCrop;
  Preview.Width := FBitmap.Width * trackScale.Value;
  Preview.Height := FBitmap.Height * trackScale.Value;
end;

procedure TBoundsDesigner.trackScaleChange(Sender: TObject);
begin
  Preview.Width := FBitmap.Width * trackScale.Value;
  Preview.Height := FBitmap.Height * trackScale.Value;

  FOldScale := trackScale.Value;

  if FChanging then Exit;
  FChanging := True;
  edtScale.Text := IntToStr(Round(trackScale.Value * 100));
  FChanging := False;
end;

procedure TBoundsDesigner.trackScaleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ssDouble in Shift then trackScale.Value := 1;
end;

procedure TBoundsDesigner.UpdateValue;
begin
  FBounds.Left := StrToFloatDef(edtLeft.Text, 0);
  FBounds.Top := StrToFloatDef(edtTop.Text, 0);
  FBounds.Right := StrToFloatDef(edtRight.Text, 0);
  FBounds.Bottom := StrToFloatDef(edtBottom.Text, 0);
end;

end.
