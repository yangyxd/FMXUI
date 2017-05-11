{*******************************************************}
{                                                       }
{       无边框窗体大小控制单元                          }
{                                                       }
{       版权所有 (C) 2017 by YangYxd                    }
{                                                       }
{*******************************************************}

{
  使用方法： 将需要控制大小的无边框窗口的基类设置为 TSizeForm 即可
}

unit UI.SizeForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  FMX.StdCtrls;

type
  TResizeMode = (Normal, LTop, RTop, LBottom, RBottom, Top, Bottom, Left, Right);

  TSizeForm = class(TForm)
  private
    { Private declarations }
    FCaptureDragForm: Boolean;
    FMouseDraging: Boolean;
  protected
    FSizeWH: Single;   // 可调节区域大小
    FMousePos, FDownPos, FResizeSize, FDownSize: TPointF;
    FResizeMode: TResizeMode;
    function PointInDragBorder(const X, Y: Single): Boolean;
    function CalcResizeMode(const X, Y: Single): TResizeMode;
    procedure UpdateCurror(const AResizeMode: TResizeMode);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function ObjectAtPoint(AScreenPoint: TPointF): IControl; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoClick: Boolean = True); override;
  published
    property CaptureDragForm: Boolean read FCaptureDragForm write FCaptureDragForm;
  end;

implementation

function TSizeForm.CalcResizeMode(const X, Y: Single): TResizeMode;
begin
  Result := TResizeMode.Normal;
  if (X > FSizeWH) and (X <= Width - FSizeWH) then begin
    if (Y < FSizeWH) then
      Result := TResizeMode.Top
    else if (Y >= Height - FSizeWH) then
      Result := TResizeMode.Bottom
  end else if (Y > FSizeWH) and (Y < Height - FSizeWH) then begin
    if X <= FSizeWH then
      Result := TResizeMode.Left
    else if X >= Width - FSizeWH then
      Result := TResizeMode.Right
  end else if (X <= FSizeWH) and (Y <= FSizeWH) then
    Result := TResizeMode.LTop
  else if (X >= Width - FSizeWH) and (Y <= FSizeWH) then
    Result := TResizeMode.RTop
  else if (X <= FSizeWH) and (Y >= Height - FSizeWH) then
    Result := TResizeMode.LBottom
  else if (X >= Width - FSizeWH) and (Y >= Height - FSizeWH) then
    Result := TResizeMode.RBottom;
end;

constructor TSizeForm.Create(AOwner: TComponent);
begin
  inherited;
  FSizeWH := 10;
end;

procedure TSizeForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  if (BorderStyle = TFmxFormBorderStyle.None) then begin
    if (csDesigning in ComponentState) then Exit;
    if (Button = TMouseButton.mbLeft) and (Shift = [ssLeft]) then begin
      if FullScreen then
        Exit;
      if not PointInDragBorder(X, Y) then begin
        if FCaptureDragForm then begin
          FMouseDraging := True;
          StartWindowDrag;
        end;
        Exit;
      end;
      FResizeMode := CalcResizeMode(X, Y);
      UpdateCurror(FResizeMode);
      if FResizeMode = TResizeMode.Normal then
        Exit;
      FMousePos := PointF(X, Y);
      FDownPos := FMousePos;
      FResizeSize := PointF(Width, Height);
      FDownSize := FResizeSize;
      FWinService.SetCapture(Self);
    end;
  end;
end;

procedure TSizeForm.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
begin
  if (FResizeMode <> TResizeMode.Normal) and (ssLeft in Shift) then begin
    Engage;
    try
      P.X := Left;
      P.Y := Top;
      case FResizeMode of
        TResizeMode.LTop:
          begin
            P.X := P.X + (X - FDownPos.X);
            P.Y := P.Y + (Y - FDownPos.Y);
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X - (X - FDownPos.X)));
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y - (Y - FDownPos.Y)));
          end;
        TResizeMode.RTop:
          begin
            P.Y := P.Y + (Y - FDownPos.Y);
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X));
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y - (Y - FDownPos.Y)));
          end;
        TResizeMode.LBottom:
          begin
            P.X := P.X + (X - FDownPos.X);
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X - (X - FDownPos.X)));
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y));
          end;
        TResizeMode.RBottom:
          begin
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X));
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y));
          end;
        TResizeMode.Top:
          begin
            P.Y := P.Y + (Y - FDownPos.Y);
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y) - (Y - FDownPos.Y));
          end;
        TResizeMode.Bottom:
          begin
            FResizeSize.Y := Round(FResizeSize.Y + (Y - FMousePos.Y));
          end;
        TResizeMode.Left:
          begin
            P.X := P.X + (X - FDownPos.X);
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X - (X - FDownPos.X)));
          end;
        TResizeMode.Right:
          begin
            FResizeSize.X := Round(FResizeSize.X + (X - FMousePos.X));
          end;
      end;
      SetBounds(Round(P.X), Round(P.Y), Round(FResizeSize.X), Round(FResizeSize.Y));
      FMousePos := PointF(X, Y);
    finally
      Disengage;
    end;
  end else begin
    inherited;
    if (BorderStyle = TFmxFormBorderStyle.None) then begin
      if Shift = [] then
        UpdateCurror(CalcResizeMode(X, Y))
      else
        Cursor := crArrow;
    end;
  end;
end;

procedure TSizeForm.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single;
  DoClick: Boolean);
begin
  if FMouseDraging then begin
    FMouseDraging := False;
    ReleaseCapture;
  end;
  if FResizeMode <> TResizeMode.Normal then begin
    FResizeMode := TResizeMode.Normal;
    ReleaseCapture;
  end;
  inherited;
end;

function TSizeForm.ObjectAtPoint(AScreenPoint: TPointF): IControl;

  function Innder(const P: TPointF): IControl;
  begin
    if PointInDragBorder(P.X, P.Y) then
      Result := nil
    else
      Result := inherited;
  end;

begin
  if (BorderStyle = TFmxFormBorderStyle.None) then
    Result := Innder(ScreenToClient(AScreenPoint))
  else
    Result := inherited;
end;

function TSizeForm.PointInDragBorder(const X, Y: Single): Boolean;
begin
  Result := (X < FSizeWH) or (X >= Width - FSizeWH) or (Y < FSizeWH) or (Y >= Height - FSizeWH);
end;

procedure TSizeForm.UpdateCurror(const AResizeMode: TResizeMode);
const
  CCursor: array [TResizeMode] of Integer = (
    crArrow, crSizeNWSE, crSizeNESW, crSizeNESW, crSizeNWSE, crSizeNS, crSizeNS, crSizeWE, crSizeWE
  );
begin
  Cursor := CCursor[AResizeMode]
end;

end.
