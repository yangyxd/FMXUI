unit UI.Design.ImageIndex;

interface

uses
 Winapi.Windows,
 System.SysUtils, System.Classes, FMX.Graphics, Vcl.Graphics,
 System.Types, System.UITypes, FMX.ImgList,
 DesignIntf, System.TypInfo, DesignEditors, VCLEditors;// Vcl.Controls;

type

  TImageIndexProperty = class(TIntegerProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing, ICustomPropertyDrawing80)
  private
    function GetImages: TCustomImageList;
    function DrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean): Boolean;
    procedure CreateView(AImageList: TCustomImageList; AIndex: Integer);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
  end;

implementation

uses
  UI.Base,
  System.Math,
  FMX.Helpers.Win;

const
  ImageWidth = 16;
  ImageHeight = 16;
  OffsetWidth = 8;
  OffsetHeight = 4;
  DefaultValue = '-1';

var
  FMXBitmap: FMX.Graphics.TBitmap = nil;
  VCLBitmap: VCl.Graphics.TBitmap = nil;

{ TImageIndexProperty }

function TImageIndexProperty.DrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean): Boolean;
var
  LImageList: TCustomImageList;
  LTop: Integer;
  LImageIndex: Integer;
begin
  LImageList := GetImages;
  LImageIndex := StrToIntDef(Value, -1);

  ACanvas.FillRect(ARect);
  CreateView(LImageList, LImageIndex);
  ACanvas.Draw(ARect.Left + 2, ARect.Top, VCLBitmap);
  LTop := ARect.Top + (ARect.Bottom - ARect.Top - ACanvas.TextHeight(Value)) div 2;
  ACanvas.TextOut(ARect.Left + ImageWidth + OffsetWidth, LTop, Value);

  Result := True;
end;

function TImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited + [paValueList];
end;

function TImageIndexProperty.GetImages: TCustomImageList;
var
  LEditedComponent: TPersistent;
begin
  Result := nil;
  try
    LEditedComponent := GetComponent(0) as TPersistent;
    if (LEditedComponent <> nil) and (LEditedComponent is TViewImagesBrush) then
      Result := TCustomImageList(TViewImagesBrush(LEditedComponent).Images);
  except
  end;
end;

procedure TImageIndexProperty.GetValues(Proc: TGetStrProc);
var
  LImageList: TCustomImageList;
  I: Integer;
begin
  Proc(DefaultValue);
  LImageList := GetImages;
  if LImageList = nil then
    Exit;
  for I := 0 to LImageList.Count - 1 do
    Proc(IntToStr(I));
end;

procedure TImageIndexProperty.SetValue(const Value: string);
begin
  if Value = '' then
    inherited SetValue(DefaultValue)
  else
    inherited SetValue(Value);
end;

procedure TImageIndexProperty.CreateView(AImageList: TCustomImageList; AIndex: Integer);
var
  LR: TRect;
  LHBitmap: Winapi.Windows.HBITMAP;
begin
  if FMXBitmap = nil then
    FMXBitmap := FMX.Graphics.TBitmap.Create(ImageWidth, ImageHeight);
  // 重置
  FMXBitmap.Clear(TAlphaColors.White);
  if VCLBitmap = nil then
  begin
    VCLBitmap := VCl.Graphics.TBitmap.Create;
    VCLBitmap.SetSize(ImageWidth, ImageHeight);
    VCLBitmap.PixelFormat := TPixelFormat.pf32bit;
  end;
  // 填充下颜色,覆盖掉原来的
  with VCLBitmap do
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clWhite;
    LR := Rect(0, 0, ImageWidth, ImageHeight);
    Canvas.FillRect(LR);
  end;
  // 画图标
  if Assigned(AImageList) and (AImageList.Count > 0) and (AIndex >=0) and (AIndex < AImageList.Count) then
  begin
    FMXBitmap.Canvas.BeginScene();
    try
      AImageList.Draw(FMXBitmap.Canvas, RectF(0, 0, ImageWidth, ImageHeight), AIndex);
    finally
      FMXBitmap.Canvas.EndScene;
    end;
    LHBitmap := BitmapToWinBitmap(FMXBitmap, True);
    try
      VCLBitmap.Handle := LHBitmap;
    except
      if LHBitmap <> 0 then
      begin
        DeleteObject(LHBitmap);
        LHBitmap := 0;
      end;
    end;
  end else
  begin
    with VCLBitmap do
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := $6D6D6D;
      Canvas.Pen.Style := psDot;
      LR := Rect(LR.Left + 1, LR.Top + 1, LR.Right - 1, LR.Bottom - 1);
      Canvas.Rectangle(LR);
      // 画个叉
      Canvas.Pen.Style := psSolid;
      Canvas.MoveTo(LR.Left + 2, LR.Top + 2);
      Canvas.LineTo(LR.Left + 9, LR.Top + 9);
      Canvas.MoveTo(LR.Left + 9, LR.Top + 2);
      Canvas.LineTo(LR.Left + 2, LR.Top + 9);
    end;
  end;
end;

procedure TImageIndexProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  DrawValue(Value, ACanvas, ARect, ASelected);
end;

procedure TImageIndexProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  LImageList: TCustomImageList;
begin
  LImageList := GetImages;
  if Assigned(LImageList) and (LImageList.Count > 0) then
    AWidth := AWidth + ImageWidth + OffsetWidth;
end;

procedure TImageIndexProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TImageIndexProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

procedure TImageIndexProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DrawValue(GetValue, ACanvas, ARect, ASelected);
end;

function TImageIndexProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

procedure TImageIndexProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  LImageList: TCustomImageList;
begin
  LImageList := GetImages;
  if Assigned(LImageList) and (LImageList.Count > 0) then
  begin
    if Value = DefaultValue then
      AHeight := ImageHeight + OffsetHeight
    else
      AHeight := Max(AHeight, ImageHeight + OffsetHeight);
  end else
    AHeight := ImageHeight + OffsetHeight
end;

initialization

finalization
  if Assigned(FMXBitmap) then
    FreeAndNil(FMXBitmap);
  if Assigned(VCLBitmap) then
    FreeAndNil(VCLBitmap);


end.
