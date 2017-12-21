{*******************************************************}
{                                                       }
{       FMXUI SVG图标支持单元                           }
{                                                       }
{       版权所有 (C) 2016 YangYxd                       }
{                                                       }
{*******************************************************}

unit UI.Utils.SVGImage;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Generics.Collections, System.Math.Vectors,
  FMX.Graphics, FMX.Surfaces, FMX.Types, FMX.Consts, System.UITypes,
  Types, Classes, Sysutils, Math, Xml.VerySimple;

const
  SSVGImageExtension = '.svg';
  SVSVGImages = 'SVG Images';

const
  DefaultSvgSize = 64;

type
  TXMLDocument = class(TXmlVerySimple);

type
  TSVGDecode = class(TObject)
  private
    FData: TXMLDocument;
    FViewBox: TPointF;
    [Weak] FSvg: TXmlNode;
    function IsEmpty: Boolean;
    function GetSize: TSize;
  protected
    procedure DecodeSVG(); virtual;
    function ReadFloat(Node: TXmlNode; const Name: string; const DefaultValue: Double = 0): Double;
    function ReadColor(Node: TXmlNode; const Name: string; const DefaultValue: TAlphaColor = 0): TAlphaColor;
    function ReadString(Node: TXmlNode; const Name: string): string;
  public
    destructor Destroy; override;
    procedure LoadFormFile(const AFileName: string);
    procedure LoadFormStream(const AStream: TStream); virtual;
    property SvgStyle: TXmlNode read FSvg;
    property Data: TXMLDocument read FData;
    property Empty: Boolean read IsEmpty;
    property Size: TSize read GetSize;
    property ViewBox: TPointF read FViewBox write FViewBox;
  end;

type
  TSVGImage = class(TPersistent)
  private
    FData: TSVGDecode;
    FBitmap: TBitmap;
    function GetEmpty: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    procedure CreateDecode();
    procedure DrawSVG();
  public
    destructor Destroy; override;
    procedure LoadFormFile(const AFileName: string);
    procedure LoadFormStream(const AStream: TStream);
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Draw(Canvas: TCanvas; const X, Y: Single; const AOpacity: Single = 1; const HighSpeed: Boolean = False);
    procedure SetSize(const Width, Height: Integer);
    property Empty: Boolean read GetEmpty;
    property Data: TSVGDecode read FData;
    property Bitmap: TBitmap read FBitmap;
  published
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

implementation

uses
  UI.Utils;

{ TSVGDecode }

procedure TSVGDecode.DecodeSVG;
var
  S: string;
  List: TStrings;
  LSize: TPointF;
begin
  if not Assigned(FData) then
    raise Exception.Create('SVG not Initialization.');
  FSvg := FData.ChildNodes.FindNode('svg');
  if Assigned(FSvg) then begin
    LSize := PointF(ReadFloat(FSvg, 'width', 0), ReadFloat(FSvg, 'height', 0));
    S := ReadString(FSvg, 'viewBox');
    if S = '' then
      FViewBox := LSize
    else begin
      List := TStringList.Create;
      try
        List.DelimitedText := S;
        if List.Count = 2 then 
          FViewBox := PointF(StrToFloatDef(List[0], LSize.X), StrToFloatDef(List[1], LSize.Y))
        else if List.Count = 4 then              
          FViewBox := PointF(StrToFloatDef(List[2], LSize.X), StrToFloatDef(List[3], LSize.Y))
        else
          FViewBox := LSize;
      finally
        FreeAndNil(List);
      end;
    end;
  end else begin
    LSize := PointF(DefaultSvgSize, DefaultSvgSize);
    FViewBox := LSize;
  end;
end;

destructor TSVGDecode.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function TSVGDecode.GetSize: TSize;
begin
  if Assigned(FSvg) then begin
    Result.Width := Round(ReadFloat(FSvg, 'width', 0));
    Result.Height := Round(ReadFloat(FSvg, 'height', 0));
  end else begin
    Result.Width := DefaultSvgSize;
    Result.Height := DefaultSvgSize;
  end;
end;

function TSVGDecode.IsEmpty: Boolean;
begin
  Result := (FSvg = nil) or (FData = nil);
end;

procedure TSVGDecode.LoadFormFile(const AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFormStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TSVGDecode.LoadFormStream(const AStream: TStream);
begin
  if not Assigned(AStream) then
    Exit;
  if not Assigned(FData) then
    FData := TXMLDocument.Create();
  FData.LoadFromStream(AStream);
  try
    DecodeSVG();
  except
  end;
end;

function TSVGDecode.ReadColor(Node: TXmlNode; const Name: string;
  const DefaultValue: TAlphaColor): TAlphaColor;
begin
  try
    Result := HtmlColorToColor(ReadString(Node, Name), DefaultValue);
  except
    Result := DefaultValue;
  end;
end;

function TSVGDecode.ReadFloat(Node: TXmlNode; const Name: string; const DefaultValue: Double): Double;
begin
  Result := StrToFloatDef(ReadString(Node, Name), DefaultValue);
end;

function TSVGDecode.ReadString(Node: TXmlNode; const Name: string): string;
begin
  Result := '';
  if not Assigned(Node) then Exit;
  if Node.HasAttribute(Name) then
    Result := Node.Attributes[Name];
end;

{ TSVGImage }

procedure TSVGImage.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Source is TSVGImage then begin
    if TSVGImage(Source).Empty then
      Clear
    else begin
      CreateDecode();
      Stream := TMemoryStream.Create;
      try
        TSVGImage(Source).FData.FData.SaveToStream(Stream);
        Stream.Position := 0;
        Self.FData.LoadFormStream(Stream);
      finally
        Stream.Free;
      end;
    end;
  end else
    inherited;
end;

procedure TSVGImage.Clear;
begin
  FreeAndNil(FData);
  FreeAndNil(FBitmap);
end;

procedure TSVGImage.CreateDecode;
begin
  if not Assigned(FData) then
    FData := TSVGDecode.Create;
end;

destructor TSVGImage.Destroy;
begin
  FreeAndNil(FData);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TSVGImage.Draw(Canvas: TCanvas; const X, Y: Single; const AOpacity: Single; const HighSpeed: Boolean);
var
  W, H: Single;
begin
  if Assigned(FBitmap) then begin
    W := FBitmap.Width;
    H := FBitmap.Height;
    Canvas.DrawBitmap(FBitmap, RectF(0, 0, W, H), RectF(X, Y, X + W, Y + H), AOpacity, HighSpeed);
  end;
end;

procedure TSVGImage.DrawSVG;
var
  FillOpacity, StrokeOpacity: Single;
  SX, SY: Single;

  function StrLCompX(const Str1, Str2: PWideChar; MaxLen, Len: Cardinal): Integer;
  begin
    if Len <> MaxLen then
      Result := -1
    else
      Result := StrLComp(Str1, Str2, MaxLen);
  end;

  procedure ParserStyleItem(Canvas: TCanvas; const PS: PChar; const PN: Integer;
    const PV: PChar; const PL: Integer);
  var
    Value: string;
  begin
    if (PL < 1) or (PN < 1) then Exit;
    if StrLCompX(PS, 'fill', 4, PN) = 0 then begin
      SetString(Value, PV, PL);
      Canvas.Fill.Color := HtmlColorToColor(TrimRight(Value), Canvas.Fill.Color);
    end else if StrLCompX(PS, 'stroke', 6, PN) = 0 then begin
      SetString(Value, PV, PL);
      Canvas.Stroke.Color := HtmlColorToColor(TrimRight(Value), Canvas.Stroke.Color);
    end else if StrLCompX(PS, 'stroke-width', 12, PN) = 0 then begin
      SetString(Value, PV, PL);
      Canvas.Stroke.Thickness := StrToFloatDef(TrimRight(Value), 0) * SX;
    end else if StrLCompX(PS, 'fill-opacity', 12, PN) = 0 then begin
      SetString(Value, PV, PL);
      FillOpacity := StrToFloatDef(TrimRight(Value), 1);
    end else if StrLCompX(PS, 'stroke-opacity', 14, PN) = 0 then begin
      SetString(Value, PV, PL);
      StrokeOpacity := StrToFloatDef(TrimRight(Value), 1);
    end else if StrLCompX(PS, 'opacity', 7, PN) = 0 then begin
      SetString(Value, PV, PL);
      StrokeOpacity := StrToFloatDef(TrimRight(Value), 1);
      FillOpacity := StrokeOpacity;
    end;
  end;

  procedure ParserStyle(Canvas: TCanvas; const Data: string); overload;
  var
    P, PE, PS, PV: PChar;
    PN: Integer;
  begin
    if Data = '' then Exit;
    P := PChar(Data);
    PE := P + Length(Data);
    while (P < PE) and ((P^ = ' ') or (P^ = #13) or (P^ = #10)) do Inc(P);
    PS := P;
    PV := nil;
    PN := 0;
    while P < PE do begin
      if (PV = nil) and (P^ = ':') then begin
        PN := P - PS;
        Inc(P);
        while (P < PE) and ((P^ = ' ') or (P^ = #13) or (P^ = #10)) do Inc(P);
        PV := P;
        Continue;
      end else if (PV <> nil) and (P^ = ';') then begin
        if (PN > 0) and (P > PV) then
          ParserStyleItem(Canvas, PS, PN, PV, P - PV);
        PN := 0;
        PV := nil;
        Inc(P);
        while (P < PE) and ((P^ = ' ') or (P^ = #13) or (P^ = #10)) do Inc(P);
        PS := P;
      end else
        Inc(P);
    end;
    if (PV <> nil) and (P > PV) and (PN > 0) then
      ParserStyleItem(Canvas, PS, PN, PV, P - PV);
  end;

  procedure ParserStyle(Canvas: TCanvas; Item: TXmlNode); overload;
  begin
    Canvas.Fill.Color := FData.ReadColor(Item, 'fill', Canvas.Fill.Color);
    if Item.HasAttribute('stroke') then begin
      Canvas.Stroke.Color := FData.ReadColor(Item, 'stroke', Canvas.Stroke.Color);
      Canvas.Stroke.Thickness := FData.ReadFloat(Item, 'stroke-width') * SX;
    end;
    ParserStyle(Canvas, FData.ReadString(Item, 'style'));
  end;

  procedure ParserPoints(var LPoints: TPolygon; const Data: string; Offset: Single = 0);
  var
    List: TStrings;
    I, J, P: Integer;
    V, L: string;
  begin
    List := TStringList.Create;
    J := 0;
    try
      List.Delimiter := ' ';
      List.DelimitedText := Data;
      SetLength(LPoints, List.Count);
      for I := 0 to List.Count - 1 do begin
        V := Trim(List[I]);
        if V = '' then Continue;
        P := Pos(',', V);
        if P = 0 then Continue;
        L := Copy(V, 0, P - 1);
        V := Copy(V, P + 1, Length(V) - P);
        LPoints[J] := PointF(StrToFloatDef(L, 0) + Offset, StrToFloatDef(V, 0) + Offset);
        Inc(J);
      end;
    finally
      List.Free;
      SetLength(LPoints, J);
    end;
  end;

  procedure ParserPointsSize(const Data: string; var W, H: Single; Offset: Single = 0);
  var
    List: TStrings;
    I, P: Integer;
    V, L: string;
  begin
    List := TStringList.Create;
    try
      List.Delimiter := ' ';
      List.DelimitedText := Data;
      W := 0;
      H := 0;
      for I := 0 to List.Count - 1 do begin
        V := Trim(List[I]);
        if V = '' then Continue;
        P := Pos(',', V);
        if P = 0 then Continue;
        L := Copy(V, 0, P - 1);
        V := Copy(V, P + 1, Length(V) - P);
        W := Max(W, StrToFloatDef(L, 0) + Offset);
        H := Max(H, StrToFloatDef(V, 0) + Offset);
      end;
      W := W + Offset;
      H := H + Offset;
    finally
      List.Free;
    end;
  end;

  procedure ParserPathSize(Path: TPathData; var W, H: Single);
  var
    I: Integer;
  begin
    W := 0;
    H := 0;
    for I := 0 to Path.Count - 1 do begin
      with Path.Points[I] do begin
        if Kind = TPathPointKind.Close then
          Continue;
        W := Max(Point.X, W);
        H := Max(Point.Y, H);
      end;
    end;
  end;

var
  I, J: Integer;
  Path: TPathData;
  W, H, X, Y, RX, RY, LD, MW, MH: Single;
  LPoints: TPolygon;
  Item: TXmlNode;
  LName, LData: string;
begin
  if (FData = nil) or (Fdata.FData = nil) or (FData.FSvg = nil) then
    Exit;
  if FData.FViewBox.X = 0 then
    SX := 1
  else
    SX := Width / FData.FViewBox.X;
  if FData.FViewBox.Y = 0 then
    SY := 1
  else
    SY := Height / FData.FViewBox.Y;
  Path := TPathData.Create;
  try
    // 自动大小
    if (FBitmap.Width = 0) and (FBitmap.Height = 0) then begin
      MW := 0;
      MH := 0;
      for I := 0 to FData.FSvg.ChildNodes.Count - 1 do begin
        Item := FData.FSvg.ChildNodes.Items[I];
        if Item = nil then Continue;
        LName := LowerCase(Item.Name);
        if LName = 'path' then begin
          ParserStyle(FBitmap.Canvas, Item);
          Path.Data := FData.ReadString(Item, 'd');
          Path.Scale(SX, SY);
          ParserPathSize(Path, W, H);
          MW := Max(MW, W + FBitmap.Canvas.Stroke.Thickness * 0.5);
          MH := Max(MH, H + FBitmap.Canvas.Stroke.Thickness * 0.5);
        end else if LName = 'rect' then begin
          W := FData.ReadFloat(Item, 'width') * SX;
          H := FData.ReadFloat(Item, 'height') * SY;
          X := FData.ReadFloat(Item, 'x') * SX;
          Y := FData.ReadFloat(Item, 'y') * SY;
          MW := Max(MW, X + W);
          MH := Max(MH, Y + H);
        end else if LName = 'circle' then begin
          RX := FData.ReadFloat(Item, 'cx') * SX;
          RY := FData.ReadFloat(Item, 'cy') * SY;
          W := FData.ReadFloat(Item, 'r') * SX;
          MW := Max(MW, RX + W);
          MH := Max(MH, RY + W);
        end else if LName = 'ellipse' then begin
          RX := FData.ReadFloat(Item, 'cx') * SX;
          RY := FData.ReadFloat(Item, 'cy') * SY;
          W := FData.ReadFloat(Item, 'rx') * SX;
          H := FData.ReadFloat(Item, 'ry') * SX;
          MW := Max(MW, RX + W);
          MH := Max(MH, RY + H);
        end else if LName = 'line' then begin
          X := FData.ReadFloat(Item, 'x1') * SX;
          Y := FData.ReadFloat(Item, 'y1') * SY;
          RX := FData.ReadFloat(Item, 'x2') * SX;
          RY := FData.ReadFloat(Item, 'y2') * SY;
          MW := Max(MW, Max(X, RX));
          MH := Max(MH, Max(Y, RY));
        end else if LName = 'polygon' then begin
          LData := FData.ReadString(Item, 'points');
          ParserPointsSize(LData, W, H);
          MW := Max(MW, W);
          MH := Max(MH, H);
        end else if LName = 'polyline' then begin
          ParserStyle(FBitmap.Canvas, Item);
          LData := FData.ReadString(Item, 'points');
          ParserPointsSize(LData, W, H, FBitmap.Canvas.Stroke.Thickness * 0.5);
          MW := Max(MW, W);
          MH := Max(MH, H);
        end;
      end;
      FData.ViewBox := PointF(MW, MH);
      FBitmap.SetSize(Round(MW), Round(MH));
    end;

    if (FBitmap.Width = 0) or (FBitmap.Height = 0) then
      Exit;

    FBitmap.Canvas.BeginScene();
    FBitmap.Clear(0);    
    try
      for I := 0 to FData.FSvg.ChildNodes.Count - 1 do begin
        Item := FData.FSvg.ChildNodes.Items[I];
        if Item = nil then Continue;
        LName := LowerCase(Item.Name);

        FillOpacity := 1;
        StrokeOpacity := 1;
        FBitmap.Canvas.Stroke.Thickness := 0;
        FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Black;
        FBitmap.Canvas.Fill.Color := TAlphaColorRec.Black;

        if LName = 'path' then begin
          // 路径
          LData := Item.Attributes['d'];
          if LData <> '' then begin
            Path.Data := LData;
            Path.Scale(SX, SY);
            ParserStyle(FBitmap.Canvas, Item);
            FBitmap.Canvas.FillPath(Path, FillOpacity);
            if FBitmap.Canvas.Stroke.Thickness > 0 then
              FBitmap.Canvas.DrawPath(Path, StrokeOpacity);
          end;
        end else if LName = 'rect' then begin
          // 矩形
          ParserStyle(FBitmap.Canvas, Item);
          W := FData.ReadFloat(Item, 'width') * SX;
          H := FData.ReadFloat(Item, 'height') * SY;
          if (W <= 0) or (H <= 0) then
            Continue;
          X := FData.ReadFloat(Item, 'x') * SX;
          Y := FData.ReadFloat(Item, 'y') * SY;
          RX := FData.ReadFloat(Item, 'rx') * SX;
          RY := FData.ReadFloat(Item, 'ry') * SY;
          FBitmap.Canvas.FillRect(RectF(X, Y, X + W, Y + H), RX, RY, AllCorners, FillOpacity);
          LD := FBitmap.Canvas.Stroke.Thickness;
          if LD > 0 then
            FBitmap.Canvas.DrawRect(RectF(X + LD * 0.5, Y + LD * 0.5, X + W - LD * 0.5, Y + H - LD * 0.5), RX, RY, AllCorners, StrokeOpacity);
        end else if LName = 'circle' then begin
          // 圆
          ParserStyle(FBitmap.Canvas, Item);
          RX := FData.ReadFloat(Item, 'cx') * SX;
          RY := FData.ReadFloat(Item, 'cy') * SY;
          W := FData.ReadFloat(Item, 'r') * SX;
          if W > 0 then begin
            FBitmap.Canvas.FillArc(PointF(RX, RY), PointF(W, W), 0, 360, FillOpacity);
            LD := FBitmap.Canvas.Stroke.Thickness;
            if LD > 0 then
              FBitmap.Canvas.DrawArc(PointF(RX, RY), PointF(W - LD * 0.5, W - LD* 0.5), 0, 360, StrokeOpacity);
          end;
        end else if LName = 'ellipse' then begin
          // 椭圆
          ParserStyle(FBitmap.Canvas, Item);
          RX := FData.ReadFloat(Item, 'cx') * SX;
          RY := FData.ReadFloat(Item, 'cy') * SY;
          W := FData.ReadFloat(Item, 'rx') * SX;
          H := FData.ReadFloat(Item, 'ry') * SX;
          if (W > 0) or (H > 0) then begin
            FBitmap.Canvas.FillArc(PointF(RX, RY), PointF(W, H), 0, 360, FillOpacity);
            LD := FBitmap.Canvas.Stroke.Thickness;
            if LD > 0 then
              FBitmap.Canvas.DrawArc(PointF(RX, RY), PointF(W - LD * 0.5, H - LD* 0.5), 0, 360, StrokeOpacity);
          end;
        end else if LName = 'line' then begin
          // 线条
          ParserStyle(FBitmap.Canvas, Item);
          X := FData.ReadFloat(Item, 'x1') * SX;
          Y := FData.ReadFloat(Item, 'y1') * SY;
          RX := FData.ReadFloat(Item, 'x2') * SX;
          RY := FData.ReadFloat(Item, 'y2') * SY;
          Path.Clear;
          Path.MoveTo(PointF(X, Y));
          Path.LineTo(PointF(RX, RY));
          if FBitmap.Canvas.Stroke.Thickness > 0 then
            FBitmap.Canvas.DrawPath(Path, StrokeOpacity);
        end else if LName = 'polygon' then begin
          // 多边形
          ParserStyle(FBitmap.Canvas, Item);
          LData := FData.ReadString(Item, 'points');
          if LData <> '' then begin
            ParserPoints(LPoints, LData);
            if Length(LPoints) = 0 then
              Continue;
            FBitmap.Canvas.FillPolygon(LPoints, FillOpacity);
            if FBitmap.Canvas.Stroke.Thickness > 0 then
             FBitmap.Canvas.DrawPolygon(LPoints, StrokeOpacity);
          end;
        end else if LName = 'polyline' then begin
          // 折线
          ParserStyle(FBitmap.Canvas, Item);
          LData := FData.ReadString(Item, 'points');
          if LData <> '' then begin
            ParserPoints(LPoints, LData, FBitmap.Canvas.Stroke.Thickness * 0.5);
            if (Length(LPoints) = 0) or (FBitmap.Canvas.Stroke.Thickness <= 0) then
              Continue;
            Path.Clear;
            Path.MoveTo(LPoints[0]);
            for J := 1 to High(LPoints) do 
              Path.LineTo(LPoints[J]);            
            FBitmap.Canvas.DrawPath(Path, StrokeOpacity);
          end;
        end;


      end;
    finally
      FBitmap.Canvas.EndScene;
    end;
  finally
    FreeAndNil(Path);
  end;
end;

function TSVGImage.GetEmpty: Boolean;
begin
  Result := (not Assigned(FData)) or (FData.Empty);
end;

function TSVGImage.GetHeight: Integer;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Height
  else
    Result := 0;
end;

function TSVGImage.GetWidth: Integer;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Width
  else
    Result := 0;
end;

procedure TSVGImage.LoadFormFile(const AFileName: string);
begin
  CreateDecode();
  FData.LoadFormFile(AFileName);
  if Empty then begin
    FreeAndNil(FBitmap);
    Exit;
  end;
  if not Assigned(FBitmap) then
    FBitmap := TBitmap.Create;
  FBitmap.SetSize(FData.Size);
  DrawSVG;
end;

procedure TSVGImage.LoadFormStream(const AStream: TStream);
begin
  CreateDecode();
  FData.LoadFormStream(AStream);
  if Empty then begin
    FreeAndNil(FBitmap);
    Exit;
  end;
  if not Assigned(FBitmap) then
    FBitmap := TBitmap.Create;
  FBitmap.SetSize(FData.Size);
  DrawSVG;
end;

procedure TSVGImage.SetHeight(const Value: Integer);
begin
  if Value <> Height then
    SetSize(Width, Value);
end;

procedure TSVGImage.SetSize(const Width, Height: Integer);
begin
  if Empty then
    Exit;
  if (Width <> FBitmap.Width) or (Height <> FBitmap.Height) then begin
    FBitmap.SetSize(Width, Height);
    if (Width > 0) and (Height > 0) then
      DrawSVG();
  end;
end;

procedure TSVGImage.SetWidth(const Value: Integer);
begin
  if Value <> Width then
    SetSize(Value, Height);
end;

initialization

end.
