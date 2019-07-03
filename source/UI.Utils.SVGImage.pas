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
  FMX.TextLayout,
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
    function ReadString(Node: TXmlNode; const Name: string): string;
    function ReadFloat(Node: TXmlNode; const Name: string; const DefaultValue: Double = 0): Double; overload;
    function ReadColor(Node: TXmlNode; const Name: string; const DefaultValue: TAlphaColor = 0): TAlphaColor; overload;
    function ReadFontAnchor(Node: TXmlNode; DefaultValue: Integer = 0): Integer; overload;
    function ReadFloat(const Value: string; const DefaultValue: Double = 0): Double; overload;
    function ReadColor(const Value: string; const DefaultValue: TAlphaColor = 0): TAlphaColor; overload;
    function ReadFontAnchor(const Value: string; DefaultValue: Integer = 0): Integer; overload;
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
    FOnChange: TNotifyEvent;
    FData: TSVGDecode;
    FBitmap: TBitmap;
    FLayout: TTextLayout;
    FColor: TAlphaColor;
    FLoss: Boolean;
    function GetEmpty: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetLoss(const Value: Boolean);
  protected
    { rtl }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Reader: TReader); virtual;
    procedure WriteData(Writer: TWriter); virtual;
  protected
    procedure CreateDecode();
    procedure InitBitmap();
    procedure DrawSVG();

    procedure DoChange();

    procedure FillText(const Canvas: TCanvas; const ARect: TRectF; const AText: string;
      const AColor: TAlphaColor; const AOpacity: Single;
      const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center); overload;

    procedure TextSize(const AText: string; var ASize: TSizeF; const SceneScale: Single;
      const MaxWidth: Single = -1; AWordWrap: Boolean = False);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFormFile(const AFileName: string);
    procedure LoadFormStream(const AStream: TStream);
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure ReSize();
    procedure Draw(Canvas: TCanvas; const X, Y: Single; const AOpacity: Single = 1; const HighSpeed: Boolean = False);
    procedure SetSize(const Width, Height: Integer);
    property Empty: Boolean read GetEmpty;
    property Data: TSVGDecode read FData;
    property Bitmap: TBitmap read FBitmap;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Width: Integer read GetWidth write SetWidth default 0;
    property Height: Integer read GetHeight write SetHeight default 0;
    property Color: TAlphaColor read FColor write SetColor default 0;
    property Loss: Boolean read FLoss write SetLoss default True;  // 矢量化
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
    FData := TXMLDocument.Create()
  else
    FData.Clear;
  FSvg := nil;
  FViewBox := TPoint.Zero;
  try
    FData.LoadFromStream(AStream);
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

function TSVGDecode.ReadFontAnchor(Node: TXmlNode; DefaultValue: Integer): Integer;
begin
  Result := ReadFontAnchor(ReadString(Node, 'text-anchor'));
end;

function TSVGDecode.ReadString(Node: TXmlNode; const Name: string): string;
begin
  Result := '';
  if Assigned(Node) then
    Result := Node.Attributes[Name];
end;

function TSVGDecode.ReadColor(const Value: string;
  const DefaultValue: TAlphaColor): TAlphaColor;
begin
  try
    Result := HtmlColorToColor(Value, DefaultValue);
  except
    Result := DefaultValue;
  end;
end;

function TSVGDecode.ReadFloat(const Value: string;
  const DefaultValue: Double): Double;
begin
  Result := StrToFloatDef(Value, DefaultValue);
end;

function TSVGDecode.ReadFontAnchor(const Value: string;
  DefaultValue: Integer): Integer;
var
  S: string;
begin
  Result := DefaultValue;
  S := LowerCase(Value);
  if S = 'start' then
    Result := 0
  else if S = 'middle' then
    Result := 1
  else if S = 'end' then
    Result := 2
  else if S = 'inherit' then
    Result := 3
end;

{ TSVGImage }

procedure TSVGImage.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Source is TSVGImage then begin
    FColor := TSVGImage(Source).FColor;
    if TSVGImage(Source).Empty then
      Clear
    else begin
      CreateDecode();
      Stream := TMemoryStream.Create;
      try
        TSVGImage(Source).FData.FData.SaveToStream(Stream);
        Stream.Position := 0;
        Self.FData.LoadFormStream(Stream);
        if not Assigned(FBitmap) then
          InitBitmap();
        FBitmap.SetSize(FData.Size.Width, FData.Size.Height);
        DrawSVG;
      finally
        Stream.Free;
      end;
    end;
    DoChange();
  end else
    inherited;
end;

procedure TSVGImage.Clear;
begin
  FreeAndNil(FData);
  FreeAndNil(FBitmap);
  DoChange();
end;

constructor TSVGImage.Create;
begin
  FLoss := True;
end;

procedure TSVGImage.CreateDecode;
begin
  if not Assigned(FData) then
    FData := TSVGDecode.Create;
end;

procedure TSVGImage.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('SVGData', ReadData, WriteData, Assigned(FData));
end;

destructor TSVGImage.Destroy;
begin
  FreeAndNil(FLayout);
  FreeAndNil(FData);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TSVGImage.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
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
  FillOpacity, StrokeOpacity: Single;  // 填充和边线透明度
  SX, SY: Single; // X, Y 缩放
  FontAnchor: Integer;

  procedure ParserStyleItem(Canvas: TCanvas; const PS: PChar; const PN: Integer;
    const PV: PChar; PL: Integer; const Flag: Integer);
  var
    P: PChar;
  begin
    //if (PL < 1) or (PN < 1) then Exit;
    // 跳过尾部空格
    P := PV + PL - 1;
    while (P >= PV) and ((P^ = ' ') or (P^ = #13) or (P^ = #10)) do begin
      Dec(PL);
      Dec(P);
    end;

    if PL < 1 then Exit;
    if (PN = 12) and (StrLComp(PS, 'stroke-width', PN) = 0) then begin
      P := PV + PL;
      while P > PV do begin
        Dec(P);
        if (P^ >= '0') and (P^ <= '9') then begin
          Inc(P);
          Break;
        end;
      end;
      Canvas.Stroke.Thickness := PCharToFloatDef(PV, P - PV, 0) * SX;
    end;
    if Flag <> 0 then Exit;
    if (PN = 4) and (FColor = 0) and (StrLComp(PS, 'fill', PN) = 0) then begin
      Canvas.Fill.Color := HtmlColorToColor(PCharToStr(PV, PL), TAlphaColorRec.Black);
    end else if (PN = 6) and (FColor = 0) and (StrLComp(PS, 'stroke', PN) = 0) then begin
      Canvas.Stroke.Color := HtmlColorToColor(PCharToStr(PV, PL), TAlphaColorRec.Black);
    end else if (PN = 12) and (StrLComp(PS, 'fill-opacity', PN) = 0) then begin
      FillOpacity := StrToFloatDef(PCharToStr(PV, PL), 1);
    end else if (PN = 14) and (StrLComp(PS, 'stroke-opacity', PN) = 0) then begin
      StrokeOpacity := PCharToFloatDef(PV, PL, 1);
    end else if (PN = 7) and (StrLComp(PS, 'opacity', PN) = 0) then begin
      StrokeOpacity := PCharToFloatDef(PV, PL, 1);
      FillOpacity := StrokeOpacity;
    end else if (PN = 9) and (StrLComp(PS, 'font-size', PN) = 0) then begin
      FLayout.Font.Size := PCharToFloatDef(PV, PL, FLayout.Font.Size);
    end else if (PN = 11) and (StrLComp(PS, 'font-family', PN) = 0) then begin
      FLayout.Font.Family := PCharToStr(PV, PL);
    end;
  end;

  procedure ParserStyle(Canvas: TCanvas; const Data: string; Flag: Integer); overload;
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
      end else if (P^ = ';') and (PV <> nil) then begin
        if (PN > 0) and (P > PV) then
          ParserStyleItem(Canvas, PS, PN, PV, P - PV, Flag);
        PN := 0;
        PV := nil;
        Inc(P);
        while (P < PE) and ((P^ = ' ') or (P^ = #13) or (P^ = #10)) do Inc(P);
        PS := P;
      end else
        Inc(P);
    end;
    if (PV <> nil) and (P > PV) and (PN > 0) then
      ParserStyleItem(Canvas, PS, PN, PV, P - PV, Flag);
  end;

  function TryGetAttr(Item: TXmlNode; var Value: string; const Key: string): Boolean;
  var
    Attr: TXmlAttribute;
  begin
    Attr := Item.AttributeList.Find(Key);
    if Assigned(Attr) then begin
      Value := Attr.Value;
      Result := True;
    end else
      Result := False;
  end;

  procedure ParserStyle(Canvas: TCanvas; Item: TXmlNode; Flag: Integer = 0); overload;
  var
    Value: string;
  begin
    if (Flag = 0) and (FColor = 0) then
      Canvas.Fill.Color := FData.ReadColor(Item, 'fill', Canvas.Fill.Color);
    if (Flag = 0) and (FColor = 0) and TryGetAttr(Item, Value, 'stroke') then
      Canvas.Stroke.Color := FData.ReadColor(Value, Canvas.Stroke.Color);
    if TryGetAttr(Item, Value, 'stroke-width') then
      Canvas.Stroke.Thickness := FData.ReadFloat(Value, Canvas.Stroke.Thickness) * SX;
    if TryGetAttr(Item, Value, 'text-anchor') then
      FontAnchor := FData.ReadFontAnchor(Value, FontAnchor);
    if TryGetAttr(Item, Value, 'font-size') then
      FLayout.Font.Size := FData.ReadFloat(Value, FLayout.Font.Size);
    if TryGetAttr(Item, Value, 'font-family') then
      FLayout.Font.Family := Value;
    if TryGetAttr(Item, Value, 'style') then
      ParserStyle(Canvas, Value, Flag);
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
      if (Pos(',', Data) = 0) and (List.Count mod 2 = 0) then begin
        SetLength(LPoints, List.Count div 2);
        I := 0;
        while I < List.Count do begin
          LPoints[J] := PointF(StrToFloatDef(List[I], 0) + Offset, StrToFloatDef(List[I + 1], 0) + Offset);
          Inc(I, 2);
          Inc(J);
        end;
      end else begin
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
        if Kind <> TPathPointKind.Close then begin
          W := Max(Point.X, W);
          H := Max(Point.Y, H);
        end;
      end;
    end;
  end;

  procedure ParserNodesCaleSize(Canvas: TCanvas; Nodes: TXmlNodeList; Path: TPathData; var MW, MH: Single);
  var
    Item: TXmlNode;
    I, DefFontAnchor: Integer;
    W, H, X, Y, RX, RY, DefFontSize, DefStrokeSize: Single;
    ASize: TSizeF;
    LName, LData, DefFontName: string;
  begin
    DefStrokeSize := Canvas.Stroke.Thickness;
    DefFontSize := FLayout.Font.Size;
    DefFontName := FLayout.Font.Family;
    DefFontAnchor := FontAnchor;

    for I := 0 to Nodes.Count - 1 do begin
      Item := Nodes.Items[I];
      if Item = nil then Continue;
      LName := LowerCase(Item.Name);
      Canvas.Stroke.Thickness := DefStrokeSize;

      if (LName = 'g') or (LName = 'a') then begin
        // 扩展
        Canvas.Font.Size := FData.ReadFloat(Item, 'font-size', 12);
        Canvas.Font.Family := FData.ReadString(Item, 'font');
        ParserStyle(Canvas, Item, 1);
        if Item.ChildNodes.Count > 0 then
          ParserNodesCaleSize(Canvas, Item.ChildNodes, Path, MW, MH);
        FLayout.Font.Size := DefFontSize;
        FLayout.Font.Family := DefFontName;
        FontAnchor := DefFontAnchor;
      end else if LName = 'path' then begin
        ParserStyle(Canvas, Item, 1);
        Path.Data := FData.ReadString(Item, 'd');
        Path.Scale(SX, SY);
        ParserPathSize(Path, W, H);
        MW := Max(MW, W + Canvas.Stroke.Thickness * 0.5);
        MH := Max(MH, H + Canvas.Stroke.Thickness * 0.5);
      end else if LName = 'rect' then begin
        ParserStyle(Canvas, Item, 1);
        W := FData.ReadFloat(Item, 'width') * SX;
        H := FData.ReadFloat(Item, 'height') * SY;
        X := FData.ReadFloat(Item, 'x') * SX;
        Y := FData.ReadFloat(Item, 'y') * SY;
        MW := Max(MW, X + W + Canvas.Stroke.Thickness);
        MH := Max(MH, Y + H + Canvas.Stroke.Thickness);
      end else if LName = 'circle' then begin
        ParserStyle(Canvas, Item, 1);
        RX := FData.ReadFloat(Item, 'cx') * SX;
        RY := FData.ReadFloat(Item, 'cy') * SY;
        W := FData.ReadFloat(Item, 'r') * SX;
        MW := Max(MW, RX + W + Canvas.Stroke.Thickness * 0.5);
        MH := Max(MH, RY + W + Canvas.Stroke.Thickness * 0.5);
      end else if LName = 'ellipse' then begin
        ParserStyle(Canvas, Item, 1);
        RX := FData.ReadFloat(Item, 'cx') * SX;
        RY := FData.ReadFloat(Item, 'cy') * SY;
        W := FData.ReadFloat(Item, 'rx') * SX;
        H := FData.ReadFloat(Item, 'ry') * SX;
        MW := Max(MW, RX + W + Canvas.Stroke.Thickness * 0.5);
        MH := Max(MH, RY + H + Canvas.Stroke.Thickness * 0.5);
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
        ParserStyle(Canvas, Item, 1);
        LData := FData.ReadString(Item, 'points');
        ParserPointsSize(LData, W, H, Canvas.Stroke.Thickness * 0.5);
        MW := Max(MW, W);
        MH := Max(MH, H);
      end else if LName = 'text' then begin
        // 文本
        ParserStyle(Canvas, Item, 1);
        X := FData.ReadFloat(Item, 'x') * SX;
        Y := FData.ReadFloat(Item, 'y') * SY;
        RX := FData.ReadFloat(Item, 'dx') * SX;
        RY := FData.ReadFloat(Item, 'dy') * SX;
        if Item.ChildNodes.Count = 0 then begin
          TextSize(Item.Text, ASize, Canvas.Scale);
          if FontAnchor = 3 then
            FontAnchor := DefFontAnchor;
          case FontAnchor of
            1: // middle
              W := X + ASize.Width * 0.5 + RX;
            2: // end
              W := X + RX;
          else
            W := X + ASize.Width + RX;
          end;
          MH := Max(MH, Y + RY);
          MW := Max(MW, W);
        end;
        FLayout.Font.Size := DefFontSize;
        FLayout.Font.Family := DefFontName;
        FontAnchor := DefFontAnchor;
      end;
    end;
  end;

  procedure ParserNodes(Canvas: TCanvas; Nodes: TXmlNodeList; Path: TPathData);
  var
    LPoints: TPolygon;
    Item: TXmlNode;
    I, J, DefFontAnchor: Integer;
    ASize: TSizeF;
    W, H, X, Y, RX, RY, BX, BY, LD, DefStrokeSize, DefFontSize: Single;
    DefStrokeColor, DefFillColor: TAlphaColor;
    LName, LData, DefFontName: string;
  begin
    DefStrokeColor := Canvas.Stroke.Color;
    DefFillColor := Canvas.Fill.Color;
    DefStrokeSize := Canvas.Stroke.Thickness;
    DefFontSize := FLayout.Font.Size;
    DefFontName := FLayout.Font.Family;
    DefFontAnchor := FontAnchor;

    for I := 0 to Nodes.Count - 1 do begin
      Item := Nodes.Items[I];
      if Item = nil then Continue;
      LName := LowerCase(Item.Name);

      FillOpacity := 1;
      StrokeOpacity := 1;
      Canvas.Stroke.Color := DefStrokeColor;
      Canvas.Fill.Color := DefFillColor;
      Canvas.Stroke.Thickness := DefStrokeSize;

      if (LName = 'g') or (LName = 'a') then begin
        // 扩展
        Canvas.Font.Size := FData.ReadFloat(Item, 'font-size', 12);
        Canvas.Font.Family := FData.ReadString(Item, 'font');
        ParserStyle(Canvas, Item);
        if Item.ChildNodes.Count > 0 then
          ParserNodes(Canvas, Item.ChildNodes, Path);
        FLayout.Font.Size := DefFontSize;
        FLayout.Font.Family := DefFontName;
        FontAnchor := DefFontAnchor;
      end else if LName = 'path' then begin
        // 路径
        LData := Item.Attributes['d'];
        if LData <> '' then begin
          Path.Data := LData;
          if (SX <> 1) or (SY <> 1) then
            Path.Scale(SX, SY);
          ParserStyle(Canvas, Item);
          if (Canvas.Fill.Color and $FF000000 <> 0) and (FillOpacity > 0) then
            Canvas.FillPath(Path, FillOpacity);
          if (Canvas.Stroke.Thickness > 0) and (StrokeOpacity > 0) then
            Canvas.DrawPath(Path, StrokeOpacity);
        end;
      end else if LName = 'rect' then begin
        // 矩形
        ParserStyle(Canvas, Item);
        W := FData.ReadFloat(Item, 'width') * SX;
        H := FData.ReadFloat(Item, 'height') * SY;
        if (W <= 0) or (H <= 0) then
          Continue;
        LD := Canvas.Stroke.Thickness;
        X := FData.ReadFloat(Item, 'x') * SX;
        Y := FData.ReadFloat(Item, 'y') * SY;
        RX := FData.ReadFloat(Item, 'rx') * SX;
        RY := FData.ReadFloat(Item, 'ry') * SY;
        if RX > 0 then
          BX := Max(0, RX - LD * 0.5)
        else
          BX := RX;
        if RY > 0 then
          BY := Max(0, RY - LD * 0.5)
        else
          BY := RY;
        if (Canvas.Fill.Color and $FF000000 <> 0) and (FillOpacity > 0) then
          Canvas.FillRect(RectF(X + LD * 0.5, Y + LD * 0.5, X + W - LD * 0.5, Y + H - LD * 0.5), BX, BY, AllCorners, FillOpacity);
        if LD > 0 then begin
          Canvas.DrawRect(RectF(X, Y, X + W, Y + H), RX, RY, AllCorners, StrokeOpacity);
        end;
      end else if LName = 'circle' then begin
        // 圆
        ParserStyle(Canvas, Item);
        RX := FData.ReadFloat(Item, 'cx') * SX;
        RY := FData.ReadFloat(Item, 'cy') * SY;
        W := FData.ReadFloat(Item, 'r') * SX;
        if W > 0 then begin
          if (Canvas.Fill.Color and $FF000000 <> 0) and (FillOpacity > 0) then
            Canvas.FillArc(PointF(RX, RY), PointF(W, W), 0, 360, FillOpacity);
          if Canvas.Stroke.Thickness > 0 then
            Canvas.DrawArc(PointF(RX, RY), PointF(W, W), 0, 360, StrokeOpacity);
        end;
      end else if LName = 'ellipse' then begin
        // 椭圆
        ParserStyle(Canvas, Item);
        RX := FData.ReadFloat(Item, 'cx') * SX;
        RY := FData.ReadFloat(Item, 'cy') * SY;
        W := FData.ReadFloat(Item, 'rx') * SX;
        H := FData.ReadFloat(Item, 'ry') * SX;
        if (W > 0) or (H > 0) then begin
          Canvas.FillArc(PointF(RX, RY), PointF(W, H), 0, 360, FillOpacity);
          if Canvas.Stroke.Thickness > 0 then
            Canvas.DrawArc(PointF(RX, RY), PointF(W, H), 0, 360, StrokeOpacity);
        end;
      end else if LName = 'line' then begin
        // 线条
        ParserStyle(Canvas, Item);
        X := FData.ReadFloat(Item, 'x1') * SX;
        Y := FData.ReadFloat(Item, 'y1') * SY;
        RX := FData.ReadFloat(Item, 'x2') * SX;
        RY := FData.ReadFloat(Item, 'y2') * SY;
        Path.Clear;
        Path.MoveTo(PointF(X, Y));
        Path.LineTo(PointF(RX, RY));
        if Canvas.Stroke.Thickness > 0 then
          Canvas.DrawPath(Path, StrokeOpacity);
      end else if LName = 'polygon' then begin
        // 多边形
        ParserStyle(Canvas, Item);
        LData := FData.ReadString(Item, 'points');
        if LData <> '' then begin
          ParserPoints(LPoints, LData);
          if Length(LPoints) = 0 then
            Continue;
          Canvas.FillPolygon(LPoints, FillOpacity);
          if Canvas.Stroke.Thickness > 0 then
           Canvas.DrawPolygon(LPoints, StrokeOpacity);
        end;
      end else if LName = 'polyline' then begin
        // 折线
        ParserStyle(Canvas, Item);
        LData := FData.ReadString(Item, 'points');
        if LData <> '' then begin
          ParserPoints(LPoints, LData, Canvas.Stroke.Thickness * 0.5);
          if (Length(LPoints) = 0) or (Canvas.Stroke.Thickness <= 0) then
            Continue;
          Path.Clear;
          Path.MoveTo(LPoints[0]);
          for J := 1 to High(LPoints) do
            Path.LineTo(LPoints[J]);
          Canvas.DrawPath(Path, StrokeOpacity);
        end;
      end else if LName = 'text' then begin
        // 文本
        ParserStyle(Canvas, Item);
        X := FData.ReadFloat(Item, 'x') * SX;
        Y := FData.ReadFloat(Item, 'y') * SY;
        RX := FData.ReadFloat(Item, 'dx') * SX;
        RY := FData.ReadFloat(Item, 'dy') * SX;

        if Item.ChildNodes.Count = 0 then begin
          LData := Item.Text;
          TextSize(LData, ASize, Canvas.Scale);
          if FontAnchor = 3 then
            FontAnchor := DefFontAnchor;
          case FontAnchor of
            1: // middle
              FillText(Canvas,
                RectF(X - ASize.Width * 0.5 + RX, Y - ASize.Height + RY, X + ASize.Width * 0.5 + RX, Y + RY),
                LData, Canvas.Fill.Color, FillOpacity, [],
                TTextAlign.Leading, TTextAlign.Trailing);
            2: // end
              FillText(Canvas,
                RectF(X - ASize.Width + RX, Y - ASize.Height + RY, X + RX, Y + RY),
                LData, Canvas.Fill.Color, FillOpacity, [],
                TTextAlign.Leading, TTextAlign.Trailing);
          else  // start
            FillText(Canvas,
              RectF(X + RX, Y - ASize.Height + RY, X + ASize.Width + RX, Y + RY),
              LData, Canvas.Fill.Color, FillOpacity, [],
              TTextAlign.Leading, TTextAlign.Trailing);
          end;
        end;
        FLayout.Font.Size := DefFontSize;
        FLayout.Font.Family := DefFontName;
        FontAnchor := DefFontAnchor;
      end;

    end;
  end;

var
  Path: TPathData;
  MW, MH: Single;
  Canvas: TCanvas;
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
  if not Assigned(FLayout) then begin
    Canvas := FBitmap.Canvas;
    FLayout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create(Canvas);
  end;
  Path := TPathData.Create;
  try
    // 自动大小
    if (FBitmap.Width = 0) and (FBitmap.Height = 0) then begin
      MW := 0;
      MH := 0;
      FontAnchor := 0;
      Canvas := FBitmap.Canvas;
      Canvas.Stroke.Thickness := 0;
      ParserNodesCaleSize(Canvas, FData.FSvg.ChildNodes, Path, MW, MH);
      FData.ViewBox := PointF(MW, MH);
      FBitmap.SetSize(Round(MW), Round(MH));
    end;

    if (FBitmap.Width = 0) or (FBitmap.Height = 0) then
      Exit;

    FBitmap.Clear(0);
    Canvas := FBitmap.Canvas;
    Canvas.BeginScene();
    try
      FontAnchor := 0;
      Canvas.Stroke.Thickness := 0;
      if FColor = 0 then begin
        Canvas.Stroke.Color := TAlphaColorRec.Black;
        Canvas.Fill.Color := TAlphaColorRec.Black;
      end else begin
        Canvas.Stroke.Color := FColor;
        Canvas.Fill.Color := FColor;
      end;
      ParserNodes(Canvas, FData.FSvg.ChildNodes, Path);
    finally
      Canvas.EndScene;
    end;
  finally
    FreeAndNil(Path);
  end;
end;

procedure TSVGImage.FillText(const Canvas: TCanvas; const ARect: TRectF;
  const AText: string; const AColor: TAlphaColor; const AOpacity: Single;
  const Flags: TFillTextFlags; const ATextAlign, AVTextAlign: TTextAlign);
begin
  with FLayout do begin
    BeginUpdate;
    TopLeft := ARect.TopLeft;
    MaxSize := PointF(ARect.Width, ARect.Height);
    Text := AText;
    WordWrap := False;
    Opacity := AOpacity;
    HorizontalAlign := ATextAlign;
    VerticalAlign := AVTextAlign;
    Color := AColor;
    Trimming := TTextTrimming.None;
    RightToLeft := False;
    EndUpdate;
    RenderLayout(Canvas);
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

procedure TSVGImage.InitBitmap;
begin
  FBitmap := TBitmap.Create;
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
    InitBitmap();
  FBitmap.SetSize(FData.Size.Width, FData.Size.Height);
  DrawSVG;
  DoChange();
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
    InitBitmap;
  FBitmap.SetSize(FData.Size.Width, FData.Size.Height);
  DrawSVG;
  DoChange();
end;

procedure TSVGImage.ReadData(Reader: TReader);
var
  Stream: TStringStream;
begin
  try
    Stream := TStringStream.Create;
    try
      Stream.WriteString(Reader.ReadString);
      Stream.Position := 0;
      Self.LoadFormStream(Stream);
    finally
      Stream.Free;
    end;
  except
  end;
end;

procedure TSVGImage.ReSize;
var
  LSize: TSizeF;
begin
  if Assigned(FData) then begin
    LSize := FData.GetSize;
    SetSize(Round(LSize.Width), Round(LSize.Height));
  end;
end;

procedure TSVGImage.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    DrawSVG;
    DoChange;
  end;
end;

procedure TSVGImage.SetHeight(const Value: Integer);
begin
  if Value <> Height then begin
    SetSize(Width, Value);
    DoChange;
  end;
end;

procedure TSVGImage.SetLoss(const Value: Boolean);
begin
  if FLoss <> Value then begin
    FLoss := Value;
    if (not Value) then
      ReSize();
    DoChange();
  end;
end;

procedure TSVGImage.SetSize(const Width, Height: Integer);
begin
  if Assigned(FBitmap) and ((Width <> FBitmap.Width) or (Height <> FBitmap.Height)) then begin
    FBitmap.SetSize(Width, Height);
    if (Width > 0) and (Height > 0) and (not Empty) then
      DrawSVG();
  end;
end;

procedure TSVGImage.SetWidth(const Value: Integer);
begin
  if Value <> Width then begin
    SetSize(Value, Height);
    DoChange;
  end;
end;

function RoundToScale(const Value, Scale: Single): Single;
begin
  if Scale > 0 then
    Result := Ceil(Value * Scale) / Scale
  else
    Result := Ceil(Value);
end;

procedure TSVGImage.TextSize(const AText: string; var ASize: TSizeF;
  const SceneScale, MaxWidth: Single; AWordWrap: Boolean);
begin
  with FLayout do begin
    BeginUpdate;
    TopLeft := TPointF.Zero;
    if MaxWidth < 0 then
      MaxSize := TTextLayout.MaxLayoutSize
    else
      MaxSize := PointF(MaxWidth, $FFFFFF);
    Text := AText;
    WordWrap := AWordWrap;
    HorizontalAlign := TTextAlign.Leading;
    VerticalAlign := TTextAlign.Leading;
    RightToLeft := False;
    EndUpdate;
    ASize.Width := Width;
    ASize.Height := Height;
    //ASize.Width := RoundToScale(Width, SceneScale);
    //ASize.Height := RoundToScale(Height, SceneScale);
  end;
end;

procedure TSVGImage.WriteData(Writer: TWriter);
var
  Stream: TStringStream;
begin
  try
    Stream := TStringStream.Create;
    try
      if Assigned(FData) and Assigned(FData.FData) then
        FData.FData.SaveToStream(Stream);
    finally
      Writer.WriteString(Stream.DataString);
      Stream.Free;
    end;
  except
  end;
end;

initialization

end.
