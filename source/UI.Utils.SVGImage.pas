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
    function ReadFloat(Node: TXmlNode; const Name: string; const DefaultValue: Double = 0): Double;
    function ReadColor(Node: TXmlNode; const Name: string; const DefaultValue: TAlphaColor = 0): TAlphaColor;
    function ReadString(Node: TXmlNode; const Name: string): string;
    function ReadFontAnchor(Node: TXmlNode; DefaultValue: Integer = 0): Integer;
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

function TSVGDecode.ReadFontAnchor(Node: TXmlNode; DefaultValue: Integer): Integer;
var
  S: string;
begin
  Result := DefaultValue;
  S := LowerCase(ReadString(Node, 'text-anchor'));
  if S = 'start' then
    Result := 0
  else if S = 'middle' then
    Result := 1
  else if S = 'end' then
    Result := 2   
  else if S = 'inherit' then
    Result := 3   
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
        FBitmap.SetSize(FData.Size);
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
  FillOpacity, StrokeOpacity: Single;
  SX, SY: Single;
  FontAnchor: Integer;

  function StrLCompX(const Str1, Str2: PWideChar; MaxLen, Len: Cardinal): Integer;
  begin
    if Len <> MaxLen then
      Result := -1
    else
      Result := StrLComp(Str1, Str2, MaxLen);
  end;

  procedure ParserStyleItem(Canvas: TCanvas; const PS: PChar; const PN: Integer;
    const PV: PChar; const PL, Flag: Integer);
  var
    Value: string;
    P: PChar;
  begin
    if (PL < 1) or (PN < 1) then Exit;
    if StrLCompX(PS, 'stroke-width', 12, PN) = 0 then begin
      P := PV + PL;
      while P > PV do begin
        Dec(P);
        if ('0123456789'.Contains(P^)) then begin
          Inc(P);
          Break;
        end;
      end;
      SetString(Value, PV, P - PV);
      Canvas.Stroke.Thickness := StrToFloatDef(TrimRight(Value), 0) * SX;    
    end;
    if Flag <> 0 then Exit;

    if (FColor = 0) and (StrLCompX(PS, 'fill', 4, PN) = 0) then begin
      SetString(Value, PV, PL);
      Canvas.Fill.Color := HtmlColorToColor(TrimRight(Value), Canvas.Fill.Color);
    end else if (FColor = 0) and (StrLCompX(PS, 'stroke', 6, PN) = 0) then begin
      SetString(Value, PV, PL);
      Canvas.Stroke.Color := HtmlColorToColor(TrimRight(Value), Canvas.Stroke.Color);
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
    end else if StrLCompX(PS, 'font-size', 9, PN) = 0 then begin
      SetString(Value, PV, PL);
      FLayout.Font.Size := StrToFloatDef(TrimRight(Value), FLayout.Font.Size);
    end else if StrLCompX(PS, 'font-family', 11, PN) = 0 then begin
      SetString(Value, PV, PL);
      FLayout.Font.Family := Value;
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
      end else if (PV <> nil) and (P^ = ';') then begin
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

  procedure ParserStyle(Canvas: TCanvas; Item: TXmlNode; Flag: Integer = 0); overload;
  begin
    if (Flag = 0) and (FColor = 0) then
      Canvas.Fill.Color := FData.ReadColor(Item, 'fill', Canvas.Fill.Color);
    if (Flag = 0) and (FColor = 0) and Item.HasAttribute('stroke') then
      Canvas.Stroke.Color := FData.ReadColor(Item, 'stroke', Canvas.Stroke.Color);
    if Item.HasAttribute('stroke-width') then
      Canvas.Stroke.Thickness := FData.ReadFloat(Item, 'stroke-width', Canvas.Stroke.Thickness) * SX;
    if Item.HasAttribute('text-anchor') then
      FontAnchor := FData.ReadFontAnchor(Item, FontAnchor);
    if Item.HasAttribute('font-size') then
      FLayout.Font.Size := FData.ReadFloat(Item, 'font-size', FLayout.Font.Size);
    if Item.HasAttribute('font-family') then
      FLayout.Font.Family := FData.ReadString(Item, 'font-family');
    ParserStyle(Canvas, FData.ReadString(Item, 'style'), Flag);
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
        if Kind = TPathPointKind.Close then
          Continue;
        W := Max(Point.X, W);
        H := Max(Point.Y, H);
      end;
    end;
  end;

  procedure ParserNodesCaleSize(Nodes: TXmlNodeList; Path: TPathData; var MW, MH: Single);
  var
    Item: TXmlNode;
    I, DefFontAnchor: Integer;
    W, H, X, Y, RX, RY, DefFontSize, DefStrokeSize: Single;
    ASize: TSizeF;
    LName, LData, DefFontName: string;
  begin
    DefStrokeSize := FBitmap.Canvas.Stroke.Thickness;
    DefFontSize := FLayout.Font.Size;
    DefFontName := FLayout.Font.Family; 
    DefFontAnchor := FontAnchor;
    
    for I := 0 to Nodes.Count - 1 do begin
      Item := Nodes.Items[I];
      if Item = nil then Continue;
      LName := LowerCase(Item.Name);
      FBitmap.Canvas.Stroke.Thickness := DefStrokeSize;
      FLayout.Font.Size := DefFontSize;
      FLayout.Font.Family := DefFontName;
      FontAnchor := DefFontAnchor;
            
      if (LName = 'g') or (LName = 'a') then begin
        // 扩展
        FBitmap.Canvas.Font.Size := FData.ReadFloat(Item, 'font-size', 12);
        FBitmap.Canvas.Font.Family := FData.ReadString(Item, 'font');
        ParserStyle(FBitmap.Canvas, Item, 1);
        if Item.ChildNodes.Count > 0 then
          ParserNodesCaleSize(Item.ChildNodes, Path, MW, MH);
      end else if LName = 'path' then begin
        ParserStyle(FBitmap.Canvas, Item, 1);
        Path.Data := FData.ReadString(Item, 'd');
        Path.Scale(SX, SY);
        ParserPathSize(Path, W, H);
        MW := Max(MW, W + FBitmap.Canvas.Stroke.Thickness * 0.5);
        MH := Max(MH, H + FBitmap.Canvas.Stroke.Thickness * 0.5);
      end else if LName = 'rect' then begin
        ParserStyle(FBitmap.Canvas, Item, 1);
        W := FData.ReadFloat(Item, 'width') * SX;
        H := FData.ReadFloat(Item, 'height') * SY;
        X := FData.ReadFloat(Item, 'x') * SX;
        Y := FData.ReadFloat(Item, 'y') * SY;
        MW := Max(MW, X + W + FBitmap.Canvas.Stroke.Thickness);
        MH := Max(MH, Y + H + FBitmap.Canvas.Stroke.Thickness);
      end else if LName = 'circle' then begin
        ParserStyle(FBitmap.Canvas, Item, 1);
        RX := FData.ReadFloat(Item, 'cx') * SX;
        RY := FData.ReadFloat(Item, 'cy') * SY;
        W := FData.ReadFloat(Item, 'r') * SX;
        MW := Max(MW, RX + W + FBitmap.Canvas.Stroke.Thickness * 0.5);
        MH := Max(MH, RY + W + FBitmap.Canvas.Stroke.Thickness * 0.5);
      end else if LName = 'ellipse' then begin
        ParserStyle(FBitmap.Canvas, Item, 1);
        RX := FData.ReadFloat(Item, 'cx') * SX;
        RY := FData.ReadFloat(Item, 'cy') * SY;
        W := FData.ReadFloat(Item, 'rx') * SX;
        H := FData.ReadFloat(Item, 'ry') * SX;
        MW := Max(MW, RX + W + FBitmap.Canvas.Stroke.Thickness * 0.5);
        MH := Max(MH, RY + H + FBitmap.Canvas.Stroke.Thickness * 0.5);
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
        ParserStyle(FBitmap.Canvas, Item, 1);
        LData := FData.ReadString(Item, 'points');
        ParserPointsSize(LData, W, H, FBitmap.Canvas.Stroke.Thickness * 0.5);
        MW := Max(MW, W);
        MH := Max(MH, H);
      end else if LName = 'text' then begin
        // 文本
        ParserStyle(FBitmap.Canvas, Item, 1);
        X := FData.ReadFloat(Item, 'x') * SX;
        Y := FData.ReadFloat(Item, 'y') * SY;
        RX := FData.ReadFloat(Item, 'dx') * SX;
        RY := FData.ReadFloat(Item, 'dy') * SX;
        if Item.ChildNodes.Count = 0 then begin
          TextSize(Item.Text, ASize, FBitmap.Canvas.Scale);
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
      end;
    end;
  end;

  procedure ParserNodes(Nodes: TXmlNodeList; Path: TPathData);
  var
    LPoints: TPolygon;
    Item: TXmlNode;
    I, J, DefFontAnchor: Integer;
    ASize: TSizeF;
    W, H, X, Y, RX, RY, BX, BY, LD, DefStrokeSize, DefFontSize: Single;
    DefStrokeColor, DefFillColor: TAlphaColor;
    LName, LData, DefFontName: string;
  begin
    DefStrokeColor := FBitmap.Canvas.Stroke.Color;
    DefFillColor := FBitmap.Canvas.Fill.Color;
    DefStrokeSize := FBitmap.Canvas.Stroke.Thickness;
    DefFontSize := FLayout.Font.Size;
    DefFontName := FLayout.Font.Family; 
    DefFontAnchor := FontAnchor;
    
    for I := 0 to Nodes.Count - 1 do begin
      Item := Nodes.Items[I];
      if Item = nil then Continue;
      LName := LowerCase(Item.Name);

      FillOpacity := 1;
      StrokeOpacity := 1;
      FBitmap.Canvas.Stroke.Color := DefStrokeColor;
      FBitmap.Canvas.Fill.Color := DefFillColor;
      FBitmap.Canvas.Stroke.Thickness := DefStrokeSize;
      FLayout.Font.Size := DefFontSize;
      FLayout.Font.Family := DefFontName;
      FontAnchor := DefFontAnchor;

      if (LName = 'g') or (LName = 'a') then begin
        // 扩展
        FBitmap.Canvas.Font.Size := FData.ReadFloat(Item, 'font-size', 12);
        FBitmap.Canvas.Font.Family := FData.ReadString(Item, 'font');
        ParserStyle(FBitmap.Canvas, Item);
        if Item.ChildNodes.Count > 0 then
          ParserNodes(Item.ChildNodes, Path);
      end else if LName = 'path' then begin
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
        LD := FBitmap.Canvas.Stroke.Thickness;
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
        FBitmap.Canvas.FillRect(RectF(X + LD * 0.5, Y + LD * 0.5, X + W - LD * 0.5, Y + H - LD * 0.5), BX, BY, AllCorners, FillOpacity);
        if LD > 0 then begin
          FBitmap.Canvas.DrawRect(RectF(X, Y, X + W, Y + H), RX, RY, AllCorners, StrokeOpacity);
        end;
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
            FBitmap.Canvas.DrawArc(PointF(RX, RY), PointF(W, W), 0, 360, StrokeOpacity);
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
            FBitmap.Canvas.DrawArc(PointF(RX, RY), PointF(W, H), 0, 360, StrokeOpacity);
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
      end else if LName = 'text' then begin
        // 文本
        ParserStyle(FBitmap.Canvas, Item);
        X := FData.ReadFloat(Item, 'x') * SX;
        Y := FData.ReadFloat(Item, 'y') * SY;
        RX := FData.ReadFloat(Item, 'dx') * SX;
        RY := FData.ReadFloat(Item, 'dy') * SX;
        
        if Item.ChildNodes.Count = 0 then begin
          LData := Item.Text;  
          TextSize(LData, ASize, FBitmap.Canvas.Scale);
          if FontAnchor = 3 then
            FontAnchor := DefFontAnchor;
          case FontAnchor of
            1: // middle 
              FillText(FBitmap.Canvas, 
                RectF(X - ASize.Width * 0.5 + RX, Y - ASize.Height + RY, X + ASize.Width * 0.5 + RX, Y + RY), 
                LData, FBitmap.Canvas.Fill.Color, FillOpacity, [], 
                TTextAlign.Leading, TTextAlign.Trailing);                
            2: // end
              FillText(FBitmap.Canvas, 
                RectF(X - ASize.Width + RX, Y - ASize.Height + RY, X + RX, Y + RY), 
                LData, FBitmap.Canvas.Fill.Color, FillOpacity, [], 
                TTextAlign.Leading, TTextAlign.Trailing); 
          else  // start
            FillText(FBitmap.Canvas, 
              RectF(X + RX, Y - ASize.Height + RY, X + ASize.Width + RX, Y + RY), 
              LData, FBitmap.Canvas.Fill.Color, FillOpacity, [], 
              TTextAlign.Leading, TTextAlign.Trailing);   
          end;
        end;
      end;

    end;
  end;

var
  Path: TPathData;
  MW, MH: Single;
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
  FLayout := TTextLayoutManager.TextLayoutByCanvas(FBitmap.Canvas.ClassType).Create(FBitmap.Canvas);
  try
    // 自动大小
    if (FBitmap.Width = 0) and (FBitmap.Height = 0) then begin
      MW := 0;
      MH := 0;
      FontAnchor := 0;
      FBitmap.Canvas.Stroke.Thickness := 0;
      ParserNodesCaleSize(FData.FSvg.ChildNodes, Path, MW, MH);
      FData.ViewBox := PointF(MW, MH);
      FBitmap.SetSize(Round(MW), Round(MH));
    end;

    if (FBitmap.Width = 0) or (FBitmap.Height = 0) then
      Exit;

    FBitmap.Clear(0);
    FBitmap.Canvas.BeginScene();
    try
      FontAnchor := 0;
      FBitmap.Canvas.Stroke.Thickness := 0;
      if FColor = 0 then begin
        FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Black;
        FBitmap.Canvas.Fill.Color := TAlphaColorRec.Black;
      end else begin
        FBitmap.Canvas.Stroke.Color := FColor;
        FBitmap.Canvas.Fill.Color := FColor;
      end;
      ParserNodes(FData.FSvg.ChildNodes, Path);
    finally
      FBitmap.Canvas.EndScene;
    end;
  finally
    FreeAndNil(Path);
    FreeAndNil(FLayout);
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
  FBitmap.SetSize(FData.Size);
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
  FBitmap.SetSize(FData.Size);
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
