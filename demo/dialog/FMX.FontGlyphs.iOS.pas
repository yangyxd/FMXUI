{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.FontGlyphs.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Classes, System.SysUtils, System.UITypes, System.UIConsts, System.Generics.Collections,
  System.Generics.Defaults, Macapi.ObjectiveC, Macapi.CoreFoundation, iOSapi.CocoaTypes, iOSapi.CoreGraphics,
  iOSapi.Foundation, iOSapi.CoreText, iOSapi.UIKit, FMX.Types, FMX.Surfaces, FMX.FontGlyphs;

type
  TIOSFontGlyphManager = class(TFontGlyphManager)
  const
    BoundsLimit = $FFFF;
  private
    FColorSpace: CGColorSpaceRef;
    FFontRef: CTFontRef;
    FDefaultBaseline: Single;
    FDefaultVerticalAdvance: Single;
    procedure GetDefaultBaseline;
    function GetPostScriptFontName: CFStringRef;
  protected
    procedure LoadResource; override;
    procedure FreeResource; override;
    function DoGetGlyph(const Char: UCS4Char; const Settings: TFontGlyphSettings): TFontGlyph; override;
    function DoGetBaseline: Single; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.Math, System.Character, System.Math.Vectors, FMX.Graphics, FMX.Consts, FMX.Utils,  Macapi.Helpers;

{ TIOSFontGlyphManager }

constructor TIOSFontGlyphManager.Create;
begin
  inherited Create;
  FColorSpace := CGColorSpaceCreateDeviceRGB;
end;

destructor TIOSFontGlyphManager.Destroy;
begin
  CGColorSpaceRelease(FColorSpace);
  inherited;
end;

procedure TIOSFontGlyphManager.LoadResource;
const
  //Rotating matrix to simulate Italic font attribute
  ItalicMatrix: CGAffineTransform = (
    a: 1;
    b: 0;
    c: 0.176326981; //~tan(10 degrees)
    d: 1;
    tx: 0;
    ty: 0
  );
var
  NewFontRef: CTFontRef;
  Matrix: PCGAffineTransform;
begin
  Matrix := nil;
  FFontRef := CTFontCreateWithName(GetPostScriptFontName, CurrentSettings.Size * CurrentSettings.Scale, nil);
  try
    if TFontStyle.fsItalic in CurrentSettings.Style then
    begin
      NewFontRef := CTFontCreateCopyWithSymbolicTraits(FFontRef, 0, nil,
        kCTFontItalicTrait, kCTFontItalicTrait);
      if NewFontRef <> nil then
      begin
        CFRelease(FFontRef);
        FFontRef := NewFontRef;
      end
      else
      begin
        Matrix := @ItalicMatrix;
        //Font has no Italic version, applying transform matrix
        NewFontRef := CTFontCreateWithName(GetPostScriptFontName, CurrentSettings.Size * CurrentSettings.Scale,
          @ItalicMatrix);
        if NewFontRef <> nil then
        begin
          CFRelease(FFontRef);
          FFontRef := NewFontRef;
        end;
      end;
    end;
    if TFontStyle.fsBold in CurrentSettings.Style then
    begin
      NewFontRef := CTFontCreateCopyWithSymbolicTraits(FFontRef, 0, Matrix, kCTFontBoldTrait, kCTFontBoldTrait);
      if NewFontRef <> nil then
      begin
        CFRelease(FFontRef);
        FFontRef := NewFontRef;
      end;
    end;
    //
    GetDefaultBaseline;
  except
    CFRelease(FFontRef);
  end;
end;

procedure TIOSFontGlyphManager.FreeResource;
begin
  if FFontRef <> nil then
    CFRelease(FFontRef);
end;

procedure TIOSFontGlyphManager.GetDefaultBaseline;
var
  Chars: string;
  Str: CFStringRef;
  Frame: CTFrameRef;
  Attr: CFMutableAttributedStringRef;
  Path: CGMutablePathRef;
  Bounds: CGRect;
  FrameSetter: CTFramesetterRef;
  // Metrics
  Line: CTLineRef;
  Lines: CFArrayRef;
  Runs: CFArrayRef;
  Run: CTRunRef;
  Ascent, Descent, Leading: CGFloat;
  BaseLinePos: CGPoint;
begin
  Path := CGPathCreateMutable();
  Bounds := CGRectMake(0, 0, BoundsLimit, BoundsLimit);
  CGPathAddRect(Path, nil, Bounds);
  if TOSVersion.Check(9) then // yangyxd 
    Chars := 'жа'
  else
    Chars := 'a';
  Str := CFStringCreateWithCharacters(kCFAllocatorDefault, PChar(Chars), 1);

  Attr := CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
  CFAttributedStringReplaceString(Attr, CFRangeMake(0, 0), Str);

  CFAttributedStringBeginEditing(Attr);
  try
    // Font
    if FFontRef <> nil then
      CFAttributedStringSetAttribute(Attr, CFRangeMake(0, 1), kCTFontAttributeName, FFontRef);
  finally
    CFAttributedStringEndEditing(Attr);
  end;

  FrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(Attr));
  CFRelease(Attr);

  Frame := CTFramesetterCreateFrame(FrameSetter, CFRangeMake(0, 0), Path, nil);
  CFRelease(FrameSetter);
  CFRelease(Str);

  // Metrics
  Lines := CTFrameGetLines(Frame);
  Line := CTLineRef(CFArrayGetValueAtIndex(Lines, 0));
  Runs := CTLineGetGlyphRuns(Line);
  Run := CFArrayGetValueAtIndex(Runs, 0);
  CTRunGetTypographicBounds(Run, CFRangeMake(0, 1), @Ascent,  @Descent, @Leading);

  CTFrameGetLineOrigins(Frame, CFRangeMake(0, 0), @BaseLinePos);
  FDefaultBaseline := BoundsLimit - BaseLinePos.y;

  FDefaultVerticalAdvance := FDefaultBaseline + Descent;

  CFRelease(Frame);
  CFRelease(Path);
end;

function TIOSFontGlyphManager.GetPostScriptFontName: CFStringRef;
var
  LUIFont: UIFont;
  LocalObject: ILocalObject;
begin
  Result := nil;
  LUIFont := TUIFont.Wrap(TUIFont.OCClass.fontWithName(StrToNSStr(CurrentSettings.Family), CurrentSettings.Size * CurrentSettings.Scale));
  if Supports(LUIFont, ILocalObject, LocalObject) then
    Result := CTFontCopyPostScriptName(LocalObject.GetObjectID);
  if Result = nil then
    //In case there is no direct name for the requested font returns source name and let CoreText to select appropriate font
    Result := CFSTR(CurrentSettings.Family);
end;

procedure PathApplierFunction(info: Pointer; const element: PCGPathElement); cdecl;
var
  P, P1, P2: PCGPoint;
begin
  P := element^.points;
  case element.type_ of
    kCGPathElementMoveToPoint:
      TPathData(info).MoveTo(TPointF.Create(P.x, P.y));
    kCGPathElementAddLineToPoint:
      TPathData(info).LineTo(TPointF.Create(P.x, P.y));
    kCGPathElementAddQuadCurveToPoint:
      begin
        P1 := P;
        Inc(P1);
        TPathData(info).QuadCurveTo(TPointF.Create(P.x, P.y), TPointF.Create(P1.x, P1.y));
      end;
    kCGPathElementAddCurveToPoint:
      begin
        P1 := P;
        Inc(P1);
        P2 := P1;
        Inc(P2);
        TPathData(info).CurveTo(TPointF.Create(P.x, P.y), TPointF.Create(P1.x, P1.y), TPointF.Create(P2.x, P2.y));
      end;
    kCGPathElementCloseSubpath:
      TPathData(info).ClosePath;
  end;
end;

function TIOSFontGlyphManager.DoGetBaseline: Single;
begin
  Result := FDefaultBaseline;
end;

function TIOSFontGlyphManager.DoGetGlyph(const Char: UCS4Char; const Settings: TFontGlyphSettings): TFontGlyph;
var
  CharsString: string;
  CharsStringLength: Integer;
  Str: CFStringRef;
  Frame: CTFrameRef;
  Attr: CFMutableAttributedStringRef;
  Path: CGMutablePathRef;
  Bounds: CGRect;
  Rgba: array [0..3] of CGFloat;
  TextColor: CGColorRef;
  FrameSetter: CTFramesetterRef;
  Context: CGContextRef;
  I, J: Integer;
  Color: TAlphaColorRec;
  C: Byte;
  GlyphRect: TRect;
  // Metrics
  Line: CTLineRef;
  Lines: CFArrayRef;
  Runs: CFArrayRef;
  Run: CTRunRef;
  Ascent, Descent, Leading: CGFloat;
  Size: CGSize;
  GlyphStyle: TFontGlyphStyles;
  BaseLinePos: CGPoint;
  BaseLineOffset: Single;
  //
  RunGlyphCount: CFIndex;
  glyph: CGGlyph;
  glyphMatrix: CGAffineTransform;
  position:  CGPoint;
  glyphPath: CGPathRef;
  M: TMatrix;
  LImageChar: Boolean;
  Bits: PAlphaColorRecArray;
  ContextSize: TSize;
begin
  LImageChar :=
    ((Char >= $1F000) and (Char <= $1F0FF)) or
    ((Char >= $1F170) and (Char <= $1F19F)) or
    ((Char >= $1F200) and (Char <= $1F2FF)) or
    ((Char >= $1F300) and (Char <= $1F5FF)) or
    ((Char >= $1F600) and (Char <= $1F64F)) or
    ((Char >= $1F680) and (Char <= $1F6FF)) or
    ((Char >= $1F700) and (Char <= $1F77F)) or
    (Char = $2139) or
    ((Char >= $2190) and (Char <= $21FF)) or
    ((Char >= $2300) and (Char <= $23FF)) or
    ((Char >= $2460) and (Char <= $24FF)) or
    ((Char >= $25A0) and (Char <= $25FF)) or
    ((Char >= $2600) and (Char <= $26FF)) or
    ((Char >= $2700) and (Char <= $27BF)) or
    ((Char >= $27C0) and (Char <= $27EF)) or
    ((Char >= $27F0) and (Char <= $27FF)) or
    ((Char >= $2900) and (Char <= $297F)) or
    ((Char >= $2B00) and (Char <= $2BFF)) or
    ((Char >= $3200) and (Char <= $32FF));
  Path := CGPathCreateMutable();
  Bounds := CGRectMake(0, 0, BoundsLimit, BoundsLimit);
  CGPathAddRect(Path, nil, Bounds);
  CharsString := System.Char.ConvertFromUtf32(Char);
  CharsStringLength := CharsString.Length;
  Str := CFStringCreateWithCharacters(kCFAllocatorDefault, PChar(CharsString), CharsStringLength);

  Attr := CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
  CFAttributedStringReplaceString(Attr, CFRangeMake(0, 0), Str);

  CFAttributedStringBeginEditing(Attr);
  try
    // Font
    if FFontRef <> nil then
      CFAttributedStringSetAttribute(Attr, CFRangeMake(0, CharsStringLength), kCTFontAttributeName, FFontRef);
    // Color
    Rgba[0] := 1;
    Rgba[1] := 1;
    Rgba[2] := 1;
    Rgba[3] := 1;
    TextColor := CGColorCreate(FColorSpace, @Rgba[0]);
    try
      CFAttributedStringSetAttribute(Attr, CFRangeMake(0, CharsStringLength), kCTForegroundColorAttributeName, TextColor);
    finally
      CFRelease(TextColor);
    end;
  finally
    CFAttributedStringEndEditing(Attr);
  end;

  FrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(Attr));
  CFRelease(Attr);

  Frame := CTFramesetterCreateFrame(FrameSetter, CFRangeMake(0, 0), Path, nil);
  CFRelease(FrameSetter);
  CFRelease(Str);

  // Metrics
  Context := CGBitmapContextCreate(nil, 1, 1, 8, 4, FColorSpace, kCGImageAlphaPremultipliedLast);
  try
    Lines := CTFrameGetLines(Frame);

    Line := CTLineRef(CFArrayGetValueAtIndex(Lines, 0));
    Runs := CTLineGetGlyphRuns(Line);

    Run := CFArrayGetValueAtIndex(Runs, 0);

    Bounds := CTRunGetImageBounds(Run, Context, CFRangeMake(0, 1));
    CTRunGetAdvances(Run, CFRangeMake(0, 1), @Size);
    CTRunGetTypographicBounds(Run, CFRangeMake(0, 1), @Ascent,  @Descent, @Leading);

    GlyphRect := Rect(Trunc(Bounds.origin.x),
      Max(Trunc(Ascent - Bounds.origin.y - Bounds.size.height) - 1, 0),
      Ceil(Bounds.origin.x + Bounds.size.width),
      Round(Ascent + Descent + Descent));

    CTFrameGetLineOrigins(Frame, CFRangeMake(0, 0), @BaseLinePos);
    BaseLineOffset := BoundsLimit - BaseLinePos.y;

    GlyphStyle := [];
    if ((Bounds.size.width = 0) and (Bounds.size.height = 0)) or not HasGlyph(Char) then
      GlyphStyle := [TFontGlyphStyle.NoGlyph];
    if TFontGlyphSetting.Path in Settings then
      GlyphStyle := GlyphStyle + [TFontGlyphStyle.HasPath];
    if LImageChar then
      GlyphStyle := GlyphStyle + [TFontGlyphStyle.ColorGlyph];
  finally
    CGContextRelease(Context);
  end;

  Result := TFontGlyph.Create(Point(GlyphRect.Left, GlyphRect.Top), Size.width,
    Round(FDefaultVerticalAdvance), GlyphStyle);
  if (TFontGlyphSetting.Bitmap in Settings) and
     (HasGlyph(Char) or ((Bounds.size.width > 0) and (Bounds.size.height > 0))) then
  begin
    ContextSize := TSize.Create(Max(GlyphRect.Right, GlyphRect.Width), GlyphRect.Bottom);

    Context := CGBitmapContextCreate(nil, ContextSize.Width, ContextSize.Height, 8, ContextSize.Width * 4, FColorSpace,
      kCGImageAlphaPremultipliedLast);
    try
      Bits := PAlphaColorRecArray(CGBitmapContextGetData(Context));

      if GlyphRect.Left < 0 then
        CGContextTranslateCTM(Context, -GlyphRect.Left, 0);
      CGContextTranslateCTM(Context, 0, -(BoundsLimit - ContextSize.Height));
      if not SameValue(FDefaultBaseline - BaseLineOffset, 0, TEpsilon.Position) then
        CGContextTranslateCTM(Context, 0, -Abs(FDefaultBaseline - BaseLineOffset));
      CTFrameDraw(Frame, Context);

      Result.Bitmap.SetSize(GlyphRect.Width, GlyphRect.Height, TPixelFormat.BGRA);

      if TFontGlyphSetting.PremultipliedAlpha in Settings then
      begin
        for I := GlyphRect.Top to GlyphRect.Bottom - 1 do
          Move(Bits^[I * ContextSize.Width + Max(GlyphRect.Left, 0)],
            Result.Bitmap.GetPixelAddr(0, I - GlyphRect.Top)^, Result.Bitmap.Pitch);
      end
      else
        for I := GlyphRect.Left to GlyphRect.Right - 1 do
          for J := GlyphRect.Top to GlyphRect.Bottom - 1 do
          begin
            Color := Bits[J * ContextSize.Width + Max(I, 0)];
            if Color.R > 0 then
            begin
              C := (Color.R + Color.G + Color.B) div 3;
              Result.Bitmap.Pixels[I - GlyphRect.Left, J - GlyphRect.Top] := MakeColor($FF, $FF, $FF, C);
            end
          end;
    finally
      CGContextRelease(Context);
    end;
  end;
  //Path
  if TFontGlyphSetting.Path in Settings then
  begin
    RunGlyphCount := CTRunGetGlyphCount(Run);
    for I := 0 to RunGlyphCount - 1 do
    begin
      CTRunGetGlyphs(Run, CFRangeMake(I, 1), @glyph);
      CTRunGetPositions(run, CFRangeMake(I, 1), @position);
                                                                                    
      glyphMatrix := CGAffineTransformTranslate(CGAffineTransformIdentity,
        position.x, position.y);
      glyphPath := CTFontCreatePathForGlyph(FFontRef, glyph, @glyphMatrix);
      if glyphPath <> nil then
      begin
        CGPathApply(glyphPath, Result.Path, @PathApplierFunction);
        CFRelease(glyphPath);
      end;
    end;
    M := TMatrix.Identity;
    M.m22 := -1;
    Result.Path.ApplyMatrix(M);
  end;
  CFRelease(Frame);
  CFRelease(Path);
end;

end.
