unit UI.FontGlyphs.Android;

interface

uses
  FMX.FontGlyphs,
  FMX.FontGlyphs.Android,
  Androidapi.JNI.GraphicsContentViewText;

type
  TAndroidFontGlyphManagerFMXUI = class(TAndroidFontGlyphManager)
  protected
    procedure LoadResource; override;
  end;

implementation

uses
  UI.Base, System.IOUtils,
  System.Types, System.Math, System.Character, System.Generics.Collections, System.UIConsts, System.UITypes,
  System.Classes, System.SysUtils, FMX.Types, FMX.Surfaces, FMX.Graphics, Androidapi.JNI.JavaTypes, Androidapi.Bitmap,
  Androidapi.JNIBridge, Androidapi.Helpers;

{ TAndroidFontGlyphManagerFMXUI }

procedure TAndroidFontGlyphManagerFMXUI.LoadResource;
var
  TypefaceFlag: Integer;
  Typeface: JTypeface;
  FamilyName: JString;
  Metrics: JPaint_FontMetricsInt;
  FPaint: JPaint;
  FontFile: string;
begin
  FPaint := TView.GetRttiValue<JPaint>(Self, 'FPaint');
  FPaint.setAntiAlias(True);
  FPaint.setTextSize(CurrentSettings.Size * CurrentSettings.Scale);
  FPaint.setARGB(255, 255, 255, 255);
  if TOSVersion.Check(4, 0) then
    FPaint.setHinting(TJPaint.JavaClass.HINTING_ON);
  //Font
  try
    FamilyName := StringToJString(CurrentSettings.Family);
    {$IF CompilerVersion > 30}
    if not CurrentSettings.Style.Slant.IsRegular and not CurrentSettings.Style.Weight.IsRegular then
      TypefaceFlag := TJTypeface.JavaClass.BOLD_ITALIC
    else if not CurrentSettings.Style.Weight.IsRegular then
      TypefaceFlag := TJTypeface.JavaClass.BOLD
    else if not CurrentSettings.Style.Slant.IsRegular then
      TypefaceFlag := TJTypeface.JavaClass.ITALIC
    else
      TypefaceFlag := TJTypeface.JavaClass.NORMAL;
    {$ELSE}
    if (TFontStyle.fsBold in CurrentSettings.Style) and (TFontStyle.fsItalic in CurrentSettings.Style) then
      TypefaceFlag := TJTypeface.JavaClass.BOLD_ITALIC
    else if (TFontStyle.fsBold in CurrentSettings.Style) then
      TypefaceFlag := TJTypeface.JavaClass.BOLD
    else if (TFontStyle.fsItalic in CurrentSettings.Style) then
      TypefaceFlag := TJTypeface.JavaClass.ITALIC
    else
      TypefaceFlag := TJTypeface.JavaClass.NORMAL;
    {$ENDIF}
    FontFile := TPath.GetDocumentsPath + PathDelim + CurrentSettings.Family + '.ttf';
    if FileExists(FontFile) then
      Typeface := TJTypeface.JavaClass.createFromFile(StringToJString(FontFile))
    else
      Typeface := TJTypeface.JavaClass.Create(FamilyName, TypefaceFlag);

    FPaint.setTypeface(Typeface);
    try
      Metrics := FPaint.getFontMetricsInt;
      //
      TView.SetRttiValue<Integer>(Self, 'FTop', Metrics.top);
      TView.SetRttiValue<Integer>(Self, 'FAscent', Metrics.ascent);
      TView.SetRttiValue<Integer>(Self, 'FDescent', Metrics.descent);
      TView.SetRttiValue<Integer>(Self, 'FBottom', Metrics.bottom);
      TView.SetRttiValue<Integer>(Self, 'FLeading', Metrics.leading);
      {
      FTop := Metrics.top;
      FAscent := Metrics.ascent;
      FDescent := Metrics.descent;
      FBottom := Metrics.bottom;
      FLeading := Metrics.leading;
      }
    finally
      Metrics := nil;
    end;
  finally
    FamilyName := nil;
    Typeface := nil;
  end;
end;

end.
