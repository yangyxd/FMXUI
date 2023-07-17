{*******************************************************}
{                                                       }
{       FMX UI 标准组件TextView的拓展                   }
{             修改TextView的外观样式                    }
{                                                       }
{         版权所有 (C) 2023 dqi1999                     }
{                                                       }
{*******************************************************}

unit UI.TextViewEditor;

interface

uses
  FMX.Controls, FMX.Controls.Presentation, FMX.Dialogs, FMX.Forms, FMX.Graphics,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Platform, FMX.StdCtrls,
  FMX.TabControl, FMX.Types, System.JSON, System.Classes, System.Math,
  System.SysUtils, System.Types, System.UITypes, System.Variants,
  UI.ButtonViewstyle, UI.Json, UI.Base, UI.Edit, UI.Standard;

type
  TTextViewStyleEditor = class(TForm)
    TextViewFamily: TTextView;
    EditView1: TEditView;
    ViewStyle: TView;
    View11: TView;
    ButtonView1: TButtonView;
    ButtonView2: TButtonView;
    ButtonView3: TButtonView;
    ViewMain: TView;
    GridPanelLayout1: TGridPanelLayout;
    FlowLayout1: TFlowLayout;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    FlowLayout2: TFlowLayout;
    SourceButton: TTextView;
    DestButton: TTextView;
    VertScrollView1: TVertScrollView;
    VertScrollView2: TVertScrollView;
    procedure ButtonView3Click(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
    procedure TextViewFamilyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ViewMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FStyledButton: TTextView;
    AFamily, AClass, AAppearance: string;

    procedure StyleViewclick(Sender: TObject);
    procedure ApplyStyle;
    procedure GetcolorJson;
  public
    { Public declarations }
  end;

function EditTextViewStyled(const AButton: TTextView): Boolean;

implementation

{$R *.fmx}

function SetClipboardTxt(txt: string): Boolean;
var
  AClipboard: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, AClipboard) then
    AClipboard.SetClipboard(txt);
end;

procedure CopyStyle(sbtn, dbtn: TTextView);
begin
  dbtn.Background.Assign(sbtn.Background);

  if (dbtn is TTextView) and (sbtn is TTextView) then
  begin
    dbtn.TextSettings.Assign(sbtn.TextSettings);
  end;
end;

function EditTextViewStyled(const AButton: TTextView): Boolean;
var
  LEditor: TTextViewStyleEditor;
begin
  LEditor := TTextViewStyleEditor.Create(nil);
  with LEditor do
  begin
    try
      FStyledButton := AButton;
      CopyStyle(AButton, SourceButton);
      CopyStyle(AButton, DestButton);
      Result := ShowModal = mrOk;
    finally
      FStyledButton.ResetFocus;
      Free;
    end;
  end;
end;

procedure TTextViewStyleEditor.StyleViewclick(Sender: TObject);
var
  btn: TTextView;
  strlist: TStringList;
begin
  btn := TTextView(Sender);

  if Assigned(btn) then
  begin
    strlist := TStringList.Create;
    try
      strlist.Text := btn.Text;
      case btn.Tag of
        1: // 'cn'
          begin
            AFamily := 'CN';
            AClass := strlist[0];
          end;
        2: // 'jp'
          begin
            AFamily := 'JP';
            AClass := strlist[0];
          end;
      end;
      DestButton.SetTextViewStyle(AFamily, AClass);
      EditView1.Text := format('.SetTextViewStyle(%s,%s)', [QuotedStr(AFamily), QuotedStr(AClass)]);
    finally
      strlist.Free;
    end;
  end;

  SetClipboardTxt(EditView1.Text);
end;

procedure TTextViewStyleEditor.ApplyStyle;
begin
  SourceButton.SetTextViewStyle(AFamily, AClass);
  FStyledButton.SetTextViewStyle(AFamily, AClass);
end;

procedure TTextViewStyleEditor.ButtonView1Click(Sender: TObject);
begin
  ApplyStyle;
end;

procedure TTextViewStyleEditor.ButtonView3Click(Sender: TObject);
begin
  ApplyStyle;
end;

procedure TTextViewStyleEditor.FormDestroy(Sender: TObject);
begin
  if Assigned(CNColorJson) then
  begin
    CNColorJson.DisposeOf;
    CNColorJson := nil;
  end;
  if Assigned(JPColorJson) then
  begin
    JPColorJson.DisposeOf;
    JPColorJson := nil;
  end;
end;

procedure TTextViewStyleEditor.FormShow(Sender: TObject);
const
  ItemWidth = 100;
  ItemHeight = 48;
var
  i: Integer;
  textview: TTextView;
  colorstr: string;

  function addTextView(parent: TFmxObject): TTextView;
  begin
    Result := TTextView.Create(self);
    Result.parent := parent;
    if Assigned(Result) then
    begin
      Result.HitTest := True;
      Result.TextSettings.Gravity := TLayoutGravity.Center;
      Result.TextSettings.Font.Style := [TFontStyle.fsBold];
      Result.OnClick := StyleViewclick;
      Result.Height := ItemHeight;
      Result.Width := ItemWidth;
      Result.Background.XRadius := 5;
      Result.Background.yRadius := 5;
      Result.Background.ItemDefault.Kind := TViewBrushKind.Solid;

      Result.Margins.Rect := RectF(2, 2, 2, 2);
    end;
  end;

  procedure Setcontainerheight(parentcontro: TControl; contorl: TControl; itemcount: Integer);
  var
    coln, rown: Integer;
  begin
    coln := trunc(parentcontro.Width / (ItemWidth + 4));
    contorl.Align := TAlignLayout.None;
    contorl.Height := Max((trunc(itemcount / coln) + 1) * (ItemHeight + 4), parentcontro.Height);
    contorl.Align := TAlignLayout.top;
  end;

var
  json: TJSONObject;
begin
  GetcolorJson;
  if Assigned(CNColorJson) then
  begin
    for i := 0 to CNColorJson.Count - 1 do
    begin
      textview := addTextView(FlowLayout1);
      textview.Tag := 1;
      json := CNColorJson.Items[i] as TJSONObject;
      colorstr := json.S['bkcolor'];
      textview.Text := json.S['name'].Replace(colorstr, '') + #13 + colorstr;
      textview.TextSettings.Color.Default := HtmlToColor(json.S['fcolor']);
      textview.Background.ItemDefault.Color := HtmlToColor(colorstr);
    end;

    for i := 0 to JPColorJson.Count - 1 do
    begin
      textview := addTextView(FlowLayout2);
      textview.Tag := 2;
      json := JPColorJson.Items[i] as TJSONObject;
      colorstr := json.S['bkcolor'];
      textview.Text := json.S['name'].Replace(colorstr, '') + #13 + colorstr;
      textview.TextSettings.Color.Default := HtmlToColor(json.S['fcolor']);
      textview.Background.ItemDefault.Color := HtmlToColor(colorstr);
    end;

  end;
  Setcontainerheight(VertScrollView1, FlowLayout1, CNColorJson.Count);
  Setcontainerheight(VertScrollView2, FlowLayout2, JPColorJson.Count);
  FlowLayout2.RecalcUpdateRect;
end;

procedure TTextViewStyleEditor.GetcolorJson;
begin
  GetJson(CNColorJson, CNColorJsonTxt);
  GetJson(JPColorJson, JPColorJsonTxt);
end;

procedure TTextViewStyleEditor.TextViewFamilyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  StartWindowDrag;
end;

procedure TTextViewStyleEditor.ViewMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  StartWindowDrag;
end;

end.

