{*******************************************************}
{                                                       }
{       FMX UI 标准组件ButtonView的拓展                 }
{             修改buttonview的外观样式                  }
{              (兼容textview)                           }
{         版权所有 (C) 2023 dqi1999                     }
{                                                       }
{*******************************************************}

unit UI.ButtonViewEditor;

interface

uses
  FMX.Controls, FMX.Controls.Presentation, FMX.Dialogs, FMX.Forms, FMX.Graphics,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.StdCtrls, FMX.TabControl, FMX.Types,
  FMX.Platform, System.Classes, System.SysUtils, System.Types, System.UITypes,
  System.Variants, UI.Base, UI.Standard, UI.Edit, UI.ButtonViewstyle;

type
  TButtonViewStyleEditor = class(TForm)
    TextViewFamily: TTextView;
    View2: TView;
    View1: TView;
    Rectangle2: TRectangle;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Rectangle3: TRectangle;
    View3: TView;
    View4: TView;
    View5: TView;
    View6: TView;
    Rectangle1: TRectangle;
    View7: TView;
    View8: TView;
    View9: TView;
    View10: TView;
    EditView1: TEditView;
    TextView1: TTextView;
    TextView2: TTextView;
    TextView3: TTextView;
    TextView4: TTextView;
    TextView5: TTextView;
    TextView6: TTextView;
    TextView7: TTextView;
    TextView8: TTextView;
    TextView9: TTextView;
    TextView10: TTextView;
    ViewStyle: TView;
    SourceButton: TButtonView;
    View11: TView;
    ButtonView1: TButtonView;
    ButtonView2: TButtonView;
    ButtonView3: TButtonView;
    ViewMain: TView;
    GridPanelLayout1: TGridPanelLayout;
    DestButton: TButtonView;
    procedure FormShow(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure ButtonView3Click(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
    procedure TextViewFamilyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ViewMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }
    FStyledButton: TTextView;
    AFamily, AClass, AAppearance: string;
    procedure Stylebtnclick(Sender: TObject);
    procedure ApplyStyle;
  public
    { Public declarations }
  end;

function EditStyledButton(const AButton: TTextView): Boolean;

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
    dbtn.TextSettings.Assign(sbtn.TextSettings);
end;

function EditStyledButton(const AButton: TTextView): Boolean;
var
  LEditor: TButtonViewStyleEditor;
begin
  LEditor := TButtonViewStyleEditor.Create(nil);
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

procedure TButtonViewStyleEditor.Stylebtnclick(Sender: TObject);
var
  btn: TTextView;
  kindidx: integer;
  classidx: integer;
begin
  btn := TTextView(Sender);

  if Assigned(btn) then
  begin
    kindidx := btn.Tag div 10;
    classidx := btn.Tag mod 10;
    case kindidx of
      1, 2:
        begin
          AFamily := Ttextviewstyle.GetFAMILYI(0);
          AClass := btn.Text;
          AAppearance := Ttextviewstyle.GetAAppearance(AFamily, kindidx - 1);
        end;
      3..6:
        begin
          AFamily := Ttextviewstyle.GetFAMILYI(1);
          AClass := btn.Text;
          AAppearance := Ttextviewstyle.GetAAppearance(AFamily, kindidx - 3);
        end;
      7..10:
        begin
          AFamily := Ttextviewstyle.GetFAMILYI(2);
          AClass := btn.Text;
          AAppearance := Ttextviewstyle.GetAAppearance(AFamily, kindidx - 7);
        end;
    end;
  end;
  EditView1.Text := format('%s.SetButtonStyle(%s,%s,%s)', [btn.Name, QuotedStr(AFamily), QuotedStr(AClass), QuotedStr(AAppearance)]);
  DestButton.SetButtonStyle(AFamily, AClass, AAppearance);
  SetClipboardTxt(EditView1.Text);
end;

procedure TButtonViewStyleEditor.ApplyStyle;
begin
  SourceButton.SetButtonStyle(AFamily, AClass, AAppearance);
  FStyledButton.SetButtonStyle(AFamily, AClass, AAppearance);
end;

procedure TButtonViewStyleEditor.ButtonView1Click(Sender: TObject);
begin
  ApplyStyle;
end;

procedure TButtonViewStyleEditor.ButtonView3Click(Sender: TObject);
begin
  ApplyStyle;
end;

procedure TButtonViewStyleEditor.FormShow(Sender: TObject);
var
  i, j, k: integer;
  view: TView;
  textview: TTextView;
  button: TTextView;
  outline: Boolean;
  C1, c2: TAlphaColor;
  FAMILYary: TArray<string>;
  classsArr: TArray<string>;
  AAppearanceArr: TArray<string>;
  viewidx: integer;

  function addbutton(parent: TFmxObject): TTextView;
  begin
    Result := TTextView.Create(self);
    Result.parent := parent;
    if Assigned(Result) then
    begin
      Result.Position.Y := 999;
      Result.HitTest := True;
      Result.Align := TAlignLayout.Top;
      Result.TextSettings.Gravity := TLayoutGravity.Center;
      Result.OnClick := Stylebtnclick;
      Result.Height := 32;

      Result.Margins.Top := 5;
    end;
  end;

begin
  viewidx := 0;
  FAMILYary := Ttextviewstyle.GetFAMILYs;

  for i := 0 to High(FAMILYary) do
  begin

    if Assigned(view) then
    begin
      AAppearanceArr := Ttextviewstyle.GetFAMILYAAppearance(FAMILYary[i]);
      classsArr := Ttextviewstyle.GetAllClassFromFAMILYidx(i);
      for j := 0 to High(AAppearanceArr) do
      begin
        inc(viewidx);
        view := TView(self.FindComponent('view' + viewidx.ToString));
        textview := TTextView(self.FindComponent('textview' + viewidx.ToString));
        textview.Text := AAppearanceArr[j];
        for k := 0 to High(classsArr) do
        begin
          button := addbutton(view);
          button.Tag := 10 * viewidx + k;
          button.Text := classsArr[k];
          button.SetButtonStyle(FAMILYary[i], classsArr[k], AAppearanceArr[j]);
        end;
      end;
    end;
  end;
end;

procedure TButtonViewStyleEditor.TabControl1Change(Sender: TObject);
begin
  TextViewFamily.Text := Ttextviewstyle.GetFAMILYI(TabControl1.ActiveTab.Tag);
end;

procedure TButtonViewStyleEditor.TextViewFamilyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  StartWindowDrag;
end;

procedure TButtonViewStyleEditor.ViewMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  StartWindowDrag;
end;

end.

