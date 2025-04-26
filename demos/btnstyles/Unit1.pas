unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  UI.Standard, FMX.Controls.Presentation, FMX.StdCtrls, UI.Calendar;

type
  TForm2 = class(TForm)
    ButtonView1: TButtonView;
    ButtonView2: TButtonView;
    ButtonView3: TButtonView;
    ButtonView4: TButtonView;
    ButtonView5: TButtonView;
    ButtonView6: TButtonView;
    ButtonView7: TButtonView;
    ButtonView8: TButtonView;
    ButtonView9: TButtonView;
    ButtonView10: TButtonView;
    ButtonView11: TButtonView;
    ButtonView12: TButtonView;
    ButtonView13: TButtonView;
    ButtonView14: TButtonView;
    ButtonView15: TButtonView;
    CheckBox1: TCheckBox;
    TextView1: TTextView;
    TextView2: TTextView;
    StyleViewManager1: TStyleViewManager;
    ButtonView16: TButtonView;
    procedure ButtonView1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ButtonView16Click(Sender: TObject);
  private
    { Private declarations }
    FStyles: TStyleViewManager;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.ButtonView16Click(Sender: TObject);
begin
  FreeAndNil(FStyles);
end;

procedure TForm2.ButtonView1Click(Sender: TObject);
begin
  if not Assigned(FStyles) then
    FStyles := TStyleViewManager.Create(Self);
  ButtonView2.StyleManager := FStyles;
  ButtonView3.StyleManager := FStyles;
  ButtonView4.StyleManager := FStyles;
  ButtonView5.StyleManager := FStyles;
  ButtonView6.StyleManager := FStyles;
  ButtonView7.StyleManager := FStyles;
  ButtonView8.StyleManager := FStyles;
  ButtonView9.StyleManager := FStyles;
  ButtonView10.StyleManager := FStyles;
  ButtonView11.StyleManager := FStyles;
  ButtonView12.StyleManager := FStyles;
  ButtonView13.StyleManager := FStyles;
  ButtonView14.StyleManager := FStyles;
  ButtonView15.StyleManager := FStyles;
end;

procedure TForm2.CheckBox1Change(Sender: TObject);
begin
  ButtonView2.Enabled := CheckBox1.IsChecked;
  ButtonView3.Enabled := ButtonView2.Enabled;
  ButtonView4.Enabled := ButtonView2.Enabled;
  ButtonView5.Enabled := ButtonView2.Enabled;
  ButtonView6.Enabled := ButtonView2.Enabled;
  ButtonView7.Enabled := ButtonView2.Enabled;
  ButtonView8.Enabled := ButtonView2.Enabled;
  ButtonView9.Enabled := ButtonView2.Enabled;
  ButtonView10.Enabled := ButtonView2.Enabled;
  ButtonView11.Enabled := ButtonView2.Enabled;
  ButtonView12.Enabled := ButtonView2.Enabled;
  ButtonView13.Enabled := ButtonView2.Enabled;
  ButtonView14.Enabled := ButtonView2.Enabled;
  ButtonView15.Enabled := ButtonView2.Enabled;
end;

end.
