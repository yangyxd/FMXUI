unit uFrame2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.Frame;

type
  TFrame2 = class(TFrame)
    GridsLayout1: TGridsLayout;
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
    TextView11: TTextView;
    TextView12: TTextView;
    TextView13: TTextView;
    TextView14: TTextView;
    TextView15: TTextView;
    TextView16: TTextView;
    LinearLayout1: TLinearLayout;
    TextView17: TTextView;
    tvTitle: TTextView;
    procedure TextView17Click(Sender: TObject);
    procedure GridsLayout1Resize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrame2.GridsLayout1Resize(Sender: TObject);
begin
  TextView16.Text := Format('%.2f', [GridsLayout1.Width])
end;

procedure TFrame2.TextView17Click(Sender: TObject);
begin
  Finish();
end;

end.
