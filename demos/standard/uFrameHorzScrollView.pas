unit uFrameHorzScrollView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.Frame;

type
  TFrameHorzScrollView = class(TFrame)
    LinearLayout1: TLinearLayout;
    btnBack: TTextView;
    tvTitle: TTextView;
    HorzScrollView1: THorzScrollView;
    TextView1: TTextView;
    procedure btnBackClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameHorzScrollView.btnBackClick(Sender: TObject);
begin
  Finish;
end;

end.
