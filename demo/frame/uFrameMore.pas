unit uFrameMore;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, UI.Standard, FMX.Layouts, UI.Base, UI.Frame;

type
  TFrameMore = class(TFrame)
    LinearLayout1: TLinearLayout;
    TextView1: TTextView;
    tvTitle: TTextView;
    VertScrollBox1: TVertScrollBox;
    LinearLayout2: TLinearLayout;
    ImageList1: TImageList;
    TextView2: TTextView;
    procedure TextView1Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure DoShow(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameMore.DoShow;
begin
  inherited;
  tvTitle.Text := Title;
end;

procedure TFrameMore.TextView1Click(Sender: TObject);
begin
  Finish();
end;

end.
