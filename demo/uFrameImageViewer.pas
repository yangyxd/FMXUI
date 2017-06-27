unit uFrameImageViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, UI.Frame,
  UI.Standard, UI.Base;

type
  TFrameImageViewer = class(TFrame)
    LinearLayout1: TLinearLayout;
    TextView17: TTextView;
    tvTitle: TTextView;
    ImageViewerEx1: TImageViewerEx;
    procedure TextView17Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameImageViewer.TextView17Click(Sender: TObject);
begin
  Finish;
end;

end.
