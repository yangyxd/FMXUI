unit uFrameImageViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, UI.Frame,
  UI.Standard, UI.Base;

type
  TFrameImageViewer = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    ImageViewerEx1: TImageViewerEx;
    btnBack: TTextView;
    procedure btnBackClick(Sender: TObject);
    procedure ImageViewerEx1Zoom(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameImageViewer.btnBackClick(Sender: TObject);
begin
  Finish;
end;

procedure TFrameImageViewer.ImageViewerEx1Zoom(Sender: TObject);
begin
  Hint('%d%%', [ImageViewerEx1.Zoom]);
end;

end.
