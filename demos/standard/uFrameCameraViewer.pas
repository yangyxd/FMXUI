unit uFrameCameraViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.Frame;

type
  TFrameCameraViewer = class(TFrame)
    LinearLayout1: TLinearLayout;
    btnBack: TTextView;
    tvTitle: TTextView;
    CameraViewer1: TCameraViewer;
    procedure btnBackClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameCameraViewer.btnBackClick(Sender: TObject);
begin
  Finish;
end;

end.
