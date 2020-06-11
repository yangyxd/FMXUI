unit FrameRelativeLayoutFMXControls;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects,
  UI.Standard, UI.Base, UI.Frame;

type
  TFrameRelativeFMX = class(TFrame)
    RelativeLayout1: TRelativeLayout;
    rctCenter: TRectangle;
    rctLeft: TRectangle;
    rctRight: TRectangle;
    rctTop: TRectangle;
    rctBottom: TRectangle;
    rctAkLeft: TRectangle;
    rctAkRight: TRectangle;
    rctAkTop: TRectangle;
    rctAkBottom: TRectangle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
