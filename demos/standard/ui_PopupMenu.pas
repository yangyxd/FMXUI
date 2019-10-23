unit ui_PopupMenu;

interface

uses
  UI.Dialog,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Base, UI.Standard;

type
  TMainPopupMenu = class(TFrame)
    LinearLayout1: TLinearLayout;
    TextView1: TTextView;
    TextView2: TTextView;
    procedure TextView2Click(Sender: TObject);
    procedure TextView1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  ui_CustomListview, UI.Frame;

procedure TMainPopupMenu.TextView1Click(Sender: TObject);
begin
  TCustomListview.ShowFrame(Self, '', TFrameAniType.MoveInOut);
end;

procedure TMainPopupMenu.TextView2Click(Sender: TObject);
begin
  TDialog.CloseDialog(Self);
  Application.Terminate;
end;

end.
