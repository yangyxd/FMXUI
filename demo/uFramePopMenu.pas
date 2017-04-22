unit uFramePopMenu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,System.Generics.Collections,
  UI.Standard, UI.ListView, FMX.Types, FMX.Controls, ui_CustomListView,
  FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, UI.Base, UI.Frame;

type
  TFramePopMenu = class(TFrame)
    View1: TView;
    ImageView1: TImageView;
    View2: TView;
    View3: TView;
    ButtonView1: TButtonView;
    ButtonView2: TButtonView;
    TextView1: TTextView;
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure DoCreate(); override;
    procedure DoFree(); override;
    procedure DoShow(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses UI.Dialog, FMX.Forms;

procedure TFramePopMenu.ButtonView1Click(Sender: TObject);
begin
  TDialog.CloseDialog(self) ;
  Hint('Done');
end;

procedure TFramePopMenu.ButtonView2Click(Sender: TObject);
begin
  TDialog.CloseDialog(Self);
  Application.Terminate;
end;

procedure TFramePopMenu.DoCreate;
begin
  inherited;
end;

procedure TFramePopMenu.DoFree;
begin
  inherited;
end;

procedure TFramePopMenu.DoShow;
begin
  inherited;
end;

end.
