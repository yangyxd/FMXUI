unit uFrameDialog_CustomView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Edit, UI.Base, UI.Standard, UI.Frame;

type
  TFrameDialogCustomView = class(TFrame)
    LinearLayout1: TLinearLayout;
    EditView1: TEditView;
    EditView2: TEditView;
    ButtonView1: TButtonView;
    procedure ButtonView1Click(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  UI.Dialog;

procedure TFrameDialogCustomView.ButtonView1Click(Sender: TObject);
begin
  if EditView1.Text = '' then begin
    Hint('请输入用户名');
    Exit;
  end;
  if EditView2.Text = '' then begin
    Hint('请输入密码');
    Exit;
  end;
  TDialog.CloseDialog(Self);
end;

procedure TFrameDialogCustomView.FrameResize(Sender: TObject);
begin
  Height := LinearLayout1.Height;
end;

end.
