unit uFrameDialog_CustomView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Edit, UI.Base, UI.Standard, UI.Frame;

type
  TLoginEvent = procedure (AView: TFrame; AUserName, APassword: string) of object;

  TFrameDialogCustomView = class(TFrame)
    LinearLayout1: TLinearLayout;
    EditView1: TEditView;
    EditView2: TEditView;
    ButtonView1: TButtonView;
    TextView1: TTextView;
    TextView2: TTextView;
    procedure ButtonView1Click(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure TextView1Click(Sender: TObject);
    procedure TextView2Click(Sender: TObject);
    procedure EditView1Change(Sender: TObject);
  private
    { Private declarations }
    FOnLogin: TLoginEvent;
  public
    { Public declarations }
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
  end;

implementation

{$R *.fmx}

uses
  UI.Dialog;

procedure TFrameDialogCustomView.ButtonView1Click(Sender: TObject);
var
  LName, LPassword: string;
begin
  if EditView1.Text = '' then begin
    Hint('请输入用户名');
    Exit;
  end;
  if EditView2.Text = '' then begin
    Hint('请输入密码');
    Exit;
  end;

  LName := EditView1.Text;
  LPassword := EditView2.Text;
  TDialog.CloseDialog(Self);
  if Assigned(OnLogin) then
    OnLogin(Self, LName, LPassword);
end;

procedure TFrameDialogCustomView.EditView1Change(Sender: TObject);
begin
  TextView2.Visible := EditView1.Text <> '';
end;

procedure TFrameDialogCustomView.FrameResize(Sender: TObject);
begin
  Height := LinearLayout1.Height;
end;

procedure TFrameDialogCustomView.TextView1Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
      .SetSingleChoiceItems(['User1', 'User2', 'User3', 'User4', 'User5'], EditView1.Tag,
        procedure (Dialog: IDialog; Which: Integer)
        begin
          EditView1.Tag := Dialog.Builder.CheckedItem;
          EditView1.Text := Dialog.Builder.ItemArray[Dialog.Builder.CheckedItem];
          Dialog.AsyncDismiss;
        end
      )
      //.SetWidth(160)
      //.SetMaxHeight(320)
      .SetDownPopup(EditView1, 0, 0, TLayoutGravity.LeftBottom)
      .SetListItemDefaultHeight(30)
      .Show;
end;

procedure TFrameDialogCustomView.TextView2Click(Sender: TObject);
begin
  EditView1.Text := '';
end;

end.
