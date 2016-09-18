unit uFrameLogin;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Base, FMX.Controls.Presentation, UI.Standard, FMX.Layouts,
  System.ImageList, FMX.ImgList, UI.Edit;

type
  TFrmaeLogin = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    VertScrollBox1: TVertScrollBox;
    LinearLayout2: TLinearLayout;
    ButtonView1: TButtonView;
    ImageList1: TImageList;
    TextView1: TTextView;
    edtUser: TEditView;
    edtPwd: TEditView;
    procedure ButtonView1Click(Sender: TObject);
  private
    { Private declarations }
  protected
    // 显示事件
    procedure DoShow(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  UI.Dialog, UI.Async, uFrameMain;

{ TFrmaeDialog }

procedure TFrmaeLogin.ButtonView1Click(Sender: TObject);
begin
  if edtUser.Text = '' then begin
    Hint('请输入用户名');
    Exit;
  end;

  if edtPwd.Text = '' then begin
    Hint('请输入用户名');
    Exit;
  end;

  ShowWaitDialog('正在登录, 请稍等...', False);
  TAsync.Create()
    .SetExecute(
      procedure (Async: TAsync) begin
        Sleep(1000);
      end
    )
    .SetExecuteComplete(
      procedure (Async: TAsync) begin
        HideWaitDialog();
        StartFrame(TFrameMain, '欢迎使用 FMXUI');
        Finish();
      end
    ).Start;
end;

procedure TFrmaeLogin.DoShow;
begin
  inherited;
  tvTitle.Text := Title;
end;

end.
