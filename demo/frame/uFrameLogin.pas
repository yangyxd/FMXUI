unit uFrameLogin;

interface

uses
  FMX.InertialMovement,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Base, FMX.Controls.Presentation, UI.Standard, FMX.Layouts,
  System.ImageList, FMX.ImgList, UI.Edit, UI.ListView, FMX.Edit;

type
  TFrmaeLogin = class(TFrame)
    VertScrollBox1: TVertScrollBox;
    ImageList1: TImageList;
    LinearLayout3: TLinearLayout;
    edtUser: TEditView;
    edtPwd: TEditView;
    ButtonView1: TButtonView;
    LinearLayout1: TLinearLayout;
    TextView1: TTextView;
    tvTitle: TTextView;
    ButtonView2: TButtonView;
    EditView1: TEditView;
    Edit1: TEdit;
    EditButton1: TEditButton;
    procedure ButtonView1Click(Sender: TObject);
    procedure TextView1Click(Sender: TObject);
    //procedure ListViewEx1ScrollChange(Sender: TObject);
    procedure TextView3Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
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
  UI.Dialog, UI.Async, uFrameMain, uFrameListViewTest;

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
        Sleep(100);
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

procedure TFrmaeLogin.ButtonView2Click(Sender: TObject);
begin
  StartFrame(TFrameListViewTest, 'ListView 测试');
end;

procedure TFrmaeLogin.DoShow;
begin
  inherited;
  tvTitle.Text := Title;
end;

//procedure TFrmaeLogin.ListViewEx1ScrollChange(Sender: TObject);
//var
//  Targets: array of TAniCalculations.TTarget;
//
//  function GetTargetsValue(Index: Integer): Double;
//  begin
//    if Index < Length(Targets) then
//      Result := Targets[Index].Point.Y
//    else
//      Result := 0
//  end;
//
//begin
//  SetLength(Targets, ListViewEx1.AniCalculations.TargetCount);
//  ListViewEx1.AniCalculations.GetTargets(Targets);
//
//  TextView2.Text := Format(
//    'ScrollValue: %.3f, ' +
//    'VScrollBar: %.3f, ' +
//    'MouseTX: %.3f, %.3f, ' +
//    'TargetCount: %d, ' +
//    'Target0: %.3f, ' +
//    'Target1: %.3f, ' +
//    'Target2: %.3f, ' +
//    'DownPoint: %.3f, ' +
//    'Elasticity: %.3f, ' +
//    'BoundsAnimation: %d, ' +
//    'Animation: %d',
//  [
//    ListViewEx1.VScrollBarValue,
//    ListViewEx1.VScrollBar.Value,
//    ListViewEx1.AniCalculations.MouseTarget.Point.X,
//    ListViewEx1.AniCalculations.MouseTarget.Point.Y,
//    Length(Targets),
//    GetTargetsValue(0),
//    GetTargetsValue(1),
//    GetTargetsValue(2),
//    ListViewEx1.AniCalculations.DownPoint.Y,
//    ListViewEx1.AniCalculations.Elasticity,
//    Abs(Ord(ListViewEx1.AniCalculations.BoundsAnimation)),
//    Abs(Ord(ListViewEx1.AniCalculations.Animation))
//  ])
//end;

procedure TFrmaeLogin.TextView1Click(Sender: TObject);
begin
  Finish;
end;

procedure TFrmaeLogin.TextView3Click(Sender: TObject);
begin
  StartFrame(TFrameMain, '欢迎使用 FMXUI');
end;

end.
