unit uFrameDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Base, FMX.Controls.Presentation, UI.Standard, FMX.Layouts,
  System.ImageList, FMX.ImgList, FMX.Menus, UI.ListView;

type
  TFrmaeDialog = class(TFrame)
    LinearLayout1: TLinearLayout;
    SpeedButton1: TSpeedButton;
    tvTitle: TTextView;
    VertScrollBox1: TVertScrollBox;
    LinearLayout2: TLinearLayout;
    ButtonView1: TButtonView;
    ImageList1: TImageList;
    ButtonView2: TButtonView;
    ButtonView3: TButtonView;
    ButtonView4: TButtonView;
    ButtonView5: TButtonView;
    ButtonView6: TButtonView;
    ButtonView7: TButtonView;
    ButtonView8: TButtonView;
    ButtonView9: TButtonView;
    ButtonView11: TButtonView;
    ButtonView10: TButtonView;
    ButtonView12: TButtonView;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    procedure SpeedButton1Click(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
    procedure ButtonView3Click(Sender: TObject);
    procedure ButtonView4Click(Sender: TObject);
    procedure ButtonView5Click(Sender: TObject);
    procedure ButtonView6Click(Sender: TObject);
    procedure ButtonView7Click(Sender: TObject);
    procedure ButtonView8Click(Sender: TObject);
    procedure ButtonView9Click(Sender: TObject);
    procedure ButtonView11Click(Sender: TObject);
    procedure ButtonView10Click(Sender: TObject);
    procedure ButtonView12Click(Sender: TObject);
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
  uFrameListViewTest,
  UI.Dialog, UI.Async, uFrameDialog_CustomView;

{ TFrmaeDialog }

procedure TFrmaeDialog.ButtonView10Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('我是标题文本')
    .Show;
end;

procedure TFrmaeDialog.ButtonView11Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('我是标题文本')
    .SetSingleChoiceItems(
      [
      '列表项 - 1',
      '列表项 - 2',
      '列表项 - 3',
      '列表项 - 4',
      '列表项 - 1',
      '列表项 - 2',
      '列表项 - 3',
      '列表项 - 4',
      '列表项 - 1',
      '列表项 - 2',
      '列表项 - 3',
      '列表项 - 4',
      '列表项 - 1',
      '列表项 - 2',
      '列表项 - 3',
      '列表项 - 4',
      '列表项 - 1',
      '列表项 - 2',
      '列表项 - 3',
      '列表项 - 4',
      '列表项 - 5'
    ], 1)
    .SetPositiveButton('取消')
    .SetNegativeButton('确定',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint('选择了: ' + Dialog.Builder.ItemArray[Dialog.Builder.CheckedItem]);
      end
    )
    .Show;
end;

procedure TFrmaeDialog.ButtonView12Click(Sender: TObject);
var
  View: TFrameDialogCustomView;
begin
  View := TFrameDialogCustomView.Create(Self);
  TDialogBuilder.Create(Self)
    .SetTitle('登录')
    .SetView(View)
    .Show;
end;

procedure TFrmaeDialog.ButtonView1Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetMessage('我是一个消息框。')
    .Show;
end;

procedure TFrmaeDialog.ButtonView2Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetMessage('我是一个消息框。这里显示消息内容')
    .SetNegativeButton('Negative',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.NegativeButtonText);
      end
    )
    .SetNeutralButton('Neutral',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.NeutralButtonText);
      end
    )
    .SetPositiveButton('Positive',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.PositiveButtonText);
      end
    )
    .Show;
end;

procedure TFrmaeDialog.ButtonView3Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('我是标题文本')
    .SetMessage('我是一个消息框。这里显示消息内容')
    .SetNegativeButton('Negative',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.NegativeButtonText);
      end
    )
    .SetPositiveButton('Positive',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.PositiveButtonText);
      end
    )
    .Show;
end;

procedure TFrmaeDialog.ButtonView4Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('我是标题文本')
    .SetItems(['列表项 - 1', '列表项 - 2', '列表项 - 3', '列表项 - 4', '列表项 - 5'],
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.ItemArray[Which]);
      end
    )
    .Show;
end;

procedure TFrmaeDialog.ButtonView5Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('我是标题文本')
    .SetSingleChoiceItems(['列表项 - 1', '列表项 - 2', '列表项 - 3', '列表项 - 4', '列表项 - 5'], 1)
    .SetPositiveButton('取消')
    .SetNegativeButton('确定',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint('选择了: ' + Dialog.Builder.ItemArray[Dialog.Builder.CheckedItem]);
      end
    )
    .Show;
end;

procedure TFrmaeDialog.ButtonView6Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('我是标题文本')
    .SetMultiChoiceItems(['列表项 - 1', '列表项 - 2', '列表项 - 3', '列表项 - 4', '列表项 - 5'], [])
    .SetPositiveButton('取消')
    .SetNegativeButton('确定',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Format('选择了 %d 项.', [Dialog.Builder.CheckedCount]));
      end
    )
    .Show;
end;

procedure TFrmaeDialog.ButtonView7Click(Sender: TObject);
begin
  ShowWaitDialog('正在执行任务...', False);
  TAsync.Create()
  .SetExecute(
    procedure (Async: TAsync) begin
      Sleep(3000);
    end
  )
  .SetExecuteComplete(
    procedure (Async: TAsync) begin
      HideWaitDialog;
    end
  ).Start;
end;

procedure TFrmaeDialog.ButtonView8Click(Sender: TObject);
begin
  ShowWaitDialog('正在执行任务...',
    procedure (Dialog: IDialog) begin
      Hint('任务被取消');
    end
  );
  TAsync.Create()
  .SetExecute(
    procedure (Async: TAsync) begin
      Sleep(5000);
    end
  )
  .SetExecuteComplete(
    procedure (Async: TAsync) begin
      if not IsWaitDismiss then // 如果任务没有被中断
        Hint('任务执行完成.');
      HideWaitDialog;
    end
  ).Start;
end;

procedure TFrmaeDialog.ButtonView9Click(Sender: TObject);
begin
  StartFrame(TFrameListViewTest);
end;

procedure TFrmaeDialog.DoShow;
begin
  inherited;
  tvTitle.Text := Title;
end;

procedure TFrmaeDialog.SpeedButton1Click(Sender: TObject);
begin
  Finish(TFrameAniType.FadeInOut);
end;

end.
