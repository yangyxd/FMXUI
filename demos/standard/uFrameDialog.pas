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
    tvTitle: TTextView;
    VertScrollBox1: TVertScrollBox;
    LinearLayout2: TLinearLayout;
    ButtonView1: TButtonView;
    ButtonView2: TButtonView;
    ButtonView3: TButtonView;
    ButtonView4: TButtonView;
    ButtonView5: TButtonView;
    ButtonView6: TButtonView;
    ButtonView7: TButtonView;
    ButtonView8: TButtonView;
    ButtonView11: TButtonView;
    ButtonView10: TButtonView;
    ButtonView12: TButtonView;
    btnBack: TTextView;
    ButtonView9: TButtonView;
    ButtonView13: TButtonView;
    ButtonView14: TButtonView;
    ButtonView15: TButtonView;
    ButtonView16: TButtonView;
    ButtonView17: TButtonView;
    ButtonView18: TButtonView;
    ButtonView19: TButtonView;
    ButtonView20: TButtonView;
    ButtonView21: TButtonView;
    ButtonView22: TButtonView;
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
    procedure ButtonView3Click(Sender: TObject);
    procedure ButtonView4Click(Sender: TObject);
    procedure ButtonView5Click(Sender: TObject);
    procedure ButtonView6Click(Sender: TObject);
    procedure ButtonView7Click(Sender: TObject);
    procedure ButtonView8Click(Sender: TObject);
    procedure ButtonView11Click(Sender: TObject);
    procedure ButtonView10Click(Sender: TObject);
    procedure ButtonView12Click(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure ButtonView9Click(Sender: TObject);
    procedure ButtonView13Click(Sender: TObject);
    procedure ButtonView14Click(Sender: TObject);
    procedure ButtonView15Click(Sender: TObject);
    procedure ButtonView16Click(Sender: TObject);
    procedure ButtonView17Click(Sender: TObject);
    procedure ButtonView18Click(Sender: TObject);
    procedure ButtonView19Click(Sender: TObject);
    procedure ButtonView20Click(Sender: TObject);
    procedure ButtonView21Click(Sender: TObject);
    procedure ButtonView22Click(Sender: TObject);
  private
    { Private declarations }
  protected
    // 初始事件
    procedure DoCreate(); override;
    // 释放事件
    procedure DoFree(); override;
    // 显示事件
    procedure DoShow(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  uFrameListViewTest,
  ui_PopupMenu,
  UI.Dialog, UI.Async, uFrameDialog_CustomView, uFrameDialog_CustomViewVertical;

type
  /// <summary>
  /// 单选列表适配器
  /// </summary>
  TStringsListAdapter = class(UI.ListView.TStringsListAdapter)
  protected
    function GetView(const Index: Integer; ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
  end;

var
  IosStyleManager: TDialogStyleManager;
  IosTitleStyleManager: TDialogStyleManager;

{ TFrmaeDialog }

procedure TFrmaeDialog.btnBackClick(Sender: TObject);
begin
  Finish();
end;

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
      '列表项 - 5',
      '列表项 - 6',
      '列表项 - 7',
      '列表项 - 8',
      '列表项 - 9',
      '列表项 - 10',
      '列表项 - 11',
      '列表项 - 12',
      '列表项 - 13',
      '列表项 - 14',
      '列表项 - 15',
      '列表项 - 16',
      '列表项 - 17',
      '列表项 - 18',
      '列表项 - 19',
      '列表项 - 20',
      '列表项 - 21'
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

procedure TFrmaeDialog.ButtonView13Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
      .SetSingleChoiceItems(['Item 1', 'Item 2', 'Item 3', 'Item 4', 'Item 5'], 0,
        procedure (Dialog: IDialog; Which: Integer)
        begin
          Dialog.AsyncDismiss;
        end
      )
      //.SetWidth(160)
      //.SetMaxHeight(320)
      .SetDownPopup(TView(Sender), 0, 0, TLayoutGravity.LeftBottom)
      .SetListItemDefaultHeight(30)
      .Show;
end;

procedure TFrmaeDialog.ButtonView14Click(Sender: TObject);
begin
  TDialog.ShowView(Self, TView(Sender), TMainPopupMenu, 0, 0, TDialogViewPosition.Bottom);
end;

procedure TFrmaeDialog.ButtonView15Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetPosition(TDialogViewPosition.Top)
    .SetMessage('我是一个位于顶部的消息框。')
    .Show;
end;

procedure TFrmaeDialog.ButtonView16Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetPosition(TDialogViewPosition.Bottom)
    .SetMessage('我是一个位于底部的消息框。这里显示消息内容')
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

procedure TFrmaeDialog.ButtonView17Click(Sender: TObject);
var
  View: TFrameDialogCustomViewVertical;
begin
  View := TFrameDialogCustomViewVertical.Create(Self);
  TDialogBuilder.Create(Self)
    .SetView(View)
    .SetWidth(50)
    .SetPosition(TDialogViewPosition.Left)
    .SetMessage('我是一个位于左侧的消息框。')
    .Show;
end;

procedure TFrmaeDialog.ButtonView18Click(Sender: TObject);
var
  View: TFrameDialogCustomViewVertical;
begin
  View := TFrameDialogCustomViewVertical.Create(Self);
  TDialogBuilder.Create(Self)
    .SetView(View)
    .SetWidth(50)
    .SetPosition(TDialogViewPosition.Right)
    .SetMessage('我是一个位于右侧的消息框。')
    .Show;
end;

procedure TFrmaeDialog.ButtonView19Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetStyleManager(IosStyleManager)
    .SetMessage('我是一个消息框。这里显示消息内容')
    .SetNegativeButton('Negative',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.NegativeButtonText);
      end
    )
    .SetNegativeButtonStyle(TAlphaColors.Red, [TFontStyle.fsBold])
    .SetPositiveButton('Positive',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.PositiveButtonText);
      end
    )
    .Show;
end;

procedure TFrmaeDialog.ButtonView1Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetMessage('我是一个消息框。')
    .Show;
end;

procedure TFrmaeDialog.ButtonView20Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetStyleManager(IosTitleStyleManager)
    .SetPosition(TDialogViewPosition.Bottom)
    .SetTitle('我是标题文本')
    .SetMessage('我是一个消息框。这里显示<b><font color="#000080">消息内容</font></b>', True)
    .SetItems(['列表项 - 1', '列表项 - 2', '列表项 - 3', '列表项 - 4', '列表项 - 5'],
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.ItemArray[Which]);
      end
    )
    .SetOnInitListAdapterA(
      procedure (Dialog: IDialog; Builder: TDialogBuilder; var Adapter: IListAdapter) begin
        Adapter := TStringsListAdapter.Create(Builder.ItemArray);
        TDialog(Dialog).RootView.ListView.ShowScrollBars := False;
      end
    )
    .SetCancelButton('Cancel',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.CancelButtonText);
      end
    )
    .Show;
end;

procedure TFrmaeDialog.ButtonView21Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetStyleManager(IosTitleStyleManager)
    .SetTitle('我是标题文本')
    .SetMessage('我是一个消息框。这里显示消息内容')
    .SetNegativeButton('Negative',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.NegativeButtonText);
      end
    )
    .SetNegativeButtonStyle(TAlphaColors.Red, [TFontStyle.fsBold])
    .SetPositiveButton('Positive',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.PositiveButtonText);
      end
    )
    .Show;
end;

procedure TFrmaeDialog.ButtonView22Click(Sender: TObject);
var
  View: TFrameDialogCustomView;
begin
  View := TFrameDialogCustomView.Create(Self);
  TDialogBuilder.Create(Self)
    .SetStyleManager(IosStyleManager)
    .SetView(View)
    .SetPosition(TDialogViewPosition.Bottom)
    .SetCancelButton('Cancel',
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.CancelButtonText);
      end
    )
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
  TDialogBuilder.Create(Self)
    .SetTitle('多列列表')
    .SetItems(['列表项 - 1', '列表项 - 2', '列表项 - 3', '列表项 - 4', '列表项 - 5'],
      procedure (Dialog: IDialog; Which: Integer) begin
        Hint(Dialog.Builder.ItemArray[Which]);
      end
    )
    .SetOnInitListAdapterA(
      procedure (Dialog: IDialog; Builder: TDialogBuilder; var Adapter: IListAdapter)
      begin
        TDialogView(Dialog.ViewRoot).ListView.ColumnCount := 3;
      end
    )
    .Show;
end;

procedure TFrmaeDialog.DoCreate;
begin
  inherited;
  IosStyleManager := TDialogStyleManager.Create(nil);
  IosStyleManager.BackgroundRadius := 15;
  IosStyleManager.MessageTextMargins.Top := 15;
  IosStyleManager.MessageTextMargins.Left := 10;
  IosStyleManager.MessageTextMargins.Right := 10;
  IosStyleManager.MessageTextMargins.Bottom := 15;
  IosStyleManager.MessageTextColor := $FF030303;
  IosStyleManager.MessageTextGravity := TLayoutGravity.Center;
  IosStyleManager.ButtonTextColor.Default := $FF0D69FF;
  IosStyleManager.ButtonHeight := 50;

  IosTitleStyleManager := TDialogStyleManager.Create(nil);
  IosTitleStyleManager.BackgroundRadius := 15;
  IosTitleStyleManager.TitleHeight := 55;
  IosTitleStyleManager.TitleTextBold := True;
  IosTitleStyleManager.TitleGravity := TLayoutGravity.CenterHBottom;
  IosTitleStyleManager.TitleTextSize := 17;
  IosTitleStyleManager.TitleTextColor := $FF1A1A1A;
  IosTitleStyleManager.TitleSpaceColor := IosStyleManager.BodyBackgroundColor;
  IosTitleStyleManager.MessageTextMargins.Left := 10;
  IosTitleStyleManager.MessageTextMargins.Right := 10;
  IosTitleStyleManager.MessageTextMargins.Bottom := 10;
  IosTitleStyleManager.MessageTextColor := $FF030303;
  IosTitleStyleManager.MessageTextGravity := TLayoutGravity.Center;
  IosTitleStyleManager.ButtonTextColor.Default := $FF0D69FF;
  IosTitleStyleManager.ButtonHeight := 50;
end;

procedure TFrmaeDialog.DoFree;
begin
  FreeAndNil(IosStyleManager);
  FreeAndNil(IosTitleStyleManager);

  inherited;
end;

procedure TFrmaeDialog.DoShow;
begin
  inherited;
  tvTitle.Text := Title;
end;

{ TStringsListAdapter }

function TStringsListAdapter.GetView(const Index: Integer;
  ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
var
  ViewItem: TListTextItem;
begin
  if (ConvertView = nil) or (not (ConvertView is TListTextItem)) then begin
    ViewItem := TListTextItem.Create(Parent);
    ViewItem.Parent := Parent;
    ViewItem.Width := Parent.Width;
    ViewItem.MinHeight := ItemDefaultHeight;
    ViewItem.TextSettings.Font.Size := FFontSize;
    ViewItem.TextSettings.WordWrap := FWordWrap;
    ViewItem.Gravity := TLayoutGravity.Center;
    ViewItem.Padding.Rect := RectF(8, 8, 8, 8);
    ViewItem.CanFocus := False;
  end else
    ViewItem := ConvertView as TListTextItem;
  Result := inherited GetView(Index, ViewItem, Parent);
end;

end.
