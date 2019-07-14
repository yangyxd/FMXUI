unit uFrameListViewTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Base, UI.Standard, UI.ListView, FMX.Controls.Presentation;

type
  TFrameListViewTest = class(TFrame)
    ListViewEx1: TListViewEx;
    TextView1: TTextView;
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    btnBack: TTextView;
    procedure ListViewEx1ItemClick(Sender: TObject; ItemIndex: Integer;
      const ItemView: TControl);
    procedure ListViewEx1ScrollChange(Sender: TObject);
    procedure tvTitleClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure DoShow(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  UI.Dialog;

procedure TFrameListViewTest.btnBackClick(Sender: TObject);
begin
  Finish();
end;

procedure TFrameListViewTest.DoShow;
var
  I: Integer;
  Adapter: TStringsListCheckAdapter;
begin
  inherited;

  Adapter := TStringsListCheckAdapter.Create(nil);
  Adapter.SetArrayLength(1000);
  for I := 0 to Adapter.Count - 1 do
    Adapter.Items[I] := Format('列表项列表项列表项列表项列表项列表项列表项列表项列表项列表项项列表项列表项项列表项列表项列表项 %d', [I]);
  ListViewEx1.Adapter := Adapter;
end;

procedure TFrameListViewTest.ListViewEx1ItemClick(Sender: TObject;
  ItemIndex: Integer; const ItemView: TControl);
begin
  Exit;
  TDialogBuilder.Create(Self)
    .SetTitle('删除提示')
    .SetMessage(TStringsListAdapter(ListViewEx1.Adapter).Items[ItemIndex])
    .SetNegativeButton('删除',
      procedure (Dialog: IDialog; Which: Integer)
      begin
        TStringsListAdapter(ListViewEx1.Adapter).Delete(ItemIndex);
        ListViewEx1.Adapter.NotifyDataChanged;
      end
    )
    .SetPositiveButton('取消')
    .Show();
end;

procedure TFrameListViewTest.ListViewEx1ScrollChange(Sender: TObject);
begin
  TCustomForm(Parent).Caption := Format('ScrollBarValue: %.2f/%.2f',
    [ListViewEx1.VScrollBar.ValueD, ListViewEx1.VScrollBar.MaxD])
end;

procedure TFrameListViewTest.tvTitleClick(Sender: TObject);
begin
  ShowMessage(IntToStr(ListViewEx1.VisibleRowCount));

  Hint(Format('H: %.2f, Top: %.2f',
    [ListViewEx1.ItemPosition[0].H, ListViewEx1.ItemViews[0].Position.Y]));
end;

end.
