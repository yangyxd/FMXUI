unit uFrameListView;

interface

uses
  UI.Dialog,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Base, UI.Standard, UI.ListView, System.ImageList, FMX.ImgList;

type
  TFrameListView = class(TFrame)
    ListViewEx1: TListViewEx;
    LinearLayout1: TLinearLayout;
    TextView1: TTextView;
    tvTitle: TTextView;
    TextView2: TTextView;
    ImageList1: TImageList;
    procedure TextView1Click(Sender: TObject);
    procedure ListViewEx1PullRefresh(Sender: TObject);
    procedure ListViewEx1PullLoad(Sender: TObject);
    procedure TextView2Click(Sender: TObject);
  private
    { Private declarations }
    Adapter: TStringsListAdapter;
  protected
    procedure DoShow(); override;
  public
    { Public declarations }
    procedure AddItems(const Count: Integer);
  end;

implementation

{$R *.fmx}

uses
  ui_PopupMenu;

procedure TFrameListView.AddItems(const Count: Integer);
var
  I, LastCount: Integer;
begin
  LastCount := Adapter.Count;
  Adapter.SetArrayLength(Adapter.Count + Count);
  for I := LastCount to Adapter.Count - 1 do
    Adapter.Items[I] := Format('列表项列表项列表项列表项列表项列表项列表项列表项列表项列 %d', [I]);

  Adapter.NotifyDataChanged;
end;

procedure TFrameListView.DoShow;
begin
  inherited;

  Adapter := TStringsListCheckAdapter.Create(nil);
  ListViewEx1.Adapter := Adapter;
  AddItems(20);
end;

procedure TFrameListView.ListViewEx1PullLoad(Sender: TObject);
begin
  DelayExecute(2,
    procedure (Sender: TObject)
    begin
      AddItems(20);
      Adapter.NotifyDataChanged;
      ListViewEx1.PullLoadComplete;
    end
  );
end;

procedure TFrameListView.ListViewEx1PullRefresh(Sender: TObject);
begin
  Hint('正在加载数据');
  DelayExecute(2,
    procedure (Sender: TObject)
    begin
      Adapter.Clear;
      AddItems(20);
      Adapter.NotifyDataChanged;
      ListViewEx1.PullRefreshComplete;
    end
  );
end;

procedure TFrameListView.TextView1Click(Sender: TObject);
begin
  Finish();
end;

procedure TFrameListView.TextView2Click(Sender: TObject);
begin
  TDialog.ShowView(Self, TextView2, TMainPopupMenu, -50, TextView2.Height, TDialogViewPosition.Left);
end;

end.
