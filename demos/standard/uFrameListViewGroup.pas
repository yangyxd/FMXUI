unit uFrameListViewGroup;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Standard, UI.Base, UI.ListView;

type
  TDataItem = record
    Name: string;
    Phone: string;
    Color: TAlphaColor;
    constructor Create(const Name, Phone: string; const Color: TAlphaColor);
  end;

  TCustomListDataAdapter = class(TCustomTreeListDataAdapter<TDataItem>)
  protected
    function GetNodeItemView(const Index: Integer; const ANode: TTreeListNode<TDataItem>;
      ConvertView: TViewBase; Parent: TViewGroup): TViewBase; override;
    function GetNodeText(const ANode: TTreeListNode<TDataItem>): string; override;
  end;

type
  TFrameListViewGroup = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    ListView: TListViewEx;
    btnBack: TTextView;
    procedure btnBackClick(Sender: TObject);
    procedure ListViewItemClick(Sender: TObject; ItemIndex: Integer;
      const ItemView: TControl);
  private
    { Private declarations }
    FAdapter: TCustomListDataAdapter;
  protected
    procedure DoCreate(); override;
    procedure DoFree(); override;
    procedure DoShow(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  ui_CustomListView_ListItem;

procedure TFrameListViewGroup.btnBackClick(Sender: TObject);
begin
  Finish();
end;

procedure TFrameListViewGroup.DoCreate;
begin
  inherited;
  FAdapter := TCustomListDataAdapter.Create();

  FAdapter.Root.AddNode(TDataItem.Create('我是组1', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组2', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组3', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组4', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组5', '', TAlphaColorRec.Crimson));

  FAdapter.Root.AddNode(TDataItem.Create('我是组1', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组2', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组3', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组4', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组5', '', TAlphaColorRec.Crimson));

  FAdapter.Root.AddNode(TDataItem.Create('我是组1', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组2', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组3', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组4', '', TAlphaColorRec.Crimson));
  FAdapter.Root.AddNode(TDataItem.Create('我是组5', '', TAlphaColorRec.Crimson));

  FAdapter.Root.Nodes[0].AddNode(TDataItem.Create('我是节点1', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[0].AddNode(TDataItem.Create('我是节点2', '131 0000 0000', TAlphaColorRec.Yellow));
  FAdapter.Root.Nodes[0].AddNode(TDataItem.Create('我是节点3', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[0].AddNode(TDataItem.Create('我是节点4', '131 0000 0000', TAlphaColorRec.Yellow));

  FAdapter.Root.Nodes[1].AddNode(TDataItem.Create('我是节点1', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[1].AddNode(TDataItem.Create('我是节点2', '131 0000 0000', TAlphaColorRec.Yellow));
  FAdapter.Root.Nodes[1].AddNode(TDataItem.Create('我是节点3', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[1].AddNode(TDataItem.Create('我是节点4', '131 0000 0000', TAlphaColorRec.Yellow));

  FAdapter.Root.Nodes[2].AddNode(TDataItem.Create('我是节点1', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[2].AddNode(TDataItem.Create('我是节点2', '131 0000 0000', TAlphaColorRec.Yellow));
  FAdapter.Root.Nodes[2].AddNode(TDataItem.Create('我是节点3', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[2].AddNode(TDataItem.Create('我是节点4', '131 0000 0000', TAlphaColorRec.Yellow));

  FAdapter.Root.Nodes[3].AddNode(TDataItem.Create('我是节点1', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[3].AddNode(TDataItem.Create('我是节点2', '131 0000 0000', TAlphaColorRec.Yellow));
  FAdapter.Root.Nodes[3].AddNode(TDataItem.Create('我是节点3', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[3].AddNode(TDataItem.Create('我是节点4', '131 0000 0000', TAlphaColorRec.Yellow));

  FAdapter.Root.Nodes[4].AddNode(TDataItem.Create('我是节点1', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[4].AddNode(TDataItem.Create('我是节点2', '131 0000 0000', TAlphaColorRec.Yellow));
  FAdapter.Root.Nodes[4].AddNode(TDataItem.Create('我是节点3', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[4].AddNode(TDataItem.Create('我是节点4', '131 0000 0000', TAlphaColorRec.Yellow));

  FAdapter.Root.Nodes[2].AddNode(TDataItem.Create('我是节点1', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[3].AddNode(TDataItem.Create('我是节点2', '131 0000 0000', TAlphaColorRec.Yellow));
  FAdapter.Root.Nodes[4].AddNode(TDataItem.Create('我是节点3', '131 0000 0000', TAlphaColorRec.Crimson));
  FAdapter.Root.Nodes[5].AddNode(TDataItem.Create('我是节点4', '131 0000 0000', TAlphaColorRec.Yellow));

  FAdapter.InitList;

  ListView.Adapter := FAdapter;
end;

procedure TFrameListViewGroup.DoFree;
begin
  inherited;
  ListView.Adapter := nil;
  FAdapter := nil;
end;

procedure TFrameListViewGroup.DoShow;
begin
  inherited;
  tvTitle.Text := Title;
end;

procedure TFrameListViewGroup.ListViewItemClick(Sender: TObject;
  ItemIndex: Integer; const ItemView: TControl);
begin
  with FAdapter.FList.Items[ItemIndex] do
    if ItemView is TCustomListView_ListItem then begin
      Hint(Format('点击了%d. Level:%d. Name:%s', [Index, Level, Data.Name]));
      Hint(TCustomListView_ListItem(ItemView).TextView1.Text);
    end
    else
      Hint(Format('点击了分组%d. Level:%d. Name:%s', [Index, Level, Data.Name]));
end;

{ TDataItem }

constructor TDataItem.Create(const Name, Phone: string;
  const Color: TAlphaColor);
begin
  Self.Name := Name;
  Self.Phone := Phone;
  Self.Color := Color;
end;

{ TCustomListDataAdapter }

function TCustomListDataAdapter.GetNodeItemView(const Index: Integer;
  const ANode: TTreeListNode<TDataItem>; ConvertView: TViewBase;
  Parent: TViewGroup): TViewBase;
var
  ViewItem: TCustomListView_ListItem;
begin
  if (ConvertView = nil) or (not (ConvertView.ClassType = TCustomListView_ListItem)) then begin
    ViewItem := TCustomListView_ListItem.Create(Parent);
    ViewItem.Parent := Parent;
    ViewItem.Width := Parent.Width;
    ViewItem.CanFocus := False;
  end else
    ViewItem := TObject(ConvertView) as TCustomListView_ListItem;

  ViewItem.BeginUpdate;
  ViewItem.TextView1.Text := ANode.Data.Name;
  ViewItem.TextView2.Text := ANode.Data.Phone;
  ViewItem.View1.Background.ItemDefault.Color := ANode.Data.Color;
  ViewItem.BadgeView1.Value := Index + 1;
  ViewItem.EndUpdate;
  Result := TViewBase(ViewItem);
end;

function TCustomListDataAdapter.GetNodeText(
  const ANode: TTreeListNode<TDataItem>): string;
begin
  Result := ANode.Data.Name;
end;

end.
