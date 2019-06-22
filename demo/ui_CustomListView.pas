unit ui_CustomListView;

interface

uses
  System.Generics.Collections,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, System.ImageList, FMX.ImgList, UI.ListView, UI.Frame;

type
  TDataItem = record
    Name: string;
    Phone: string;
    Color: TAlphaColor;
  end;

  TCustomListDataAdapter = class(TListAdapterBase)
  private
    [Weak] FList: TList<TDataItem>;
  protected
    function GetCount: Integer; override;
    function ItemDefaultHeight: Single; override;
    function GetItem(const Index: Integer): Pointer; override;
    function IndexOf(const AItem: Pointer): Integer; override;
    function GetView(const Index: Integer; ConvertView: TViewBase;
      Parent: TViewGroup): TViewBase; override;
  public
    constructor Create(const AList: TList<TDataItem>);
  end;

type
  TCustomListview = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    TextView2: TTextView;
    ImageList1: TImageList;
    ListView: TListViewEx;
    btnBack: TTextView;
    procedure TextView1Click(Sender: TObject);
    procedure ListViewPullRefresh(Sender: TObject);
    procedure ListViewPullLoad(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure ListViewItemClick(Sender: TObject; ItemIndex: Integer;
      const ItemView: TControl);
  private
    { Private declarations }
    FAdapter: TCustomListDataAdapter;
    FList: TList<TDataItem>;
  protected
    procedure DoCreate(); override;
    procedure DoFree(); override;
    procedure DoShow(); override;
  public
    { Public declarations }
    procedure AddItems(const Count: Integer);
  end;

implementation

{$R *.fmx}

uses
  ui_CustomListView_ListItem;

procedure TCustomListview.AddItems(const Count: Integer);
var
  I: Integer;
  Item: TDataItem;
begin
  for I := 0 to Count - 1 do begin
    Item.Name := '用户名称' + IntToStr(I);
    if I mod 2 = 0 then
      Item.Color := TAlphaColorRec.Crimson
    else
      Item.Color := TAlphaColorRec.Yellow;
    Item.Phone := '131 0000 0000';
    FList.Add(Item);
  end;
  FAdapter.NotifyDataChanged;
end;

procedure TCustomListview.btnBackClick(Sender: TObject);
begin
  Finish();
end;

procedure TCustomListview.DoCreate;
var
  LV: TTextView;
begin
  inherited;
  FList := TList<TDataItem>.Create();
  FAdapter := TCustomListDataAdapter.Create(FList);

  LV := TTextView.Create(Self);
  LV.Name := '';
  LV.Text := 'HeaderView';
  LV.Gravity := TLayoutGravity.Center;
  LV.Background.ItemDefault.Color := $ef33ccff;
  LV.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  LV.Height := 36;
  ListView.AddHeaderView(LV);

  LV := TTextView.Create(Self);
  LV.Name := '';
  LV.Text := 'FooterView';
  LV.Gravity := TLayoutGravity.Center;
  LV.Background.ItemDefault.Color := $ef009966;
  LV.Background.ItemDefault.Kind := TViewBrushKind.Solid;
  LV.Height := 42;
  ListView.AddFooterView(LV);

  ListView.Adapter := FAdapter;
  AddItems(20);
end;

procedure TCustomListview.DoFree;
begin
  inherited;
  ListView.Adapter := nil;
  FAdapter := nil;
  FreeAndNil(FList);
end;

procedure TCustomListview.DoShow;
begin
  inherited;
end;

procedure TCustomListview.ListViewItemClick(Sender: TObject; ItemIndex: Integer;
  const ItemView: TControl);
begin
  Hint(Format('点击了%d. Name：%s', [ItemIndex, FAdapter.FList.Items[ItemIndex].Name]));
  Hint(TCustomListView_ListItem(ItemView).TextView1.Text);
end;

procedure TCustomListview.ListViewPullLoad(Sender: TObject);
begin
  DelayExecute(1,
    procedure (Sender: TObject)
    begin
      AddItems(20);
      if FList.Count > 50 then
        ListView.EnablePullLoad := False;
      ListView.PullLoadComplete;
    end
  );
end;

procedure TCustomListview.ListViewPullRefresh(Sender: TObject);
begin
  Hint('正在加载数据');
  DelayExecute(2,
    procedure (Sender: TObject)
    begin
      FList.Clear;
      AddItems(20);
      ListView.EnablePullLoad := True;
      ListView.PullRefreshComplete;
    end
  );
end;

procedure TCustomListview.TextView1Click(Sender: TObject);
begin
end;

{ TCustomListDataAdapter }

constructor TCustomListDataAdapter.Create(const AList: TList<TDataItem>);
begin
  FList := AList;
end;

function TCustomListDataAdapter.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Count
  else
    Result := 0;
end;

function TCustomListDataAdapter.GetItem(const Index: Integer): Pointer;
begin
  Result := nil;
end;

function TCustomListDataAdapter.GetView(const Index: Integer;
  ConvertView: TViewBase; Parent: TViewGroup): TViewBase;
var
  ViewItem: TCustomListView_ListItem;
  Item: TDataItem;
begin
  if (ConvertView = nil) or (not (ConvertView.ClassType = TCustomListView_ListItem)) then begin
    ViewItem := TCustomListView_ListItem.Create(Parent);
    ViewItem.Parent := Parent;
    ViewItem.Width := Parent.Width;
    ViewItem.CanFocus := False;
  end else
    ViewItem := TObject(ConvertView) as TCustomListView_ListItem;

  Item := FList.Items[Index];
  ViewItem.BeginUpdate;
  ViewItem.TextView1.Text := Item.Name;
  ViewItem.TextView2.Text := Item.Phone;
  ViewItem.View1.Background.ItemDefault.Color := Item.Color;
  ViewItem.BadgeView1.Enabled := Index < 9;
  ViewItem.BadgeView1.Visible := Index < 9;
  case Index mod 9 of
    0..2: ViewItem.BadgeView1.Background.Color := TAlphaColors.Red;
    3..5: ViewItem.BadgeView1.Background.Color := TAlphaColors.Green;
    6..8: ViewItem.BadgeView1.Background.Color := TAlphaColors.Blue;
  end;
  case Index mod 9 of
    0: ViewItem.BadgeView1.Gravity := TLayoutGravity.LeftTop;
    1: ViewItem.BadgeView1.Gravity := TLayoutGravity.CenterVertical;
    2: ViewItem.BadgeView1.Gravity := TLayoutGravity.LeftBottom;
    3: ViewItem.BadgeView1.Gravity := TLayoutGravity.CenterHorizontal;
    4: ViewItem.BadgeView1.Gravity := TLayoutGravity.Center;
    5: ViewItem.BadgeView1.Gravity := TLayoutGravity.CenterHBottom;
    6: ViewItem.BadgeView1.Gravity := TLayoutGravity.RightTop;
    7: ViewItem.BadgeView1.Gravity := TLayoutGravity.CenterVRight;
    8: ViewItem.BadgeView1.Gravity := TLayoutGravity.RightBottom;
  end;
  ViewItem.BadgeView1.Value := Index + 1;
  ViewItem.BadgeView2.Enabled := Index = 1;
  ViewItem.BadgeView2.Visible := Index = 1;
  ViewItem.BadgeView3.Enabled := Index mod 2 = 1;
  ViewItem.BadgeView3.Visible := Index mod 2 = 1;
  ViewItem.EndUpdate;
  Result := TViewBase(ViewItem);
end;

function TCustomListDataAdapter.IndexOf(const AItem: Pointer): Integer;
begin
  Result := -1;
end;

function TCustomListDataAdapter.ItemDefaultHeight: Single;
begin
  Result := 72;
end;

end.
