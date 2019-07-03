unit uFrame4_Page1;

interface

uses
  ui_CustomListView,
  System.Generics.Collections,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Base, UI.Standard, UI.ListView;

type
  TFrame4_Page1 = class(TFrame)
    ListView: TListViewEx;
  private
    { Private declarations }
    FAdapter: IListAdapter;
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

{ TFrame4_Page1 }

procedure TFrame4_Page1.AddItems(const Count: Integer);
var
  I: Integer;
  Item: TDataItem;
begin
  for I := 0 to Count - 1 do begin
    Item.Name := 'ÓÃ»§Ãû³Æ' + IntToStr(I);
    if I mod 2 = 0 then
      Item.Color := TAlphaColorRec.Crimson
    else
      Item.Color := TAlphaColorRec.Yellow;
    Item.Phone := '131 0000 0000';
    FList.Add(Item);
  end;
  FAdapter.NotifyDataChanged;
end;

procedure TFrame4_Page1.DoCreate;
begin
  inherited;
  FList := TList<TDataItem>.Create();
  FAdapter := TCustomListDataAdapter.Create(FList);
  ListView.Adapter := FAdapter;
  AddItems(20);
end;

procedure TFrame4_Page1.DoFree;
begin
  inherited;
  ListView.Adapter := nil;
  FAdapter := nil;
  FreeAndNil(FList);
end;

procedure TFrame4_Page1.DoShow;
begin
  inherited;
end;

end.
