unit uFrame1;

interface

uses
  System.Generics.Collections,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  UI.Standard, UI.Base, UI.Frame, UI.Dialog, UI.ListView, uBaseFrame{Base Frame for project};

type
  TDataItem = record
    TiTle: string;
    Accessory: TViewAccessoryType;
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

  TFrame1 = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    ListViewEx1: TListViewEx;
    procedure ListViewEx1ItemClick(Sender: TObject; ItemIndex: Integer;
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
  end;

implementation

{$R *.fmx}

uses
  uFrame2;

{ TFrame1 }

procedure TFrame1.DoCreate;

  procedure AddItem(ATitle: string; AAccessory: TViewAccessoryType; AColor: TAlphaColor);
  var
    Item: TDataItem;
  begin
    Item.Accessory := AAccessory;
    Item.TiTle := ATitle;
    Item.Color := AColor;
    FList.Add(Item);
  end;

begin
  inherited;

  FList := TList<TDataItem>.Create();
  FAdapter := TCustomListDataAdapter.Create(FList);
  ListViewEx1.Adapter := FAdapter;

  AddItem('OnInit', TViewAccessoryType.Pagecurl, TAlphaColorRec.Green);
  AddItem('OnReturn', TViewAccessoryType.Refresh, TAlphaColorRec.Blue);
  AddItem('OnCallback', TViewAccessoryType.Password, TAlphaColorRec.Gray);
  AddItem('Exit', TViewAccessoryType.Exit, TAlphaColorRec.Crimson);
  FAdapter.NotifyDataChanged;
end;

procedure TFrame1.DoFree;
begin
  inherited;

  ListViewEx1.Adapter := nil;
  FAdapter := nil;
  FreeAndNil(FList);
end;

procedure TFrame1.DoShow;
begin
  inherited;

end;

procedure TFrame1.ListViewEx1ItemClick(Sender: TObject; ItemIndex: Integer;
  const ItemView: TControl);
begin
  if FList[ItemIndex].TiTle = 'Exit' then begin
    Application.MainForm.Close;
  end
  else if FList[ItemIndex].TiTle = 'OnCallback' then begin
    with TFrame2(TFrame2.ShowMainFrame) do begin
      Hint('Callback demo, please click the button');
      BackColor := FList[ItemIndex].Color;
      tvTitle.Text := FList[ItemIndex].TiTle;
      EnableLogin := True;
      OnLogin := procedure (AView: TFrame; AUserName, APassword: string) begin
        if (AUserName = '') or (APassword = '') then begin
          Hint('Please input all items.');
          Exit;
        end;

        Hint('UserName: %s Password:%s', [AUserName, APassword]);
        AView.Finish;
      end;
    end;
  end
  else if FList[ItemIndex].TiTle = 'OnReturn' then begin
    with TFrame2(TFrame2.ShowMainFrame) do begin
      BackColor := FList[ItemIndex].Color;
      tvTitle.Text := FList[ItemIndex].TiTle;
    end;
  end
  else if FList[ItemIndex].TiTle = 'OnInit' then begin
    TFrame2.ShowWithInit(
      procedure (View: TFrameView) begin
        with TFrame2(View) do begin
          BackColor := FList[ItemIndex].Color;
          tvTitle.Text := FList[ItemIndex].TiTle;
        end;
      end)
  end;
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
  ViewItem: TTextView;
  Item: TDataItem;
begin
  if (ConvertView = nil) or (not (ConvertView.ClassType = TTextView)) then begin
    ViewItem := TTextView.Create(Parent);
    with ViewItem do begin
      Parent := Parent;
      Align := TAlignLayout.Client;
      WidthSize := TViewSize.FillParent;
      HeightSize := TViewSize.FillParent;
      CanFocus := False;
      Padding.Left := 8;
      Padding.Right := 8;
      Padding.Top := 15;
      Padding.Bottom := 15;
      Drawable.Position := TDrawablePosition.Top;
      Drawable.SizeWidth := 40;
      Drawable.SizeHeight := 40;
      Drawable.Padding := 10;
      TextSettings.Gravity := TLayoutGravity.CenterHorizontal;
      TextSettings.Font.Size := 11;
      with TViewBrushBase(Drawable.ItemDefault) do begin
        Kind := TViewBrushKind.AccessoryBitmap;
      end;
      Size.Width := 80;
      Size.Height := 100;
    end;
  end else
    ViewItem := TObject(ConvertView) as TTextView;

  Item := FList.Items[Index];
  ViewItem.BeginUpdate;
  try
    with ViewItem do begin
      with TViewBrushBase(Drawable.ItemDefault) do begin
        Accessory.Accessory := Item.Accessory;
        Accessory.Color := Item.Color;
      end;
      Text := Item.TiTle;
    end;
  finally
    ViewItem.EndUpdate;
  end;
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
