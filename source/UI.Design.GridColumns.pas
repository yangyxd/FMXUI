unit UI.Design.GridColumns;

interface

uses
  UI.Grid,
  System.Math,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, UI.Base, UI.Standard,
  FMX.ListBox;

type
  TGridColumnsDesigner = class(TForm)
    Layout3: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    edtOpacity: TEdit;
    Label3: TLabel;
    edtRowsPan: TEdit;
    Label4: TLabel;
    edtPaddingBottom: TEdit;
    GridView: TStringGridView;
    cbGravity: TComboBox;
    cbDataType: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    edtColsPan: TEdit;
    Label7: TLabel;
    edtPaddingLeft: TEdit;
    Label8: TLabel;
    edtPaddingTop: TEdit;
    Label9: TLabel;
    edtPaddingRight: TEdit;
    Label10: TLabel;
    edtTag: TEdit;
    ckLocked: TCheckBox;
    ckDataFilter: TCheckBox;
    ckReadOnly: TCheckBox;
    ckVisible: TCheckBox;
    ckEnabled: TCheckBox;
    ckWordWrap: TCheckBox;
    Label11: TLabel;
    edtTitle: TEdit;
    Layout1: TLayout;
    Button2: TButton;
    btnOk: TButton;
    Button1: TButton;
    Button3: TButton;
    tvIndex: TLabel;
    Label12: TLabel;
    edtRowCount: TEdit;
    tvColCount: TLabel;
    edtColCount: TEdit;
    Line1: TLine;
    edtFieldName: TEdit;
    tvField: TLabel;
    edtFixedColCount: TEdit;
    tvFixedColCount: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure GridViewFixedCellClick(Sender: TObject; const ACol,
      ARow: Integer);
    procedure GridViewTitleClick(Sender: TObject; Item: TGridColumnItem);
    procedure edtRowCountKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    { Private declarations }
    [Weak] SrcGridView: TGridBase;
    [Weak] CurItem: TGridColumnItem;
    FIsDBGrid: Boolean;
    FCol, FRow: Integer;


    procedure SetColumns(const Value: TGridColumns);
    function GetColumns: TGridColumns;

    procedure UpdateState;
  public
    { Public declarations }
    property Columns: TGridColumns read GetColumns write SetColumns;
  end;

var
  GridColumnsDesigner: TGridColumnsDesigner;

implementation

{$R *.fmx}

const
  CInvNo = -9999;

procedure TGridColumnsDesigner.btnOkClick(Sender: TObject);
begin
  if Assigned(SrcGridView) and (SrcGridView is TStringGridView) then begin
    TStringGridView(SrcGridView).ColCount := GridView.ColCount;
  end;
  ModalResult := mrOk;
end;

procedure TGridColumnsDesigner.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TGridColumnsDesigner.edtRowCountKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
    TControl(Sender).FocusToNext();
end;

function TGridColumnsDesigner.GetColumns: TGridColumns;
begin
  Result := GridView.Columns;
end;

procedure TGridColumnsDesigner.GridViewFixedCellClick(Sender: TObject;
  const ACol, ARow: Integer);
begin
  FCol := ACol;
  FRow := ARow;
  UpdateState;
  ShowMessage(Format('GridViewFixedCellClick. Col: %d, Row: %d', [FCol, FRow]));
end;

procedure TGridColumnsDesigner.GridViewTitleClick(Sender: TObject;
  Item: TGridColumnItem);
begin
  FCol := Item.ColIndex;
  FRow := Item.RowIndex;
  CurItem := Item;
  UpdateState;
end;

procedure TGridColumnsDesigner.SetColumns(const Value: TGridColumns);
begin
  GridView.Columns.Assign(Value);
  FIsDBGrid := False;
  SrcGridView := nil;

  if Assigned(Value) and Assigned(Value.GridView) then begin
    SrcGridView := Value.GridView;
    FIsDBGrid := Value.GridView is TDBGridView;
    GridView.ColCount := Value.GridView.ColCount;
    edtFixedColCount.Enabled := SrcGridView is TStringGridView;
    edtFixedColCount.Text := IntToStr(SrcGridView.FixedCols);
    GridView.FixedCols := SrcGridView.FixedCols;
  end else
    edtFixedColCount.Enabled := False;

  tvFixedColCount.Enabled := edtFixedColCount.Enabled;

  if not FIsDBGrid then begin
    tvField.Visible := False;
    edtFieldName.Visible := False;
    edtTitle.Width := edtFieldName.Position.X + edtFieldName.Width - edtTitle.Position.X;
  end;

  FCol := CInvNo;
  FRow := CInvNo;

  UpdateState;
end;

procedure TGridColumnsDesigner.UpdateState;
begin
  edtRowCount.Text := IntToStr(Columns.RowsCount);
  edtColCount.Text := IntToStr(Columns.ColsCount);

  if (CurItem = nil) or (FCol = CInvNo) or (FRow = CInvNo) then begin
    tvIndex.Text := '';
    Exit;
  end else
    tvIndex.Text := Format('Col: %d  Row: %d', [FCol, FRow]);
end;

end.
