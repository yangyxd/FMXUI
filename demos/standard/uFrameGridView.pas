unit uFrameGridView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  {$IF CompilerVersion >= 31}
  FMX.DialogService,
  {$ENDIF}
  UI.Standard, UI.Base, UI.Grid, UI.Frame, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Phys, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TFrameGridView = class(TFrame)
    GridView1: TStringGridView;
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    DBGridView1: TDBGridView;
    FDMemTable1: TFDMemTable;
    FDMemTable1Name: TStringField;
    FDMemTable1Title: TStringField;
    DataSource1: TDataSource;
    FDMemTable1Total: TFloatField;
    LinearLayout2: TLinearLayout;
    ButtonView5: TButtonView;
    ButtonView4: TButtonView;
    ButtonView3: TButtonView;
    ButtonView2: TButtonView;
    ButtonView1: TButtonView;
    btnBack: TTextView;
    procedure TextView1Click(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
    procedure ButtonView3Click(Sender: TObject);
    procedure ButtonView4Click(Sender: TObject);
    procedure ButtonView5Click(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure GridView1CellClick(Sender: TObject; const ACell: TGridCell);
    procedure GridView1TitleClick(Sender: TObject; Item: TGridColumnItem);
    procedure GridView1TitleDbClick(Sender: TObject; Item: TGridColumnItem);
    procedure GridView1CellDbClick(Sender: TObject; const ACell: TGridCell);
    procedure GridView1DrawFixedColText(Sender: TObject; Canvas: TCanvas;
      Item: TGridColumnItem; const R: TRectF; var DefaultDraw: Boolean);
    procedure DBGridView1DrawCells(Sender: TObject; Canvas: TCanvas; const ACol,
      ARow: Integer; const R: TRectF; ADrawState: TViewState;
      Column: TGridColumnItem; var DefaultDraw: Boolean);
  private
    { Private declarations }
  protected
    procedure DoShow(); override;
  public
    { Public declarations }
  end;

implementation

uses
  UI.Design.GridColumns;

{$R *.fmx}

procedure TFrameGridView.btnBackClick(Sender: TObject);
begin
  Finish;
end;

procedure TFrameGridView.ButtonView1Click(Sender: TObject);
begin
  GridView1.Clear;
end;

procedure TFrameGridView.ButtonView2Click(Sender: TObject);
begin
  GridView1.SelectIndex := -1;
end;

procedure TFrameGridView.ButtonView3Click(Sender: TObject);
begin
  {$IF CompilerVersion >= 31}
  TDialogService.InputQuery('输入新行数', ['新行数'], [IntToStr(GridView1.RowCount)],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOk then
        GridView1.RowCount := StrToIntDef(AValues[0], GridView1.RowCount);
    end
  );
  {$ELSE}
  GridView1.RowCount := StrToIntDef(InputBox('输入新行数', '新行数', IntToStr(GridView1.RowCount)), GridView1.RowCount);
  {$ENDIF}
end;

procedure TFrameGridView.ButtonView4Click(Sender: TObject);
begin
  {$IF CompilerVersion >= 31}
  TDialogService.InputQuery('输入新列数', ['新列数'], [IntToStr(GridView1.ColCount)],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOk then
        GridView1.ColCount := StrToIntDef(AValues[0], GridView1.ColCount);
    end
  );
  {$ELSE}
  GridView1.ColCount := StrToIntDef(InputBox('输入新列数', '新列数', IntToStr(GridView1.ColCount)), GridView1.ColCount);
  {$ENDIF}
end;

procedure TFrameGridView.ButtonView5Click(Sender: TObject);
var
  Dialog: TGridColumnsDesigner;
begin
  Dialog := TGridColumnsDesigner.Create(Self);
  try
    Dialog.Columns := DBGridView1.Columns;
    if Dialog.ShowModal = mrOk then
      DBGridView1.Columns.Assign(Dialog.Columns);
  finally
    Dialog.Free;
  end;
end;

procedure TFrameGridView.DBGridView1DrawCells(Sender: TObject; Canvas: TCanvas;
  const ACol, ARow: Integer; const R: TRectF; ADrawState: TViewState;
  Column: TGridColumnItem; var DefaultDraw: Boolean);
var
  FStyle: TFontStyles;
begin
  //蓝色下划线显示第3列第2行
  if (ACol = 2) and (ARow = 1) then begin
    DBGridView1.TextSettings.CustomColor := TAlphaColorrec.Blue;
    FStyle := DBGridView1.TextSettings.Font.Style;
    try
      DBGridView1.TextSettings.Font.Style := [TFontStyle.fsUnderline];
      DBGridView1.TextSettings.Draw(Canvas, DBGridView1.Cells[ACol, ARow], R,
        Column.Opacity * DBGridView1.Opacity, TViewState.Custom, Column.Gravity);
    finally
      DBGridView1.TextSettings.Font.Style := FStyle;
    end;
    DefaultDraw := False;
  end
  else
    DefaultDraw := True;
end;

procedure TFrameGridView.DoShow;
begin
  inherited;

  GridView1.Columns[11, 0].Title := '[11]';
  GridView1.Columns[14, 0].Title := '[14]';

  DBGridView1.Columns[0, 0].DataFilter := True;
  DBGridView1.Columns[0, 0].Width := 100;

  FDMemTable1.InsertRecord(['test', 'name', '12']);
  FDMemTable1.InsertRecord(['test1', 'name', '25']);
  FDMemTable1.InsertRecord(['test2', 'name', '21.5']);
  FDMemTable1.InsertRecord(['test3', 'name', '22']);
  //FDMemTable1.InsertRecord(['test4', 'name']);
  //FDMemTable1.InsertRecord(['test5', 'name']);

//  FAdapter := TStringGridAdapter.Create;
//  FAdapter.RowCount := 100;
//  GridView1.Adapter := FAdapter;

end;

procedure TFrameGridView.GridView1CellClick(Sender: TObject;
  const ACell: TGridCell);
begin
  //ShowMessage(Format('Cell Row: %d, Col: %d.', [ACell.Row, Acell.Col]));
end;

procedure TFrameGridView.GridView1CellDbClick(Sender: TObject;
  const ACell: TGridCell);
begin
  //ShowMessage(Format('DbClick Cell Row: %d, Col: %d.', [ACell.Row, Acell.Col]));
end;

procedure TFrameGridView.GridView1DrawFixedColText(Sender: TObject;
  Canvas: TCanvas; Item: TGridColumnItem; const R: TRectF;
  var DefaultDraw: Boolean);
begin
  if (Item.ColIndex = 1) and (Item.RowIndex = 0) then begin
    // 红色字体显示第2列第0行表头
    GridView1.FixedTextSettings.CustomColor := TAlphaColorrec.Red;
    GridView1.FixedTextSettings.Draw(Canvas, Item.DisplayText, R, Item.Opacity * GridView1.Opacity,
      TViewState.Custom, GridView1.FixedTextSettings.Gravity);
  end else
    DefaultDraw := True;
end;

procedure TFrameGridView.GridView1TitleClick(Sender: TObject;
  Item: TGridColumnItem);
begin
  //ShowMessage(Format('FixedColumn Row: %d, Col: %d.', [Item.RowIndex, Item.ColIndex]));
end;

procedure TFrameGridView.GridView1TitleDbClick(Sender: TObject;
  Item: TGridColumnItem);
begin
  ShowMessage(Format('FixedColumn DbClick Row: %d, Col: %d.', [Item.RowIndex, Item.ColIndex]));
end;

procedure TFrameGridView.TextView1Click(Sender: TObject);
begin
  Finish();
end;

end.
