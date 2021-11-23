unit uFrameGridSum;

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
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Objects,
  FireDAC.DApt, FireDAC.Stan.StorageJSON;

type
  TFrameGridSum = class(TFrame)
    FDMemTable1: TFDMemTable;
    GridView2: TStringGridView;
    LinearLayout1: TLinearLayout;
    btnBack: TTextView;
    tvTitle: TTextView;
    TextView2: TTextView;
    LinearLayout2: TLinearLayout;
    ButtonView5: TButtonView;
    ButtonView3: TButtonView;
    ButtonView4: TButtonView;
    ButtonView2: TButtonView;
    ButtonView1: TButtonView;
    ButtonView6: TButtonView;
    ButtonView7: TButtonView;
    ButtonView9: TButtonView;
    ButtonView8: TButtonView;
    DataSource1: TDataSource;
    DBGridView2: TDBGridView;
    Splitter1: TSplitter;
    View1: TView;
    TextView3: TTextView;
    procedure btnBackClick(Sender: TObject);
    procedure ButtonView5Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure DoShow(); override;
  public
    { Public declarations }
  end;

var
  FrameGridSum: TFrameGridSum;

implementation

{$R *.fmx}

procedure TFrameGridSum.btnBackClick(Sender: TObject);
begin
  Finish();
end;

procedure TFrameGridSum.ButtonView5Click(Sender: TObject);
begin
  GridView2.NotifyDataChanged;
  DBGridView2.NotifyDataChanged;
end;

procedure TFrameGridSum.DoShow;
var
  c, r: integer;
  LStream: TResourceStream;
begin
  inherited;

  LStream := TResourceStream.Create(HInstance, 'Grid_Test_Json', RT_RCDATA);
  try
    FDMemTable1.LoadFromStream(LStream, sfJSON);
  finally
    FreeAndNil(LStream);
  end;
  FDMemTable1.DisableControls;
  GridView2.RowCount := FDMemTable1.RecordCount;
  GridView2.ColCount := FDMemTable1.FieldCount;
  try
    FDMemTable1.First;
    for r := 0 to FDMemTable1.RecordCount - 1 do
    begin
      for c := 0 to FDMemTable1.FieldCount - 1 do
      begin
        GridView2.Cells[c, r] := FDMemTable1.Fields.Fields[c].AsString;
      end;
      FDMemTable1.Next;
    end;
  finally
    FDMemTable1.EnableConstraints;
  end;

  DBGridView2.DataSource := DataSource1;
  with DBGridView2.Columns do
  begin
    with ColumnByCols(0) do
    begin
      //FieldType:=TFieldType.ftFloat;
      //FieldName:='aa0';
      //Title:='bb0';
      FooterStyle := TGridFooterStyle.DoSum;
    end;
    with ColumnByCols(1) do
    begin
      //FieldType:=TFieldType.ftInteger;
      //FieldName:='aa1';
      //Title:='bb1';
      FooterStyle := TGridFooterStyle.DoSum;
    end;
    with ColumnByCols(2) do
    begin
      //FieldType:=TFieldType.ftFloat;
      //FieldName:='aa2';
      //Title:='bb2';
      FooterStyle := TGridFooterStyle.DoAvg;
    end;
    with ColumnByCols(3) do
    begin
      //FieldType:=TFieldType.ftInteger;
      //FieldName:='aa3';
      //Title:='bb3';
      FooterStyle := TGridFooterStyle.DoAvg;
    end;
    with ColumnByCols(4) do
    begin
      //FieldType:=TFieldType.ftInteger;
      //FieldName:='aa3';
      //Title:='bb3';
      FooterStyle := TGridFooterStyle.DoMin;
    end;
    with ColumnByCols(5) do
    begin
      //FieldType:=TFieldType.ftInteger;
      //FieldName:='aa3';
      //Title:='bb3';
      FooterStyle := TGridFooterStyle.DoMax;
    end;
    with ColumnByCols(6) do
    begin
      //FieldType:=TFieldType.ftInteger;
      //FieldName:='aa3';
      //Title:='bb3';
      FooterCountStr := '8';
      FooterStyle := TGridFooterStyle.DoCount;
    end;
  end;

  for c := 0 to DBGridView2.ColCount - 1 do
  begin
    with GridView2.Columns.ItemCols[c] do
    begin
      Width := DBGridView2.Columns.ItemCols[c].Width;
      fieldname := DBGridView2.Columns.ItemCols[c].DisplayName;
      Title := DBGridView2.Columns.ItemCols[c].DisplayText;
      FieldType := DBGridView2.Columns.ItemCols[c].FieldType;
      FooterStyle := DBGridView2.Columns.ItemCols[c].FooterStyle;
      FooterCountStr := DBGridView2.Columns.ItemCols[c].FooterCountStr;
    end;
  end;
  GridView2.NotifyDataChanged;
  DBGridView2.NotifyDataChanged;
end;

end.
