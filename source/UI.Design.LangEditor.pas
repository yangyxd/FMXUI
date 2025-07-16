unit UI.Design.LangEditor;

interface

uses
  UI.Base,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox, UI.Standard, UI.Grid;

type
  TLangDesigner = class(TForm)
    cbLangs: TComboBox;
    Label1: TLabel;
    Add: TButton;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    lvItems: TStringGridView;
    Button2: TButton;
    ButtonView1: TButtonView;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbLangsChange(Sender: TObject);
    procedure lvItemsCellEditDone(Sender: TObject; const ACell: TGridCell;
      const Value: string);
  private
    { Private declarations }
    FLang: TLangManager;
    FNames: TStrings;
    procedure InitForm;
    procedure InitValue;
    procedure InitNames;
  public
    { Public declarations }
    property Lang: TLangManager read FLang write FLang;
  end;

var
  LangDesigner: TLangDesigner;

implementation

{$R *.fmx}
{$WARN SYMBOL_DEPRECATED OFF}

procedure TLangDesigner.AddClick(Sender: TObject);
var
  S: string;
begin
  S := Trim(InputBox('Add Language Code', 'Language: ', ''));
  if S = '' then Exit;
  if FLang.ExistLang(S) then Exit;
  FLang.AddLang(S);
  cbLangs.Items.Add(S);
end;

procedure TLangDesigner.Button1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  if not Assigned(FLang) then Exit;
  try
    FLang.LoadFromFile(OpenDialog1.FileName);
  finally
    InitForm;
  end;    
end;

procedure TLangDesigner.ButtonView1Click(Sender: TObject);
var
  S: string;
begin
  S := Trim(InputBox('Add Name', 'Name: ', ''));
  if S = '' then Exit;
  if FLang.ExistName(S) then Exit; 
  FLang.SetLangText(S, '');
  lvItems.RowCount := lvItems.RowCount + 1;
  lvItems.FixedCells[0, lvItems.RowCount - 1] := S;
end;

procedure TLangDesigner.ButtonView2Click(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  if cbLangs.ItemIndex < 0 then begin
    ShowMessage('Please select the language first!');
    Exit;
  end;
  I := lvItems.SelectIndex;
  ShowMessage(IntToStr(lvItems.SelectIndex));
  if lvItems.SelectIndex < 0 then Exit;
  S := lvItems.FixedCells[0, I];
  if S = '' then Exit;
  FLang.DeleteName(cbLangs.Items[cbLangs.ItemIndex], S);
  FNames.Delete(I);
  InitNames();
  InitValue();
end;

procedure TLangDesigner.cbLangsChange(Sender: TObject);
begin
  InitValue();
end;

procedure TLangDesigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FNames);
end;

procedure TLangDesigner.FormShow(Sender: TObject);
begin
  InitForm;
end;

procedure TLangDesigner.InitForm;
var
  S: TStrings;
begin
  if not Assigned(FLang) then Exit;
  S := FLang.LangsList;
  try
    cbLangs.Items.Assign(S);
  finally
    S.Free;
  end;
  FreeAndNil(FNames);
  FNames := FLang.NamesList;
  cbLangs.ItemIndex := -1;
  if cbLangs.Count > 0 then cbLangs.ItemIndex := 0;
  InitNames();
end;

procedure TLangDesigner.InitNames;
var
  I: Integer;
begin
  lvItems.RowCount := FNames.Count;
  lvItems.BeginUpdate;
  try
    if FNames.Count = 0 then begin
      FNames.Add('Source');
      FLang.SetLangText(FNames[0], '');
    end;
    for I := 0 to FNames.Count - 1 do
      lvItems.FixedCells[0, I] := FNames[I];
  finally
    lvItems.EndUpdate;
  end;
end;

procedure TLangDesigner.InitValue;
var
  I: Integer;
  S: string;
begin
  if cbLangs.ItemIndex < 0 then Exit;
  lvItems.BeginUpdate;
  try
    S := cbLangs.Items[cbLangs.ItemIndex];
    for I := 0 to FNames.Count - 1 do
      lvItems.Cells[0, I] := FLang.GetLangText(S, FNames[I], '');
  finally
    lvItems.EndUpdate;
  end;
end;

procedure TLangDesigner.lvItemsCellEditDone(Sender: TObject;
  const ACell: TGridCell; const Value: string);
begin
  if cbLangs.ItemIndex < 0 then Exit;
  FLang.SetLangText(cbLangs.Items[cbLangs.ItemIndex], lvItems.FixedCells[0, ACell.Row], Value);
end;

end.
