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
    ButtonView2: TButtonView;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
  private
    { Private declarations }
    FLang: TLangManager;
    FNames: TStrings;
    procedure InitForm;
    procedure InitValue;
  public
    { Public declarations }
    property Lang: TLangManager read FLang write FLang;
  end;

var
  LangDesigner: TLangDesigner;

implementation

{$R *.fmx}

procedure TLangDesigner.AddClick(Sender: TObject);
var
  S: string;
begin
  S := Trim(InputBox('Add Language Code', '', ''));
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
  S := Trim(InputBox('Add Language Code', '', ''));
  if S = '' then Exit;
  if FLang.ExistName(S) then Exit; 
  FLang.SetLangText(S, '');
  lvItems.RowCount := lvItems.RowCount + 1;
  lvItems.Cells[0, lvItems.RowCount - 1] := '';
end;

procedure TLangDesigner.ButtonView2Click(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  if cbLangs.Text = '' then Exit;  
  if lvItems.ItemIndex < 0 then Exit;
  S := lvItems.Cells[0, lvItems.ItemIndex];
  if S = '' then Exit;
  FLang.DeleteName(cbLangs.Text, S);
  FNames.Delete(lvItems.ItemIndex);
  lvItems.RowCount := lvItems.RowCount - 1;
  lvItems.BeginUpdate;
  try
    for I := 0 to FNames.Count - 1 do
      lvItems.Cells[0, I] := FNames[I];
  finally
    lvItems.EndUpdate;
  end;
end;

procedure TLangDesigner.FormCreate(Sender: TObject);
begin
  InitForm;
end;

procedure TLangDesigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FNames);
end;

procedure TLangDesigner.InitForm;
var
  S: TStrings;
  I: Integer;
begin
  if not Assigned(FLang) then Exit;
  S := FLang.LangsList;
  try
    cbLangs.Assign(S);
  finally
    S.Free;
  end;
  FreeAndNil(FNames);
  FNames := FLang.NamesList;
  if cbLangs.Count > 0 then cbLangs.ItemIndex := 0;  
  lvItems.RowCount := FNames.Count;
  lvItems.BeginUpdate;
  try
    for I := 0 to FNames.Count - 1 do
      lvItems.Cells[0, I] := FNames[I];
  finally
    lvItems.EndUpdate;
  end;
end;

procedure TLangDesigner.InitValue;
var
  I: Integer;
  S: string;
begin
  lvItems.BeginUpdate;
  try
    S := cbLangs.Text;
    for I := 0 to FNames.Count - 1 do
      lvItems.Cells[1, I] := FLang.GetLangText(S, FNames[I], '');
  finally
    lvItems.EndUpdate;
  end;
end;

end.
