unit UI.Design.LangEditor;

interface

uses
  UI.Base,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Platform, FMX.Dialogs,
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
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbLangsChange(Sender: TObject);
    procedure lvItemsCellEditDone(Sender: TObject; const ACell: TGridCell;
      const Value: string);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FLang: TLangManager;
    FNames: TStrings;
    FIsZh: Boolean;
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

procedure TLangDesigner.Button3Click(Sender: TObject);

  procedure StoreControls(AContainer: TFmxObject);
  var
    I: Integer;
    V: ILang;
    Control: TControl;
    AName, ALangName: string;
  begin
    for I := 0 to TFmxObject(AContainer).ChildrenCount - 1 do begin
      if TFmxObject(AContainer).Children[I] is TControl then begin
        Control := TControl(TFmxObject(AContainer).Children[I]);
        AName := Control.Name;
        if Supports(Control, ILang, V) then begin
          ALangName := V.LangName;
          if ALangName <> '' then AName := ALangName;
        end;
        if (AName <> '') and (not FLang.ExistName(AName)) then
          FLang.SetLangText(AName, '');
        StoreControls(Control);
      end;
    end;
  end;

var
  sMsg: string;
  P: TCustomForm;
begin
  if FIsZh then
    sMsg := '此操作将自动加载Lang组件所属窗口中的名称列表，是否继续?'
  else
    sMsg := 'This operation will automatically load the list of names in the window to which the Lang component belongs. Do you want to continue?';
  if MessageDlg(sMsg, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], 0) <> 1 then
    Exit;
  P := FLang.GetParentForm;
  if not Assigned(P) then Exit;
  P.BeginUpdate;
  try
    StoreControls(P);
    FreeAndNil(FNames);
    FNames := FLang.NamesList.ToStrings;
    InitNames();
    InitValue();
  finally
    P.EndUpdate;
  end;
end;

procedure TLangDesigner.ButtonView1Click(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  S := Trim(InputBox('Add Name', 'Name: ', ''));
  if S = '' then Exit;
  if FLang.ExistName(S) then begin
    I := FNames.IndexOf(S);
    if I >= 0 then
      lvItems.SelectionAnchor := I;
    Exit;
  end;
  FNames.Add(S);
  FLang.SetLangText(S, '');
  lvItems.RowCount := lvItems.RowCount + 1;
  lvItems.FixedCells[0, lvItems.RowCount - 1] := S;
end;

procedure TLangDesigner.ButtonView2Click(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  I := lvItems.SelectionAnchor;
  if (I < 0) or (I >= FNames.Count) then Exit;
  S := lvItems.FixedCells[0, I];
  if S = '' then Exit;
  FLang.DeleteName(S);
  FNames.Delete(I);
  InitNames();
  InitValue();
end;

procedure TLangDesigner.cbLangsChange(Sender: TObject);
begin
  InitValue();
end;

procedure TLangDesigner.FormCreate(Sender: TObject);
var
  LocaleSvc: IFMXLocaleService;
begin
  try
    FIsZh := TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, LocaleSvc) and (LocaleSvc.GetCurrentLangID = 'zh');
  except
    FIsZh := False;
  end;
end;

procedure TLangDesigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FNames);
end;

procedure TLangDesigner.FormShow(Sender: TObject);
begin
  if FIsZh then begin
    Label1.Text := '语言代码:';
    Add.Text := '添加';
    Button1.Text := '加载文件...';
    Button2.Text := '保存文件...';
    Button3.Text := '自动加载名称';
  end;
  InitForm;
end;

procedure TLangDesigner.InitForm;
begin
  if not Assigned(FLang) then Exit;
  cbLangs.Items.Assign(FLang.LangsList.ToStrings);
  FreeAndNil(FNames);
  FNames := FLang.NamesList.ToStrings;
  cbLangs.ItemIndex := -1;
  if cbLangs.Count > 0 then cbLangs.ItemIndex := 0;
  InitNames();
end;

procedure TLangDesigner.InitNames;
var
  I: Integer;
begin
  lvItems.BeginUpdate;
  try
    lvItems.RowCount := FNames.Count;
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
