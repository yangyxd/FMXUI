unit Unit2;

interface

uses
  UI.SizeForm, UI.Ani, UI.Frame,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  UI.Standard, FMX.Effects, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm2 = class(TSizeForm)
    layTitle: TLinearLayout;
    tvTitle: TTextView;
    btnMin: TTextView;
    btnClose: TTextView;
    layBackground: TLinearLayout;
    layBody: TRelativeLayout;
    btnOk: TButtonView;
    btnCancel: TButtonView;
    txvMsg: TTextView;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseMouseEnter(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnMinClick(Sender: TObject);
    procedure btnMinMouseLeave(Sender: TObject);
  private
    { Private declarations }
  protected
    function GetShadowBackgroundColor: TAlphaColor; override;
    function GetShadowColor: TAlphaColor; override;
  public
    { Public declarations }
    procedure AniTextViewBackgroundColor(Sender: TObject; IsIn: Boolean);
  end;

function TextDialog(AText: string): TModalResult;

implementation

{$R *.fmx}

function TextDialog(AText: string): TModalResult;
var
  Form2: TForm2;
  LScale: Single;
begin
  Application.CreateForm(TForm2, Form2);
  LScale := Form2.GetSceneScale;
  Form2.txvMsg.Text := AText;
  Form2.Left := Application.MainForm.Left + (Round(Application.MainForm.Width * LScale) - Form2.Width) div 2;
  Form2.Top := Application.MainForm.Top + (Round(Application.MainForm.Height * LScale) - Form2.Height) div 2;
  Form2.btnOk.SetFocus;
  Result := Form2.ShowModal;
  Form2.Free;
end;

procedure TForm2.AniTextViewBackgroundColor(Sender: TObject; IsIn: Boolean);
var
  SrcColor, DsetColor: TAlphaColor;
begin
  SrcColor := TTextView(Sender).Background.ItemHovered.Color;
  DsetColor := SrcColor;
  if IsIn then begin
    TAlphaColorRec(DsetColor).A := $FF;
  end else begin
    TAlphaColorRec(DsetColor).A := $0;
  end;
  TFrameAnimator.AnimateColor(TTextView(Sender), 'Background.ItemHovered.Color', DsetColor);
end;

procedure TForm2.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TForm2.btnCloseMouseEnter(Sender: TObject);
begin
  AniTextViewBackgroundColor(Sender, True);
end;

procedure TForm2.btnMinClick(Sender: TObject);
begin
  ShowMin();
end;

procedure TForm2.btnMinMouseLeave(Sender: TObject);
begin
  AniTextViewBackgroundColor(Sender, False);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  tvTitle.Text := Self.Caption;

  Resizable := False;
  ShowShadow := True;  // ´ò¿ªÒõÓ°
end;

function TForm2.GetShadowBackgroundColor: TAlphaColor;
begin
  Result := Fill.Color;
end;

function TForm2.GetShadowColor: TAlphaColor;
begin
  Result := $7f101010;
end;

end.
