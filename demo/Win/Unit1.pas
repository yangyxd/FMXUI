unit Unit1;

interface

uses
  UI.SizeForm, UI.Ani, UI.Frame,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  UI.Standard, FMX.Effects;

type
  TForm1 = class(TSizeForm)
    layTitle: TLinearLayout;
    tvTitle: TTextView;
    btnMin: TTextView;
    btnMax: TTextView;
    btnClose: TTextView;
    layBackground: TLinearLayout;
    ShadowEffect1: TShadowEffect;
    btnRestore: TTextView;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseMouseEnter(Sender: TObject);
    procedure btnCloseMouseLeave(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AniTextViewBackgroundColor(Sender: TObject; IsIn: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.AniTextViewBackgroundColor(Sender: TObject; IsIn: Boolean);
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

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnCloseMouseEnter(Sender: TObject);
begin
  AniTextViewBackgroundColor(Sender, True);
end;

procedure TForm1.btnCloseMouseLeave(Sender: TObject);
begin
  AniTextViewBackgroundColor(Sender, False);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  tvTitle.Text := Self.Caption;
end;

end.
