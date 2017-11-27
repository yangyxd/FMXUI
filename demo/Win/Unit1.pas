unit Unit1;

interface

uses
  UI.SizeForm, UI.Ani, UI.Frame,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Base,
  UI.Standard, FMX.Effects, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TSizeForm)
    layTitle: TLinearLayout;
    tvTitle: TTextView;
    btnMin: TTextView;
    btnMax: TTextView;
    btnClose: TTextView;
    layBackground: TLinearLayout;
    btnRestore: TTextView;
    layBody: TRelativeLayout;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseMouseEnter(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnMaxClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure btnMinClick(Sender: TObject);
    procedure btnMinMouseLeave(Sender: TObject);
    procedure layTitleDblClick(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
  private
    { Private declarations }
  protected
    function GetShadowBackgroundColor: TAlphaColor; override;
    function GetShadowColor: TAlphaColor; override;
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

procedure TForm1.btnMaxClick(Sender: TObject);
begin
  btnMax.Visible := False;
  btnRestore.Visible := True;
  ShowMax();
  layTitle.CaptureDragForm := False;
end;

procedure TForm1.btnMinClick(Sender: TObject);
begin
  ShowMin();
end;

procedure TForm1.btnMinMouseLeave(Sender: TObject);
begin
  AniTextViewBackgroundColor(Sender, False);
end;

procedure TForm1.btnRestoreClick(Sender: TObject);
begin
  btnRestore.Visible := False;
  btnMax.Visible := True;
  ShowReSize();
  layTitle.CaptureDragForm := True;
end;

procedure TForm1.ButtonView1Click(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  tvTitle.Text := Self.Caption;
  ShowShadow := True;  // ´ò¿ªÒõÓ°
end;

function TForm1.GetShadowBackgroundColor: TAlphaColor;
begin
  Result := Fill.Color;
end;

function TForm1.GetShadowColor: TAlphaColor;
begin
  Result := $7f101010;
end;

procedure TForm1.layTitleDblClick(Sender: TObject);
begin
  if btnMax.Visible then begin
    btnMaxClick(Sender);
  end else begin
    TFrameAnimator.DelayExecute(Self,
      procedure (Sender: TObject)
      begin
        btnRestoreClick(Sender);
      end, 0.08);
  end;
end;

end.
