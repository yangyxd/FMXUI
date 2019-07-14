unit uFrame2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.Edit, uBaseFrame{Base Frame for project};

type
  TLoginEvent = reference to procedure (AView: TFrame; AUserName, APassword: string);

  TFrame2 = class(TFrame)
    LinearLayout1: TLinearLayout;
    btnBack: TTextView;
    tvTitle: TTextView;
    LinearLayout2: TLinearLayout;
    EditView2: TEditView;
    EditView3: TEditView;
    ButtonView1: TButtonView;
    procedure btnBackClick(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
  private
    { Private declarations }
    FOnLogin: TLoginEvent;
    function GetEnableLogin: Boolean;
    procedure SetEnableLogin(const Value: Boolean);
  protected
    procedure DoCreate(); override;
  public
    { Public declarations }
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
    property EnableLogin: Boolean read GetEnableLogin write SetEnableLogin;
  end;

implementation

{$R *.fmx}

procedure TFrame2.btnBackClick(Sender: TObject);
begin
  Finish;
end;

procedure TFrame2.ButtonView1Click(Sender: TObject);
begin
  if Assigned(OnLogin) then
    OnLogin(Self, EditView2.Text, EditView3.Text);
end;

procedure TFrame2.DoCreate;
begin
  inherited;

  FOnLogin := nil;
  LinearLayout2.Visible := False;
end;

function TFrame2.GetEnableLogin: Boolean;
begin
  Result := LinearLayout2.Visible;
end;

procedure TFrame2.SetEnableLogin(const Value: Boolean);
begin
  LinearLayout2.Visible := Value;
end;

end.
