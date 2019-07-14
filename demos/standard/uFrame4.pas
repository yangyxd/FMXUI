unit uFrame4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Standard, UI.Base;

type
  TFrame4 = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    LinearLayout3: TLinearLayout;
    TextView1: TTextView;
    TextView2: TTextView;
    TextView3: TTextView;
    TextView4: TTextView;
    btnBack: TTextView;
    procedure TextView1Click(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
  private
    { Private declarations }
    FViews: array of TFrameView;
    LastPage: Integer;
  protected
    procedure DoShow(); override;
    procedure DoCreate(); override;
    procedure DoFree(); override;
    procedure DoFinish(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  uFrame4_Page1,
  uFrame4_Page2,
  uFrame4_Page3,
  uFrame4_Page0;

procedure TFrame4.btnBackClick(Sender: TObject);
begin
  Finish();
end;

procedure TFrame4.DoCreate;
begin
  inherited;
  SetLength(FViews, 4);
  FViews[0] := TFrame4_Page0.ShowFrame(Self);
  LastPage := 0;
end;

procedure TFrame4.DoFinish;
begin
  inherited;

end;

procedure TFrame4.DoFree;
begin
  inherited;

end;

procedure TFrame4.DoShow;
begin
  inherited;

end;

procedure TFrame4.TextView1Click(Sender: TObject);
var
  Index: Integer;
  Cls: TFrameViewClass;
begin
  TTextView(Sender).Checked := True;

  Index := TTextView(Sender).Tag;
  if Index = LastPage then
    Exit;

  case Index of
    0: Cls := TFrame4_Page0;
    1: Cls := TFrame4_Page1;
    2: Cls := TFrame4_Page2;
    3: Cls := TFrame4_Page3;
  else
    Exit;
  end;

  if LastPage < TTextView(Sender).Tag then begin
    FViews[LastPage].Hide(TFrameAniType.MoveInOut, False);
    if Assigned(FViews[Index]) then
      FViews[Index].Show(TFrameAniType.MoveInOut, nil, False)
    else
      FViews[Index] := Cls.ShowFrame(Self, '', TFrameAniType.MoveInOut, False);
  end else begin
    FViews[LastPage].Hide(TFrameAniType.MoveInOut, True);
    if Assigned(FViews[Index]) then
      FViews[Index].Show(TFrameAniType.MoveInOut, nil, True)
    else
      FViews[Index] := Cls.ShowFrame(Self, '', TFrameAniType.MoveInOut, True);
  end;

  LastPage := Index;
end;

end.
