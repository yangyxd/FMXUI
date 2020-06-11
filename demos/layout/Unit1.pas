unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UI.Standard,
  UI.Base, UI.Frame;

type
  TForm1 = class(TForm)
    LinearLayout3: TLinearLayout;
    TextView1: TTextView;
    TextView2: TTextView;
    procedure TextView1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FViews: array of TFrameView;
    LastPage: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  FrameRelativeLayoutFMXUIViews, FrameRelativeLayoutFMXControls;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLength(FViews, 2);
  FViews[0] := TFrameRelativeFMXUI.ShowFrame(Self);
  LastPage := 0;
end;

procedure TForm1.TextView1Click(Sender: TObject);
var
  Index: Integer;
  Cls: TFrameViewClass;
begin
  TTextView(Sender).Checked := True;

  Index := TTextView(Sender).Tag;
  if Index = LastPage then
    Exit;

  case Index of
    0: Cls := TFrameRelativeFMXUI;
    1: Cls := TFrameRelativeFMX;
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
