unit uFrameProgressView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, UI.Base, UI.Standard, UI.ListView, System.ImageList, FMX.ImgList,
  FMX.Objects;

type
  TFrameProgressView = class(TFrame)
    LinearLayout1: TLinearLayout;
    TextView1: TTextView;
    tvTitle: TTextView;
    TextView2: TTextView;
    ImageList1: TImageList;
    ProgressView1: TProgressView;
    Timer1: TTimer;
    ProgressView2: TProgressView;
    ProgressView3: TProgressView;
    ProgressView4: TProgressView;
    procedure TextView1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ProgressView1Click(Sender: TObject);
  private
    { Private declarations }
    Adapter: TStringsListAdapter;
  protected
    procedure DoShow(); override;
  public
    { Public declarations }
    procedure AddItems(const Count: Integer);
  end;

implementation

{$R *.fmx}

procedure TFrameProgressView.AddItems(const Count: Integer);
var
  I, LastCount: Integer;
begin
  LastCount := Adapter.Count;
  Adapter.SetArrayLength(Adapter.Count + Count);
  for I := LastCount to Adapter.Count - 1 do
    Adapter.Items[I] := Format('列表项列表项列表项列表项列表项列表项列表项列表项列表项列 %d', [I]);

  Adapter.NotifyDataChanged;
end;

procedure TFrameProgressView.DoShow;
begin
  inherited;
end;

procedure TFrameProgressView.ProgressView1Click(Sender: TObject);
begin
  Timer1Timer(Timer1);
end;

procedure TFrameProgressView.TextView1Click(Sender: TObject);
begin
  Finish();
end;

procedure TFrameProgressView.Timer1Timer(Sender: TObject);
begin
  ProgressView1.Value := ProgressView1.Value + 1;
  if ProgressView1.Value >= ProgressView1.Max then
    ProgressView1.Value := ProgressView1.Min;
  ProgressView2.Value := ProgressView2.Value + 1;
  if ProgressView2.Value >= ProgressView2.Max then
    ProgressView2.Value := ProgressView2.Min;
  ProgressView3.Value := ProgressView3.Value + 1;
  if ProgressView3.Value >= ProgressView3.Max then
    ProgressView3.Value := ProgressView3.Min;
  ProgressView4.Value := ProgressView4.Value + 1;
  if ProgressView4.Value >= ProgressView4.Max then
    ProgressView4.Value := ProgressView4.Min;
end;

end.
