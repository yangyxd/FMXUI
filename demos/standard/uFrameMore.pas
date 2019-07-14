unit uFrameMore;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, UI.Standard, FMX.Layouts, UI.Base, UI.Frame,
  UI.Calendar,
  UI.Edit, FMX.DateTimeCtrls;

type
  TFrameMore = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    VertScrollBox1: TVertScrollBox;
    LinearLayout2: TLinearLayout;
    ImageList1: TImageList;
    TextView2: TTextView;
    LinearLayout3: TLinearLayout;
    ButtonView1: TButtonView;
    ButtonView2: TButtonView;
    btnBack: TTextView;
    DateView1: TDateView;
    TimeView1: TTimeView;
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure TextView2LinkClick(Sender: TObject; const Text, URL: string);
  private
    { Private declarations }
  protected
    procedure DoShow(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  UI.Dialog, UI.Utils, uFrameListViewTest;

procedure TFrameMore.btnBackClick(Sender: TObject);
begin
  Finish();
end;

procedure TFrameMore.ButtonView1Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('测试')
    .SetMessage('我是消息内容。我是消息内容。我是消息内容。我是消息内容。abcd123456我是消息内容。我是消息内容。我是消息内容。我是消息内容。abcd123456000')
    .SetPositiveButton('FMXUI')
    .SetNeutralButton('确定')
    .SetNegativeButton('取消')
    .Show();
end;

procedure TFrameMore.ButtonView2Click(Sender: TObject);
begin
  StartFrame(TFrameListViewTest, 'ListView 测试');
  Finish;
end;

procedure TFrameMore.DoShow;
begin
  inherited;
  tvTitle.Text := Title;
end;

procedure TFrameMore.TextView2LinkClick(Sender: TObject; const Text,
  URL: string);
begin
  OpenURL(URL);
end;

end.
