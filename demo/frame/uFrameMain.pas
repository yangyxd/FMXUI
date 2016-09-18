unit uFrameMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame, System.ImageList, FMX.ImgList, UI.Standard, UI.Edit, FMX.Layouts,
  UI.Base;

type
  TFrameMain = class(TFrame)
    LinearLayout1: TLinearLayout;
    TextView1: TTextView;
    tvTitle: TTextView;
    VertScrollBox1: TVertScrollBox;
    LinearLayout2: TLinearLayout;
    ButtonView1: TButtonView;
    ImageList1: TImageList;
    TextView2: TTextView;
    procedure TextView1Click(Sender: TObject);
    procedure TextView2Click(Sender: TObject);
    procedure ButtonView1Click(Sender: TObject);
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
  uFrameMore, uFrameLogin, UI.Dialog;

{ TFrameMain }

procedure TFrameMain.ButtonView1Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('退出登录?')
    .SetNeutralButton('确定',
      procedure (Dialog: IDialog; w: Integer) begin
        StartFrame(TFrmaeLogin, '登录');
        Finish;
      end
    )
    .SetNegativeButton('取消')
    .Show();
end;

procedure TFrameMain.DoShow;
begin
  inherited;
  tvTitle.Text := Title;
end;

procedure TFrameMain.TextView1Click(Sender: TObject);
begin
  Finish;
end;

procedure TFrameMain.TextView2Click(Sender: TObject);
begin
  StartFrame(TFrameMore, '更多选项');
end;

end.
