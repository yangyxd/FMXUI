unit uFrameImageView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Standard, UI.Base, UI.Frame;

type
  TFrameImageView = class(TFrame)
    LinearLayout1: TLinearLayout;
    tvTitle: TTextView;
    ImageView1: TImageView;
    ButtonView1: TButtonView;
    ButtonView2: TButtonView;
    btnBack: TTextView;
    procedure ButtonView1Click(Sender: TObject);
    procedure ButtonView2Click(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  UI.Dialog, TypInfo;

procedure TFrameImageView.btnBackClick(Sender: TObject);
begin
  Finish;
end;

procedure TFrameImageView.ButtonView1Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('ScaleType')
    .SetItems(['None', 'Matrix', 'Center', 'CenterCrop', 'CenterInside', 'FitCenter', 'FitStart', 'FitEnd'],
      procedure (Dialog: IDialog; Which: Integer) begin
        ImageView1.ScaleType := TImageScaleType(Which);
        ButtonView1.Text := 'ScaleType: ' + GetEnumName(Typeinfo(TImageScaleType), Which);
      end
    )
    .Show();
end;

procedure TFrameImageView.ButtonView2Click(Sender: TObject);
begin
  TDialogBuilder.Create(Self)
    .SetTitle('WrapMode')
    .SetItems(['Tile', 'TileOriginal', 'TileStretch'],
      procedure (Dialog: IDialog; Which: Integer) begin
        ImageView1.Image.ItemDefault.Bitmap.WrapMode := TWrapMode(Which);
        ButtonView2.Text := 'WrapMode: ' + GetEnumName(Typeinfo(TWrapMode), Which);
      end
    )
    .Show();
end;

end.
