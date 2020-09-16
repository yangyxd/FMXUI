unit ListItem_TextImage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls,
  UI.Base, UI.Standard, BaseListItem;

type
  TfrmListItem_TextImage = class(TFrame)
    LinearLayout1: TLinearLayout;
    ImageView1: TImageView;
    TextView1: TTextView;
    TextView2: TTextView;
    TextView3: TTextView;
    View1: TView;
  private
    { Private declarations }
  protected
    function GetImageView: TImageView; override;
    function GetTextView: TTextView; override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

{ TfrmListItem_TextImage }

function TfrmListItem_TextImage.GetImageView: TImageView;
begin
  Result := ImageView1;
end;

function TfrmListItem_TextImage.GetTextView: TTextView;
begin
  Result := TextView1;
end;

end.
