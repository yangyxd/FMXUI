unit ListItem_Image;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls,
  UI.Base, UI.Standard, BaseListItem;

type
  TfrmListItem_Image = class(TFrame)
    ImageView1: TImageView;
    TextView1: TTextView;
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

{ TfrmListItem_Image }

function TfrmListItem_Image.GetImageView: TImageView;
begin
  Result := ImageView1;
end;

function TfrmListItem_Image.GetTextView: TTextView;
begin
  Result := TextView1;
end;

end.
