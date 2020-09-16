unit BaseListItem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Base, UI.Standard;

type
  TBaseListItemFrame = class(FMX.Forms.TFrame)
  private
    { Private declarations }
    FItemIndex: Integer;
  protected
    function GetImageView: TImageView; virtual; abstract;
    function GetTextView: TTextView; virtual; abstract;
  public
    { Public declarations }
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property Image: TImageView read GetImageView;
    property Text: TTextView read GetTextView;
  end;

  TFrame = class(TBaseListItemFrame);
  TFrameClass = type of TBaseListItemFrame;

implementation

end.
