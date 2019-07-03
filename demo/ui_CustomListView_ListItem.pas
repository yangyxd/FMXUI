unit ui_CustomListView_ListItem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Base, UI.Standard;

type
  TCustomListView_ListItem = class(TFrame)
    RelativeLayout1: TRelativeLayout;
    View1: TView;
    TextView1: TTextView;
    TextView2: TTextView;
    View2: TView;
    BadgeView1: TBadgeView;
    BadgeView2: TBadgeView;
    BadgeView3: TBadgeView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.

