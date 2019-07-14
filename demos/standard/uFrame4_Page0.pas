unit uFrame4_Page0;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UI.Frame,
  UI.Base, UI.Standard, UI.ListView;

type
  TFrame4_Page0 = class(TFrame)
    ListViewEx1: TListViewEx;
  private
    { Private declarations }
    FAdapter: TStringsListSingleAdapter;
  protected
    procedure DoCreate(); override;
    procedure DoShow(); override;
    procedure DoFree(); override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

{ TFrame5 }

procedure TFrame4_Page0.DoCreate;
var
  I: Integer;
begin
  inherited;
  FAdapter := TStringsListSingleAdapter.Create();
  for I := 0 to 30 do
    FAdapter.Add(Self.ClassName + IntToStr(I));
  ListViewEx1.Adapter := FAdapter;
end;

procedure TFrame4_Page0.DoFree;
begin
  inherited;
  Hint('DoFree');
end;

procedure TFrame4_Page0.DoShow;
begin
  inherited;
end;

end.
