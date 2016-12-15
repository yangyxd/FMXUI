program ListViewDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uFrameListView in 'uFrameListView.pas' {FrameListView: TFrame},
  ui_PopupMenu in 'ui_PopupMenu.pas' {MainPopupMenu: TFrame},
  ui_CustomListView in 'ui_CustomListView.pas' {CustomListview: TFrame},
  ui_CustomListView_ListItem in 'ui_CustomListView_ListItem.pas' {CustomListView_ListItem: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
