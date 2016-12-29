program FMXUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uFrame2 in 'uFrame2.pas' {Frame2: TFrame},
  uFrame1 in 'uFrame1.pas' {Frame1: TFrame},
  uFrame3 in 'uFrame3.pas' {Frame3: TFrame},
  uFrameDialog in 'uFrameDialog.pas' {FrmaeDialog: TFrame},
  uFrameListViewTest in 'uFrameListViewTest.pas' {FrameListViewTest: TFrame},
  uFrameMore in 'uFrameMore.pas' {FrameMore: TFrame},
  ui_CustomListView in 'ui_CustomListView.pas' {CustomListview: TFrame},
  ui_CustomListView_ListItem in 'ui_CustomListView_ListItem.pas' {CustomListView_ListItem: TFrame},
  ui_PopupMenu in 'ui_PopupMenu.pas' {MainPopupMenu: TFrame},
  uFrameProgressView in 'uFrameProgressView.pas' {FrameProgressView: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
