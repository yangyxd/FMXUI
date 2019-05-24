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
  uFrameProgressView in 'uFrameProgressView.pas' {FrameProgressView: TFrame},
  uFrame4 in 'uFrame4.pas' {Frame4: TFrame},
  uFrame4_Page0 in 'uFrame4_Page0.pas' {Frame4_Page0: TFrame},
  uFrame4_Page1 in 'uFrame4_Page1.pas' {Frame4_Page1: TFrame},
  uFrame4_Page2 in 'uFrame4_Page2.pas' {Frame4_Page2: TFrame},
  uFrame4_Page3 in 'uFrame4_Page3.pas' {Frame4_Page3: TFrame},
  uFrameRingView in 'uFrameRingView.pas' {FrameRingView: TFrame},
  uFrameMultiPathView in 'uFrameMultiPathView.pas' {FrameMultiPathView: TFrame},
  uFrameDialog_CustomView in 'uFrameDialog_CustomView.pas' {FrameDialogCustomView: TFrame},
  uFramePopMenu in 'uFramePopMenu.pas' {FramePopMenu: TFrame},
  uFrameImageView in 'uFrameImageView.pas' {FrameImageView: TFrame},
  uFrameListViewGroup in 'uFrameListViewGroup.pas' {FrameListViewGroup: TFrame},
  uFrameImageViewer in 'uFrameImageViewer.pas' {FrameImageViewer: TFrame},
  uFrameGridView in 'uFrameGridView.pas' {FrameGridView: TFrame},
  uFrameCameraViewer in 'uFrameCameraViewer.pas' {FrameCameraViewer: TFrame},
  uFrameVertScrollView in 'uFrameVertScrollView.pas' {FrameVertScrollView: TFrame},
  uFrameHorzScrollView in 'uFrameHorzScrollView.pas' {FrameHorzScrollView: TFrame},
  uFrameDateView in 'uFrameDateView.pas' {FrameDateView: TFrame},
  uFrameDialog_CustomViewVertical in 'uFrameDialog_CustomViewVertical.pas' {FrameDialogCustomViewVertical: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
