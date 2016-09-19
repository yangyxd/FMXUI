program FrameDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uFrameLogin in 'uFrameLogin.pas' {FrmaeLogin: TFrame},
  uFrameMain in 'uFrameMain.pas' {FrameMain: TFrame},
  uFrameMore in 'uFrameMore.pas' {FrameMore: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
