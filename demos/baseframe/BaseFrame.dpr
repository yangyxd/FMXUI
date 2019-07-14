program BaseFrame;

uses
  System.StartUpCopy,
  FMX.Forms,
  uBaseFrame in 'uBaseFrame.pas',
  Unit1 in 'Unit1.pas' {Form1},
  uFrame1 in 'uFrame1.pas' {Frame1: TFrame},
  uFrame2 in 'uFrame2.pas' {Frame2: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
