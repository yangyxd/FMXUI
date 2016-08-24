program UIDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'demo\Unit1.pas' {Form1};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
