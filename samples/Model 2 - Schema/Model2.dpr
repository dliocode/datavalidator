program Model2;

uses
  Vcl.Forms,
  UModel2 in 'UModel2.pas' {Form1};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
