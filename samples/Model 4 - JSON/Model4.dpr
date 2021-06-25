program Model4;

uses
  Vcl.Forms,
  UModel4 in 'UModel4.pas' {Form1};

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
