program Model1;

uses
  Vcl.Forms,
  usample in 'usample.pas' {Form1},
  DataValidator.Base.Intf in '..\..\src\core\DataValidator.Base.Intf.pas',
  DataValidator.Base in '..\..\src\core\DataValidator.Base.pas',
  DataValidator.Information.Intf in '..\..\src\core\DataValidator.Information.Intf.pas',
  DataValidator.Information in '..\..\src\core\DataValidator.Information.pas',
  DataValidator.Intf in '..\..\src\core\DataValidator.Intf.pas',
  DataValidator.ItemBase.Intf in '..\..\src\core\DataValidator.ItemBase.Intf.pas',
  DataValidator.ItemBase in '..\..\src\core\DataValidator.ItemBase.pas',
  DataValidator.ItemBase.Sanitizer in '..\..\src\core\DataValidator.ItemBase.Sanitizer.pas',
  DataValidator.JSON.Intf in '..\..\src\core\DataValidator.JSON.Intf.pas',
  DataValidator.JSON in '..\..\src\core\DataValidator.JSON.pas',
  DataValidator in '..\..\src\core\DataValidator.pas',
  DataValidator.Result.Intf in '..\..\src\core\DataValidator.Result.Intf.pas',
  DataValidator.Result in '..\..\src\core\DataValidator.Result.pas',
  DataValidator.Schema in '..\..\src\core\DataValidator.Schema.pas',
  DataValidator.Types in '..\..\src\core\DataValidator.Types.pas',
  DataValidator.Values.Intf in '..\..\src\core\DataValidator.Values.Intf.pas',
  DataValidator.Values in '..\..\src\core\DataValidator.Values.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
