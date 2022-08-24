unit USchema;

interface

uses
  DataValidator,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    btnLimpar: TButton;
    btnCheck: TButton;
    Panel2: TPanel;
    EditCodigo: TLabeledEdit;
    EditNome: TLabeledEdit;
    btnCheckAllFirst: TButton;
    btnCheckAll: TButton;
    EditNomeMeio: TLabeledEdit;
    EditApelido: TLabeledEdit;
    procedure btnLimparClick(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnCheckAllFirstClick(Sender: TObject);
  private
    { Private declarations }
    function SchemaNome: IDataValidatorSchemaContext;
    function Validation: IDataValidatorValueResult;
    procedure ValidationResult(const AResult: IDataValidatorResult);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnLimparClick(Sender: TObject);
var
  I : Integer;
begin
  for I := 0 to Pred(Self.ComponentCount) do
    if (Self.Components[i] is TLabeledEdit) then
      (Self.Components[i] as TLabeledEdit).Clear;
end;

procedure TForm2.btnCheckClick(Sender: TObject);
var
  LValidatorResult: IDataValidatorResult;
begin
  LValidatorResult := Validation.Check;
  ValidationResult(LValidatorResult);
end;

procedure TForm2.btnCheckAllClick(Sender: TObject);
var
  LValidatorResult: IDataValidatorResult;
begin
  LValidatorResult := Validation.CheckAll();
  ValidationResult(LValidatorResult);
end;

procedure TForm2.btnCheckAllFirstClick(Sender: TObject);
var
  LValidatorResult: IDataValidatorResult;
begin
  LValidatorResult := Validation.CheckAll(TDataValidatorCheckAll.tcFirst);
  ValidationResult(LValidatorResult);
end;

procedure TForm2.ValidationResult(const AResult: IDataValidatorResult);
begin
  if AResult.OK then
     ShowMessage('Tudo certo!')
  else
  begin
    ShowMessage('Problema na validação: ' + sLineBreak + sLineBreak + AResult.Informations.Message);
    AResult.Informations.GetItem(0).OnExecute;
  end;
end;

function TForm2.Validation: IDataValidatorValueResult;
begin
  Result :=
    TDataValidator.Values
      .Validate(EditCodigo.Text, 'Código').Execute(EditCodigo.SetFocus)
        .Trim
        .&Not.IsEmpty.WithMessage('O campo ${name} não pode ser vazio!')
        .IsInteger.WithMessage('Informe somente números no campo ${name}!')
        .IsGreaterThan(0).WithMessage('Informe um ${name} maior que zero!')
      .&End

      .Validate(EditNome.Text, 'Nome').Execute(EditNome.SetFocus)
        .AddSchema(SchemaNome)
      .&End

      .Validate(EditNomeMeio.Text, 'Nome do Meio').Execute(EditNomeMeio.SetFocus)
        .AddSchema(SchemaNome)
      .&End

      .Validate(EditApelido.Text, 'Apelido').Execute(EditApelido.SetFocus)
        .AddSchema(SchemaNome)
      .&End
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

function TForm2.SchemaNome: IDataValidatorSchemaContext;
begin
  Result :=
  TDataValidator.Schema
    .Validate
      .Trim
      .&Not.IsEmpty.WithMessage('O campo ${name} não pode ser vazio!')
      .&Not.IsInteger.WithMessage('O campo ${name} não pode conter números!')
      .IsLength(3, 0).WithMessage('Informe um ${name} maior que 3 caracteres!')
   .&End;
end;

end.
