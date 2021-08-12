unit usample;

interface

uses
  DataValidator,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    LabeledEditNome: TLabeledEdit;
    LabeledEditIdade: TLabeledEdit;
    LabeledEditDataNascimento: TLabeledEdit;
    LabeledEditResultado: TLabeledEdit;
    LabelMatematica: TLabel;
    btnValidarTodos: TButton;
    LabeledEditCNPJ: TLabeledEdit;
    LabeledEditCPF: TLabeledEdit;
    LabeledEditEmail: TLabeledEdit;
    LabeledEditCPFCNPJ: TLabeledEdit;
    Memo1: TMemo;
    btnValidar: TButton;
    procedure btnValidarTodosClick(Sender: TObject);
    procedure btnValidarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Valid: IDataValidatorValueResult;
    procedure ShowResult(const AResult: IDataValidatorResult);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnValidarClick(Sender: TObject);
begin
  ShowResult(Valid.Check);
end;

procedure TForm1.btnValidarTodosClick(Sender: TObject);
begin
  ShowResult(Valid.CheckAll);
end;

procedure TForm1.ShowResult(const AResult: IDataValidatorResult);
begin
  Memo1.Clear;

  if AResult.OK then
    Exit;

  Memo1.Text := AResult.Informations.Message;
  Memo1.Lines.Add(Format('Value validate: %s',[AResult.Informations.GetItem(0).Value]));
  Memo1.Lines.Add(Format('Total errors: %d',[AResult.Informations.Count]));

  AResult.Informations.GetItem(0).OnExecute;
end;

function TForm1.Valid: IDataValidatorValueResult;
begin
  Result :=

  TDataValidator.Values

  .Validate(LabeledEditNome.Text).Execute(LabeledEditNome.SetFocus)
    .Trim
    .&Not.IsEmpty.WithMessage('Informe um nome!') // Não pode ser vazio
    .IsLength(0,10).WithMessage('O nome deve conter no máximo 10 caracteres!')
    .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('Nome com caracteres inválidos!')
  .&End

  .Validate(LabeledEditIdade.Text)
    .Trim
    .&Not.IsEmpty.WithMessage('Informe a idade!') // Não pode ser vazio
    .IsNumeric.WithMessage('Deve ser númerico!')
    .&Not.IsNegative.WithMessage('A idade não pode ser negativo!')
    .&Not.IsZero.WithMessage('A idade não pode ser zero!')
    .&Not.IsLessThan(18).WithMessage('Não é permitido idade menor que 18!')
    .&Not.IsGreaterThan(64).WithMessage('Não é permitido idade maior que 64!')
  .&End

  .Validate(LabeledEditDataNascimento.Text)
    .IsDate.WithMessage('Data de Nascimento inválida!')
    .&Not.IsDateGreaterThan(Now).WithMessage('Data de Nascimento não pode ser maior que e a data atual!')
  .&End

  .Validate(LabeledEditResultado.Text)
    .Trim
    .&Not.IsEmpty.WithMessage('Informe o resultado!') // Não pode ser vazio
    .IsEquals('1').WithMessage('Resultado inválido!')
  .&End

  .Validate(LabeledEditCNPJ.Text)
    .Trim
    .&Not.IsEmpty.WithMessage('Informe o CNPJ!') // Não pode ser vazio
    .IsCNPJ.WithMessage('CNPJ inválido!')
  .&End

  .Validate(LabeledEditCPF.Text)
    .Trim
    .&Not.IsEmpty.WithMessage('Informe o CPF!') // Não pode ser vazio
    .IsCPF.WithMessage('CPF inválido!')
  .&End

  .Validate(LabeledEditCPFCNPJ.Text)
    .Trim
    .&Not.IsEmpty.WithMessage('Informe o CPF ou CNPJ!') // Não pode ser vazio
    .IsCPFCNPJ.WithMessage('CPF/CNPJ inválido!')
  .&End

  .Validate(LabeledEditEmail.Text)
    .Trim
    .&Not.IsEmpty.WithMessage('Informe o Email!') // Não pode ser vazi
    .IsEmail.WithMessage('Email inválido!')
  .&End
end;

end.
