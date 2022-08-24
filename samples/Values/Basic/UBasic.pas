unit UBasic;

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
    EditEmail: TLabeledEdit;
    EditFone: TLabeledEdit;
    EditFone2: TLabeledEdit;
    EditFone3: TLabeledEdit;
    EditDataNascimento: TLabeledEdit;
    btnCheckAllFirst: TButton;
    btnCheckAll: TButton;
    procedure btnLimparClick(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnCheckAllFirstClick(Sender: TObject);
  private
    { Private declarations }
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
        .Trim
        .&Not.IsEmpty.WithMessage('O campo ${name} não pode ser vazio!')
        .&Not.IsInteger.WithMessage('O campo ${name} não pode conter números!')
        .IsLength(3, 0).WithMessage('Informe um ${name} maior que 3 caracteres!')
      .&End

      .Validate(EditDataNascimento.Text, 'Data Nascimento').Execute(EditDataNascimento.SetFocus)
        .Trim
        .&Not.IsEmpty.WithMessage('O campo ${name} não pode ser vazio!')
        .IsDate.WithMessage('A ${name}(${value}) informada é inválida!')
        .&Not.IsDateGreaterThan(Now).WithMessage('A ${name}(${value}) não pode ser maior que a data atual!')
      .&End

      .Validate(EditEmail.Text, 'E-mail').Execute(EditEmail.SetFocus)
        .IsOptional // Não é obrigatório, mas se tiver valor deve validar
        .IsEmail.WithMessage('O ${name}(${value}) informado é inválido!')
      .&End

      .Validate([EditFone.Text, EditFone2.Text, EditFone3.Text], 'Fone').Execute(EditFone.SetFocus)
        .IsOptional
        .IsPhoneNumber(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('O ${name}(${value}) informado não é válido!')
      .&End
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

end.
