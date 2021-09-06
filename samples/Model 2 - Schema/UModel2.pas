unit UModel2;

interface

uses
  DataValidator,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    LabeledEditNome: TLabeledEdit;
    btnValidarTodos: TButton;
    Memo1: TMemo;
    btnValidar: TButton;
    LabeledEditApelido: TLabeledEdit;
    LabeledEditRazaoSocial: TLabeledEdit;
    procedure btnValidarTodosClick(Sender: TObject);
    procedure btnValidarClick(Sender: TObject);
  private
    { Private declarations }
    function SchemaNome(const AField: string): IDataValidatorSchemaContext;
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

function TForm1.SchemaNome(const AField: string): IDataValidatorSchemaContext;
begin
  Result :=
  TDataValidator.Schema
    .Validate
      .Trim
      .&Not.IsEmpty.WithMessage(Format('Preencha o campo %s !', [AField])) // Não pode ser vazio
      .IsLength(0, 10).WithMessage(Format('O campo %s deve conter no máximo 10 caracteres!', [AField]))
      .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage(Format('O campo %s possui caracteres inválidos!', [AField]))
    .&End;
end;

procedure TForm1.ShowResult(const AResult: IDataValidatorResult);
begin
  Memo1.Clear;

  if AResult.OK then
    Exit;

  Memo1.Text := AResult.Informations.Message;
  Memo1.Lines.Add(Format('Value validate: %s', [AResult.Informations.GetItem(0).Value]));
  Memo1.Lines.Add(Format('Total errors: %d', [AResult.Informations.Count]));
end;

function TForm1.Valid: IDataValidatorValueResult;
begin
  Result := TDataValidator.Values

  .Validate(LabeledEditNome.Text)
    .AddSchema(SchemaNome('Nome'))
  .&End

  .Validate(LabeledEditApelido.Text)
    .AddSchema(SchemaNome('Apelido'))
    .IsUppercase.WithMessage('O apelido (${value}) deve ser digitado tudo em maiúscula!') // Add outra validação
  .&End

  .Validate(LabeledEditRazaoSocial.Text)
    .AddSchema(SchemaNome('Razao Social'))
  .&End
end;

end.
