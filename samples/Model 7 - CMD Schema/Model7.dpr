program Model7;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  DataValidator,
  System.SysUtils;

function SchemaNome(const AField: string): IDataValidatorSchemaContext;
begin
  Result :=
  TDataValidator.Schema
    .Validate
      .Trim
      .&Not.IsEmpty.WithMessage('Preencha o campo %s !', [AField]) // Não pode ser vazio
      .IsLength(2, 10).WithMessage('O campo %s deve conter entre 2 a 10 caracteres!', [AField])
      .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('O campo %s possui caracteres inválidos!', [AField])
    .&End;
end;

function SchemaEmail(const AField: string): IDataValidatorSchemaContext;
begin
  Result :=
  TDataValidator.Schema
    .Validate
      .Trim
      .&Not.IsEmpty.WithMessage('O %s não pode ser vazio!',[AField])
      .IsLength(2, 999).WithMessage('O campo %s deve ter mais de 6 caracteres!', [AField])
      .IsEmail.WithMessage('Não é um %s válido!',[AField])
      .NormalizeEmail
    .&End;
end;

function Valid(const ANome: string; const AApelido: string; const AEmail: string; const AEmailConfirmacao: string): IDataValidatorResult;
begin
  Result :=

    TDataValidator.Values

    .Validate(ANome)
      .AddSchema(SchemaNome('Nome'))
    .&End

    .Validate(AApelido)
      .AddSchema(SchemaNome('Apelido'))
    .&End

    .Validate(AEmail)
      .AddSchema(SchemaEmail('E-mail'))
    .&End

    .Validate(AEmailConfirmacao)
      .AddSchema(SchemaEmail('E-mail de confirmação'))
    .&End

    .CheckAll;
end;

var
  LNome: string;
  LApelido: string;
  LEmail: string;
  LEmailConfirmacao: string;
  LResult: IDataValidatorResult;

begin
  repeat
    Writeln;
    Write(' Digite seu nome: ');
    Readln(LNome);

    Write(' Digite seu apelido: ');
    Readln(LApelido);

    Write(' Digite seu e-mail: ');
    Readln(LEmail);

    Write(' Confirme seu e-mail: ');
    Readln(LEmailConfirmacao);

    Writeln;
    LResult := Valid(LNome, LApelido, LEmail, LEmailConfirmacao);

    if not LResult.OK then
    begin
      Writeln('-------------------------------------');
      Writeln;
      Write(LResult.Informations.Message);
      Writeln;
      Writeln(' Tente novamente!');
      Writeln;
      Writeln('-------------------------------------');
    end;

  until LResult.OK;

  Writeln('Parabéns: tudo certo por aqui!');

  Readln;
end.
