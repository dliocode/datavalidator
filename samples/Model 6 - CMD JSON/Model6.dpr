program Model6;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  DataValidator, System.JSON;

function Valid(const AJO: TJSONObject): IDataValidatorResult;
begin
  Result :=

    TDataValidator.JSON(AJO)

    .Validate('apelido')
      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('Você não informou o seu apelido!')
        .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('Seu apelido deve conter apenas letras!')
        .IsLength(1, 10).WithMessage('O apelido deve ter no máximo 10 caracteres!')
      .&End
    .&End

    .Validate('email')
      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('Você não informou o seu e-mail!')
        .IsEmail.WithMessage('Não é um e-mail válido!')
        .NormalizeEmail
      .&End
    .&End

    .Validate('login')
      .Key
        .IsRequired.WithMessage('É obrigatório ter a Key "login" no JSON.') // Obriga
      .&End

      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('Você não informou o login!')
        .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('Seu login deve conter apenas letras!')
        .IsLength(1, 10).WithMessage('O login deve ter no máximo 10 caracteres!')
      .&End
    .&End

    .Validate('nome')
      .Key
        .IsOptional // É opcional - se existir a Key "nome" ele faz a validação
      .&End

      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('Você não informou o nome!')
        .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('Seu nome deve conter apenas letras!')
        .IsLength(1, 10).WithMessage('O nome deve ter no máximo 10 caracteres!')
      .&End
    .&End

    .CheckAll;
end;

var
  LJO: TJSONObject;
  LResult: IDataValidatorResult;
begin
  LJO := TJSONObject.ParseJSONValue('{"apelido":"DLIO", "email":"developer.dlio@gmail.com"}') as TJSONObject;

  Writeln;
  LResult := Valid(LJO);

  if not LResult.OK then
  begin
    Writeln('-------------------------------------');
    Writeln;
    Write(LResult.Informations.Message);
    Writeln;
    Write(LResult.Informations.GetItem(0).Message);
    Writeln;
    Writeln(' Tente novamente!');
    Writeln;
    Writeln('-------------------------------------');
  end;

  Readln;
end.
