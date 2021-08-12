program Model9;

uses
  DataValidator,
  Horse,
  Horse.Jhonson,
  System.JSON,
  System.SysUtils,
  System.DateUtils;

function ValidateRegister(const AJSON: TJSONObject): IDataValidatorResult;
begin
  Result :=
  TDataValidator.JSON(AJSON)
    .Validate('nome')
      .Key
        .IsRequired.WithMessage('Key "nome" não informado!')
      .&End

      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('O campo nome não foi preenchido!')
        .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('O campo nome não pode possuir caracters especiais!')
        .IsLength(3, 100).WithMessage('O campo nome deve ter no mínimo 3 caracters e no mázimo 100')
      .&End
    .&End

    .Validate('email')
      .Key
        .IsRequired.WithMessage('Key "email" não informado!')
      .&End

      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('Informe o e-mail!')
        .IsLength(6, 100).WithMessage('Informe um e-mail maior que 5 caracteres!')
        .NormalizeEmail(True, False).IsEmail.WithMessage('E-mail inválido!')
        .CustomValue(
        function(const AValue: string): Boolean
        begin
          // Recebe o valor no AValue para analisar e fazer o que for necessário
          //
          // Ex:
          // Consulta no banco verifica se esse email já existe;
          //
          // Result -> True = significa tudo certo! - False = significa que tem algum problema

          Result := AValue = 'developer.dlio@gmail.com';

        end).WithMessage('E-mail já cadastrado!')
      .&End
    .&End

    .Validate('data_nascimento')
      .Key
        .IsRequired.WithMessage('Key "data_nascimento" não informado!')
      .&End

      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('Informe a data de nascimento!')
        .IsDate.WithMessage('Data de nascimento inválido!')
        .IsDateLessThan(Now).WithMessage('A data de nascimento não pode ser maior que a data atual!')
        .IsDateGreaterThan(IncYear(Now, -100)).WithMessage('A data de nascimento não pode ser menor que 100 anos atrás!')
      .&End
    .&End

    .Validate('telefone')
      .Key
        .IsRequired.WithMessage('Key "telefone" não informado!')
      .&End

      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('Informe o telefone!')
        .IsPhoneNumber(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('Número de telefone inválido!')
      .&End
    .&End

    .Validate('cpf')
      .Key
        .IsRequired.WithMessage('Key "cpf" não informado!')
      .&End

      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('Informe o CPF!')
        .IsLength(14, 14).WithMessage('Informe corretamente todos os dígitos do CPF!')
        .OnlyNumbers.IsCPF.WithMessage('CPF inválido!')
      .&End
    .&End

    .CheckAll;
end;

begin
// Router:
// POST http://localhost:9000/register

// Body
// JSON Test
{
    "nome":"Danilo Lucas",
    "email":"developer.dlio@gmail.com",
    "data_nascimento":"23/08/1994",
    "telefone":"48999999999",
    "cpf":"012.345.678-90"
}


 THorse.Use(Jhonson());

  THorse.Post('/register',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LBody: TJSONObject;
      LValidatorResult: IDataValidatorResult;
      LResponse: TJsonObject;
      LReponseErrorCause: TJsonArray;
      I: Integer;
    begin
      try
        LBody := Req.Body<TJSONObject>;
      except
        LBody := nil;
      end;

      if not Assigned(LBody) then
      begin
        Res.Status(401).Send('Não foi informado o JSON no body corretamente!');
        raise EHorseCallbackInterrupted.Create;
      end;

      // faz a validação completa
      LValidatorResult := ValidateRegister(LBody);

      // Se tive tudo ok, já faz o retorno
      if LValidatorResult.OK then
      begin
        LResponse := TJsonObject.Create;
        LResponse.AddPair('error', TJSONBool.Create(False));
        LResponse.AddPair('message', TJSONString.Create('Registrado com sucesso!'));
        LResponse.AddPair('data', LBody.Clone as TJSONObject);

        Res.Status(201).Send<TJSONObject>(LResponse);
        raise EHorseCallbackInterrupted.Create;
      end;

      // Faz a exibição da mensagem de erro!
      LReponseErrorCause := TJsonArray.Create;
      for I := 0 to Pred(LValidatorResult.Informations.Count) do
        LReponseErrorCause.Add(LValidatorResult.Informations.GetItem(I).Message);

      LResponse := TJsonObject.Create;
      LResponse.AddPair('error', TJSONBool.Create(True));
      LResponse.AddPair('causes', LReponseErrorCause);

      Res.Send<TJSONObject>(LResponse);
    end);

  THorse.Listen(9000);

end.
