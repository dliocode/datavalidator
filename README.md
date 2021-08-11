<p align="center">
  <a href="https://user-images.githubusercontent.com/54585337/123715878-a52ae580-d84f-11eb-9d2d-32a98aaac5d8.png">
    <img alt="DataValidator" src="https://user-images.githubusercontent.com/54585337/123715878-a52ae580-d84f-11eb-9d2d-32a98aaac5d8.png">
  </a>  
</p>
<br>
<p align="center"> 
  <img src="https://img.shields.io/github/v/release/dliocode/datavalidator?style=flat-square">
  <img src="https://img.shields.io/github/stars/dliocode/datavalidator?style=flat-square">
  <img src="https://img.shields.io/github/forks/dliocode/datavalidator?style=flat-square">
  <img src="https://img.shields.io/github/contributors/dliocode/datavalidator?color=orange&style=flat-square">
  <img src="https://tokei.rs/b1/github/dliocode/datavalidator?color=red&category=lines">
  <img src="https://tokei.rs/b1/github/dliocode/datavalidator?color=green&category=code">
  <img src="https://tokei.rs/b1/github/dliocode/datavalidator?color=yellow&category=files">
</p>

# DataValidator

DataValidator foi projetado para ser uma biblioteca simples de validação de dados.

Support: developer.dlio@gmail.com

## Instalação

#### Para instalar em seu projeto usando [boss](https://github.com/HashLoad/boss):
```sh
$ boss install github.com/dliocode/datavalidator
```

#### Instalação Manual

Adicione as seguintes pastas ao seu projeto, em *Project > Options > Delphi Compiler > Search path*

```
../datavalidator/src/core
../datavalidator/src/sanitializators
../datavalidator/src/validators
```

#### Samples
  * Veja alguns exemplos: [samples](https://github.com/dliocode/datavalidator/tree/main/samples)


## Como usar

#### **Uses necessária**

```
uses DataValidator;
``` 

#### **Tipos de Validação**

* Values
* JSON 

##### Modo: Values

- No modo values, o valor informado no **_validate_** é o que será analisado! 

```
var
  LResult: IDataValidatorResult;
begin
  LResult :=  

  TDataValidator.Values

  .Validate('APELIDO')
      .Trim
      .&Not.IsEmpty.WithMessage('Você não informou o seu apelido!')
      .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('Seu apelido deve conter apenas letras!')
      .IsLength(1, 10).WithMessage('O apelido deve ter no máximo 10 caracteres!')
  .&End

  .Validate('developer.dlio@gmail.com')
      .Trim
      .&Not.IsEmpty.WithMessage('Você não informou o seu e-mail!')
      .IsEmail.WithMessage('Não é um e-mail válido!')
      .NormalizeEmail
  .&End

  .CheckedAll;  
```

##### Modo: JSON

- No modo JSON, o que deve ser informado **_validate_** é o nome da key do json! 
- Caso seja utilizado algum **_sanitizer_**, o valor dentro do JSON será modificado!

```
var
  LJO: TJSONObject;
  LResult: IDataValidatorResult;
begin
  LJO := TJSONObject.ParseJSONValue('{"apelido":"DLIO", "email":"developer.dlio@gmail.com"}') as TJSONObject;

  LResult :=  

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
      .Key // Faz a validação somente da key
        .IsRequired.WithMessage('É obrigatório ter a Key "login" no JSON.')
      .&End

      .Value // Faz a validação somente do valor dentro da Key 'login'
        .Trim
        .&Not.IsEmpty.WithMessage('Você não informou o login!')
        .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('Seu login deve conter apenas letras!')
        .IsLength(1, 10).WithMessage('O login deve ter no máximo 10 caracteres!')
      .&End      
    .&End

    .Validate('nome')
      .Key
        .IsOptionalKey // É opcional - se existir a Key "nome" ele faz a validação
      .&End  

      .Value      
        .Trim
        .&Not.IsEmpty.WithMessage('Você não informou o nome!')
        .IsAlpha(TDataValidatorLocaleLanguage.tl_pt_BR).WithMessage('Seu nome deve conter apenas letras!')
        .IsLength(1, 10).WithMessage('O nome deve ter no máximo 10 caracteres!')
      .&End      
    .&End

    .CheckedAll;
```


#### Como saber se houve error

```
    Result := LResult.Ok; // True = Nenhum erro | False = Tem erro
```

#### Como recuperar todas as mensagens de errors
```
    Result := LResult.Informations.Message;
```

#### Como recuperar uma única mensagem de error
```
    Result := LResult.Informations.GetItem(0).Message;
```

#### Como recuperar o total de errors
```
    Result := LResult.Informations.Count;
```

#### Diferença de Checked e CheckedAll

- **Checked**: Faz a verificação de todos os **_validate_**, mas se houver um item com retorno **_false_** ele interrompe a verificação!
- **CheckedAll**: Faz a verificação de todos os **_validate_**!

#### **Validação Schema**
- É uma forma de criar um esqueleto de validação e conseguir reutilizar em outras validações.

```
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

begin
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

  .CheckedAll;

  Readln;  
end.
```

#### Sanitizers

- **_Sanitizer_** é o tratamento de um valor. 

- Exemplo: Sanitizers MD5: Pega o valor do **_validate_** e converte para MD5

```
TDataValidator.Values

  .Validate('APELIDO')
    .ToMD5
  .&End
```

#### Validators Especiais

| Nome | Informação |
| ------------ | ------------ |
| Not | Quando usado esse validador, ele nega o resultado do validador. <br /> Ex: **Validate('email').&Not.IsEmpty** <br /> Se entende que o valor não deve ser vazio.|
| Execute | Define o que deve ser executado se aquele **_validate_** não passar na validação. A execução dessa procedure é forma manual. <br> Ex: <br> CheckedAll.Informations.GetItem(0).OnExecute;|
| WithMessage | Define a mensagem do error |

## Validators / Sanitizers

|Validação para values  |Validação JSON (Key/Value)       |Sanitizers para Valus |
| ------------  	      | ------------  	                | ------------  	  |
|CustomValue            |(Key) IsOptional                 |CustomSanitizer	  |
|Contains       	      |(Key) IsRequired	                |NormalizeEmail     |
|IsAlpha        	      |(Value) CustomJSONValue          |OnlyNumbers   		  |
|IsAlphaNumeric 	      |(Value) IsArray                  |RemoveAccents      |
|IsAscii         	      |(Value) IsObject                 |Replace            |
|IsBase32        	      |(Value) MinItems                 |ToBase64Decode     |
|IsBase58        	      |(Value) MaxItems                 |ToBase64Encode     |
|IsBase64       	      |(Value) + Validação para values  |ToDate             |
|IsBetween      	      |                                 |ToDateTime         |
|IsBoolean       		    |                                 |ToHTMLDecode	      |
|IsBTCAddress   		    |                                 |ToHTMLEncode	      |
|IsCEP          		    |                                 |ToInteger			    |
|IsCNPJ         		    |                                 |ToLowerCase		    |
|IsCPF          		    |                                 |ToMD5				 	    |
|IsCPFCNPJ      		    |                                 |ToNumeric     	    |
|IsDate         	      |                                 |ToTime             |
|IsDateBetween  	      |                                 |ToUpperCase	      |
|IsDateEquals     	    |                                 |ToURLDecode        |
|IsDateGreaterThan	    |                                 |ToURLEncode		    |
|IsDateLessThan		      |                                 |Trim			          |
|IsDateTime			        |                                 |TrimLeft      	    |
|IsEmail			          |                                 |TrimRight          |
|IsEmpty			          |                                 |                   |
|IsEquals			          |                                 |                   |
|IsEthereumAddress	    |                                 |					          |
|IsGreaterThan		      |                                 |					          |
|IsGTIN       		      |                                 |					          |
|IsGTIN8       		      |                                 |					          |
|IsGTIN12     		      |                                 |					          |
|IsGTIN13      		      |                                 |					          |
|IsGTIN14      		      |                                 |					          |
|IsHexadecimal		      |                                 |					          |
|IsHexColor			        |                                 |					          |
|IsInteger			        |                                 |					          |
|IsIP				            |                                 |					          |
|IsISO8601              |                                 |					          |
|IsJSON				          |                                 |					          |
|IsJWT				          |                                 |					          |
|IsLength			          |                                 |					          |
|IsLessThan			        |                                 |					          |
|IsLocale    		        |                                 |					          |
|IsLowercase		        |                                 |					          |
|IsMACAddress		        |                                 |					          |
|IsMagnetURI 	          |                                 |					          |
|IsMD5				          |                                 |					          |
|IsMimeType  	          |                                 |					          |
|IsNegative			        |                                 |					          |
|IsNumeric			        |                                 |					          |
|IsOptional		          |                                 |					          |
|IsOctal     	          |                                 |					          |
|IsPassportNumber       |                                 |					          |
|IsPhoneNumber		      |                                 |					          |
|IsPositive			        |                                 |					          |
|IsRGBColor   	        |                                 |					          |
|IsSSN			            |                                 |					          |
|IsTime				          |                                 |					          |
|IsTimeBetween		      |                                 |					          |
|IsTimeEquals		        |                                 |					          |
|IsTimeGreaterThan	    |                                 |					          |
|IsTimeLessThan		      |                                 |					          |
|IsUppercase		        |                                 |					          |
|IsURL				          |                                 |					          |
|IsUUID				          |                                 |					          |
|IsUUIDv1			          |                                 |					          |
|IsUUIDv2			          |                                 |					          |
|IsUUIDv3			          |                                 |					          |
|IsUUIDv4			          |                                 |					          |
|IsUUIDv5			          |                                 |					          |
|IsZero				          |                                 |					          |
|RegexIsMatch	          |                                 |					          |