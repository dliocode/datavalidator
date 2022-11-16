unit UBasic;

interface

uses
  DataValidator, System.DateUtils,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask, Vcl.ComCtrls;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    btnLimpar: TButton;
    btnCheck: TButton;
    Panel2: TPanel;
    btnCheckAllFirst: TButton;
    btnCheckAll: TButton;
    PageControl1: TPageControl;
    TabSample1: TTabSheet;
    TabSample2: TTabSheet;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo22: TMemo;
    Memo11: TMemo;
    procedure btnLimparClick(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnCheckAllFirstClick(Sender: TObject);
  private
    { Private declarations }
    FJO: TJSONObject;
    function ValidationTab1: IDataValidatorJSONResult;
    function ValidationTab2: IDataValidatorJSONResult;
    function Validation: IDataValidatorJSONResult;
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
  I: Integer;
begin
  for I := 0 to Pred(Self.ComponentCount) do
    if (Self.Components[I] is TMemo) then
      (Self.Components[I] as TMemo).Clear;
end;

procedure TForm2.btnCheckClick(Sender: TObject);
begin
  ValidationResult(Validation.Check);
end;

procedure TForm2.btnCheckAllClick(Sender: TObject);
begin
  ValidationResult(Validation.CheckAll());
end;

procedure TForm2.btnCheckAllFirstClick(Sender: TObject);
begin
  ValidationResult(Validation.CheckAll(TDataValidatorCheckAll.tcFirst));
end;

procedure TForm2.ValidationResult(const AResult: IDataValidatorResult);
begin
  try
    if AResult.OK then
      ShowMessage('Tudo certo!')
    else
    begin
      ShowMessage('Problema na validação: ' + sLineBreak + sLineBreak + AResult.Informations.Message);
      AResult.Informations.GetItem(0).OnExecute;
    end;
  finally
    if Assigned(FJO) then
    begin
      case PageControl1.ActivePageIndex of
        0:
          Memo11.Text := FJO.Format();

        1:
          Memo22.Text := FJO.Format();

//        2:
      else
          Memo11.Text := FJO.Format();
      end;

      FJO.Free;
      FJO := nil;
    end;
  end;
end;

function TForm2.Validation: IDataValidatorJSONResult;
begin
  case PageControl1.ActivePageIndex of
    0:
      begin
        FJO := TJSONObject.ParseJSONValue(Memo1.Text) as TJSONObject;

        Result := ValidationTab1;
      end;
    1:
      begin
        FJO := TJSONObject.ParseJSONValue(Memo2.Text) as TJSONObject;

        Result := ValidationTab2;
      end;
  else
    Result := ValidationTab1;
  end;
end;

function TForm2.ValidationTab1: IDataValidatorJSONResult;
begin
  Result :=
    TDataValidator.JSON(FJO)
    .Validate('id')
      .Key
        .IsRequired.WithMessage('Informe a key ${key}')
      .&End

      .Value
        .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
        .IsJSONNumeric.WithMessage('${key} - ${value} - Não é do tipo numeric!')
        .IsGreaterThan(0).WithMessage('O ${key} deve ser maior que zero!')
        .IsInteger.WithMessage('O ${key} deve ser do tipo inteiro!')
      .&End
    .&End

    .Validate(['name', 'description'])
      .Key
        .IsRequired.WithMessage('Informe a key ${key}')
      .&End

      .Value
        .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
        .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
        .IsAlphaNumeric(TDataValidatorLocaleLanguage.tl_pt_BR, ['-', ',', '/', '\']).WithMessage('${key} - ${value} - Não é um valor AlphaNumeric!')
      .&End
    .&End

    .Validate('price')
      .Key
        .IsRequired.WithMessage('Informe a key ${key}')
      .&End

      .Value
        .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
        .IsJSONNumeric.WithMessage('${key} - ${value} - Não é do tipo numeric!')
        .IsGreaterThan(0).WithMessage('O ${key} deve ser maior que zero!')
      .&End
    .&End;
end;

function TForm2.ValidationTab2: IDataValidatorJSONResult;
begin
  Result :=
    TDataValidator.JSON(FJO)
    .Validate('id')
      .Key
        .IsRequired.WithMessage('Informe a key ${key}')
      .&End

      .Value
        .Trim
        .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
        .IsJSONNumeric.WithMessage('${key} - ${value} - Não é do tipo numeric!')
        .IsGreaterThan(0).WithMessage('O ${key} deve ser maior que zero!')
        .IsInteger.WithMessage('O ${key} deve ser do tipo inteiro!')
      .&End
    .&End

    .Validate('customers', 'Cliente')
      .Key
        .IsRequired.WithMessage('Informe a key ${key}')
      .&End

      .Value
        .IsJSONObject.WithMessage('${key} - Não é do tipo JSONObject!')
        .CustomJSONSubValidator(
          function(const AValue: IDataValidatorJSON; var AMessage: TDataValidatorMessage): Boolean
          var
            LJSONResult: IDataValidatorJSONResult;
            LResult: IDataValidatorResult;
          begin
            LJSONResult :=
              AValue
                .Validate('id')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .Trim
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsJSONNumeric.WithMessage('${key} - ${value} - Não é do tipo numeric!')
                    .IsGreaterThan(0).WithMessage('O ${key} deve ser maior que zero!')
                    .IsInteger.WithMessage('O ${key} deve ser do tipo inteiro!')
                  .&End
                .&End

                .Validate('name')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .Trim
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
                    .IsAlphaNumeric(TDataValidatorLocaleLanguage.tl_pt_BR, ['-', ',', '/', '\']).WithMessage('${key} - ${value} - Não é um valor AlphaNumeric!')
                  .&End
                .&End

                .Validate('gender')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .Trim
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
                    .IsLength(1,1).WithMessage('')
                    .ToUpperCase
                    .Contains(['F', 'M']).WithMessage('${key} - Deve ser somente "F" ou "M" e não ${value}')
                  .&End
                .&End

                .Validate('birthDate')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .Trim
                    .IsOptional(
                    function(const AValue: TJSONValue): Boolean
                    begin
                      Result := AValue.Null;
                    end)
                    .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
                    .IsLength(10,10).WithMessage('${key} - Deve ter somente 10 caracteres!')
                    .IsDate(False).WithMessage('${key} - ${value} - Não é uma data válida!')
                    .&Not.IsDateGreaterThan(Now, False).WithMessage('${key} - ${value} - A data informada não pode ser maior que a data atual!')
                    .&Not.IsDateLessThan(IncYear(Now, -90), False).WithMessage('${key} - ${value} - A data informada não pode ser menor que %s', [FormatDateTime('yyyy-mm-dd', IncYear(Now, -90))])
                  .&End
                .&End

                .Validate('documents')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsJSONArray.WithMessage('${key} deve ser do tipo JSONArray')
                    .CustomJSONSubValidator(
                      function(const AValue: IDataValidatorJSON; var AMessage: TDataValidatorMessage): Boolean
                      var
                        LJSONResult: IDataValidatorJSONResult;
                        LResult: IDataValidatorResult;
                      begin
                        LJSONResult :=
                          AValue
                            .Validate('type')
                              .Key
                                .IsRequired.WithMessage('Informe a key ${key}')
                              .&End

                              .Value
                                .Trim
                                .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                                .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
                                .ToUpperCase
                                .IsEquals(['CPF', 'CNPJ']).WithMessage('${key} - Deve ser somente "CPF" ou "CNPJ" e não ${value}')
                              .&End
                            .&End

                            .Validate('number')
                              .Key
                                .IsRequired.WithMessage('Informe a key ${key}')
                              .&End

                              .Value
                                .Trim
                                .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                                .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
                                .IsInteger.WithMessage('${key} - ${value} - Informe somente números!')
                              .&End
                            .&End
                            ;

                        LResult := LJSONResult.Check;

                        Result := LResult.OK;

                        if not Result then
                          AMessage := LResult.Informations.GetItem(0).Messages;
                      end)
                  .&End
                .&End

                .Validate('phones')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsJSONArray.WithMessage('${key} deve ser do tipo JSONArray')
                    .CustomJSONSubValidator(
                      function(const AValue: IDataValidatorJSON; var AMessage: TDataValidatorMessage): Boolean
                      var
                        LJSONResult: IDataValidatorJSONResult;
                        LResult: IDataValidatorResult;
                      begin
                        LJSONResult :=
                          AValue
                            .Validate('type')
                              .Key
                                .IsRequired.WithMessage('Informe a key ${key}')
                              .&End

                              .Value
                                .Trim
                                .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                                .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
                                .ToUpperCase
                                .Contains(['PRIMARY', 'SECONDARY']).WithMessage('${key} - Deve ser somente "CPF" ou "CNPJ" e não ${value}')
                              .&End
                            .&End

                            .Validate('number')
                              .Key
                                .IsRequired.WithMessage('Informe a key ${key}')
                              .&End

                              .Value
                                .Trim
                                .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                                .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
                                .IsLength(12,13).WithMessage('${key} - ${value} - Informe um fone com 12 ou 13 digitos!')
                                .IsInteger.WithMessage('${key} - ${value} - Informe somente números!')
                              .&End
                            .&End
                            ;

                        LResult := LJSONResult.Check;

                        Result := LResult.OK;

                        if not Result then
                          AMessage := LResult.Informations.GetItem(0).Messages;
                      end)
                  .&End
                .&End

                .Validate('email')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .Trim.NormalizeEmail()
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
                    .IsEmail.WithMessage('${key} - ${value} - Não é um e-mail válido!')
                  .&End
                .&End
                ;

            LResult := LJSONResult.Check;

            Result := LResult.OK;

            if not Result then
              AMessage := LResult.Informations.GetItem(0).Messages;
          end)

      .&End
    .&End

    .Validate('items')
      .Key
        .IsRequired.WithMessage('Informe a key ${key}')
      .&End

      .Value
        .IsJSONArray.WithMessage('${key} - Não é do tipo JSONArray!')
        .CustomJSONSubValidator(
          function(const AValue: IDataValidatorJSON; var AMessage: TDataValidatorMessage): Boolean
          var
            LJSONResult: IDataValidatorJSONResult;
            LResult: IDataValidatorResult;
          begin
            LJSONResult :=
              AValue
                .Validate('product_id')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .Trim
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsJSONNumeric.WithMessage('${key} - ${value} - Não é do tipo numeric!')
                    .IsGreaterThan(0).WithMessage('O ${key} deve ser maior que zero!')
                    .IsInteger.WithMessage('O ${key} deve ser do tipo inteiro!')
                  .&End
                .&End

                .Validate('product_name')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .Trim
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsJSONString.WithMessage('${key} - ${value} - Não é do tipo string!')
                    .IsAlphaNumeric(TDataValidatorLocaleLanguage.tl_pt_BR, ['-', ',', '/', '\']).WithMessage('${key} - ${value} - Não é um valor AlphaNumeric!')
                  .&End
                .&End

                .Validate('price')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .Trim
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsNumeric.WithMessage('O ${key} não é um valor inválido!')
                    .IsGreaterThan(-1).WithMessage('Informe um valor maior que Zero!')
                  .&End
                .&End

                .Validate('quantity')
                  .Key
                    .IsRequired.WithMessage('Informe a key ${key}')
                  .&End

                  .Value
                    .Trim
                    .&Not.IsEmpty.WithMessage('O ${key} deve ser informado!')
                    .IsNumeric.WithMessage('O ${key} não é um valor inválido!')
                  .&End
                .&End
                ;

            LResult := LJSONResult.Check;

            Result := LResult.OK;

            if not Result then
              AMessage := LResult.Informations.GetItem(0).Messages;
          end)
        .&End
      .&End
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  PageControl1.ActivePageIndex := 0;
end;

end.
