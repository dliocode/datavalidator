unit USanitize;

interface

uses
  DataValidator,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    btnSanitize: TButton;
    Panel2: TPanel;
    EditEmail: TLabeledEdit;
    EditBase64ToDecode: TLabeledEdit;
    EditBase64ToEncode: TLabeledEdit;
    MemoInfo: TMemo;
    procedure btnSanitizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm2.btnSanitizeClick(Sender: TObject);
var
  LValidatorResult: IDataValidatorResult;
begin
  LValidatorResult := Validation.Check;
  ValidationResult(LValidatorResult);
end;

procedure TForm2.ValidationResult(const AResult: IDataValidatorResult);
begin
  MemoInfo.Clear;

  if AResult.OK then
  begin
    MemoInfo.Lines.Add(Format('Value Validate 1: %s', [AResult.Values[0]])); // Recupera o valor da 1 validação
    MemoInfo.Lines.Add(Format('Value Validate 2: %s', [AResult.Values[1]])); // Recupera o valor da 2 validação
    MemoInfo.Lines.Add(Format('Value Validate 3: %s', [AResult.Values[2]])); // Recupera o valor da 3 validação

    Exit;
  end;

  MemoInfo.Text := AResult.Informations.Message;
  MemoInfo.Lines.Add(Format('Total errors: %d', [AResult.Informations.Count]));

  AResult.Informations.GetItem(0).OnExecute; // Executa o que foi informado no validate execute
end;

function TForm2.Validation: IDataValidatorValueResult;
begin
  Result :=
    TDataValidator.Values
      .Validate(EditEmail.Text, 'E-mail').Execute(EditEmail.SetFocus)
        .Trim.NormalizeEmail
        .IsEmail.WithMessage('${name} inválido')
      .&End

      .Validate(EditBase64ToDecode.Text, 'Base64ToDecode').Execute(EditBase64ToDecode.SetFocus)
        .Trim
        .&Not.IsEmpty.WithMessage('${name} não pode ser vazio!')
        .IsBase64.WithMessage('${name} não é um valor Base64 válido!')
        .ToBase64Decode
      .&End

      .Validate(EditBase64ToEncode.Text,'Base64ToEncode').Execute(EditBase64ToEncode.SetFocus)
        .&Not.IsEmpty.WithMessage('${name} não pode ser vazio!')
        .ToBase64Encode
      .&End;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

end.
