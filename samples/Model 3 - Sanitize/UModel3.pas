unit UModel3;

interface

uses
  DataValidator,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    LabeledEditEmail: TLabeledEdit;
    btnSanitize: TButton;
    Memo1: TMemo;
    LabeledEditBase64ToDecode: TLabeledEdit;
    LabeledEditBase64ToEncode: TLabeledEdit;
    procedure btnSanitizeClick(Sender: TObject);
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

function TForm1.Valid: IDataValidatorValueResult;
begin
  Result := TDataValidator.Values

  .Validate(LabeledEditEmail.Text).Execute(LabeledEditEmail.SetFocus)
    .Trim.NormalizeEmail
    .IsEmail.WithMessage('E-mail inválido')
  .&End

  .Validate(LabeledEditBase64ToDecode.Text).Execute(LabeledEditBase64ToDecode.SetFocus)
    .Trim
    .&Not.IsEmpty.WithMessage('Base64ToDecode não pode ser vazio!')
    .IsBase64.WithMessage('Não é um valor Base64 válido!')
    .ToBase64Decode
  .&End

  .Validate(LabeledEditBase64ToEncode.Text).Execute(LabeledEditBase64ToEncode.SetFocus)
    .&Not.IsEmpty.WithMessage('Base64ToEncode não pode ser vazio!')
    .ToBase64Encode
  .&End;
end;

procedure TForm1.btnSanitizeClick(Sender: TObject);
begin
  ShowResult(Valid.CheckAll);
end;

procedure TForm1.ShowResult(const AResult: IDataValidatorResult);
begin
  Memo1.Clear;

  if AResult.OK then
  begin
    Memo1.Lines.Add(Format('Value Validate 1: %s', [AResult.Values[0]])); // Recupera o valor da 1 validação
    Memo1.Lines.Add(Format('Value Validate 2: %s', [AResult.Values[1]])); // Recupera o valor da 2 validação
    Memo1.Lines.Add(Format('Value Validate 3: %s', [AResult.Values[2]])); // Recupera o valor da 3 validação

    Exit;
  end;

  Memo1.Text := AResult.Informations.Message;
  Memo1.Lines.Add(Format('Total errors: %d', [AResult.Informations.Count]));

  AResult.Informations.GetItem(0).OnExecute; // Executa o que foi informado no validate execute
end;

end.
