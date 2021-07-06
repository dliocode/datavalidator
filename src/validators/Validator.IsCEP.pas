{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsCEP;  // (Brasil) CEP (Código de Endereçamento Postal)

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsCEP = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsCEP }

constructor TValidatorIsCEP.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsCEP.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    R := TRegEx.IsMatch(LValue, '^[\d]{2}\.?[\d]{3}\-?[\d]{3}$');

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(LValue, FMessage, FExecute));
end;

end.
