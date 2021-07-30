{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsCNPJ; // (Brasil) Cadastro Nacional de Pessoas Jurídicas

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsCNPJ = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    function Validate(const ACNPJ: string): Boolean;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsCNPJ }

constructor TValidatorIsCNPJ.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsCNPJ.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    R := TRegEx.IsMatch(LValue, '^(\d{2}\.\d{3}\.\d{3}\/\d{4}\-\d{2}|\d{14})$');

    if R then
      R := Validate(LValue);
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

function TValidatorIsCNPJ.Validate(const ACNPJ: string): Boolean;
var
  LCNPJ: string;
  LDig13, LDig14: ShortString;
  S, I, R, LPeso: Integer;
begin
  Result := False;

  LCNPJ := TRegEx.Replace(ACNPJ, '\D', '');

  if (
    (LCNPJ = '00000000000000') or (LCNPJ = '11111111111111') or
    (LCNPJ = '22222222222222') or (LCNPJ = '33333333333333') or
    (LCNPJ = '44444444444444') or (LCNPJ = '55555555555555') or
    (LCNPJ = '66666666666666') or (LCNPJ = '77777777777777') or
    (LCNPJ = '88888888888888') or (LCNPJ = '99999999999999') or
    (Length(LCNPJ) <> 14)
    )
  then
    Exit;

  try
    S := 0;
    LPeso := 2;

    for I := 12 downto 1 do
    begin
      S := S + (StrToInt(LCNPJ[I]) * LPeso);
      LPeso := LPeso + 1;

      if (LPeso = 10) then
        LPeso := 2;
    end;

    R := S mod 11;
    if ((R = 0) or (R = 1)) then
      LDig13 := '0'
    else
      str((11 - R): 1, LDig13);

    S := 0;
    LPeso := 2;

    for I := 13 downto 1 do
    begin
      S := S + (StrToInt(LCNPJ[I]) * LPeso);
      LPeso := LPeso + 1;

      if (LPeso = 10) then
        LPeso := 2;
    end;

    R := S mod 11;
    if ((R = 0) or (R = 1)) then
      LDig14 := '0'
    else
      str((11 - R): 1, LDig14);

    Result := (LDig13 = ShortString(LCNPJ[13])) and (LDig14 = ShortString(LCNPJ[14]));
  except
    Result := False
  end;
end;

end.
