{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsCPF; // (Brasil) Comprovante de Situação Cadastral

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsCPF = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    function Validate(const ACPF: string): Boolean;
  public
    function GetMessage: string;
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsCPF }

constructor TValidatorIsCPF.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsCPF.GetMessage: string;
begin
  Result := FMessage;
end;

function TValidatorIsCPF.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    R := TRegEx.IsMatch(LValue, '^(\d{3}\.\d{3}\.\d{3}\-\d{2}|\d{11})$');

    if R then
      R := Validate(LValue);
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

function TValidatorIsCPF.Validate(const ACPF: string): Boolean;
var
  LCPF: string;
  LDig10, LDig11: ShortString;
  S, I, R, LPeso: Integer;
begin
  Result := False;

  LCPF := TRegEx.Replace(ACPF, '\D', '');

  if (
    (LCPF = '00000000000') or (LCPF = '11111111111') or
    (LCPF = '22222222222') or (LCPF = '33333333333') or
    (LCPF = '44444444444') or (LCPF = '55555555555') or
    (LCPF = '66666666666') or (LCPF = '77777777777') or
    (LCPF = '88888888888') or (LCPF = '99999999999') or
    (Length(LCPF) <> 11)
    )
  then
    Exit;

  try
    S := 0;
    LPeso := 10;
    for I := 1 to 9 do
    begin
      S := S + (StrToInt(LCPF[I]) * LPeso);
      LPeso := LPeso - 1;
    end;

    R := 11 - (S mod 11);
    if ((R = 10) or (R = 11)) then
      LDig10 := '0'
    else
      str(R: 1, LDig10);

    S := 0;
    LPeso := 11;
    for I := 1 to 10 do
    begin
      S := S + (StrToInt(LCPF[I]) * LPeso);
      LPeso := LPeso - 1;
    end;

    R := 11 - (S mod 11);
    if ((R = 10) or (R = 11)) then
      LDig11 := '0'
    else
      str(R: 1, LDig11);

    Result := (LDig10 = ShortString(LCPF[10])) and (LDig11 = ShortString(LCPF[11]));
  except
    Result := False
  end;
end;

end.
