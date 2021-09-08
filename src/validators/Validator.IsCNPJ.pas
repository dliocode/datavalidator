{
  ********************************************************************************

  Github - https://github.com/dliocode/datavalidator

  ********************************************************************************

  MIT License

  Copyright (c) 2021 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
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
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsCNPJ }

constructor TValidatorIsCNPJ.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsCNPJ.Check: IDataValidatorResult;
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

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

function TValidatorIsCNPJ.Validate(const ACNPJ: string): Boolean;
var
  LCNPJ: string;
  LSum: Integer;
  LPeso: Integer;
  I: Integer;
  LResultSum: Integer;
  LDig13: ShortString;
  LDig14: ShortString;
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
    LSum := 0;
    LPeso := 2;

    for I := 12 downto 1 do
    begin
      LSum := LSum + (StrToInt(LCNPJ[I]) * LPeso);
      LPeso := LPeso + 1;

      if (LPeso = 10) then
        LPeso := 2;
    end;

    LResultSum := LSum mod 11;
    if ((LResultSum = 0) or (LResultSum = 1)) then
      LDig13 := '0'
    else
      str((11 - LResultSum): 1, LDig13);

    LSum := 0;
    LPeso := 2;

    for I := 13 downto 1 do
    begin
      LSum := LSum + (StrToInt(LCNPJ[I]) * LPeso);
      LPeso := LPeso + 1;

      if (LPeso = 10) then
        LPeso := 2;
    end;

    LResultSum := LSum mod 11;
    if ((LResultSum = 0) or (LResultSum = 1)) then
      LDig14 := '0'
    else
      str((11 - LResultSum): 1, LDig14);

    Result := (LDig13 = ShortString(LCNPJ[13])) and (LDig14 = ShortString(LCNPJ[14]));
  except
    Result := False
  end;
end;

end.
