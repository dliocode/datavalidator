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
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsCPF }

constructor TValidatorIsCPF.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsCPF.Check: IDataValidatorResult;
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

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

function TValidatorIsCPF.Validate(const ACPF: string): Boolean;
var
  LCPF: string;
  LSum: Integer;
  LPeso: Integer;
  I: Integer;
  LResultSum: Integer;
  LDig10: ShortString;
  LDig11: ShortString;
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
    LSum := 0;
    LPeso := 10;
    for I := 1 to 9 do
    begin
      LSum := LSum + (StrToInt(LCPF[I]) * LPeso);
      LPeso := LPeso - 1;
    end;

    LResultSum := 11 - (LSum mod 11);
    if ((LResultSum = 10) or (LResultSum = 11)) then
      LDig10 := '0'
    else
      str(LResultSum: 1, LDig10);

    LSum := 0;
    LPeso := 11;
    for I := 1 to 10 do
    begin
      LSum := LSum + (StrToInt(LCPF[I]) * LPeso);
      LPeso := LPeso - 1;
    end;

    LResultSum := 11 - (LSum mod 11);
    if ((LResultSum = 10) or (LResultSum = 11)) then
      LDig11 := '0'
    else
      str(LResultSum: 1, LDig11);

    Result := (LDig10 = ShortString(LCPF[10])) and (LDig11 = ShortString(LCPF[11]));
  except
    Result := False
  end;
end;

end.
