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

unit Validator.IsCPFCNPJ;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils;

type
  TValidatorIsCPFCNPJ = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

uses
  Validator.IsCPF, Validator.IsCNPJ;

{ TValidatorIsCPFCNPJ }

constructor TValidatorIsCPFCNPJ.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsCPFCNPJ.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LValidatorCPF: IDataValidatorItem;
  LValidatorCNPJ: IDataValidatorItem;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValidatorCPF := TValidatorIsCPF.Create('');
    LValidatorCPF.SetValue(LValue);
    R := LValidatorCPF.Check.OK;

    if not R then
    begin
      LValidatorCNPJ := TValidatorIsCNPJ.Create('');
      LValidatorCNPJ.SetValue(LValue);
      R := LValidatorCNPJ.Check.OK;
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

end.
