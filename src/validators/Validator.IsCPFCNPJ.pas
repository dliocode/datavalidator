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
  DataValidator.ItemBase, Validator.IsCPF, Validator.IsCNPJ,
  System.SysUtils;

type
  TValidatorIsCPFCNPJ = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValidatorCPF: IDataValidatorItem;
    FValidatorCNPJ: IDataValidatorItem;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsCPFCNPJ }

constructor TValidatorIsCPFCNPJ.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;

  FValidatorCPF := TValidatorIsCPF.Create(AMessage, AExecute);
  FValidatorCNPJ := TValidatorIsCNPJ.Create(AMessage, AExecute);
end;

function TValidatorIsCPFCNPJ.Check: IDataValidatorResult;
var
  R: Boolean;
  LResult: IDataValidatorResult;
  LValue: string;
begin
  FValidatorCPF.SetIsNot(FIsNot);
  FValidatorCPF.SetValue(FValue);

  LResult := FValidatorCPF.Check;
  R := LResult.OK;
  LValue := LResult.Values[0];

  if not R then
  begin
    FValidatorCNPJ.SetIsNot(FIsNot);
    FValidatorCNPJ.SetValue(FValue);

    LResult := FValidatorCNPJ.Check;
    R := LResult.OK;
    LValue := LResult.Values[0];
  end;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
