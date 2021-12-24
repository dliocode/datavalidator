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

unit Validator.IsPort;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils;

type
  TValidatorIsPort = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

uses
  Validator.IsInteger, Validator.IsBetween;

{ TValidatorIsPort }

constructor TValidatorIsPort.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsPort.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LValidatorInteger: IDataValidatorItem;
  LValidatorBetween: IDataValidatorItem;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValidatorInteger := TValidatorIsInteger.Create('');
    LValidatorInteger.SetValue(LValue);
    R := LValidatorInteger.Check.OK;

    if R then
    begin
      LValidatorBetween := TValidatorIsBetween.Create(0, 65535, '');
      LValidatorBetween.SetValue(LValue);
      R := LValidatorBetween.Check.OK;
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

end.
