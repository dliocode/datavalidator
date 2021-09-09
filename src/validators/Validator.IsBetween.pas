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

unit Validator.IsBetween;

interface

uses
  DataValidator.ItemBase, DataValidator.ItemBase.Sanitizer,
  System.Math, System.SysUtils, System.RTTI;

type
  TValidatorIsBetween = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueA: TValue;
    FValueB: TValue;
  public
    function Check: IDataValidatorResult;

    constructor Create(const AValueA: TValue; const AValueB: TValue; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil); overload;
  end;

implementation

uses
  Sanitizer.ToNumeric,
  Validator.IsNumeric;

{ TValidatorIsBetween }

constructor TValidatorIsBetween.Create(const AValueA: TValue; const AValueB: TValue; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FValueA := AValueA;
  FValueB := AValueB;
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsBetween.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LSanitizerNumeric: IDataSanitizerItem;
  LValidatorNumeric: IDataValidatorItem;
  LValueA: Variant;
  LValueB: Variant;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LSanitizerNumeric := TSanitizerToNumeric.Create;
    LSanitizerNumeric.SetValue(LValue);
    LValue := LSanitizerNumeric.Sanitize.AsString;

    LValidatorNumeric := TValidatorIsNumeric.Create('');
    LValidatorNumeric.SetValue(LValue);
    R := LValidatorNumeric.Check.OK;

    if R then
    begin
      LValueA := FValueA.AsVariant;
      LValueB := FValueB.AsVariant;

      R := InRange(StrToFloat(LValue), LValueA, LValueB);
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

end.
