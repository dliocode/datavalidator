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

unit Sanitizer.Custom;

interface

uses
  DataValidator.Types, DataValidator.ItemBase.Sanitizer;

type
  TSanitizerCustom = class(TDataValidatorItemBaseSanitizer)
  private
    FExecute: TDataValidatorCustomSanitizerExecute;
  public
    function Sanitize: TValue; override;
    constructor Create(const AExecute: TDataValidatorCustomSanitizerExecute);
  end;

implementation

{ TSanitizerCustom }

constructor TSanitizerCustom.Create(const AExecute: TDataValidatorCustomSanitizerExecute);
begin
  FExecute := AExecute;
end;

function TSanitizerCustom.Sanitize: TValue;
var
  LValue: string;
begin
  LValue := GetValueAsString;

  if Assigned(FExecute) then
  begin
    LValue := FExecute(LValue);
    SetValueAdapter(LValue);
  end;

  Result := FValue;
end;

end.
