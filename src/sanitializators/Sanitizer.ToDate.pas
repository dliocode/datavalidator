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

unit Sanitizer.ToDate;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils, System.DateUtils;

type
  TSanitizerToDate = class(TDataValidatorItemBaseSanitizer)
  private
    FJSONISO8601ReturnUTC: Boolean;
  public
    function Sanitize: TValue; override;
    constructor Create(const AJSONISO8601ReturnUTC: Boolean);
  end;

implementation

{ TSanitizerToDate }

constructor TSanitizerToDate.Create(const AJSONISO8601ReturnUTC: Boolean);
begin
  FJSONISO8601ReturnUTC := AJSONISO8601ReturnUTC;
end;

function TSanitizerToDate.Sanitize: TValue;
var
  LValue: string;
  LValueSanitize: string;
  R: Boolean;
  LDate: TDateTime;
begin
  LValue := Trim(GetValueAsString);
  LValueSanitize := LValue;

  R := TryStrToDate(LValueSanitize, LDate);

  if not R then
    R := TryISO8601ToDate(LValueSanitize, LDate, FJSONISO8601ReturnUTC);

  if R then
    if LValue <> LValueSanitize then
      SetValueAdapter(LValueSanitize)
    else
      SetValueAdapter(TValue.From<TDate>(LDate));

  Result := FValue;
end;

end.
