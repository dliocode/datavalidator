{
  ********************************************************************************

  Github - https://github.com/dliocode/datavalidator

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

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

unit Validator.IsRGBColor; // RGB (Red, Green, Blue)

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsRGBColor = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsRGBColor }

constructor TValidatorIsRGBColor.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  inherited Create;

  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsRGBColor.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LRegexRGBColor: string;
  LRegexRGBAColor: string;
  LRegexRGBColorPercent: string;
  LRegexRGBAColorPercent: string;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LRegexRGBColor := '^rgb\((([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]),){2}([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\)$';
    LRegexRGBAColor := '^rgba\((([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]),){3}(0?\.\d|1(\.0)?|0(\.0)?)\)$';
    LRegexRGBColorPercent := '^rgb\((([0-9]%|[1-9][0-9]%|100%),){2}([0-9]%|[1-9][0-9]%|100%)\)';
    LRegexRGBAColorPercent := '^rgba\((([0-9]%|[1-9][0-9]%|100%),){3}(0?\.\d|1(\.0)?|0(\.0)?)\)';

    R := TRegEx.IsMatch(LValue, LRegexRGBColor) or
      TRegEx.IsMatch(LValue, LRegexRGBAColor) or
      TRegEx.IsMatch(LValue, LRegexRGBColorPercent) or
      TRegEx.IsMatch(LValue, LRegexRGBAColorPercent);
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(FKey, FName, LValue, GetMessage, FExecute));
end;

end.
