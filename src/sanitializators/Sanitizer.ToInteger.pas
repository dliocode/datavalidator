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

unit Sanitizer.ToInteger;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils, System.Variants, System.StrUtils;

type
  TSanitizerToInteger = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerToInteger }

constructor TSanitizerToInteger.Create;
begin
end;

function TSanitizerToInteger.Sanitize: TValue;
var
  LValue: string;
  LPosPoint: Integer;
  LPosComma: Integer;
  LValueFloat: Double;
begin
  LValue := GetValueAsString;

  if Trim(LValue).IsEmpty then
    Exit;

  LPosPoint := Pos('.', LValue);
  LPosComma := Pos(',', LValue);

  case IndexStr(FormatSettings.DecimalSeparator, ['.', ',']) of
    0:
      begin
        if (LPosComma > 0) and (LPosPoint > 0) then
        begin
          if LPosPoint < LPosComma then
            LValue := VarToStr(LValue).Replace('.', '').Replace(',', '.')
          else
            LValue := VarToStr(LValue).Replace(',', '');
        end
        else
        begin
          if (LPosPoint = 0) and (LPosComma > 0) then
            LValue := VarToStr(LValue).Replace(',', '.')
        end;
      end;
    1:
      begin
        if (LPosComma > 0) and (LPosPoint > 0) then
        begin
          if LPosComma < LPosPoint then
            LValue := VarToStr(LValue).Replace(',', '').Replace('.', ',')
          else
            LValue := VarToStr(LValue).Replace('.', '');
        end
        else
        begin
          if (LPosComma = 0) and (LPosPoint > 0) then
            LValue := VarToStr(LValue).Replace('.', ',')
        end;
      end;
  end;

  if TryStrToFloat(LValue, LValueFloat) then
    LValue := IntToStr(Round(LValueFloat));

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
