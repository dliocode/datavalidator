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

unit Validator.IsLatLong;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsLatLong = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCheckDMS: Boolean;
  public
    function Check: IDataValidatorResult;
    constructor Create(const ACheckDMS: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsLatLong }

constructor TValidatorIsLatLong.Create(const ACheckDMS: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FCheckDMS := ACheckDMS;
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsLatLong.Check: IDataValidatorResult;
const
  C_LAT = '^\(?[+-]?(90(\.0+)?|[1-8]?\d(\.\d+)?)$';
  C_LONG = '^\s?[+-]?(180(\.0+)?|1[0-7]\d(\.\d+)?|\d{1,2}(\.\d+)?)\)?$';
  C_LAT_DMS = '^(([1-8]?\d)\D+([1-5]?\d|60)\D+([1-5]?\d|60)(\.\d+)?|90\D+0\D+0)\D+[NSns]?$';
  C_LONG_DMS = '^\s*([1-7]?\d{1,2}\D+([1-5]?\d|60)\D+([1-5]?\d|60)(\.\d+)?|180\D+0\D+0)\D+[EWew]?$';
var
  LValue: string;
  R: Boolean;
  LValueLatLong: TArray<string>;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValueLatLong := LValue.Split([',']);

    if Length(LValueLatLong) = 2 then
      if FCheckDMS then
        R := TRegEx.IsMatch(LValueLatLong[0], C_LAT_DMS) and TRegEx.IsMatch(LValueLatLong[1], C_LONG_DMS)
      else
        R := TRegEx.IsMatch(LValueLatLong[0], C_LAT) and TRegEx.IsMatch(LValueLatLong[1], C_LONG);
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

end.
