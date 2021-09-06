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

unit Validator.IsIP; // IP (Internet Protocol)

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TTypeIPVersion = (tvAll, tvIPv4, tvIPv6);

  TValidatorIsIP = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FVersion: TTypeIPVersion;
    function GetPattern(const AVersion: TTypeIPVersion): string;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AVersion: TTypeIPVersion; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsIP }

constructor TValidatorIsIP.Create(const AVersion: TTypeIPVersion; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FVersion := AVersion;
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsIP.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    if FVersion = TTypeIPVersion.tvAll then
      R := TRegEx.IsMatch(LValue, GetPattern(TTypeIPVersion.tvIPv4)) or TRegEx.IsMatch(LValue, GetPattern(TTypeIPVersion.tvIPv6))
    else
      R := TRegEx.IsMatch(LValue, GetPattern(FVersion));

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

function TValidatorIsIP.GetPattern(const AVersion: TTypeIPVersion): string;
const
  C_IPv4SegmentFormat = '([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])';
  C_IPv4AddressFormat = '(' + C_IPv4SegmentFormat + '[.]){3}' + C_IPv4SegmentFormat;
  C_IPv4AddressRegExp = '^' + C_IPv4AddressFormat + '$';

const
  C_IPv6SegmentFormat = '([0-9a-fA-F]{1,4})';
  C_IPv6AddressRegExp =
    '^(' + C_IPv6SegmentFormat + '{7,7}' + C_IPv6SegmentFormat + '|' +
    C_IPv6SegmentFormat + '{1,7}:|' +
    C_IPv6SegmentFormat + '{1,6}:' + C_IPv6SegmentFormat + '|' +
    C_IPv6SegmentFormat + '{1,5}(:' + C_IPv6SegmentFormat + '){1,2}|' +
    C_IPv6SegmentFormat + '{1,4}(:' + C_IPv6SegmentFormat + '){1,3}|' +
    C_IPv6SegmentFormat + '{1,3}(:' + C_IPv6SegmentFormat + '){1,4}|' +
    C_IPv6SegmentFormat + '{1,2}(:' + C_IPv6SegmentFormat + '){1,5}|' +
    C_IPv6SegmentFormat + ':((:' + C_IPv6SegmentFormat + '){1,6})|:((:' + C_IPv6SegmentFormat + '){1,7}|:)|' +
    'fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}){0,1}' +
    C_IPv4AddressFormat + '|' + C_IPv6SegmentFormat + '{1,4}:' + C_IPv4AddressFormat + ')$';

begin
  case AVersion of
    tvIPv4:
      Result := C_IPv4AddressRegExp;
    tvIPv6:
      Result := C_IPv6AddressRegExp;
  end;
end;

end.
