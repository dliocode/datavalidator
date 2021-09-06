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

unit Validator.IsUUID; // UUID (Universally Unique Identifier)

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TTypeUUID = (tuAll, tuV1, tuV2, tuV3, tuV4, tuV5);

  TValidatorIsUUID = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FVersion: TTypeUUID;
    function GetPattern: string;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AVersion: TTypeUUID; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsUUID }

constructor TValidatorIsUUID.Create(const AVersion: TTypeUUID; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FVersion := AVersion;
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsUUID.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    R := TRegEx.IsMatch(LValue, GetPattern);

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, GetMessage, FExecute));
end;

function TValidatorIsUUID.GetPattern: string;
var
  LVersion: string;
begin
  if FVersion = tuAll then
    LVersion := Format('[1-%d]', [Integer(High(TTypeUUID))])
  else
    LVersion := Format('%d', [Integer(FVersion)]);

  Result := Format('^[0-9a-fA-F]{8}-?[0-9a-fA-F]{4}-?%s[0-9a-fA-F]{3}-?[89aAbB][0-9a-fA-F]{3}-?[0-9a-fA-F]{12}$', [LVersion]);
end;

end.
