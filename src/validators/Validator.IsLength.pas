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

unit Validator.IsLength;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.Math;

type
  TValidatorIsLength = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FMin: Integer;
    FMax: Integer;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMin: Integer; const AMax: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsLength }

constructor TValidatorIsLength.Create(const AMin: Integer; const AMax: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  inherited Create;

  FMin := AMin;
  FMax := AMax;

  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsLength.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LLength: Integer;
begin
  LValue := GetValueAsString;
  R := False;
  LLength := Length(LValue);

  if not Trim(LValue).IsEmpty then
  begin
    R := (LLength >= FMin) and (LLength <= FMax);

    if not R then
      R := (FMin = 0) and (LLength <= FMax);

    if not R then
      R := (LLength >= FMin) and (FMax = 0);
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(FKey, FName, LValue, GetMessage, FExecute));
end;

end.
