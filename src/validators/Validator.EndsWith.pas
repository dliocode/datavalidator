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

unit Validator.EndsWith;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.StrUtils;

type
  TValidatorEndsWith = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueEndsWith: TArray<string>;
    FCaseSensitive: Boolean;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AValueEndsWith: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorEndsWith }

constructor TValidatorEndsWith.Create(const AValueEndsWith: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  inherited Create;

  FValueEndsWith := AValueEndsWith;
  FCaseSensitive := ACaseSensitive;

  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorEndsWith.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  I: Integer;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    for I := Low(FValueEndsWith) to High(FValueEndsWith) do
    begin
      if FCaseSensitive then
        R := EndsStr(FValueEndsWith[I], LValue)
      else
        R := EndsStr(LowerCase(FValueEndsWith[I]), LowerCase(LValue));

      if R then
        Break;
    end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(FKey, FName, LValue, GetMessage, FExecute));
end;

end.
