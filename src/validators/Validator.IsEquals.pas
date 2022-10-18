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

unit Validator.IsEquals;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.Variants;

type
  TValidatorIsEquals = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueEquals: TArray<string>;
    FCaseSensitive: Boolean;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AValueEquals: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsEquals }

constructor TValidatorIsEquals.Create(const AValueEquals: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  inherited Create;

  FValueEquals := AValueEquals;
  FCaseSensitive := ACaseSensitive;

  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsEquals.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  I: Integer;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    for I := Low(FValueEquals) to High(FValueEquals) do
    begin
      if FCaseSensitive then
        R := VarCompareValue(LValue, FValueEquals[I]) = vrEqual
      else
        R := VarCompareValue(LowerCase(LValue), LowerCase(FValueEquals[I])) = vrEqual;

      if R then
        Break;
    end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(FKey, FName, LValue, GetMessage, FExecute));
end;

end.
