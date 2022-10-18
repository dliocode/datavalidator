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

unit Validator.JSON.Value.Custom;

interface

uses
  DataValidator.ItemBase, DataValidator.Intf,
  System.SysUtils, System.JSON;

type
  TValidatorJSONValueCustom = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCustomJSONValueExecute: TDataValidatorCustomJSONValue;
    FCustomJSONValueMessageExecute: TDataValidatorCustomJSONValueMessage;
    FCustomJSONMessageExecute: TDataValidatorCustomJSONMessage;
  public
    function Check: IDataValidatorResult;
    constructor Create(
      const ACustomJSONValueExecute: TDataValidatorCustomJSONValue;
      const ACustomJSONValueMessageExecute: TDataValidatorCustomJSONValueMessage;
      const ACustomJSONMessageMessageExecute: TDataValidatorCustomJSONMessage;

      const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorJSONValueCustom }

constructor TValidatorJSONValueCustom.Create(
  const ACustomJSONValueExecute: TDataValidatorCustomJSONValue;
  const ACustomJSONValueMessageExecute: TDataValidatorCustomJSONValueMessage;
  const ACustomJSONMessageMessageExecute: TDataValidatorCustomJSONMessage;

  const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  inherited Create;

  FCustomJSONValueExecute := ACustomJSONValueExecute;
  FCustomJSONValueMessageExecute := ACustomJSONValueMessageExecute;
  FCustomJSONMessageExecute := ACustomJSONMessageMessageExecute;

  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorJSONValueCustom.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LJSONPair: TJSONPair;
  LMessage: TDataValidatorMessage;
begin
  LValue := GetValueAsString;
  R := False;

  try
    if not Trim(LValue).IsEmpty then
      if FValue.IsType<TJSONPair> then
      begin
        LJSONPair := FValue.AsType<TJSONPair>;

        if Assigned(LJSONPair) then
        begin
          if Assigned(FCustomJSONValueExecute) then
            R := FCustomJSONValueExecute(LJSONPair.JsonValue)
          else
          begin
            LMessage := GetMessage;

            if Assigned(FCustomJSONValueMessageExecute) then
              R := FCustomJSONValueMessageExecute(LJSONPair.JsonValue, LMessage.Message)
            else
              if Assigned(FCustomJSONMessageExecute) then
                R := FCustomJSONMessageExecute(LJSONPair.JsonValue, LMessage);

            SetMessage(LMessage);
          end;

          LValue := GetValueAsString;
        end;
      end;
  except
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(FKey, FName, LValue, GetMessage, FExecute));
end;

end.
