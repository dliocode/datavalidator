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

unit Validator.JSON.SubValidator.Custom;

interface

uses
  DataValidator.ItemBase, DataValidator.Intf,
  System.SysUtils, System.JSON;

type
  TValidatorJSONSubValidatorCustom = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCustomJSONSubValidatorExecute: TDataValidatorCustomJSONSubValidator;
    FCustomJSONSubValidatorMessageExecute: TDataValidatorCustomJSONSubValidatorMessage;
    FCustomJSONSubMessageExecute: TDataValidatorCustomJSONSubMessage;
  public
    function Check: IDataValidatorResult;
    constructor Create(
      const ACustomJSONSubValidatorExecute: TDataValidatorCustomJSONSubValidator;
      const ACustomJSONSubValidatorMessageExecute: TDataValidatorCustomJSONSubValidatorMessage;
      const ACustomJSONSubMessageExecute: TDataValidatorCustomJSONSubMessage;

      const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

uses
  DataValidator.JSON;

{ TValidatorJSONSubValidatorCustom }

constructor TValidatorJSONSubValidatorCustom.Create(
  const ACustomJSONSubValidatorExecute: TDataValidatorCustomJSONSubValidator;
  const ACustomJSONSubValidatorMessageExecute: TDataValidatorCustomJSONSubValidatorMessage;
  const ACustomJSONSubMessageExecute: TDataValidatorCustomJSONSubMessage;

  const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  inherited Create;

  FCustomJSONSubValidatorExecute := ACustomJSONSubValidatorExecute;
  FCustomJSONSubValidatorMessageExecute := ACustomJSONSubValidatorMessageExecute;
  FCustomJSONSubMessageExecute := ACustomJSONSubMessageExecute;

  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorJSONSubValidatorCustom.Check: IDataValidatorResult;
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
          if Assigned(FCustomJSONSubValidatorExecute) then
            R := FCustomJSONSubValidatorExecute(TDataValidatorJSON.Create(LJSONPair.JsonValue))
          else
          begin
            LMessage := GetMessage;

            if Assigned(FCustomJSONSubValidatorMessageExecute) then
              R := FCustomJSONSubValidatorMessageExecute(TDataValidatorJSON.Create(LJSONPair.JsonValue), LMessage.Message)
            else
              if Assigned(FCustomJSONSubMessageExecute) then
                R := FCustomJSONSubMessageExecute(TDataValidatorJSON.Create(LJSONPair.JsonValue), LMessage);

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
