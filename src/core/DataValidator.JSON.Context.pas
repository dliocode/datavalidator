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

unit DataValidator.JSON.Context;

interface

uses
  DataValidator.Types,
  DataValidator.Intf, DataValidator.Context,
  System.JSON, System.Generics.Collections, System.SysUtils;

type
  TDataValidatorJSONContext<T: IInterface> = class(TDataValidatorContext<IDataValidatorJSONContextValue<T>>, IDataValidatorJSONContext<T>, IDataValidatorJSONContextKey<T>, IDataValidatorJSONContextValue<T>)
  strict private
  [weak]
    FOwner: T;
  public
    function Key: IDataValidatorJSONContextKey<T>;
    function Value: IDataValidatorJSONContextValue<T>;

    // Key
    function IsOptional: IDataValidatorJSONContextKey<T>; overload;
    function IsOptional(const AExecute: TDataValidatorCustomResult): IDataValidatorJSONContextKey<T>; overload;
    function IsRequired: IDataValidatorJSONContextKey<T>; overload;
    function IsRequired(const AExecute: TDataValidatorCustomResult): IDataValidatorJSONContextKey<T>; overload;

    function WithMessage(const AMessage: TDataValidatorWithMessage): IDataValidatorJSONContextKey<T>; overload;
    function WithMessage(const AMessage: string): IDataValidatorJSONContextKey<T>; overload;
    function WithMessage(const AMessage: string; const AParams: array of const): IDataValidatorJSONContextKey<T>; overload;
    function Execute(const AExecute: TDataValidatorInformationExecute): IDataValidatorJSONContextKey<T>;

    // Value
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValue): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueMessage): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONMessage): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONSubValidator(const AExecute: TDataValidatorCustomJSONSubValidator): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONSubValidator(const AExecute: TDataValidatorCustomJSONSubValidatorMessage): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONSubValidator(const AExecute: TDataValidatorCustomJSONSubMessage): IDataValidatorJSONContextValue<T>; overload;

    function IsJSONNull: IDataValidatorJSONContextValue<T>;
    function IsJSONBoolean: IDataValidatorJSONContextValue<T>;
    function IsJSONNumeric: IDataValidatorJSONContextValue<T>;
    function IsJSONString: IDataValidatorJSONContextValue<T>;

    function JSONMinItems(const AMinItems: Integer): IDataValidatorJSONContextValue<T>;
    function JSONMaxItems(const AMaxItems: Integer): IDataValidatorJSONContextValue<T>;

    function IsOptional(const AExecute: TDataValidatorCustomJSONValue): IDataValidatorJSONContextValue<T>; overload;

    function &End: T;

    constructor Create(const AOwner: T; const AValue: TJSONPair);
  end;

implementation

uses
  Validator.JSON.Key.IsOptional,
  Validator.JSON.Key.IsRequired,
  Validator.JSON.SubValidator.Custom,
  Validator.JSON.Value.Custom,
  Validator.JSON.Value.IsJSONValue,
  Validator.JSON.Value.IsMinMaxItems,
  Validator.IsOptional;

{ TDataValidatorJSONContext<T> }

constructor TDataValidatorJSONContext<T>.Create(const AOwner: T; const AValue: TJSONPair);
begin
  FOwner := AOwner;

  inherited Create(Self, AValue);
end;

function TDataValidatorJSONContext<T>.Key: IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
end;

function TDataValidatorJSONContext<T>.Value: IDataValidatorJSONContextValue<T>;
begin
  Result := Self
end;

function TDataValidatorJSONContext<T>.IsOptional: IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONKeyIsOptional.Create(nil, 'Key ${key} is optional!'));
end;

function TDataValidatorJSONContext<T>.IsOptional(const AExecute: TDataValidatorCustomResult): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONKeyIsOptional.Create(AExecute, 'Key ${key} is optional!'));
end;

function TDataValidatorJSONContext<T>.IsRequired: IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONKeyIsRequired.Create(nil, 'Key ${key} is required!'));
end;

function TDataValidatorJSONContext<T>.IsRequired(const AExecute: TDataValidatorCustomResult): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONKeyIsRequired.Create(AExecute, 'Key ${key} is required!'));
end;

function TDataValidatorJSONContext<T>.WithMessage(const AMessage: TDataValidatorWithMessage): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  TDataValidatorContext < IDataValidatorJSONContextValue < T >> (Self).WithMessage(AMessage);
end;

function TDataValidatorJSONContext<T>.WithMessage(const AMessage: string): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  TDataValidatorContext < IDataValidatorJSONContextValue < T >> (Self).WithMessage(AMessage);
end;

function TDataValidatorJSONContext<T>.WithMessage(const AMessage: string; const AParams: array of const): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  TDataValidatorContext < IDataValidatorJSONContextValue < T >> (Self).WithMessage(AMessage, AParams);
end;

function TDataValidatorJSONContext<T>.Execute(const AExecute: TDataValidatorInformationExecute): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  TDataValidatorContext < IDataValidatorJSONContextValue < T >> (Self).Execute(AExecute);
end;

function TDataValidatorJSONContext<T>.CustomJSONValue(const AExecute: TDataValidatorCustomJSONValue): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONValueCustom.Create(AExecute, nil, nil, 'Value is false!'));
end;

function TDataValidatorJSONContext<T>.CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueMessage): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONValueCustom.Create(nil, AExecute, nil, 'Value is false!'));
end;

function TDataValidatorJSONContext<T>.CustomJSONValue(const AExecute: TDataValidatorCustomJSONMessage): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONValueCustom.Create(nil, nil, AExecute,  'Value is false!'));
end;

function TDataValidatorJSONContext<T>.CustomJSONSubValidator(const AExecute: TDataValidatorCustomJSONSubValidator): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONSubValidatorCustom.Create(AExecute, nil, nil, 'Value is false!'));
end;

function TDataValidatorJSONContext<T>.CustomJSONSubValidator(const AExecute: TDataValidatorCustomJSONSubValidatorMessage): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONSubValidatorCustom.Create(nil, AExecute, nil, 'Value is false!'));
end;

function TDataValidatorJSONContext<T>.CustomJSONSubValidator(const AExecute: TDataValidatorCustomJSONSubMessage): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONSubValidatorCustom.Create(nil, nil, AExecute, 'Value is false!'));
end;

function TDataValidatorJSONContext<T>.IsJSONNull: IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueIsJSONValue.Create(TTypeJSONValue.tjNull, 'Value not is null!'));
end;

function TDataValidatorJSONContext<T>.IsJSONBoolean: IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueIsJSONValue.Create(TTypeJSONValue.tjBoolean, 'Value not is boolean!'));
end;

function TDataValidatorJSONContext<T>.IsJSONNumeric: IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueIsJSONValue.Create(TTypeJSONValue.tjNumeric, 'Value not is numeric!'));
end;

function TDataValidatorJSONContext<T>.IsJSONString: IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueIsJSONValue.Create(TTypeJSONValue.tjString, 'Value not is string!'));
end;

function TDataValidatorJSONContext<T>.JSONMinItems(const AMinItems: Integer): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueIsMinMaxItems.Create(TTypeJSONValueMinMax.tmMin, AMinItems, Format('Its size is less than %d!', [AMinItems])));
end;

function TDataValidatorJSONContext<T>.JSONMaxItems(const AMaxItems: Integer): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueIsMinMaxItems.Create(TTypeJSONValueMinMax.tmMax, AMaxItems, Format('Its size is greater than %d!', [AMaxItems])));
end;

function TDataValidatorJSONContext<T>.IsOptional(const AExecute: TDataValidatorCustomJSONValue): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorIsOptional.Create(nil, AExecute, 'Value is optional!'));
end;

function TDataValidatorJSONContext<T>.&End: T;
begin
  Result := FOwner;
end;

end.
