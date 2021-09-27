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

unit DataValidator.JSON.Context;

interface

uses
  DataValidator.JSON.Context.Intf, DataValidator.ItemBase.Intf, DataValidator.Information.Intf,
  DataValidator.Types, DataValidator.Context,
  System.JSON, System.Generics.Collections, System.SysUtils;

type
  TDataValidatorJSONContext<T: IInterface> = class(TDataValidatorContext<IDataValidatorJSONContextValue<T>>, IDataValidatorJSONContext<T>, IDataValidatorJSONContextKey<T>, IDataValidatorJSONContextValue<T>)
  strict private
  [weak]
    FOwner: T;
  public
    function Key(): IDataValidatorJSONContextKey<T>;
    function Value(): IDataValidatorJSONContextValue<T>;

    // Key
    function IsOptional(): IDataValidatorJSONContextKey<T>; overload;
    function IsOptional(const AExecute: TDataValidatorFuncExecute): IDataValidatorJSONContextKey<T>; overload;
    function IsRequired(): IDataValidatorJSONContextKey<T>; overload;
    function IsRequired(const AExecute: TDataValidatorFuncExecute): IDataValidatorJSONContextKey<T>; overload;

    function WithMessage(const AMessage: string): IDataValidatorJSONContextKey<T>; overload;
    function WithMessage(const AMessage: string; const AParams: array of const): IDataValidatorJSONContextKey<T>; overload;
    function Execute(const AExecute: TDataValidatorInformationExecute): IDataValidatorJSONContextKey<T>;

    // Value
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueExecute): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueMessageExecute): IDataValidatorJSONContextValue<T>; overload;
    function IsNull(): IDataValidatorJSONContextValue<T>;
    function MinItems(const AMinItems: Integer): IDataValidatorJSONContextValue<T>;
    function MaxItems(const AMaxItems: Integer): IDataValidatorJSONContextValue<T>;

    function &End(): T;

    constructor Create(const AOwner: T; const AValue: TJSONPair);
    destructor Destroy; override;
  end;

implementation

uses
  Validator.JSON.Key.IsOptional,
  Validator.JSON.Key.IsRequired,
  Validator.JSON.Value.Custom,
  Validator.JSON.Value.IsNull,
  Validator.JSON.Value.MinItems,
  Validator.JSON.Value.MaxItems;

{ TDataValidatorJSONContext<T> }

constructor TDataValidatorJSONContext<T>.Create(const AOwner: T; const AValue: TJSONPair);
begin
  FOwner := AOwner;

  inherited Create(Self, AValue);
end;

destructor TDataValidatorJSONContext<T>.Destroy;
begin
  inherited;
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

function TDataValidatorJSONContext<T>.IsOptional(const AExecute: TDataValidatorFuncExecute): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONKeyIsOptional.Create(AExecute, 'Key ${key} is optional!'));
end;

function TDataValidatorJSONContext<T>.IsRequired: IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONKeyIsRequired.Create(nil, 'Key ${key} is required!'));
end;

function TDataValidatorJSONContext<T>.IsRequired(const AExecute: TDataValidatorFuncExecute): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONKeyIsRequired.Create(AExecute, 'Key ${key} is required!'));
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

function TDataValidatorJSONContext<T>.CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueExecute): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONValueCustom.Create(AExecute, nil, 'Value is false!'));
end;

function TDataValidatorJSONContext<T>.CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueMessageExecute): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONValueCustom.Create(nil, AExecute, 'Value is false!'));
end;

function TDataValidatorJSONContext<T>.IsNull: IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueIsNull.Create('Value not is null!'));
end;

function TDataValidatorJSONContext<T>.MinItems(const AMinItems: Integer): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueMinItems.Create(AMinItems, Format('Its size is less than %d!', [AMinItems])));
end;

function TDataValidatorJSONContext<T>.MaxItems(const AMaxItems: Integer): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueMaxItems.Create(AMaxItems, Format('Its size is greater than %d!', [AMaxItems])));
end;

function TDataValidatorJSONContext<T>.&End: T;
begin
  Result := FOwner;
end;

end.
