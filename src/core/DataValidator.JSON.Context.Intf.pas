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

unit DataValidator.JSON.Context.Intf;

interface

uses
  DataValidator.Context.Intf, DataValidator.Types;

type
  IDataValidatorJSONContextKey<T> = interface;
  IDataValidatorJSONContextValue<T> = interface;

  IDataValidatorJSONContext<T> = interface
    ['{AA90066C-DB3C-4CDE-8D7E-5CE867FF4073}']
    function Key(): IDataValidatorJSONContextKey<T>;
    function Value(): IDataValidatorJSONContextValue<T>;
  end;

  IDataValidatorJSONContextKeyContext<T> = interface(IDataValidatorContextMessage < IDataValidatorJSONContextKey < T >> )
    ['{67057C09-3C02-46C6-A975-FF1037CF8E0B}']
  end;

  IDataValidatorJSONContextKey<T> = interface(IDataValidatorJSONContextKeyContext<T>)
    ['{54906AC4-363A-4031-927C-2D007C6279AC}']
    function &End(): T;
    function IsOptional(): IDataValidatorJSONContextKey<T>; overload;
    function IsOptional(const AExecute: TDataValidatorFuncExecute): IDataValidatorJSONContextKey<T>; overload;
    function IsRequired(): IDataValidatorJSONContextKey<T>; overload;
    function IsRequired(const AExecute: TDataValidatorFuncExecute): IDataValidatorJSONContextKey<T>; overload;
  end;

  IDataValidatorJSONContextValueContext<T> = interface(IDataValidatorContext < IDataValidatorJSONContextValue < T >> )
    ['{67057C09-3C02-46C6-A975-FF1037CF8E0B}']
  end;

  IDataValidatorJSONContextValue<T> = interface(IDataValidatorJSONContextValueContext<T>)
    ['{320BD31E-810F-4EB6-A8C4-C17CA2C186DB}']
    function &End(): T;
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueExecute): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueMessageExecute): IDataValidatorJSONContextValue<T>; overload;
    function IsNull(): IDataValidatorJSONContextValue<T>;
    function MinItems(const AMinItems: Integer): IDataValidatorJSONContextValue<T>;
    function MaxItems(const AMaxItems: Integer): IDataValidatorJSONContextValue<T>;
  end;

implementation

end.
