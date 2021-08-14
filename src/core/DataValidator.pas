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

unit DataValidator;

interface

uses
  DataValidator.Intf, DataValidator.Context.Intf, DataValidator.Result.Intf,
  DataValidator.Types, DataValidator.Schema, DataValidator.Value, DataValidator.JSON,
  System.JSON;

type
  TDataValidatorLocaleLanguage = DataValidator.Types.TDataValidatorLocaleLanguage;

  IDataValidatorSchema = DataValidator.Intf.IDataValidatorSchema;
  IDataValidatorValue = DataValidator.Intf.IDataValidatorValue;
  IDataValidatorJSON = DataValidator.Intf.IDataValidatorJSON;

  IDataValidatorValueResult = DataValidator.Intf.IDataValidatorValueResult;
  IDataValidatorJSONResult = DataValidator.Intf.IDataValidatorJSONResult;

  IDataValidatorResult = DataValidator.Result.Intf.IDataValidatorResult;
  IDataValidatorSchemaContext = DataValidator.Context.Intf.IDataValidatorSchemaContext;

  TDataValidator = class
  private
  public
    class function Schema: IDataValidatorSchema;
    class function Values: IDataValidatorValue;
    class function JSON(const AJSON: TJSONObject): IDataValidatorJSON; overload;
    class function JSON(const AJSON: TJSONArray): IDataValidatorJSON; overload;
  end;

implementation

{ TDataValidator }

class function TDataValidator.Schema: IDataValidatorSchema;
begin
  Result := TDataValidatorSchema.Create();
end;

class function TDataValidator.Values: IDataValidatorValue;
begin
  Result := TDataValidatorValue.Create();
end;

class function TDataValidator.JSON(const AJSON: TJSONObject): IDataValidatorJSON;
begin
  Result := TDataValidatorJSON.Create(AJSON);
end;

class function TDataValidator.JSON(const AJSON: TJSONArray): IDataValidatorJSON;
begin
  Result := TDataValidatorJSON.Create(AJSON);
end;

end.
