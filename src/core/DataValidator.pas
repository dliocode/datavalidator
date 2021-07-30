{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator;

interface

uses
  DataValidator.Intf, DataValidator.Context.Intf, DataValidator.Result.Intf,
  DataValidator.Types, DataValidator.Schema, DataValidator.Value, DataValidator.JSON,
  System.JSON;

type
  TDataValidatorLocaleLanguage = DataValidator.Types.TDataValidatorLocaleLanguage;

  IDataValidatorSchemaContext = DataValidator.Context.Intf.IDataValidatorSchemaContext;
  IDataValidatorValueResult = DataValidator.Intf.IDataValidatorValueResult;
  IDataValidatorJSONResult = DataValidator.Intf.IDataValidatorJSONResult;
  IDataValidatorResult = DataValidator.Result.Intf.IDataValidatorResult;

  TDataValidator = class
  private
  public
    class function Schema: IDataValidatorSchema;
    class function Values: IDataValidatorValue;
    class function JSON(const AJSON: TJSONObject): IDataValidatorJSON;
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

end.
