{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator;

interface

uses
  DataValidator.Base.Intf, DataValidator.JSON.Intf, DataValidator.Values.Intf, DataValidator.Result.Intf,
  DataValidator.JSON, DataValidator.Values, DataValidator.Schema, DataValidator.Types,
  System.JSON;

type
  IDataValidatorValues = DataValidator.Values.Intf.IDataValidatorValues;
  IDataValidatorSchema = DataValidator.Base.Intf.IDataValidatorSchema;
  IDataValidatorResult = DataValidator.Result.Intf.IDataValidatorResult;
  TDataValidatorLocaleLanguage = DataValidator.Types.TDataValidatorLocaleLanguage;

  TDataValidator = class
  public
    class function Schema: IDataValidatorsBase<IDataValidatorSchema>;
    class function Values: IDataValidatorValuesBase;
    class function JSON(const AJSON: TJSONObject): IDataValidatorJSONBase;
  end;
implementation

{ TDataValidator }

class function TDataValidator.Schema: IDataValidatorsBase<IDataValidatorSchema>;
begin
  Result := TDataValidatorSchema.New;
end;

class function TDataValidator.Values: IDataValidatorValuesBase;
begin
  Result := TDataValidatorValues.New;
end;

class function TDataValidator.JSON(const AJSON: TJSONObject): IDataValidatorJSONBase;
begin
  Result := TDataValidatorJSON.New(AJSON);
end;

end.
