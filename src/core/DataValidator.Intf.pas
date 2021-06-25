{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Intf;

interface

uses
  DataValidator.Base.Intf, DataValidator.Result.Intf, DataValidator.ItemBase.Intf;

type
  IDataValidatorValues = interface;
  IDataValidatorJSON = interface;
  IDataValidatorJSONBase = interface;

  // Validator

  IDataValidatorValuesBase = interface
    ['{59B444A8-3DC0-4B75-B027-FEA19739FB4C}']
    function Validate(const AValue: string): IDataValidatorsBase<IDataValidatorValues>;
//    function Schema: IDataValidatorsBase<IDataValidatorSchema>;
  end;

  IDataValidatorValues = interface(IDataValidatorValuesBase)
    ['{69331F14-D8A2-4E40-ADB4-D3195C59100E}']
    function Checked: IDataValidatorResult;
    function CheckedAll: IDataValidatorResult;
  end;

  // Validator JSON

  IDataValidatorJSONBase = interface
    ['{846E4126-B5B8-47AA-8F79-06A403A129C3}']
    function Validate(const AName: string): IDataValidatorsBaseJSON<IDataValidatorJSON>;
  end;

  IDataValidatorJSON = interface(IDataValidatorJSONBase)
    ['{9D8CFF0F-FB49-4627-A7A4-851CAD97BC69}']
    function Checked: IDataValidatorResult;
    function CheckedAll: IDataValidatorResult;
  end;

implementation

end.
