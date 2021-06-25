{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.JSON.Intf;

interface

uses
  DataValidator.Base.Intf, DataValidator.Result.Intf, DataValidator.ItemBase.Intf;

type
  IDataValidatorJSON = interface;
  IDataValidatorJSONBase = interface;

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
