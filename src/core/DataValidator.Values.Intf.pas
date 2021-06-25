{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Values.Intf;

interface

uses
  DataValidator.Base.Intf, DataValidator.Result.Intf, DataValidator.ItemBase.Intf;

type
  IDataValidatorValues = interface;

  IDataValidatorValuesBase = interface
    ['{59B444A8-3DC0-4B75-B027-FEA19739FB4C}']
    function Validate(const AValue: string): IDataValidatorsBase<IDataValidatorValues>;
  end;

  IDataValidatorValues = interface(IDataValidatorValuesBase)
    ['{69331F14-D8A2-4E40-ADB4-D3195C59100E}']
    function Checked: IDataValidatorResult;
    function CheckedAll: IDataValidatorResult;
  end;

implementation

end.
