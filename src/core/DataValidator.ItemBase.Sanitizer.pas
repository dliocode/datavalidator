{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.ItemBase.Sanitizer;

interface

uses
  DataValidator.ItemBase.Intf, DataValidator.ItemBase, DataValidator.Result.Intf, DataValidator.Result, DataValidator.Information;

type
  TValue = DataValidator.ItemBase.TValue;
  IDataValidatorResult = DataValidator.Result.Intf.IDataValidatorResult;
  IDataSanitizerItem = DataValidator.ItemBase.Intf.IDataSanitizerItem;
  TDataValidatorResult = DataValidator.Result.TDataValidatorResult;
  TDataValidatorInformation = DataValidator.Information.TDataValidatorInformation;

  TDataValidatorItemBaseSanitizer = class(TDataValidatorItemBase, IDataSanitizerItem)
  private
  public
    function Check: IDataValidatorResult;
    function Sanitize: TValue; virtual; abstract;
  end;

implementation

{ TDataValidatorItemBaseSanitizer }

function TDataValidatorItemBaseSanitizer.Check: IDataValidatorResult;
begin
  // not implementation
end;

end.
