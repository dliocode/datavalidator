{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Result.Intf;

interface

uses
  DataValidator.Information.Intf;

type
  IDataValidatorResult = interface
    ['{1527205B-B5F7-4058-B056-53C4F89EC8C9}']
    function OK: Boolean;
    function Informations: IDataValidatorInformationsResult;
    function Values: TArray<string>;
  end;

implementation

end.
