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
    ['{AE61F42C-0EEF-4369-A748-11B16AEAB644}']
    function OK: Boolean;
    function Informations: IDataValidatorInformationsResult;
    function Values: TArray<string>;
  end;

implementation

end.
