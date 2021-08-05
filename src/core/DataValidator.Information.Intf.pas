{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Information.Intf;

interface

uses
  System.SysUtils;

type
  TDataValidatorInformationExecute = TProc;

  IDataValidatorInformation = interface
    ['{972F5617-FDED-4D8E-8F89-5F372C1D62AB}']
    function Value: string;
    function Message: string;
    function Execute: TDataValidatorInformationExecute;
    procedure OnExecute;
  end;

  IDataValidatorInformationsResult = interface
    ['{571983C3-94A1-4FB3-A855-6E5B37BC56C8}']
    function GetItem(const Index: Integer): IDataValidatorInformation;
    function Count: Integer;
    function Message: string;
  end;

  IDataValidatorInformations = interface(IDataValidatorInformationsResult)
    ['{8DF2AE1E-860E-4488-8052-4C94E6F1F3A1}']
    function Add(const ADataInformation: IDataValidatorInformation): IDataValidatorInformations; overload;
    function Add(const ADataInformations: IDataValidatorInformations): IDataValidatorInformations; overload;
  end;

implementation

end.
