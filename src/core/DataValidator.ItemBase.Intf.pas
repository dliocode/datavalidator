{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.ItemBase.Intf;

interface

uses
  DataValidator.Types, DataValidator.Result.Intf,
  System.RTTI;

type
  IDataValidatorItemBase = interface
    ['{7A448738-20D6-439D-868C-F28D135B65D8}']
    function GetDataValidatorLocaleLanguage: TDataValidatorLocaleLanguage;
    procedure SeTDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US);
    procedure SetIsNot(const AIsNot: Boolean);

    procedure SetValue(const AValue: TValue);
    procedure SetMessage(const AMessage: string);
    procedure SetExecute(const AExecute: TDataValidatorInformationExecute); overload;
  end;

  IDataValidatorItem = interface(IDataValidatorItemBase)
    ['{277F2E2E-BBA1-4823-8DCC-8D4FD399CF02}']
    function Checked: IDataValidatorResult;
  end;

  IDataSanitizerItem = interface(IDataValidatorItem)
    ['{0491AB3E-2D23-42AC-9CE8-FBA77C3D253D}']
    function Sanitize: TValue;
  end;

implementation

end.
