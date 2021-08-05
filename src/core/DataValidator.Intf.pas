{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Intf;

interface

uses
  DataValidator.Context.Intf, DataValidator.Result.Intf, DataValidator.JSON.Context.Intf;

type
  // Schema
  IDataValidatorSchemaBase = interface;
  IDataValidatorSchemaBaseContext = IDataValidatorContext<IDataValidatorSchemaBase>;

  IDataValidatorSchema = interface
    ['{9F24DF06-AD48-4FBA-A83D-5410369B8CC9}']
    function Validate: IDataValidatorSchemaBaseContext;
  end;

  IDataValidatorSchemaBase = interface(IDataValidatorSchemaBaseContext)
    ['{C57BE351-C1DA-4368-9053-D9B71A2A9607}']
    function &End: IDataValidatorSchemaContext;
  end;

  // Value
  IDataValidatorValueBase = interface;
  IDataValidatorValueBaseContext = IDataValidatorContext<IDataValidatorValueBase>;
  IDataValidatorValueResult = interface;

  IDataValidatorValue = interface
    ['{1D7D5CB4-5188-4F45-8878-A0120A9C1EC2}']
    function Validate(const AValue: string): IDataValidatorValueBaseContext;
  end;

  IDataValidatorValueBase = interface(IDataValidatorValueBaseContext)
    ['{6FAD251E-6E4E-4359-B81D-EC08A0684489}']
    function &End(): IDataValidatorValueResult;
  end;

  IDataValidatorValueResult = interface(IDataValidatorValue)
    ['{69331F14-D8A2-4E40-ADB4-D3195C59100E}']
    function Checked(): IDataValidatorResult;
    function CheckedAll(): IDataValidatorResult;
  end;

  // JSON
  IDataValidatorJSONBase = interface;
  IDataValidatorJSONBaseContext = IDataValidatorJSONContext<IDataValidatorJSONBase>;
  IDataValidatorJSONResult = interface;

  IDataValidatorJSON = interface
    ['{8409957E-995E-40F6-99F4-6867EEEA2E78}']
    function Validate(const AName: string): IDataValidatorJSONBaseContext;
  end;

  IDataValidatorJSONBase = interface(IDataValidatorJSONBaseContext)
    ['{0AE8315D-D7BF-48CF-8917-DB1AE2A0881B}']
    function &End(): IDataValidatorJSONResult;
  end;

  IDataValidatorJSONResult = interface(IDataValidatorJSON)
    ['{69331F14-D8A2-4E40-ADB4-D3195C59100E}']
    function Checked(): IDataValidatorResult;
    function CheckedAll(): IDataValidatorResult;
  end;

  IDataValidatorJSONValueName = interface
    ['{AE29455D-802F-4527-9E17-546A28091809}']
    function GetName: string;
  end;

implementation

end.
