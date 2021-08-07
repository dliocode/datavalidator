{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.JSON.Context.Intf;

interface

uses
  DataValidator.Context.Intf, DataValidator.Types, DataValidator.Result.Intf;

type
  IDataValidatorJSONContextKey<T> = interface;
  IDataValidatorJSONContextValue<T> = interface;

  IDataValidatorJSONContext<T> = interface
    ['{AA90066C-DB3C-4CDE-8D7E-5CE867FF4073}']
    function Key(): IDataValidatorJSONContextKey<T>;
    function Value(): IDataValidatorJSONContextValue<T>;
  end;

  IDataValidatorJSONContextKeyContext<T> = interface(IDataValidatorContextMessage < IDataValidatorJSONContextKey < T >> )
    ['{67057C09-3C02-46C6-A975-FF1037CF8E0B}']
  end;

  IDataValidatorJSONContextKey<T> = interface(IDataValidatorJSONContextKeyContext<T>)
    ['{54906AC4-363A-4031-927C-2D007C6279AC}']
    function &End(): T;
    function IsOptional(): IDataValidatorJSONContextKey<T>;
    function IsRequired(): IDataValidatorJSONContextKey<T>;
  end;

  IDataValidatorJSONContextValueContext<T> = interface(IDataValidatorContext < IDataValidatorJSONContextValue < T >> )
    ['{67057C09-3C02-46C6-A975-FF1037CF8E0B}']
  end;

  IDataValidatorJSONContextValue<T> = interface(IDataValidatorJSONContextValueContext<T>)
    ['{320BD31E-810F-4EB6-A8C4-C17CA2C186DB}']
    function &End(): T;
    function IsArray(): IDataValidatorJSONContextValue<T>;
    function IsObject(): IDataValidatorJSONContextValue<T>;
    function MinItems(const AMinItems: Integer): IDataValidatorJSONContextValue<T>;
    function MaxItems(const AMaxItems: Integer): IDataValidatorJSONContextValue<T>;
  end;

implementation

end.
