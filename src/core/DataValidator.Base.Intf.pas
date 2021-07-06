{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Base.Intf;

interface

uses
  DataValidator.Types, DataValidator.ItemBase.Intf,
  System.Generics.Collections, System.Rtti;

type
  IDataValidatorSchema = interface;

  IDataValidatorsBase<T> = interface
    ['{91B6BE02-3C92-4CB5-8293-70B9005445DA}']
    // Schema
    function AddSchema(const ASchema: IDataValidatorSchema): IDataValidatorsBase<T>;

    // Validator
    function Contains(const AValueContains: string; const ACaseSensitive: Boolean = False): IDataValidatorsBase<T>;
    function IsAlpha(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): IDataValidatorsBase<T>;
    function IsAlphaNumeric(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): IDataValidatorsBase<T>;
    function IsBase64(): IDataValidatorsBase<T>;
    function IsBetween(const AValueA: Integer; const AValueB: Integer): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: Int64; const AValueB: Int64): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: Double; const AValueB: Double): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: Extended; const AValueB: Extended): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: Single; const AValueB: Single): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: UInt64; const AValueB: UInt64): IDataValidatorsBase<T>; overload;
    function IsBTCAddress(): IDataValidatorsBase<T>;
    function IsCEP(): IDataValidatorsBase<T>;
    function IsCNPJ(): IDataValidatorsBase<T>;
    function IsCPF(): IDataValidatorsBase<T>;
    function IsCPFCNPJ(): IDataValidatorsBase<T>;
    function IsDate(): IDataValidatorsBase<T>;
    function IsDateBetween(const AValueA: TDate; const AValueB: TDate): IDataValidatorsBase<T>;
    function IsDateEquals(const ACompareDate: TDate): IDataValidatorsBase<T>;
    function IsDateGreaterThan(const ACompareDate: TDate): IDataValidatorsBase<T>;
    function IsDateLessThan(const ACompareDate: TDate): IDataValidatorsBase<T>;
    function IsEmail(): IDataValidatorsBase<T>;
    function IsEmpty(): IDataValidatorsBase<T>;
    function IsEquals(const AValueEquals: string; const ACaseSensitive: Boolean = False): IDataValidatorsBase<T>;
    function IsEthereumAddress(): IDataValidatorsBase<T>;
    function IsGreaterThan(const AValueGreaterThan: Integer): IDataValidatorsBase<T>;
    function IsHexadecimal(): IDataValidatorsBase<T>;
    function IsInteger(): IDataValidatorsBase<T>;
    function IsIP(): IDataValidatorsBase<T>;
    function IsJSON(): IDataValidatorsBase<T>;
    function IsLength(const AMin: Integer; const AMax: Integer): IDataValidatorsBase<T>;
    function IsLessThan(const AValueLessThan: Integer): IDataValidatorsBase<T>;
    function IsLowercase(): IDataValidatorsBase<T>;
    function IsMACAddress(): IDataValidatorsBase<T>;
    function IsMD5(): IDataValidatorsBase<T>;
    function IsNegative(): IDataValidatorsBase<T>;
    function IsNumeric(): IDataValidatorsBase<T>;
    function IsPhoneNumber(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_pt_BR): IDataValidatorsBase<T>;
    function IsPositive(): IDataValidatorsBase<T>;
    function IsSSN(): IDataValidatorsBase<T>;
    function IsTime(): IDataValidatorsBase<T>;
    function IsTimeBetween(const AValueA: TTime; const AValueB: TTime): IDataValidatorsBase<T>;
    function IsTimeEquals(const ACompareTime: TTime): IDataValidatorsBase<T>;
    function IsTimeGreaterThan(const ACompareTime: TTime): IDataValidatorsBase<T>;
    function IsTimeLessThan(const ACompareTime: TTime): IDataValidatorsBase<T>;
    function IsUppercase(): IDataValidatorsBase<T>;
    function IsURL(): IDataValidatorsBase<T>;
    function IsUUID(): IDataValidatorsBase<T>;
    function IsUUIDv1(): IDataValidatorsBase<T>;
    function IsUUIDv2(): IDataValidatorsBase<T>;
    function IsUUIDv3(): IDataValidatorsBase<T>;
    function IsUUIDv4(): IDataValidatorsBase<T>;
    function IsUUIDv5(): IDataValidatorsBase<T>;
    function IsZero(): IDataValidatorsBase<T>;

    // Especial
    function &Not(): IDataValidatorsBase<T>;
    function Custom(const ADataItem: IDataValidatorItem): IDataValidatorsBase<T>; overload;
    function Custom(const AExecute: TDataValidatorCustomExecute): IDataValidatorsBase<T>; overload;

    // Sanitizer
    function NormalizeEmail(const AAllLowercase: Boolean = True; const AGmailRemoveDots: Boolean = True): IDataValidatorsBase<T>;
    function RemoveAccents(): IDataValidatorsBase<T>;
    function Replace(const AOldValue: string; const ANewValue: string): IDataValidatorsBase<T>;
    function ToBase64Decode(): IDataValidatorsBase<T>;
    function ToBase64Encode(): IDataValidatorsBase<T>;
    function ToHTMLDecode(): IDataValidatorsBase<T>;
    function ToHTMLEncode(): IDataValidatorsBase<T>;
    function ToInteger(): IDataValidatorsBase<T>;
    function ToLowerCase(): IDataValidatorsBase<T>;
    function ToMD5(): IDataValidatorsBase<T>;
    function ToNumeric(): IDataValidatorsBase<T>;
    function ToUpperCase(): IDataValidatorsBase<T>;
    function ToURLDecode(): IDataValidatorsBase<T>;
    function ToURLEncode(): IDataValidatorsBase<T>;
    function Trim(): IDataValidatorsBase<T>;
    function TrimLeft(): IDataValidatorsBase<T>;
    function TrimRight(): IDataValidatorsBase<T>;

    // Message
    function WithMessage(const AMessage: string): IDataValidatorsBase<T>; overload;
    function WithMessage(const AMessage: string; const AParams: array of const): IDataValidatorsBase<T>; overload;
    function Execute(const AExecute: TDataValidatorInformationExecute): IDataValidatorsBase<T>;
    function &End(): T;
  end;

  IDataValidatorsBaseItem<T> = interface(IDataValidatorsBase<T>)
    ['{35120564-5E08-4504-8D13-2C2C2B8DB628}']
    function GetItem(): TList<IDataValidatorItem>;
    function GetValue(): TValue;
  end;

  IDataValidatorSchema = interface
    ['{060BAB5C-AD75-4C88-93A3-CFEEB92085E1}']
    function ListSchema(): TList<IDataValidatorsBase<IDataValidatorSchema>>;
  end;

  IDataValidatorsBaseJSON<T> = interface(IDataValidatorsBase<T>)
     ['{2C60DD5E-3892-4CC0-A2C0-2DC99232BED9}']
    function IsOptionalKey(): IDataValidatorsBase<T>;
    function IsRequiredKey(): IDataValidatorsBase<T>;
  end;

implementation

end.
