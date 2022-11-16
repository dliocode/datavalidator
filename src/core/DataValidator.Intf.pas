{
  ********************************************************************************

  Github - https://github.com/dliocode/datavalidator

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
}

unit DataValidator.Intf;

interface

uses
  DataValidator.Types,
  System.Generics.Collections, System.Rtti;

type
  IDataValidatorValue = interface;
  IDataValidatorJSON = interface;

  // Information
  IDataValidatorInformation = interface
    ['{972F5617-FDED-4D8E-8F89-5F372C1D62AB}']
    function Key: string;
    function Name: string;
    function Value: string;
    function Messages: TDataValidatorMessage;
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

  // Result
  IDataValidatorResult = interface
    ['{1527205B-B5F7-4058-B056-53C4F89EC8C9}']
    function OK: Boolean;
    function Informations: IDataValidatorInformationsResult;
    function Values: TArray<string>;
  end;

  // ItemBase
  IDataValidatorItemBase = interface
    ['{7A448738-20D6-439D-868C-F28D135B65D8}']
    function GetDataValidatorLocaleLanguage: TDataValidatorLocaleLanguage;
    procedure SetDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = TDataValidatorLocaleLanguage.tl_en_US);
    procedure SetIsNot(const AIsNot: Boolean);

    procedure SetKey(const AKey: string);
    procedure SetName(const AName: string);
    procedure SetValue(const AValue: TValue);
    procedure SetIndex(const AValue: string);
    procedure SetMessage(const AMessage: string); overload;
    procedure SetMessage(const AMessage: TDataValidatorMessage); overload;
    procedure SetExecute(const AExecute: TDataValidatorInformationExecute); overload;
  end;

  IDataValidatorItem = interface(IDataValidatorItemBase)
    ['{277F2E2E-BBA1-4823-8DCC-8D4FD399CF02}']
    function Check: IDataValidatorResult;
  end;

  IDataSanitizerItem = interface(IDataValidatorItem)
    ['{0491AB3E-2D23-42AC-9CE8-FBA77C3D253D}']
    function Sanitize: TValue;
  end;

  // Context
  IDataValidatorContextMessage<T> = interface
    ['{54CF9567-25E9-4B1A-A62B-7F270E5351E2}']
    function WithMessage(const AMessage: TDataValidatorWithMessage): T; overload;
    function WithMessage(const AMessage: string): T; overload;
    function WithMessage(const AMessage: string; const AParams: array of const): T; overload;
    function Execute(const AExecute: TDataValidatorInformationExecute): T;
  end;

  IDataValidatorContextSanitizer<T> = interface(IDataValidatorContextMessage<T>)
    ['{80BCAFBF-2BBB-46FA-9C39-E7CC7D188350}']
    function CustomSanitizer(const ASanitizerItem: IDataSanitizerItem): T; overload;
    function CustomSanitizer(const AExecute: TDataValidatorCustomSanitizer): T; overload;
    function NormalizeEmail(const AAllLowercase: Boolean = True; const AGmailRemoveDots: Boolean = True): T;
    function OnlyNumbers: T;
    function RemoveAccents: T;
    function Replace(const AOldValue: string; const ANewValue: string): T;
    function ToBase64Decode: T;
    function ToBase64Encode: T;
    function ToDate(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function ToDateTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function ToHTMLDecode: T;
    function ToHTMLEncode: T;
    function ToInteger: T;
    function ToLowerCase: T;
    function ToMD5: T;
    function ToNumeric: T;
    function ToTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function ToUpperCase: T;
    function ToURLDecode: T;
    function ToURLEncode: T;
    function Trim: T;
    function TrimLeft: T;
    function TrimRight: T;
  end;

  IDataValidatorContextValidator<T> = interface(IDataValidatorContextSanitizer<T>)
    ['{F61EA315-86CA-4807-B1A1-F9030FADB844}']
    function CustomValue(const ADataItem: IDataValidatorItem): T; overload;
    function CustomValue(const AExecute: TDataValidatorCustomValue): T; overload;
    function CustomValue(const AExecute: TDataValidatorCustomValueMessage): T; overload;
    function CustomValue(const AExecute: TDataValidatorCustomMessage): T; overload;
    function Contains(const AValueContains: string; const ACaseSensitive: Boolean = False): T; overload;
    function Contains(const AValueContains: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;
    function EndsWith(const AValueEndsWith: string; const ACaseSensitive: Boolean = False): T; overload;
    function EndsWith(const AValueEndsWith: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;
    function IsAlpha(const ALocaleLanguage: TDataValidatorLocaleLanguage = TDataValidatorLocaleLanguage.tl_en_US; const AAllowedCharacters: TArray<Char> = []): T;
    function IsAlphaNumeric(const ALocaleLanguage: TDataValidatorLocaleLanguage = TDataValidatorLocaleLanguage.tl_en_US; const AAllowedCharacters: TArray<Char> = []): T;
    function IsAscii: T;
    function IsBase32: T;
    function IsBase58: T;
    function IsBase64: T;
    function IsBetween(const AValueA: Integer; const AValueB: Integer): T; overload;
    function IsBetween(const AValueA: Int64; const AValueB: Int64): T; overload;
    function IsBetween(const AValueA: Double; const AValueB: Double): T; overload;
    function IsBetween(const AValueA: Extended; const AValueB: Extended): T; overload;
    function IsBetween(const AValueA: Single; const AValueB: Single): T; overload;
    function IsBetween(const AValueA: UInt64; const AValueB: UInt64): T; overload;
    function IsBoolean: T;
    function IsBTCAddress: T;
    function IsCNPJ: T;
    function IsCPF: T;
    function IsCPFCNPJ: T;
    function IsDate(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateBetween(const AValueA: TDate; const AValueB: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateEquals(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateGreaterThan(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateLessThan(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsEmail: T;
    function IsEmpty: T;
    function IsEquals(const AValueEquals: string; const ACaseSensitive: Boolean = False): T; overload;
    function IsEquals(const AValueEquals: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;
    function IsEthereumAddress: T;
    function IsGreaterThan(const AValueGreaterThan: Integer): T;
    function IsGTIN: T;
    function IsGTIN8: T;
    function IsGTIN12: T;
    function IsGTIN13: T;
    function IsGTIN14: T;
    function IsHexadecimal: T;
    function IsHexColor: T;
    function IsInteger: T;
    function IsIP: T;
    function IsIPv4: T;
    function IsIPv6: T;
    function IsISO8601: T;
    function IsJSON: T;
    function IsJSONArray: T;
    function IsJSONObject: T;
    function IsJWT: T;
    function IsLatLong(const ACheckDMS: Boolean = False): T;
    function IsLength(const AMin: Integer; const AMax: Integer): T;
    function IsLessThan(const AValueLessThan: Integer): T;
    function IsLocale: T;
    function IsLowercase: T;
    function IsMACAddress: T;
    function IsMagnetURI: T;
    function IsMD5: T;
    function IsMimeType: T;
    function IsMongoId: T;
    function IsNegative: T;
    function IsNumeric: T;
    function IsOctal: T;
    function IsOptional: T; overload;
    function IsOptional(const AExecute: TDataValidatorCustomValue): T; overload;
    function IsPassportNumber(const ALocaleLanguage: TDataValidatorLocaleLanguage = TDataValidatorLocaleLanguage.tl_en_US): T;
    function IsPhoneNumber(const ALocaleLanguage: TDataValidatorLocaleLanguage = TDataValidatorLocaleLanguage.tl_en_US): T;
    function IsPort: T;
    function IsPositive: T;
    function IsPostalCode(const ALocaleLanguage: TDataValidatorLocaleLanguage = TDataValidatorLocaleLanguage.tl_en_US): T;
    function IsRGBColor: T;
    function IsSSN: T;
    function IsTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsTimeBetween(const AValueA: TTime; const AValueB: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsTimeEquals(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsTimeGreaterThan(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsTimeLessThan(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsUppercase: T;
    function IsURL: T;
    function IsUUID: T;
    function IsUUIDv1: T;
    function IsUUIDv2: T;
    function IsUUIDv3: T;
    function IsUUIDv4: T;
    function IsUUIDv5: T;
    function IsZero: T;
    function RegexIsMatch(const ARegex: string): T;
    function StartsWith(const AValueStartsWith: string; const ACaseSensitive: Boolean = False): T; overload;
    function StartsWith(const AValueStartsWith: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;

    function &Not: T;
  end;

  // Schema Context
  IDataValidatorSchemaContext = interface
    ['{B282DFB4-FDDC-424C-8C76-C8265822C79B}']
  end;

  IDataValidatorContextSchema<T> = interface(IDataValidatorContextValidator<T>)
    ['{01249950-D03B-4A14-BBE7-DB820FE1917E}']
    function AddSchema(const ASchema: IDataValidatorSchemaContext): T;
  end;

  // ValueContext

  IDataValidatorContext<T> = interface(IDataValidatorContextSchema<T>)
    ['{990209EE-C2A5-4C95-AE82-30277DF40B35}']
  end;

  IDataValidatorContextBase<T> = interface(IDataValidatorContext<T>)
    ['{990209EE-C2A5-4C95-AE82-30277DF40B35}']
    function GetItem: TList<IDataValidatorItem>;
    function GetValue: TValue;
  end;


  // JSONContext
  IDataValidatorJSONContextKey<T> = interface;
  IDataValidatorJSONContextValue<T> = interface;

  IDataValidatorJSONContext<T> = interface
    ['{AA90066C-DB3C-4CDE-8D7E-5CE867FF4073}']
    function Key: IDataValidatorJSONContextKey<T>;
    function Value: IDataValidatorJSONContextValue<T>;
  end;

  IDataValidatorJSONContextKeyContext<T> = interface(IDataValidatorContextMessage < IDataValidatorJSONContextKey < T >> )
    ['{67057C09-3C02-46C6-A975-FF1037CF8E0B}']
  end;

  IDataValidatorJSONContextKey<T> = interface(IDataValidatorJSONContextKeyContext<T>)
    ['{54906AC4-363A-4031-927C-2D007C6279AC}']
    function &End: T;
    function IsOptional: IDataValidatorJSONContextKey<T>; overload;
    function IsOptional(const AExecute: TDataValidatorCustomResult): IDataValidatorJSONContextKey<T>; overload;
    function IsRequired: IDataValidatorJSONContextKey<T>; overload;
    function IsRequired(const AExecute: TDataValidatorCustomResult): IDataValidatorJSONContextKey<T>; overload;
  end;

  IDataValidatorJSONContextValueContext<T> = interface(IDataValidatorContext < IDataValidatorJSONContextValue < T >> )
    ['{67057C09-3C02-46C6-A975-FF1037CF8E0B}']
  end;

  TDataValidatorCustomJSONSubValidator = reference to function(const AValue: IDataValidatorJSON): Boolean;
  TDataValidatorCustomJSONSubValidatorMessage = reference to function(const AValue: IDataValidatorJSON; var AMessage: string): Boolean;
  TDataValidatorCustomJSONSubMessage = reference to function(const AValue: IDataValidatorJSON; var AMessage: TDataValidatorMessage): Boolean;

  IDataValidatorJSONContextValue<T> = interface(IDataValidatorJSONContextValueContext<T>)
    ['{320BD31E-810F-4EB6-A8C4-C17CA2C186DB}']
    function &End: T;

    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValue): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueMessage): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONMessage): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONSubValidator(const AExecute: TDataValidatorCustomJSONSubValidator): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONSubValidator(const AExecute: TDataValidatorCustomJSONSubValidatorMessage): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONSubValidator(const AExecute: TDataValidatorCustomJSONSubMessage): IDataValidatorJSONContextValue<T>; overload;
    function IsJSONNull: IDataValidatorJSONContextValue<T>;
    function IsJSONBoolean: IDataValidatorJSONContextValue<T>;
    function IsJSONNumeric: IDataValidatorJSONContextValue<T>;
    function IsJSONString: IDataValidatorJSONContextValue<T>;
    function JSONMinItems(const AMinItems: Integer): IDataValidatorJSONContextValue<T>;
    function JSONMaxItems(const AMaxItems: Integer): IDataValidatorJSONContextValue<T>;
    function IsOptional(const AExecute: TDataValidatorCustomJSONValue): IDataValidatorJSONContextValue<T>; overload;
  end;

  // DataValidator

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
    function Validate(const AValue: string; const AName: string = ''): IDataValidatorValueBaseContext; overload;
    function Validate(const AValue: TArray<string>; const AName: string = ''): IDataValidatorValueBaseContext; overload;
  end;

  IDataValidatorValueBase = interface(IDataValidatorValueBaseContext)
    ['{6FAD251E-6E4E-4359-B81D-EC08A0684489}']
    function &End: IDataValidatorValueResult;
  end;

  IDataValidatorValueResult = interface(IDataValidatorValue)
    ['{69331F14-D8A2-4E40-ADB4-D3195C59100E}']
    function Check: IDataValidatorResult;
    function CheckAll(const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;
  end;

  IDataValidatorValueValues = interface
    ['{AE29455D-802F-4527-9E17-546A28091809}']
    function GetValues: TArray<string>;
    function GetName: string;
  end;

  // JSON
  IDataValidatorJSONBase = interface;
  IDataValidatorJSONBaseContext = IDataValidatorJSONContext<IDataValidatorJSONBase>;
  IDataValidatorJSONResult = interface;

  IDataValidatorJSON = interface
    ['{8409957E-995E-40F6-99F4-6867EEEA2E78}']
    function Validate(const AKey: TArray<string>; const AName: string = ''): IDataValidatorJSONBaseContext; overload;
    function Validate(const AKey: string; const AName: string = ''): IDataValidatorJSONBaseContext; overload;
  end;

  IDataValidatorJSONBase = interface(IDataValidatorJSONBaseContext)
    ['{0AE8315D-D7BF-48CF-8917-DB1AE2A0881B}']
    function &End: IDataValidatorJSONResult;
  end;

  IDataValidatorJSONResult = interface(IDataValidatorJSON)
    ['{69331F14-D8A2-4E40-ADB4-D3195C59100E}']
    function Check: IDataValidatorResult;
    function CheckAll(const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;

    function CheckItem(const AKey: string): IDataValidatorResult;
    function CheckItemAll(const AKey: string): IDataValidatorResult;
  end;

  IDataValidatorJSONValues = interface
    ['{A7C6C273-713D-46C1-9320-2417BA7FE636}']
    function GetName: string;
  end;

implementation

end.
