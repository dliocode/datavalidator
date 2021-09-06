{
  ********************************************************************************

  Github - https://github.com/dliocode/datavalidator

  ********************************************************************************

  MIT License

  Copyright (c) 2021 Danilo Lucas

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

unit DataValidator.Context.Intf;

interface

uses
  DataValidator.Types, DataValidator.ItemBase.Intf, DataValidator.Information.Intf,
  System.Generics.Collections, System.Rtti;

type
  IDataValidatorSchemaContext = interface
    ['{B282DFB4-FDDC-424C-8C76-C8265822C79B}']
  end;

  IDataValidatorContextMessage<T> = interface
    ['{54CF9567-25E9-4B1A-A62B-7F270E5351E2}']
    function WithMessage(const AMessage: string): T; overload;
    function WithMessage(const AMessage: string; const AParams: array of const): T; overload;
    function Execute(const AExecute: TDataValidatorInformationExecute): T;
  end;

  IDataValidatorContextSanitizer<T> = interface(IDataValidatorContextMessage<T>)
    ['{80BCAFBF-2BBB-46FA-9C39-E7CC7D188350}']
    function CustomSanitizer(const ASanitizerItem: IDataSanitizerItem): T; overload;
    function CustomSanitizer(const AExecute: TDataValidatorCustomSanitizerExecute): T; overload;
    function NormalizeEmail(const AAllLowercase: Boolean = True; const AGmailRemoveDots: Boolean = True): T;
    function OnlyNumbers(): T;
    function RemoveAccents(): T;
    function Replace(const AOldValue: string; const ANewValue: string): T;
    function ToBase64Decode(): T;
    function ToBase64Encode(): T;
    function ToDate(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function ToDateTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function ToHTMLDecode(): T;
    function ToHTMLEncode(): T;
    function ToInteger(): T;
    function ToLowerCase(): T;
    function ToMD5(): T;
    function ToNumeric(): T;
    function ToTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function ToUpperCase(): T;
    function ToURLDecode(): T;
    function ToURLEncode(): T;
    function Trim(): T;
    function TrimLeft(): T;
    function TrimRight(): T;
  end;

  IDataValidatorContextValidator<T> = interface(IDataValidatorContextSanitizer<T>)
    ['{F61EA315-86CA-4807-B1A1-F9030FADB844}']
    function CustomValue(const ADataItem: IDataValidatorItem): T; overload;
    function CustomValue(const AExecute: TDataValidatorCustomExecute): T; overload;
    function CustomValue(const AExecute: TDataValidatorCustomMessageExecute): T; overload;
    function Contains(const AValueContains: string; const ACaseSensitive: Boolean = False): T; overload;
    function Contains(const AValueContains: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;
    function EndsWith(const AValueEndsWith: string; const ACaseSensitive: Boolean = False): T; overload;
    function EndsWith(const AValueEndsWith: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;
    function IsAlpha(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): T;
    function IsAlphaNumeric(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): T;
    function IsAscii(): T;
    function IsBase32(): T;
    function IsBase58(): T;
    function IsBase64(): T;
    function IsBetween(const AValueA: Integer; const AValueB: Integer): T; overload;
    function IsBetween(const AValueA: Int64; const AValueB: Int64): T; overload;
    function IsBetween(const AValueA: Double; const AValueB: Double): T; overload;
    function IsBetween(const AValueA: Extended; const AValueB: Extended): T; overload;
    function IsBetween(const AValueA: Single; const AValueB: Single): T; overload;
    function IsBetween(const AValueA: UInt64; const AValueB: UInt64): T; overload;
    function IsBoolean(): T;
    function IsBTCAddress(): T;
    function IsCNPJ(): T;
    function IsCPF(): T;
    function IsCPFCNPJ(): T;
    function IsDate(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateBetween(const AValueA: TDate; const AValueB: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateEquals(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateGreaterThan(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateLessThan(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsDateTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsEmail(): T;
    function IsEmpty(): T;
    function IsEquals(const AValueEquals: string; const ACaseSensitive: Boolean = False): T; overload;
    function IsEquals(const AValueEquals: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;
    function IsEthereumAddress(): T;
    function IsGreaterThan(const AValueGreaterThan: Integer): T;
    function IsGTIN(): T;
    function IsGTIN8(): T;
    function IsGTIN12(): T;
    function IsGTIN13(): T;
    function IsGTIN14(): T;
    function IsHexadecimal(): T;
    function IsHexColor(): T;
    function IsInteger(): T;
    function IsIP(): T;
    function IsIPv4(): T;
    function IsIPv6(): T;
    function IsISO8601(): T;
    function IsJSON(): T;
    function IsJSONArray(): T;
    function IsJSONObject(): T;
    function IsJWT(): T;
    function IsLatLong(const ACheckDMS: Boolean = False): T;
    function IsLength(const AMin: Integer; const AMax: Integer): T;
    function IsLessThan(const AValueLessThan: Integer): T;
    function IsLocale(): T;
    function IsLowercase(): T;
    function IsMACAddress(): T;
    function IsMagnetURI(): T;
    function IsMD5(): T;
    function IsMimeType(): T;
    function IsMongoId(): T;
    function IsNegative(): T;
    function IsNumeric(): T;
    function IsOctal(): T;
    function IsOptional(): T; overload;
    function IsOptional(const AExecute: TDataValidatorCustomExecute): T; overload;
    function IsPassportNumber(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): T;
    function IsPhoneNumber(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): T;
    function IsPort(): T;
    function IsPositive(): T;
    function IsPostalCode(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): T;
    function IsRGBColor(): T;
    function IsSSN(): T;
    function IsTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsTimeBetween(const AValueA: TTime; const AValueB: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsTimeEquals(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsTimeGreaterThan(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsTimeLessThan(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
    function IsUppercase(): T;
    function IsURL(): T;
    function IsUUID(): T;
    function IsUUIDv1(): T;
    function IsUUIDv2(): T;
    function IsUUIDv3(): T;
    function IsUUIDv4(): T;
    function IsUUIDv5(): T;
    function IsZero(): T;
    function RegexIsMatch(const ARegex: string): T;
    function StartsWith(const AValueStartsWith: string; const ACaseSensitive: Boolean = False): T; overload;
    function StartsWith(const AValueStartsWith: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;

    function &Not(): T;
  end;

  IDataValidatorContextSchema<T> = interface(IDataValidatorContextValidator<T>)
    ['{01249950-D03B-4A14-BBE7-DB820FE1917E}']
    function AddSchema(const ASchema: IDataValidatorSchemaContext): T;
  end;

  IDataValidatorContext<T> = interface(IDataValidatorContextSchema<T>)
    ['{990209EE-C2A5-4C95-AE82-30277DF40B35}']
  end;

  IDataValidatorContextBase<T> = interface(IDataValidatorContext<T>)
    ['{990209EE-C2A5-4C95-AE82-30277DF40B35}']
    function GetItem(): TList<IDataValidatorItem>;
    function GetValue(): TValue;
  end;

implementation

end.
