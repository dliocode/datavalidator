{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Context;

interface

uses
  DataValidator.Types,
  DataValidator.Context.Intf, DataValidator.ItemBase.Intf,
  System.RTTI, System.JSON, System.Generics.Collections, System.SysUtils, System.Variants;

type
  TDataValidatorContext<T: IInterface> = class(TInterfacedObject, IDataValidatorContext<T>, IDataValidatorContextBase<T>)
  private
    [weak]
    FOwner: T;
    FValue: TValue;

    FList: TList<IDataValidatorItem>;

    FMessage: string;
    FExecute: TDataValidatorInformationExecute;
    FIsNot: Boolean;
    FIsOptional: Boolean;
  protected
    procedure SetOptional(const AValue: Boolean);
    function Add(const AValidatorItem: IDataValidatorItem; const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US; const AModeSchema: Boolean = False): T;
  public
    // Schema
    function AddSchema(const ASchema: IDataValidatorSchemaContext): T;

    // Values
    function CustomValue(const AValidatorItem: IDataValidatorItem): T; overload;
    function CustomValue(const AExecute: TDataValidatorCustomExecute): T; overload;
    function CustomValue(const AExecute: TDataValidatorCustomMessageExecute): T; overload;
    function Contains(const AValueContains: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;
    function Contains(const AValueContains: string; const ACaseSensitive: Boolean = False): T; overload;
    function IsAlpha(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): T;
    function IsAlphaNumeric(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): T;
    function IsBase64(): T;
    function IsBetween(const AValueA: Integer; const AValueB: Integer): T; overload;
    function IsBetween(const AValueA: Int64; const AValueB: Int64): T; overload;
    function IsBetween(const AValueA: Double; const AValueB: Double): T; overload;
    function IsBetween(const AValueA: Extended; const AValueB: Extended): T; overload;
    function IsBetween(const AValueA: Single; const AValueB: Single): T; overload;
    function IsBetween(const AValueA: UInt64; const AValueB: UInt64): T; overload;
    function IsBTCAddress(): T;
    function IsCEP(): T;
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
    function IsEquals(const AValueEquals: TArray<string>; const ACaseSensitive: Boolean = False): T; overload;
    function IsEquals(const AValueEquals: string; const ACaseSensitive: Boolean = False): T; overload;
    function IsEthereumAddress(): T;
    function IsGreaterThan(const AValueGreaterThan: Integer): T;
    function IsHexadecimal(): T;
    function IsInteger(): T;
    function IsIP(): T;
    function IsJSON(): T;
    function IsLength(const AMin: Integer; const AMax: Integer): T;
    function IsLessThan(const AValueLessThan: Integer): T;
    function IsLowercase(): T;
    function IsMACAddress(): T;
    function IsMD5(): T;
    function IsNegative(): T;
    function IsNumeric(): T;
    function IsPhoneNumber(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): T;
    function IsPositive(): T;
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

    function &Not(): T;

    // Sanitizer
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

    // Message
    function WithMessage(const AMessage: string): T; overload;
    function WithMessage(const AMessage: string; const AParams: array of const): T; overload;
    function Execute(const AExecute: TDataValidatorInformationExecute): T;

    function GetItem(): TList<IDataValidatorItem>;
    function GetValue(): TValue;

    constructor Create(const AOwner: T; const AValue: string); overload;
    constructor Create(const AOwner: T; const AValue: TJSONPair); overload;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  Sanitizer.Custom,
  Sanitizer.Base64.Decode,
  Sanitizer.Base64.Encode,
  Sanitizer.ToDate,
  Sanitizer.ToDateTime,
  Sanitizer.HTML.Decode,
  Sanitizer.HTML.Encode,
  Sanitizer.NormalizeEmail,
  Sanitizer.OnlyNumbers,
  Sanitizer.RemoveAccents,
  Sanitizer.Replace,
  Sanitizer.ToInteger,
  Sanitizer.ToLowerCase,
  Sanitizer.ToMD5,
  Sanitizer.ToNumeric,
  Sanitizer.ToTime,
  Sanitizer.ToUpperCase,
  Sanitizer.Trim,
  Sanitizer.TrimLeft,
  Sanitizer.TrimRight,
  Sanitizer.URL.Decode,
  Sanitizer.URL.Encode,
  Validator.Contains,
  Validator.Custom,
  Validator.IsAlpha,
  Validator.IsAlphaNumeric,
  Validator.IsBase64,
  Validator.IsBetween,
  Validator.IsBTCAddress,
  Validator.IsCEP,
  Validator.IsCNPJ,
  Validator.IsCPF,
  Validator.IsCPFCNPJ,
  Validator.IsDate,
  Validator.IsDateBetween,
  Validator.IsDateEquals,
  Validator.IsDateGreaterThan,
  Validator.IsDateLessThan,
  Validator.IsDateTime,
  Validator.IsEmail,
  Validator.IsEmpty,
  Validator.IsEquals,
  Validator.IsEthereumAddress,
  Validator.IsPhoneNumber,
  Validator.IsGreaterThan,
  Validator.IsHexadecimal,
  Validator.IsInteger,
  Validator.IsIP,
  Validator.IsJSON,
  Validator.IsLength,
  Validator.IsLessThan,
  Validator.IsLowercase,
  Validator.IsMACAddress,
  Validator.IsMD5,
  Validator.IsNegative,
  Validator.IsNumeric,
  Validator.IsPositive,
  Validator.IsSSN,
  Validator.IsTime,
  Validator.IsTimeBetween,
  Validator.IsTimeEquals,
  Validator.IsTimeGreaterThan,
  Validator.IsTimeLessThan,
  Validator.IsUppercase,
  Validator.IsURL,
  Validator.IsUUID,
  Validator.IsZero;

{ TDataValidatorContext<T> }

constructor TDataValidatorContext<T>.Create(const AOwner: T; const AValue: string);
begin
  FOwner := AOwner;
  FValue := AValue;
end;

constructor TDataValidatorContext<T>.Create(const AOwner: T; const AValue: TJSONPair);
begin
  FOwner := AOwner;
  FValue := TValue.From<TJSONPair>(AValue);
end;

procedure TDataValidatorContext<T>.AfterConstruction;
begin
  inherited;
  FList := TList<IDataValidatorItem>.Create;
  FMessage := '';
  FExecute := nil;
  FIsNot := False;
  FIsOptional := False;
end;

destructor TDataValidatorContext<T>.Destroy;
begin
  FList.Clear;
  FList.DisposeOf;

  inherited;
end;

// Schema

function TDataValidatorContext<T>.AddSchema(const ASchema: IDataValidatorSchemaContext): T;
var
  LListValidatorItem: TList<IDataValidatorItem>;
  I: Integer;
begin
  Result := FOwner;

  if not Assigned(ASchema) then
    raise Exception.Create('Schema is nil!');

  LListValidatorItem := (ASchema as IDataValidatorContextBase<IDataValidatorItem>).GetItem;

  for I := 0 to Pred(LListValidatorItem.Count) do
    Add(LListValidatorItem.Items[I], LListValidatorItem.Items[I].GetDataValidatorLocaleLanguage, True);
end;

// Value

function TDataValidatorContext<T>.CustomValue(const AValidatorItem: IDataValidatorItem): T;
begin
  Result := Add(AValidatorItem);
end;

function TDataValidatorContext<T>.CustomValue(const AExecute: TDataValidatorCustomExecute): T;
begin
  Result := Add(TValidatorCustom.Create(AExecute, nil, 'Value false!'));
end;

function TDataValidatorContext<T>.CustomValue(const AExecute: TDataValidatorCustomMessageExecute): T;
begin
  Result := Add(TValidatorCustom.Create(nil, AExecute, 'Value false!'));
end;

function TDataValidatorContext<T>.Contains(const AValueContains: TArray<string>; const ACaseSensitive: Boolean): T;
var
  LValue: string;
  LMessage: string;
begin
  LMessage := '';
  for LValue in AValueContains do
    LMessage := LMessage + LValue + ' ';

  Result := Add(TValidatorContains.Create(AValueContains, ACaseSensitive, Format('Value not contains %s!', [LMessage])));
end;

function TDataValidatorContext<T>.Contains(const AValueContains: string; const ACaseSensitive: Boolean): T;
begin
  Result := Contains([AValueContains], ACaseSensitive);
end;

function TDataValidatorContext<T>.IsAlpha(const ALocaleLanguage: TDataValidatorLocaleLanguage): T;
begin
  Result := Add(TValidatorIsAlpha.Create('Value is not alpha!'), ALocaleLanguage);
end;

function TDataValidatorContext<T>.IsAlphaNumeric(const ALocaleLanguage: TDataValidatorLocaleLanguage): T;
begin
  Result := Add(TValidatorIsAlphaNumeric.Create('Value is not alphanumeric!'), ALocaleLanguage);
end;

function TDataValidatorContext<T>.IsBase64: T;
begin
  Result := Add(TValidatorIsBase64.Create('Value is not base64!'));
end;

function TDataValidatorContext<T>.IsBetween(const AValueA, AValueB: Single): T;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorContext<T>.IsBetween(const AValueA, AValueB: Extended): T;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorContext<T>.IsBetween(const AValueA, AValueB: Double): T;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorContext<T>.IsBetween(const AValueA, AValueB: Int64): T;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorContext<T>.IsBetween(const AValueA, AValueB: Integer): T;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorContext<T>.IsBetween(const AValueA, AValueB: UInt64): T;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorContext<T>.IsBTCAddress: T;
begin
  Result := Add(TValidatorIsBTCAddress.Create('Value is not BTC (Bitcoin) Adddress!'));
end;

function TDataValidatorContext<T>.IsCEP: T;
begin
  Result := Add(TValidatorIsCEP.Create('Value is not CEP (Código de Endereçamento Postal)!'));
end;

function TDataValidatorContext<T>.IsCNPJ: T;
begin
  Result := Add(TValidatorIsCNPJ.Create('Value is not CNPJ (Cadastro Nacional de Pessoas Jurídicas)!'));
end;

function TDataValidatorContext<T>.IsCPF: T;
begin
  Result := Add(TValidatorIsCPF.Create('Value is not CPF (Comprovante de Situação Cadastral)!'));
end;

function TDataValidatorContext<T>.IsCPFCNPJ: T;
begin
  Result := Add(TValidatorIsCPFCNPJ.Create('Value is not CPF (Comprovante de Situação Cadastral) or CNPJ (Cadastro Nacional de Pessoas Jurídicas)!'));
end;

function TDataValidatorContext<T>.IsDate(const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsDate.Create(AJSONISO8601ReturnUTC, 'Value is not date!'));
end;

function TDataValidatorContext<T>.IsDateBetween(const AValueA: TDate; const AValueB: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsDateBetween.Create(AValueA, AValueB, AJSONISO8601ReturnUTC, Format('Value is not between date %s and %s!', [DateToStr(AValueA), DateToStr(AValueB)])));
end;

function TDataValidatorContext<T>.IsDateEquals(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsDateEquals.Create(ACompareDate, AJSONISO8601ReturnUTC, Format('Value is not equal to date %s!', [DateToStr(ACompareDate)])));
end;

function TDataValidatorContext<T>.IsDateGreaterThan(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsDateGreaterThan.Create(ACompareDate, AJSONISO8601ReturnUTC, Format('Value is not greater than the date %s!', [DateToStr(ACompareDate)])));
end;

function TDataValidatorContext<T>.IsDateLessThan(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsDateLessThan.Create(ACompareDate, AJSONISO8601ReturnUTC, Format('Value is not less than the date %s!', [DateToStr(ACompareDate)])));
end;

function TDataValidatorContext<T>.IsDateTime(const AJSONISO8601ReturnUTC: Boolean): T;
begin
  Result := Add(TValidatorIsDateTime.Create(AJSONISO8601ReturnUTC, 'Value is not date time!'));
end;

function TDataValidatorContext<T>.IsEmail: T;
begin
  Result := Add(TValidatorIsEmail.Create('Value is not email!'));
end;

function TDataValidatorContext<T>.IsEmpty: T;
begin
  Result := Add(TValidatorIsEmpty.Create('Value is not empty!'));
end;

function TDataValidatorContext<T>.IsEquals(const AValueEquals: TArray<string>; const ACaseSensitive: Boolean): T;
var
  LValue: string;
  LMessage: string;
begin
  LMessage := '';
  for LValue in AValueEquals do
    LMessage := LMessage + LValue + ' ';

  Result := Add(TValidatorIsEquals.Create(AValueEquals, ACaseSensitive, Format('Value is not equals %s!', [LMessage])));
end;

function TDataValidatorContext<T>.IsEquals(const AValueEquals: string; const ACaseSensitive: Boolean): T;
begin
  Result := IsEquals([AValueEquals], ACaseSensitive);
end;

function TDataValidatorContext<T>.IsEthereumAddress: T;
begin
  Result := Add(TValidatorIsEthereumAddress.Create('Value is not Ethereum Adddress!'));
end;

function TDataValidatorContext<T>.IsGreaterThan(const AValueGreaterThan: Integer): T;
begin
  Result := Add(TValidatorIsGreaterThan.Create(AValueGreaterThan, Format('Value is not greate than %d!', [AValueGreaterThan])));
end;

function TDataValidatorContext<T>.IsHexadecimal: T;
begin
  Result := Add(TValidatorIsHexadecimal.Create('Value is not hexadecimal!'));
end;

function TDataValidatorContext<T>.IsInteger: T;
begin
  Result := Add(TValidatorIsInteger.Create('Value is not integer!'));
end;

function TDataValidatorContext<T>.IsIP: T;
begin
  Result := Add(TValidatorIsIP.Create('Value is not IP (Internet Protocol)!'));
end;

function TDataValidatorContext<T>.IsJSON: T;
begin
  Result := Add(TValidatorIsJSON.Create('Value is not JSON (JavaScript Object Notation)!'));
end;

function TDataValidatorContext<T>.IsLength(const AMin, AMax: Integer): T;
begin
  Result := Add(TValidatorIsLength.Create(AMin, AMax, Format('Value required length min(%d) and max(%d)!', [AMin, AMax])));
end;

function TDataValidatorContext<T>.IsLessThan(const AValueLessThan: Integer): T;
begin
  Result := Add(TValidatorIsLessThan.Create(AValueLessThan, Format('Value is not less than %d!', [AValueLessThan])));
end;

function TDataValidatorContext<T>.IsLowercase: T;
begin
  Result := Add(TValidatorIsLowercase.Create('Value is not lowercase!'));
end;

function TDataValidatorContext<T>.IsMACAddress: T;
begin
  Result := Add(TValidatorIsMACAddress.Create('Value is not MAC (Media Access Control) address!'));
end;

function TDataValidatorContext<T>.IsMD5: T;
begin
  Result := Add(TValidatorIsMD5.Create('Value is not MD5!'));
end;

function TDataValidatorContext<T>.IsNegative: T;
begin
  Result := Add(TValidatorIsNegative.Create('Value is not negative!'));
end;

function TDataValidatorContext<T>.IsNumeric: T;
begin
  Result := Add(TValidatorIsNumeric.Create('Value is not numeric!'));
end;

function TDataValidatorContext<T>.IsPhoneNumber(const ALocaleLanguage: TDataValidatorLocaleLanguage): T;
begin
  Result := Add(TValidatorIsPhoneNumber.Create('Value is not phone number!'), ALocaleLanguage);
end;

function TDataValidatorContext<T>.IsPositive: T;
begin
  Result := Add(TValidatorIsPositive.Create('Value is not positive!'));
end;

function TDataValidatorContext<T>.IsSSN: T;
begin
  Result := Add(TValidatorIsSSN.Create('Value is not SSN (Social Security Number)!'));
end;

function TDataValidatorContext<T>.IsTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsTime.Create(AJSONISO8601ReturnUTC, 'Value is not time!'));
end;

function TDataValidatorContext<T>.IsTimeBetween(const AValueA: TTime; const AValueB: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsTimeBetween.Create(AValueA, AValueB, AJSONISO8601ReturnUTC, Format('Value is not between time %s and %s!', [TimeToStr(AValueA), TimeToStr(AValueB)])));
end;

function TDataValidatorContext<T>.IsTimeEquals(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsTimeEquals.Create(ACompareTime, AJSONISO8601ReturnUTC, Format('Value is not equal to time %s!', [TimeToStr(ACompareTime)])));
end;

function TDataValidatorContext<T>.IsTimeGreaterThan(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsTimeGreaterThan.Create(ACompareTime, AJSONISO8601ReturnUTC, Format('Value is not greater than the time %s!', [TimeToStr(ACompareTime)])));
end;

function TDataValidatorContext<T>.IsTimeLessThan(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TValidatorIsTimeLessThan.Create(ACompareTime, AJSONISO8601ReturnUTC, Format('Value is not less than the time %s!', [TimeToStr(ACompareTime)])));
end;

function TDataValidatorContext<T>.IsUppercase: T;
begin
  Result := Add(TValidatorIsUppercase.Create('Value is not uppercase!'));
end;

function TDataValidatorContext<T>.IsURL: T;
begin
  Result := Add(TValidatorIsURL.Create('Value is not URL (Uniform Resource Locator)!'));
end;

function TDataValidatorContext<T>.IsUUID: T;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuAll, 'Value is not UUID (Universally Unique Identifier)!'));
end;

function TDataValidatorContext<T>.IsUUIDv1: T;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV1, 'Value is not UUID (Universally Unique Identifier) v1!'));
end;

function TDataValidatorContext<T>.IsUUIDv2: T;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV2, 'Value is not UUID (Universally Unique Identifier) v2!'));
end;

function TDataValidatorContext<T>.IsUUIDv3: T;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV3, 'Value is not UUID (Universally Unique Identifier) v3!'));
end;

function TDataValidatorContext<T>.IsUUIDv4: T;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV4, 'Value is not UUID (Universally Unique Identifier) v4!'));
end;

function TDataValidatorContext<T>.IsUUIDv5: T;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV5, 'Value is not UUID (Universally Unique Identifier) v5!'));
end;

function TDataValidatorContext<T>.IsZero: T;
begin
  Result := Add(TValidatorIsZero.Create('Value is not zero!'));
end;

function TDataValidatorContext<T>.&Not: T;
begin
  Result := FOwner;
  FIsNot := True;
end;

// Sanitizer

function TDataValidatorContext<T>.CustomSanitizer(const ASanitizerItem: IDataSanitizerItem): T;
begin
  Result := Add(ASanitizerItem);
end;

function TDataValidatorContext<T>.CustomSanitizer(const AExecute: TDataValidatorCustomSanitizerExecute): T;
begin
  Result := Add(TSanitizerCustom.Create(AExecute) as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.NormalizeEmail(const AAllLowercase, AGmailRemoveDots: Boolean): T;
begin
  Result := Add(TSanitizerNormalizeEmail.Create(AAllLowercase, AGmailRemoveDots) as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.OnlyNumbers: T;
begin
  Result := Add(TSanitizerOnlyNumbers.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.RemoveAccents: T;
begin
  Result := Add(TSanitizerRemoveAccents.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.Replace(const AOldValue, ANewValue: string): T;
begin
  Result := Add(TSanitizerReplace.Create(AOldValue, ANewValue) as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToBase64Decode: T;
begin
  Result := Add(TSanitizerBase64Decode.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToBase64Encode: T;
begin
  Result := Add(TSanitizerBase64Encode.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToDate(const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TSanitizerToDate.Create(AJSONISO8601ReturnUTC) as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToDateTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TSanitizerToDateTime.Create(AJSONISO8601ReturnUTC) as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToHTMLDecode: T;
begin
  Result := Add(TSanitizerHTMLDecode.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToHTMLEncode: T;
begin
  Result := Add(TSanitizerHTMLEncode.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToInteger: T;
begin
  Result := Add(TSanitizerToInteger.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToLowerCase: T;
begin
  Result := Add(TSanitizerToLowerCase.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToMD5: T;
begin
  Result := Add(TSanitizerToMD5.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToNumeric: T;
begin
  Result := Add(TSanitizerToNumeric.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToTime(const AJSONISO8601ReturnUTC: Boolean = True): T;
begin
  Result := Add(TSanitizerToTime.Create(AJSONISO8601ReturnUTC) as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToUpperCase: T;
begin
  Result := Add(TSanitizerToUpperCase.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToURLDecode: T;
begin
  Result := Add(TSanitizerURLDecode.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.ToURLEncode: T;
begin
  Result := Add(TSanitizerURLEncode.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.Trim: T;
begin
  Result := Add(TSanitizerTrim.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.TrimLeft: T;
begin
  Result := Add(TSanitizerTrimLeft.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.TrimRight: T;
begin
  Result := Add(TSanitizerTrimRight.Create as IDataSanitizerItem);
end;

function TDataValidatorContext<T>.WithMessage(const AMessage: string): T;
begin
  Result := FOwner;

  if FList.Count > 0 then
    FList.Last.SetMessage(AMessage)
  else
    FMessage := AMessage;
end;

function TDataValidatorContext<T>.WithMessage(const AMessage: string; const AParams: array of const): T;
begin
  Result := WithMessage(Format(AMessage, AParams));
end;

function TDataValidatorContext<T>.Execute(const AExecute: TDataValidatorInformationExecute): T;
begin
  Result := FOwner;

  if FList.Count > 0 then
    FList.Last.SetExecute(AExecute)
  else
    FExecute := AExecute;
end;

function TDataValidatorContext<T>.GetItem: TList<IDataValidatorItem>;
begin
  Result := FList;
end;

function TDataValidatorContext<T>.GetValue: TValue;
begin
  Result := FValue;
end;

function TDataValidatorContext<T>.Add(const AValidatorItem: IDataValidatorItem; const ALocaleLanguage: TDataValidatorLocaleLanguage; const AModeSchema: Boolean): T;
begin
  Result := FOwner;

  if FIsOptional then
  begin
    FList.Add(AValidatorItem);
    FList.Remove(FList.Last);
    Exit;
  end;

  AValidatorItem.SetDataValidatorLocaleLanguage(ALocaleLanguage);
  AValidatorItem.SetExecute(FExecute);
  AValidatorItem.SetMessage(FMessage);

  if not AModeSchema then
  begin
    AValidatorItem.SetIsNot(FIsNot);
    FIsNot := False;
  end;

  FList.Add(AValidatorItem);
end;

procedure TDataValidatorContext<T>.SetOptional(const AValue: Boolean);
begin
  FIsOptional := AValue;
end;

end.
