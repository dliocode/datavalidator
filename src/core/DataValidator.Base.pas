unit DataValidator.Base;

interface

uses
  DataValidator.ItemBase.Intf, DataValidator.Base.Intf, DataValidator.Types, DataValidator.ItemBase.Sanitizer,
  System.Generics.Collections, System.SysUtils, System.Variants, System.JSON, System.RTTI;

type
  TDataValidatorsBase<T> = class(TInterfacedObject, IDataValidatorsBase<T>, IDataValidatorsBaseItem<T>, IDataValidatorsBaseJSON<T>)
  strict private
    FOwner: T;
    FValue: TValue;
    FListItem: TList<IDataValidatorItem>;
    FMessage: string;
    FExecute: TDataValidatorInformationExecute;
    FIsNot: Boolean;
    FIsOptionalKey: Boolean;

    function Add(const AValidatorItem: IDataValidatorItem; const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US; const AModeSchema: Boolean = False): IDataValidatorsBase<T>;
  public
    // Schema
    function AddSchema(const ASchema: IDataValidatorSchema): IDataValidatorsBase<T>;

    // Validator to JSON
    function IsOptionalKey: IDataValidatorsBase<T>;
    function IsRequiredKey: IDataValidatorsBase<T>;

    // Validator
    function Contains(const AValueContains: string; const ACaseSensitive: Boolean = False): IDataValidatorsBase<T>;
    function IsAlpha(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): IDataValidatorsBase<T>;
    function IsAlphaNumeric(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): IDataValidatorsBase<T>;
    function IsBase64: IDataValidatorsBase<T>;
    function IsBetween(const AValueA: Integer; const AValueB: Integer): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: Int64; const AValueB: Int64): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: UInt64; const AValueB: UInt64): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: Single; const AValueB: Single): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: Double; const AValueB: Double): IDataValidatorsBase<T>; overload;
    function IsBetween(const AValueA: Extended; const AValueB: Extended): IDataValidatorsBase<T>; overload;
    function IsBTCAddress: IDataValidatorsBase<T>;
    function IsCEP: IDataValidatorsBase<T>;
    function IsCNPJ: IDataValidatorsBase<T>;
    function IsCPF: IDataValidatorsBase<T>;
    function IsCPFCNPJ: IDataValidatorsBase<T>;
    function IsDate: IDataValidatorsBase<T>;
    function IsDateBetween(const AValueA: TDate; const AValueB: TDate): IDataValidatorsBase<T>;
    function IsDateEquals(const ACompareDate: TDate): IDataValidatorsBase<T>;
    function IsDateGreaterThan(const ACompareDate: TDate): IDataValidatorsBase<T>;
    function IsDateLessThan(const ACompareDate: TDate): IDataValidatorsBase<T>;
    function IsEmail: IDataValidatorsBase<T>;
    function IsEmpty: IDataValidatorsBase<T>;
    function IsEquals(const AValueEquals: string; const ACaseSensitive: Boolean = False): IDataValidatorsBase<T>;
    function IsEthereumAddress: IDataValidatorsBase<T>;
    function IsNumeric: IDataValidatorsBase<T>;
    function IsFone: IDataValidatorsBase<T>;
    function IsGreaterThan(const AValueGreaterThan: Integer): IDataValidatorsBase<T>;
    function IsHexadecimal: IDataValidatorsBase<T>;
    function IsIP: IDataValidatorsBase<T>;
    function IsJSON: IDataValidatorsBase<T>;
    function IsLength(const AMin: Integer; const AMax: Integer): IDataValidatorsBase<T>;
    function IsLessThan(const AValueLessThan: Integer): IDataValidatorsBase<T>;
    function IsLowercase: IDataValidatorsBase<T>;
    function IsMACAddress: IDataValidatorsBase<T>;
    function IsMD5: IDataValidatorsBase<T>;
    function IsNegative: IDataValidatorsBase<T>;
    function IsPositive: IDataValidatorsBase<T>;
    function IsInteger: IDataValidatorsBase<T>;
    function IsTime: IDataValidatorsBase<T>;
    function IsTimeBetween(const AValueA: TTime; const AValueB: TTime): IDataValidatorsBase<T>;
    function IsTimeEquals(const ACompareTime: TTime): IDataValidatorsBase<T>;
    function IsTimeGreaterThan(const ACompareTime: TTime): IDataValidatorsBase<T>;
    function IsTimeLessThan(const ACompareTime: TTime): IDataValidatorsBase<T>;
    function IsUppercase: IDataValidatorsBase<T>;
    function IsURL: IDataValidatorsBase<T>;
    function IsUUID: IDataValidatorsBase<T>;
    function IsUUIDv1: IDataValidatorsBase<T>;
    function IsUUIDv2: IDataValidatorsBase<T>;
    function IsUUIDv3: IDataValidatorsBase<T>;
    function IsUUIDv4: IDataValidatorsBase<T>;
    function IsUUIDv5: IDataValidatorsBase<T>;
    function IsZero: IDataValidatorsBase<T>;

    // Especial
    function &Not: IDataValidatorsBase<T>;
    function Custom(const AValidatorItem: IDataValidatorItem): IDataValidatorsBase<T>; overload;
    function Custom(const AExecute: TDataValidatorCustomExecute): IDataValidatorsBase<T>; overload;

    // Sanitizer
    function NormalizeEmail(const AAllLowercase: Boolean = True; const AGmailRemoveDots: Boolean = True): IDataValidatorsBase<T>;
    function RemoveAccents: IDataValidatorsBase<T>;
    function Replace(const AOldValue: string; const ANewValue: string): IDataValidatorsBase<T>;
    function ToBase64Decode: IDataValidatorsBase<T>;
    function ToBase64Encode: IDataValidatorsBase<T>;
    function ToHTMLDecode: IDataValidatorsBase<T>;
    function ToHTMLEncode: IDataValidatorsBase<T>;
    function ToInteger: IDataValidatorsBase<T>;
    function ToLowerCase: IDataValidatorsBase<T>;
    function ToMD5: IDataValidatorsBase<T>;
    function ToNumeric: IDataValidatorsBase<T>;
    function ToUpperCase: IDataValidatorsBase<T>;
    function ToURLDecode: IDataValidatorsBase<T>;
    function ToURLEncode: IDataValidatorsBase<T>;
    function Trim: IDataValidatorsBase<T>;
    function TrimLeft: IDataValidatorsBase<T>;
    function TrimRight: IDataValidatorsBase<T>;

    // Message
    function WithMessage(const AMessage: string): IDataValidatorsBase<T>; overload;
    function WithMessage(const AMessage: string; const AParams: array of const): IDataValidatorsBase<T>; overload;
    function Execute(const AExecute: TDataValidatorInformationExecute): IDataValidatorsBase<T>;
    function &End: T;

    // Validators Base
    function GetItem: TList<IDataValidatorItem>;
    function GetValue: TValue;

    constructor Create(const AOwner: T; const AValue: string); overload;
    constructor Create(const AOwner: T; const AValue: TJSONPair); overload;
    procedure AfterConstruction; override;
    destructor Destroy; override;

    class function New(const AOwner: T; const AValue: string): IDataValidatorsBase<T>; overload;
    class function New(const AOwner: T; const AValue: TJSONPair): IDataValidatorsBase<T>; overload;
  end;

implementation

uses
  Sanitizer.Base64.Decode,
  Sanitizer.Base64.Encode,
  Sanitizer.HTML.Decode,
  Sanitizer.HTML.Encode,
  Sanitizer.MD5,
  Sanitizer.NormalizeEmail,
  Sanitizer.RemoveAccents,
  Sanitizer.Replace,
  Sanitizer.ToInteger,
  Sanitizer.ToLowerCase,
  Sanitizer.ToNumeric,
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
  Validator.IsEmail,
  Validator.IsEmpty,
  Validator.IsEquals,
  Validator.IsEthereumAddress,
  Validator.IsFone,
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
  Validator.IsTime,
  Validator.IsTimeBetween,
  Validator.IsTimeEquals,
  Validator.IsTimeGreaterThan,
  Validator.IsTimeLessThan,
  Validator.IsUppercase,
  Validator.IsURL,
  Validator.IsUUID,
  Validator.IsZero,
  Validator.JSON.IsRequiredKey;

{ TDataValidatorsBase<T> }

class function TDataValidatorsBase<T>.New(const AOwner: T; const AValue: string): IDataValidatorsBase<T>;
begin
  Result := TDataValidatorsBase<T>.Create(AOwner, AValue);
end;

class function TDataValidatorsBase<T>.New(const AOwner: T; const AValue: TJSONPair): IDataValidatorsBase<T>;
begin
  Result := TDataValidatorsBase<T>.Create(AOwner, AValue);
end;

constructor TDataValidatorsBase<T>.Create(const AOwner: T; const AValue: string);
begin
  FOwner := AOwner;
  FValue := AValue;
end;

constructor TDataValidatorsBase<T>.Create(const AOwner: T; const AValue: TJSONPair);
begin
  FOwner := AOwner;
  FValue := TValue.From<TJSONPair>(AValue);
end;

procedure TDataValidatorsBase<T>.AfterConstruction;
begin
  inherited;
  FListItem := TList<IDataValidatorItem>.Create;
  FMessage := '';
  FExecute := nil;
  FIsNot := False;
  FIsOptionalKey := False;
end;

destructor TDataValidatorsBase<T>.Destroy;
begin
  FListItem.Clear;
  FListItem.DisposeOf;
  inherited;
end;

// Schema

function TDataValidatorsBase<T>.AddSchema(const ASchema: IDataValidatorSchema): IDataValidatorsBase<T>;
var
  LList: TList<IDataValidatorItem>;
  LArrayItem: TArray<IDataValidatorItem>;
  LItem: IDataValidatorItem;
  I: Integer;
begin
  Result := Self;

  if not Assigned(ASchema) then
    raise Exception.Create('Schema is nil!');

  LList := (ASchema.ListSchema.Items[0] as IDataValidatorsBaseItem<IDataValidatorSchema>).GetItem;
  LArrayItem := LList.ToArray;

  LList.Clear;
  ASchema.ListSchema.Clear;

  for I := 0 to Pred(Length(LArrayItem)) do
    Add(LArrayItem[I], LArrayItem[I].GeTDataValidatorLocaleLanguage, True);
end;

function TDataValidatorsBase<T>.IsOptionalKey: IDataValidatorsBase<T>;
var
  LValidatorRequiredKey: IDataValidatorItem;
begin
  Result := Self;

  LValidatorRequiredKey := TDataValidatorJSONIsRequiredKey.Create('Value is required!');
  LValidatorRequiredKey.SetValue(FValue);
  FIsOptionalKey := not LValidatorRequiredKey.Checked.OK;
end;

function TDataValidatorsBase<T>.IsRequiredKey: IDataValidatorsBase<T>;
begin
  Result := Add(TDataValidatorJSONIsRequiredKey.Create('Value is required!'));
end;

// Validator

function TDataValidatorsBase<T>.Contains(const AValueContains: string; const ACaseSensitive: Boolean = False): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorContains.Create(AValueContains, ACaseSensitive, Format('Value not contains %s!', [AValueContains])));
end;

function TDataValidatorsBase<T>.IsAlpha(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsAlpha.Create('Value is not alpha!'), ALocaleLanguage);
end;

function TDataValidatorsBase<T>.IsAlphaNumeric(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsAlphaNumeric.Create('Value is not alphanumeric!'), ALocaleLanguage);
end;

function TDataValidatorsBase<T>.IsBase64: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsBase64.Create('Value is not base64!'));
end;

function TDataValidatorsBase<T>.IsBetween(const AValueA: Integer; const AValueB: Integer): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorsBase<T>.IsBetween(const AValueA: Int64; const AValueB: Int64): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorsBase<T>.IsBetween(const AValueA, AValueB: UInt64): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorsBase<T>.IsBetween(const AValueA: Single; const AValueB: Single): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorsBase<T>.IsBetween(const AValueA: Double; const AValueB: Double): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorsBase<T>.IsBetween(const AValueA: Extended; const AValueB: Extended): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsBetween.Create(AValueA, AValueB, Format('Value is not between %s and %s!', [VarToStr(AValueA), VarToStr(AValueB)])));
end;

function TDataValidatorsBase<T>.IsBTCAddress: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsBTCAddress.Create('Value is not BTC Adddress!'));
end;

function TDataValidatorsBase<T>.IsCEP: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsCEP.Create('Value is not CEP!'));
end;

function TDataValidatorsBase<T>.IsCNPJ: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsCNPJ.Create('Value is not CNPJ!'));
end;

function TDataValidatorsBase<T>.IsCPF: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsCPF.Create('Value is not CPF!'));
end;

function TDataValidatorsBase<T>.IsCPFCNPJ: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsCPFCNPJ.Create('Value is not CPF or CNPJ!'));
end;

function TDataValidatorsBase<T>.IsDate: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsDate.Create('Value is not date!'));
end;

function TDataValidatorsBase<T>.IsDateBetween(const AValueA: TDate; const AValueB: TDate): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsDateBetween.Create(AValueA, AValueB, Format('Value is not between date %s and %s!', [DateToStr(AValueA), DateToStr(AValueB)])));
end;

function TDataValidatorsBase<T>.IsDateEquals(const ACompareDate: TDate): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsDateEquals.Create(ACompareDate, Format('Value is not equal to date %s!', [DateToStr(ACompareDate)])));
end;

function TDataValidatorsBase<T>.IsDateGreaterThan(const ACompareDate: TDate): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsDateGreaterThan.Create(ACompareDate, Format('Value is not greater than the date %s!', [DateToStr(ACompareDate)])));
end;

function TDataValidatorsBase<T>.IsDateLessThan(const ACompareDate: TDate): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsDateLessThan.Create(ACompareDate, Format('Value is not less than the date %s!', [DateToStr(ACompareDate)])));
end;

function TDataValidatorsBase<T>.IsEmail: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsEmail.Create('Value is not email!'));
end;

function TDataValidatorsBase<T>.IsEmpty: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsEmpty.Create('Value is not empty!'));
end;

function TDataValidatorsBase<T>.IsEquals(const AValueEquals: string; const ACaseSensitive: Boolean = False): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsEquals.Create(AValueEquals, ACaseSensitive, Format('Value is not equals %s!', [AValueEquals])));
end;

function TDataValidatorsBase<T>.IsEthereumAddress: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsEthereumAddress.Create('Value is not Ethereum Adddress!'));
end;

function TDataValidatorsBase<T>.IsNumeric: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsNumeric.Create('Value is not float!'));
end;

function TDataValidatorsBase<T>.IsFone: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsFone.Create('Value is not fone!'));
end;

function TDataValidatorsBase<T>.IsGreaterThan(const AValueGreaterThan: Integer): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsGreaterThan.Create(AValueGreaterThan, Format('Value is not greate than %d!', [AValueGreaterThan])));
end;

function TDataValidatorsBase<T>.IsHexadecimal: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsHexadecimal.Create('Value is not hexadecimal!'));
end;

function TDataValidatorsBase<T>.IsIP: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsIP.Create('Value is not IP!'));
end;

function TDataValidatorsBase<T>.IsJSON: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsJSON.Create('Value is not JSON!'));
end;

function TDataValidatorsBase<T>.IsLength(const AMin: Integer; const AMax: Integer): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsLength.Create(AMin, AMax, Format('Value required length min(%d) and max(%d)!', [AMin, AMax])));
end;

function TDataValidatorsBase<T>.IsLessThan(const AValueLessThan: Integer): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsLessThan.Create(AValueLessThan, Format('Value is not less than %d!', [AValueLessThan])));
end;

function TDataValidatorsBase<T>.IsLowercase: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsLowercase.Create('Value is not lowercase!'));
end;

function TDataValidatorsBase<T>.IsMACAddress: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsMACAddress.Create('Value is not mac address!'));
end;

function TDataValidatorsBase<T>.IsMD5: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsMD5.Create('Value is not MD5!'));
end;

function TDataValidatorsBase<T>.IsNegative: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsNegative.Create('Value is not negative!'));
end;

function TDataValidatorsBase<T>.IsPositive: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsPositive.Create('Value is not positive!'));
end;

function TDataValidatorsBase<T>.IsInteger: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsInteger.Create('Value is not numeric!'));
end;

function TDataValidatorsBase<T>.IsTime: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsTime.Create('Value is not time!'));
end;

function TDataValidatorsBase<T>.IsTimeBetween(const AValueA: TTime; const AValueB: TTime): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsTimeBetween.Create(AValueA, AValueB, Format('Value is not between time %s and %s!', [TimeToStr(AValueA), TimeToStr(AValueB)])));
end;

function TDataValidatorsBase<T>.IsTimeEquals(const ACompareTime: TTime): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsTimeEquals.Create(ACompareTime, Format('Value is not equal to time %s!', [TimeToStr(ACompareTime)])));
end;

function TDataValidatorsBase<T>.IsTimeGreaterThan(const ACompareTime: TTime): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsTimeGreaterThan.Create(ACompareTime, Format('Value is not greater than the time %s!', [TimeToStr(ACompareTime)])));
end;

function TDataValidatorsBase<T>.IsTimeLessThan(const ACompareTime: TTime): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsTimeLessThan.Create(ACompareTime, Format('Value is not less than the time %s!', [TimeToStr(ACompareTime)])));
end;

function TDataValidatorsBase<T>.IsUppercase: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsUppercase.Create('Value is not uppercase!'));
end;

function TDataValidatorsBase<T>.IsURL: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsURL.Create('Value is not url!'));
end;

function TDataValidatorsBase<T>.IsUUID: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuAll, 'Value is not uuid!'));
end;

function TDataValidatorsBase<T>.IsUUIDv1: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV1, 'Value is not uuid v1!'));
end;

function TDataValidatorsBase<T>.IsUUIDv2: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV2, 'Value is not uuid v2!'));
end;

function TDataValidatorsBase<T>.IsUUIDv3: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV3, 'Value is not uuid v3!'));
end;

function TDataValidatorsBase<T>.IsUUIDv4: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV4, 'Value is not uuid v4!'));
end;

function TDataValidatorsBase<T>.IsUUIDv5: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsUUID.Create(TUUIDVersion.tuV5, 'Value is not uuid v5!'));
end;

function TDataValidatorsBase<T>.IsZero: IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorIsZero.Create('Value is not zero!'));
end;

function TDataValidatorsBase<T>.&Not: IDataValidatorsBase<T>;
begin
  Result := Self;
  FIsNot := True;
end;

function TDataValidatorsBase<T>.Custom(const AValidatorItem: IDataValidatorItem): IDataValidatorsBase<T>;
begin
  Result := Add(AValidatorItem);
end;

function TDataValidatorsBase<T>.Custom(const AExecute: TDataValidatorCustomExecute): IDataValidatorsBase<T>;
begin
  Result := Add(TValidatorCustom.Create(AExecute, 'Value false!'));
end;

// Sanitizer

function TDataValidatorsBase<T>.NormalizeEmail(const AAllLowercase: Boolean = True; const AGmailRemoveDots: Boolean = True): IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerNormalizeEmail.Create(AAllLowercase, AGmailRemoveDots) as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.RemoveAccents: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerRemoveAccents.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.Replace(const AOldValue, ANewValue: string): IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerReplace.Create(AOldValue, ANewValue) as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToBase64Decode: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerBase64Decode.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToBase64Encode: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerBase64Encode.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToHTMLDecode: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerHTMLDecode.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToHTMLEncode: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerHTMLEncode.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToInteger: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerToInteger.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToLowerCase: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerToLowerCase.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToMD5: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerMD5.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToNumeric: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerToNumeric.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToUpperCase: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerToUpperCase.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToURLDecode: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerURLDecode.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.ToURLEncode: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerURLEncode.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.Trim: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerTrim.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.TrimLeft: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerTrimLeft.Create as IDataSanitizerItem);
end;

function TDataValidatorsBase<T>.TrimRight: IDataValidatorsBase<T>;
begin
  Result := Add(TSanitizerTrimRight.Create as IDataSanitizerItem);
end;

// Message

function TDataValidatorsBase<T>.WithMessage(const AMessage: string): IDataValidatorsBase<T>;
begin
  Result := Self;

  if FListItem.Count > 0 then
    FListItem.Last.SetMessage(AMessage)
  else
    FMessage := AMessage;
end;

function TDataValidatorsBase<T>.WithMessage(const AMessage: string; const AParams: array of const): IDataValidatorsBase<T>;
begin
  Result := WithMessage(Format(AMessage, AParams));
end;

function TDataValidatorsBase<T>.Execute(const AExecute: TDataValidatorInformationExecute): IDataValidatorsBase<T>;
begin
  Result := Self;

  if FListItem.Count > 0 then
    FListItem.Last.SetExecute(AExecute)
  else
    FExecute := AExecute;
end;

function TDataValidatorsBase<T>.&End: T;
begin
  Result := FOwner;
end;

// Validators Base

function TDataValidatorsBase<T>.GetItem: TList<IDataValidatorItem>;
begin
  Result := FListItem;
end;

function TDataValidatorsBase<T>.GetValue: TValue;
begin
  Result := FValue;
end;

// Add

function TDataValidatorsBase<T>.Add(const AValidatorItem: IDataValidatorItem; const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US; const AModeSchema: Boolean = False): IDataValidatorsBase<T>;
begin
  Result := Self;

  // For JSON Optional
  if FIsOptionalKey then
  begin
    FListItem.Add(AValidatorItem);
    FListItem.Remove(FListItem.Last);
    Exit;
  end;

  AValidatorItem.SeTDataValidatorLocaleLanguage(ALocaleLanguage);
  AValidatorItem.SetExecute(FExecute);
  AValidatorItem.SetMessage(FMessage);

  if not AModeSchema then
  begin
    AValidatorItem.SetIsNot(FIsNot);
    FIsNot := False;
  end;

  FListItem.Add(AValidatorItem);
end;

end.
