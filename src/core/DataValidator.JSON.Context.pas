{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.JSON.Context;

interface

uses
  DataValidator.JSON.Context.Intf, DataValidator.ItemBase.Intf,
  DataValidator.Types, DataValidator.Context,
  System.JSON, System.Generics.Collections, System.SysUtils;

type
  TDataValidatorJSONContext<T: IInterface> = class(TDataValidatorContext<IDataValidatorJSONContextValue<T>>, IDataValidatorJSONContext<T>, IDataValidatorJSONContextKey<T>, IDataValidatorJSONContextValue<T>)
  strict private
  [weak]
    FOwner: T;
    FValue: TJSONPair;
  public
    function Key(): IDataValidatorJSONContextKey<T>;
    function Value(): IDataValidatorJSONContextValue<T>;

    // Key
    function IsOptional(): IDataValidatorJSONContextKey<T>;
    function IsRequired(): IDataValidatorJSONContextKey<T>;

    function WithMessage(const AMessage: string): IDataValidatorJSONContextKey<T>; overload;
    function WithMessage(const AMessage: string; const AParams: array of const): IDataValidatorJSONContextKey<T>; overload;
    function Execute(const AExecute: TDataValidatorInformationExecute): IDataValidatorJSONContextKey<T>;

    // Value
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueExecute): IDataValidatorJSONContextValue<T>; overload;
    function CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueMessageExecute): IDataValidatorJSONContextValue<T>; overload;
    function IsArray(): IDataValidatorJSONContextValue<T>;
    function IsObject(): IDataValidatorJSONContextValue<T>;
    function MinItems(const AMinItems: Integer): IDataValidatorJSONContextValue<T>;
    function MaxItems(const AMaxItems: Integer): IDataValidatorJSONContextValue<T>;

    function &End(): T;

    constructor Create(const AOwner: T; const AValue: TJSONPair);
    destructor Destroy; override;
  end;

implementation

uses
  Validator.JSON.Key.IsRequired,
  Validator.JSON.Value.Custom,
  Validator.JSON.Value.IsArray,
  Validator.JSON.Value.IsObject,
  Validator.JSON.Value.MinItems,
  Validator.JSON.Value.MaxItems;

{ TDataValidatorJSONContext<T> }

constructor TDataValidatorJSONContext<T>.Create(const AOwner: T; const AValue: TJSONPair);
begin
  FOwner := AOwner;
  FValue := AValue;

  inherited Create(Self, FValue);
end;

destructor TDataValidatorJSONContext<T>.Destroy;
begin
  inherited;
end;

function TDataValidatorJSONContext<T>.Key: IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
end;

function TDataValidatorJSONContext<T>.Value: IDataValidatorJSONContextValue<T>;
begin
  Result := Self
end;

function TDataValidatorJSONContext<T>.IsOptional: IDataValidatorJSONContextKey<T>;
var
  LValidatorRequiredKey: IDataValidatorItem;
  LIsOptional: Boolean;
begin
  Result := Self;

  LValidatorRequiredKey := TDataValidatorJSONKeyIsRequired.Create('Key is required!');
  LValidatorRequiredKey.SetValue(FValue);
  LIsOptional := not LValidatorRequiredKey.Checked.OK;

  SetOptional(LIsOptional);
end;

function TDataValidatorJSONContext<T>.IsRequired: IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONKeyIsRequired.Create('Key is required!'));
end;

function TDataValidatorJSONContext<T>.WithMessage(const AMessage: string): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  TDataValidatorContext < IDataValidatorJSONContextValue < T >> (Self).WithMessage(AMessage);
end;

function TDataValidatorJSONContext<T>.WithMessage(const AMessage: string; const AParams: array of const): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  TDataValidatorContext < IDataValidatorJSONContextValue < T >> (Self).WithMessage(AMessage, AParams);
end;

function TDataValidatorJSONContext<T>.Execute(const AExecute: TDataValidatorInformationExecute): IDataValidatorJSONContextKey<T>;
begin
  Result := Self;
  TDataValidatorContext < IDataValidatorJSONContextValue < T >> (Self).Execute(AExecute);
end;

function TDataValidatorJSONContext<T>.CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueExecute): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONValueCustom.Create(AExecute, nil, 'Value is false!'));
end;

function TDataValidatorJSONContext<T>.CustomJSONValue(const AExecute: TDataValidatorCustomJSONValueMessageExecute): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TValidatorJSONValueCustom.Create(nil, AExecute, 'Value is false!'));
end;

function TDataValidatorJSONContext<T>.IsArray: IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueIsArray.Create('Value not is JSONArray!'));
end;

function TDataValidatorJSONContext<T>.IsObject: IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueIsObject.Create('Value not is JSONObject!'));
end;

function TDataValidatorJSONContext<T>.MinItems(const AMinItems: Integer): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueMinItems.Create(AMinItems, Format('Its size is less than %d!', [AMinItems])));
end;

function TDataValidatorJSONContext<T>.MaxItems(const AMaxItems: Integer): IDataValidatorJSONContextValue<T>;
begin
  Result := Self;
  Add(TDataValidatorJSONValueMaxItems.Create(AMaxItems, Format('Its size is greater than %d!', [AMaxItems])));
end;

function TDataValidatorJSONContext<T>.&End: T;
begin
  Result := FOwner;
end;

end.
