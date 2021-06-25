{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.ItemBase;

interface

uses
  DataValidator.Types, DataValidator.ItemBase.Intf, DataValidator.Result.Intf, DataValidator.Result, DataValidator.Information,
  System.SysUtils, System.RTTI, System.JSON;

type
  TJSONPair = System.JSON.TJSONPair;
  TValue = System.RTTI.TValue;

  TDataValidatorLocaleLanguage = DataValidator.Types.TDataValidatorLocaleLanguage;
  TDataValidatorCustomExecute = DataValidator.Types.TDataValidatorCustomExecute;
  TDataValidatorInformationExecute = DataValidator.Types.TDataValidatorInformationExecute;
  IDataValidatorItem = DataValidator.ItemBase.Intf.IDataValidatorItem;
  IDataValidatorResult = DataValidator.Result.Intf.IDataValidatorResult;
  TDataValidatorResult = DataValidator.Result.TDataValidatorResult;
  TDataValidatorInformation = DataValidator.Information.TDataValidatorInformation;

  TDataValidatorItemBase = class(TInterfacedObject, IDataValidatorItemBase)
  protected
    FLocaleLanguage: TDataValidatorLocaleLanguage;
    FIsNot: Boolean;
    FValue: TValue;
    FMessage: string;
    FExecute: TDataValidatorInformationExecute;
    function GetValueAsString: string;
    procedure SetValueAdapter(const AValue: TValue);
  public
    function GeTDataValidatorLocaleLanguage: TDataValidatorLocaleLanguage;
    procedure SeTDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US);

    procedure SetIsNot(const AIsNot: Boolean);
    procedure SetValue(const AValue: TValue);
    procedure SetMessage(const AMessage: string);
    procedure SetExecute(const AExecute: TDataValidatorInformationExecute);
  end;

implementation

{ TDataValidatorItemBase }

function TDataValidatorItemBase.GeTDataValidatorLocaleLanguage: TDataValidatorLocaleLanguage;
begin
  Result := FLocaleLanguage;
end;

procedure TDataValidatorItemBase.SeTDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US);
begin
  FLocaleLanguage := ALocaleLanguage;
end;

procedure TDataValidatorItemBase.SetValue(const AValue: TValue);
begin
  FValue := AValue;
end;

procedure TDataValidatorItemBase.SetIsNot(const AIsNot: Boolean);
begin
  FIsNot := AIsNot;
end;

procedure TDataValidatorItemBase.SetMessage(const AMessage: string);
begin
  if not AMessage.IsEmpty then
    FMessage := AMessage;
end;

procedure TDataValidatorItemBase.SetExecute(const AExecute: TDataValidatorInformationExecute);
begin
  FExecute := AExecute;
end;

function TDataValidatorItemBase.GetValueAsString: string;
var
  LJSONPair: TJSONPair;
begin
  Result := '';

  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    if Assigned(LJSONPair) then
      Result := LJSONPair.JsonValue.Value;
  end
  else
    Result := FValue.AsString;
end;

procedure TDataValidatorItemBase.SetValueAdapter(const AValue: TValue);
var
  LJSONPair: TJSONPair;
  LJSONGetValue: TJSONValue;
  LValueInt: Int64;
begin
  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    if Assigned(LJSONPair) then
    begin
      LJSONGetValue := LJSONPair.JsonValue;

      if LJSONGetValue is TJSONBool then
        FValue.AsType<TJSONPair>.JsonValue := TJSONBool.Create(AValue.AsBoolean)
      else
        if LJSONGetValue is TJSONNumber then
        begin
          if TryStrToInt64(LJSONGetValue.Value, LValueInt) then
            FValue.AsType<TJSONPair>.JsonValue := TJSONNumber.Create(AValue.AsInt64)
          else
            FValue.AsType<TJSONPair>.JsonValue := TJSONNumber.Create(AValue.AsType<Double>)
        end
        else
          if LJSONGetValue is TJSONString then
            FValue.AsType<TJSONPair>.JsonValue := TJSONString.Create(AValue.AsString);

      Exit;
    end;
  end;

  FValue := AValue;
end;

end.
